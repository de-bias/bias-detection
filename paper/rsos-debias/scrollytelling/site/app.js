(() => {
  'use strict';

  const svg = document.querySelector('#story-svg');
  if (!svg) return;

  const NS = 'http://www.w3.org/2000/svg';
  const enhancementQuery = window.matchMedia('(min-width: 761px) and (prefers-reduced-motion: no-preference)');
  const progress = document.querySelector('#story-progress');
  const summary = document.querySelector('#chart-summary');
  const tooltip = document.querySelector('#chart-tooltip');
  const chartShell = document.querySelector('.chart-shell');
  const steps = [...document.querySelectorAll('.story-step')];
  const chapterButtons = [...document.querySelectorAll('[data-go-state]')];
  const titleNode = svg.querySelector('title');
  const descriptionNode = svg.querySelector('desc');

  const formatInteger = new Intl.NumberFormat('en-GB', { maximumFractionDigits: 0 });
  const formatRate = new Intl.NumberFormat('en-GB', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  const compactNumber = new Intl.NumberFormat('en-GB', { notation: 'compact', maximumFractionDigits: 1 });

  const stateContent = [
    {
      label: '2 / 8 · Counts',
      title: 'Census population and Meta active-account estimates',
      description: 'Scatter plot of 331 local authorities. Larger populations generally have larger Meta estimates; Pearson r equals 0.91.',
      summary: 'Larger places generally produce larger counts. Pearson r = .91 across 331 local authorities.'
    },
    {
      label: '3 / 8 · Population coverage rates',
      title: 'Meta active-account population estimates per 100 residents',
      description: 'Strip plot of all 331 local rates. The middle 90 percent spans 4.61 to 12.31 around a fitted rate of 8.09.',
      summary: 'The fitted rate is 8.09 per 100 residents; the middle 90% of local rates spans 4.61–12.31.'
    },
    {
      label: '4 / 8 · Map',
      title: 'Local population coverage rate minus fitted Meta rate',
      description: 'Map of 331 local authorities in England and Wales. Teal areas have fewer estimates per resident than fitted; coral areas have more.',
      summary: ''
    }
  ];

  let storyData;
  let metadata;
  let areas;
  let nodes;
  let axisLayer;
  let pathLayer;
  let circleLayer;
  let annotationLayer;
  let currentState = -1;
  let initialised = false;
  let scrollQueued = false;
  let loadPromise;

  const makeSvg = (tag, attributes = {}) => {
    const element = document.createElementNS(NS, tag);
    Object.entries(attributes).forEach(([key, value]) => element.setAttribute(key, value));
    return element;
  };

  const addText = (parent, text, x, y, className = '', anchor = 'start') => {
    const element = makeSvg('text', { x, y, 'text-anchor': anchor });
    if (className) element.setAttribute('class', className);
    element.textContent = text;
    parent.append(element);
    return element;
  };

  const addLine = (parent, x1, y1, x2, y2, className = 'axis-line') => {
    const element = makeSvg('line', { x1, y1, x2, y2, class: className });
    parent.append(element);
    return element;
  };

  const linearScale = (domainMin, domainMax, rangeMin, rangeMax) => value => (
    rangeMin + ((value - domainMin) / (domainMax - domainMin)) * (rangeMax - rangeMin)
  );

  const interpolateHex = (from, to, amount) => {
    const start = from.match(/\w\w/g).map(value => parseInt(value, 16));
    const end = to.match(/\w\w/g).map(value => parseInt(value, 16));
    const colour = start.map((value, index) => Math.round(value + (end[index] - value) * amount));
    return `rgb(${colour.join(' ')})`;
  };

  const departureColour = value => {
    const strength = Math.pow(Math.min(1, Math.abs(value) / metadata.departure_limit), 0.72);
    return value < 0
      ? interpolateHex('#edece7', '#078481', strength)
      : interpolateHex('#edece7', '#f2553d', strength);
  };

  const deterministicJitter = code => {
    let hash = 0;
    for (const character of code) hash = ((hash << 5) - hash) + character.charCodeAt(0);
    return ((Math.abs(hash) % 101) / 100 - 0.5) * 82;
  };

  const approximately = (actual, expected, tolerance = 1e-6) => Math.abs(actual - expected) <= tolerance;

  const validateData = data => {
    if (!data || !data.metadata || !Array.isArray(data.areas) || !Array.isArray(data.pair)) {
      throw new Error('Story data are incomplete.');
    }
    if (data.areas.length !== 331 || data.metadata.area_count !== 331) {
      throw new Error(`Expected 331 local authorities; received ${data.areas.length}.`);
    }
    if (!approximately(data.metadata.pearson, 0.9133192, 1e-7)) throw new Error('Pearson coefficient does not match the reviewed value.');
    if (!approximately(data.metadata.alpha_per_100, 8.0948036, 1e-7)) throw new Error('Fitted Meta rate does not match the reviewed value.');
    if (!approximately(data.metadata.rate_q05, 4.6092857, 1e-7)) throw new Error('Lower rate quantile does not match the reviewed value.');
    if (!approximately(data.metadata.rate_q95, 12.3077977, 1e-7)) throw new Error('Upper rate quantile does not match the reviewed value.');

    const expectedPair = new Map([
      ['E07000103', { population: 102246, meta: 2419, rate: 2.3658627 }],
      ['E07000038', { population: 102001, meta: 16204, rate: 15.8861188 }]
    ]);

    data.pair.forEach(area => {
      const expected = expectedPair.get(area.code);
      if (!expected || area.population !== expected.population || !approximately(area.meta, expected.meta) || !approximately(area.rate, expected.rate)) {
        throw new Error(`Pair evidence does not match the reviewed value for ${area.code}.`);
      }
    });

    document.querySelectorAll('.place-card[data-lad-code]').forEach(card => {
      const expected = expectedPair.get(card.dataset.ladCode);
      if (!expected) throw new Error(`Unexpected pair card ${card.dataset.ladCode}.`);
      if (Number(card.dataset.population) !== expected.population || Number(card.dataset.meta) !== expected.meta || !approximately(Number(card.dataset.rate), expected.rate)) {
        throw new Error(`First-frame HTML does not match generated data for ${card.dataset.ladCode}.`);
      }
    });
  };

  const tooltipHtml = area => {
    const difference = Math.abs(area.departure);
    const direction = area.departure < 0 ? 'fewer' : 'more';
    return `<strong>${area.name}</strong>
      <span>Census population: ${formatInteger.format(area.population)}</span>
      <span>Meta estimate: ${formatInteger.format(area.meta)}</span>
      <span>${formatRate.format(area.rate)} per 100 residents</span>
      <span>${formatRate.format(difference)} ${direction} than fitted</span>`;
  };

  const positionTooltip = event => {
    const shellBounds = chartShell.getBoundingClientRect();
    const tooltipBounds = tooltip.getBoundingClientRect();
    const eventX = event.clientX || (shellBounds.left + shellBounds.width / 2);
    const eventY = event.clientY || (shellBounds.top + shellBounds.height / 2);
    tooltip.style.left = `${Math.min(shellBounds.width - tooltipBounds.width - 12, Math.max(12, eventX - shellBounds.left + 14))}px`;
    tooltip.style.top = `${Math.min(shellBounds.height - tooltipBounds.height - 12, Math.max(12, eventY - shellBounds.top + 14))}px`;
  };

  const showTooltip = (event, area) => {
    tooltip.innerHTML = tooltipHtml(area);
    tooltip.hidden = false;
    positionTooltip(event);
  };

  const hideTooltip = () => {
    tooltip.hidden = true;
  };

  const addMarkEvents = (element, area) => {
    element.setAttribute('role', 'graphics-symbol');
    element.setAttribute('aria-label', `${area.name}: ${formatRate.format(area.rate)} Meta active-account population estimates per 100 residents.`);
    element.addEventListener('pointerenter', event => showTooltip(event, area));
    element.addEventListener('pointermove', positionTooltip);
    element.addEventListener('pointerleave', hideTooltip);
    element.addEventListener('focus', event => showTooltip(event, area));
    element.addEventListener('blur', hideTooltip);
  };

  const setVisibility = (element, opacity, pointerEvents = 'none', focusable = false) => {
    element.style.opacity = opacity;
    element.style.pointerEvents = pointerEvents;
    element.setAttribute('tabindex', focusable ? '0' : '-1');
    if (focusable) element.removeAttribute('aria-hidden');
    else element.setAttribute('aria-hidden', 'true');
  };

  const initialiseChart = () => {
    svg.replaceChildren();
    const title = makeSvg('title', { id: 'chart-title' });
    const description = makeSvg('desc', { id: 'chart-description' });
    svg.append(title, description);

    axisLayer = makeSvg('g', { class: 'axis-layer', 'aria-hidden': 'true' });
    pathLayer = makeSvg('g', { class: 'path-layer' });
    circleLayer = makeSvg('g', { class: 'circle-layer' });
    annotationLayer = makeSvg('g', { class: 'annotation-layer', 'aria-hidden': 'true' });
    svg.append(axisLayer, pathLayer, circleLayer, annotationLayer);

    const pairCodes = new Set(storyData.pair.map(area => area.code));
    nodes = new Map();

    areas.forEach(area => {
      const path = makeSvg('path', {
        class: 'area-path',
        d: area.path,
        fill: departureColour(area.departure),
        'data-lad-code': area.code
      });
      const circle = makeSvg('circle', {
        class: 'area-circle',
        cx: 105,
        cy: 575,
        r: 0,
        fill: '#31506d',
        'data-lad-code': area.code
      });
      if (pairCodes.has(area.code)) {
        path.classList.add('is-pair');
        circle.classList.add('is-pair');
      }
      setVisibility(path, 0);
      setVisibility(circle, 0);
      addMarkEvents(path, area);
      addMarkEvents(circle, area);
      pathLayer.append(path);
      circleLayer.append(circle);
      nodes.set(area.code, { area, path, circle });
    });

    titleNode?.remove();
    descriptionNode?.remove();
  };

  const clearScaffolding = () => {
    axisLayer.replaceChildren();
    annotationLayer.replaceChildren();
    hideTooltip();
  };

  const prepareCircles = () => {
    clearScaffolding();
    pathLayer.removeAttribute('transform');
    nodes.forEach(({ path, circle, area }) => {
      setVisibility(path, 0);
      const pair = area.name === 'Watford' || area.name === 'North East Derbyshire';
      setVisibility(circle, pair ? 1 : 0.54, 'auto', pair);
    });
  };

  const prepareMap = () => {
    clearScaffolding();
    pathLayer.setAttribute('transform', 'translate(55 5) scale(0.83)');
    nodes.forEach(({ path, circle, area }) => {
      setVisibility(circle, 0);
      const pair = area.name === 'Watford' || area.name === 'North East Derbyshire';
      setVisibility(path, 0.96, 'auto', pair);
    });
  };

  const drawAxes = ({ xScale, yScale, xTicks, yTicks, xLabel, yLabel, xFormat, yFormat }) => {
    const left = 105;
    const right = 835;
    const top = 72;
    const bottom = 575;

    yTicks.forEach(tick => {
      const y = yScale(tick);
      addLine(axisLayer, left, y, right, y, 'grid-line');
      addText(axisLayer, yFormat(tick), left - 13, y + 5, 'tick-label', 'end');
    });
    xTicks.forEach(tick => {
      const x = xScale(tick);
      addLine(axisLayer, x, bottom, x, bottom + 7, 'tick-line');
      addText(axisLayer, xFormat(tick), x, bottom + 29, 'tick-label', 'middle');
    });
    addLine(axisLayer, left, bottom, right, bottom, 'axis-line');
    addLine(axisLayer, left, top, left, bottom, 'axis-line');
    addText(axisLayer, xLabel, (left + right) / 2, 645, 'axis-label', 'middle');
    const yTitle = addText(axisLayer, yLabel, 27, (top + bottom) / 2, 'axis-label', 'middle');
    yTitle.setAttribute('transform', `rotate(-90 27 ${(top + bottom) / 2})`);
  };

  const pairColour = area => area.name === 'Watford' ? '#078481' : '#f2553d';

  const drawCounts = () => {
    prepareCircles();
    const left = 105;
    const right = 835;
    const top = 72;
    const bottom = 575;
    const x = linearScale(0, 1200000, left, right);
    const y = linearScale(0, 100000, bottom, top);

    drawAxes({
      xScale: x,
      yScale: y,
      xTicks: [0, 250000, 500000, 750000, 1000000, 1200000],
      yTicks: [0, 25000, 50000, 75000, 100000],
      xLabel: '2021 Census resident population',
      yLabel: 'Meta active-account population estimate',
      xFormat: value => value === 0 ? '0' : compactNumber.format(value),
      yFormat: value => value === 0 ? '0' : compactNumber.format(value)
    });

    const fitted = addLine(annotationLayer, x(0), y(0), x(1200000), y((metadata.alpha_per_100 / 100) * 1200000), 'fitted-line');
    fitted.setAttribute('stroke', '#9aa0a5');
    fitted.setAttribute('stroke-width', '2');
    fitted.setAttribute('stroke-dasharray', '7 6');
    fitted.setAttribute('vector-effect', 'non-scaling-stroke');

    areas.forEach(area => {
      const { circle } = nodes.get(area.code);
      const pair = area.name === 'Watford' || area.name === 'North East Derbyshire';
      circle.setAttribute('cx', x(area.population));
      circle.setAttribute('cy', y(area.meta));
      circle.setAttribute('r', pair ? 8 : 4.1);
      circle.setAttribute('fill', pair ? pairColour(area) : '#123b63');
      circle.setAttribute('stroke', pair ? '#082b55' : 'none');
    });

    addText(annotationLayer, 'Pearson r', 665, 130, 'chart-note');
    addText(annotationLayer, '.91', 665, 190, 'stat-large');
    addText(annotationLayer, 'The relationship is strong.', 665, 219, 'chart-label');

    storyData.pair.forEach((area, index) => {
      const labelX = x(area.population) + (index === 0 ? 42 : 46);
      const labelY = y(area.meta) + (index === 0 ? 24 : -42);
      const leader = addLine(annotationLayer, x(area.population) + 7, y(area.meta), labelX - 7, labelY - 5, 'tick-line');
      leader.style.stroke = pairColour(area);
      const label = addText(annotationLayer, area.name, labelX, labelY, 'chart-label');
      label.style.fill = pairColour(area);
    });
  };

  const drawRates = () => {
    prepareCircles();
    const left = 105;
    const right = 835;
    const baselineY = 340;
    const x = linearScale(0, 25, left, right);

    const band = makeSvg('rect', {
      x: x(metadata.rate_q05),
      y: 180,
      width: x(metadata.rate_q95) - x(metadata.rate_q05),
      height: 320,
      fill: '#078481',
      opacity: 0.08
    });
    axisLayer.append(band);
    addText(axisLayer, 'middle 90% of local authorities', (x(metadata.rate_q05) + x(metadata.rate_q95)) / 2, 164, 'chart-note', 'middle');
    addLine(axisLayer, left, baselineY, right, baselineY, 'axis-line');

    [0, 5, 10, 15, 20, 25].forEach(tick => {
      addLine(axisLayer, x(tick), baselineY - 8, x(tick), baselineY + 8, 'tick-line');
      addText(axisLayer, String(tick), x(tick), 555, 'tick-label', 'middle');
    });
    addText(axisLayer, 'Meta active-account population estimates per 100 residents', (left + right) / 2, 605, 'axis-label', 'middle');

    const fittedX = x(metadata.alpha_per_100);
    const fittedLine = addLine(annotationLayer, fittedX, 120, fittedX, 505, 'fitted-line');
    fittedLine.setAttribute('stroke', '#082b55');
    fittedLine.setAttribute('stroke-width', '2');
    fittedLine.setAttribute('stroke-dasharray', '7 6');
    addText(annotationLayer, 'fitted proportional rate', fittedX, 92, 'chart-note', 'middle');
    addText(annotationLayer, formatRate.format(metadata.alpha_per_100), fittedX, 118, 'chart-label', 'middle');

    areas.forEach(area => {
      const { circle } = nodes.get(area.code);
      const pair = area.name === 'Watford' || area.name === 'North East Derbyshire';
      circle.setAttribute('cx', x(area.rate));
      circle.setAttribute('cy', baselineY + deterministicJitter(area.code));
      circle.setAttribute('r', pair ? 8 : 4.2);
      circle.setAttribute('fill', pair ? pairColour(area) : '#34546f');
      circle.setAttribute('stroke', pair ? '#082b55' : 'none');
    });

    storyData.pair.forEach((area, index) => {
      const xPosition = x(area.rate);
      const labelY = index === 0 ? 242 : 458;
      const leader = addLine(annotationLayer, xPosition, baselineY + (index === 0 ? -55 : 55), xPosition, labelY + (index === 0 ? 12 : -24), 'tick-line');
      leader.style.stroke = pairColour(area);
      const label = addText(annotationLayer, area.name, xPosition, labelY, 'chart-label', 'middle');
      label.style.fill = pairColour(area);
      addText(annotationLayer, formatRate.format(area.rate), xPosition, labelY + 23, 'chart-note', 'middle');
    });
  };

  const drawMapLegend = () => {
    const legend = makeSvg('g', { transform: 'translate(185 558)' });
    annotationLayer.append(legend);
    addText(legend, 'Difference from fitted rate · estimates per 100 census residents', 0, 0, 'chart-label');

    const stepsCount = 11;
    for (let index = 0; index < stepsCount; index += 1) {
      const value = -metadata.departure_limit + (index / (stepsCount - 1)) * metadata.departure_limit * 2;
      legend.append(makeSvg('rect', { x: index * 45, y: 22, width: 46, height: 14, fill: departureColour(value) }));
    }
    addText(legend, 'fewer than fitted', 0, 58, 'chart-note');
    addText(legend, '0 = fitted', 248, 58, 'chart-note', 'middle');
    addText(legend, 'more than fitted', 496, 58, 'chart-note', 'end');
    addText(legend, `full scale: ±${formatRate.format(metadata.departure_limit)}`, 496, 82, 'chart-note', 'end');
  };

  const drawMap = () => {
    prepareMap();
    nodes.forEach(({ path, area }) => path.setAttribute('fill', departureColour(area.departure)));
    addText(annotationLayer, 'Local rate minus fitted Meta rate', 46, 55, 'chart-label');
    addText(annotationLayer, 'England and Wales · 331 local authorities', 46, 80, 'chart-note');
    drawMapLegend();
  };

  const renderState = state => {
    if (state < 0 || state > 2 || state === currentState) return;
    currentState = state;
    const content = stateContent[state];
    progress.textContent = content.label;
    summary.textContent = content.summary;
    svg.querySelector('title').textContent = content.title;
    svg.querySelector('desc').textContent = content.description;
    steps.forEach((step, index) => step.classList.toggle('is-active', index === state));
    chapterButtons.forEach((button, index) => {
      const active = index === state;
      button.classList.toggle('is-current', active);
      if (active) button.setAttribute('aria-current', 'step');
      else button.removeAttribute('aria-current');
    });
    [drawCounts, drawRates, drawMap][state]();
  };

  const currentStepFromViewport = () => {
    const trigger = window.innerHeight * 0.58;
    let state = 0;
    steps.forEach((step, index) => {
      if (step.getBoundingClientRect().top <= trigger) state = index;
    });
    return state;
  };

  const updateFromScroll = () => {
    scrollQueued = false;
    renderState(currentStepFromViewport());
  };

  const setupScrollState = () => {
    const queueUpdate = () => {
      if (scrollQueued) return;
      scrollQueued = true;
      window.requestAnimationFrame(updateFromScroll);
    };
    window.addEventListener('scroll', queueUpdate, { passive: true });
    window.addEventListener('resize', queueUpdate, { passive: true });
  };

  const setupControls = () => {
    chapterButtons.forEach(button => {
      button.addEventListener('click', () => {
        const state = Number(button.dataset.goState);
        steps[state].scrollIntoView({ behavior: 'smooth', block: 'center' });
      });
    });
  };

  const enableStory = () => {
    if (!storyData || initialised || !enhancementQuery.matches) return;
    initialised = true;
    initialiseChart();
    setupControls();
    setupScrollState();
    document.documentElement.classList.add('story-ready');
    renderState(currentStepFromViewport());
  };

  const loadStoryData = () => {
    if (!enhancementQuery.matches || storyData || loadPromise) return;
    loadPromise = fetch('data/meta-story.json')
      .then(response => {
        if (!response.ok) throw new Error(`Story data request failed: ${response.status}`);
        return response.json();
      })
      .then(data => {
        validateData(data);
        storyData = data;
        metadata = data.metadata;
        areas = data.areas;
        enableStory();
      })
      .catch(error => {
        console.error(error);
        document.documentElement.classList.remove('story-ready');
      })
      .finally(() => {
        loadPromise = undefined;
      });
  };

  loadStoryData();

  enhancementQuery.addEventListener?.('change', event => {
    if (event.matches) {
      loadStoryData();
      enableStory();
    }
  });
})();
