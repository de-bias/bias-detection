(() => {
  'use strict';

  const story = document.querySelector('.source-story');
  const svg = document.querySelector('#source-svg');
  if (!story || !svg) return;

  const NS = 'http://www.w3.org/2000/svg';
  const enhancementQuery = window.matchMedia('(min-width: 991px) and (min-height: 680px) and (prefers-reduced-motion: no-preference)');
  const progress = story.querySelector('#source-progress');
  const summary = story.querySelector('#source-summary');
  const steps = [...story.querySelectorAll('.source-step')];
  const railLinks = [...story.querySelectorAll('.source-rail a')];
  const stateLinks = [...story.querySelectorAll('[data-source-state]')];
  const title = svg.querySelector('title');
  const description = svg.querySelector('desc');

  const expectedSources = ['twitter_x', 'meta', 'multiapp1', 'multiapp2'];
  const pairCodes = ['E07000103', 'E07000038'];
  const colours = {
    navy: '#082b55',
    grey: '#6d747c',
    line: '#cfd2d1',
    paper: '#fbfaf6',
    teal: '#078481',
    coral: '#f2553d'
  };

  const stateContent = [
    {
      progress: '5 / 8 · Sources',
      title: 'Watford and North East Derbyshire across four digital sources',
      description: 'Four horizontal source tracks show the two local authorities changing sides of separate source-specific fitted rates.',
      summary: 'One times marks each source’s fitted population coverage rate. The same area can move above or below that fitted rate depending on the data source.'
    },
    {
      progress: '5 / 8 · Sources',
      title: 'Three hundred of 331 local authorities change sides across sources',
      description: 'A unit chart of 331 local authorities. Three hundred sit above a fitted rate in at least one source and below it in another.',
      summary: '300 of 331 authorities change sides across the four source-specific fitted rates.'
    }
  ];

  let data;
  let loadPromise;
  let currentState = -1;
  let chartInitialised = false;
  let stepObserver;

  const makeSvg = (tag, attributes = {}) => {
    const element = document.createElementNS(NS, tag);
    Object.entries(attributes).forEach(([key, value]) => element.setAttribute(key, value));
    return element;
  };

  const addText = (parent, value, x, y, className = '', anchor = 'start') => {
    const element = makeSvg('text', { x, y, 'text-anchor': anchor });
    if (className) element.setAttribute('class', className);
    element.textContent = value;
    parent.append(element);
    return element;
  };

  const approximately = (actual, expected, tolerance = 1e-9) => Math.abs(actual - expected) <= tolerance;

  const validateData = payload => {
    if (!payload?.metadata || !Array.isArray(payload.sources) || !Array.isArray(payload.pair) || !Array.isArray(payload.areas)) {
      throw new Error('Four-source story data are incomplete.');
    }
    if (payload.metadata.area_count !== 331 || payload.areas.length !== 331) throw new Error('Expected 331 local authorities.');
    if (payload.metadata.switch_count !== 300 || payload.metadata.consistent_below_count !== 22 || payload.metadata.consistent_above_count !== 9) {
      throw new Error('Cross-source pattern counts do not match the reviewed evidence.');
    }
    if (payload.sources.length !== 4 || payload.pair.length !== 2) throw new Error('Expected four sources and two featured areas.');
    const sourceIds = payload.sources.map(source => source.id);
    if (sourceIds.join('|') !== expectedSources.join('|')) throw new Error('Source order changed unexpectedly.');
    const expectedPair = new Map([
      ['E07000103', { twitter_x: 1.086108007597, meta: 0.2922693162924, multiapp1: 1.438424489387, multiapp2: 1.266706796378 }],
      ['E07000038', { twitter_x: 0.4855779024976, meta: 1.962508231758, multiapp1: 0.8265236005252, multiapp2: 1.212251262979 }]
    ]);
    payload.pair.forEach(area => {
      const fixture = expectedPair.get(area.code);
      if (!fixture) throw new Error(`Unexpected featured area ${area.code}.`);
      expectedSources.forEach(sourceId => {
        const ratio = area.observations?.[sourceId]?.ratio_to_fitted;
        if (!Number.isFinite(ratio) || !approximately(ratio, fixture[sourceId])) {
          throw new Error(`Featured-area evidence changed for ${area.code}/${sourceId}.`);
        }
      });
    });
    const recomputedSwitches = payload.areas.filter(area => area.switches_sides).length;
    if (recomputedSwitches !== 300) throw new Error('Stored area classifications do not reproduce 300 switches.');
  };

  const clearChart = content => {
    svg.replaceChildren();
    const chartTitle = makeSvg('title', { id: 'source-chart-title' });
    const chartDescription = makeSvg('desc', { id: 'source-chart-description' });
    chartTitle.textContent = content.title;
    chartDescription.textContent = content.description;
    svg.append(chartTitle, chartDescription);
  };

  const sourceById = id => data.sources.find(source => source.id === id);

  const ratioX = ratio => {
    const clamped = Math.max(0.25, Math.min(4, ratio));
    return 190 + ((Math.log2(clamped) + 2) / 4) * 650;
  };

  const sideColour = ratio => ratio < 1 ? colours.teal : colours.coral;

  const addDiamond = (parent, x, y, size, fill) => {
    parent.append(makeSvg('rect', {
      x: x - size / 2,
      y: y - size / 2,
      width: size,
      height: size,
      rx: 1,
      fill,
      transform: `rotate(45 ${x} ${y})`
    }));
  };

  const drawFingerprint = () => {
    const content = stateContent[0];
    clearChart(content);
    addText(svg, 'Relative to each source’s fitted rate', 40, 48, 'source-label');
    addText(svg, 'Fewer than fitted', 190, 77, 'source-note');
    addText(svg, 'More than fitted', 840, 77, 'source-note', 'end');

    [0.25, 0.5, 1, 2, 4].forEach(tick => {
      const x = ratioX(tick);
      svg.append(makeSvg('line', { x1: x, y1: 94, x2: x, y2: 550, class: tick === 1 ? 'source-axis-line' : 'source-grid-line' }));
      addText(svg, `${tick}×`, x, 575, 'source-axis-label', 'middle');
    });

    expectedSources.forEach((sourceId, sourceIndex) => {
      const source = sourceById(sourceId);
      const y = 145 + sourceIndex * 106;
      addText(svg, source.label, 40, y + 5, 'source-label');
      addText(svg, `r = ${source.raw_count_pearson_r.toFixed(2).replace(/^0/, '')}`, 40, y + 28, 'source-note');
      svg.append(makeSvg('line', { x1: 190, y1: y, x2: 840, y2: y, class: 'source-axis-line' }));

      data.pair.forEach((area, areaIndex) => {
        const ratio = area.observations[sourceId].ratio_to_fitted;
        const x = ratioX(ratio);
        const markerY = y + (areaIndex === 0 ? -17 : 17);
        const colour = sideColour(ratio);
        if (areaIndex === 0) {
          svg.append(makeSvg('circle', { cx: x, cy: markerY, r: 8, fill: colour, stroke: colours.navy, 'stroke-width': 1.5 }));
        } else {
          addDiamond(svg, x, markerY, 13, colour);
        }
        const anchor = x > 735 ? 'end' : 'start';
        const labelX = x + (anchor === 'end' ? -12 : 12);
        const shortName = areaIndex === 0 ? 'Watford' : 'N.E. Derbyshire';
        addText(svg, `${shortName} ${ratio.toFixed(2)}×`, labelX, markerY + 5, 'source-value', anchor);
      });
    });

    svg.append(makeSvg('circle', { cx: 240, cy: 620, r: 7, fill: colours.paper, stroke: colours.navy, 'stroke-width': 2 }));
    addText(svg, 'Watford', 255, 625, 'source-note');
    addDiamond(svg, 390, 620, 11, colours.paper);
    const diamond = svg.lastElementChild;
    diamond.setAttribute('stroke', colours.navy);
    diamond.setAttribute('stroke-width', '2');
    addText(svg, 'North East Derbyshire', 405, 625, 'source-note');
  };

  const drawSwitches = () => {
    const content = stateContent[1];
    clearChart(content);
    const defs = makeSvg('defs');
    const switchGradient = makeSvg('linearGradient', { id: 'source-switch-gradient', x1: '0%', y1: '0%', x2: '100%', y2: '0%' });
    switchGradient.append(
      makeSvg('stop', { offset: '0%', 'stop-color': colours.teal }),
      makeSvg('stop', { offset: '49.5%', 'stop-color': colours.teal }),
      makeSvg('stop', { offset: '50.5%', 'stop-color': colours.coral }),
      makeSvg('stop', { offset: '100%', 'stop-color': colours.coral })
    );
    defs.append(switchGradient);
    svg.append(defs);
    addText(svg, '300', 175, 105, 'source-stat');
    addText(svg, '/ 331 local authorities', 355, 98, 'source-label');
    addText(svg, 'change sides across source-specific fitted rates', 355, 122, 'source-note');

    const pairFirst = [...data.areas].sort((a, b) => {
      const aPair = pairCodes.includes(a.code) ? 0 : 1;
      const bPair = pairCodes.includes(b.code) ? 0 : 1;
      if (aPair !== bPair) return aPair - bPair;
      if (a.switches_sides !== b.switches_sides) return a.switches_sides ? -1 : 1;
      return a.name.localeCompare(b.name);
    });
    const columns = 19;
    const size = 18;
    const gap = 7;
    const chartWidth = columns * size + (columns - 1) * gap;
    const startX = (900 - chartWidth) / 2;
    const startY = 165;

    pairFirst.forEach((area, index) => {
      const column = index % columns;
      const row = Math.floor(index / columns);
      const x = startX + column * (size + gap);
      const y = startY + row * (size + gap);
      const mark = makeSvg('rect', {
        x,
        y,
        width: size,
        height: size,
        rx: 2.5,
        fill: area.switches_sides ? 'url(#source-switch-gradient)' : colours.paper,
        stroke: pairCodes.includes(area.code) ? colours.navy : (area.switches_sides ? colours.white : '#939aa0'),
        'stroke-width': pairCodes.includes(area.code) ? 3 : 1.2,
        class: pairCodes.includes(area.code) ? 'source-unit source-pair-mark' : 'source-unit'
      });
      svg.append(mark);
    });

    const legendY = 625;
    svg.append(makeSvg('rect', { x: 205, y: legendY - 12, width: 14, height: 14, rx: 2, fill: 'url(#source-switch-gradient)' }));
    addText(svg, '300 change sides', 228, legendY, 'source-note');
    svg.append(makeSvg('rect', { x: 420, y: legendY - 12, width: 14, height: 14, rx: 2, fill: colours.paper, stroke: '#939aa0' }));
    addText(svg, '31 remain on one side', 443, legendY, 'source-note');
  };

  const updateRail = () => {
    railLinks.forEach(link => {
      const active = link.getAttribute('href') === '#source-intro';
      link.classList.toggle('is-current', active);
      if (active) link.setAttribute('aria-current', 'step');
      else link.removeAttribute('aria-current');
    });
  };

  const renderState = state => {
    if (!story.classList.contains('is-ready') || state < 0 || state > 1 || state === currentState) return;
    currentState = state;
    const content = stateContent[state];
    progress.textContent = content.progress;
    summary.textContent = content.summary;
    steps.forEach((step, index) => step.classList.toggle('is-active', index === state));
    updateRail(state);
    [drawFingerprint, drawSwitches][state]();
    svg.animate?.([{ opacity: 0.35 }, { opacity: 1 }], { duration: 220, easing: 'ease-out' });
  };

  const setupStepObserver = () => {
    if (stepObserver || !('IntersectionObserver' in window)) return;
    stepObserver = new IntersectionObserver(entries => {
      if (!entries.some(entry => entry.isIntersecting)) return;
      renderState(currentStateFromViewport());
    }, { rootMargin: '-38% 0px -38% 0px', threshold: [0, 0.1, 0.25, 0.5] });
    steps.forEach(step => stepObserver.observe(step));
  };

  const currentStateFromViewport = () => {
    const trigger = window.innerHeight * 0.55;
    let state = 0;
    steps.forEach((step, index) => {
      if (step.getBoundingClientRect().top <= trigger) state = index;
    });
    return Math.min(1, state);
  };

  const syncEnhancement = () => {
    if (!data || !enhancementQuery.matches) {
      story.classList.remove('is-ready');
      currentState = -1;
      return;
    }
    story.classList.add('is-ready');
    if (!chartInitialised) {
      chartInitialised = true;
      setupStepObserver();
    }
    requestAnimationFrame(() => renderState(currentStateFromViewport()));
  };

  const loadData = () => {
    if (data) {
      syncEnhancement();
      return Promise.resolve(data);
    }
    if (loadPromise) return loadPromise;

    loadPromise = fetch('data/source-story.json')
      .then(response => {
        if (!response.ok) throw new Error(`Four-source story request failed: ${response.status}`);
        return response.json();
      })
      .then(payload => {
        validateData(payload);
        data = payload;
        syncEnhancement();
        return data;
      })
      .catch(error => {
        console.error(error);
        story.classList.remove('is-ready');
        loadPromise = undefined;
      });

    return loadPromise;
  };

  if ('IntersectionObserver' in window) {
    const loadObserver = new IntersectionObserver(entries => {
      if (!entries.some(entry => entry.isIntersecting)) return;
      loadObserver.disconnect();
      if (enhancementQuery.matches) loadData();
    }, { rootMargin: '1200px 0px' });
    loadObserver.observe(story);
  } else if (enhancementQuery.matches) {
    loadData();
  }

  enhancementQuery.addEventListener?.('change', event => {
    if (event.matches && !data) loadData();
    else syncEnhancement();
  });

  stateLinks.forEach(link => {
    link.addEventListener('click', () => {
      const state = Number(link.dataset.sourceState);
      if (Number.isInteger(state)) renderState(state);
    });
  });
})();
