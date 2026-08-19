(() => {
  'use strict';

  const section = document.querySelector('#context-fingerprints');
  if (!section) return;

  const sourceIds = ['twitter', 'meta', 'multiapp1', 'multiapp2'];
  const domainIds = ['demographic', 'socioeconomic', 'resource_access', 'mobility_geography'];
  const key = (sourceId, domainId) => `${sourceId}:${domainId}`;
  const buttons = [...section.querySelectorAll('[data-context-source]')];
  const cards = new Map([...section.querySelectorAll('[data-context-card]')]
    .map(card => [card.dataset.contextCard, card]));
  const ui = {
    sourceName: section.querySelector('#context-source-name'),
    previous: section.querySelector('[data-context-prev]'),
    next: section.querySelector('[data-context-next]'),
    position: section.querySelector('#context-domain-position'),
    details: section.querySelector('#context-feature-details'),
    caption: section.querySelector('#context-feature-table-caption'),
    tableBody: section.querySelector('#context-feature-table-body')
  };

  if (
    buttons.length !== sourceIds.length ||
    domainIds.some(id => !cards.has(id)) ||
    Object.values(ui).some(element => !element)
  ) return;

  let domainIndex = 0;
  let sources;
  let groups;
  let panels;

  const valueText = value => value === null || value === undefined ? '—' : String(value);

  const addCell = (row, label, value) => {
    const cell = document.createElement('td');
    cell.dataset.label = label;
    cell.textContent = value;
    row.append(cell);
  };

  const makeAlt = (source, group, panel) => {
    const labels = panel.features
      .filter(feature => feature.is_labelled)
      .map(feature => feature.short_label || feature.label);
    const shown = labels.slice(0, 3).join(', ');
    const extra = labels.length - Math.min(labels.length, 3);
    const labelNote = labels.length
      ? ` Labels above 0.5 include ${shown}${extra ? ` and ${extra} more` : ''}.`
      : ' No feature in this group is labelled above 0.5.';
    return `Radial fingerprint for ${source.label}: ${group.label}. Radius shows within-source relative mean absolute SHAP importance, not direction or user composition.${labelNote}`;
  };

  const updateTable = source => {
    const fragment = document.createDocumentFragment();
    domainIds.forEach(domainId => {
      const group = groups.get(domainId);
      panels.get(key(source.id, domainId)).features.forEach(feature => {
        const row = document.createElement('tr');
        row.classList.toggle('is-labelled', feature.is_labelled);
        addCell(row, 'Context', group.label);
        addCell(row, 'Area characteristic', feature.label || feature.short_label || feature.id);
        addCell(row, 'Raw mean absolute SHAP', valueText(feature.raw_mean_abs_shap));
        addCell(row, 'Relative importance (0–1)', valueText(feature.normalised_importance));
        addCell(row, 'Source rank', valueText(feature.source_rank));
        fragment.append(row);
      });
    });
    ui.tableBody.replaceChildren(fragment);
    ui.caption.textContent = `${source.label} accepted-revision main-model feature importance`;
    ui.details.querySelector('summary').textContent = `View all feature-importance values for ${source.label}`;
  };

  const showDomain = () => {
    const domainId = domainIds[domainIndex];
    domainIds.forEach((id, index) => {
      cards.get(id).classList.toggle('is-current', index === domainIndex);
    });
    ui.previous.disabled = domainIndex === 0;
    ui.next.disabled = domainIndex === domainIds.length - 1;
    ui.position.textContent = `${domainIndex + 1} of ${domainIds.length} · ${groups.get(domainId).label}`;
  };

  const selectSource = sourceId => {
    const source = sources.get(sourceId);
    if (!source) return;

    buttons.forEach(button => {
      const active = button.dataset.contextSource === sourceId;
      button.classList.toggle('is-active', active);
      button.setAttribute('aria-pressed', String(active));
    });
    ui.sourceName.textContent = source.label;

    domainIds.forEach(domainId => {
      const card = cards.get(domainId);
      const group = groups.get(domainId);
      const panel = panels.get(key(sourceId, domainId));
      const image = card.querySelector('img[data-context-domain]');
      image.src = panel.svg_path;
      image.alt = makeAlt(source, group, panel);
      card.querySelector('h3').textContent = `${source.label} · ${group.label}`;
      card.querySelector('figcaption').textContent = `${source.label} · ${group.note}`;
    });

    updateTable(source);
    showDomain();
  };

  const prepare = data => {
    if (![data.sources, data.display_groups, data.panels].every(Array.isArray)) return false;
    sources = new Map(data.sources.map(source => [source.id, source]));
    groups = new Map(data.display_groups.map(group => [group.id, group]));
    panels = new Map(data.panels.map(panel => [key(panel.source_id, panel.display_group_id), panel]));
    return sourceIds.every(id => sources.has(id)) &&
      domainIds.every(id => groups.has(id)) &&
      sourceIds.every(sourceId => domainIds.every(domainId => {
        const panel = panels.get(key(sourceId, domainId));
        return panel && Array.isArray(panel.features);
      }));
  };

  const settleDirectLink = () => {
    if (location.hash !== '#context-fingerprints') return;
    const align = () => requestAnimationFrame(() => requestAnimationFrame(() => {
      section.scrollIntoView({ block: 'start' });
    }));
    const settle = () => [0, 150, 400, 900].forEach(delay => setTimeout(align, delay));
    if (document.readyState === 'complete') settle();
    else window.addEventListener('load', settle, { once: true });
  };

  const initialise = async () => {
    try {
      const response = await fetch('data/context-radials.json');
      if (!response.ok || !prepare(await response.json())) return;

      buttons.forEach(button => button.addEventListener('click', () => {
        selectSource(button.dataset.contextSource);
      }));
      ui.previous.addEventListener('click', () => {
        if (domainIndex > 0) domainIndex -= 1;
        showDomain();
      });
      ui.next.addEventListener('click', () => {
        if (domainIndex < domainIds.length - 1) domainIndex += 1;
        showDomain();
      });

      selectSource('twitter');
      section.classList.add('is-ready');
    } catch {}
  };

  settleDirectLink();
  initialise();
})();
