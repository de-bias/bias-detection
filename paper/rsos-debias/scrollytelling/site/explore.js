(() => {
  'use strict';

  const form = document.querySelector('#area-form');
  if (!form) return;

  const input = document.querySelector('#area-search');
  const datalist = document.querySelector('#area-options');
  const error = document.querySelector('#area-error');
  const result = document.querySelector('#area-result');
  const copyButton = document.querySelector('#copy-area-link');
  const formatInteger = new Intl.NumberFormat('en-GB', { maximumFractionDigits: 0 });
  const formatRate = new Intl.NumberFormat('en-GB', { minimumFractionDigits: 2, maximumFractionDigits: 2 });

  let areas = [];
  let metadata;
  let selectedArea;

  const normalise = value => value.trim().toLocaleLowerCase('en-GB');

  const updateAddress = area => {
    const address = new URL(window.location.href);
    address.searchParams.set('area', area.code);
    window.history.replaceState({}, '', address);
  };

  const showArea = (area, updateUrl = true) => {
    selectedArea = area;
    document.querySelector('#result-name').textContent = area.name;
    document.querySelector('#result-population').textContent = formatInteger.format(area.population);
    document.querySelector('#result-meta').textContent = formatInteger.format(area.meta);
    document.querySelector('#result-rate').textContent = formatRate.format(area.rate);

    const difference = Math.abs(area.departure);
    const direction = area.departure < 0 ? 'fewer' : 'more';
    document.querySelector('#result-direction').textContent = `${formatRate.format(difference)} ${direction} active-account estimates per 100 residents than the fitted Meta rate of ${formatRate.format(metadata.alpha_per_100)}.`;

    const markerPosition = 50 + (area.departure / metadata.departure_limit) * 48;
    document.querySelector('#gauge-marker').style.left = `${Math.max(2, Math.min(98, markerPosition))}%`;
    input.value = area.name;
    result.hidden = false;
    copyButton.hidden = false;
    copyButton.textContent = 'Copy this area link';
    error.textContent = '';
    if (updateUrl) updateAddress(area);
  };

  const findArea = value => {
    const query = normalise(value);
    const exact = areas.find(area => normalise(area.name) === query);
    const partialMatches = areas.filter(area => normalise(area.name).includes(query));
    return exact || (partialMatches.length === 1 ? partialMatches[0] : null);
  };

  form.addEventListener('submit', event => {
    event.preventDefault();
    const area = findArea(input.value);
    if (!area) {
      error.textContent = input.value.trim()
        ? 'Choose one local authority from the suggested list.'
        : 'Enter a local authority name.';
      result.hidden = true;
      copyButton.hidden = true;
      return;
    }
    showArea(area);
  });

  copyButton.addEventListener('click', async () => {
    if (!selectedArea) return;
    updateAddress(selectedArea);
    try {
      await navigator.clipboard.writeText(window.location.href);
      copyButton.textContent = 'Area link copied';
    } catch {
      copyButton.textContent = 'Copy the address from your browser';
    }
  });

  fetch('data/meta-areas.json')
    .then(response => {
      if (!response.ok) throw new Error(`Area data request failed: ${response.status}`);
      return response.json();
    })
    .then(data => {
      if (!data.metadata || !Array.isArray(data.areas) || data.areas.length !== 331) {
        throw new Error('Area data did not pass the 331-area validation.');
      }
      metadata = data.metadata;
      areas = data.areas.slice().sort((a, b) => a.name.localeCompare(b.name, 'en-GB'));
      areas.forEach(area => {
        const option = document.createElement('option');
        option.value = area.name;
        datalist.append(option);
      });

      const requestedCode = new URL(window.location.href).searchParams.get('area');
      const requestedArea = areas.find(area => area.code === requestedCode);
      const defaultArea = requestedArea || areas.find(area => area.name === 'Liverpool');
      if (defaultArea) {
        showArea(defaultArea, Boolean(requestedArea));
        if (requestedCode && !requestedArea) {
          const canonicalAddress = new URL(window.location.href);
          canonicalAddress.searchParams.delete('area');
          window.history.replaceState({}, '', canonicalAddress);
          error.textContent = 'That area link was not recognised; showing Liverpool instead.';
        }
      }
    })
    .catch(loadError => {
      console.error(loadError);
      error.textContent = 'The explorer could not load. Download the complete CSV below.';
      input.disabled = true;
      form.querySelector('button').disabled = true;
    });
})();
