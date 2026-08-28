#!/usr/bin/env node

import { access, readFile, readdir, stat } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptsDir = dirname(fileURLToPath(import.meta.url));
const siteDir = join(scriptsDir, '..');
const repoDir = join(siteDir, '../../../..');

const fail = message => {
  throw new Error(message);
};

const approximately = (actual, expected, tolerance = 1e-7) => Math.abs(actual - expected) <= tolerance;
const readJson = async relativePath => JSON.parse(await readFile(join(siteDir, relativePath), 'utf8'));
const sha256 = buffer => createHash('sha256').update(buffer).digest('hex');
const parseFeatureImportanceCsv = buffer => {
  const lines = buffer.toString('utf8').trim().split(/\r?\n/);
  if (lines.shift() !== 'feature,value') fail('A radial input has an unexpected CSV schema.');
  return new Map(lines.map((line, index) => {
    const comma = line.lastIndexOf(',');
    const feature = line.slice(0, comma);
    const value = Number(line.slice(comma + 1));
    if (!feature || !Number.isFinite(value) || value < 0) fail(`Invalid radial CSV row ${index + 2}.`);
    return [feature, value];
  }));
};

const story = await readJson('data/meta-story.json');
const explorer = await readJson('data/meta-areas.json');
const sourceStory = await readJson('data/source-story.json');
const nonlinearManifest = await readJson('scripts/nonlinear_panels_manifest.json');
const nonlinearManifestRaw = await readFile(join(siteDir, 'scripts/nonlinear_panels_manifest.json'));
const nonlinearBuilder = await readFile(join(siteDir, 'scripts/build_nonlinear_panels.py'));
const radialManifest = await readJson('scripts/context_radials_manifest.json');
const radialManifestRaw = await readFile(join(siteDir, 'scripts/context_radials_manifest.json'));
const radialData = await readJson('data/context-radials.json');
const radialDataRaw = await readFile(join(siteDir, 'data/context-radials.json'));
const radialBuilder = await readFile(join(siteDir, 'scripts/build_context_radials.py'));
const radialRuntime = await readFile(join(siteDir, 'context-radials.js'), 'utf8');
const indexHtml = await readFile(join(siteDir, 'index.html'), 'utf8');
const researchHtml = await readFile(join(siteDir, 'research.html'), 'utf8');
const exploreHtml = await readFile(join(siteDir, 'explore.html'), 'utf8');
const mediaBriefHtml = await readFile(join(siteDir, 'media/media-brief.html'), 'utf8');
const mediaVisualsHtml = await readFile(join(siteDir, 'media/visuals.html'), 'utf8');
const mediaCsv = await readFile(join(siteDir, 'media/meta-local-coverage.csv'), 'utf8');
const readme = await readFile(join(siteDir, 'README.md'), 'utf8');
const appJs = await readFile(join(siteDir, 'app.js'), 'utf8');
const sourceStoryJs = await readFile(join(siteDir, 'source-story.js'), 'utf8');
const stylesCss = await readFile(join(siteDir, 'styles.css'), 'utf8');

const protectedArtifacts = {
  'app.js': ['90db24e48400e9a7323b4797d7ddb71405187dd67392307656a4a5363ea1848c', Buffer.from(appJs)],
  'source-story.js': ['78da5ec8971889566c2f30cd9dcbfb678eafcfa79ae9a033dfa32b883291af80', Buffer.from(sourceStoryJs)],
  'explore.js': ['866005c3f08beeae8be9ee644c48ee2ded1ec3bfa3f211e715fe1ee84818d1c2', await readFile(join(siteDir, 'explore.js'))],
  'data/meta-story.json': ['e34c57038a2eabbb8e59a0eb213a5279a12dcca8b3636ce04d45bc8f7137a12b', await readFile(join(siteDir, 'data/meta-story.json'))],
  'data/source-story.json': ['8d97edfb9333d8df9d3576fc6c523988b582b2272d0a6817f219c7ae2c817839', await readFile(join(siteDir, 'data/source-story.json'))],
  'scripts/build_nonlinear_panels.py': ['3ea2e23c22b2ef31631049922c92cbaa97e3cb003fd1f01ebb8784039cfda95e', nonlinearBuilder],
  'scripts/nonlinear_panels_manifest.json': ['9b536313dfb16db40d60690e984ed7661c07c07e79f0eb52b0f5dc9a0fbf7edb', nonlinearManifestRaw]
};
for (const [relativePath, [expectedHash, buffer]] of Object.entries(protectedArtifacts)) {
  if (sha256(buffer) !== expectedHash) fail(`Protected out-of-scope artifact changed: ${relativePath}.`);
}

if (story.areas.length !== 331 || story.metadata.area_count !== 331) fail('The story must contain exactly 331 areas.');
if (explorer.areas.length !== 331 || explorer.metadata.area_count !== 331) fail('The explorer must contain exactly 331 areas.');

for (const [label, areas] of [['story', story.areas], ['explorer', explorer.areas]]) {
  const codes = areas.map(area => area.code);
  if (new Set(codes).size !== 331) fail(`${label} LAD codes are not unique.`);
  areas.forEach(area => {
    for (const field of ['population', 'meta', 'rate', 'departure']) {
      if (!Number.isFinite(area[field])) fail(`${label} ${area.code} has a non-finite ${field}.`);
    }
  });
}

const expectedPair = [
  { code: 'E07000103', name: 'Watford', population: 102246, meta: 2419, rate: 2.3658627 },
  { code: 'E07000038', name: 'North East Derbyshire', population: 102001, meta: 16204, rate: 15.8861188 }
];

if (story.pair.length !== 2 || new Set(story.pair.map(area => area.code)).size !== 2) fail('The illustrative pair must contain two unique areas.');
expectedPair.forEach(expected => {
  const actual = story.pair.find(area => area.code === expected.code);
  if (!actual || actual.name !== expected.name || actual.population !== expected.population || actual.meta !== expected.meta || !approximately(actual.rate, expected.rate)) {
    fail(`Generated pair evidence does not match ${expected.name}.`);
  }
  const cardPattern = new RegExp(`data-lad-code="${expected.code}"[\\s\\S]*?data-population="${expected.population}"[\\s\\S]*?data-meta="${expected.meta}"[\\s\\S]*?data-rate="${expected.rate}"`);
  if (!cardPattern.test(indexHtml)) fail(`First-frame data attributes do not match ${expected.name}.`);
  for (const visibleValue of [expected.population.toLocaleString('en-GB'), expected.meta.toLocaleString('en-GB'), expected.rate.toFixed(2)]) {
    if (!indexHtml.includes(visibleValue)) fail(`Visible Pair copy is missing ${visibleValue}.`);
  }
});

if (!indexHtml.includes('6.7×')) fail('The visible 6.7× pair ratio is missing.');
if (!indexHtml.includes('Digital-trace counts provide a useful signal but they do not represent the local populations.')) {
  fail('The first-frame aggregate-data boundary changed unexpectedly.');
}
if (!indexHtml.includes('width="23.7" height="10"') || !indexHtml.includes('y="10" width="58.9" height="10"')) {
  fail('The first-frame waffle fills do not match the reviewed local rates.');
}
if (!approximately(story.metadata.pair_rate_ratio, 6.7147255)) fail('The generated pair ratio is incorrect.');

const maximumPopulation = Math.max(...story.areas.map(area => area.population));
const maximumMeta = Math.max(...story.areas.map(area => area.meta));
if (maximumPopulation > 1200000 || maximumMeta > 100000) fail('An observation falls outside the fixed Counts axes.');
if (!appJs.includes('xTicks: [0, 250000, 500000, 750000, 1000000, 1200000]') || !appJs.includes('yTicks: [0, 25000, 50000, 75000, 100000]')) {
  fail('The reviewed fixed Counts axes changed unexpectedly.');
}
if (!stylesCss.includes('--grey: #5f6872;') || !/\.form-error\s*\{[\s\S]*?color:\s*var\(--coral-text\)/.test(stylesCss)) {
  fail('The reviewed small-text contrast safeguards changed unexpectedly.');
}

for (const relativePath of [
  'assets/favicon.svg',
  'media/02-counts-line-up-mobile.svg',
  'media/03-local-rates-mobile.svg',
  'media/04-local-map-mobile.svg',
  'assets/fonts/RobotoCondensed-latin.woff2',
  'assets/fonts/OFL-RobotoCondensed.txt'
]) {
  await access(join(siteDir, relativePath));
}

const socialCard = await readFile(join(siteDir, 'assets/social-card.png'));
const width = socialCard.readUInt32BE(16);
const height = socialCard.readUInt32BE(20);
if (width !== 1200 || height !== 630) fail(`Social card must be 1200×630; found ${width}×${height}.`);

// Act II: source-specific local coverage, not direct observations of people.
if (
  sourceStory.metadata.area_count !== 331 ||
  sourceStory.metadata.source_count !== 4 ||
  sourceStory.metadata.switch_count !== 300 ||
  sourceStory.metadata.consistent_below_count !== 22 ||
  sourceStory.metadata.consistent_above_count !== 9
) {
  fail('The four-source area-pattern fixtures must remain 331 / 300 / 22 / 9.');
}
if (sourceStory.metadata.input_sha256 !== 'e8e6d0bc4b9bb146f713c9ce6ab882316569a79d4664a9e7e7b0dcf29804e7a8') {
  fail('The four-source story input snapshot changed without an evidence re-audit.');
}
if (sourceStory.metadata.exact_switch_predicate !== 'minimum ratio_to_fitted across the four sources < 1 AND maximum ratio_to_fitted across the four sources > 1') {
  fail('The exact cross-source switch predicate changed.');
}
if (JSON.stringify(sourceStory.metadata.source_order) !== JSON.stringify(['twitter_x', 'meta', 'multiapp1', 'multiapp2'])) {
  fail('The reviewed source order changed.');
}
if (sourceStory.areas.length !== 331 || new Set(sourceStory.areas.map(area => area.code)).size !== 331) {
  fail('The four-source story must contain 331 uniquely coded areas.');
}

const allowedAreaFields = ['code', 'name', 'pattern_class', 'side_pattern', 'switches_sides'].sort();
sourceStory.areas.forEach(area => {
  if (JSON.stringify(Object.keys(area).sort()) !== JSON.stringify(allowedAreaFields)) {
    fail(`Compact source-story area ${area.code} contains an unexpected field.`);
  }
  if (!/^[AB]{4}$/.test(area.side_pattern)) fail(`Invalid or tied side pattern for ${area.code}.`);
  const hasAbove = area.side_pattern.includes('A');
  const hasBelow = area.side_pattern.includes('B');
  const recomputedSwitch = hasAbove && hasBelow;
  const recomputedClass = recomputedSwitch ? 'switches_sides' : (hasAbove ? 'consistent_above' : 'consistent_below');
  if (area.switches_sides !== recomputedSwitch || area.pattern_class !== recomputedClass) {
    fail(`Stored source-story classification disagrees with the side pattern for ${area.code}.`);
  }
});
const patternCounts = sourceStory.areas.reduce((counts, area) => {
  counts[area.pattern_class] = (counts[area.pattern_class] || 0) + 1;
  return counts;
}, {});
if (
  sourceStory.areas.filter(area => area.switches_sides).length !== 300 ||
  patternCounts.switches_sides !== 300 ||
  patternCounts.consistent_below !== 22 ||
  patternCounts.consistent_above !== 9
) {
  fail('The compact four-source area classifications changed.');
}

const expectedSources = {
  twitter_x: {
    period: 'March 2021', rate: 0.1918051487135, r: 0.7994608855349,
    numerator: 'Inferred monthly home locations of active accounts',
    caveat: 'Unique accounts are used as a proxy for unique users; they are not verified unique residents.'
  },
  meta: {
    period: 'March 2021', rate: 8.09480363265, r: 0.9133191945598,
    numerator: 'Average nighttime active-account estimate',
    caveat: 'This is an aggregate estimate from accounts with location services and includes privacy transformations; it is not a count of verified unique residents.'
  },
  multiapp1: {
    period: 'First week of April 2021', rate: 2.377048422418, r: 0.9498728035474,
    numerator: 'Inferred home locations of qualifying observed devices',
    caveat: 'Devices proxy users, and one individual may use multiple devices; the numerator is not identical to persons.'
  },
  multiapp2: {
    period: 'November 2021', rate: 1.023041969631, r: 0.8987967012366,
    numerator: 'Inferred home locations from preprocessed multi-application GPS data',
    caveat: 'This is an analysis-ready aggregate with source-specific home inference; the numerator is not identical to persons.'
  }
};
for (const [id, expected] of Object.entries(expectedSources)) {
  const actual = sourceStory.sources.find(source => source.id === id);
  if (
    !actual ||
    actual.period !== expected.period ||
    actual.numerator_label !== expected.numerator ||
    actual.denominator_label !== '2021 Census resident population, all ages' ||
    actual.numerator_caveat !== expected.caveat ||
    !approximately(actual.fitted_rate_per_100_residents, expected.rate, 1e-10) ||
    !approximately(actual.raw_count_pearson_r, expected.r, 1e-10)
  ) {
    fail(`The reviewed ${id} source fixture changed.`);
  }
}
if (
  sourceStory.metadata.metric.formula !== '(local source count / 2021 Census resident population) / fitted through-origin source rate' ||
  sourceStory.metadata.metric.baseline !== 1 ||
  sourceStory.metadata.metric.public_name !== 'Relative local coverage index'
) {
  fail('The reviewed four-source metric definition changed.');
}

const releasedCountsBuffer = await readFile(join(repoDir, 'outputs/manuscript-data/lad-derived-population-counts.csv'));
const metaStoryBuffer = await readFile(join(siteDir, 'data/meta-story.json'));
const acceptedFigureBuffer = await readFile(join(repoDir, 'paper/rsos-debias-r1/figures/explain-bias.png'));
if (
  sha256(releasedCountsBuffer) !== sourceStory.metadata.inputs.released_lad_counts.sha256 ||
  sha256(metaStoryBuffer) !== sourceStory.metadata.inputs.lad_code_lookup.sha256 ||
  sha256(acceptedFigureBuffer) !== nonlinearManifest.source.sha256
) {
  fail('An actual source artifact no longer matches its declared evidence checksum.');
}

const expectedSourcePair = {
  E07000103: {
    name: 'Watford', population: 102246, sidePattern: 'ABAA',
    observations: {
      twitter_x: { count: 213, rate: 0.2083211079162, ratio: 1.086108007597, side: 'above' },
      meta: { count: 2419, rate: 2.365862723236, ratio: 0.2922693162924, side: 'below' },
      multiapp1: { count: 3496, rate: 3.419204663263, ratio: 1.438424489387, side: 'above' },
      multiapp2: { count: 1325, rate: 1.295894215911, ratio: 1.266706796378, side: 'above' }
    }
  },
  E07000038: {
    name: 'North East Derbyshire', population: 102001, sidePattern: 'BABA',
    observations: {
      twitter_x: { count: 95, rate: 0.09313634180057, ratio: 0.4855779024976, side: 'below' },
      meta: { count: 16204, rate: 15.886118763542, ratio: 1.962508231758, side: 'above' },
      multiapp1: { count: 2004, rate: 1.964686620719, ratio: 0.8265236005252, side: 'below' },
      multiapp2: { count: 1265, rate: 1.240183919765, ratio: 1.212251262979, side: 'above' }
    }
  }
};
for (const [code, expected] of Object.entries(expectedSourcePair)) {
  const actual = sourceStory.pair.find(area => area.code === code);
  if (!actual) fail(`Four-source pair is missing ${code}.`);
  if (actual.name !== expected.name || actual.population !== expected.population || actual.side_pattern !== expected.sidePattern) {
    fail(`Four-source pair identity changed for ${code}.`);
  }
  for (const [sourceId, fixture] of Object.entries(expected.observations)) {
    const observation = actual.observations[sourceId];
    if (
      observation.count !== fixture.count ||
      !approximately(observation.rate_per_100_residents, fixture.rate, 1e-10) ||
      !approximately(observation.ratio_to_fitted, fixture.ratio, 1e-10) ||
      observation.side_of_fitted !== fixture.side
    ) {
      fail(`Four-source pair observation changed for ${code} / ${sourceId}.`);
    }
  }
}

const sourceDataBytes = (await stat(join(siteDir, 'data/source-story.json'))).size;
const sourceScriptBytes = (await stat(join(siteDir, 'source-story.js'))).size;
if (sourceDataBytes > 50 * 1024) fail(`source-story.json exceeds 50 KB (${sourceDataBytes} bytes).`);
if (sourceScriptBytes > 16 * 1024) fail(`source-story.js exceeds 16 KB (${sourceScriptBytes} bytes).`);

if ((indexHtml.match(/class="story-step(?:\s|\")/g) || []).length !== 4) fail('Act I must retain exactly four story steps.');
if ((indexHtml.match(/class="source-step(?:\s|\")/g) || []).length !== 2) fail('Act II must contain exactly two enhanced story steps.');
if ((indexHtml.match(/data-source-step="[01]"/g) || []).length !== 2) fail('Act II must expose exactly two enhanced story states.');
if (!sourceStoryJs.includes("document.querySelector('.source-story')") || sourceStoryJs.includes("document.querySelectorAll('.story-step')")) {
  fail('Act II JavaScript must remain isolated from Act I selectors.');
}
if (!sourceStoryJs.includes("(min-width: 991px) and (min-height: 680px)") || !sourceStoryJs.includes('let loadPromise;')) {
  fail('Act II must keep the reviewed readable enhancement threshold and in-flight request guard.');
}
if (
  !stylesCss.includes('.source-story.is-ready #source-fingerprint .source-static') ||
  !/\.chapter-rail a,[\s\S]*?color:\s*var\(--grey\)/.test(stylesCss)
) {
  fail('Act II exact-value accessibility or navigation contrast safeguards changed.');
}

const sourceStart = indexHtml.indexOf('<section id="source-intro"');
const sourceEnd = indexHtml.indexOf('<section id="verdict"', sourceStart);
const mapPosition = indexHtml.indexOf('<article id="map"');
if (!(mapPosition >= 0 && sourceStart > mapPosition && sourceEnd > sourceStart)) fail('Act II must sit between the Meta map and the verdict.');
const sourceMarkup = indexHtml.slice(sourceStart, sourceEnd);
const fingerprintPosition = sourceMarkup.indexOf('id="source-fingerprint"');
const switchesPosition = sourceMarkup.indexOf('id="source-switches"');
const contextsPosition = sourceMarkup.indexOf('id="context-fingerprints"');
if (!(fingerprintPosition >= 0 && switchesPosition > fingerprintPosition && contextsPosition > switchesPosition)) {
  fail('Act II must remain ordered as pair, 300/331 and why-one-fix-fails evidence.');
}
if ((sourceMarkup.match(/id="context-fingerprints"/g) || []).length !== 1 || (sourceMarkup.match(/class="source-shape-card"/g) || []).length !== 3) {
  fail('Act II must contain one context atlas followed by exactly three nonlinear cards.');
}
for (const requiredCopy of [
  '300 of 331 local authority areas',
  '6 / 8 · Understand the influence of local attributes',
  'The same place is portrayed differently through different data.',
  'Each data source has its own fitted coverage rate.',
  'Population coverage for the same area can vary widely across data sources',
  'The places have not changed. The source has.',
  '91% of local authorities change position',
  'Understanding which populations are under- or over-represented.',
  'The radial plots below identify the most important place-based attributes',
  'Machine learning is employed to capture these non-linear relationships.',
  'Local attributes do not relate to coverage in a simple, linear way',
  'The illustrative examples below show the shape of the relationship',
  'What differs between the four sources?',
  'it does not make the sources equivalent',
  'These attributes identify the population groups which are under- or over-represented'
]) {
  if (!sourceMarkup.includes(requiredCopy)) fail(`Act II is missing its evidence boundary: ${requiredCopy}`);
}
for (const visiblePairValue of [
  '1.09× <small>9% above</small>',
  '0.49× <small>51% below</small>',
  '0.29× <small>71% below</small>',
  '1.96× <small>96% above</small>',
  '1.44× <small>44% above</small>',
  '0.83× <small>17% below</small>',
  '1.27× <small>27% above</small>',
  '1.21× <small>21% above</small>'
]) {
  if (!sourceMarkup.includes(visiblePairValue)) fail(`The static four-source table changed: ${visiblePairValue}`);
}
const shapesMarkup = sourceMarkup.slice(contextsPosition);
for (const prohibitedShapeCopy of [
  'increase bias',
  'decrease bias',
  'overrepresented',
  'underrepresented',
  'causes bias',
  'drives bias'
]) {
  if (shapesMarkup.toLowerCase().includes(prohibitedShapeCopy)) fail(`The nonlinear cards contain unsupported directional or causal wording: ${prohibitedShapeCopy}`);
}
for (const prohibitedCopy of [
  'Twitter represents young adults',
  'Meta represents urban people',
  'which groups are represented',
  'population density causes'
]) {
  if (sourceMarkup.toLowerCase().includes(prohibitedCopy.toLowerCase())) fail(`Act II contains unsafe wording: ${prohibitedCopy}`);
}

const unitGrid = sourceMarkup.match(/<div class="source-unit-grid source-unit-grid-fallback"[\s\S]*?<\/div>/)?.[0] || '';
if ((unitGrid.match(/<i(?:\s|>)/g) || []).length !== 331) fail('The static source-switch grid must contain exactly 331 marks.');

const expectedPanelFixtures = {
  'twitter-age-20-29': {
    source_plot_rect: [0, 2926, 940, 3410], source_label: 'Twitter/X',
    feature_display_label: 'Local authority share aged 20–29', context_label: 'Demographic context',
    shape_label: 'S-shape', output_stems: { media: 'twitter-age-20-29', story: 'twitter-age-20-29-plot' }
  },
  'meta-population-density': {
    source_plot_rect: [1895, 2950, 2835, 3435], source_label: 'Meta',
    feature_display_label: 'Population density', context_label: 'Geographic context',
    shape_label: 'Curved reversal', output_stems: { media: 'meta-population-density', story: 'meta-population-density-plot' }
  },
  'multiapp1-level4-qualifications': {
    source_plot_rect: [3795, 2950, 4725, 3470], source_label: 'Multi-app1',
    feature_display_label: 'Local authority share with Level 4 qualifications', context_label: 'Socioeconomic context',
    shape_label: 'Threshold', output_stems: { media: 'multiapp1-level4-qualifications', story: 'multiapp1-level4-qualifications-plot' },
    blank_plot_rects: [[190, 0, 930, 45]]
  }
};
const expectedVariants = {
  media: { dimensions: [900, 720], plotBounds: [24, 142, 876, 620] },
  story: { dimensions: [900, 520], plotBounds: [10, 8, 890, 512] }
};
if (
  nonlinearManifest.schema_version !== 2 ||
  nonlinearManifest.composition.font_path_from_site_root !== 'assets/fonts/RobotoCondensed-latin.woff2' ||
  nonlinearManifest.composition.palette.navy !== '#082b55' ||
  nonlinearManifest.composition.palette.teal !== '#078481' ||
  nonlinearManifest.composition.palette.coral !== '#f2553d' ||
  !nonlinearManifest.method.includes('No data are digitised') ||
  !nonlinearManifest.method.includes('no observations, axes, tick labels or fitted curves are redrawn') ||
  !nonlinearManifest.method.includes('horizontal position remains authoritative') ||
  sha256(nonlinearBuilder) !== '3ea2e23c22b2ef31631049922c92cbaa97e3cb003fd1f01ebb8784039cfda95e' ||
  sha256(nonlinearManifestRaw) !== '9b536313dfb16db40d60690e984ed7661c07c07e79f0eb52b0f5dc9a0fbf7edb'
) {
  fail('The reviewed nonlinear builder, manifest or provenance boundary changed.');
}
for (const [variant, fixture] of Object.entries(expectedVariants)) {
  const actual = nonlinearManifest.composition.variants[variant];
  if (
    JSON.stringify(actual.canvas_dimensions_px) !== JSON.stringify(fixture.dimensions) ||
    JSON.stringify(actual.plot_bounds_px) !== JSON.stringify(fixture.plotBounds)
  ) fail(`The nonlinear ${variant} composition changed without review.`);
}
let nonlinearBytes = 0;
for (const [panelId, fixture] of Object.entries(expectedPanelFixtures)) {
  const manifestPanel = nonlinearManifest.panels.find(panel => panel.id === panelId);
  if (!manifestPanel) fail(`The nonlinear manifest is missing ${panelId}.`);
  for (const field of ['source_plot_rect', 'source_label', 'feature_display_label', 'context_label', 'shape_label', 'output_stems']) {
    if (JSON.stringify(manifestPanel[field]) !== JSON.stringify(fixture[field])) {
      fail(`The reviewed ${panelId} ${field} fixture changed.`);
    }
  }
  if (JSON.stringify(manifestPanel.blank_plot_rects) !== JSON.stringify(fixture.blank_plot_rects)) {
    fail(`The reviewed ${panelId} title-mask fixture changed.`);
  }
  if (panelId === 'multiapp1-level4-qualifications' && !manifestPanel.blank_plot_purpose.includes('no empirical mark')) {
    fail('The Multi-app1 duplicate-title mask is not transparently documented.');
  }
  for (const [variant, variantFixture] of Object.entries(expectedVariants)) {
    const stem = fixture.output_stems[variant];
    for (const extension of ['png', 'webp']) {
      const relativePath = `media/nonlinear/${stem}.${extension}`;
      await access(join(siteDir, relativePath));
      const fileStats = await stat(join(siteDir, relativePath));
      const fileBuffer = await readFile(join(siteDir, relativePath));
      const expectedOutput = manifestPanel.expected_outputs[variant][extension];
      nonlinearBytes += fileStats.size;
      if (fileStats.size > 120 * 1024) fail(`${relativePath} exceeds 120 KB.`);
      if (
        expectedOutput.path !== relativePath ||
        expectedOutput.bytes !== fileStats.size ||
        expectedOutput.sha256 !== sha256(fileBuffer)
      ) fail(`The manifest-locked ${relativePath} output changed.`);
    }
    const png = await readFile(join(siteDir, `media/nonlinear/${stem}.png`));
    const pngWidth = png.readUInt32BE(16);
    const pngHeight = png.readUInt32BE(20);
    if (pngWidth !== variantFixture.dimensions[0] || pngHeight !== variantFixture.dimensions[1]) {
      fail(`${stem}.png must be ${variantFixture.dimensions.join('×')}; found ${pngWidth}×${pngHeight}.`);
    }
    const targetHtml = variant === 'story' ? indexHtml : mediaVisualsHtml;
    if (
      !targetHtml.includes(`media/nonlinear/${stem}.png`.replace(/^media\//, variant === 'media' ? '../media/' : 'media/')) ||
      !targetHtml.includes(`width="${variantFixture.dimensions[0]}" height="${variantFixture.dimensions[1]}"`)
    ) fail(`The ${variant} HTML asset or dimensions are missing for ${stem}.`);
  }
}
if (nonlinearBytes > 512 * 1024) fail(`Nonlinear assets exceed 512 KB in total (${nonlinearBytes} bytes).`);
if (nonlinearManifest.source.sha256 !== 'efed2f79f46b5f386f013f4fceffa5536795aa4c5aa5a7cccade6cbd504fcc9c') {
  fail('The accepted nonlinear source figure changed without a crop re-audit.');
}
if (indexHtml.includes('rsos-debias-r1/figures/explain-bias.png')) fail('The full accepted figure must not be shipped in the story.');

// Option B: site-native profiles from the revised R1 main-model outputs.
const expectedRadialSources = [
  ['twitter', 'Twitter/X', 'twitter', 'outputs/preliminary/twitter/explain-bias/feature-importance-data_lad_holdout_nolagcov.csv', '7db31f979773b05063100802fe706de075bdd87434b4e2e97a496ef5e68d27a7'],
  ['meta', 'Meta', 'fb_tts', 'outputs/preliminary/fb_tts/explain-bias/feature-importance-data_lad_holdout_nolagcov.csv', '30ff49b8aabcb30bf02d9f74fad2d2d8885050c758a269d91c8861ecb9227d25'],
  ['multiapp1', 'Multi-app1', 'mapp1', 'outputs/preliminary/mapp1/explain-bias/feature-importance-data_lad_holdout_nolagcov.csv', '0e4af9a1dd60f2c25f4bb048389fe80d53fd567c40cec88ee7a4fc0a965ca643'],
  ['multiapp2', 'Multi-app2', 'mapp2', 'outputs/preliminary/mapp2/explain-bias/feature-importance-data_lad_holdout_nolagcov.csv', '20bee26444776703c401f2b953e9c3be6ec67e9c8e4e0dd619f860ad16f9dae4']
];
const expectedRadialGroups = [
  ['demographic', 10],
  ['socioeconomic', 11],
  ['resource_access', 4],
  ['mobility_geography', 5]
];
const radialCsvBySource = new Map();
if (
  radialManifest.schema_version !== 1 ||
  radialManifest.artifact_id !== 'context-radials-r1-main' ||
  JSON.stringify(radialManifest.source_order) !== JSON.stringify(expectedRadialSources.map(source => source[0])) ||
  JSON.stringify(radialManifest.display_group_order) !== JSON.stringify(expectedRadialGroups.map(group => group[0])) ||
  radialManifest.normalization.method !== 'within_source_min_max' ||
  radialManifest.normalization.scope !== 'all_30_features' ||
  radialManifest.normalization.formula !== '(raw_mean_abs_shap - source_min) / (source_max - source_min)' ||
  radialManifest.axis_order.method !== 'descending_four_source_mean_raw_importance_within_display_group' ||
  radialManifest.axis_order.tie_break !== 'metadata_order' ||
  radialManifest.label_threshold.operator !== '>' ||
  radialManifest.label_threshold.value !== 0.5 ||
  radialManifest.outputs.panel_count !== 16 ||
  radialManifest.outputs.svg_width !== 600 ||
  radialManifest.outputs.svg_height !== 560
) fail('The revised-model radial manifest contract changed without review.');

for (const [sourceId, label, datasetKey, inputPath, inputSha] of expectedRadialSources) {
  const manifestInput = radialManifest.inputs.find(input => input.source_id === sourceId);
  const dataSource = radialData.sources.find(source => source.id === sourceId);
  const inputBuffer = await readFile(join(repoDir, inputPath));
  const csvValues = parseFeatureImportanceCsv(inputBuffer);
  radialCsvBySource.set(sourceId, csvValues);
  if (
    !manifestInput || manifestInput.label !== label || manifestInput.dataset_key !== datasetKey ||
    manifestInput.path !== inputPath || manifestInput.sha256 !== inputSha || sha256(inputBuffer) !== inputSha ||
    !dataSource || dataSource.label !== label || dataSource.dataset_key !== datasetKey ||
    dataSource.input_path !== inputPath || dataSource.input_sha256 !== inputSha || dataSource.feature_count !== 30 ||
    csvValues.size !== 30
  ) fail(`The approved Option B input changed for ${sourceId}.`);
}
if (
  radialData.schema_version !== 1 ||
  radialData.artifact_type !== 'new_site_derived_analysis' ||
  radialData.metadata.unit_of_analysis !== '2021 local authority district in England and Wales' ||
  radialData.metadata.model_specification !== 'XGBoost; random holdout; no lagged covariates' ||
  radialData.metadata.feature_count !== 30 || radialData.metadata.panel_count !== 16 ||
  radialData.metadata.accepted_figure_match !== false ||
  !radialData.metadata.accepted_figure_note.includes('do not exactly reproduce') ||
  radialData.normalization.zero_meaning !== "Lowest-scoring feature among the source's 30 features; not no effect." ||
  !radialData.axis_order.interpretation.includes('Layout only')
) fail('The public radial data lost its revised-model provenance or interpretation boundary.');

const radialBoundaryText = radialData.evidence_boundaries.join(' ');
for (const boundary of [
  'do not measure the composition or inclusion rate of individual users or population groups',
  'magnitude, not the direction of an association',
  'Resource accessibility features are Census household proxies',
  'do not exactly reproduce the accepted paper\'s radial figure',
  'not an additional importance estimate'
]) {
  if (!radialBoundaryText.includes(boundary)) fail(`The radial evidence boundary changed: ${boundary}`);
}

const featureIdsBySource = new Map();
let radialBytes = 0;
for (const [groupId, featureCount] of expectedRadialGroups) {
  const group = radialData.display_groups.find(candidate => candidate.id === groupId);
  if (!group || group.feature_count !== featureCount || group.axis_feature_ids.length !== featureCount) {
    fail(`The reviewed radial display-group fixture changed for ${groupId}.`);
  }
  if (groupId === 'mobility_geography' && JSON.stringify(group.model_domains) !== JSON.stringify(['mobility', 'geographic'])) {
    fail('The final radial display group must continue to span mobility and geographic model domains.');
  }
}
for (const [sourceId] of expectedRadialSources) {
  const dataSource = radialData.sources.find(source => source.id === sourceId);
  const csvValues = radialCsvBySource.get(sourceId);
  const sourcePanels = radialData.panels.filter(panel => panel.source_id === sourceId);
  if (sourcePanels.length !== 4) fail(`${sourceId} must have four radial display groups.`);
  const sourceFeatures = sourcePanels.flatMap(panel => panel.features);
  if (sourceFeatures.length !== 30 || new Set(sourceFeatures.map(feature => feature.id)).size !== 30) {
    fail(`${sourceId} must contain the same 30 unique area features.`);
  }
  featureIdsBySource.set(sourceId, sourceFeatures.map(feature => feature.id).sort());
  const sourceMinimum = Math.min(...sourceFeatures.map(feature => feature.raw_mean_abs_shap));
  const sourceMaximum = Math.max(...sourceFeatures.map(feature => feature.raw_mean_abs_shap));
  if (!(Number.isFinite(sourceMinimum) && Number.isFinite(sourceMaximum) && sourceMaximum > sourceMinimum)) {
    fail(`${sourceId} has an invalid raw feature-importance range.`);
  }
  for (const feature of sourceFeatures) {
    const recomputed = (feature.raw_mean_abs_shap - sourceMinimum) / (sourceMaximum - sourceMinimum);
    const recomputedRank = 1 + [...csvValues.values()].filter(value => value > feature.raw_mean_abs_shap).length;
    if (
      !Number.isFinite(feature.raw_mean_abs_shap) || feature.raw_mean_abs_shap < 0 ||
      !csvValues.has(feature.id) || !approximately(feature.raw_mean_abs_shap, csvValues.get(feature.id), 1e-15) ||
      !Number.isFinite(feature.normalised_importance) || feature.normalised_importance < 0 || feature.normalised_importance > 1 ||
      !approximately(feature.normalised_importance, recomputed, 1e-11) ||
      feature.is_labelled !== (feature.normalised_importance > 0.5) || feature.source_rank !== recomputedRank
    ) fail(`The radial value contract failed for ${sourceId} / ${feature.id}.`);
  }
  if (!approximately(Math.min(...sourceFeatures.map(feature => feature.normalised_importance)), 0) || !approximately(Math.max(...sourceFeatures.map(feature => feature.normalised_importance)), 1)) {
    fail(`${sourceId} radial normalisation must span exactly 0–1.`);
  }
  const recomputedTopIds = [...csvValues.entries()].sort((a, b) => b[1] - a[1]).slice(0, 5).map(([id]) => id);
  if (JSON.stringify(dataSource.top_feature_ids) !== JSON.stringify(recomputedTopIds)) {
    fail(`${sourceId} top-feature fixture disagrees with its approved CSV.`);
  }
}
const referenceFeatureIds = featureIdsBySource.get('twitter');
for (const [sourceId, ids] of featureIdsBySource) {
  if (JSON.stringify(ids) !== JSON.stringify(referenceFeatureIds)) fail(`${sourceId} does not share the reviewed 30-feature set.`);
}

const twitterFeatures = radialData.panels.filter(panel => panel.source_id === 'twitter').flatMap(panel => panel.features);
const metadataOrder = new Map(twitterFeatures.map(feature => [feature.id, feature.metadata_order]));
const meanRawByFeature = new Map(referenceFeatureIds.map(featureId => [
  featureId,
  expectedRadialSources.reduce((total, [sourceId]) => total + radialCsvBySource.get(sourceId).get(featureId), 0) / expectedRadialSources.length
]));
for (const [groupId] of expectedRadialGroups) {
  const group = radialData.display_groups.find(candidate => candidate.id === groupId);
  const recomputedAxisIds = [...group.axis_feature_ids].sort((a, b) =>
    meanRawByFeature.get(b) - meanRawByFeature.get(a) || metadataOrder.get(a) - metadataOrder.get(b)
  );
  if (JSON.stringify(group.axis_feature_ids) !== JSON.stringify(recomputedAxisIds)) {
    fail(`${groupId} axis order disagrees with the reviewed QMD layout rule.`);
  }
}

const expectedSvgOutputs = new Map(radialManifest.expected_svg_outputs.map(output => [output.path, output]));
if (expectedSvgOutputs.size !== 16 || radialManifest.expected_svg_outputs.length !== 16) {
  fail('The radial manifest must pin exactly 16 unique SVG outputs.');
}
const actualRadialFiles = (await readdir(join(siteDir, 'media/radial-generated'))).filter(name => name.endsWith('.svg')).sort();
const expectedRadialFiles = [...expectedSvgOutputs.keys()].map(path => path.split('/').at(-1)).sort();
if (JSON.stringify(actualRadialFiles) !== JSON.stringify(expectedRadialFiles)) {
  fail('The generated radial directory does not match the reviewed 16-file inventory.');
}
for (const [sourceId] of expectedRadialSources) {
  for (const [groupId, featureCount] of expectedRadialGroups) {
    const panelId = `${sourceId}-${groupId}`;
    const panel = radialData.panels.find(candidate => candidate.id === panelId);
    if (!panel || panel.feature_count !== featureCount || panel.svg_path !== `media/radial-generated/${panelId}.svg`) {
      fail(`The radial Cartesian panel fixture changed for ${panelId}.`);
    }
    const group = radialData.display_groups.find(candidate => candidate.id === groupId);
    if (JSON.stringify(panel.features.map(feature => feature.id)) !== JSON.stringify(group.axis_feature_ids)) {
      fail(`${panelId} feature order disagrees with its display-group axis order.`);
    }
    panel.features.forEach((feature, index) => {
      if (feature.axis_order !== index + 1) fail(`${panelId} contains a stale feature axis position.`);
    });
    const svgBuffer = await readFile(join(siteDir, panel.svg_path));
    const svg = svgBuffer.toString('utf8');
    const expectedSvg = expectedSvgOutputs.get(panel.svg_path);
    radialBytes += svgBuffer.length;
    if (
      panel.svg_sha256 !== sha256(svgBuffer) || panel.svg_bytes !== svgBuffer.length || svgBuffer.length > 25 * 1024 ||
      !expectedSvg || expectedSvg.sha256 !== sha256(svgBuffer) || expectedSvg.bytes !== svgBuffer.length ||
      !svg.includes('viewBox="0 0 600 560"') || !svg.includes('width="600" height="560"') ||
      !svg.includes(`id="title-${panelId}"`) || !svg.includes(`id="desc-${panelId}"`) ||
      !svg.includes('role="img"') || !svg.includes('within-source relative mean absolute SHAP importance') ||
      !svg.includes('font-family="Roboto Condensed, Arial, sans-serif"') || !svg.includes('stroke="#078481"')
    ) fail(`The deterministic accessible SVG contract failed for ${panelId}.`);
  }
}
if (radialBytes > 240 * 1024 || radialDataRaw.length > 50 * 1024 || Buffer.byteLength(radialRuntime) > 8 * 1024) {
  fail('The radial SVG, data or isolated source-switch runtime exceeds its reviewed budget.');
}
if (
  radialData.provenance.generator_sha256 !== sha256(radialBuilder) ||
  radialData.provenance.manifest_sha256 !== sha256(radialManifestRaw) ||
  sha256(radialBuilder) !== '7ac6e110af54558485dc517ccdb4ca23e11475fe3b97ccd1007f74cbdd24f682' ||
  sha256(radialManifestRaw) !== '435f0a1ce08b5a3de9feb57583c199d82f02d00055d34c237645a16f417749e4' ||
  sha256(Buffer.from(radialRuntime)) !== '6df418f92b4b938b5c34066a56bd57ff4e0a72dfc15600fba468b75e45d63a47'
) fail('The radial data provenance hashes do not match the actual builder and manifest.');

for (const requiredCopy of [
  'Coverage rates vary systematically with local population attributes.',
  'These attributes identify the population groups which are under- or over-represented',
  'The radial plots below identify the most important place-based attributes',
  'We find that the associations between local attributes and population coverage rates can flatten, reverse direction or change shape',
  'Machine learning is employed to capture these non-linear relationships.'
]) {
  if (!sourceMarkup.includes(requiredCopy)) fail(`The Act II radial atlas is missing its evidence boundary: ${requiredCopy}`);
}
if (
  (sourceMarkup.match(/data-context-source=/g) || []).length !== 4 ||
  (sourceMarkup.match(/data-context-domain=/g) || []).length !== 4 ||
  (sourceMarkup.match(/data-context-card=/g) || []).length !== 4 ||
  !indexHtml.includes('context-radials.js') || researchHtml.includes('context-radials.js') ||
  !sourceMarkup.includes('class="context-current" aria-live="polite"') ||
  (researchHtml.match(/data-context-source=/g) || []).length !== 0 ||
  !researchHtml.includes('four pinned released R1 main-model feature-importance files') ||
  !researchHtml.includes('does not reproduce the accepted radial figure') ||
  !researchHtml.includes('The paper is the source for published figures; the pinned R1 files are the source for this atlas.') ||
  !researchHtml.includes('Radius reports relative mean absolute SHAP importance within each source.') ||
  !researchHtml.includes('The features describe local authorities, not the people using each dataset.') ||
  !researchHtml.includes('Coverage represents numbers (i.e. a quantity). Representativeness is a relationship to the target population.') ||
  !researchHtml.includes('Individual points in the nonlinear panels represent local authorities.') ||
  !researchHtml.includes('index.html#context-fingerprints') ||
  /which populations are represented|demographic composition of (?:the )?users/i.test(sourceMarkup + researchHtml)
) fail('The canonical Act II radial atlas is duplicated, incomplete or overclaims individual representation.');
if (!sourceMarkup.includes('Swipe horizontally to see raw values and ranks.')) {
  fail('The exact-value table is missing its mobile horizontal-scroll cue.');
}
if (!sourceMarkup.includes('<noscript>') || !sourceMarkup.includes('twitter-demographic.svg')) {
  fail('The Act II radial atlas is missing its reviewed no-JavaScript evidence fallback.');
}
const fallbackBody = sourceMarkup.match(/<tbody id="context-feature-table-body">([\s\S]*?)<\/tbody>/)?.[1] || '';
const fallbackRows = [...fallbackBody.matchAll(/<tr(?: class="is-labelled")?>([\s\S]*?)<\/tr>/g)].map(match =>
  [...match[1].matchAll(/<td data-label="[^"]+">([^<]*)<\/td>/g)].map(cell => cell[1])
);
const expectedFallbackFeatures = radialData.panels.filter(panel => panel.source_id === 'twitter').flatMap(panel => panel.features);
if (fallbackRows.length !== 30 || expectedFallbackFeatures.some((feature, index) => {
  const row = fallbackRows[index];
  return !row || row.length !== 5 || row[1] !== feature.label || row[2] !== String(feature.raw_mean_abs_shap) ||
    row[3] !== String(feature.normalised_importance) || row[4] !== String(feature.source_rank);
})) fail('The semantic Twitter/X fallback table disagrees with the reviewed radial payload.');
if (!/\.context-source-controls\s*\{[\s\S]*?display:\s*none;/.test(stylesCss) || !/\.context-fingerprints\.is-ready \.context-source-controls\s*\{[\s\S]*?display:\s*(?:flex|grid);/.test(stylesCss)) {
  fail('Radial controls must remain hidden until the data and listeners are ready.');
}
const nonlinearOrder = [
  sourceMarkup.indexOf('meta-population-density-plot.webp'),
  sourceMarkup.indexOf('twitter-age-20-29-plot.webp'),
  sourceMarkup.indexOf('multiapp1-level4-qualifications-plot.webp')
];
if (!(nonlinearOrder[0] >= 0 && nonlinearOrder[1] > nonlinearOrder[0] && nonlinearOrder[2] > nonlinearOrder[1])) {
  fail('The accepted nonlinear examples must read as curved reversal, S-shape and threshold.');
}
for (const shape of ['Curved', 'S-Shape', 'Threshold']) {
  if (!sourceMarkup.includes(shape)) fail(`The nonlinear hierarchy is missing ${shape}.`);
}
for (const forbidden of ['Multi-app1 · Rural', 'Lower supervisory (%)', 'Level 4 qualifications · S-shape']) {
  if (sourceMarkup.includes(forbidden)) fail(`The live story reintroduced a slide-only conflicting example: ${forbidden}.`);
}

const combinedHtml = [indexHtml, researchHtml, exploreHtml, mediaBriefHtml, mediaVisualsHtml].join('\n');
const articleTitle = 'Making hidden biases visible in population location data from mobile phones';
const articleDoi = '10.1098/rsos.251703';
const articleUrl = `https://doi.org/${articleDoi}`;
const storyUrl = 'https://de-bias.github.io/debias/stories/making-hidden-biases-visible/';
for (const [label, html] of [['story', indexHtml], ['research', researchHtml], ['media brief', mediaBriefHtml]]) {
  if (!html.includes(articleTitle) || !html.includes(articleUrl)) {
    fail(`${label} is missing the exact published article title or DOI URL.`);
  }
}
if (!indexHtml.includes(`rel="canonical" href="${storyUrl}"`) || !indexHtml.includes(`property="og:url" content="${storyUrl}"`)) {
  fail('The story canonical or Open Graph URL is not the reviewed public route.');
}
for (const [label, html] of [['story', indexHtml], ['research', researchHtml]]) {
  if (
    (html.match(/class="altmetric-embed"/g) || []).length !== 1 ||
    (html.match(/data-doi="10\.1098\/rsos\.251703"/g) || []).length !== 1 ||
    (html.match(/d1bxh8uas1mnw7\.cloudfront\.net\/assets\/embed\.js/g) || []).length !== 1
  ) fail(`${label} must contain one official Altmetric badge for the complete published DOI.`);
}
if (/data-doi=""|data-doi="10\.1098\/rsos\."/.test(combinedHtml) || combinedHtml.includes('Altmetric attention badge will appear here when the final')) {
  fail('A blank DOI or prepublication Altmetric placeholder remains.');
}
if (
  !researchHtml.includes('The reported coverage rate and SHAP visualisations answer related but different questions') ||
  !researchHtml.includes('Individual points in the nonlinear panels represent local authorities')
) {
  fail('The Research page is missing the fitted-rate / SHAP interpretation bridge.');
}
if (
  !mediaBriefHtml.includes('<strong>300 of 331</strong>') ||
  !mediaBriefHtml.includes('not demographic profiles of observed users') ||
  !mediaBriefHtml.includes('reporting 300 of 331, or 91%, as a share of residents or people represented') ||
  !mediaBriefHtml.includes('same-period, same-unit samples') ||
  !mediaBriefHtml.includes('../index.html#context-fingerprints')
) {
  fail('The media brief is missing the reviewed Act II result or reporting boundary.');
}
for (const [label, html, favicon] of [
  ['story', indexHtml, 'assets/favicon.svg'],
  ['research', researchHtml, 'assets/favicon.svg'],
  ['explorer', exploreHtml, 'assets/favicon.svg'],
  ['media brief', mediaBriefHtml, '../assets/favicon.svg'],
  ['media visuals', mediaVisualsHtml, '../assets/favicon.svg']
]) {
  if (!html.includes(`rel="icon" href="${favicon}"`) || !html.includes('<main id="main">')) {
    fail(`${label} is missing its favicon or main landmark.`);
  }
}
for (const [label, html] of [['media brief', mediaBriefHtml], ['media visuals', mediaVisualsHtml]]) {
  if (!html.includes('class="skip-link"') || !html.includes('<nav aria-label="Primary navigation">')) {
    fail(`${label} is missing its reviewed navigation accessibility.`);
  }
}
if (
  (mediaVisualsHtml.match(/<figure(?:\s|>)/g) || []).length !== 10 ||
  (mediaVisualsHtml.match(/Branded crop from the accepted SHAP figure/g) || []).length !== 3 ||
  !/\.visual-downloads img\s*\{[\s\S]*?display:\s*block;[\s\S]*?width:\s*100%;[\s\S]*?height:\s*auto;/.test(stylesCss)
) {
  fail('The media page must contain ten figures including two cross-source proofs and three area-level model panels.');
}
for (const relativePath of ['media/05-four-source-fingerprint.svg', 'media/06-cross-source-switches.svg']) {
  const svg = await readFile(join(siteDir, relativePath), 'utf8');
  if (
    !svg.includes('Cabrera &amp; Rowe / DEBIAS') ||
    (!svg.includes('Not a representativeness score') && !svg.includes('not the percentage of people represented')) ||
    !svg.toLowerCase().includes('source numerators') ||
    !svg.toLowerCase().includes('dates differ')
  ) {
    fail(`${relativePath} is missing its credit or interpretation boundary.`);
  }
}
const expectedCsvHeader = '"lad_code","local_authority","census_population_2021","meta_average_nighttime_active_account_estimate_march_2021","meta_estimates_per_100_census_residents","difference_from_fitted_meta_estimate_rate_per_100"';
if (mediaCsv.split(/\r?\n/, 1)[0] !== expectedCsvHeader) fail('The public Meta CSV uses unsafe or stale field names.');
for (const requiredDocumentation of [
  'build_source_story_data.R',
  'build_nonlinear_panels.py',
  'build_context_radials.py',
  'build_source_story_media.mjs',
  '300 of 331 authorities',
  'not a reproduction of the accepted paper radial figure',
  '10.1098/rsos.251703',
  'Canonical story URL'
]) {
  if (!readme.includes(requiredDocumentation)) fail(`README is missing release guidance: ${requiredDocumentation}`);
}

console.log(
  'Site evidence contract passed: Act I exact pair and axes; Act II 331 / 300 / 22 / 9 fixtures, exact four-source pair, revised-model 30×4 radial inputs and safe nonlinear language; isolated progressive enhancement, compact payloads and deterministic media; published DOI, Altmetric badges, canonical URLs, mobile assets, contrast and 1200×630 social card.'
);
