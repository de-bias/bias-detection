#!/usr/bin/env node

import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptsDir = dirname(fileURLToPath(import.meta.url));
const siteDir = join(scriptsDir, '..');
const mediaDir = join(siteDir, 'media');
const payload = JSON.parse(await readFile(join(siteDir, 'data/source-story.json'), 'utf8'));

if (
  payload.metadata.area_count !== 331 ||
  payload.metadata.switch_count !== 300 ||
  payload.pair?.length !== 2 ||
  payload.sources?.length !== 4
) {
  throw new Error('The reviewed four-source evidence fixtures changed.');
}

const colours = {
  paper: '#fbfaf6',
  white: '#ffffff',
  navy: '#082b55',
  navySoft: '#304b68',
  teal: '#078481',
  coral: '#f2553d',
  grey: '#5f6872',
  line: '#cfd2d1'
};
const font = "'Arial Narrow', 'Roboto Condensed', Arial, sans-serif";

const xml = value => String(value)
  .replaceAll('&', '&amp;')
  .replaceAll('<', '&lt;')
  .replaceAll('>', '&gt;')
  .replaceAll('"', '&quot;');

const lineText = (lines, x, y, options = {}) => {
  const {
    size = 24,
    weight = 700,
    fill = colours.navy,
    lineHeight = Math.round(size * 1.08),
    anchor = 'start',
    letterSpacing = 0
  } = options;
  const spans = lines.map((line, index) =>
    `<tspan x="${x}" dy="${index === 0 ? 0 : lineHeight}">${xml(line)}</tspan>`
  ).join('');
  return `<text x="${x}" y="${y}" fill="${fill}" font-family="${font}" font-size="${size}" font-weight="${weight}" letter-spacing="${letterSpacing}" text-anchor="${anchor}">${spans}</text>`;
};

const shell = (title, description, body) => `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="800" viewBox="0 0 1200 800" role="img" aria-labelledby="title description">
  <title id="title">${xml(title)}</title>
  <desc id="description">${xml(description)}</desc>
  <rect width="1200" height="800" fill="${colours.paper}"/>
  <style>
    text { font-family: ${font}; }
    .caps { font-weight: 900; letter-spacing: 1.2px; }
  </style>
  ${body}
</svg>
`;

const header = label => `
  <text x="55" y="55" fill="${colours.navy}" font-size="31" font-weight="900">DEBIAS</text>
  <text x="186" y="52" fill="${colours.grey}" font-size="14" font-weight="800" letter-spacing="1.1">${xml(label)}</text>
  <line x1="55" y1="76" x2="1145" y2="76" stroke="${colours.line}"/>
`;

const ratioX = ratio => {
  const clamped = Math.max(0.25, Math.min(4, ratio));
  return 255 + ((Math.log2(clamped) + 2) / 4) * 510;
};

const fingerprintChart = () => {
  const ticks = [0.25, 0.5, 1, 2, 4];
  const rows = [320, 410, 500, 590];
  const watford = payload.pair.find(area => area.code === 'E07000103');
  const derbyshire = payload.pair.find(area => area.code === 'E07000038');
  const tickMarkup = ticks.map(tick => {
    const x = ratioX(tick);
    return `
      <line x1="${x}" y1="294" x2="${x}" y2="620" stroke="${tick === 1 ? colours.navy : colours.line}" stroke-width="${tick === 1 ? 1.6 : 1}"/>
      <text x="${x}" y="646" fill="${colours.grey}" font-size="14" text-anchor="middle">${tick}×</text>`;
  }).join('');

  const rowMarkup = payload.sources.map((source, index) => {
    const y = rows[index];
    const watfordRatio = watford.observations[source.id].ratio_to_fitted;
    const derbyshireRatio = derbyshire.observations[source.id].ratio_to_fitted;
    const watfordX = ratioX(watfordRatio);
    const derbyshireX = ratioX(derbyshireRatio);
    const watfordColour = watfordRatio < 1 ? colours.teal : colours.coral;
    const derbyshireColour = derbyshireRatio < 1 ? colours.teal : colours.coral;
    const watfordY = y - 17;
    const derbyshireY = y + 17;
    const diamond = `${derbyshireX},${derbyshireY - 9} ${derbyshireX + 9},${derbyshireY} ${derbyshireX},${derbyshireY + 9} ${derbyshireX - 9},${derbyshireY}`;
    return `
      <text x="65" y="${y + 3}" fill="${colours.navy}" font-size="20" font-weight="900">${xml(source.label)}</text>
      <text x="65" y="${y + 26}" fill="${colours.grey}" font-size="13">r = ${source.raw_count_pearson_r.toFixed(2).replace(/^0/, '')}</text>
      <line x1="255" y1="${y}" x2="765" y2="${y}" stroke="${colours.line}" stroke-width="1.5"/>
      <circle cx="${watfordX}" cy="${watfordY}" r="8" fill="${watfordColour}" stroke="${colours.navy}" stroke-width="1.5"/>
      <text x="${watfordX + 12}" y="${watfordY + 5}" fill="${colours.navy}" font-size="14" font-weight="800">Watford ${watfordRatio.toFixed(2)}×</text>
      <polygon points="${diamond}" fill="${derbyshireColour}" stroke="${colours.navy}" stroke-width="1.5"/>
      <text x="${derbyshireX + 13}" y="${derbyshireY + 5}" fill="${colours.navy}" font-size="14" font-weight="800">N.E. Derbyshire ${derbyshireRatio.toFixed(2)}×</text>`;
  }).join('');

  return `${tickMarkup}${rowMarkup}`;
};

const fingerprintSvg = shell(
  'The same pair switches sides across four digital sources',
  'Watford and North East Derbyshire sit above or below different source-specific fitted proportional rates as the source changes.',
  `${header('DATA STORY · ACT II · FOUR-SOURCE FINGERPRINT')}
  ${lineText(['THE SAME PAIR', 'SWITCHES SIDES.'], 55, 142, { size: 50, weight: 900, lineHeight: 50 })}
  ${lineText(['The places have not changed. The source has.'], 55, 225, { size: 21, weight: 700, fill: colours.navySoft })}
  <text x="65" y="258" fill="${colours.teal}" font-size="14" font-weight="900" letter-spacing="1">RELATIVE TO EACH SOURCE’S FITTED PROPORTIONAL RATE</text>
  <text x="255" y="281" fill="${colours.grey}" font-size="13">Fewer than fitted</text>
  <text x="765" y="281" fill="${colours.grey}" font-size="13" text-anchor="end">More than fitted</text>
  ${fingerprintChart()}
  <rect x="835" y="250" width="310" height="370" fill="${colours.white}" stroke="${colours.line}" rx="5"/>
  <text x="865" y="292" fill="${colours.teal}" font-size="14" font-weight="900" letter-spacing="1">THE RESULT</text>
  ${lineText(['THE SAME AREA', 'CAN CHANGE', 'POSITION WHEN', 'THE SOURCE', 'CHANGES.'], 865, 340, { size: 33, weight: 900, lineHeight: 34 })}
  ${lineText(['One times marks each source’s', 'own fitted proportional rate.'], 865, 540, { size: 18, weight: 650, fill: colours.navySoft, lineHeight: 25 })}
  <circle cx="875" cy="603" r="7" fill="none" stroke="${colours.navy}" stroke-width="2"/>
  <text x="892" y="608" fill="${colours.grey}" font-size="13">Watford</text>
  <polygon points="987,596 994,603 987,610 980,603" fill="none" stroke="${colours.navy}" stroke-width="2"/>
  <text x="1004" y="608" fill="${colours.grey}" font-size="13">North East Derbyshire</text>
  <line x1="55" y1="705" x2="1145" y2="705" stroke="${colours.line}"/>
  ${lineText(['Observed identifiers per resident relative to each source’s own fitted rate. Not a representativeness score or a percentage of residents included.'], 55, 734, { size: 15, weight: 650, fill: colours.navySoft })}
  <text x="55" y="770" fill="${colours.grey}" font-size="12">England and Wales · source numerators and dates differ · 2021 snapshots · released LAD-level dataset</text>
  <text x="1145" y="770" fill="${colours.grey}" font-size="12" text-anchor="end">Cabrera &amp; Rowe / DEBIAS</text>`
);

const switchGrid = () => {
  const columns = 19;
  const size = 14;
  const gap = 5;
  const startX = 65;
  const startY = 350;
  return payload.areas
    .slice()
    .sort((a, b) => {
      const aPair = ['E07000103', 'E07000038'].includes(a.code) ? 0 : 1;
      const bPair = ['E07000103', 'E07000038'].includes(b.code) ? 0 : 1;
      if (aPair !== bPair) return aPair - bPair;
      if (a.switches_sides !== b.switches_sides) return a.switches_sides ? -1 : 1;
      return a.name.localeCompare(b.name);
    })
    .map((area, index) => {
      const column = index % columns;
      const row = Math.floor(index / columns);
      const x = startX + column * (size + gap);
      const y = startY + row * (size + gap);
      const pair = ['E07000103', 'E07000038'].includes(area.code);
      return `<rect x="${x}" y="${y}" width="${size}" height="${size}" rx="2" fill="${area.switches_sides ? 'url(#switch-gradient)' : colours.paper}" stroke="${pair ? colours.navy : (area.switches_sides ? colours.white : '#939aa0')}" stroke-width="${pair ? 2.5 : 1}"/>`;
    }).join('');
};

const switchesSvg = shell(
  'Three hundred of 331 local authorities change sides across sources',
  'A unit chart shows 300 local authorities above a fitted rate in at least one source and below it in another.',
  `${header('DATA STORY · ACT II · 331 LOCAL AUTHORITIES')}
  <defs><linearGradient id="switch-gradient" x1="0%" y1="0%" x2="100%" y2="0%"><stop offset="0%" stop-color="${colours.teal}"/><stop offset="49.5%" stop-color="${colours.teal}"/><stop offset="50.5%" stop-color="${colours.coral}"/><stop offset="100%" stop-color="${colours.coral}"/></linearGradient></defs>
  ${lineText(['MOST PLACES CHANGE SIDES', 'WHEN THE SOURCE CHANGES.'], 55, 142, { size: 50, weight: 900, lineHeight: 52 })}
  <text x="65" y="292" fill="${colours.navy}" font-size="95" font-weight="900" letter-spacing="-3">300</text>
  <text x="245" y="272" fill="${colours.navy}" font-size="22" font-weight="900">/ 331 local authorities</text>
  ${lineText(['change sides across four', 'source-specific fitted rates'], 245, 298, { size: 14, weight: 650, fill: colours.grey, lineHeight: 18 })}
  ${switchGrid()}
  <rect x="65" y="698" width="14" height="14" rx="2" fill="url(#switch-gradient)"/>
  <text x="88" y="710" fill="${colours.grey}" font-size="13">300 change sides</text>
  <rect x="240" y="698" width="14" height="14" rx="2" fill="${colours.paper}" stroke="#939aa0"/>
  <text x="263" y="710" fill="${colours.grey}" font-size="13">31 remain on one side</text>
  <rect x="500" y="250" width="645" height="420" fill="${colours.white}" stroke="${colours.line}" rx="5"/>
  <text x="535" y="300" fill="${colours.teal}" font-size="14" font-weight="900" letter-spacing="1">THE ENGLAND-AND-WALES PATTERN</text>
  <text x="535" y="410" fill="${colours.navy}" font-size="120" font-weight="900" letter-spacing="-4">91%</text>
  ${lineText(['SIT ABOVE A FITTED RATE IN', 'AT LEAST ONE DATASET AND', 'BELOW IT IN ANOTHER.'], 535, 468, { size: 31, weight: 900, lineHeight: 35 })}
  ${lineText(['The pair is memorable. The full pattern', 'shows it is not an edge case.'], 535, 590, { size: 19, weight: 650, fill: colours.navySoft, lineHeight: 27 })}
  <line x1="55" y1="730" x2="1145" y2="730" stroke="${colours.line}"/>
  <text x="55" y="755" fill="${colours.navySoft}" font-size="14" font-weight="650">The 91% describes cross-source position relative to fitted rates—not the percentage of people represented.</text>
  <text x="55" y="782" fill="${colours.grey}" font-size="12">Source numerators and observation dates differ; fitted rates are source specific.</text>
  <text x="1145" y="782" fill="${colours.grey}" font-size="12" text-anchor="end">Cabrera &amp; Rowe / DEBIAS</text>`
);

await mkdir(mediaDir, { recursive: true });
await Promise.all([
  writeFile(join(mediaDir, '05-four-source-fingerprint.svg'), fingerprintSvg),
  writeFile(join(mediaDir, '06-cross-source-switches.svg'), switchesSvg)
]);

console.log('Wrote media/05-four-source-fingerprint.svg and media/06-cross-source-switches.svg.');
