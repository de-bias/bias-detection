# Who is missing from the map?

A static, data-driven scrollytelling page for Carmen Cabrera and Francisco Rowe's published *Royal Society Open Science* article, [“Making hidden biases visible in population location data from mobile phones”](https://doi.org/10.1098/rsos.251703) (2026, volume 13, article 251703).

Canonical story URL: <https://de-bias.github.io/debias/stories/making-hidden-biases-visible/>

The main page tells two connected, reach-first acts. Act I uses Meta to show that a strong aggregate count correlation can coexist with substantial variation in local population coverage. Act II shows that the same place changes position across four source-specific fitted rates and that coverage bias has different, often nonlinear associations with area context. The conclusion is operational: representativeness must be diagnosed and adjustments validated source by source.

## Preview locally

From this directory, serve the files over HTTP:

```sh
python3 -m http.server 4113
```

Then open `http://127.0.0.1:4113/`.

The site has no framework, package installation or build-time JavaScript dependency. It can be copied into a subdirectory of either target website. All internal links are relative.

## Rebuild the story data and media

From the repository root:

```sh
Rscript paper/rsos-debias/scrollytelling/site/scripts/build_story_data.R
```

The script reads:

- `outputs/manuscript-data/lad-derived-population-counts.csv`; and
- the ONS 2021 ultra-generalised Local Authority District boundary service.

It validates the expected 331 rows, joins stable LAD codes, calculates all public metrics, creates compact browser-ready map paths, exports geometry-free explorer data and the local-authority CSV, and regenerates the four static SVG figures. Set `LAD_BOUNDARY_GEOJSON=/path/to/file.geojson` to use a downloaded boundary file without network access.

The build also creates portrait versions of the Counts, Rates and Map figures for narrow screens. Rebuild the 1200×630 sharing image separately:

```sh
Rscript paper/rsos-debias/scrollytelling/site/scripts/build_social_card.R
```

Build the compact four-source Act II payload separately:

```sh
Rscript paper/rsos-debias/scrollytelling/site/scripts/build_source_story_data.R
```

That builder pins the released LAD input by SHA-256, converts integer inputs to doubles before through-origin calculations, validates the exact pair and 300/22/9 area classifications, and writes `data/source-story.json`. It has no boundary or network dependency.

Rebuild the two downloadable cross-source media graphics from that reviewed payload:

```sh
node paper/rsos-debias/scrollytelling/site/scripts/build_source_story_media.mjs
```

Rebuild the three nonlinear figure treatments with a Python environment that provides Pillow and NumPy:

```sh
python3 paper/rsos-debias/scrollytelling/site/scripts/build_nonlinear_panels.py
```

`scripts/nonlinear_panels_manifest.json` pins the accepted `explain-bias.png` checksum, plot-pixel crop rectangles, the DEBIAS templates and output budgets. Each crop is built in two forms: a 900×520 plot-forward asset for responsive story cards and a 900×720 self-contained media download with context, metric guidance, provenance and credit. The script preserves the accepted observations, axes, tick labels and fitted curves as pixels, applying only a deterministic palette remap and proportional resize. It does not digitise, redraw or infer model curves. The Multi-app1 crop masks only a clipped duplicate feature title inherited from the neighbouring accepted-figure panel; the manifest pins and explains that rectangle.

The accepted figure uses colour as a redundant encoding of horizontal feature value. The navy-to-teal remap preserves its luminance order, while horizontal position remains the authoritative encoding. The media footer explicitly decodes the inherited axes as a standardised area characteristic (x) and SHAP contribution to predicted coverage bias (y); the responsive story treatment provides the same guidance as live text rather than shrinking it into the raster.

The reviewed byte-identical crop build used Pillow 12.3.0 with libwebp 1.6.0. The manifest’s output checksums deliberately make an encoder change fail loudly so it can be visually re-audited rather than silently accepted.

Rebuild the site-native area-context radial atlas from the revised R1 main-model outputs:

```sh
python3 paper/rsos-debias/scrollytelling/site/scripts/build_context_radials.py
```

`scripts/context_radials_manifest.json` pins four feature-importance inputs from the accepted-revision main specification: random holdout, no lagged covariates, and the `fb_tts` construction for Meta. The standard-library builder validates the four checksums and a common 30-feature set, applies the paper's within-source min–max transformation across all 30 features, and emits `data/context-radials.json` plus 16 accessible SVGs: four sources × four display groups. It fails if the inputs, groups or expected feature matrix change.

The four display groups contain 10 demographic, 11 socioeconomic, 4 resource-accessibility and 5 combined mobility/geographic characteristics. They span five manuscript domains because the final display group combines mobility and geography. Within each display group, axis order follows the layout rule in `code/06_radial-plots.qmd`—descending four-source mean raw importance with metadata order as the tie-break—but the order is layout only, not an additional importance estimate.

This is **a website re-render of the accepted-revision main-model outputs, not a reproduction of the accepted paper radial figure**. The archived paper raster uses a different older Twitter/Meta build chain. The previous accepted-raster crop builder and assets remain in the repository only as an audit archive and are not referenced by the live pages.

The released CSV calls one Welsh local authority `Rhondda Cynon Taff`; the official ONS layer calls it `Rhondda Cynon Taf`. The build script contains that one explicit name mapping and fails on any other unmatched geography.

## Evidence contract

- Geography: 331 2021 Local Authority Districts in England and Wales.
- Census benchmark: 2021 resident population.
- Meta measure: average nighttime active-account estimate, March 2021.
- Pearson correlation: 0.9133192, shown publicly as `.91`.
- Fitted proportional rate: 8.0948036 per 100 census residents.
- Middle 90% of unweighted LAD rates: 4.6092857 to 12.3077977.
- Illustrative pair: Watford and North East Derbyshire, with rates of 2.3658627 and 15.8861188 per 100.
- Map measure: local rate minus the fitted rate. Positive means more than fitted and negative means fewer than fitted. This has the opposite sign to the paper's residual-bias convention.
- Four-source fitted rates per 100 Census residents, calculated from the released LAD table: Twitter/X 0.1918051, Meta 8.0948036, Multi-app1 2.3770484 and Multi-app2 1.0230420.
- Raw-count Pearson correlations across those four released series: 0.7994609, 0.9133192, 0.9498728 and 0.8987967.
- Cross-source result: 300 of 331 authorities are above their source-specific fitted rate in at least one dataset and below it in another; 22 are below all four and 9 are above all four.
- Four-source input snapshot SHA-256: `e8e6d0bc4b9bb146f713c9ce6ab882316569a79d4664a9e7e7b0dcf29804e7a8`.
- Nonlinear panels: illustrative SHAP dependence crops from the accepted figure for Twitter/X × share aged 20–29 (S-shape), Meta × population density (curved reversal), and Multi-app1 × share with Level 4 qualifications (threshold). These are feature-level examples, not source-wide signatures.
- Radial profiles: a site-native re-render of the accepted-revision main-model outputs for all 30 features, organised as four display groups spanning five model domains and switchable by source in Act II. Radius encodes within-source relative mean absolute SHAP importance: 1 is that source's highest-scoring feature and 0 its lowest, not “no effect.” It does not encode user composition, direction, causality, population shares or group-specific inclusion rates; equal radii across sources are not equal absolute SHAP contributions.
- Radial input snapshots: Twitter/X `7db31f979773b05063100802fe706de075bdd87434b4e2e97a496ef5e68d27a7`; Meta (`fb_tts`) `30ff49b8aabcb30bf02d9f74fad2d2d8885050c758a269d91c8861ecb9227d25`; Multi-app1 `0e4af9a1dd60f2c25f4bb048389fe80d53fd567c40cec88ee7a4fc0a965ca643`; Multi-app2 `20bee26444776703c401f2b953e9c3be6ec67e9c8e4e0dd619f860ad16f9dae4`.

The four source numerators and observation periods are not interchangeable: Twitter/X and Meta use March 2021 snapshots, Multi-app1 uses the first week of April, and Multi-app2 uses November. Do not describe active-account estimates or devices as interchangeable unique people. Do not infer that higher coverage means better representation. Aggregate data cannot identify which individuals or demographic groups are included or missing. The nonlinear panels contain area-level model associations, not group-specific inclusion rates or causal effects; compare shapes, not magnitudes across panels.

## Page structure

- `index.html`: reach-first two-act story, including the canonical four-source radial atlas and three illustrative nonlinear relationships, compact conclusion, About the Research, Research Metrics, live article-attention badge and calls to action.
- `explore.html` and `explore.js`: shareable local-authority finder using stable LAD-code URLs.
- `research.html`: About the Research, Research Metrics, Article attention, interpretation boundaries, radial provenance and a deep link to the canonical Act II atlas.
- `context-radials.js`: isolated Act II source switcher for prebuilt radial SVGs, captions and the exact-value table; it performs no statistical calculation or chart geometry.
- `media/media-brief.html`: reporting summary, cross-source evidence and language guidance.
- `media/visuals.html`: lead news card, Act I figures, two cross-source proof graphics and self-contained Act II crops from the accepted model figure.
- `data/meta-story.json`: generated, compact interactive data.
- `data/meta-areas.json`: generated geometry-free data for the explorer.
- `data/source-story.json`: generated compact four-source evidence and exact featured-pair observations.
- `app.js`: isolated Act I progressive enhancement, scroll state and chart rendering.
- `source-story.js`: isolated, lazy Act II enhancement with two chart states.
- `media/nonlinear/`: deterministic plot-forward story assets and self-contained media panels, each with WebP and PNG outputs.
- `media/radial-generated/`: deterministic, site-native SVG profiles for all four sources and four display groups.
- `styles.css`: responsive visual system, reduced-motion and print rules.
- `assets/fonts/`: self-hosted Roboto Condensed webfont and its OFL licence.

Run the lightweight release check from the repository root:

```sh
node paper/rsos-debias/scrollytelling/site/scripts/validate_site.mjs
```

It checks the evidence-bearing public values, exact pair fixtures, 300/22/9 area classifications, source periods and rates, all four approved R1 feature-importance input checksums, the 30×4 feature matrix, within-source normalisation, 16 SVG hashes and accessibility metadata, safe area-level language, interaction isolation, compact file budgets, deterministic-media provenance, fallbacks and sharing-card dimensions. It complements visual browser testing; it does not replace it.

## Publication configuration

- Published article DOI: `10.1098/rsos.251703`.
- Published article URL: <https://doi.org/10.1098/rsos.251703>.
- Publication date: `2026-08-19`.
- Canonical story URL: <https://de-bias.github.io/debias/stories/making-hidden-biases-visible/>.
- The official Altmetric embed is configured with the complete DOI; no score is fabricated when attention has not yet been indexed.
- The files are mounted as an isolated static route so the Quarto host cannot override the story's CSS or JavaScript.
- Source, figure-interpretation and aggregate-data boundary notes remain visible below the story.

## Performance and accessibility

The opening Pair comparison is complete semantic HTML and does not depend on JavaScript. On larger screens with motion enabled, Act I loads a roughly 150 KB story-data file and reuses the same 331 circles from Counts to Rates before crossfading to map paths. At viewports at least 991 px wide and 680 px high, the first part of Act II lazy-loads a payload under 50 KB and uses two isolated SVG states while retaining its exact comparison table for assistive technology. The radial atlas is a separate progressive enhancement: Twitter/X's four SVGs and exact table are the semantic fallback, while source selection swaps prebuilt assets and values without recomputing statistics or geometry. Its three nonlinear WebP panels total under 120 KB, with PNG fallbacks. Narrower, reduced-motion, failed-load and no-JavaScript modes keep the complete story as stacked semantic HTML and static figures. The separate explorer loads a geometry-free file of roughly 40 KB and supports stable LAD-code URLs.
