# DEBIAS scrollytelling mockup v4 prompt

## Generation mode

Built-in image generation, reference-image edit.

Initial reference:

`debias-scrollytelling-mockup-v3-hybrid-draft.png`

## Main edit prompt

Edit the attached long desktop webpage mockup into a polished, credible, high-engagement data-driven scrollytelling page for the DEBIAS research project. Keep its warm off-white paper background, navy/teal/coral/royal-blue palette, editorial data-journalism feel, DEBIAS header, strong typography, and England-and-Wales geography. Make the page visibly shorter and less like a numbered infographic: exactly SIX numbered core story acts, followed by unnumbered optional sections. Preserve generous whitespace and create a clear 1440px-wide desktop editorial layout.

Scientific and editorial priority: the page must explain that a high Pearson correlation between census population and active-account counts is a useful first diagnostic but is not evidence that each local area has representative coverage. Differences in records per resident can remain large. The primary display hook is “WHO IS MISSING FROM THE MAP?” and the thesis is “THE DOTS LINE UP. THE COVERAGE STILL VARIES.”

Hero:

- Keep title “WHO IS MISSING FROM THE MAP?”
- Keep thesis “THE DOTS LINE UP. THE COVERAGE STILL VARIES.”
- Deck: “A high correlation is a useful first check — not proof of representative local coverage.”
- Make this caveat large and clearly visible, not fine print: “This study cannot identify missing people. It can show where aggregate local coverage differs and which characteristics of places are associated with those differences.”
- Metadata: 4 DATA SOURCES • 331 LOCAL AREAS • ENGLAND & WALES • 2021.
- A subtle abstract dot-map of England and Wales may sit in the hero, but the text dominates.

ACT 01 — SAME POPULATION. VERY DIFFERENT COVERAGE.

Show Watford and North East Derbyshire side by side as an “ILLUSTRATIVE EXTREME FROM 331 AREAS”. Use neutral abstract dots, device/account-record marks or tiny squares—NO human person icons. Verified values:

- Watford: 102,246 census residents; 2.37 active-account records per 100 census residents.
- North East Derbyshire: 102,001 census residents; 15.89 active-account records per 100 census residents.
- Centre: “6.7× DIFFERENT LOCAL RATES.”

Directly beneath, add credibility line: “Across all areas, the middle 90% spans 4.61–12.31 records per 100 — a 2.7× range.”

Footnote: “Meta nighttime active-account estimates; privacy-processed account records, not verified unique people.”

ACT 02 — THE CORRELATION TRAP.

Design one large sticky-scene storyboard/interactive module, not three separate numbered sections. It may show four compact scroll-state labels A–D inside the same act:

- A “OBSERVED COUNTS” — a raw-count scatterplot with LINEAR axes, census population on x and active-account records on y, dots and trend, “Pearson r = 0.91”. Label Watford and North East Derbyshire.
- B “SAME-RATE COUNTERFACTUAL” — points align because every area has the same records-per-resident rate.
- C “RESTORE OBSERVED RATES” — dots still trend together, but local rates separate.
- D “PUT EVERY PLACE ON THE SAME SCALE” — horizontal dot strip from 0 to 25 records per 100 census residents. Mark Watford 2.37, fitted proportional rate 8.09, and North East Derbyshire 15.89. Exact label: “Fitted proportional rate: 8.09 active-account records per 100 census residents.” Do not call it a national baseline or weighted average.

A prominent insight card says: “A strong correlation can coexist with unequal local coverage.”

ACT 03 — PUT THE DIFFERENCES BACK ON THE MAP.

Show one clean hex/dot choropleth of England and Wales for Meta. Legend must run “fewer records than fitted rate” in teal, “about fitted rate” neutral cream, “more records than fitted rate” in coral. Use an intuitive local coverage departure definition, without inheriting any residual sign confusion. Show source tabs Twitter/X, Meta selected, Multi-app 1, Multi-app 2.

Immediately beneath the map, before the reader reaches halfway down the page, add a strong editorial twist card:

“MORE APPS DID NOT GUARANTEE HIGHER OBSERVED COVERAGE.”

“Two provider datasets drew from multiple apps. Their observed counts per resident were not consistently higher than those of the single-app datasets, and local differences remained. Because products, providers and windows differ, this comparison does not isolate the causal effect of app number.”

NO plus signs, equal signs, or imagery suggesting that the multi-app products were made by combining Twitter/X and Meta.

ACT 04 — WHO IS MISSING? THESE DATA CANNOT IDENTIFY PEOPLE.

Large subline: “WHAT THEY CAN REVEAL: WHICH KINDS OF PLACES HAVE DIFFERENT COVERAGE.”

Show one checked, restrained “META COVERAGE PROFILE” module instead of four fingerprint bar panels. Feature population density and rurality as “among the higher-ranked area-level features” using qualitative, signed or directional visual cues, without inventing numeric effect sizes. Add a small optional link/button “Explore all four source profiles.”

Caveat beside the graphic: “Area-level associations do not identify individual users and do not establish causes.”

ACT 05 — WHY ONE STRAIGHT LINE FALLS SHORT.

Show one real-looking Meta example: a scatterplot of local authority coverage departure versus census population density, with a thin grey straight-line fit and a clear flexible smooth curve with uncertainty band that bends/changes direction. Do not show three generic curve cartoons. Caption:

“For Meta, local coverage departure did not change at one constant rate with census population density. One straight slope would conceal part of the pattern.”

Note: “Each point is a local authority. Descriptive area-level model, not an individual-level or causal result.”

ACT 06 — CORRELATION IS A FIRST DIAGNOSTIC, NOT A CERTIFICATE.

Subhead: “MEASURE BEFORE YOU INFER.”

Show a crisp five-step audit path:

1. CORRELATE — check the national relationship.
2. SCALE — compare records per resident.
3. LOCALISE — map departures from the fitted rate.
4. EXAMINE — test which area characteristics are associated.
5. ADJUST WITH CARE — design and validate any source-specific adjustment separately.

This should feel like the memorable practical resolution and be easy to screenshot.

After the six acts, add compact, UNNUMBERED optional tail sections:

- “EXPLORE A LOCAL AUTHORITY” search and share module with a small verified example.
- “ABOUT THE RESEARCH” cards: Research question; Publication (Cabrera, C. & Rowe, F. (2026), Royal Society Open Science, accepted article); Authors & team; Data & code; Methods & limitations; Funding & contact. Include buttons READ THE PAPER and VIEW THE CODE. State “Independent author-created companion to the accepted article.”
- “STUDY AT A GLANCE” only: 4 datasets; 331 local areas; 2021 census; raw-count Pearson r = 0.80–0.95. Label the correlation range explicitly as raw digital counts versus census population, not validation scores.
- Do NOT show an empty Article Attention dashboard. Instead use a small discreet placeholder note only: “Article attention will appear here after publication.”
- Footer with DEBIAS, authors Carmen Cabrera and Francisco Rowe, Royal Society Open Science.

Audience and engagement:

- The first screen must strongly feature the title plus the paired-place visual.
- Preserve press-ready quotable cards and shareable visual moments.
- Use neutral visual metaphors for accounts/devices/records, not silhouettes of missing people.
- Keep data caveats visible but concise.
- Make it look like an elite science-media interactive by a newsroom graphics desk: accessible labels, highly legible English text, credible plots, clean hierarchy, restrained animation cues, elegant polish.
- Avoid invented claims, invented model effect sizes, fake publisher metrics, fake citations, and nonsensical placeholder text.

## Precision correction prompt

Make only the following precision corrections to this polished scrollytelling webpage mockup. Preserve the exact overall page design, title, hierarchy, six-act structure, colours, maps, layout, typography, and all other approved copy.

1. In ACT 02 “THE CORRELATION TRAP”, the raw-count Pearson comparison MUST use visibly LINEAR axes, never logarithmic axes. Redraw panels A, B and C with evenly spaced arithmetic ticks:
   - x-axis census population: 0, 250k, 500k, 750k, 1.0m
   - y-axis active-account records: 0, 25k, 50k, 75k, 100k
   Keep A labelled “OBSERVED COUNTS — Raw counts on linear axes. Pearson r = 0.91.” Keep Watford and North East Derbyshire labels. The same-rate counterfactual B and restored-rates C must use the same visibly linear scales. Do not show 1K/10K/100K/1M tick sequences.

2. In ACT 04 “META COVERAGE PROFILE”, remove every invented directional claim and all upward/downward arrows. Do not claim that denser places have higher observed coverage or rural areas have more rural coverage. Replace the module content with restrained neutral ranking information:
   - “Population density — among the higher-ranked area-level features”
   - “Rurality — among the higher-ranked area-level features”
   - “Other census characteristics — source-specific patterns”
   Use neutral rank markers or dots, not directional arrows and not numerical effect sizes. Keep the caveat exactly visible: “Area-level associations do not identify individual users and do not establish causes.” Keep “Explore all four source profiles”.

3. In the bottom “EXPLORE A LOCAL AUTHORITY” verified Watford example, correct active-account records to 2,419. Keep census residents 102,246 and records per 100 residents 2.37.

4. In ACT 05, preserve the nonlinear population-density chart and its uncertainty. A logarithmic density x-axis is acceptable there only; it must still be clearly titled CENSUS POPULATION DENSITY and retain the descriptive area-level, non-individual, non-causal note.

Do not introduce any other wording, numerical, or visual changes.
