# DEBIAS lean-alternative image prompts

## Mode

Built-in image generation. Both alternatives were generated as new `ui-mockup` assets and then precision-edited.

## Alternative A — primary prompt

Create a completely new, exceptionally clean and restrained data-journalism webpage mockup for the DEBIAS research project. It should maximise readership, social sharing and media reuse through one cinematic visual argument. Do not copy the dense multi-section composition of earlier versions. Use only three core story movements and one compact footer.

Use a warm off-white editorial page, generous whitespace, thin pale dividers, deep navy typography, one teal for fewer/lower records, one coral-orange for more/higher records and light grey neutrals. Use a condensed bold editorial headline with calm modern sans-serif body text.

Composition:

- compact DEBIAS header with only STORY, EXPLORE, PAPER;
- broad hero occupying the first screen, with the title and message block on the left and the paired-place comparison visible on the right; no decorative hero map;
- below, one large cinematic scrolly canvas whose four endpoint labels are discreetly shown as Pair → Counts → Rates → Map, but only one visual idea dominates at a time;
- final compact conclusion and one-line utility footer.

Text:

- Headline: “WHO IS MISSING FROM THE MAP?”
- Message block: “Mobile-app counts can track census population closely while producing very different observed counts per resident across places. These data cannot identify missing people. They reveal where aggregate coverage differs—and why correlation alone cannot establish representative local coverage.”
- Caveat: “Counts are privacy-processed account estimates divided by the all-age census population—not verified unique people or individual inclusion probabilities.”
- Pair label: “ILLUSTRATIVE EXTREME FROM 331 AREAS”.
- Watford: “102,246 residents”; “2.37 records per 100”.
- North East Derbyshire: “102,001 residents”; “15.89 records per 100”.
- Centre: “6.7× DIFFERENT LOCAL RATES”.
- Use neutral small squares or record/device dots, never people icons.

Show one uncluttered raw-count scatterplot on visibly linear axes, x-axis CENSUS POPULATION, y-axis META ACTIVE-ACCOUNT ESTIMATE, with the two areas labelled, `Pearson r = 0.91`, and the takeaway: “Larger places tend to generate more records. This is where many checks stop.”

Show one horizontal rate strip from 0 to 25 ACTIVE-ACCOUNT RECORDS PER 100 CENSUS RESIDENTS. Mark Watford 2.37, fitted proportional rate 8.09 and North East Derbyshire 15.89. Add “MIDDLE 90%: 4.61–12.31 — A 2.7× RANGE”. Let the strip visually resolve into one clean dot/hex map of England and Wales labelled “META EXAMPLE: LOCAL RATE − FITTED RATE”, with a legend running from “FEWER RECORDS THAN FITTED RATE” to “MORE RECORDS THAN FITTED RATE”.

Conclusion:

- “CORRELATION IS A FIRST DIAGNOSTIC, NOT A CERTIFICATE.”
- “Compare counts. Check local rates. Map the differences.”
- Optional ribbon: “Across these four 2021 snapshots, multi-app sources did not consistently produce higher observed counts per resident.”

Footer:

- “EXPLORE A LOCAL AUTHORITY    ABOUT THE RESEARCH    PAPER & CODE    STUDY AT A GLANCE”
- “CARMEN CABRERA · FRANCISCO ROWE · ROYAL SOCIETY OPEN SCIENCE”

Use a slim four-stop progress rail and one small REPLAY EXPLANATION button only. Keep one message block, no more than three core movements, and a short total page height. Show Pearson only on the linear raw-count scatter; label 8.09 as fitted proportional rate; use England and Wales; make the map explicitly a Meta example; keep the caveat attached to the headline.

Avoid fingerprints, nonlinear curves, source tabs, survey comparison, equations, person pictograms, app-addition arithmetic, plus signs, dashboard grids, article-metric tiles, dense footers, excessive numbering, fake citations or metrics, tiny text, logarithmic axes and decorative clutter.

## Alternative A — precision correction

Preserve the overall composition, compact height, hero, message block, typography, palette, navigation, progress rail, England-and-Wales map, conclusion and footer. Do not add sections.

Correct the scatter to use linear x-axis ticks 0, 250k, 500k, 750k and 1.0m and y-axis ticks 0, 25k, 50k, 75k and 100k. Place Watford and North East Derbyshire at essentially the same x-position near 102k; place Watford near y=2,419 and North East Derbyshire near y=16,204. Keep Pearson `r = 0.91`.

Correct the rate strip so Watford is at 2.37, the fitted proportional rate at 8.09, North East Derbyshire at 15.89, and the middle-90% bracket spans only 4.61 to 12.31.

Use the section labels “THE COUNTS STILL LINE UP.” and “PUT EVERY PLACE ON THE SAME SCALE.” Remove their numbered circles. Keep all other text and values unchanged.

## Alternative B — primary prompt

Create a completely new, minimal data-journalism webpage mockup for the DEBIAS research project. This is the static newsroom-brief alternative: maximum completion, scientific clarity and direct press reuse. Use exactly three large self-contained evidence cards and no core animation. Do not use dashboard grids or numerous sections.

Use a pure warm off-white page, large margins, abundant whitespace, fine grey rules, deep navy, restrained teal, coral-orange and light grey. Use a large direct sans-serif headline, bold condensed card titles and calm readable body type. Every card should be exportable as a standalone press graphic.

Composition:

- compact DEBIAS header with STORY, LOCAL LOOKUP, PAPER;
- concise hero with headline and one bounded pale-navy message block;
- one visible caveat line;
- exactly three spacious full-width cards stacked vertically: raw counts, local rates, geography;
- a three-step conclusion rule beneath card 3;
- one compact unnumbered footer.

Text:

- Headline: “A HIGH CORRELATION CAN HIDE UNEVEN LOCAL COVERAGE”.
- Message block: “Across 331 local authorities in England and Wales, counts from four mobile-app datasets rose strongly with census population. But observed counts per resident varied within every source. Correlation is a useful first diagnostic, not a certificate of representativeness.”
- Caveat: “The four sources use different 2021 windows and account/device definitions; their rates are descriptive, not directly comparable individual capture probabilities.”

Card 1:

- “1 THE REASSURING VIEW”.
- “More populous areas tend to generate more records.”
- One linear raw-count Meta scatterplot with x-axis ticks 0, 250k, 500k, 750k and 1.0m and y-axis ticks 0, 25k, 50k, 75k and 100k.
- “Pearson r = 0.91”.
- Highlight Watford at 102,246 residents and 2,419 records and North East Derbyshire at 102,001 residents and 16,204 records, tagged “ILLUSTRATIVE EXTREME”.

Card 2:

- “2 CHANGE THE QUESTION”.
- “Compare records per resident—not only whether the counts move together.”
- One horizontal strip from 0 to 25 META ACTIVE-ACCOUNT RECORDS PER 100 CENSUS RESIDENTS.
- Mark Watford 2.37, fitted proportional rate 8.09 and North East Derbyshire 15.89.
- Correctly positioned bracket: “MIDDLE 90%: 4.61–12.31 — A 2.7× RANGE”.
- “Strong correlation. Unequal local rates. Both facts are true.”

Card 3:

- “3 PUT THE DIFFERENCES BACK IN PLACE”.
- “One strong national relationship can contain many local departures.”
- One map titled “META EXAMPLE: LOCAL RATE − FITTED PROPORTIONAL RATE”.
- Diverging legend: “FEWER RECORDS THAN FITTED RATE — ABOUT FITTED RATE — MORE RECORDS THAN FITTED RATE”.
- Beneath the map: “CORRELATE → COMPARE LOCAL RATES → MAP DEPARTURES”.
- Final block: “CORRELATION IS A FIRST DIAGNOSTIC, NOT A CERTIFICATE.”

Footer:

- “FIND A LOCAL AUTHORITY    ABOUT & METHODS    PAPER · DATA · CODE    MEDIA ASSETS”.
- “CARMEN CABRERA · FRANCISCO ROWE · ROYAL SOCIETY OPEN SCIENCE”.

Keep exactly three cards, one message block, no core interaction, visibly linear raw-count axes, exact units, 8.09 labelled fitted proportional rate, and a Meta example map. Avoid “Who is missing?” language, fingerprints, nonlinear plots, multi-app results, source tabs, survey comparison, formulas, person icons, dashboards, metric tiles, numbered About sections, controls, animation, fake citations or metrics, tiny copy and log axes.

## Alternative B — precision correction

Preserve the entire lean three-card layout, headline, message block, caveat, typography, palette, whitespace, headings, conclusion and footer.

On the linear scatter, place both highlighted areas at essentially the same x-position near 102k. Place Watford near y=2,419 and North East Derbyshire near y=16,204. Do not place either near one million population. Retain Pearson `r = 0.91` and “ILLUSTRATIVE EXTREME”.

On the rate strip, place Watford at 2.37, fitted proportional rate at 8.09, North East Derbyshire just right of 15 at 15.89, and draw the middle-90% bracket only from 4.61 to 12.31. Preserve the exact bracket text.

Make the map a recognisable upright outline of England and Wales and retain its Meta-example title and legend. Keep everything else invariant.
