# Scrollytelling structure assessment

## Decision

Use a **question-led, diagnostic-reversal story followed by an optional local explorer**.

The page should not open with methods, a dashboard or the survey comparison. It should expose the weakness of the validation used in much existing work: a high Pearson correlation between census population and active-user counts is often treated as evidence of representativeness. Correlation shows that more populous areas tend to generate more records; it does not show that the active-user-to-population scale is constant in every area.

Recommended display title:

> **Who is missing from the map?**

Recommended narrative thesis, displayed directly beneath the title:

> **The dots line up. The coverage still varies.**

Recommended deck:

> Across 331 local authority areas in England and Wales, counts from four 2021 mobile-app datasets rose closely with census population. Yet local coverage varied by place, data source and area context. A high correlation was a useful first check—not an answer to who or where the data may under-see.

Recommended SEO title:

> A high correlation can hide uneven coverage in mobile-phone population data

Recommended social-card line:

> **Virtually identical populations. Nearly sevenfold different local rates.**

This hybrid offers the strongest combination of curiosity, visual revelation, public relevance and scientific defensibility. “Who is missing?” supplies the human question; the correlation trap supplies the paper's methodological novelty; the map supplies local relevance; and “why one straight line falls short” supplies a practical resolution. It gives journalists a clean framing: **a commonly used correlation check can look convincing while missing systematic local differences—and those differences vary by place and data source**.

## Assessment method

The recommendation was developed through two review rounds covering:

1. editorial and media strategy;
2. scrollytelling evidence, interaction and mobile performance; and
3. scientific communication and overclaim risk.

The first round compared competing architectures. The second round red-teamed the leading structure for click appeal, completion, sharing, journalist reuse and scientific integrity. The scores below are editorial judgements, not experimentally measured effects.

| Rank | Architecture | Readership | Completion | Media reuse | Scientific safety | Overall |
|---|---|---:|---:|---:|---:|---:|
| 1 | Diagnostic reversal: high correlation → unequal local scales | 5 | 5 | 5 | 5 | **4.9** |
| 2 | Mystery: more apps → do they solve bias? | 5 | 4 | 5 | 4 | **4.5** |
| 3 | Geography first: four maps and local lookup | 4 | 3 | 4 | 4 | **3.8** |
| 4 | Coverage first: mobile data versus surveys | 4 | 3 | 4 | 3 | **3.5** |
| 5 | Methods first: framework and diagnostics | 2 | 2 | 3 | 5 | **3.0** |

The recommended story combines the first two options: diagnostic reversal is the spine; the multi-app result supplies a second, media-friendly turn. After the second adversarial review, the compressed hybrid scored approximately **8.5/10 for completion, 9/10 for media reuse, 9/10 for sharing and 9/10 for scientific trust**. These are editorial assessments, not measured outcomes.

## Recommended narrative

The guided story should contain **six compact acts, presented in about nine visual states**, followed by an evidence-and-utility tail. On a typical phone, the paired-place comparison should appear in the first screen and the practical audit should be reachable within roughly 2–3 minutes. The statistical contradiction should be clear within approximately 20–30 seconds, and the central finding must be understandable without using the explorer.

### Hero — Who is missing from the map?

Use the emotionally resonant question as the display headline, paired immediately with the analytical thesis:

> **WHO IS MISSING FROM THE MAP?**
>
> **The dots line up. The coverage still varies.**

Supporting copy:

> Mobile-app counts can closely follow census population and still observe places at very different rates. This study cannot identify missing individuals. It reveals where coverage differs and which kinds of areas are associated with those differences.

The title therefore functions as the motivating question, not as a claim that the analysis observes individual identities.

### Act I — Same population. Very different coverage

#### Virtually equal populations, nearly sevenfold different rates

Open with a concrete comparison from the accepted study data:

| Area | Census population | Meta nighttime active-account estimate | Estimate per 100 census residents |
|---|---:|---:|---:|
| Watford | 102,246 | 2,419 | 2.37 |
| North East Derbyshire | 102,001 | 16,204 | 15.89 |

Their census populations differ by about 0.24%, while their local Meta estimates per resident differ by about 6.7 times.

Hero copy:

> **Virtually identical populations. Nearly sevenfold different local rates.**

Label the comparison as an illustrative extreme, then immediately show that the wider result is not driven only by this pair: within the middle 90% of local authorities, Meta estimates range from approximately 4.61 to 12.31 active-account records per 100 census residents—a 2.7-fold spread.

Persistent definition:

> These are average nighttime active-account estimates relative to census population, not verified unique residents or probabilities that individuals were observed.

This paired-place card should be pre-rendered, fast to load and reusable as the lead press/social asset.

### Act II — The correlation trap

#### Zoom out: the dots line up

Transform the two area cards into their points on the full Meta scatterplot and let the other 329 areas appear.

Show the correctly labelled Pearson correlation, approximately \(r=0.91\), separately from the accepted paper's through-origin \(R^2\), approximately 0.947.

Takeaway copy:

> **Across all 331 areas, the counts line up closely. This is where many representativeness checks stop.**

Then define the limitation:

> Pearson correlation asks whether larger places tend to generate larger counts. It does not ask whether every place generates records at a comparable rate.

#### Restore the observed local rates

Use one purposeful counterfactual animation:

1. **If every area had the same local rate:** all points sit on one proportional line through zero and Pearson \(r=1\).
2. **Restore the observed local rates:** points move vertically to their real positions; Pearson \(r\) updates truthfully but remains high, at approximately 0.91 for Meta.

The synthetic transition should autoplay once through scrolling and offer a keyboard-accessible **Replay explanation** control. Do not expose the interpolation formula or require the reader to manipulate a slider. Under reduced motion, show labelled before-and-after panels.

Takeaway copy:

> **The national relationship remains strong. The local rates separate dramatically.**

#### Put every place on the same scale

Let the scatter points settle onto one horizontal axis:

> active-account estimate per 100 census residents

Add the fitted proportional baseline and show arrows for the two named areas only. Define the public-facing diagnostic as:

\[
\Delta_i = \frac{D_i}{P_i} - \hat\alpha
\]

Call \(\Delta_i\) the **local coverage-rate departure**:

- positive means more observed identifiers per resident than the fitted source baseline;
- negative means fewer; and
- the unit is percentage points.

This measure is intentionally defined for intuitive public communication and must be documented as the negative of the accepted proof's residual-bias sign convention. Avoid “people overrepresented” and “people underrepresented”; use **more records than baseline** and **fewer records than baseline**.

Takeaway copy:

> **Dividing by local population reveals what one national correlation compresses away.**

### Act III — Geography: put the differences back on the map

Preserve each point's identity and departure colour as it moves to its local-authority position.

Takeaway copy:

> **The scatterplot compresses 331 local differences into one strong relationship. The map puts those differences back in place.**

Use words at the two ends of the legend—**fewer records than baseline** and **more records than baseline**—with signed values secondary. Do not inherit the proof figure's current sign/colour wording. Explain that broad spatial clustering is generally weak rather than inventing a simple regional divide.

Place the second media-friendly turn immediately below the map and before the reader reaches the midpoint:

> **More apps did not guarantee higher observed coverage.**

Supporting copy:

> Two provider datasets drew from multiple apps. Their observed counts per resident were not consistently higher than those of the single-app datasets, and local differences remained. Because products, providers and windows differ, this comparison does not isolate the causal effect of app number.

Do not depict the multi-app products as additions or combinations of Twitter/X and Meta. They are independent provider products. Do not claim that multi-app coverage was necessarily less even.

### Act IV — Who is missing? These data cannot identify people

Use large source tabs while retaining the same selected local authority and visual state. Let readers switch between:

- count–population correlation;
- local observed-count rate;
- local departure from the source baseline; and
- mapped pattern.

Keep one common absolute rate scale where defensible. Add a relative-to-source-baseline index for pattern comparison when sources have very different overall rates.

Then move from geography to source-specific area associations. Lead with the boundary and the answer the analysis can support:

> **What the data can reveal is which kinds of places have different coverage.**

Show one checked Meta coverage profile in the guided story. Population density and rurality can be identified as among the higher-ranked area-level features, but do not assign a simple direction where the fitted association is nonlinear. Put the full four-source profiles behind an optional control. State persistently:

> These are associations between coverage and characteristics of areas. They do not identify the attributes of missing individuals.

Avoid a platform league table, invented feature-strength values and any conversion of contextual associations into individual-level claims.

### Act V — Why one straight line falls short

Use one checked, real Meta example rather than a collection of generic curve types. Population density is the strongest public example because density/rurality rank highly in the accepted source-specific model, the fitted relationship is visibly nonlinear, and the interpretation remains intuitive at area level.

Show local coverage-rate departure against census population density with:

- one dot per local authority;
- a straight reference fit;
- the flexible fitted relationship and uncertainty; and
- a visible area-level, non-causal caveat.

Takeaway copy:

> **For Meta, local coverage departure did not change at one constant rate with census population density. One straight slope would conceal part of the pattern.**

The page should not imply that the paper tested and rejected every possible correction. It shows why one global correlation or one straight slope is not an adequate diagnosis of all local differences.

### Act VI — Measure before you infer

Resolve the guided story with both statements:

> **Correlation is a first diagnostic, not a certificate.**
>
> **Measure before you infer.**

Then assemble the audit on one screen:

1. **Correlate:** do larger areas generate more records?
2. **Scale:** is one proportional rate adequate?
3. **Localise:** where do observed rates depart from that baseline?
4. **Examine:** which area characteristics are associated with the variation?
5. **Adjust with care:** design and validate any source-specific adjustment separately.

The framework diagnoses coverage differences. Do not claim that it corrects bias or validates individual-level representativeness.

### Evidence and utility tail

After the main conclusion, add three optional layers.

#### Explore a local authority

For each validated area, show:

- census population and the source-specific observed-count definition;
- observed identifiers per 100 or 1,000 census residents;
- fitted source baseline and local coverage-rate departure;
- comparison with the study-area distribution;
- source window and processing note; and
- a permanent area-level interpretation caveat.

Give every area a stable URL and downloadable share card for local and regional media.

#### About the Research

Following the component pattern used by [Riccardo Di Clemente](https://www.riccardodiclemente.com/assets/progetti/time_milan/index.html), include:

- a short research statement and the central question;
- study design, geography, dates and units;
- the accepted Royal Society Open Science publication and DOI;
- authors, affiliations and roles;
- funding and partners;
- data, code and supplementary links;
- ethics/privacy statement; and
- media and researcher contacts.

#### Research Metrics

Separate **Study at a glance** from **Article attention**.

Study at a glance:

- **4** mobile-app datasets;
- **331** local authority areas;
- **2021** census benchmark; and
- approximately **0.80–0.95** Pearson correlation across the four released count series.

Article attention should be hidden at launch until real post-publication values exist. Once populated, keep it dated and separate:

- Altmetric attention;
- citations with named source and last-updated date;
- selected press coverage; and
- transparent page indicators such as story completions, paper clicks and media-asset downloads where appropriate.

Do not combine citations, attention, web analytics and scientific study dimensions into one score. Do not sum “users” across sources whose entities and observation windows differ.

## Changes to mock-up version 1

Keep:

- the clean editorial palette and strong typographic hierarchy;
- the persistent-map concept;
- the four-source colour identity;
- “More signal. Still selective.” as supporting copy;
- the nonlinear-curve visual language; and
- “Measure before you infer” as the resolution.

Change:

- restore “Who is missing from the map?” as the motivating headline and pair it with the precise thesis “The dots line up. The coverage still varies.”;
- replace the opening coverage grid with the Watford–North East Derbyshire paired-place hook;
- insert the same-rate counterfactual → observed local rates → map reveal immediately after the hero;
- move the survey/coverage comparison out of the opening and mark it pending reconciliation;
- place the safe multi-app result directly after the map and before 50% scroll depth;
- follow it with “Who is missing? These data cannot identify people” and one checked Meta area-profile act;
- use neutral ranking markers rather than invented directional fingerprint bars;
- make “Why one straight line falls short” one real, checked Meta population-density relationship rather than generic illustrative curves;
- label the analysis geography as England and Wales;
- add the local explorer, About the Research, Research Metrics and media-download tail; and
- make the area-level and identifier-definition caveats visible, not buried in methods.

The current mock-up is a strong visual system. Its present sequence is closer to an infographic stack than a story with a reversal and payoff.

## Media-engagement plan

### Three highest-leverage assets

1. **Paired-place/correlation animation:** a 10–15 second equal-populations → divergent local rates → high-correlation scatter transformation in 16:9, square and vertical formats.
2. **Journalist press pack:** a 150-word summary, three verified findings, two quotes, limitations, DOI, author contacts, code/data links, and high-resolution SVG/PNG figures with captions and credits.
3. **Validated local profiles:** stable URLs and Open Graph images for each local authority, giving local newsrooms a ready-to-use geographical angle.

The static pack should include:

- the paired-place hook and “one dataset, three questions” graphic: correlation, proportionality and local departure;
- a four-source comparison focused on the multi-app finding;
- one source-pattern/association figure; and
- the proposed assessment framework.

Use takeaway titles, short source notes, alt text and explicit source units. Offer SVG for editing and high-resolution PNG for direct reuse.

### Publication and hosting

- Make the DEBIAS page the canonical full story.
- Publish a shorter personal-site feature that links to the canonical page, or set an explicit cross-domain canonical URL. Avoid two uncontrolled, identical copies.
- Coordinate the public launch, press release and social assets with the Royal Society and University of Liverpool press teams.
- Stage the page privately before publication and follow the Royal Society's media embargo requirements.
- Add DOI, authorship, publication date, descriptive metadata, Open Graph data and indexable chart captions.

Royal Society Publishing recommends early press-office coordination, a non-technical summary and strong visual assets in its [paper-promotion guidance](https://royalsociety.org/journals/authors/early-career-researchers/promoting-your-paper/). Its [media embargo policy](https://royalsociety.org/journals/ethics-policies/media-embargo/) should govern launch timing. Google's [canonicalization guidance](https://developers.google.com/search/docs/crawling-indexing/canonicalization) supports using one canonical full version across the two planned sites.

### Mobile and accessibility

- Build static HTML first and progressively enhance it.
- Use one sticky visual state at a time; never require simultaneous side-by-side reading on a phone.
- Avoid hover-only controls and swipe-dependent source switching.
- Preserve a readable no-JavaScript version of every central conclusion.
- Honour `prefers-reduced-motion` with explicit before/after states.
- Pre-render the hero and lazy-load the explorer.
- Avoid autoplay video, iframe-heavy sections and thousands of DOM marks.
- Reserve image dimensions to prevent layout shifts.

The UK news environment is mobile-led, so mobile is the primary reading condition, not a reduced desktop version ([Reuters Institute, Digital News Report 2025: UK](https://reutersinstitute.politics.ox.ac.uk/digital-news-report/2025/united-kingdom)). Aim for the current “good” [Core Web Vitals thresholds](https://web.dev/articles/defining-core-web-vitals-thresholds): LCP at or below 2.5 seconds, INP at or below 200 milliseconds and CLS at or below 0.1 at the 75th percentile.

## Why this structure is most likely to travel

- It turns the paper's methodological contribution into a simple correction to a familiar misconception: correlation is not representativeness.
- It begins with a concrete, local and quotable contradiction before introducing the statistic.
- It delivers the statistical reversal within the first two scroll transitions, reducing the risk that readers leave with only the misleading high-correlation view.
- It creates a second newsworthy turn—the multi-app result—before the halfway point.
- It repeats the same visual grammar across sources, reducing the effort needed to compare them.
- It keeps the guided story responsible for comprehension and leaves free exploration to motivated readers.
- It creates reusable national and local assets for science, technology, data, public-policy and regional media.

The design aligns with the qualities highlighted in the [Closeread Prize review](https://posit.co/blog/closeread-prize-winners/) and the ONS recommendations to make one precise point at a time and use meaningful local personalization ([ONS scrollytelling guidance](https://digitalblog.ons.gov.uk/2021/05/24/what-makes-for-a-good-scrollytelling-article/)). Research also supports guided narration for comprehension ([Segel-style narration study](https://doi.org/10.1016/j.cola.2019.04.006)) and parallel visual sequences for memory and preference ([Hullman et al.](https://doi.org/10.1109/TVCG.2013.119)). Evidence that introductory narrative alone does not reliably drive later exploration reinforces the need for the guided section to carry the finding before the explorer ([Boy, Detienne and Fekete](https://doi.org/10.1145/2702123.2702452)).

## Pre-launch validation loop

### Message test

Test two titles:

- A: **The dots line up. The coverage still varies.**
- B: **A high correlation can hide uneven mobile-data coverage.**

Show the hero and first two transitions to non-specialist readers and research-communications staff. After 30 seconds, ask:

1. What is the one-sentence finding?
2. What does Pearson correlation establish—and what does it miss?
3. Does the page tell you which individual people are missing?

The successful version should produce an accurate answer to all three, not just a preferred headline.

### Prototype test

Test the phone version first. Record:

- whether readers understand the difference between correlation and equal local coverage;
- whether the paired-place hook is remembered as an illustration rather than the full result;
- whether they understand the multi-app finding;
- where terminology causes confusion;
- whether the maps are legible without zooming; and
- whether readers discover the paper and local explorer.

### Launch measurement

Track a short engagement funnel:

- hero → first reveal;
- 25%, 50%, 75% and narrative completion;
- multi-app act reached;
- local search used;
- media asset downloaded;
- DOI/paper click;
- code/data click; and
- referring source and social-card click-through.

Judge success using completion, paper clicks, qualified local exploration, asset reuse and media pickup—not page views alone. Set numerical targets only after a baseline or a genuine title test exists.

## Scientific and data QA blockers

Do not freeze public copy or rebuild the quantitative figures until these are resolved against the final accepted manuscript, publisher proof and release dataset.

1. **Authoritative version:** the user has confirmed `paper/rsos-debias-r1` as the accepted version. Use `paper/rsos-debias-r1/submission/RSOS-251703.R1_Proof_hi-2.pdf` as the publication reference and document any webpage measure that intentionally differs from the proof's notation.
2. **Coverage totals:** the manuscript's “22 to 70 per 1,000” statement does not reconcile with the current released LAD CSV. A direct sum of the current file implies approximately 1.79 Twitter/X, 83.28 Meta, 23.89 Multi-app1 and 10.98 Multi-app2 observations per 1,000 census residents. These diagnostic calculations are not release-ready claims.
3. **Survey graphic:** `code/05_compare-surveys.qmd` maps `DFD == "Y"` to the mobile-app colour but labels it “Survey” in the simplified plot. Earlier code also labels coverage points as “Response rate”. Rebuild this figure from checked definitions.
4. **Geographic scope:** the analytical dataset contains 331 local authorities and a census denominator of about 59.6 million, consistent with England and Wales. Reconcile manuscript references to the UK before public use.
5. **Observation windows:** Meta and Twitter/X use March 2021, Multi-app1 uses one week in April 2021, and Multi-app2 uses November 2021. Public prose should say “2021 snapshots” and display the source-specific window.
6. **Units:** distinguish active accounts, unique accounts, observed devices and inferred home locations. Never collapse these into “people” without qualification.
7. **Residual units:** the accepted proof's expected-bias expression uses a factor of 1,000 although coverage bias is defined on a 0–100 percentage scale. Its residual equation is a percentage-point difference, while the figure-generating code divides the gap by expected bias and therefore produces a relative percentage. Reconcile these before reproducing the proof residual numerically.
8. **Residual sign:** both the proof equation and plotting code make positive residual bias mean greater bias/lower coverage than baseline, while the Figure 4 caption describes red/positive as over-represented. The page should instead use the explicitly documented coverage-rate departure \(D_i/P_i-\hat\alpha\), with “more/fewer records than baseline” labels.
9. **Map comparability:** proof residual maps normalized independently to [-1, 1] cannot support cross-source magnitude comparisons. Use raw percentage-point coverage departures on a defensible shared scale, or a clearly labelled relative-to-source index.
10. **Pearson versus through-origin fit:** do not treat Pearson \(r\) or \(r^2\) as the accepted proof's through-origin \(R^2\). Label and calculate each statistic separately.
11. **Ecological inference:** area-level associations cannot identify which individuals are missing or establish causal mechanisms.
12. **Model reliability:** select contextual and nonlinear examples only after checking out-of-sample performance and robustness for each source; do not present every model as equally reliable.

The current LAD CSV was independently read as **331 data rows**. A line-count discrepancy raised during review was therefore not retained as a blocker.

## Claim-safe language

| Avoid | Use |
|---|---|
| “The apps accurately estimate population.” | “Their counts are correlated with census population across areas.” |
| “The high correlation proves representativeness.” | “The correlation shows that more populous areas tend to generate more records; local coverage ratios can still differ.” |
| “More mobile data are more representative than surveys.” | “Larger raw coverage does not guarantee representativeness.” |
| “The model reveals who is missing.” | “The model identifies area characteristics associated with uneven coverage.” |
| “Multi-app data are worse.” | “In these four 2021 snapshots, being multi-app did not guarantee higher observed coverage, and local differences remained.” |
| “Older, poorer or rural people are excluded.” | “Coverage differed between areas with different demographic, socioeconomic or geographic profiles.” |
| “The framework corrects bias.” | “The proposed framework detects and diagnoses coverage gaps.” |
| “Current UK app users…” | “These 2021 England-and-Wales data sources…” |

## Production recommendation

Proceed in this order:

1. use the confirmed accepted proof and reconcile the quantitative/figure QA blockers;
2. create a data/claim sheet with one approved value and wording for every public statement;
3. storyboard the six acts on mobile;
4. prototype the paired-place → high correlation → local-rate transition and one source comparison;
5. run the message and comprehension test;
6. build the complete guided narrative;
7. add the explorer and press assets; and
8. stage privately for journal and press-office review before the embargoed launch.

The next visual design iteration should therefore be a revised mock-up centred on **virtually equal populations → different local rates → reassuring Pearson correlation → mapped coverage departures**, not simply a cosmetic revision of version 1.
