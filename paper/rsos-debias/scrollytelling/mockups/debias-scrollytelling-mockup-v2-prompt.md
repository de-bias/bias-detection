# DEBIAS scrollytelling mock-up v2 prompt

Built-in image generation was used, with mock-up v1 as the visual-system and edit reference.

## Prompt

Use case: ui-mockup

Asset type: high-fidelity, full-length vertical scrollytelling webpage concept, version 2

Input image: Image 1 is the edit target and visual-system reference. Preserve its premium editorial character, cream paper background, dark navy typography, blue/teal/pink/orange source palette, fine contour/hex textures, generous spacing, and Royal Society-quality scientific tone. Redesign the content and narrative structure completely around the correlation misconception described below. Do not merely relabel the old panels.

Primary request: Create a polished long vertical webpage mock-up for the DEBIAS research project. The first dramatic message is that a high Pearson correlation between census population and mobile-app counts can coexist with unequal local count-per-resident rates. Make the page look like a real interactive data-journalism feature, with scroll-step cards, interactive control cues, charts, maps, research information, and metrics.

Composition/framing: one tall complete webpage shown straight-on, about the same width-to-height ratio as Image 1. Clear numbered story sections. One visual state per section. Strong hierarchy and plenty of whitespace. Desktop layout that also visibly suggests a clean single-column mobile reading order.

Exact page content and hierarchy:

1. Top navigation: DEBIAS at left; STORY, EXPLORE, ABOUT, PAPER, CODE at right.
2. Hero eyebrow: A DATA STORY ABOUT REPRESENTATION.
3. Hero title, large and verbatim: “THE DOTS LINE UP. THE COVERAGE STILL VARIES.”
4. Hero deck, verbatim: “A high correlation is a useful first check — not proof of representative local coverage.”
5. Small metadata line: “4 DATA SOURCES • 331 LOCAL AREAS • ENGLAND & WALES • 2021”.
6. Cold-open section 01 titled “VIRTUALLY EQUAL POPULATIONS”. Show two elegant matched place cards: WATFORD, 102,246 residents, 2.37 active-account records per 100; NORTH EAST DERBYSHIRE, 102,001 residents, 15.89 active-account records per 100. Use two 10-by-10 icon grids or proportional bars. Central large callout: “NEARLY 7× DIFFERENT LOCAL RATES”. Small source note says these are Meta nighttime active-account estimates, not verified unique people.
7. Section 02 titled “THE REASSURING RESULT”. Show a clean scatterplot of census population versus active-account count, hundreds of small dots, the two named places highlighted, a fitted trend, large “PEARSON r = 0.91”, and a dark editorial callout: “THIS IS WHERE MANY CHECKS STOP.”
8. Section 03 titled “THE CORRELATION TRAP”. Show a clear two-state author-led animation storyboard within one panel: left “SAME LOCAL RATE EVERYWHERE” with dots tightly on one proportional line and r = 1.00; right “OBSERVED LOCAL RATES” with dots fanned vertically but still a strong trend and r = 0.91. Include a small accessible button cue “REPLAY EXPLANATION”. Main takeaway: “THE NATIONAL RELATIONSHIP REMAINS STRONG. THE LOCAL RATES SEPARATE.” No exposed formulas or slider.
9. Section 04 titled “PUT EVERY PLACE ON THE SAME SCALE”. Show scatter dots transforming into a horizontal distribution labelled “active-account records per 100 census residents”, with a vertical source-baseline marker at 8.09 and the two selected areas labelled at 2.37 and 15.89. Use arrows labelled “fewer records than baseline” and “more records than baseline”.
10. Section 05 titled “PUT THE DIFFERENCES BACK ON THE MAP”. Show an accurate-looking England-and-Wales hexagonal local-authority cartogram, not a full UK map. Use a balanced teal-to-neutral-to-coral scale labelled “FEWER RECORDS THAN BASELINE” to “MORE RECORDS THAN BASELINE”. Include four large source tabs: TWITTER/X, META selected, MULTI-APP 1, MULTI-APP 2.
11. Short high-impact dark band with verbatim text: “MORE APPS DID NOT AUTOMATICALLY MEAN MORE REPRESENTATIVE COVERAGE.”
12. Section 06 titled “CORRELATION IS A FIRST DIAGNOSTIC, NOT A CERTIFICATE.” Show a five-step audit with simple icons: CORRELATE, SCALE, LOCALISE, EXPLAIN, ADJUST WITH CARE.
13. Explorer card titled “EXPLORE A LOCAL AUTHORITY” with a search field, source tabs, a miniature comparison chart, and a “COPY SHARE LINK” button.
14. Lower research-information section titled “ABOUT THE RESEARCH”. Arrange concise cards for RESEARCH QUESTION, PUBLICATION, METHODS & LIMITATIONS, AUTHORS & TEAM, DATA & CODE, FUNDING & CONTACT. Include buttons READ THE PAPER and VIEW THE CODE.
15. Section titled “RESEARCH METRICS”. Show a tasteful “STUDY AT A GLANCE” row with large figures: 4 DATASETS, 331 LOCAL AREAS, 2021 CENSUS, r = 0.80–0.95. Beside or below, an “ARTICLE ATTENTION” area with restrained placeholders for ALTMETRIC, CITATIONS, and ON THE NEWS, clearly designed to update after publication.
16. Footer: CARMEN CABRERA • FRANCISCO ROWE; ROYAL SOCIETY OPEN SCIENCE; DEBIAS.

Style/medium: sophisticated editorial UI mock-up, data-journalism scrollytelling, crisp vector-like charts and typography, tactile paper texture, no photorealism.

Color palette: warm cream #F5F0E7, deep navy #0B1A2A, cobalt blue, teal, coral, muted pink, restrained orange. Accessible contrast.

Constraints: Treat visible numeric text as a design mock-up but render it as accurately as possible. Use England and Wales only in the main map. Make About the Research and Research Metrics feel integral and credible, not an afterthought. The narrative must visibly prioritize readership and media engagement: concrete cold open, immediate reversal, one memorable explanatory animation, clear local explorer, reusable press-ready graphics.

Avoid: old title “Who is missing from the map?”, old survey 22–70 comparison, full UK silhouette, decorative petal/radar charts, 3D graphics, dashboard clutter, excessive controls, autoplay-video appearance, tiny unreadable text, fake media logos, invented citation counts, claims about unique people, watermarks.

## Precision correction prompts

The first generated variant was corrected non-destructively in two image-edit passes:

1. Make the 100-icon fields encode approximately 2–3 filled icons for Watford and 16 for North East Derbyshire; remove unverified explorer values; change the publication card to “Cabrera, C. & Rowe, F. (2026). Royal Society Open Science. Accepted article.” Preserve all other content and styling.
2. In the explorer table, show only verified Meta values: fitted source baseline 8.09, Watford 2.37 and North East Derbyshire 15.89. Show dashes under the other three sources and retain “Values load from verified study data”. Preserve every other part of the page.
