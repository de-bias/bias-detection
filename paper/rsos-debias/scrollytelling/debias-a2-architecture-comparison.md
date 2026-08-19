# Alternative A.2 architecture comparison

## Shared story

Both alternatives retain the complete public argument on the Story page:

1. virtually equal populations can produce very different local identifier rates;
2. raw counts can still correlate strongly with census population;
3. local per-resident rates reveal what one correlation compresses;
4. the geographical and local-rate pattern differs by source;
5. several area-level associations are nonlinear; and
6. an adjustment calibrated for one source should not be assumed to transfer to another without independent validation.

The Story therefore does not require a reader to navigate elsewhere to understand the finding, its principal limitations or its methodological implication.

## Alternative 1 — Integrated single page

The Story is followed directly by:

- Explore a local authority;
- About the Research accordions;
- Study at a glance; and
- paper and code links.

### Advantages

- Everything is available without leaving the page.
- Suitable for a single self-contained feature or personal-site project page.
- Reduces the risk that readers overlook the About material.

### Limitations

- The research tail weakens the decisiveness of the final story conclusion.
- The page becomes substantially longer on phones.
- About, methods and metrics become compressed into footer-sized content.
- Detailed source definitions and limitations are harder to make legible without reintroducing clutter.

## Alternative 2 — Split Story and Research pages

The Story ends immediately after the source-specific-validation conclusion. A compact credibility band retains:

- 4 datasets;
- 331 local authority areas;
- 2021 census benchmark;
- accepted Royal Society Open Science status;
- one limitations sentence; and
- direct links to Explore, About and Methods, Study at a glance, paper, data, code and media assets.

The dedicated Research page contains:

- About the Research;
- Study at a glance;
- research question and contribution;
- source definitions and observation windows;
- methods and limitations;
- paper, authorship and contributions;
- funding, partners and contacts;
- data, code and supplementary materials; and
- ethics and privacy.

### Advantages

- The Story retains a strong, memorable ending.
- Research information has enough space to remain readable and properly qualified.
- Journalists receive stable, indexable anchors for methods, sources, authors, contacts and media assets.
- The initial Story payload and mobile page length are lower.
- The Research page can be updated after publication without disturbing the narrative.

### Limitations

- Research details require one additional navigation action.
- Strong cross-linking and a clear return-to-story route are essential.
- The Research route must not contain empty metadata or placeholder impact metrics at launch.

## Recommended page allocation

Use Alternative 2 for the final site.

| Route | Purpose | Content |
|---|---|---|
| `/story/` | Main scrollytelling narrative | Complete finding, essential caveats, source-specific-validation conclusion, credibility teaser |
| `/explore/` | Local utility | Local-authority search, stable area URLs, share cards, accessible tables |
| `/research/` | Research credibility and documentation | About, Study at a glance, sources, methods, limitations, authors, funding, ethics, data/code, contacts, media assets |
| Publisher DOI | Formal article | Accepted/published paper and supplementary material |
| Code repository | Reproducibility | Analysis code, derived data and documentation |

The header should expose **Story**, **Explore**, **Research** and **Paper** on every route. The Story conclusion should link directly to `/research/#sources`, `/research/#methods`, `/research/#study-at-a-glance` and `/research/#media-assets`.

## Research Metrics recommendation

Use **Study at a glance** as the visible section title. “Research Metrics” can remain a navigation or metadata label, but it is easily confused with citations and online attention.

Display at launch:

- 4 mobile-app datasets;
- 331 local authority areas;
- 2021 census benchmark; and
- raw-count Pearson correlations of approximately 0.80–0.95, only after final recalculation and clearly labelled as calculations from the released LAD data—not representativeness scores.

Keep Article Attention absent until verified post-publication values exist. When added, place it separately on the Research page with named sources and a last-updated date.

## Review result

After the rendered-mockup review:

| Architecture | Readership | Completion | Media reuse | Credibility |
|---|---:|---:|---:|---:|
| Integrated single page | 8.5/10 | 7.5/10 | 8/10 | 8.5/10 |
| Split Story and Research | 9/10 | 9/10 | 9/10 | 9.5/10 |

These are editorial assessments rather than measured outcomes. The split architecture should still be tested on phones and with non-specialist readers before implementation is frozen.

## Production caveat

The mock-ups define architecture, hierarchy and copy placement. Production scatterplots, rate strips, maps and nonlinear curves must be generated from the verified released data and model outputs. The current mock-up marks are not substitutes for data-bound figures.
