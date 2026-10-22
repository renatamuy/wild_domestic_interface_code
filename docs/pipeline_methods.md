# LLM-assisted extraction pipeline: methods and provenance (v5.5)

> **Repository documentation, not an additional manuscript requirement.** This document explains how the deposited code was designed, what its outputs mean, and which steps depend on external source publications. It is supplementary *repository documentation* for readers inspecting or reusing the code. It is **not** a replacement for, or an additional set of required analyses beyond, the manuscript's Methods and supplementary material.

**Scope:** Original v5.5 extraction architecture, classification and scoring rules, diagnostics, and research provenance. The authoritative report of completed analyses, validation outcomes and scientific conclusions is the manuscript and its associated data.

**How to use this document:** Start with the [repository README](../README.md) for file locations, access limitations and the intended reproducibility route. This manual explains the *upstream* pipeline; the manuscript's downstream analyses are intended to run from frozen consolidated outputs, without rerunning PDF extraction.

**Production model noted in the original manual:** `gpt-5.6-terra` with low reasoning effort. This is a historical configuration record, not a guarantee that the same model identifier will remain accessible. The Ollama implementation is described as a fallback, not as a verified equivalent reproduction of production outputs.

**Important distinction:** R applies fixed scoring rules to structured model classifications. The calculation is deterministic given the classifications, but the classifications themselves may contain model errors and require quality assurance.

---


**Frozen scientific version:** v5.5  
**Primary production pipeline:** OpenAI API (`gpt-5.6-terra`, reasoning effort = `low`)  
**Local test pipeline:** local Ollama version with the same scientific extraction/scoring framework  
**Corpus target:** 270 PDFs  
**Primary connector:** `paper_id`

---

## 1. What this pipeline is trying to answer

The pipeline was deliberately reduced to the information needed for the synthesis rather than attempting to extract everything that could conceivably be said about each paper.

It has four main jobs:

1. **Position every paper in the main synthesis figure** using two conceptually separate axes:
   - **Host-domain Orientation (X):** which host domain is substantively centred in the study?
   - **Interface Centrality / Operationalisation (Y):** how far does the paper move from mentioning the wild–domestic interface to actually representing, measuring, and analytically using it?
2. **Create manuscript-level descriptive summaries** of study type/design, countries, host taxa/species, diseases/pathogens, agent types, and transmission modes.
3. **Extract interface-specific quantitative measures and conceptual evidence** for downstream summary and conceptual tables.
4. **Retain evidence and quality-assurance information for human auditing.**

A key principle is that **the LLM extracts structured evidence, while R deterministically calculates the two synthesis scores**. The LLM does not directly choose the final X or Y coordinate.

---

## 2. Overall workflow

```text
PDF filename
   |
   +--> paper_id (e.g., 34.pdf -> a34)
   |
   +--> master_table.xlsx / Records
   |       +--> authoritative Author
   |       +--> authoritative Title
   |       +--> Journal
   |       +--> DOI
   |
   +--> PDF text extraction + readability diagnostics
   |
   +--> three LLM modules
           |
           +--> descriptive metadata
           +--> interface evidence
           +--> host-domain roles
                    |
                    +--> deterministic R scoring
                    |       +--> Host-domain Orientation
                    |       +--> Interface Centrality
                    |
                    +--> evidence / quotations / quantitative measures
                    +--> audit and QA outputs
                    +--> Fig. 5-ready data
```

Each module is a separate model call, and publications are processed independently without conversational context carried over between papers.

---

## 3. Bibliographic metadata: one authoritative source

### Decision

**Author, Title, Journal and DOI are not inferred from the PDF or extracted by the LLM.**

The pipeline derives `paper_id` from the PDF filename and joins it to the `Records` sheet of `master_table.xlsx`.

Example:

```text
34.pdf -> a34 -> master_table.xlsx -> correct bibliographic metadata
```

### Reasoning

PDF metadata are often dirty, incomplete, or describe a parent book rather than the retrieved chapter. DOI regex extraction can also introduce unnecessary errors. The master table already contains curated bibliographic information.

### Safeguards

Before model processing, the script checks that:

- `paper_id` exists in the master table;
- `paper_id` is unique;
- every selected PDF has a unique master-table match.

A missing or duplicate match stops the relevant workflow rather than silently guessing.

---

## 4. What the LLM extracts

### Module A — descriptive metadata

This module extracts only manuscript-relevant descriptive information for a publication:

- study type;
- study design;
- countries investigated;
- wildlife taxa;
- domestic taxa;
- wildlife species;
- domestic species;
- diseases/pathogens evaluated by the paper;
- disease agent type;
- supported transmission modes.

The prompt explicitly tells the model to distinguish **what the focal paper itself did** from what cited studies did.

### Module B — interface evidence

This is the evidence base for the Y-axis and for conceptual/quantitative outputs.

The model answers eight Boolean criteria:

1. `interface_mentioned`
2. `interface_in_objectives`
3. `conceptual_framing`
4. `qualitative_representation`
5. `quantitative_variable_present`
6. `variable_used_in_analysis`
7. `interface_specific_question_addressed`
8. `interface_explicit`

For every criterion, the output structure contains its **own evidence quotation, page, and section**.

The module also extracts:

- interface variable names (that are manually aggregated on code downstream);
- terminology used by the authors;
- whether the interface statement is explicit or implicit;
- a representative verbatim interface statement or passage and its location;
- interface-specific quantitative measures and their analytical roles/results.

### Module C — host-domain roles

The model classifies, non-exclusively:

```text
wildlife_role
domestic_role
```

Each must be one of the following:

- `ABSENT`
- `CONTEXTUAL`
- `SECONDARY`
- `PRIMARY`
- `UNCLEAR`

For reviews, modelling studies, and conceptual papers, `PRIMARY` does **not** require physical sampling. It means a central substantive focus of the study, synthesis, analysis, or interpretation.

---

## 5. Host-domain Orientation: the X-axis

### Scientific question

> Relative to the other host domain, how central are wildlife and domestic animals to what this paper actually studies, analyses, or synthesises?

The model supplies the two host roles. **R applies the scoring rules.**

| Index | Interpretation | Operational logic |
|---:|---|---|
| **-1.00** | Exclusively wildlife-focused | Wildlife substantive; domestic absent |
| **-0.75** | Predominantly wildlife-focused | Wildlife substantive; domestic contextual only |
| **-0.50** | Wildlife-centred interface | Wildlife primary; domestic secondary |
| **0.00** | Balanced host focus | Both primary, or both substantive at a comparable secondary level |
| **+0.50** | Domestic-centred interface | Domestic primary; wildlife secondary |
| **+0.75** | Predominantly domestic-focused | Domestic substantive; wildlife contextual only |
| **+1.00** | Exclusively domestic-focused | Domestic substantive; wildlife absent |

If either role is missing/unclear, or a combination does not satisfy a defined rule, the score is `NA`.

### Important distinction

`CONTEXTUAL` is not the same as `SECONDARY`.

- **Contextual:** the other host domain is mainly background, a potential source/recipient, or contextual factor.
- **Secondary:** it genuinely contributes to the study/synthesis/analysis, but is subordinate to the primary host domain.

This distinction is what separates ±0.75 from ±0.50.

### Balanced does not mean uncertain

`0` is a substantive scientific classification. Uncertainty is retained as `NA`; it is never converted to the centre of the figure.

---

## 6. Interface Centrality / Operationalisation: the Y-axis

### Scientific question

> How far does the paper move from merely invoking the wild–domestic relationship to representing it empirically and using it analytically?

| Score | Interpretation | Meaning |
|---:|---|---|
| **-1.00** | Introductory mention | Interface/relationship is mentioned but not substantively represented later on. Values here expected to be zero due to human screening levels. |
| **-0.50** | Conceptual framing | Interface structures objectives, rationale, or interpretation but is not translated into an observable/quantified representation |
| **0.00** | Qualitative representation | Observable interface settings, contacts, shared resources, behaviours, or relationships are represented qualitatively |
| **+0.50** | Quantitative representation | A measurable interface exposure/category/variable is quantified |
| **+1.00** | Interface operationalised | An interface exposure/category/variable is used in a comparison, model, association test, effect estimate, transmission analysis, or other quantitative analysis |

### Critical decision: operationalisation can be implicit

The authors do **not** need to use the word "interface" for scoring.

Examples that can operationalise the interface features include:

- shared versus non-shared sites;
- mixed-host versus single-host settings;
- wildlife presence/absence on livestock properties;
- spatial overlap or proximity;
- contact rates;
- shared water/feed resources;
- exposure categories based on wildlife–livestock contact;
- comparative pathogen infection, prevalence, shedding, or transmission across host domains when the comparison represents their ecological relationship.

This decision was important because papers can operationalise the relationship scientifically without labelling their variable "interface".

### Paper 34 as a development example

Paper 34 was used during development and is provided here as example. It used sheep and guanaco in shared/non-shared or mixed/single-host contexts and analysed MAP-related outcomes.

An earlier version of prompts classified it too low because the model found the relevant evidence but placed it into the wrong structured field. v5.5 therefore:

- gives every interface criterion its own evidence fields;
- explicitly defines implicit operationalisation;
- recognises categorical variables such as shared/non-shared sites as quantitative interface representations;
- prevents incomplete structured extraction from becoming a confident low score.

This is why paper 34 became a useful validation case for the final Y-axis logic.

---

## 7. The Interface Centrality

Seven criteria are used to calculate the Y score:

- interface mentioned;
- interface in objectives;
- conceptual framing;
- qualitative representation;
- quantitative variable present;
- variable used in analysis;
- interface-specific question addressed.

Before scoring, R checks whether all seven have resolved logical TRUE/FALSE values.

If any is unresolved:

```text
interface_operationalisation = NA
interface_label = "Insufficient classification evidence"
interface_score_complete = FALSE
interface_needs_review = TRUE
```

This prevents a structurally incomplete LLM answer from being silently interpreted as "introductory" or "conceptual".

`interface_explicit` is retained as useful descriptive evidence but is **not required for a high score**. An implicitly operationalised interface can therefore score highly.

---

## 8. Why the two axes are independent

The main figure is intended to separate two questions that are often conflated.

### X asks:
**Which host domain is centred?**

### Y asks:
**How deeply is the relationship between the domains represented/operationalised?**

Therefore:

- an empirical wildlife study can still have low Interface Centrality;
- a domestic-centred study can strongly operationalise the interface;
- a balanced study is **not automatically highly operationalised**;
- collecting any data alone does not produce a high Y score.

This independence is one of the main interpretive strengths of the synthesis figure (code downstream).

---

## 9. Fig. 5 interpretation

The plotting script does not recalculate either scientific score. It simply plots:

```r
x = host_orientation
y = interface_operationalisation
```

Positive X = more domestic-centred.  
Negative X = more wildlife-centred.  
X near 0 = balanced host focus.

Higher Y = greater interface representation/operationalisation.

The visual quadrants are labelled more cautiously as:

- **Higher interface centrality · Wildlife-centred**
- **Higher interface centrality · Domestic-centred**
- **Lower interface centrality · Wildlife-centred**
- **Lower interface centrality · Domestic-centred**

This avoids calling every paper above the visual dividing line fully "operationalised" or every paper below it purely "conceptual". The five Y-axis categories retain the precise interpretation.

---

## 10. Quantitative interface measures

The pipeline deliberately does **not** extract every number reported by a study.

It extracts a quantitative measure only when it directly:

- characterises;
- compares;
- tests;
- models; or
- otherwise operationalises

the wild–domestic relationship.

For each qualifying measure, it attempts to retain:

- measure name;
- what it represents;
- measurement method;
- metric/unit;
- analytical role;
- one concise related quantitative result;
- supporting quotation;
- page;
- section.

This output supports a manuscript table of quantitative approaches used to study the interface.

A paper being quantitative is not enough. Its quantitative measure must be relevant to the interface features.

---

## 11. Conceptual evidence and verbatim statements

For each paper, the pipeline retains one best/most representative verbatim statement showing how the paper frames, represents, or operationalises the wild–domestic relationship for definitions discussions.

It also records whether that statement is:

- `explicit`;
- `implicit`; or
- `none`.

These fields support the planned conceptual table, especially for:

- papers high in different Figure 5 quadrants;
- papers near the balanced host-domain centre;
- papers with high Interface Centrality;
- papers that explicitly or implicitly articulate what the interface means.

The quotation is evidence to inspect and enrich discussions around definition and conceptualisation.

---

## 12. Evidence grounding

The pipeline attempts to check whether extracted quotations can be located in the page text and records a grounding status.

During pilot testing, some highly plausible paper-specific quotations were labelled `NOT_FOUND`. The working interpretation is that the text matcher can be sensitive to PDF artefacts such as whitespace, line breaks, hyphenation, or encoding.

Therefore:

- `NOT_FOUND` is a **QA flag**, not automatic proof of hallucination or evidence of successful verification;
- flagged quotations can be manually checked against the PDF;

---

## 13. PDF readability QA

The pipeline calculates PDF text-quality diagnostics and retains `pdf_text_status`.

A paper can therefore be flagged as difficult before interpreting apparent model failures.

This distinction matters:

```text
bad extraction/readability
        versus
model classification problem
        versus
R scoring problem
```

`pdf_text_status = OK` indicates that automated readability diagnostics did not identify a problem; it does not establish that all relevant text was extracted or correctly interpreted. Consult the manuscript and quality-control records for the actual inspection and rerun outcomes.

---

## 14. Human audit

The pipeline supports a stratified human audit. The final number of reviewers, completed assessments, agreement estimates, adjudications, and any changes incorporated into the consolidated dataset should be reported from the completed validation records and manuscript Methods; the sampling design below is not itself a report of completed validation.

### Main classification audit

The pipeline creates an audit table containing:

- human-readable question;
- definition;
- AI value;
- evidence quotation;
- page;
- grounding status;
- reviewer value;
- reviewer agreement;
- reviewer notes;
- final human value.

The documented sampling procedure targets **100 audit rows** using a fixed seed (`42`).

The sample is stratified to:

1. represent 100 individual variables for the qualitative review;
2. and **50/50 split between Interface and Host constructs**;
3. top up from remaining eligible rows if needed.

Important: **100 refers to audit criterion rows, not necessarily 100 unique papers.**

For Interface rows, the human reviewers use:

```text
TRUE / FALSE / UNCLEAR
```

For host-role rows:

```text
ABSENT / CONTEXTUAL / SECONDARY / PRIMARY / UNCLEAR
```

### Quantitative-measure audit

The documented procedure separately samples up to **100 extracted quantitative interface measures and their supporting evidence (50+50)** for human verification. The completed sample size and findings must be checked against the archived audit files.

This is important because verifying a Boolean classification and verifying a quantitative extraction are different tasks.

### What the human reviewer should ask

For every audited item:

1. Does the model's classification match the paper?
2. Does the quotation genuinely support that classification?
3. Is the page correct?
4. Is the model describing the focal paper rather than a cited study?
5. For host roles, is `CONTEXTUAL` being distinguished from genuine `SECONDARY` inclusion?
6. For interface criteria, has an implicit operationalisation been recognised?
7. For quantitative measures, is the measure actually about the interface rather than merely a number reported by the paper?

---

## 15. What should trigger manual review

Further inspection on any flags:

- `interface_needs_review = TRUE`;
- `interface_score_complete = FALSE`;
- `host_orientation = NA`;
- `pdf_text_status` indicating poor readability;
- `any_parse_error = TRUE`;
- evidence inconsistencies;
- `grounding_status = NOT_FOUND`;
- biologically implausible or internally contradictory classifications;
- quantitative measures that conflict with the Boolean interface classification (none found in final agreement auditing).

Also inspect unusual/extreme papers that will carry substantial interpretive weight in the manuscript, even if they are not automatically flagged.

---

## 16. Repeatability versus validity

The pipeline was not designed around running all 270 papers more than once.

The preferred validation logic is:

1. **small runs** during development to assess LLM repeatability;
2. **human audit** to assess whether classifications/evidence are scientifically defensible;
3. targeted manual review of QA flags and influential papers.

Running the full corpus twice would approximately double model cost and produce a second corpus requiring reconciliation without replacing the need for human auditing.

---

## 17. Checkpointing

The frozen production versions save scientific checkpoints **after every successfully completed paper**.

A persistent processing log records:

- `paper_id`;
- filename;
- completion/error status;
- start/end time;
- elapsed time;
- model;
- prompt tokens;
- output tokens;
- estimated API cost;
- error message.

With `resume = TRUE`, completed papers are recognised from the checkpoint and skipped on restart.

Conceptually:

```text
paper 1 -> process -> save
paper 2 -> process -> save
...
computer/network interruption
...
restart -> skip completed papers -> continue
```

---

## 18. Model and cost accounting

The frozen OpenAI script records model usage per paper and produces:

- per-paper benchmark output;
- run-level benchmark summary;
- mean/median processing time;
- prompt/output token totals;
- estimated API cost;
- projected full-corpus runtime/cost.

The price constants in the script are assumptions used for estimation and should be checked if pricing changes.

A local Ollama implementation was considered as a fallback. It should be labelled an alternative implementation rather than a source of production results unless its use in the final corpus is documented.

---

## 19. Main output types

The pipeline classifies and exports data for:

### Paper summary
One row per paper with bibliographic metadata, descriptive synthesis fields, X/Y scores, QA metrics, timing, tokens, and estimated cost.

### Plot data
The fields required for Fig. 5, including `paper_id`, host orientation, interface operationalisation, and identifiers for joining other bibliometric information.

### Disease long table
One disease/pathogen/transmission representation per relevant record.

### Quantitative measures long
One row per extracted interface-specific quantitative measure.

### Conceptual evidence
Paper-level X/Y classifications plus the best interface statement and bibliographic identifiers.

### Evidence long
Criterion-specific quotations/pages/sections and grounding status.

### Audit
Exported tables with criterion rows.

### Module information
Module-level raw outputs, parsing status, context size, runtime, token estimates, and completion metrics.

### PDF quality
Text-extraction/readability diagnostics.

### Benchmark
Per-paper and run-level runtime/token/cost/QA summaries.

### Processing log
Persistent completion/error history for safe restart.

---

## 20. Pipeline summary

> We used an LLM as a structured evidence extractor rather than allowing it to directly score papers. Each PDF was linked by paper ID to our curated bibliographic master table. Separate model calls extracted descriptive study information, evidence about how the wild–domestic interface was represented, and the relative role of wildlife and domestic hosts. We then converted those structured outputs into two deterministic indices in R: Host-domain Orientation, from wildlife-centred to domestic-centred, and Interface Centrality, from introductory mention to analytical operationalisation. Operationalisation could be explicit or implicit, so measurable constructs such as shared versus non-shared sites or mixed-host settings counted even if authors did not call them an 'interface'. Every interface criterion retained supporting quotations and pages, incomplete classifications were not forced into a score, and a stratified subset plus quantitative measures was prepared for human audit.

---

## 21. How to explain why an LLM was useful

The LLM is being used where conventional keyword rules are weak:

- recognising that different wording can represent the same interface concept;
- distinguishing contextual host mentions from substantive inclusion;
- recognising implicit operationalisation;
- extracting concise evidence from heterogeneous papers;
- standardising information across a large corpus.

But interpretive control remains outside the model:

- bibliographic metadata come from the curated master table;
- scoring rules are explicit R code;
- incomplete classifications become missing rather than guessed;
- evidence is retained;
- human audit is built into the workflow.

---

## 22. FAQ

### "Did ChatGPT decide where each paper goes in the figure?"
Not directly. The model extracts predefined evidence. R applies fixed scoring rules to generate X and Y.

### "Does a quantitative paper automatically score highly on Interface Centrality?"
No. Only quantitative variables and categories that represent the wild–domestic interface features count.

### "Do authors have to explicitly say 'wildlife–livestock interface'?"
No. The framework intentionally recognises implicit operationalisation.

### "Why isn't uncertainty scored as zero?"
Because zero has substantive meanings on both axes. Uncertainty is retained as `NA`.

### "How do you know the model did not hallucinate?"
The workflow retains quotations/pages, grounding flags, PDF-quality flags, parsing/completion diagnostics, and human audit fields. A grounding flag is not treated as human verification.

### "Why not run the model twice on all papers?"
Repeatability and validity are different. A repeated subset can assess model consistency; human audit is still required to assess correctness.

### "Why use the master table?"
It prevents unreliable PDF metadata or model inference from contaminating bibliographic identifiers.

---

## 23. How to read Fig. 5 

Do not interpret the figure as a simple "good paper / bad paper" ranking.

A paper low on Y may be scientifically strong but not operationalise the wild–domestic relationship. A paper far left or right may appropriately focus on one host domain.

Instead ask:

1. **Where has the literature concentrated its host-domain attention?**
2. **How often is the interface merely invoked versus actually represented or tested?**
3. **Where are the genuinely integrative studies?**
4. **Are balanced studies also operationalised, or merely inclusive in terminology?**
5. **Which regions of conceptual space are sparse?**
6. **What methodological approaches characterise papers with high Interface Centrality?**

The figure is therefore a map of **how the literature conceptualises and studies the interface**, not a quality score.

---

## 24. Interpretation cautions

- Host-domain Orientation is about **study emphasis**, not host importance in nature.
- Interface Centrality is about **how the paper operationalises the relationship**, not overall methodological sophistication.
- Explicit terminology and actual operationalisation are different.
- Mentioning many wildlife/domestic species does not by itself establish balanced host focus.
- Collecting data does not by itself establish interface operationalisation.
- A review can have substantive host roles without sampling animals.
- A high Y score can be based on categorical interface constructs, not only continuous metrics.
- Automated evidence grounding remains a QA aid and should not be described as human verification.

---

## 25. Frozen scientific decisions

The following should be treated as frozen for the production corpus unless a genuine systematic failure is discovered:

- master-table bibliographic join by `paper_id`;
- three-module extraction architecture;
- host-role categories;
- seven-level Host-domain Orientation scoring;
- explicit + implicit interface logic;
- five-level Interface Centrality scoring;
- incomplete-interface fail-safe;
- interface-specific quantitative extraction;
- criterion-specific evidence;
- audit structure.

Engineering changes (e.g., file writing or logging) can be made without changing the scientific framework, but any future change to prompts, definitions, schemas, or scoring rules should be documented as a new scientific version and evaluated before mixing outputs.

---

## 26. Files to preserve for research provenance

At minimum, archive together:

- frozen OpenAI v5.5 pipeline;
- frozen Ollama v5.5 fallback;
- Fig. 5 script;
- manuscript summary-table scripts;
- `master_table.xlsx`;
- bibliographic identifiers for the source corpus; retain the original PDFs only in a separately controlled private research archive where permitted, not in the distributed repository;
- final checkpoint/final CSV outputs;
- audit samples and completed human-audit files;
- processing log;
- benchmark outputs;
- a record of the model name and run date;
- this pipeline methods document.

If changes become necessary to the pipeline, create a new version (no overwriting scripts).

---

## 27. Final mental model

The easiest way to understand the workflow is:

```text
MODEL = reads and extracts evidence
R     = applies transparent rules
HUMAN = validates interpretation
```

Or, more specifically:

```text
What hosts matter?          -> Host roles -> X
What does the paper do
with the interface?         -> Interface criteria -> Y
What supports those calls?  -> Quotes/pages/quantitative evidence
Can we trust the corpus?    -> QA flags + stratified human audit
```

That is the logic to keep in mind when interpreting the synthesis.
