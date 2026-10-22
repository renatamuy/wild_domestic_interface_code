# Muylaert et al. (submitted) Wildlife–Domestic Animal Interfaces: Evidence Synthesis and Reproducible Analyses

> **Repository documentation for code users and peer reviewers.** This README explains the deposited research files and how to interpret or reuse them. The extended [pipeline methods](docs/pipeline_methods.md) document records technical decisions made during code development. **Neither document is intended as additional evidence essential to understanding the paper, nor does it replace the manuscript's Methods, figures or supplementary data.** The manuscript remains the authoritative scientific account.

## Purpose

This repository documents an LLM-assisted evidence synthesis of publications addressing wildlife–domestic animal interfaces. It is intended to make the extraction logic, data harmonisation, validation workflow and downstream analysis code inspectable by reviewers and other researchers.

**Two distinct reproducibility routes should not be confused:**

- **Downstream analysis:** The intended route for reproducing manuscript tables and figures uses the frozen, consolidated datasets and R scripts. It should not require access to article PDFs or an LLM API.
- **Upstream extraction:** The v5.5 pipeline documents how structured evidence was extracted from publications. Re-execution requires legally obtained source PDFs, a compatible model/API environment and appropriate credentials. Model-dependent outputs may differ between runs.

The technical account of the extraction modules, scoring definitions, evidence checks and limitations is in **[Pipeline Methods — v5.5](docs/pipeline_methods.md)**. This linked document is provided for *repository transparency and reuse*, not because a reader must study it to follow the manuscript's main results.

## Repository map

The following paths reflect the **currente tepository**:

| Location | Role |
| --- | --- |
| `script/05_pipeline_synthesis_lean_v5_5_OPENAI_WEEKEND_SAFE_FROZEN.R` | Original production extraction implementation; **not required** for downstream reproduction |
| `script/06_pipeline_synthesis_lean_v5_5_OPENAI_RERUN_a52_a122.R` | Targeted extraction quality-control rerun; retained for provenance |
| `script/07_MERGE_targeted_QC_rerun_a52_a122.R` | Targeted rerun merge/consolidation step; retained for provenance |
| `script/09_summary_tables.R` | Manuscript summary-table analysis script; inputs and execution still to be verified |
| `script/12_Figure_04_Interface_Figure_AB.R` | Wildlife–domestic host-group and disease-system Figure 4 code |
| `results_synthesis_consolidated/` | Frozen/derived datasets, QA files and analysis outputs; public-shareable subset still to be determined |
| `dictionary/` | Curated host and disease dictionaries |
| `master/` | Bibliographic metadata and study-level reference materials; review before public release |
| `docs/pipeline_methods.md` | Detailed scientific and technical explanation of the upstream extraction framework |

## Quick Start: downstream analyses

This section includes codes to:

1. Obtain the curated repository release containing the final, shareable consolidated datasets.
2. Open the R project or start R at the repository root.
3. Install the package dependencies declared.
4. Run the documented manuscript analysis and figure scripts in order, using project-relative input paths.
5. Provide generated tables and figures with the archived outputs and manuscript.

## How the extraction pipeline works

Each publication was associated with a curated bibliographic record through `paper_id`. PDF text was processed by three separate LLM modules covering (1) descriptive study metadata, (2) evidence of interface representation and operationalisation, and (3) wildlife/domestic host-domain roles.

The LLM generated structured classifications and supporting evidence; R then applied predefined rules to calculate two indices:

- **Host-domain Orientation (X):** relative substantive emphasis on wildlife versus domestic animals.
- **Interface Centrality / Operationalisation (Y):** extent to which the interface is mentioned, represented, measured and used analytically.

The indices are **not measures of study quality**, and deterministic scoring does not establish that upstream classifications are error-free. The full definitions, rules, examples and interpretation cautions are documented in [Pipeline Methods](docs/pipeline_methods.md#5-host-domain-orientation-the-x-axis) and [the Y-axis section](docs/pipeline_methods.md#6-interface-centrality--operationalisation-the-y-axis).

## Human validation and quality assurance

The pipeline contains evidence-location fields, PDF readability diagnostics, parsing/completion flags and sampling routines for human audit. The [pipeline methods manual](docs/pipeline_methods.md#14-human-audit) describes the audit design and review criteria.

**Audit design is not the same as completed validation.** Final reviewer counts, agreement estimates, adjudications and any manual corrections should be taken from the completed validation records and the manuscript Methods and Results.

## Source PDFs, API access and rights

Reviewed publication PDFs are **not distributed** in this repository. Access to publications through a university or publisher subscription does not itself authorise redistribution. Bibliographic metadata and identifiers should be used to locate source documents independently.

The extraction code require API credentials and an available model version. The original model configuration is recorded for provenance.

## Citation, licence and availability

A Zenodo DOI, citation metadata and licences will be added upon manuscript acceptance.