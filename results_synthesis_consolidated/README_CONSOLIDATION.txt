RESULTS SYNTHESIS CONSOLIDATED
==============================

Purpose
-------
This directory contains the consolidated evidence-synthesis dataset after
targeted source-document quality control for papers a52 and a122.

Source datasets
---------------
1. results_synthesis/ = frozen original 270-paper production run.
2. results_synthesis_rerun_a52_a122/ = targeted rerun using replacement
   machine-readable PDFs for a52 and a122 with the same frozen extraction
   and deterministic classification workflow.

Consolidation rule
------------------
For substantive full-corpus output tables, all original rows belonging to
a52/a122 were removed and replaced with the corresponding targeted-rerun
rows. All other papers remain from the original production run.

Validation
----------
Original human validation/adjudication is preserved unchanged in
validation_original/. Rerun audit samples were not merged and the original
validation estimate is not recalculated.

QC provenance
-------------
QC_a52_a122/ contains original and rerun target rows, comparison tables,
the merge log, and original/rerun run-level processing provenance.

Downstream products
-------------------
Previously generated Figure 5 tables/figures and manuscript summary products
were NOT copied because they may be stale after a52/a122 replacement.
Regenerate them from the consolidated synthesis outputs.

Created: 2026-09-29 13:01:01 AEST
