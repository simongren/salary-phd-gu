# PhD Salary Evolution — Göteborgs Universitet (2022–2025)

Visualising negotiated PhD salary progression at GU across PhD groups, levels, and years.

## Structure

```
documents/   — Source PDFs (one per year, 2022–2025)
data/        — Extracted CSV data (salaries.csv)
scripts/     — Python extraction & validation scripts
R/           — ggplot2 visualisation code
R/output/    — Generated charts
```

## Workflow

1. **Extract** — `python scripts/extract_salaries.py` → `data/salaries.csv`
2. **Validate** — `python scripts/validate_salaries.py`
3. **Visualise** — `Rscript R/plot_salaries.R` → `R/output/`

## Data notes

Salaries are negotiated per faculty/department group. Each group has up to 4 levels:
- Entry (0%)
- After 50% completion
- After 80% completion
- After 100% (defended)

## Requirements

- Python ≥ 3.8 with `pdfplumber`, `pandas`
- R ≥ 4.0 with `tidyverse`, `scales`, `ggtext`
