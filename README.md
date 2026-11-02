# The Triple-Win Potential of Sustainable Mobility

David Benatia, Gabrielle Beaudin and Muriel Julien. *Cities* 173 (2026), 106917.

[Paper](https://doi.org/10.1016/j.cities.2026.106917) · [Author website and materials](https://davidbenatia.com/research/)

R code for the analysis of modal shifts, transportation costs and emissions in Quebec.
The repository contains the analysis scripts and two reference result tables.

## Reproduce the tables

Requires R ≥ 4.2 and `dplyr`, `broom`, `sandwich`, `lmtest`, `ggplot2`, `scales`,
`stargazer`, `FNN` and `tidyr`.

Download the derived-data ZIP from the [paper materials on the author website](https://davidbenatia.com/research/), extract it into this repository (creating `data/intermediate/`), then run:

```sh
Rscript run.R --from 5
```

Outputs are written to `tables/gains.tex` and `tables/reg_modes.tex`.
Compare them with `paper_tables/`. For full-pipeline instructions, see [DATA.md](DATA.md).

## Data access

Derived replication datasets are distributed separately through Dropbox; they are
not committed to GitHub. The original origin-destination survey data are excluded.
See [DATA.md](DATA.md) for the distinction and full-pipeline inputs.
