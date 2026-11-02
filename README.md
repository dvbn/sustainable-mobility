# The Triple-Win Potential of Sustainable Mobility

David Benatia, Gabrielle Beaudin and Muriel Julien. *Cities* 173 (2026), 106917.

[Published paper](https://doi.org/10.1016/j.cities.2026.106917) · [Working paper PDF](https://www.dropbox.com/scl/fi/zr37rnwug1befulqebug3/sustainable-mobility-working-paper-2025-10-29.pdf?rlkey=9pi4ygw4r0gg7et12ymzoi88c&dl=0) · [Derived data](https://www.dropbox.com/scl/fi/ixivggnbouh93bb0nd2jx/sustainable-mobility-derived-data.zip?rlkey=rhqu6oyxam1kds04gtgnmckt0&dl=0) · [Author website](https://davidbenatia.com/research/)

R code for the analysis of modal shifts, transportation costs and emissions in Quebec.
The repository contains the analysis scripts and two reference result tables.

## Reproduce the tables

Requires R ≥ 4.2 and `dplyr`, `broom`, `sandwich`, `lmtest`, `ggplot2`, `scales`,
`stargazer`, `FNN` and `tidyr`.

Download the derived-data ZIP from the [Dropbox](https://www.dropbox.com/scl/fi/ixivggnbouh93bb0nd2jx/sustainable-mobility-derived-data.zip?rlkey=rhqu6oyxam1kds04gtgnmckt0&dl=0), extract it into this repository (creating `data/intermediate/`), then run:

```sh
Rscript run.R --from 5
```

Outputs are written to `tables/gains.tex` and `tables/reg_modes.tex`.
Compare them with `paper_tables/`. For full-pipeline instructions, see [DATA.md](DATA.md).

## Data access

Derived replication datasets are available on [Dropbox](https://www.dropbox.com/scl/fi/ixivggnbouh93bb0nd2jx/sustainable-mobility-derived-data.zip?rlkey=rhqu6oyxam1kds04gtgnmckt0&dl=0); they are
not committed to GitHub. The original origin-destination survey data are excluded.
See [DATA.md](DATA.md) for the distinction and full-pipeline inputs.
