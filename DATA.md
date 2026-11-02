# Inputs and replication

## Table reproduction

Stage 5 expects these author-held study inputs relative to this repository:

- `data/intermediate/pred_df.rds`
- `data/intermediate/regdf.rds`
- `data/intermediate/temp.rds`

These are derived study datasets. The [Dropbox download](https://www.dropbox.com/scl/fi/ixivggnbouh93bb0nd2jx/sustainable-mobility-derived-data.zip?rlkey=rhqu6oyxam1kds04gtgnmckt0&dl=0) contains
all six pre-built intermediate RDS files, including these three stage-5 inputs.
The original survey microdata and raw routing inputs are not included in that download.

Extract the ZIP into the repository root; its entries start with `data/intermediate/`.
The derived data are enough for stage 5. Rebuilding stages 0–4 requires the
additional author-held/provider inputs listed below.

## Full pipeline

`Rscript run.R` executes stages 0–5. Stages 0–4 rebuild intermediate files from:

- `data/raw/df_google.rds` and `data/raw/CFS_MTL.rds`;
- Statistics Canada census CSVs in `data/raw/census/`;
- city-level cost parameters in `data/cost_params/`.

Additional R dependencies: `readr`, `geosphere`, `VIM`, `caret`, `magrittr`, `ranger`.
Routing inputs were collected in March 2024. They are frozen inputs; no API key is
included. Stage 4 sets seed 42. The table analysis uses `target = 5` minutes.
