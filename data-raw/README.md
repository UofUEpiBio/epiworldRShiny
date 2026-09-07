# data-raw

This directory contains scripts for downloading and preparing school data used
in the measles model school selector.

## Scripts

### `02_download_and_combine.R`

Downloads school vaccination data from the
[epiENGAGE measles dashboard](https://github.com/TACC/measles-dashboard)
(TACC), combines state-level CSV files, appends Utah-specific data, and writes
the final dataset to `inst/extdata/schools_measles.csv`.

Run via `make update-other-data` (or `make update-data`, which also prints
instructions for the Utah step).

### `01_utah_school_data.R`

Processes Utah-specific school data from a locally provided raw file and writes
it to `data-raw/01_utah_school_data.csv`, which is then consumed by
`02_download_and_combine.R`.

## Utah source data (`measles_school_data_final.csv`)

The file `data-raw/measles_school_data_final.csv` is **not tracked in git**
(see `.gitignore`). It must be obtained directly from the
**Utah Department of Health and Human Services (DHHS)** before running
`01_utah_school_data.R` or `make update-utah-data`.

Once you have obtained the file, place it at
`data-raw/measles_school_data_final.csv` and then run:

```bash
make update-utah-data
make update-other-data
```
