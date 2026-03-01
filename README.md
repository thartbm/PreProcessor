# PreProcessor

A Shiny app for pre-processing experimental CSV data to make it suitable for statistical software.

## Features

- **Upload** a CSV file (header row required; no index column)
- **Assign columns** to four roles:
  - *Participant ID* – one column identifying each participant
  - *Information columns* – any number of descriptive participant columns
  - *Independent variables* – up to 4 discrete / categorical factors
  - *Dependent variables* – up to 3 continuous outcome measures
- **Clean rows** – rows with missing values in any dependent variable are removed automatically
- **Remove outliers** for each dependent variable independently:
  - Hard limits (values below a lower bound or above an upper bound are set to `NA`)
  - SD-based removal (values outside mean ± *k* SD, where *k* ∈ [2, 4], are set to `NA`)
  - Both methods combined (hard limits applied first)
  - Or no removal at all
- **Aggregate** data using R's `aggregate()` function, calculating either the mean or median for every combination of participant and independent variable(s)
- **Export** the result as a CSV file in **wide format** (default, one column per DV) or **long format** (a `variable` / `value` column pair)

## Running the App

You need R with the `shiny` package installed.

```r
# Install shiny if needed
install.packages("shiny")

# Run from the repository root
shiny::runApp("app.R")
```

## Example Data

`example_data.csv` is a small example file you can upload to explore the app.  
Suggested column assignments for the example:

| Role | Column(s) |
|------|-----------|
| Participant ID | `participant` |
| Information | `age`, `group` |
| IV 1 | `condition` |
| IV 2 | `block` |
| DV 1 | `response_time` |
| DV 2 | `accuracy` |
| DV 3 | `rating` |
