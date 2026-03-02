# PreProcessor

A Shiny app for pre-processing experimental CSV data to make it suitable for statistical software.

## Running the App

You need R with the `shiny` package installed.

```r
# Install shiny if needed
install.packages("shiny")

# Run from the repository root
shiny::runApp("app.R")
```

## Tabs

The app is divided into five tabs that guide you through the pre-processing workflow in order.

---

### 1. Upload

Load one or more CSV files and tidy up column names before any further processing.

**Options:**

- **Upload CSV File(s)** – select one or more `.csv` files. When multiple files are uploaded they are row-bound into a single data frame (missing columns are filled with `NA`). A header row is required; there should be no row-index column.
- **Column Separator** – choose the character that separates columns in the file:
  - *Comma* (`,`, default)
  - *Semicolon* (`;`)
  - *Tab* (`\t`)
- **Remove Columns** – tick any columns that should be dropped from the dataset entirely.
- **Rename Columns** – type a new name for any column you want to rename. The text box for each column is pre-filled with the original name; leave it unchanged to keep the original name.

The main panel shows a **Data Preview** of the first 10 rows after removals and renames have been applied.

---

### 2. New Columns

Derive additional columns from the existing data. Any columns created here are available in all later tabs.

**Operation types:**

| Type | Description |
|------|-------------|
| **Two-Column Math** | Combine two existing columns with `+`, `-`, `×`, or `÷`. |
| **Single-Column Transform** | Apply a mathematical function to one column: natural log (`ln`), `log10`, `log2`, log to a custom base, square root, `exp` (eˣ), power (`xⁿ`, custom exponent), square (`x²`), negate (`-x`), or absolute value (`|x|`). |
| **Boolean Rules** | Assign a value based on one or more if/then conditions. Each rule checks whether a column satisfies a comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`) and optionally chains a second condition with **AND** or **OR**. Rules are evaluated in order; the first matching rule wins. Rows that do not match any rule receive the **Default (else) value**. |

**Workflow for adding a column:**
1. Select an operation type.
2. Fill in the operation-specific fields.
3. Enter a name in **New Column Name** and click **Add Column**.

The **Created Columns** section lists all derived columns. To remove one, select it from the dropdown and click **Remove**.

The main panel shows a **Data Preview** (first 10 rows) that includes all created columns.

---

### 3. Variables

Assign data columns to the roles used by the rest of the pipeline.

**Roles:**

| Role | Limit | Description |
|------|-------|-------------|
| **Participant ID** | 1 column | Uniquely identifies each participant. |
| **Information Columns** | Any number (optional) | Descriptive columns about the participant (e.g. age, group). These are passed through to the output but are not used in aggregation. |
| **Independent Variables (IVs)** | Up to 4 | Discrete or categorical factors (IV 1 – IV 4). |
| **Dependent Variables (DVs)** | Up to 3 | Continuous outcome measures (DV 1 – DV 3). |

Rows that have a missing value (`NA`) in *any* selected DV are automatically removed.

The main panel shows:
- **Column Summary** – lists which column is assigned to each role and the number of rows remaining after NA removal.
- **Filtered Data Preview** – first 10 rows of the dataset restricted to the assigned columns, with incomplete DV rows removed.

---

### 4. Outliers

Filter out rows and replace extreme DV values with `NA` before aggregation.

#### Row Removal Rules

Remove entire rows where a column meets a specified condition (e.g. catch trials, incorrect responses).

- Select a **Column**, an **Operator** (`==`, `!=`, `<`, `<=`, `>`, `>=`), and a **Value**, then click **Add Rule**.
- All active rules are listed; click **×** next to a rule to delete it.
- Multiple rules are applied sequentially; a row is removed if it matches *any* rule.

#### Outlier Removal (per Dependent Variable)

For each selected DV an independent panel lets you choose a removal method:

| Method | Effect |
|--------|--------|
| **None** | No outlier removal (default). |
| **Hard Limits Only** | Values below the **Lower Limit** or above the **Upper Limit** are replaced with `NA`. Either bound can be left blank to apply only one side. |
| **SD-based Only** | Values further than **k × SD** from the mean are replaced with `NA`. *k* is chosen with a slider (range 2 – 4, step 0.5). |
| **Both (Hard Limits First)** | Hard limits are applied first, then SD-based removal is applied to the remaining values. |

The main panel shows a **Data after Outlier Removal** preview (first 10 rows).

---

### 5. Output

Aggregate the cleaned data and download the result.

**Options:**

- **Summary Function** – statistic used to aggregate each DV within each group:
  - *Mean* (default)
  - *Median*
- **Output Format** – shape of the downloaded CSV:
  - *Wide* (default) – one row per participant; when IVs are selected, each DV × IV-combination gets its own column, named `<DV>__<IV1>=<val1>__<IV2>=<val2>…`
  - *Long* – one row per participant per DV, with a `variable` column holding the DV name and a `value` column holding the aggregated value.
- **Download CSV** – saves the aggregated data to a timestamped `.csv` file.

The main panel shows an **Aggregated Data Preview** of the first 20 rows.

---

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
