library(shiny)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Helper functions for new-column computation ───────────────────────────────

eval_bool_condition <- function(col_vals, op, val, col_vals2 = NULL) {
  if (!is.null(col_vals2)) {
    num1    <- suppressWarnings(as.numeric(col_vals))
    num2    <- suppressWarnings(as.numeric(col_vals2))
    orig_na <- is.na(col_vals) | is.na(col_vals2)
    # Use numeric comparison only when conversion introduces no new NAs
    new_na1 <- is.na(num1) & !orig_na
    new_na2 <- is.na(num2) & !orig_na
    if (!any(new_na1) && !any(new_na2)) {
      col_vals  <- num1
      col_vals2 <- num2
    } else {
      col_vals  <- as.character(col_vals)
      col_vals2 <- as.character(col_vals2)
    }
    switch(op,
      "==" = col_vals == col_vals2,
      "!=" = col_vals != col_vals2,
      "<"  = col_vals <  col_vals2,
      "<=" = col_vals <= col_vals2,
      ">"  = col_vals >  col_vals2,
      ">=" = col_vals >= col_vals2,
      rep(FALSE, length(col_vals))
    )
  } else {
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val)) {
      col_vals <- suppressWarnings(as.numeric(col_vals))
      val <- num_val
    }
    switch(op,
      "==" = col_vals == val,
      "!=" = col_vals != val,
      "<"  = col_vals <  val,
      "<=" = col_vals <= val,
      ">"  = col_vals >  val,
      ">=" = col_vals >= val,
      rep(FALSE, length(col_vals))
    )
  }
}

compute_column <- function(df, def) {
  if (def$type == "math2") {
    v1 <- as.numeric(df[[def$col1]])
    v2 <- as.numeric(df[[def$col2]])
    switch(def$op,
      "+" = v1 + v2,
      "-" = v1 - v2,
      "*" = v1 * v2,
      "/" = v1 / v2
    )
  } else if (def$type == "transform") {
    v <- as.numeric(df[[def$col]])
    switch(def$transform,
      "log_nat" = log(v),
      "log10"   = log10(v),
      "log2"    = log2(v),
      "log_b"   = log(v, base = as.numeric(def$param)),
      "sqrt"    = sqrt(v),
      "exp"     = exp(v),
      "power"   = v ^ as.numeric(def$param),
      "square"  = v ^ 2,
      "negate"  = -v,
      "abs"     = abs(v)
    )
  } else if (def$type == "lagged") {
    col_vals <- df[[def$col]]
    n        <- length(col_vals)
    if (def$op %in% c("diff", "abs_diff", "pct_change")) {
      v     <- as.numeric(col_vals)
      first <- suppressWarnings(as.numeric(def$first_val))
      raw   <- c(NA_real_, diff(v))
      result <- switch(def$op,
        diff       = raw,
        abs_diff   = abs(raw),
        pct_change = { prev_abs <- abs(c(NA_real_, v[-n])); ifelse(prev_abs == 0, NA_real_, raw / prev_abs * 100) }
      )
      result[1] <- if (!is.na(first)) first else NA_real_
      result
    } else {  # compare
      prev_v <- c(NA, col_vals[-n])
      result <- character(n)
      result[1] <- def$first_val
      if (n > 1) {
        same     <- !is.na(col_vals[-1]) & !is.na(prev_v[-1]) &
                    as.character(col_vals[-1]) == as.character(prev_v[-1])
        result[-1] <- ifelse(same, def$same_val, def$diff_val)
      }
      num_result <- suppressWarnings(as.numeric(result))
      if (!anyNA(num_result)) num_result else result
    }
  } else if (def$type == "boolean") {
    result <- rep(NA_character_, nrow(df))
    for (rule in def$rules) {
      if (!(rule$col1 %in% names(df))) next
      cond <- if (!is.null(rule$cmp1_type) && rule$cmp1_type == "column" &&
                  nzchar(rule$cmp1_col %||% "") && rule$cmp1_col %in% names(df)) {
        eval_bool_condition(df[[rule$col1]], rule$op1, NULL, df[[rule$cmp1_col]])
      } else {
        eval_bool_condition(df[[rule$col1]], rule$op1, rule$val1)
      }
      if (!is.null(rule$logic) && rule$logic != "none" &&
          nzchar(rule$col2 %||% "") && (rule$col2 %in% names(df))) {
        cond2 <- if (!is.null(rule$cmp2_type) && rule$cmp2_type == "column" &&
                     nzchar(rule$cmp2_col %||% "") && rule$cmp2_col %in% names(df)) {
          eval_bool_condition(df[[rule$col2]], rule$op2, NULL, df[[rule$cmp2_col]])
        } else {
          eval_bool_condition(df[[rule$col2]], rule$op2, rule$val2)
        }
        cond  <- if (rule$logic == "and") cond & cond2 else cond | cond2
      }
      mask <- is.na(result) & !is.na(cond) & cond
      result[mask] <- rule$result
    }
    result[is.na(result)] <- def$default
    num_result <- suppressWarnings(as.numeric(result))
    if (!anyNA(num_result)) num_result else result
  }
}

format_def_label <- function(def) {
  if (def$type == "math2") {
    paste(def$col1, def$op, def$col2)
  } else if (def$type == "transform") {
    tr_name <- switch(def$transform,
      log_nat = "ln",
      log10   = "log10",
      log2    = "log2",
      log_b   = paste0("log_", def$param),
      sqrt    = "sqrt",
      exp     = "exp",
      power   = paste0("x^", def$param),
      square  = "x\u00b2",
      negate  = "-x",
      abs     = "|x|"
    )
    paste0(tr_name, "(", def$col, ")")
  } else if (def$type == "lagged") {
    op_label <- switch(def$op,
      diff       = "diff",
      abs_diff   = "abs_diff",
      pct_change = "pct_change",
      compare    = "compare"
    )
    paste0(op_label, "(", def$col, ")")
  } else if (def$type == "boolean") {
    paste(length(def$rules), "rule(s)")
  }
}

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- navbarPage(
  title = "Data PreProcessor",
  id    = "tabs",

  # ── Tab 1 : Upload ────────────────────────────────────────────────────────
  tabPanel(
    "1. Upload",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(
          "file", "Upload CSV File(s)",
          accept   = c("text/csv", "text/comma-separated-values", ".csv"),
          multiple = TRUE
        ),
        radioButtons(
          "sep", "Column Separator",
          choices  = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        ),
        hr(),
        checkboxInput("add_index", "Add index column", value = FALSE),
        conditionalPanel(
          "input.add_index",
          textInput("index_col_name", "Index Column Name:", value = "index")
        ),
        hr(),
        uiOutput("ui_remove_cols"),
        hr(),
        uiOutput("ui_rename_cols")
      ),
      mainPanel(
        width = 9,
        h4("Data Preview (first 10 rows)"),
        div(style = "overflow-x: auto;",
            tableOutput("preview"))
      )
    )
  ),

  # ── Tab 2 : New Columns ───────────────────────────────────────────────────
  tabPanel(
    "2. New Columns",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Create New Column"),
        radioButtons(
          "nc_type", "Operation Type:",
          choices = c(
            "Two-Column Math"         = "math2",
            "Single-Column Transform" = "transform",
            "Boolean Rules"           = "boolean",
            "Lag / Previous Value"    = "lagged"
          ),
          selected = "math2"
        ),
        hr(),
        conditionalPanel("input.nc_type == 'math2'",    uiOutput("nc_ui_math2")),
        conditionalPanel("input.nc_type == 'transform'", uiOutput("nc_ui_transform")),
        conditionalPanel("input.nc_type == 'boolean'",   uiOutput("nc_ui_boolean")),
        conditionalPanel("input.nc_type == 'lagged'",    uiOutput("nc_ui_lagged")),
        hr(),
        textInput("nc_name", "New Column Name:", placeholder = "e.g. log_RT"),
        actionButton("nc_add", "Add Column", class = "btn-primary"),
        hr(),
        h4("Created Columns"),
        uiOutput("nc_list")
      ),
      mainPanel(
        width = 8,
        h4("Data Preview with New Columns (first 10 rows)"),
        div(style = "overflow-x: auto;",
            tableOutput("nc_preview"))
      )
    )
  ),

  # ── Tab 3 : Column Assignment ─────────────────────────────────────────────
  tabPanel(
    "3. Variables",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Participant ID"),
        uiOutput("ui_participant"),
        hr(),
        h4("Information Columns"),
        p("Any number of columns describing the participant (optional)."),
        uiOutput("ui_info"),
        hr(),
        h4("Independent Variables (up to 4)"),
        p("Discrete or categorical variables."),
        uiOutput("ui_ivs"),
        hr(),
        h4("Dependent Variables (up to 3)"),
        p("Continuous variables to be analyzed."),
        uiOutput("ui_dvs")
      ),
      mainPanel(
        width = 8,
        h4("Column Summary"),
        verbatimTextOutput("col_summary"),
        h4("Filtered Data Preview (rows with complete DVs, first 10 rows)"),
        div(style = "overflow-x: auto;",
            tableOutput("filtered_preview"))
      )
    )
  ),

  # ── Tab 4 : Outlier Removal ───────────────────────────────────────────────
  tabPanel(
    "4. Outliers",
    fluidRow(
      column(12,
        h4("Row Removal Rules"),
        p("Remove rows where a column meets a condition (e.g. catch trials or incorrect responses)."),
        uiOutput("rr_ui"),
        hr()
      )
    ),
    fluidRow(
      column(12,
        p("For each dependent variable you can apply hard limits, SD-based removal,
          both (hard limits are applied first), or no removal at all."),
        uiOutput("outlier_ui")
      )
    ),
    hr(),
    h4("Data after Outlier Removal (first 10 rows)"),
    div(style = "overflow-x: auto;",
        tableOutput("outlier_preview"))
  ),

  # ── Tab 5 : Aggregate & Output ────────────────────────────────────────────
  tabPanel(
    "5. Output",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Aggregation"),
        radioButtons(
          "agg_func", "Summary Function",
          choices  = c(Mean = "mean", Median = "median"),
          selected = "mean"
        ),
        hr(),
        h4("Output Format"),
        radioButtons(
          "output_format", "Format",
          choices  = c(Wide = "wide", Long = "long"),
          selected = "wide"
        ),
        hr(),
        downloadButton("download", "Download CSV")
      ),
      mainPanel(
        width = 9,
        h4("Aggregated Data Preview (first 20 rows)"),
        div(style = "overflow-x: auto;",
            tableOutput("agg_preview"))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Raw data ──────────────────────────────────────────────────────────────
  raw_data <- reactive({
    req(input$file)
    dfs <- lapply(input$file$datapath, function(path) {
      read.csv(path, header = TRUE, sep = input$sep, check.names = FALSE)
    })
    dfs <- Filter(function(d) nrow(d) > 0, dfs)
    req(length(dfs) > 0)
    if (isTRUE(input$add_index)) {
      idx_name <- trimws(input$index_col_name %||% "index")
      if (!nzchar(idx_name)) idx_name <- "index"
      dfs <- lapply(dfs, function(d) {
        col_name <- idx_name
        if (col_name %in% names(d)) {
          suffix <- 1L
          while (paste0(col_name, "_", suffix) %in% names(d)) suffix <- suffix + 1L
          col_name <- paste0(col_name, "_", suffix)
        }
        cbind(setNames(data.frame(seq_len(nrow(d))), col_name), d)
      })
    }
    all_cols <- unique(c(names(dfs[[1]]), unlist(lapply(dfs, names))))
    dfs <- lapply(dfs, function(d) {
      missing_cols <- setdiff(all_cols, names(d))
      if (length(missing_cols) > 0) d[missing_cols] <- NA
      d[all_cols]
    })
    df <- do.call(rbind, dfs)
    # Convert any list columns to character to prevent downstream errors
    list_cols <- vapply(df, is.list, logical(1))
    df[list_cols] <- lapply(df[list_cols], function(col) {
      vapply(col, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
    })
    df
  })

  # ── Remove / rename columns (Tab 1 operations) ───────────────────────────
  output$ui_remove_cols <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    tagList(
      h4("Remove Columns"),
      checkboxGroupInput("remove_cols", NULL, choices = cols)
    )
  })

  output$ui_rename_cols <- renderUI({
    req(raw_data())
    all_cols <- names(raw_data())
    rm_cols  <- input$remove_cols %||% character(0)
    keep_idx <- which(!(all_cols %in% rm_cols))
    if (length(keep_idx) == 0)
      return(tagList(h4("Rename Columns"), p("No columns to rename.")))
    inputs <- lapply(keep_idx, function(i) {
      col <- all_cols[i]
      textInput(
        paste0("rename_col_", i),
        col,
        value = isolate(input[[paste0("rename_col_", i)]]) %||% col
      )
    })
    tagList(h4("Rename Columns"), do.call(tagList, inputs))
  })

  upload_data <- reactive({
    req(raw_data())
    df       <- raw_data()
    all_cols <- names(df)
    rm_cols  <- input$remove_cols %||% character(0)
    if (length(rm_cols) > 0)
      df <- df[, setdiff(all_cols, rm_cols), drop = FALSE]
    for (i in seq_along(all_cols)) {
      col <- all_cols[i]
      if (col %in% rm_cols) next
      new_name <- trimws(input[[paste0("rename_col_", i)]] %||% col)
      if (nzchar(new_name) && new_name != col && !(new_name %in% names(df)))
        names(df)[names(df) == col] <- new_name
    }
    df
  })

  output$preview <- renderTable({
    req(upload_data())
    head(upload_data(), 10)
  })

  # ── Column definitions & derived data ────────────────────────────────────
  col_defs        <- reactiveVal(list())
  n_bool_rules    <- reactiveVal(1)
  row_rmv_rules   <- reactiveVal(list())
  rr_rule_counter <- reactiveVal(0L)

  derived_data <- reactive({
    req(upload_data())
    df <- upload_data()
    for (def in col_defs()) {
      tryCatch(
        { df[[def$name]] <- compute_column(df, def) },
        error = function(e) NULL
      )
    }
    df
  })

  row_filtered_data <- reactive({
    df <- derived_data()
    for (rule in row_rmv_rules()) {
      if (!(rule$col %in% names(df))) next
      col_vals <- df[[rule$col]]
      num_val  <- suppressWarnings(as.numeric(rule$val))
      if (!is.na(num_val)) {
        col_vals <- suppressWarnings(as.numeric(col_vals))
        cmp_val  <- num_val
      } else {
        cmp_val <- rule$val
      }
      mask <- switch(rule$op,
        "==" = col_vals == cmp_val,
        "!=" = col_vals != cmp_val,
        "<"  = col_vals <  cmp_val,
        "<=" = col_vals <= cmp_val,
        ">"  = col_vals >  cmp_val,
        ">=" = col_vals >= cmp_val,
        rep(FALSE, nrow(df))
      )
      mask[is.na(mask)] <- FALSE
      df <- df[!mask, , drop = FALSE]
    }
    df
  })

  # ── New Columns tab: operation-specific UIs ───────────────────────────────
  output$nc_ui_math2 <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    tagList(
      selectInput("nc_math2_col1", "Column 1:", choices = cols),
      selectInput("nc_math2_op", "Operation:",
                  choices = c("Add (+)"        = "+",
                              "Subtract (-)"   = "-",
                              "Multiply (\u00d7)" = "*",
                              "Divide (\u00f7)"   = "/")),
      selectInput("nc_math2_col2", "Column 2:", choices = cols)
    )
  })

  output$nc_ui_transform <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    tagList(
      selectInput("nc_tr_col", "Column:", choices = cols),
      selectInput("nc_tr_type", "Transform:",
                  choices = c(
                    "Natural Log (ln)"   = "log_nat",
                    "Log base 10"        = "log10",
                    "Log base 2"         = "log2",
                    "Log (custom base)"  = "log_b",
                    "Square Root"        = "sqrt",
                    "Exponential (e^x)"  = "exp",
                    "Power (x^n)"        = "power",
                    "Square (x\u00b2)"   = "square",
                    "Negate (-x)"        = "negate",
                    "Absolute Value |x|" = "abs"
                  )),
      conditionalPanel(
        "input.nc_tr_type == 'log_b'",
        numericInput("nc_tr_param_logb", "Log Base:", value = 10, min = 1)
      ),
      conditionalPanel(
        "input.nc_tr_type == 'power'",
        numericInput("nc_tr_param_power", "Exponent:", value = 2)
      )
    )
  })

  output$nc_ui_boolean <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    if (length(cols) == 0) return(p("No columns available."))
    n    <- n_bool_rules()
    ops  <- c("==", "!=", "<", "<=", ">", ">=")

    rule_panels <- lapply(seq_len(n), function(i) {
      wellPanel(
        tags$b(paste("Rule", i, ":")),
        p(tags$small("IF the condition below is met, assign the value at the bottom.")),
        fluidRow(
          column(4, selectInput(paste0("br_col1_", i), "Column:", choices = cols,
                                selected = isolate(input[[paste0("br_col1_", i)]]) %||% cols[1])),
          column(4, selectInput(paste0("br_op1_", i), "Operator:", choices = ops,
                                selected = isolate(input[[paste0("br_op1_", i)]]) %||% "==")),
          column(4,
            radioButtons(paste0("br_cmp1_type_", i), "Compare to:",
                         choices  = c("Value" = "value", "Column" = "column"),
                         selected = isolate(input[[paste0("br_cmp1_type_", i)]]) %||% "value",
                         inline   = TRUE),
            conditionalPanel(
              condition = sprintf("input['br_cmp1_type_%d'] === 'value'", i),
              textInput(paste0("br_val1_", i), "Value:",
                        value = isolate(input[[paste0("br_val1_", i)]]) %||% "")
            ),
            conditionalPanel(
              condition = sprintf("input['br_cmp1_type_%d'] === 'column'", i),
              selectInput(paste0("br_cmp1_col_", i), "Column:", choices = cols,
                          selected = isolate(input[[paste0("br_cmp1_col_", i)]]) %||% cols[1])
            )
          )
        ),
        selectInput(paste0("br_logic_", i), "Additional condition:",
                    choices  = c("None" = "none", "AND" = "and", "OR" = "or"),
                    selected = isolate(input[[paste0("br_logic_", i)]]) %||% "none"),
        conditionalPanel(
          condition = sprintf("input['br_logic_%d'] != 'none'", i),
          fluidRow(
            column(4, selectInput(paste0("br_col2_", i), "Column:", choices = cols,
                                  selected = isolate(input[[paste0("br_col2_", i)]]) %||% cols[1])),
            column(4, selectInput(paste0("br_op2_", i), "Operator:", choices = ops,
                                  selected = isolate(input[[paste0("br_op2_", i)]]) %||% "==")),
            column(4,
              radioButtons(paste0("br_cmp2_type_", i), "Compare to:",
                           choices  = c("Value" = "value", "Column" = "column"),
                           selected = isolate(input[[paste0("br_cmp2_type_", i)]]) %||% "value",
                           inline   = TRUE),
              conditionalPanel(
                condition = sprintf("input['br_cmp2_type_%d'] === 'value'", i),
                textInput(paste0("br_val2_", i), "Value:",
                          value = isolate(input[[paste0("br_val2_", i)]]) %||% "")
              ),
              conditionalPanel(
                condition = sprintf("input['br_cmp2_type_%d'] === 'column'", i),
                selectInput(paste0("br_cmp2_col_", i), "Column:", choices = cols,
                            selected = isolate(input[[paste0("br_cmp2_col_", i)]]) %||% cols[1])
              )
            )
          )
        ),
        textInput(paste0("br_result_", i), "Then assign value:",
                  value = isolate(input[[paste0("br_result_", i)]]) %||% "")
      )
    })

    tagList(
      do.call(tagList, rule_panels),
      actionButton("br_add_rule", "Add Another Rule",
                   class = "btn-sm btn-default", style = "margin-bottom:10px;"),
      hr(),
      textInput("br_default", "Default (else) value:",
                value = isolate(input$br_default) %||% "NA")
    )
  })

  observeEvent(input$br_add_rule, {
    n_bool_rules(n_bool_rules() + 1)
  })

  output$nc_ui_lagged <- renderUI({
    req(derived_data())
    cols    <- names(derived_data())
    df      <- derived_data()
    sel_col <- input$nc_lag_col %||% cols[1]

    # Only offer numeric ops when the selected column is numeric
    is_num <- sel_col %in% names(df) && nrow(df) > 0 && {
      v <- df[[sel_col]]
      is.numeric(v) || any(!is.na(suppressWarnings(as.numeric(v[!is.na(v)]))))
    }

    op_choices <- if (is_num) {
      c(
        "Difference (current \u2212 previous)"                    = "diff",
        "Absolute Difference |current \u2212 previous|"           = "abs_diff",
        "Percent Change (100\u00d7\u0394/|previous|)"             = "pct_change",
        "Compare (same / different)"                               = "compare"
      )
    } else {
      c("Compare (same / different)" = "compare")
    }

    tagList(
      selectInput("nc_lag_col", "Column:", choices = cols, selected = sel_col),
      selectInput("nc_lag_op", "Operation:", choices = op_choices,
                  selected = isolate(input$nc_lag_op) %||% names(op_choices)[1]),
      conditionalPanel(
        "input.nc_lag_op == 'diff' || input.nc_lag_op == 'abs_diff' || input.nc_lag_op == 'pct_change'",
        numericInput("nc_lag_first_diff", "First-row fill value:", value = NA)
      ),
      conditionalPanel(
        "input.nc_lag_op == 'compare'",
        tagList(
          textInput("nc_lag_same_val", "Value when same:",
                    value = isolate(input$nc_lag_same_val) %||% "same"),
          textInput("nc_lag_diff_val", "Value when different:",
                    value = isolate(input$nc_lag_diff_val) %||% "different"),
          textInput("nc_lag_first_cmp", "First-row fill value:",
                    value = isolate(input$nc_lag_first_cmp) %||% "NA")
        )
      )
    )
  })

  # ── Add column ────────────────────────────────────────────────────────────
  observeEvent(input$nc_add, {
    name <- trimws(input$nc_name %||% "")
    if (!nzchar(name)) return()

    def <- if (input$nc_type == "math2") {
      list(
        type = "math2",
        name = name,
        col1 = input$nc_math2_col1,
        op   = input$nc_math2_op,
        col2 = input$nc_math2_col2
      )
    } else if (input$nc_type == "transform") {
      param <- if (isTRUE(input$nc_tr_type == "log_b"))  input$nc_tr_param_logb
               else if (isTRUE(input$nc_tr_type == "power")) input$nc_tr_param_power
               else NA
      list(
        type      = "transform",
        name      = name,
        col       = input$nc_tr_col,
        transform = input$nc_tr_type,
        param     = param
      )
    } else if (input$nc_type == "lagged") {
      lag_op <- input$nc_lag_op %||% "compare"
      list(
        type      = "lagged",
        name      = name,
        col       = input$nc_lag_col,
        op        = lag_op,
        first_val = if (lag_op == "compare") {
                      input$nc_lag_first_cmp %||% "NA"
                    } else {
                      as.character(input$nc_lag_first_diff %||% NA)
                    },
        same_val  = input$nc_lag_same_val %||% "same",
        diff_val  = input$nc_lag_diff_val %||% "different"
      )
    } else {
      n <- isolate(n_bool_rules())
      rules <- lapply(seq_len(n), function(i) {
        list(
          col1      = input[[paste0("br_col1_", i)]]      %||% "",
          op1       = input[[paste0("br_op1_", i)]]       %||% "==",
          cmp1_type = input[[paste0("br_cmp1_type_", i)]] %||% "value",
          val1      = input[[paste0("br_val1_", i)]]      %||% "",
          cmp1_col  = input[[paste0("br_cmp1_col_", i)]]  %||% "",
          logic     = input[[paste0("br_logic_", i)]]     %||% "none",
          col2      = input[[paste0("br_col2_", i)]]      %||% "",
          op2       = input[[paste0("br_op2_", i)]]       %||% "==",
          cmp2_type = input[[paste0("br_cmp2_type_", i)]] %||% "value",
          val2      = input[[paste0("br_val2_", i)]]      %||% "",
          cmp2_col  = input[[paste0("br_cmp2_col_", i)]]  %||% "",
          result    = input[[paste0("br_result_", i)]]    %||% ""
        )
      })
      list(
        type    = "boolean",
        name    = name,
        rules   = rules,
        default = input$br_default %||% "NA"
      )
    }

    defs     <- col_defs()
    existing <- which(vapply(defs, function(d) d$name == name, logical(1)))
    if (length(existing) > 0) {
      defs[[existing[1]]] <- def
    } else {
      defs <- c(defs, list(def))
    }
    col_defs(defs)
    n_bool_rules(1)
  })

  # ── Created-columns list with remove ─────────────────────────────────────
  output$nc_list <- renderUI({
    defs <- col_defs()
    if (length(defs) == 0) return(p("No columns created yet."))
    items <- lapply(defs, function(def) {
      div(style = "margin-bottom: 4px;",
          tags$code(def$name), ": ", format_def_label(def))
    })
    col_names <- vapply(defs, `[[`, character(1), "name")
    tagList(
      do.call(tagList, items),
      hr(),
      selectInput("nc_remove_sel", "Remove column:",
                  choices = c("(select)" = "", col_names), selected = ""),
      actionButton("nc_remove_btn", "Remove", class = "btn-sm btn-danger")
    )
  })

  observeEvent(input$nc_remove_btn, {
    sel <- input$nc_remove_sel %||% ""
    if (!nzchar(sel)) return()
    col_defs(Filter(function(d) d$name != sel, col_defs()))
  })

  output$nc_preview <- renderTable({
    req(derived_data())
    head(derived_data(), 10)
  })

  # ── Column selection UIs ──────────────────────────────────────────────────
  output$ui_participant <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    selectInput("participant_col", "Participant ID Column:",
                choices = cols, multiple = FALSE)
  })

  output$ui_info <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    checkboxGroupInput("info_cols", NULL, choices = cols)
  })

  output$ui_ivs <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    choices <- c("(none)" = "", cols)
    tagList(
      selectInput("iv1", "IV 1:", choices = choices, selected = ""),
      selectInput("iv2", "IV 2:", choices = choices, selected = ""),
      selectInput("iv3", "IV 3:", choices = choices, selected = ""),
      selectInput("iv4", "IV 4:", choices = choices, selected = "")
    )
  })

  output$ui_dvs <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    choices <- c("(none)" = "", cols)
    tagList(
      selectInput("dv1", "DV 1:", choices = choices, selected = ""),
      selectInput("dv2", "DV 2:", choices = choices, selected = ""),
      selectInput("dv3", "DV 3:", choices = choices, selected = "")
    )
  })

  # ── Helpers: selected IVs / DVs ───────────────────────────────────────────
  selected_ivs <- reactive({
    ivs <- c(input$iv1, input$iv2, input$iv3, input$iv4)
    unique(ivs[nzchar(ivs)])
  })

  selected_dvs <- reactive({
    dvs <- c(input$dv1, input$dv2, input$dv3)
    unique(dvs[nzchar(dvs)])
  })

  # ── Filtered data ──────────────────────────────────────────────────────────
  filtered_data <- reactive({
    req(row_filtered_data(), input$participant_col)

    part <- input$participant_col
    info <- input$info_cols  # may be NULL
    ivs  <- selected_ivs()
    dvs  <- selected_dvs()

    keep <- unique(c(part, info, ivs, dvs))
    keep <- keep[keep %in% names(row_filtered_data())]

    df <- row_filtered_data()[, keep, drop = FALSE]

    # Remove rows with any missing DV value
    if (length(dvs) > 0) {
      dv_ok <- complete.cases(df[, dvs, drop = FALSE])
      df    <- df[dv_ok, , drop = FALSE]
    }

    rownames(df) <- NULL
    df
  })

  output$col_summary <- renderPrint({
    req(input$participant_col)
    cat("Participant ID : ", input$participant_col, "\n")
    cat("Info columns   : ", paste(input$info_cols,   collapse = ", "), "\n")
    cat("IVs selected   : ", paste(selected_ivs(),    collapse = ", "), "\n")
    cat("DVs selected   : ", paste(selected_dvs(),    collapse = ", "), "\n")
    cat("Rows after NA removal (DVs): ",
        if (!is.null(filtered_data())) nrow(filtered_data()) else 0, "\n")
  })

  output$filtered_preview <- renderTable({
    req(filtered_data())
    head(filtered_data(), 10)
  })

  # ── Row Removal UI ────────────────────────────────────────────────────────
  output$rr_ui <- renderUI({
    req(derived_data())
    cols <- names(derived_data())
    ops  <- c("==", "!=", "<", "<=", ">", ">=")
    rules <- row_rmv_rules()

    rule_items <- if (length(rules) > 0) {
      lapply(rules, function(r) {
        div(style = "margin-bottom: 4px;",
            tags$code(paste(r$col, r$op, r$val)),
            " ",
            actionButton(paste0("rr_del_", r$id), "\u00d7",
                         class = "btn-xs btn-danger",
                         style = "padding:1px 6px;"))
      })
    } else {
      list(p(tags$em("No rules added yet.")))
    }

    tagList(
      do.call(tagList, rule_items),
      fluidRow(
        column(3, selectInput("rr_col", "Column:", choices = cols)),
        column(2, selectInput("rr_op",  "Operator:", choices = ops)),
        column(3, textInput("rr_val", "Value:", placeholder = "e.g. 0 or catch")),
        column(2, div(style = "margin-top: 25px;",
                      actionButton("rr_add", "Add Rule", class = "btn-primary btn-sm")))
      )
    )
  })

  observeEvent(input$rr_add, {
    col <- input$rr_col %||% ""
    op  <- input$rr_op  %||% "=="
    val <- trimws(input$rr_val %||% "")
    if (!nzchar(col) || !nzchar(val)) return()
    counter <- rr_rule_counter() + 1L
    rr_rule_counter(counter)
    rules <- row_rmv_rules()
    row_rmv_rules(c(rules, list(list(id = counter, col = col, op = op, val = val))))
  })

  # Delete buttons for individual row-removal rules (keyed by unique rule ID)
  observe({
    lapply(row_rmv_rules(), function(r) {
      btn_id <- paste0("rr_del_", r$id)
      observeEvent(input[[btn_id]], {
        row_rmv_rules(Filter(function(x) x$id != r$id, row_rmv_rules()))
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ── Outlier UI (one panel per DV, index-based IDs for safety) ─────────────
  output$outlier_ui <- renderUI({
    dvs <- selected_dvs()
    if (length(dvs) == 0) {
      return(p("Please select at least one dependent variable in the Variables tab."))
    }

    panels <- lapply(seq_along(dvs), function(i) {
      dv  <- dvs[i]
      pfx <- paste0("ot", i)   # prefix: ot1 / ot2 / ot3

      wellPanel(
        h4(paste("Dependent Variable:", dv)),

        radioButtons(
          paste0(pfx, "_type"), "Outlier Removal Method:",
          choices = c(
            "None"                       = "none",
            "Hard Limits Only"           = "hard",
            "SD-based Only"              = "sd",
            "Both (Hard Limits First)"   = "both"
          ),
          selected = "none",
          inline   = TRUE
        ),

        # Hard limits inputs
        conditionalPanel(
          condition = sprintf(
            "input['%s_type'] == 'hard' || input['%s_type'] == 'both'",
            pfx, pfx
          ),
          fluidRow(
            column(4, numericInput(paste0(pfx, "_min"), "Lower Limit:", value = NA)),
            column(4, numericInput(paste0(pfx, "_max"), "Upper Limit:", value = NA))
          )
        ),

        # SD multiplier
        conditionalPanel(
          condition = sprintf(
            "input['%s_type'] == 'sd' || input['%s_type'] == 'both'",
            pfx, pfx
          ),
          sliderInput(paste0(pfx, "_sd"), "SD Multiplier (mean ± k·SD):",
                      min = 2, max = 4, value = 2, step = 0.5, width = "40%")
        )
      )
    })

    do.call(tagList, panels)
  })

  # ── Apply outlier removal ─────────────────────────────────────────────────
  clean_data <- reactive({
    req(filtered_data())
    dvs <- selected_dvs()
    if (length(dvs) == 0) return(filtered_data())

    df <- filtered_data()

    for (i in seq_along(dvs)) {
      dv  <- dvs[i]
      pfx <- paste0("ot", i)

      otype <- input[[paste0(pfx, "_type")]]
      if (is.null(otype) || otype == "none") next

      vals <- df[[dv]]

      # 1. Hard limits (applied before SD when "both")
      if (otype %in% c("hard", "both")) {
        lo <- input[[paste0(pfx, "_min")]]
        hi <- input[[paste0(pfx, "_max")]]
        if (!is.null(lo) && !is.na(lo)) vals[vals < lo] <- NA
        if (!is.null(hi) && !is.na(hi)) vals[vals > hi] <- NA
      }

      # 2. SD-based removal
      if (otype %in% c("sd", "both")) {
        k <- input[[paste0(pfx, "_sd")]]
        if (is.null(k) || is.na(k)) k <- 2
        m <- mean(vals, na.rm = TRUE)
        s <- sd(vals,   na.rm = TRUE)
        is_outlier <- !is.na(vals) & (vals < m - k * s | vals > m + k * s)
        vals[is_outlier] <- NA
      }

      df[[dv]] <- vals
    }

    df
  })

  output$outlier_preview <- renderTable({
    req(clean_data())
    head(clean_data(), 10)
  })

  # ── Participant-level descriptives ────────────────────────────────────────
  participant_info <- reactive({
    req(clean_data(), input$participant_col)

    info <- input$info_cols
    part <- input$participant_col

    if (length(info) == 0 || is.null(info)) return(NULL)

    df   <- clean_data()
    keep <- c(part, info[info %in% names(df)])
    df_info <- df[, keep, drop = FALSE]

    participants <- unique(df_info[[part]])
    result <- data.frame(setNames(list(participants), part),
                         stringsAsFactors = FALSE)

    for (col in info) {
      if (col %in% names(df_info)) {
        vals <- sapply(participants, function(p) {
          x <- df_info[[col]][df_info[[part]] == p]
          x <- x[!is.na(x)]
          if (length(x) > 0) x[1] else NA
        })
        result[[col]] <- vals
      }
    }

    result
  })

  # ── Aggregation ───────────────────────────────────────────────────────────
  agg_data <- reactive({
    req(clean_data(), input$participant_col, input$agg_func)

    dvs  <- selected_dvs()
    ivs  <- selected_ivs()
    part <- input$participant_col

    if (length(dvs) == 0) return(NULL)

    df          <- clean_data()
    group_cols  <- unique(c(part, ivs))
    group_cols  <- group_cols[group_cols %in% names(df)]

    fun <- if (input$agg_func == "mean") {
      function(x) mean(x, na.rm = TRUE)
    } else {
      function(x) median(x, na.rm = TRUE)
    }

    lhs <- if (length(dvs) == 1) dvs else paste0("cbind(", paste(dvs, collapse = ", "), ")")
    rhs <- paste(group_cols, collapse = " + ")
    agg_formula <- as.formula(paste(lhs, "~", rhs))

    # na.action = na.pass keeps rows whose grouping variables are complete
    # while allowing DV columns to contain NAs introduced by outlier removal;
    # those NAs are handled by na.rm = TRUE inside fun.
    result <- aggregate(agg_formula, data = df, FUN = fun, na.action = na.pass)
    result
  })

  # ── Output format ─────────────────────────────────────────────────────────
  output_data <- reactive({
    req(agg_data(), input$output_format)

    df   <- agg_data()
    dvs  <- selected_dvs()
    ivs  <- selected_ivs()
    part <- input$participant_col
    pinfo <- participant_info()

    if (input$output_format == "long") {
      id_cols <- setdiff(names(df), dvs)

      long_df <- reshape(
        df,
        varying   = dvs,
        v.names   = "value",
        timevar   = "variable",
        times     = dvs,
        direction = "long"
      )
      # reshape adds an "id" column we don't need
      long_df$id   <- NULL
      rownames(long_df) <- NULL
      long_df <- long_df[, c(id_cols, "variable", "value")]

      # Attach participant info (left-join: keep all rows with valid DV data)
      if (!is.null(pinfo)) {
        long_df <- merge(pinfo, long_df, by = part, all.y = TRUE)
      }
      long_df
    } else {
      # Wide format: one row per participant
      if (length(ivs) == 0) {
        # No IVs: aggregate already produced one row per participant
        wide_df <- df[, c(part, dvs), drop = FALSE]
      } else {
        # Build IV combination key for each row
        iv_vals <- df[, ivs, drop = FALSE]
        iv_keys <- apply(iv_vals, 1, function(row) {
          paste(paste(ivs, row, sep = "="), collapse = "__")
        })

        participants <- unique(df[[part]])
        unique_keys  <- unique(iv_keys)

        # Start with participant column
        wide_df <- data.frame(setNames(list(participants), part),
                              stringsAsFactors = FALSE)

        # Add one column per DV per IV combination
        for (dv in dvs) {
          for (key in unique_keys) {
            col_name <- paste(dv, key, sep = "__")
            vals <- sapply(participants, function(p) {
              idx <- which(df[[part]] == p & iv_keys == key)
              if (length(idx) == 0) NA else df[[dv]][idx[1]]
            })
            wide_df[[col_name]] <- vals
          }
        }
      }

      # Attach participant info (left-join: keep all participants with valid DV data)
      if (!is.null(pinfo)) {
        wide_df <- merge(pinfo, wide_df, by = part, all.y = TRUE)
      }
      wide_df
    }
  })

  output$agg_preview <- renderTable({
    req(output_data())
    head(output_data(), 20)
  })

  # ── Download ──────────────────────────────────────────────────────────────
  output$download <- downloadHandler(
    filename = function() {
      paste0("preprocessed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(output_data(), file, row.names = FALSE)
    }
  )
}

# ── Launch ───────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
