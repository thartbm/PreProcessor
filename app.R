library(shiny)

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
          "file", "Upload CSV File",
          accept = c("text/csv", "text/comma-separated-values", ".csv")
        ),
        radioButtons(
          "sep", "Column Separator",
          choices  = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        )
      ),
      mainPanel(
        width = 9,
        h4("Data Preview (first 10 rows)"),
        div(style = "overflow-x: auto;",
            tableOutput("preview"))
      )
    )
  ),

  # ── Tab 2 : Column Assignment ─────────────────────────────────────────────
  tabPanel(
    "2. Columns",
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

  # ── Tab 3 : Outlier Removal ───────────────────────────────────────────────
  tabPanel(
    "3. Outliers",
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

  # ── Tab 4 : Aggregate & Output ────────────────────────────────────────────
  tabPanel(
    "4. Output",
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
    df <- read.csv(input$file$datapath,
                   header = TRUE,
                   sep    = input$sep,
                   check.names = FALSE)
    # Convert any list columns to character to prevent downstream errors
    list_cols <- vapply(df, is.list, logical(1))
    df[list_cols] <- lapply(df[list_cols], function(col) {
      vapply(col, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
    })
    df
  })

  output$preview <- renderTable({
    req(raw_data())
    head(raw_data(), 10)
  })

  # ── Column selection UIs ──────────────────────────────────────────────────
  output$ui_participant <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    selectInput("participant_col", "Participant ID Column:",
                choices = cols, multiple = FALSE)
  })

  output$ui_info <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    checkboxGroupInput("info_cols", NULL, choices = cols)
  })

  output$ui_ivs <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    choices <- c("(none)" = "", cols)
    tagList(
      selectInput("iv1", "IV 1:", choices = choices, selected = ""),
      selectInput("iv2", "IV 2:", choices = choices, selected = ""),
      selectInput("iv3", "IV 3:", choices = choices, selected = ""),
      selectInput("iv4", "IV 4:", choices = choices, selected = "")
    )
  })

  output$ui_dvs <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
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
    req(raw_data(), input$participant_col)

    part <- input$participant_col
    info <- input$info_cols  # may be NULL
    ivs  <- selected_ivs()
    dvs  <- selected_dvs()

    keep <- unique(c(part, info, ivs, dvs))
    keep <- keep[keep %in% names(raw_data())]

    df <- raw_data()[, keep, drop = FALSE]

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

  # ── Outlier UI (one panel per DV, index-based IDs for safety) ─────────────
  output$outlier_ui <- renderUI({
    dvs <- selected_dvs()
    if (length(dvs) == 0) {
      return(p("Please select at least one dependent variable in the Columns tab."))
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

    lhs <- paste(dvs,       collapse = " + ")
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
