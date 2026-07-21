# app.R - Game Search Trends Analytics Dashboard
# Based on Google Trends data for: hunt, new world, ark, smash, rust
# (4/20/26 - 7/20/26, United States)

library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(tidyr)
library(wordcloud2)
library(readr)

# ------------------------------------------------------------
# Ensure correct working directory
# ------------------------------------------------------------
# When running interactively, set WD to script location
if (interactive()) {
  tryCatch({
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(script_dir)
  }, error = function(e) {
    # Fallback: try common locations
    if (dir.exists("~/Fiverr/dummy apps/Package 1")) {
      setwd("~/Fiverr/dummy apps/Package 1")
    }
  })
}
# ------------------------------------------------------------
# Data loading helper - reads all CSV files
# ------------------------------------------------------------
load_data <- function() {
  # Time series data (multiTimeline-10.csv)
  timeline <- read_csv('multiTimeline-10.csv', skip = 2) %>%
    rename(
      date = Day,
      hunt = `hunt: (United States)`,
      new_world = `new world: (United States)`,
      ark = `ark: (United States)`,
      smash = `smash: (United States)`,
      rust = `rust: (United States)`
    ) %>%
    mutate(date = as.Date(date))
  
  # Regional combined data (geoMap-2.csv)
  geo_combined <- read_csv('geoMap-2.csv', skip = 2) %>%
    rename(
      region = Region,
      hunt_pct = `hunt: (4/20/26 - 7/20/26)`,
      new_world_pct = `new world: (4/20/26 - 7/20/26)`,
      ark_pct = `ark: (4/20/26 - 7/20/26)`,
      smash_pct = `smash: (4/20/26 - 7/20/26)`,
      rust_pct = `rust: (4/20/26 - 7/20/26)`
    ) %>%
    mutate(across(ends_with("_pct"), ~ as.numeric(gsub("%", "", .)) / 100))
  
  # Individual geo maps
  geo_hunt <- read_csv('geoMap-7.csv', skip = 2) %>% mutate(term = "hunt")
  geo_new_world <- read_csv('geoMap-6.csv', skip = 2) %>% mutate(term = "new world")
  geo_ark <- read_csv('geoMap-5.csv', skip = 2) %>% mutate(term = "ark")
  geo_smash <- read_csv('geoMap-4.csv', skip = 2) %>% mutate(term = "smash")
  geo_rust <- read_csv('geoMap-3.csv', skip = 2) %>% mutate(term = "rust")
  
  geo_all <- bind_rows(
    geo_hunt, geo_new_world, geo_ark, geo_smash, geo_rust
  ) %>%
    rename(region = Region, score = 2)
  
  # Related queries - parse each file
  parse_related <- function(file, term_name) {
    lines <- readLines(file)
    # Find TOP and RISING sections
    top_idx <- grep("^TOP", lines)
    rising_idx <- grep("^RISING", lines)
    
    top_lines <- lines[(top_idx + 1):(rising_idx - 1)]
    rising_lines <- lines[(rising_idx + 1):length(lines)]
    
    parse_block <- function(block_lines, type) {
      result <- data.frame()
      for (line in block_lines) {
        if (nchar(trimws(line)) == 0) next
        parts <- strsplit(line, ",")[[1]]
        query <- parts[1]
        value <- parts[length(parts)]
        result <- rbind(result, data.frame(
          term = term_name,
          type = type,
          query = query,
          value = value,
          stringsAsFactors = FALSE
        ))
      }
      result
    }
    
    top_df <- parse_block(top_lines, "TOP")
    rising_df <- parse_block(rising_lines, "RISING")
    bind_rows(top_df, rising_df)
  }
  
  related_hunt <- parse_related('relatedQueries-36.csv', "hunt")
  related_new_world <- parse_related('relatedQueries-37.csv', "new world")
  related_ark <- parse_related('relatedQueries-38.csv', "ark")
  related_smash <- parse_related('relatedQueries-39.csv', "smash")
  related_rust <- parse_related('relatedQueries-40.csv', "rust")
  
  related_all <- bind_rows(
    related_hunt, related_new_world, related_ark, related_smash, related_rust
  )
  
  # Word cloud data from TOP related queries
  top_queries <- related_all %>%
    filter(type == "TOP") %>%
    mutate(value_num = as.numeric(value)) %>%
    filter(!is.na(value_num), value_num > 0)
  
  wc_data <- top_queries %>%
    group_by(query) %>%
    summarise(freq = sum(value_num), .groups = "drop") %>%
    arrange(desc(freq)) %>%
    as.data.frame()
  
  list(
    timeline = timeline,
    geo_combined = geo_combined,
    geo_all = geo_all,
    related_all = related_all,
    wc_data = wc_data,
    top_queries = top_queries
  )
}

# ------------------------------------------------------------
# Small helper - mode
# ------------------------------------------------------------
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Game Search Trends Analytics Dashboard"),
  
  h5("Data Source: Google Trends (4/20/26 - 7/20/26, United States)"),
  h5("Terms: hunt, new world, ark, smash, rust"),
  hr(),
  
  # ---- Term selector -------------------------------------------------
  h3("Select Search Term to Analyze"),
  selectInput("term_select", "Choose Term:",
              choices = c("hunt", "new world", "ark", "smash", "rust"),
              selected = "rust"),
  hr(),
  
  # ---- Key metrics cards ---------------------------------------------
  h3("Key Metrics"),
  fluidRow(
    column(4,
           h4("Total Data Points"),
           verbatimTextOutput("total_points_display")
    ),
    column(4,
           h4("Average Search Interest"),
           verbatimTextOutput("avg_interest_display")
    ),
    column(4,
           h4("Peak Search Interest"),
           verbatimTextOutput("peak_interest_display")
    )
  ),
  hr(),
  
  # ---- Chart 1: Time series for selected term ------------------------
  h3("Search Interest Over Time"),
  h6("Daily search interest for selected term (relative score 0-100)"),
  plotOutput("timeline_chart"),
  hr(),
  
  # ---- Chart 2: All terms comparison ---------------------------------
  h3("All Terms Comparison"),
  h6("Search interest trends for all 5 game terms"),
  plotOutput("comparison_chart"),
  hr(),
  
  # ---- Chart 3: Regional heatmap-style bar chart ---------------------
  h3("Regional Interest Distribution"),
  h6("Search interest by state for selected term"),
  plotOutput("regional_chart"),
  hr(),
  
  # ---- Related queries table -----------------------------------------
  h3("Related Queries"),
  tabsetPanel(
    tabPanel("Top Queries", tableOutput("top_queries_table")),
    tabPanel("Rising Queries", tableOutput("rising_queries_table"))
  ),
  hr(),
  
  # ---- Tag word cloud ------------------------------------------------
  h3("Most Frequent Related Queries (All Terms)"),
  wordcloud2Output("query_cloud"),
  hr(),
  
  # ---- Statistics ----------------------------------------------------
  h3("Basic Statistics"),
  verbatimTextOutput("interest_stats"),
  tableOutput("daily_stats_table"),
  hr(),
  
  # ---- Regional comparison table -------------------------------------
  h3("Regional Interest Comparison (All Terms)"),
  tableOutput("regional_comparison_table"),
  hr(),
  
  # ---- Tip box -------------------------------------------------------
  h4("Growth Strategy Tip"),
  h6("Watch RISING queries! They show what's gaining momentum!"),
  h6("Use trending terms in your content titles to ride the wave!"),
  
  # ---- Footer --------------------------------------------------------
  tags$hr(),
  tags$p(
    style = "text-align:center; color:#888; font-size:12px;",
    "Built on Google Trends Data + Shiny + R"
  ),
  tags$hr()
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Load data once
  data_cache <- reactive({
    load_data()
  })
  
  # ---- Key metrics ----------------------------------------------------
  output$total_points_display <- renderText({
    dat <- data_cache()
    sprintf("Total Data Points: %s days", nrow(dat$timeline))
  })
  
  output$avg_interest_display <- renderText({
    dat <- data_cache()
    term <- input$term_select
    term_col <- gsub(" ", "_", term)
    vals <- dat$timeline[[term]]
    sprintf("Average Interest: %.2f", mean(vals, na.rm = TRUE))
  })
  
  output$peak_interest_display <- renderText({
    dat <- data_cache()
    term <- input$term_select
    vals <- dat$timeline[[term]]
    peak_day <- dat$timeline$date[which.max(vals)]
    sprintf("Peak Interest: %s on %s", max(vals, na.rm = TRUE), peak_day)
  })
  
  # ---- Chart 1: Timeline for selected term ----------------------------
  output$timeline_chart <- renderPlot({
    dat <- data_cache()
    term <- input$term_select
    df <- dat$timeline %>%
      select(date, value = all_of(term))
    
    ggplot(df, aes(x = date, y = value)) +
      geom_line(color = "#829FC1", linewidth = 1) +
      geom_point(size = 2, color = "#829FC1") +
      geom_smooth(method = "loess", se = FALSE, color = "#050E1C", linetype = "dashed") +
      labs(
        title = paste("Search Interest Over Time:", term),
        subtitle = "Relative search interest (0-100 scale)",
        x = "Date", y = "Search Interest"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ---- Chart 2: All terms comparison ---------------------------------
  output$comparison_chart <- renderPlot({
    dat <- data_cache()
    df <- dat$timeline %>%
      pivot_longer(
        cols = c(hunt, new_world, ark, smash, rust),
        names_to = "term",
        values_to = "interest"
      ) %>%
      mutate(term = gsub("_", " ", term))
    
    ggplot(df, aes(x = date, y = interest, color = term)) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        name = "Search Term",
        values = c(
          "hunt" = "#2596be",
          "new world" = "#8bacd1",
          "ark" = "#4676b0",
          "smash" = "#08141f",
          "rust" = "#c47a3d"
        )
      ) +
      labs(
        title = "All Terms Comparison: Search Interest Over Time",
        subtitle = "Comparing 5 game-related search terms",
        x = "Date", y = "Search Interest"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # ---- Chart 3: Regional bar chart ------------------------------------
  output$regional_chart <- renderPlot({
    dat <- data_cache()
    term <- input$term_select
    col_name <- paste0(term, "_pct")
    
    df <- dat$geo_combined %>%
      select(region, value = all_of(col_name)) %>%
      arrange(desc(value)) %>%
      head(20)
    
    ggplot(df, aes(x = reorder(region, value), y = value)) +
      geom_col(fill = "#829FC1") +
      coord_flip() +
      labs(
        title = paste("Top 20 States by Search Interest:", term),
        subtitle = "Percentage of regional search interest",
        x = "State", y = "Interest Percentage"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10)
      )
  })
  
  # ---- Related queries tables -----------------------------------------
  output$top_queries_table <- renderTable({
    dat <- data_cache()
    term <- input$term_select
    dat$related_all %>%
      filter(term == !!term, type == "TOP") %>%
      select(Query = query, Value = value) %>%
      head(25)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$rising_queries_table <- renderTable({
    dat <- data_cache()
    term <- input$term_select
    dat$related_all %>%
      filter(term == !!term, type == "RISING") %>%
      select(Query = query, Growth = value) %>%
      head(25)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Word cloud -----------------------------------------------------
  output$query_cloud <- renderWordcloud2({
    dat <- data_cache()
    if (is.null(dat$wc_data) || nrow(dat$wc_data) == 0) {
      return(wordcloud2(data.frame(word = "", freq = 0), size = 0))
    }
    wordcloud2(
      data = dat$wc_data,
      size = 1.4,
      minSize = 0,
      gridSize = 0.5,
      color = c("#050E1C", "#829FC1", "#2596be", "#08141f",
                "#8bacd1", "#294468", "#6c82a0", "#4676b0"),
      backgroundColor = "#f8f9fa",
      rotateRatio = 0.25,
      shape = "circle",
      fontFamily = "sans"
    )
  })
  
  # ---- Basic Statistics -----------------------------------------------
  output$interest_stats <- renderPrint({
    dat <- data_cache()
    term <- input$term_select
    vals <- dat$timeline[[term]]
    
    mean_val <- mean(vals, na.rm = TRUE)
    median_val <- median(vals, na.rm = TRUE)
    mode_val <- get_mode(vals)
    range_val <- max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)
    var_val <- var(vals, na.rm = TRUE)
    sd_val <- sd(vals, na.rm = TRUE)
    
    cat(paste("Search Interest Statistics for:", term, "\n"))
    cat("=====================================\n")
    cat(sprintf("Mean: %.2f\n", mean_val))
    cat(sprintf("Median: %.0f\n", median_val))
    cat(sprintf("Mode: %.0f\n", mode_val))
    cat(sprintf("Range: %.0f\n", range_val))
    cat(sprintf("Variance: %.2f\n", var_val))
    cat(sprintf("Standard Deviation: %.2f\n", sd_val))
  })
  
  # ---- Daily stats table ----------------------------------------------
  output$daily_stats_table <- renderTable({
    dat <- data_cache()
    term <- input$term_select
    
    dat$timeline %>%
      select(date, value = all_of(term)) %>%
      mutate(
        day_name = wday(date, label = TRUE),
        formatted_date = paste(format(date, "%A"), format(date, "%Y-%m-%d"))
      ) %>%
      arrange(desc(date)) %>%
      head(30) %>%
      select(
        `Date` = formatted_date,
        `Day of Week` = day_name,
        `Interest Score` = value
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Regional comparison table --------------------------------------
  output$regional_comparison_table <- renderTable({
    dat <- data_cache()
    dat$geo_combined %>%
      arrange(desc(hunt_pct)) %>%
      select(
        `State` = region,
        `Hunt %` = hunt_pct,
        `New World %` = new_world_pct,
        `Ark %` = ark_pct,
        `Smash %` = smash_pct,
        `Rust %` = rust_pct
      ) %>%
      mutate(across(ends_with("%"), ~ sprintf("%.0f%%", . * 100)))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ------------------------------------------------------------
# Launch the app
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)