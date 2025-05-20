library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(readr)

ui <- page_sidebar(
  title = "Water Quality Data Explorer",
  sidebar = sidebar(
    width = 300,
    titlePanel("Analysis Controls"),
    
    fileInput("file", "Upload CSV File",
              accept = c(".csv")),
    
    conditionalPanel(
      condition = "output.fileUploaded",
      
      selectInput("plotType", "Plot Type:",
                  choices = c("Time Series", "Scatter Plot", "Box Plot", "Histogram"),
                  selected = "Time Series"),
      
      conditionalPanel(
        condition = "input.plotType == 'Scatter Plot'",
        selectInput("xVar", "X Variable:", choices = NULL),
        selectInput("yVar", "Y Variable:", choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.plotType == 'Time Series' || input.plotType == 'Box Plot' || input.plotType == 'Histogram'",
        checkboxGroupInput("variables", "Variables to Display:", choices = NULL)
      ),
      
      selectInput("siteFilter", "Filter by Site:", choices = NULL),
      
      dateRangeInput("dateRange", "Date Range:", start = NULL, end = NULL),
      
      downloadButton("downloadData", "Download Filtered Data")
    )
  ),
  
  uiOutput("valueBoxesUI"),
  
  conditionalPanel(
    condition = "output.fileUploaded",
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Data Visualization"),
        plotlyOutput("mainPlot", height = "400px")
      )
    ),
    
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Summary Statistics"),
        tableOutput("summaryTable")
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Data Table"),
      DTOutput("dataTable")
    )
  ),
  
  conditionalPanel(
    condition = "!output.fileUploaded",
    card(
      card_header("Welcome to Water Quality Data Explorer"),
      card_body(
        h4("Please upload a CSV file to get started."),
        p("Your CSV file should include these recommended columns:"),
        tags$ul(
          tags$li("A location/site identifier column"),
          tags$li("A date column (must be in a standard date format)"),
          tags$li("Measurement columns like: Dissolved Oxygen, Conductivity, pH, Temperature, etc.")
        ),
        p("After uploading, you'll be able to explore and visualize your water quality data interactively.")
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  data <- reactiveVal(NULL)
  
  # Track whether file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Variable to store column names
  numeric_cols <- reactiveVal(NULL)
  date_col <- reactiveVal(NULL)
  site_col <- reactiveVal(NULL)
  
  # Load data from file upload
  observeEvent(input$file, {
    req(input$file)
    
    # Read the CSV file
    tryCatch({
      df <- read_csv(input$file$datapath)
      
      # Identify column types
      num_cols <- names(df)[sapply(df, is.numeric)]
      numeric_cols(num_cols)
      
      # Try to identify date column
      date_columns <- names(df)[sapply(df, function(x) {
        tryCatch({
          all(!is.na(as.Date(as.character(x), format = "%Y-%m-%d"))) ||
            all(!is.na(as.Date(as.character(x), format = "%m/%d/%Y"))) ||
            all(!is.na(as.Date(as.character(x), format = "%d-%m-%Y")))
        }, error = function(e) FALSE)
      })]
      
      if (length(date_columns) > 0) {
        date_col(date_columns[1])
        # Convert the date column to Date type
        df[[date_columns[1]]] <- as.Date(df[[date_columns[1]]])
      }
      
      # Try to identify site/location column
      # Assuming site/location is a character column with few unique values compared to total rows
      char_cols <- names(df)[sapply(df, is.character)]
      
      if (length(char_cols) > 0) {
        # Select the first character column that has a reasonable number of unique values
        possible_site_cols <- char_cols[sapply(df[char_cols], function(x) {
          n_unique <- length(unique(x))
          n_rows <- nrow(df)
          return(n_unique < n_rows/5 && n_unique > 1)  # Heuristic for "reasonable"
        })]
        
        if (length(possible_site_cols) > 0) {
          site_col(possible_site_cols[1])
        } else {
          site_col(char_cols[1])  # Default to first character column
        }
      }
      
      # Store the data
      data(df)
      
      # Update UI controls based on the data
      if (!is.null(num_cols) && length(num_cols) > 0) {
        updateSelectInput(session, "xVar", choices = num_cols, selected = num_cols[1])
        updateSelectInput(session, "yVar", choices = num_cols, selected = if(length(num_cols) > 1) num_cols[2] else num_cols[1])
        updateCheckboxGroupInput(session, "variables", choices = num_cols, 
                                 selected = if(length(num_cols) >= 2) num_cols[1:2] else num_cols)
      }
      
      if (!is.null(site_col())) {
        sites <- c("All Sites", unique(df[[site_col()]]))
        updateSelectInput(session, "siteFilter", choices = sites, selected = "All Sites")
      }
      
      if (!is.null(date_col())) {
        start_date <- min(df[[date_col()]], na.rm = TRUE)
        end_date <- max(df[[date_col()]], na.rm = TRUE)
        updateDateRangeInput(session, "dateRange", start = start_date, end = end_date)
      }
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Create dynamic value boxes UI
  output$valueBoxesUI <- renderUI({
    req(data(), numeric_cols())
    req(length(numeric_cols()) > 0)
    
    boxes <- lapply(numeric_cols()[1:min(4, length(numeric_cols()))], function(col) {
      value_box(
        title = paste(gsub("_", " ", col), "Range"),
        value = textOutput(paste0("range_", make.names(col))),
        showcase = bsicons::bs_icon("graph-up"),
        theme = sample(c("primary", "secondary", "success", "warning"), 1)
      )
    })
    
    layout_columns(fill = FALSE, !!!boxes)
  })
  
  # Dynamic range outputs for each numeric column
  observe({
    req(data(), numeric_cols())
    
    lapply(numeric_cols(), function(col) {
      output_id <- paste0("range_", make.names(col))
      output[[output_id]] <- renderText({
        req(filtered_data())
        min_val <- min(filtered_data()[[col]], na.rm = TRUE)
        max_val <- max(filtered_data()[[col]], na.rm = TRUE)
        sprintf("%.2f - %.2f", min_val, max_val)
      })
    })
  })
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    req(data())
    df <- data()
    
    # Filter by site if site column exists and filter is active
    if (!is.null(site_col()) && input$siteFilter != "All Sites") {
      df <- df %>% filter(.data[[site_col()]] == input$siteFilter)
    }
    
    # Filter by date range if date column exists
    if (!is.null(date_col()) && !is.null(input$dateRange)) {
      df <- df %>% filter(.data[[date_col()]] >= input$dateRange[1] & 
                            .data[[date_col()]] <= input$dateRange[2])
    }
    
    return(df)
  })
  
  # Main plot
  output$mainPlot <- renderPlotly({
    req(filtered_data(), input$plotType)
    data <- filtered_data()
    
    if (input$plotType == "Time Series") {
      req(date_col(), input$variables, length(input$variables) > 0)
      
      # Prepare data for time series - Modified to include site column
      if (!is.null(site_col())) {
        plot_data <- data %>%
          select(!!sym(date_col()), all_of(input$variables), !!sym(site_col())) %>%
          pivot_longer(cols = all_of(input$variables), names_to = "Parameter", values_to = "Value")
        
        p <- ggplot(plot_data, aes(x = !!sym(date_col()), y = Value, color = !!sym(site_col()))) +
          geom_point() +
          facet_wrap(~Parameter, scales = "free_y") +
          theme_minimal() +
          labs(title = "Time Series of Water Quality Parameters by Site",
               y = "Value", x = "Date")
      } else {
        plot_data <- data %>%
          select(!!sym(date_col()), all_of(input$variables)) %>%
          pivot_longer(cols = -!!sym(date_col()), names_to = "Parameter", values_to = "Value")
        
        p <- ggplot(plot_data, aes(x = !!sym(date_col()), y = Value, color = Parameter)) +
          geom_point() +
          facet_wrap(~Parameter, scales = "free_y") +
          theme_minimal() +
          labs(title = "Time Series of Water Quality Parameters",
               y = "Value", x = "Date")
      }
      
      ggplotly(p)
      
    } else if (input$plotType == "Scatter Plot") {
      req(input$xVar, input$yVar)
      
      p <- ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
        geom_point(aes_string(color = if(!is.null(site_col())) site_col() else NULL), alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE) +
        theme_minimal() +
        labs(title = paste(gsub("_", " ", input$yVar), "vs", gsub("_", " ", input$xVar)),
             x = gsub("_", " ", input$xVar),
             y = gsub("_", " ", input$yVar))
      
      ggplotly(p)
      
    } else if (input$plotType == "Box Plot") {
      req(site_col(), input$variables, length(input$variables) > 0)
      
      # Prepare data for box plot
      plot_data <- data %>%
        select(!!sym(site_col()), all_of(input$variables)) %>%
        pivot_longer(cols = -!!sym(site_col()), names_to = "Parameter", values_to = "Value")
      
      p <- ggplot(plot_data, aes_string(x = site_col(), y = "Value", fill = site_col())) +
        geom_boxplot() +
        facet_wrap(~Parameter, scales = "free_y") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Distribution of Water Quality Parameters by Site")
      
      ggplotly(p)
      
    } else if (input$plotType == "Histogram") {
      # Prepare data for histogram
      plot_data <- data %>%
        select(all_of(input$variables)) %>%
        pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value")
      
      p <- ggplot(plot_data, aes(x = Value, fill = Parameter)) +
        geom_histogram(alpha = 0.7, bins = 30) +
        facet_wrap(~Parameter, scales = "free") +
        theme_minimal() +
        labs(title = "Histograms of Water Quality Parameters")
      
      ggplotly(p)
    }
  })
  
  # Summary statistics table
  output$summaryTable <- renderTable({
    req(filtered_data(), numeric_cols())
    req(length(numeric_cols()) > 0)
    
    # Use only the numeric columns that are available
    data <- filtered_data() %>%
      select(all_of(numeric_cols()))
    
    # Create a summary for all numeric columns
    data_summary <- data.frame(
      Parameter = gsub("_", " ", numeric_cols()),
      Min = sapply(data, min, na.rm = TRUE),
      Mean = sapply(data, mean, na.rm = TRUE),
      Median = sapply(data, median, na.rm = TRUE),
      Max = sapply(data, max, na.rm = TRUE),
      SD = sapply(data, sd, na.rm = TRUE)
    )
    
    # Round to 2 decimal places
    data_summary[, 2:6] <- round(data_summary[, 2:6], 2)
    
    data_summary
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10, scrollX = TRUE),
              filter = 'top',
              rownames = FALSE)
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("water_quality_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)