satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
bq_auth(path = satoken)

# Step 2: Connect to the BigQuery database mimic4_v2_2
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

dbListTables(con_bq)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      title = "Patient characteristics",
      sidebarLayout(
        sidebarPanel(
          selectInput("analysisType", "Variable of interest", 
                      choices = c("last_careunit", 
                                  "Lab Events", "Marital Status", 
                                  "Race", "Gender", "Vital", "Insurance")),
          checkboxInput("removeOutliers", 
                        "Remove outliers in IQR method for measurements?", 
                        FALSE)
        ),
        mainPanel(
          plotOutput("plotOutput")
        )
      )
    ),
    tabPanel(
      title = ("Patient Data Visualization"),
      sidebarLayout(
        sidebarPanel(
          # input subject_id
          numericInput("sid", "Enter a patient ID", value = 10004113) 
        ),
        mainPanel(
          plotOutput("timeSeriesPlot")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  data <- readRDS("mimic_icu_cohort_A.rds")
  filtered_data <- readRDS("mimic_icu_cohort.rds")
  # Reactive expression that debounces the input
  patient_data <- reactive({
    sid <- as.numeric(input$sid) 
    
    sid_info_query <- tbl(con_bq, "patients") %>% filter(subject_id == sid)
    sid_adt_query <- tbl(con_bq, "transfers") %>% filter(subject_id == sid)
    sid_lab_query <- tbl(con_bq, "labevents") %>% filter(subject_id == sid)
    sid_proc_query <- tbl(con_bq, "procedures_icd") %>% 
      filter(subject_id == sid) %>%
      left_join(tbl(con_bq, "d_icd_procedures"), 
                by = c("icd_code", "icd_version"))
    sid_diag_query <- tbl(con_bq, "diagnoses_icd") %>% 
      filter(subject_id == sid) %>%
      left_join(tbl(con_bq, "d_icd_diagnoses"), 
                by = c("icd_code", "icd_version"))
    sid_adm_query <- tbl(con_bq, "admissions") %>% filter(subject_id == sid)
    
    sid_info <- sid_info_query %>% collect()
    sid_adt <- sid_adt_query %>% collect()
    sid_lab <- sid_lab_query %>% collect()
    sid_proc <- sid_proc_query %>% collect()
    sid_diag <- sid_diag_query %>% collect()
    sid_adm <- sid_adm_query %>% collect()
    
    
    if("intime" %in% names(sid_adt)) {
      sid_adt$intime <- ymd_hms(sid_adt$intime)
    }
    if("outtime" %in% names(sid_adt)) {
      sid_adt$outtime <- ymd_hms(sid_adt$outtime)
    }
    if("charttime" %in% names(sid_lab)) {
      sid_lab$charttime <- ymd_hms(sid_lab$charttime)
    }
    if("chartdate" %in% names(sid_proc)) {
      sid_proc$chartdate <- as.POSIXct(paste(sid_proc$chartdate, "00:00:00"))
    }
    if("admintime" %in% names(sid_adm)) {
      sid_adm$admintime <- ymd_hms(sid_adm$admintime)
    }
    if("dischtime" %in% names(sid_adm)) {
      sid_adm$dischtime <- ymd_hms(sid_adm$dischtime)
    }
    list(sid_info = sid_info, sid_adt = sid_adt, 
         sid_lab = sid_lab, sid_proc = sid_proc, 
         sid_diag = sid_diag, sid_adm = sid_adm)
  })
  
  
  output$plotOutput <- renderPlot({
    if (input$analysisType == "last_careunit") {
      p <- ggplot(data, aes_string(x = "`last_careunit`")) +
        geom_bar() +
        labs(x = "Last Care Unit", y = "Count") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p 
    } else if (input$analysisType == "Lab Events") {  
      labs_data <- melt(data, measure.vars = c(
        "Sodium", "Chloride", "Creatinine", 
        "Potassium", "Glucose", "Bicarbonate"
      ))
      
      if (input$removeOutliers) {
        # Remove outliers based on IQR
        labs_data <- labs_data %>% 
          group_by(variable) %>% 
          filter(value > boxplot.stats(value)$stats[1], 
                 value < boxplot.stats(value)$stats[5])
      }
      
      p <- ggplot(labs_data, aes(x = variable, y = value)) +
        geom_boxplot() +
        labs(x = "Variable", y = "Value") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5) 
        ) +
        coord_flip()
      p 
    } else if (input$analysisType == "Marital Status") {  
      p <- ggplot(data, aes(x = marital_status)) +
        geom_bar() +
        labs(x = "Count", y = "marital status") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p
    } else if (input$analysisType == "Race") {
      p <- ggplot(filtered_data, aes(x = race)) +
        geom_bar() +
        labs(x = "race", y = "Count") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p
    } else if (input$analysisType == "Gender") {
      p <- ggplot(filtered_data, aes(x = gender)) +
        geom_bar(width = 0.5) +
        labs(x = "Gender", y = "Count") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p
    } else if (input$analysisType == "Vital") {
      vitals_data <- melt(data, measure.vars = c(
        "Heart Rate", "Non Invasive Blood Pressure systolic", 
        "Non Invasive Blood Pressure diastolic", 
        "Temperature Fahrenheit", "Respiratory Rate"
      ))
      
      if (input$removeOutliers) {
        # Remove outliers based on IQR for each variable
        vitals_data <- vitals_data %>% 
          group_by(variable) %>% 
          filter(value > boxplot.stats(value)$stats[1], 
                 value < boxplot.stats(value)$stats[5])
      }
      
      p <- ggplot(vitals_data, aes(x = variable, y = value)) +
        geom_boxplot() +
        labs(x = "Variable", y = "Value") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p
    } else if (input$analysisType == "Insurance") {
      p <- ggplot(filtered_data, aes(x = insurance)) +
        geom_bar(width = 0.5) +
        labs(x = "Insurance Type", y = "Count") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line.x = element_line(color = "black", size = 0.5),
          axis.line.y = element_line(color = "black", size = 0.5)
        ) +
        coord_flip()
      p
    }
  })
  
  
  output$timeSeriesPlot <- renderPlot({
    # Fetch data from the reactive expression
    data <- patient_data()
    
    # Check if data is available
    if(is.null(data)) return()
    
    # Use ggplot to create the plot
    ggplot() +
      # Add segments for ADT
      geom_segment(
        data = data$sid_adt %>% filter(eventtype != "discharge"),
        mapping = aes(
          x = intime,
          xend = outtime,
          y = "ADT",
          yend = "ADT",
          color = careunit,
          linewidth = str_detect(careunit, "(ICU|CCU)")
        )
      ) +
      # Add points for labs
      geom_point(
        data = data$sid_lab %>% distinct(charttime, .keep_all = TRUE),
        mapping = aes(x = charttime, y = "Lab"),
        shape = '+',
        size = 5
      ) +
      # Add jitter for procedures
      geom_jitter(
        data = data$sid_proc,
        mapping = aes(
          x = chartdate, y = "Procedure",
          shape = str_sub(long_title, 1, 25)
        ),
        size = 3,
        height = 0
      ) +
      # Customization for plot titles and other labels
      labs(
        title = paste("Patient", input$sid, ", ",
                      data$sid_info$gender[1], ", ",
                      data$sid_info$anchor_age[1] + 
                        lubridate::year(data$sid_adm$admittime[1]) - 
                        data$sid_info$anchor_year[1], 
                      "years old, ",
                      tolower(data$sid_adm$race[1])),
        subtitle = paste(tolower(slice(data$sid_diag, 1:3)$long_title), 
                         collapse = "\n"),
        x = "Calendar Time",
        y = "",
        color = "Care Unit",
        shape = "Procedure"
      ) +
      guides(linewidth = "none") +
      scale_y_discrete(limits = rev) +
      theme_light() +
      theme(legend.position = "bottom", legend.box = "vertical")
  })
}

shinyApp(ui, server)
