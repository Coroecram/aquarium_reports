library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(pool)

require("RPostgreSQL")

aws_pg <- dbPool(
  DBI::dbDriver("PostgreSQL"),
  dbname = "aquarium",
  host = Sys.getenv("AWS_PG_HOST"), port = 5432,
  user = Sys.getenv("AWS_PG_USER"), password = Sys.getenv("AWS_PG_PW")
)

time_now = Sys.time();

# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("cosmo"),

  # App title
  titlePanel("Aquarium Pi pH, Temperature, and Lux Readings", windowTitle = "Aquarium Pi"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Inputs
    sidebarPanel(
      dateRangeInput(inputId = "dateRange", label = NULL, start = Sys.Date()-7, end = Sys.Date(), min = NULL,
        max = NULL, format = "MM dd, yyyy", startview = "month", weekstart = 0,
        language = "en", separator = " to ", width = NULL, autoclose = TRUE),

      # hr(),
      h4("Start Time"),
      fluidRow(
        column( width = 3,
          selectInput(inputId = "start_time_hr", label = NULL,
                     choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12")
                    )
        ),
        column( width = 3,
         selectInput(inputId = "start_time_min", label = NULL,
                      choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60")
                    )
        )
      ),

      h4("Start Time"),
      fluidRow(
        column( width = 3,
          selectInput(inputId = "end_time_hr", label = NULL,
                     choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12")
                    )
        ),
        column( width = 3,
         selectInput(inputId = "end_time_min", label = NULL,
                      choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60")
                    )
        ),
        column( width = 3,
          selectInput(inputId = "end_time_mer", label = NULL,
                     choices = c("AM", "PM"), selected = "AM"
                    )
        )
      ),

      # Select variable for y-axis
      # selectInput(inputId = "outputFormat",
      #             label = "Output Format:",
      #             choices = c("Tabs",
      #                         "2 x 2",
      #                         "Vertical"),
      #             selected = "Tabs"),

      hr(),

      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show Data Table Below",
                    value = FALSE),

      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
  ),

    # Output:
    mainPanel(

      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "pH",
                           plotOutput(outputId = "phPlot")),
                           br(),
                  tabPanel(title = "Temp",
                          plotOutput(outputId = "tempPlot")),
                          br(),
                  tabPanel(title = "Lux",
                         plotOutput(outputId = "luxPlot")),
                         br(),
                  tabPanel(title = "Data",
                         DT::dataTableOutput(outputId = "tabTable"))
      ),
        conditionalPanel("input.show_data == true", DT::dataTableOutput(outputId = "beneathTable"))
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {

  selected_readings <- reactive ({ invalidateLater(30000)
                         intermediate <- aws_pg %>% tbl("aquarium_data") %>%
                         filter (observed_at > input$dateRange[1] & observed_at <= input$dateRange[2]) %>% collect()
                         intermediate$str_observed_at <- format(intermediate$observed_at, "%B %d, %Y %H:%M:%S %p")
                         intermediate
                       })

  table_data <- reactive({
                          subset(selected_readings(), select = c("str_observed_at", "ph_read", "temp_read", "lux_read"))
                        })

  # Create scatterplot object of the pH data
  output$phPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$ph_read)) +
      geom_line() +
      # xlim(input$dateRange[1], input$dateRange[2]) +
      labs(x = "Time Observed",
           y = "pH Reading",
           title = "Aquarium pH")
  })

  # Create scatterplot object of the Temperature data
  output$tempPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$temp_read)) +
      geom_line() +
      # xlim(input$dateRange[1], input$dateRange[2]) +
      labs(x = "Time Observed",
           y = "Celsius Reading",
           title = "Aquarium Temperature")
  })

  # Create scatterplot object of the pH data
  output$luxPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$lux_read)) +
      geom_line() +
      # xlim(input$dateRange[1], input$dateRange[2]) +
      labs(x = "Time Observed",
           y = "Lux Reading",
           title = "Aquarium Lux")
  })

  output$tabTable <- DT::renderDataTable(
    DT::datatable(data = table_data(),
                  colnames = c("Observed at", "pH Reading", "Temperature Reading (C)", "Lux Reading"),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  )

  output$beneathTable <- DT::renderDataTable(
      DT::datatable(data = table_data(),
                  colnames = c("Observed at", "pH Reading", "Temperature Reading (C)", "Lux Reading"),
                  options = list(pageLength = 5),
                  rownames = FALSE)
  )


                  observeEvent(input$show_data, {
                    if(input$show_data){
                      hideTab(inputId = "tabsetpanel", target = "Data")
                    } else {
                      showTab(inputId = "tabsetpanel", target = "Data")
                    }
                  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
