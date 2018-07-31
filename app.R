library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)
library(pool)

source("palettes.R")

readRenviron("~/aws.Renviron")

require("RPostgreSQL")
Sys.setenv(TZ = "America/New_York")

require("RPostgreSQL")

aws_pg <- dbPool(
  PostgreSQL(),
  dbname = Sys.getenv('PG_DB'),
  host = Sys.getenv('PG_HOST'), port = Sys.getenv('PG_PORT'),
  user = Sys.getenv('PG_USER'), password = Sys.getenv('PG_PW')
)

time_now <- Sys.time();

# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),


  # App title
  titlePanel("Aquarium Pi pH, Temperature, and Lux Readings", windowTitle = "Aquarium Pi"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    sidebarPanel(
      h4("Latest Readings"),
      tags$h6(class="reading", textOutput("latesttime")),
      fluidRow(
        column( width = 3,
          tags$h5("pH"), tags$h6(class="reading", textOutput("latestph"))
        ),
        column( width = 3,
          tags$h5("°C"), tags$h6(class="reading", textOutput("latesttemp"))
        ),
        column( width = 3, inputId="lastday",
          tags$h5("Lux"), tags$h6(class="reading", textOutput("latestlux"))
        )
      ),
      hr(),
      h4("Date Range"),
      dateRangeInput(inputId = "dateRange", label = NULL, start = Sys.Date()-7, end = Sys.Date(), min = NULL,
        max = NULL, format = "MM dd, yyyy", startview = "month", weekstart = 0,
        language = "en", separator = " to ", width = NULL, autoclose = TRUE),

      # hr(),
      h4("Start Date Time"),
      fluidRow(
        column( width = 3,
          selectInput(inputId = "start_time_hr", label = NULL, selected = "12",
                     choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12")
                    )
        ),
        column( width = 3,
         selectInput(inputId = "start_time_min", label = NULL, selected = "00",
                      choices = c("00", "15","30","45")
                    )
        ),
        column( width = 3,
          selectInput(inputId = "start_time_mer", label = NULL, selected = "AM",
                     choices = c("AM", "PM")
                    )
        )
      ),

      h4("End Date Time"),
      fluidRow(
        column( width = 3,
          selectInput(inputId = "end_time_hr", label = NULL, selected = "11",
                     choices = c("00","01","02","03","04","05","06","07","08","09","10","11","12")
                    )
        ),
        column( width = 3,
         selectInput(inputId = "end_time_min", label = NULL, selected = "59",
                      choices = c("00", "15","30","45")
                    )
        ),
        column( width = 3,
          selectInput(inputId = "end_time_mer", label = NULL, selected = "PM",
                     choices = c("AM", "PM")
                    )
        )
      ),

      hr(),
      h4("Previous:"),

      fluidRow(
        column( width = 3, inputId="lastmonth",
          actionButton(inputId="lastmonth", class="date-range", label="30 Days", icon=icon("calendar"))
        ),
        column( width = 3, inputId="lastweek",
          actionButton(inputId="lastweek", class="date-range", label="7 Days", icon=icon("calendar"))
        ),
        column( width = 3, inputId="lastday",
          actionButton(inputId="lastday", class="date-range", label="1 Day", icon=icon("calendar"))
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
                    label = "Show Data Table Beneath Graph",
                    value = FALSE),

      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", class="credit-img", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", class="credit-img", height = "30px"),
         " by ", a(href="http://www.mikebudnick.com", target="_blank", "Michael Budnick"))
  ),

    # Output:
    mainPanel(

      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Aquarium pH",
                           plotOutput(outputId = "phPlot")),
                           br(),
                  tabPanel(title = "Aquarium °C",
                          plotOutput(outputId = "tempPlot")),
                          br(),
                  tabPanel(title = "Ambient Lux",
                         plotOutput(outputId = "luxPlot")),
                         br(),
                  tabPanel(title = "Data Table",
                         DT::dataTableOutput(outputId = "tabTable"))
      ),
        conditionalPanel("input.show_data == true", DT::dataTableOutput(outputId = "beneathTable"))
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {

  start_datetime <- reactive ({
    as.POSIXct(paste(as.character(input$dateRange[1]), " ", input$start_time_hr, ":", input$start_time_min, input$start_time_mer, sep = ""), tz="UTC", format = "%F %I:%M%p")
  })

  end_datetime <- reactive ({
    as.POSIXct(paste(as.character(input$dateRange[2]), " ", input$end_time_hr, ":", input$end_time_min, input$end_time_mer, sep = ""), tz="UTC", format = "%F %I:%M%p")
  })

  changeRange <- function(start_time, end_time) {
    start_time <- lubridate::round_date(start_time, "15 minutes")
    updateDateRangeInput(session, "dateRange",
     start = date(start_time),
     end = date(end_time)
    )
    if(am(end_time) == TRUE) {
      meridian <- "AM"
      hour <- hour(start_time)
      hour <- if (hour < 10) paste0("0", toString(hour)) else hour
    }  else {
      meridian <-"PM"
      hour <- hour(start_time)
      hour <- if (hour != 12) (hour(start_time) - 12) else hour
      hour <- if (hour < 10) paste0("0", toString(hour)) else hour
    }

    minute <- minute(start_time)
    minute <- if (minute < 10) paste0("0", toString(minute)) else minute

    updateSelectInput(session, "start_time_hr", selected = toString(hour))
    updateSelectInput(session, "start_time_min", selected = toString(minute))
    updateSelectInput(session, "start_time_mer", selected = meridian)

    updateSelectInput(session, "end_time_hr", selected = toString(hour))
    updateSelectInput(session, "end_time_min", selected = toString(minute))
    updateSelectInput(session, "end_time_mer", selected = meridian)
  }

  observeEvent(input$lastmonth, {
    start_time <- now()
    hour(start_time) <- hour(start_time)
    day(start_time) <- day(start_time) - 30
    end_time <- now()
    changeRange(start_time, end_time)
  })

  observeEvent(input$lastweek, {
    start_time <- now()
    hour(start_time) <- hour(start_time)
    week(start_time) <- week(start_time) - 1
    end_time <- now()
    changeRange(start_time, end_time)
  })

  observeEvent(input$lastday, {
    start_time <- now()
    hour(start_time) <- hour(start_time)
    day(start_time) <- day(start_time) - 1
    end_time <- now()
    changeRange(start_time, end_time)
  })

  latest_reading <- reactive ({
    invalidateLater(300000)
    intermediate <- aws_pg %>% tbl("aquarium_data", in_schema("public")) %>% top_n(n=1, wt=id) %>% collect()
    if(length(intermediate$observed_at) != 0) {
      attributes(intermediate$observed_at)$tzone = "UTC" #CANNOT CHANGE TZ OF SHINY SERVER
      intermediate$str_observed_at <- format(intermediate$observed_at, "%B %d, %Y %I:%M:%S %p")
      intermediate[order(intermediate$id),]
    }
  })

  selected_readings <- reactive ({
    invalidateLater(300000)
    start_time <- start_datetime()
    end_time <- end_datetime()
    intermediate <- aws_pg %>% tbl("aquarium_data", in_schema("public")) %>%
                         filter (observed_at > start_time & observed_at <= end_time) %>% collect()
                         if(length(intermediate$observed_at) != 0) {
                           attributes(intermediate$observed_at)$tzone = "UTC" #CANNOT CHANGE TZ OF SHINY SERVER
                           intermediate$str_observed_at <- format(intermediate$observed_at, "%B %d, %Y %H:%M:%S")
                           intermediate[order(intermediate$id),]
                         }
                       })

  table_data <- reactive({
                          req(selected_readings()$observed_at)
                          subset(selected_readings(), select = c("str_observed_at", "ph_read", "temp_read", "lux_read"))
                        })

  output$latesttime <- renderText({ req(latest_reading())
                                    toString(tail(latest_reading()$str_observed_at, 1))
                                  })
  output$latestph <- renderText({ req(latest_reading())
                                  toString(tail(latest_reading()$ph_read, 1))
                                })
  output$latesttemp <- renderText({ req(latest_reading())
                                    toString(tail(latest_reading()$temp_read, 1))
                                  })
  output$latestlux <- renderText({ req(latest_reading())
                                   toString(tail(latest_reading()$lux_read, 1))
                                 })

  # Create scatterplot object of the pH data
  output$phPlot <- renderPlot({
    req(selected_readings()$observed_at)
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$ph_read,  colour = selected_readings()$ph_read)) +
      geom_line(aes(group = 1)) +
      scale_color_custom(palette = "ph", discrete = FALSE) +
      theme(
        panel.background = element_rect(fill = "lightblue",
                              colour = "lightblue",
                              size = 0.5, linetype = "solid"),
                              legend.title=element_blank()) +
      labs(x = "Time Observed",
           y = "pH Reading")
  })
  # Create scatterplot object of the Temperature °C data
  output$tempPlot <- renderPlot({
    req(selected_readings()$observed_at)
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$temp_read, colour = selected_readings()$temp_read)) +
      geom_line(aes(group = 1)) +
      scale_color_custom(discrete = FALSE, palette = 'temp') +
      theme(
        panel.background = element_rect(fill = "lightgreen",
                              colour = "lightgreen",
                              size = 0.5, linetype = "solid"),
                              legend.title=element_blank()) +
      labs(x = "Time Observed",
           y = "Celsius Reading")
  })

  # Create scatterplot object of the Lux data
  output$luxPlot <- renderPlot({
    req(selected_readings()$observed_at)
    ggplot(data = selected_readings(), aes(selected_readings()$observed_at, selected_readings()$lux_read, colour = selected_readings()$lux_read)) +
      geom_line(aes(group = 1)) +
      scale_color_custom(discrete = FALSE, palette = 'mixed') +
      theme(
          panel.background = element_rect(fill = "#fff7bc",
                                colour = "#fff7bc",
                                size = 0.5, linetype = "solid"),
                                legend.title=element_blank()
                              ) +
      labs(x = "Time Observed",
           y = "Lux Reading")
  })

  output$tabTable <- DT::renderDataTable(
    DT::datatable(data = table_data(),
                  colnames = c("Observed at", "pH Reading", "Temperature Reading (°C)", "Lux Reading"),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  )

  output$beneathTable <- DT::renderDataTable(
      DT::datatable(data = table_data(),
                  colnames = c("Observed at", "pH Reading", "Temperature Reading (°C)", "Lux Reading"),
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
