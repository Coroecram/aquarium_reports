library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
require("RPostgreSQL")
pw <- {
  "test"
}

# loads the PostgreSQL driver
drv <- DBI::dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "test_python",
                 host = "localhost", port = 5432,
                 user = "test_user", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "aquarium_data")
# TRUE

# query the data from postgreSQL
df_aqua <- dbGetQuery(con, "SELECT * from aquarium_data;")

dbDisconnect(con)

min_time = min(df_aqua$observed_at)
max_time = max(df_aqua$observed_at)


# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # App title
  titlePanel("Aquarium Pi pH, Temperature, and Lux Readings", windowTitle = "Aquarium Pi"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Inputs
    sidebarPanel(
      # Date input
      sliderInput("timeRange", label = "Time range",
                   min = as.POSIXct(trunc(min_time, "mins")),
                   max = as.POSIXct(trunc(max_time, "mins")),
                   value = c(as.POSIXct(trunc(min_time, "mins")),
                             as.POSIXct(trunc(max_time, "mins"))),
                   timeFormat = "%m/%d/%y %H:%M%p"
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

  selected_readings <- reactive({
    df_aqua %>% filter (observed_at > input$timeRange[1] & observed_at < input$timeRange[2])
  })

  # Create scatterplot object of the pH data
  output$phPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$ph_read)) +
      geom_line() +
      xlim(input$timeRange[1], input$timeRange[2]) +
      labs(x = "Time Observed",
           y = "pH Reading",
           title = toTitleCase("Aquarium pH"))
  })

  # Create scatterplot object of the Temperature data
  output$tempPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$temp_read)) +
      geom_line() +
      xlim(input$timeRange[1], input$timeRange[2]) +
      labs(x = "Time Observed",
           y = "Celsius Reading",
           title = toTitleCase("Aquarium Temperature"))
  })

  # Create scatterplot object of the pH data
  output$luxPlot <- renderPlot({
    ggplot(data = selected_readings(), aes_string(x = selected_readings()$observed_at, y = selected_readings()$lux_read)) +
      geom_line() +
      xlim(input$timeRange[1], input$timeRange[2]) +
      labs(x = "Time Observed",
           y = "Lux Reading",
           title = toTitleCase("Aquarium Lux"))
  })

  df_aqua$str_observed_at = format(df_aqua$observed_at, "%B %d, %Y %H:%M:%S %p")

  output$tabTable <- DT::renderDataTable(
    DT::datatable(data = subset(selected_readings(), select = c("str_observed_at", "ph_read", "temp_read", "lux_read")),
                  colnames = c("Observed at", "pH Reading", "Temperature Reading (C)", "Lux Reading"),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  )

  output$beneathTable <- DT::renderDataTable(
      DT::datatable(data = subset(selected_readings(), select = c("str_observed_at", "ph_read", "temp_read", "lux_read")),
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
