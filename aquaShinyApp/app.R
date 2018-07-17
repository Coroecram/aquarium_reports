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


# colnames(df_aqua)
head(df_aqua)

# Define UI for application that plots features of movies
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # App title
  titlePanel("Aquarium Pi pH, Temperature, and Lux Readings", windowTitle = "Aquarium Pi"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Inputs
    sidebarPanel(

      h3("Plotting"),      # Third level header: Plotting

      # Select variable for y-axis
      selectInput(inputId = "outputFormat",
                  label = "Output Format:",
                  choices = c("Tabs",
                              "2 x 2",
                              "Vertical"),
                  selected = "Tabs"),

      hr(),

      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),

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
                           br(),
                           DT::dataTableOutput(outputId = "readtable"))
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {

  # Create a subset of data filtering for selected title types
  movies_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(movies, title_type %in% input$selected_type)
  })

  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })

  # Create scatterplot object of the pH data
  output$phPlot <- renderPlot({
    ggplot(data = df_aqua, aes_string(x = df_aqua$observed_at, y = df_aqua$ph_read)) +
      geom_line() +
      labs(x = "Time Observed",
           y = "pH Reading",
           title = toTitleCase("Aquarium pH"))
  })

  # Create scatterplot object of the Temperature data
  output$tempPlot <- renderPlot({
    ggplot(data = df_aqua, aes_string(x = df_aqua$observed_at, y = df_aqua$temp_read)) +
      geom_line() +
      labs(x = "Time Observed",
           y = "Celsius Reading",
           title = toTitleCase("Aquarium Temperature"))
  })

  # Create scatterplot object of the pH data
  output$luxPlot <- renderPlot({
    ggplot(data = df_aqua, aes_string(x = df_aqua$observed_at, y = df_aqua$lux_read)) +
      geom_line() +
      labs(x = "Time Observed",
           y = "Lux Reading",
           title = toTitleCase("Aquarium Lux"))
  })

  # # Create description of plot
  # output$description <- renderText({
  #   paste("The plot above shows the relationship between",
  #         x(),
  #         "and",
  #         y(),
  #         "for",
  #         nrow(movies_selected()),
  #         "movies.")
  # })

  # Print data table if checked
  output$readtable <- DT::renderDataTable(
    DT::datatable(data = df_aqua,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  )

  # Display data table tab only if show_data is checked
  # observeEvent(input$show_data, {
  #   if(input$show_data){
  #     showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
  #   } else {
  #     hideTab(inputId = "tabsetpanel", target = "Data")
  #   }
  # })

}

# Create Shiny app object
shinyApp(ui = ui, server = server)
