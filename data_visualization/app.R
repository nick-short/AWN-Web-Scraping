#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tools)
library(tidyverse)
library(lubridate)
library(plotly)

# Read the CSV file; use filepath with subdirectory when troubleshooting line by line
time_series <- read_csv("pilot_alldata.csv")
#time_series <- read_csv("data_visualization/pilot_alldata.csv")

# Define a named character vector containing the choices for technology class
tech_choices <- unique(time_series$tech_class)
names(tech_choices) <- toTitleCase(str_replace(unique(time_series$tech_class), pattern = "_", replacement = " "))

# Define choices for time (month and year)
#month_choices <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#year_choices <- unique(str_sub(time_series$date, start = 1, end = 4))
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Disruptive Technologies Event Analysis - Data Visualization"),
   
   # Sidebar with a selectInput for technology class
   sidebarLayout(
      sidebarPanel(
        
        # Have the user select the technology class
        selectInput(inputId = "tech",
                    label = "Technology Class:",
                    choices = tech_choices,
                    selected = "drones")#,
        # Have the user select the month and year of the start (first) and end (last) date
        #selectInput(inputId = "first_month",
        #            label = "First Month:",
        #            choices = month_choices,
        #            selected = "Jan"),
        #selectInput(inputId = "first_year",
        #           label = "First Year:",
        #           choices = year_choices,
        #           selected = "1985"),
        #selectInput(inputId = "last_month",
        #            label = "Last Month:",
        #            choices = month_choices,
        #            selected = "Dec"),
        #selectInput(inputId = "last_year",
        #            label = "Last Year:",
        #            choices = year_choices,
        #            selected = "2018")
        ),
      
      # Main panel with a single time series plot for the selected technology class
      mainPanel(
        plotlyOutput("tsPlot")
      )
   )
)

# Define server logic required to plot a simple time series showing monthly
# number of news articles containing search terms related to a given technology
# class divided by the total number of news articles published in the same month
server <- function(input, output) {

   output$tsPlot <- renderPlotly({
     
     # Convert time inputs to .Date format (including a day of 01)
     #first_date <- mdy(paste(input$first_month, "01,", input$first_year, sep = " "))
     #last_date <- mdy(paste(input$last_month, "01,", input$last_year, sep = " "))
     
     # Generate time-series plot
     time_series %>% 
       filter(tech_class == input$tech) %>% # Filter by tech class
       #filter(date >= first_date & date <= last_date) %>% # Filter by time boundaries
       ggplot(aes_string(x = "date", y = "count")) +
       geom_line(size = 0.5) +
       ylab("Relative News Frequency") + 
       theme(axis.title.x = element_blank())
   }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

