# Importing Shiny
library(shiny)
# Importing Shiny Themes
library(shinythemes)
# Loading Tidyverse
library(tidyverse)
# Loading Leaflet
library(leaflet)
# Loading Tigris
library(tigris)

# Getting states geo files.
states <- states(cb=T)

# Loading Zillow's CSV about Housing prices in the US.
data <- read_csv("State_time_series.csv")

# Creating a mappable object.
filtered_data <- data %>%
  # Matching state name with its abbreviations.
  # First the initial string is split based on the case, i.e. "NewJersey" -> "New Jersey"
  # After that, the names of the states are converted into abbreviations, i.e. "New Jersey" -> "NJ"
  # This abbreviation is stored in the newly mutated `state` column.
  mutate(state = state.abb[match(
    substring(
      gsub(
        '([[:upper:]])', ' \\1', RegionName
      ), 
      2), state.name)]) %>% 
  # Selecting Relevant columns of the cleaned dataset, pertaining to cost of houses.
  select(
    Date,
    state,
    `CostPerSqFt` = ZHVIPerSqft_AllHomes,
    `1BHK` = ZHVI_1bedroom, 
    `2BHK` = ZHVI_2bedroom,
    `3BHK` = ZHVI_3bedroom, 
    `4BHK` = ZHVI_4bedroom, 
    `5BHK+` = ZHVI_5BedroomOrMore
  )

# Deriving the options for the "dates".
year_choices <- filtered_data %>% 
  group_by(Date) %>% 
  summarise()

# Deriving the options for the "dates".
state_choices <- filtered_data %>% 
  group_by(state) %>% 
  summarise()

# Deriving the options for the "gender" input panel.
metric_choices <- filtered_data %>%
  select(-1, -2) %>%
  colnames()

# Define UI for application that analyzes closenss of actual voteshare vs actual voteshare. 
ui <- fluidPage( theme = shinytheme("slate"),
                 
                 # Application title
                 titlePanel("Analyzing Pricing of Housing in the US between 1996 and 2017"),
                 
                 sidebarLayout(
                   sidebarPanel(
                     # Select Input for "Date".
                     selectInput(inputId = "year",
                                 label = "Year",
                                 choices = year_choices,
                                 selected = "2017-12-31"),
                     # Select Input for "Metric".
                     selectInput(inputId = "metric",
                                 label = "Metric",
                                 choices = metric_choices,
                                 selected = "1BHK"),
                     selectInput(inputId = "state",
                                 label = "State",
                                 choices = state_choices,
                                 selected = "NY")
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     # Thanks to Maclaine Fields for the tabs idea for summary, plot and data view!
                     tabsetPanel(type = "tabs",
                                 # Summary for tabs.
                                 tabPanel("Summary", 
                                          h1("Summary"),
                                          p("The Huffington Post released an article in 2018 titled - 'America's Housing Crisis is a Ticking Time Bomb' - which hypothesises that low cost housining is disappearing from the US housing market, America isn't building enough homes to serve the rate of increase of the population, and that on the whole America's cities are unaffordable for most of the countries working class population."),
                                          p("The aim of this app is to help people learn more about the trends and deviations of the housing prices in the various states of the US over the past 2 decades. The data is sourced from Zillow, which is an online realestate data company founded in 2006, that has collected data about the various statistics about housing in the US between 1996 to 2017. The data is available on Quandl.")
                                          ),
                                 # Tab for viewing map!
                                 tabPanel("Map", leafletOutput("mymap")),
                                 # Tab for viewing linechart!
                                 tabPanel("Plot", plotOutput("lineChart")),
                                 # Tab for the data view!
                                 tabPanel("Data Table", dataTableOutput("data")))
                                 )
)
)

# Define server logic required to show the output.
server <- function(input, output) {

  output$mymap <- renderLeaflet({
    req(input$year, input$metric)
    
    data <- filtered_data %>% 
      filter(Date == input$year) %>% 
      select(state, value = input$metric)
    
    # Getting rid of rows with NA values
    data <- subset(data, !is.na(value))
    
    # Geojoining the states geolayer and the cleaned mappable data.
    data <- geo_join(states, data, "STUSPS", "state")
    
    # Creating a color palette based on the number range in the total column
    pal <- colorNumeric("Greens", domain=data$value)
    
    popup_sb <- paste0("Cost: ", as.character(data$value))
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addPolygons(data = data , 
                  fillColor = ~pal(data$value), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb
      ) %>%
      addLegend(pal = pal, 
                values = data$value, 
                position = "bottomright", 
                title = "Cost of housing based on size.")
  })
  
  output$lineChart <- renderPlot({
    req(input$state, input$metric)
    
    data <- filtered_data %>% 
      filter(state == input$state) %>% 
      select(state, Date, value = input$metric)
    
    ggplot(data, aes(x = Date, y = value)) + geom_line(color = "Red")
  })
  
  output$data <- renderDataTable({
    filtered <- reactive({
      df<- filtered_data %>%
        filter(Date == input$year)
        as_data_frame()
    })
    filtered()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)