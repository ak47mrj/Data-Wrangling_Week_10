#Library the needed packages
library(shiny)
library(tidyverse)
library(tidycensus)


#Load data from API
source(api.key.R)
census_api_key(api.key)


#Define UI for app
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("StateInput","State", choices = state.abb, selected = "NJ"),
      radioButtons("TypeInput","Type",choices = list("median_gross_rent",
                                                     "median_household_income",
                                                     "ratio"), selected = "median_gross_rent")
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  ),    
  titlePanel("American Community Survey")
)


#Define server for app
server <- function(input, output){
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(median_gross_rent = "B25064_001" , median_household_income = "B19013_001"),
      state = input$StateInput,
      geometry = TRUE
    ) %>% .[, -5] %>% data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  output$main_plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$TypeInput)) + geom_sf() + ggtitle(input$TypeInput) + 
      scale_fill_gradientn(colours = rainbow(7))
  })
}

#Run the application
shinyApp(ui = ui, server = server)
