#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(shinydashboard)
library(mapview)

df <- read.csv('Business_Licenses_geocoded.csv')

df <- df %>% filter((df$City == 'South Bend' | df$City == 'SOUTH BEND' | df$City == 'NOTRE DAME') & 
                      df$Classifi_1 != '#N/A' & 
                      df$License__1 == 'Active and Licensed')
df %>% group_by(Classifi_1) %>% count()
df <- df %>% filter(df$Classifi_1 == 'AUTOMOTIVE REPAIR & SVC' | 
                      df$Classifi_1 == 'HOTEL/MOTEL' | 
                      df$Classifi_1 == 'RESTAURANTS N-Z' | 
                      df$Classifi_1 == 'RESTAURANTS A-M' |
                      df$Classifi_1 == 'TAXI VEHICLE' |
                      df$Classifi_1 == 'PUBLIC PARKING FACILITY')


df_sf <- st_as_sf(df, coords = c('X','Y')) %>% st_set_crs(value = 4326)
df_tidy <- df_sf %>% na.omit()
a <- df_tidy$Zip_Code
a[nchar(a) > 5] <- gsub('^(.{5})(.*)$', '\\1-\\2', a[nchar(a) > 5])
df_tidy$Zip_Code <- a




ui <- dashboardPage(
  dashboardHeader(title = "Businesses in South Bend"),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      titlePanel("Business by Type"),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = 'Classifi_1',
                        label = 'Choose Business Type',
                        choices = unique(df_tidy$Classifi_1),
                        selected = 'TAXI VEHICLE')
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(title = 'Map',
                     leafletOutput(outputId = 'mapPlot'))
          )
        )
    )
)))

server <- function(input, output) {
  fran <- reactive({
    f <- franconia
    if(input$Classifi_1 != "All") f <- df_tidy %>% filter(Classifi_1 == input$Classifi_1)
    
    f
  })
  
  output$mapPlot <- renderLeaflet({
    
    #get the data
    f <- fran()
    
    #generate a map
    mapview(f, zcol = "Classifi_1")@map
  })
}
    
# Run the application 
shinyApp(ui = ui, server = server)