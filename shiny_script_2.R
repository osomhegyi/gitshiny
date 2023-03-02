library(tidyverse)
library(shiny)
library(readxl)
library(here)
library(lubridate)
library(shinythemes)
library(leaflet)
library(markdown)
library(tmap)
library(sp)
library(sf)



# Read in data?
coral_raw <- read_excel(here("data", "coral_data.xls")) %>%
  janitor::clean_names() %>%
  mutate(
    area = length*width
  )
# Data wrangling
coordinates <- coral_raw %>%
  select('lat', 'long', 'genus')

c_sf <- coordinates %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

fp <- read_sf(here::here("data","moorea.shp")) %>%
  filter(hasc_1=="PF.WI") %>%
  select(name_0,varname_1,geometry)


# Moorea LETR to find map shapefile
ggplot(data = c_sf) +
  geom_sf()

#user interface:
ui <- navbarPage("Moorea Corals", theme = shinytheme("superhero"),
                 tabPanel("Map of Moorea",
                          titlePanel("Map of Moorea"),

                 ),
                 tabPanel("Spatial Distribution of Coral Samples",
                          titlePanel("Spatial Distribution of Coral Samples"),
                          leafletOutput("locations", width = "100%", height = "100%")
                 ),
                 tabPanel("Coral Plot",
                          sidebarLayout(
                            sidebarPanel("Pick A Coral",
                                         radioButtons(inputId = "genus",
                                                      label = "Choose Coral Species",
                                                      choices = c("Pocillopora" = "poc","Acropora" = "acr")
                                         ),
                                         hr(),
                                         fluidRow(column(3, verbatimTextOutput("value"))
                                         ),
                                         selectInput(inputId = "pt_color",
                                                     label = "Select point color",
                                                     choices = c("Purple Coral" = "purple", "Orange Coral" = "orange"))
                            ),
                            mainPanel("Length to Width Distribution by Coral Species",
                                      plotOutput(outputId = "coral_plot"),
                                      tableOutput(outputId = "coral_table"))
                          )),
                 tabPanel("Info & Data Sources",
                          titlePanel("Works Cited"),
                          fluidRow(column(tags$img(src="poc.jpg",width="200px",height="300px"), width=2),

                                   br(),
                                   column(tags$img(src = "acr.jpg",width = "200px",height="300px"), width=2),
                                   br(),
                                   column(
                                     br(),
                                     p("INFO ABOUT CORAL",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                     br(),

                                     p("INFO ABOUT DATA",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),

                                     width=8),
                          )
                 ))

#Server function:
server <- function(input, output) {
  coral_select <- reactive({
    coral_raw %>%
      filter(species == input$genus)
  })

  coral_table <- reactive({
    coral_raw %>%
      filter(genus == input$genus) %>%
      group_by(site) %>%
      summarize(
        #mean_length = mean(length),
        #mean_width = mean(width),
        mean_area = mean(area),
        mean_perc_dead = mean(perc_dead),
        mean_perc_bleached = mean(perc_bleach)
      )
  })

  output$value <- renderPrint ({ input$date })

  output$coral_plot <- renderPlot({

    ggplot(data = coral_raw, aes(x = length, y = width)) +
      geom_point(color = input$pt_color)
  })

  output$coral_table <- renderTable({
    coral_table()
  })

  output$locations <- renderLeaflet({
    leaflet(data = coordinates) %>%
      addTiles() %>%
      addMarkers("long" =~LONGITUDE, "lat" =~LATITUDE) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
}










#Combine above into an app:
shinyApp(ui = ui, server = server)

