library(tidyverse)
library(shiny)
library(readxl)
library(here)
library(lubridate)
library(shinythemes)
#library(basemaps)
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

# Moorea LETR to find map shapefile
ggplot(data = c_sf) +
  geom_sf()

#user interface:
ui <- navbarPage("Moorea Corals", theme = shinytheme("superhero"),
                 tabPanel("Map of Moorea",
                          titlePanel("Coral Data"),
                          sidebarLayout(
                            sidebarPanel("put my widgets here",
                                         radioButtons(inputId = "genus",
                              label = "Choose Coral Species",
                              choices = c("Pocillopora" = "poc","Acropora" = "acr")
                 ),
                 dateInput("date", label = h3("Date input"), value = "2014-01-01"),

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
  tabPanel("Spatial Distribution of Coral Samples",
           leafletOutput("locations", width = "100%", height = "100%")),
  tabPanel("Coral Plot"),
  tabPanel("Info & Data Sources"),
  # mainPanel(
  # img(src = 'poc.jpg', align = "left", height = 200, width = 300),
  # img(src = 'acr.jpg', align = "left", height = 200, width = 300)
  # the rest of our code
  # )
  tabPanel("Coral vs Climate Change"),
  tabPanel("Info & Data Sources"),
)

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



#sliderInput(inputId = "perc_bleach",
            #label = "Percent Bleached", min = 0, max = 100, value =50, step = NULL,
            #round = FALSE, format = "#,##0.#####",
            #locale = "us", ticks = TRUE, animate = FALSE)
#output$coral_slider <- renderPrint ({ input$perc_bleach})
#textOutput('perc_bleach')






#Combine above into an app:
shinyApp(ui = ui, server = server)

