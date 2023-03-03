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
library(ggspatial)
library(janitor)
library(ggplot2)



# Read in data
coral_raw <- read_excel(here("data", "coral_data.xls")) %>%
  mutate(area = length*width)


# Making genus column all lowercase (but there is still a ? in one of the cells - need to fix)
coral_raw$genus <- tolower(coral_raw$genus)


# Wrangle the coordinates for tab1
coordinates <- coral_raw %>%
  select('lat', 'long', 'genus')

# This is for our map in tab1
c_sf <- coordinates %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# This is our base map for tab1
fp <- read_sf(here::here("data","moorea.shp")) %>%
  filter(hasc_1=="PF.WI") %>%
  select(name_0,varname_1,geometry)

# This is for our spatial analysis for tab2
coral_grid <- coral_raw %>%
  group_by(site, plot, genus, quadrat) %>%
  summarize((q_count=n()))


# Moorea LETR to find map shapefile
ggplot(data = c_sf) +
  geom_sf()



#user interface:
ui <- navbarPage("Moorea Corals", theme = shinytheme("superhero"),
                 tabPanel("Map of Moorea",
                          titlePanel("Map of Moorea"),
                          mainPanel(plotOutput("map"))

                 ),
                 tabPanel("Spatial Distribution of Coral Samples",
                          titlePanel("Spatial Distribution of Coral Samples"),
                          # leafletOutput("locations", width = "100%", height = "100%"),
                          mainPanel(plotOutput("grid"))

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
                          fluidRow(column(tags$img(src="poc.jpg",width="300px",height="200px"), width=5),
                                   br(),
                                   column(tags$img(src = "acr.jpg",width = "300px",height="200px"), width=5),
                                   br(),
                                   column(
                                     br(),
                                     br(),
                                     p("INFO ABOUT CORAL - La la la, here is some info about Moorea and coral!",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                     br(),
                                     br(),
                                     p("INFO ABOUT DATA - Woooo data citation!",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),

                                     width=8),
                          )
                 ))

#Server function:
server <- function(input, output) {
  coral_select <- reactive({
    coral_raw %>%
      filter(species == input$genus)
  })

  ################################# HELP - how to zoom in on only moorea
  # tab1 map
  output$map <- renderPlot({
    ggplot(data=fp)+
      geom_sf()+
      coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))+
      annotation_scale(
        location = "bl",
        width_hint = 0.2
      ) +
      geom_sf(data = c_sf, aes(color = genus))+
      coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))
  })

  # tab2 spatial analysis
   # output$locations <- renderLeaflet({
    # leaflet(data = coordinates) %>%
      # addTiles() %>%
      # addMarkers("long" =~LONGITUDE, "lat" =~LATITUDE) %>%
      # addProviderTiles(providers$Esri.WorldStreetMap)
  # })

  ################################ HELP - error: object object
  # grimes help for tab2
  makefiver <- reactive({

    data <- coral_grid %>%
      filter(site == input$site_select) %>%
      filter(genus = input$species_select) %>%
      filter(plot = input$plot_select)

    vec<-unique(data$quadrat)

    # Make an empty 25
    quad_vec<-rep(0,length.out = 25)

    # Sub in number of each counts
    quad_vec[vec]<-data$q_count=n()

    names(quad_vec)<-paste('X',1:25)

    # Make into a 5x5 grid
    quad_mat<-matrix(quad_vec,nrow=5,ncol=5)

    # Convert to tibble for grid
    quad_tibble <-
      quad_mat %>%
      as_tibble() %>%
      rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
      mutate(
        Var1 = factor(Var1, levels = 1:10),
        Var2 = factor(gsub("V", "", Var2), levels = 1:10)
      )

    return(quad_tibble)

  })

  # Make an output from the plots
  output$grid <- renderPlot({

    ggplot(makefiver(), aes(Var1, Var2)) +
      geom_tile(aes(fill = value)) +
      geom_text(aes(label = value)) +
      scale_fill_gradient(low = "white", high = "red")

  })


  # tab 3 table
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
  # tab 3 table
  output$coral_table <- renderTable({
    coral_table()
  })

  # tab 3 plot
  output$coral_plot <- renderPlot({
    ggplot(data = coral_raw, aes(x = length, y = width)) +
      geom_point(color = input$pt_color)
  })

}










#Combine above into an app:
shinyApp(ui = ui, server = server)

