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
library(plotly)
library(DT)


# Read in data
coral_raw <- read_excel(here("data", "coral_data.xls")) %>%
  mutate(area = length*width) %>%
  filter(genus != "poc?") %>%
  filter(genus != "acr?") %>%
  filter(genus != "unknown")

# Making genus column all lowercase (but there is still a ? in one of the cells - need to fix)
coral_raw$genus <- tolower(coral_raw$genus)


# Wrangle the coordinates for tab1
coordinates <- coral_raw %>%
  select('lat', 'long', 'genus', 'site') %>%
  group_by(site)

class(coordinates$genus) #character

coordinates['coral_count'] <- 1

# This is for our map in tab1
c_sf <- coordinates %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

c_sf <- c_sf %>%
  dplyr::summarise(total_corals_found = sum(coral_count))


# This is our base map for tab1
fp <- read_sf(here::here("data","moorea.shp")) %>%
  filter(hasc_1=="PF.WI") %>%
  select(name_0,varname_1,geometry)

# This is for our spatial analysis for tab2
coral_grid <- coral_raw %>%
  group_by(site, plot, genus, quadrat) %>%
  summarize(q_count=n())


# Moorea LETR to find map shapefile
ggplot(data = c_sf) +
  geom_sf()


###color_list <- list(poc = c('white', '#69b3a2'),
###                   acr = c('white', 'red'))


###add background: bg = 'antiquewhite'

#user interface:
ui <- navbarPage("Moorea Corals", theme = shinytheme("readable"),
                 tabPanel("Map of Moorea",
                          titlePanel("Moorea, French Polynesia"),
                          mainPanel(fluidRow(
                            column(
                              em("This map respresents 12 sites in the Northshore Lagoon of
                                      the Moorea Coral Reef Long-term Ecological Research (MCR LTER) site"),
                              width = 12),
                            br(),
                            br(),
                            plotlyOutput("map", height=650, width=650)
                          )),


                 ),
                 tabPanel("Spatial Distribution of Coral Samples",
                          titlePanel("Spatial Distribution of Coral Samples"),
                          # leafletOutput("locations", width = "100%", height = "100%"),
                          sidebarLayout(
                            sidebarPanel("Select Variable:",
                                         radioButtons(inputId = "genus_select",
                                                      label = h3("Species"),
                                                      choices = c("Pocillopora" = "poc","Acropora" = "acr")),
                                         selectInput(inputId = "site_select",
                                                     label = h3("Site Number"),
                                                     choices = unique(coral_grid$site),
                                                     selected = 1),
                                         selectInput(inputId = "plot_select",
                                                     label = h3("Plot Number"),
                                                     choices = unique(coral_grid$plot),
                                                     selected = 1)
                            ),
                            mainPanel(plotOutput("grid")
                            ),
                          ),
                 ),
                 tabPanel("Coral Plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "genus",
                                           label = "Select Coral Species",
                                           choices = c("Pocillopora" = "poc","Acropora" = "acr")
                              ),
                              hr(),
                              fluidRow(column(3, verbatimTextOutput("value"))
                              )##,
                              ##selectInput(inputId = "pt_color",
                              ##label = "Select point color",
                              ##choices = c("Purple Coral" = "purple", "Orange Coral" = "orange"))
                            ),
                            mainPanel("Coral Details",
                                      ##plotOutput(outputId = "coral_plot"),
                                      DT::dataTableOutput(outputId = "coral_table"))
                          )),

                 tabPanel("Info & Data Sources",
                          titlePanel("About"),
                          fluidRow(
                            tags$img(src="divers.jpg",width="800px",height="500px"),
                            width = 12,
                            align = "left"),
                          em("Researchers at the Moorea Coral Reef Long-term Ecological Research (LTER) site", align = "center"),
                          column(
                            br(),
                            br(),
                            p("Coral reefs are among the most diverse and productive ecosystems on Earth.
                              The Moorea Coral Reef Long-term Ecological Research (MCR LTER) site was established
                              by the National Science Foundation in 2004 as a model system to better understand
                              factors that mediate coral community structure and function.This app seeks to visualize
                              data collected in the Moorea Northshore Lagoon by LTER researcher and PhD student
                              Olivia Isbell from 7/1/22 - 8/26/22 to help researchers understand if there are any
                              spatial patterns surveyed or corals Acropora and Pocilopora, either random or clustered."),
                            width = 12,
                            align = "left"
                          )
                 ))


#Server function:
server <- function(input, output) {
  coral_select <- reactive({
    coral_raw %>%
      filter(species == input$genus)
  })

  # tab1 map
  output$map <- renderPlotly({
    ggplot(data=fp)+
      geom_sf()+
      #theme(panel.background = element_rect(fill = 'lightblue', color = 'purple'))+ #this is if we want to change background color
      theme_minimal() +
      coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))+
      annotation_scale(
        location = "bl",
        width_hint = 0.2
      ) +
      geom_sf(data = c_sf, color = "#69b3a2", size = 1.3, aes(labels=total_corals_found))+
      theme(legend.position = "none")+
      coord_sf(xlim=c(-149.70,-149.95),ylim=c(-17.42,-17.62))
  })

  ### Paste0("Site Number", site, "\n", "Number of Corals:", total_corals_found)




  # tab2 spatial analysis
  # output$locations <- renderLeaflet({
  # leaflet(data = coordinates) %>%
  # addTiles() %>%
  # addMarkers("long" =~LONGITUDE, "lat" =~LATITUDE) %>%
  # addProviderTiles(providers$Esri.WorldStreetMap)
  # })



  ###test colors

  ##species_color <- reactive({
  ##  color_list %>%
  ##    pluck(input$genus_select)
  ##})

  ###stop test colors


  # grimes help for tab2
  makefiver <- reactive({

    data <- coral_grid %>%
      filter(site == input$site_select) %>%
      filter(genus == input$genus_select) %>%
      filter(plot == input$plot_select)

    vec<-unique(data$quadrat)

    # Make an empty 25
    quad_vec<-rep(0,length.out = 25)

    # Sub in number of each counts
    quad_vec[vec]<-data$q_count

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
      #scale_fill_gradientn(colors = species_color())
      scale_fill_gradient(low = "white", high = "#69b3a2") +
      theme_void()

  })


  # tab 3 table
  coral_table <- reactive({
    coral_raw %>%
      filter(genus == input$genus) %>%
      group_by(site) %>%
      summarize(
        mean_length = mean(length),
        mean_width = mean(width),
        mean_area = mean(area),
        mean_perc_dead = (mean(perc_dead)/100),
        mean_perc_bleached = (mean(perc_bleach)/100)
      )
  })
  # tab 3 table
  output$coral_table <- DT::renderDataTable({
    datatable(coral_table()) %>%
      formatPercentage(c("mean_perc_dead", "mean_perc_bleached"), 2) %>%
      formatRound(c(2:4), digits = 2)
  })

  ### 3/8 Notes: Need to round to 2 decimals, need to adjust page layout so that "select species" box is smaller.

  # tab 3 plot
  output$coral_plot <- renderPlot({
    ggplot(data = coral_raw, aes(x = length, y = width)) +
      geom_point(color = input$pt_color)
  })

}










#Combine above into an app:
shinyApp(ui = ui, server = server)
