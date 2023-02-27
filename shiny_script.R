library(tidyverse)
library(shiny)
library(readxl)
library(here)
library(lubridate)
library(shinyWidgets)
library(bslib)
library(shinythemes)
library(janitor)
library(sf)
library(tmap)
library(dplyr)
library(RColorBrewer)

# Read in data?
coral_data <- read_excel(here("data", "coral_data.xls"))

# Create the user interface:
<<<<<<< HEAD
ui <- fluidPage(
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("put my widgets here",
      radioButtons(inputId = "genus",
                   label = "Choose coral species",
                   choices = c("Groovy coral" = "poc","Funky coral" = "acr"))
      ),
    mainPanel("put my graph here",
              plotOutput(outputId = "coral_plot")
    )
  )
)
=======
ui <- fluidPage(theme = shinytheme(""),
  navbarPage(
    tabPanel("Navbar 1",
             titlePanel(h2("Title of our project", align = "center")),
             titlePanel(h3("Created By: Jenna & Oooooolivia", align = "center")),
             br(),
             br(),
             br(),
        fixedRow(
          column(8,
        h1("Project Description", align = "center"),
        p("Insert description about what we doing here.",
          ),
        br(),
        p("More description here."),
        br(),
        p("Even more description here."),
        br(),
        p("So much describing.",
          ),
        br(),
        p("Maybe put meta data here."),
        br(),
        h2("Data Citation:", align = "center"),
        p("1. Citation"),
        br(),
        p("2. Citation"),
        br(),
        p("3. Citation"),
        ), # end column
        column(4,
              br(),
              br(),
              br(),
              br(),
              img(src = "", width = 0), # insert a picture here
        ), # end column
        ), # end fixedrow
    ), # end tabPanel
>>>>>>> 5293187e0cf5a154ba738be4f02a54c0e3b99447






server <- function(input, output) {}


shinyApp(ui = ui, server = server)




