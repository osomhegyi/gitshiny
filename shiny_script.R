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






server <- function(input, output) {}


shinyApp(ui = ui, server = server)




