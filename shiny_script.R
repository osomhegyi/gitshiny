library(tidyverse)
library(shiny)
library(readxl)
library(here)
library(lubridate)

# Read in data?
coral_data <- read_excel(here("data", "coral_data.xls"))

# Create the user interface:
ui <- fluidPage(
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("put my widgets here"),
    radioButtons(inputId = "genus", label = "Choose coral species", choices = c("Groovy coral" = "poc","Funky coral" = "acr"))
    mainPanel("put my graph here")
  )
)


# Create the server function:
server <- function(input, output) {}

# Combine them into an app:
shinyApp(ui = ui, server = server)

