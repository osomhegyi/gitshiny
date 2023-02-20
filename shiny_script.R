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
    sidebarPanel("put my widgets here",
      radioButtons(inputId = "genus",
                   label = "Choose coral species",
                   choices = c("Groovy coral" = "poc","Funky coral" = "acr"))
      ),
    mainPanel("put my graph here"),
    plotOutput(outputId = "coral_plot")
  )
)


# Create the server function:
coral_select <- reactive({
  coral_data %>%
    filter(genus == input$genus)
})

output$coral_plot <- renderPlot({

  ggplot(data = coral_select(), aes(x = length, y = width, color = genus)) +
    geom_point()

})


# Combine them into an app:
shinyApp(ui = ui, server = server)

