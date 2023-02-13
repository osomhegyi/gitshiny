library(tidyverse)
library(shiny)
library(palmerpenguins)

# Creating the user interface
ui <- fluidPage(
  titlePanel("I am adding a title!"), # This is the title!
  sidebarLayout( # Adding a sidebar & main panel
    sidebarPanel("put my widgets here",
                 radioButtons(inputId = "penguin_species", label = "Choose penguin species", choices = c("Adelie","Gentoo","Cool Chinstrap Penguins!" = "Chinstrap"), # This is my first widget for penguins species
                 ),
                 selectInput(inputId = "pt_color", label = "Select point color", choices = c("Awesome red!" = "red", "Pretty purple" = "purple", "ORAAANGE" = "orange"))
    ),
    mainPanel("put my graph here", # Adding things to the main panel
              plotOutput(outputId = "penguin_plot")
    )
  )
)

# Building the server:
server <- function(input, output) {

  penguin_select <- reactive({
    penguins %>%
      filter(species == input$penguin_species)
  })

  # Create a reactive plot, which depends on 'species' widget selection:

  output$penguin_plot <- renderPlot({

    ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(color = input$pt_color)

  })

}
shinyApp(ui = ui, server = server)





