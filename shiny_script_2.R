library(tidyverse)
library(shiny)
library(readxl)
library(here)
library(lubridate)
library(shinythemes)
library(basemaps)



# Read in data?
coral_raw <- read_excel(here("data", "coral_data.xls"))

#user interface:
ui <- fluidPage(theme = shinytheme("superhero"),
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
                             choices = c("Purple" = "purple", "ORAAANGE" = "orange"))
                              ),
    mainPanel("put my graph here",
              plotOutput(outputId = "coral_plot"),
              tableOutput(outputId = "coral_table"))
  )
)

#Server function:
server <- function(input, output) {
  coral_select <- reactive({
    coral_data %>%
      filter(species == input$genus)
  })

  coral_table <- reactive({
    coral_raw %>%
      filter(genus == input$genus) %>%
      group_by(site) %>%
      summarize(
        mean_length = mean(length),
        mean_width = mean(width)
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
}




#sliderInput(inputId = "perc_bleach",
            #label = "Percent Bleached", min = 0, max = 100, value =50, step = NULL,
            #round = FALSE, format = "#,##0.#####",
            #locale = "us", ticks = TRUE, animate = FALSE)
#output$coral_slider <- renderPrint ({ input$perc_bleach})
#textOutput('perc_bleach')


navbarPage("Map of Moorea",
           tabPanel("Project Overview",
                    titlePanel(h2("Map of Moorea!", align = "center")),
                    titlePanel(h3("Created By: Jenna + Ooooooolivia", align = "center")),
                    br(),
                    br(),
                    br(),
                    fixedRow(
                      column(8,
                             h1("Project Description", align = "center"),
                             p("Data was collected by the other Olivia something something description.",
                             ),
                             br(),
                             p("More text here about Moorea."),
                             br(),
                             p("More text here about corals."),
                             br(),
                             p("More text here about how data was collected.",
                             ),
                             br(),
                             p("The purpose of this Shiny App is to.... MAKE CORALS FIGHT!"),
                             br(),
                             h2("Data Citation:", align = "center"),
                             p("1. Cite data"),
                             br(),
                             p("2. Cite data"),
                             br(),
                             p("3. Cite data"),
                      ), # end column
                      column(4,
                             br(),
                             br(),
                             br(),
                             br(),
                             img(src = "insertimage.jpg", width = 450),
                      ), # end column
                    ), # end fixedrow
           ), # end tabPanel
) #end navbar


#Combine above into an app:
shinyApp(ui = ui, server = server)

