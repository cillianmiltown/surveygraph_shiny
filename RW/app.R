
library(shiny)
library(leaflet)


pts <- data.frame(
  id = letters[seq(from = 1, to = 10)],
  x = rnorm(10, mean = -93.625),
  y = rnorm(10, mean = 42.0285),
  stringsAsFactors = F
)




# Define UI
ui <- fluidPage(uiOutput('Select'))

server <- function(input, output, session) {
  
  
  output$Select <- renderUI({
    Range <- sort(unique(pts$id))
    selectInput("dataselect",
                "select",
                choices = Range,
                selected = 'a')
  })
  
  
  mydata <- reactive({
    req(input$dataselect)
    if (input$dataselect != 'a') {
      data <- pts[pts$id == input$dataselect,]
    }
    else
    {
      data <- pts
    }
    
    
  })
  
  observe(print(mydata()))
  
  
}


shinyApp(ui = ui, server = server)