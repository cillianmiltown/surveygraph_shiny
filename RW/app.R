library(shiny)
library(purrr)

## Text Module ----

textUI <- function(id) {
  div(
    id = NS(id, "ui"),
    actionButton(NS(id, "remove"), "Remove UI"),
    textInput(NS(id, "text"), "This is no longer useful", "plop")
  )
}

textServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      text = reactive(input$text),
      delete = reactive(input$remove)
    )
  })
}


## Main App ----

# utility to hide away the mess of updating the reactiveVal(list())
update_values <- function(values, name, value) {
  vals <- values()
  vals[[name]] <- value
  values(vals)
}

add_module <- function(values, name, server, delete_hook = NULL, remove_selector = NULL) {
  # add module server's return to values list
  update_values(values, name, server)
  
  # if module has a reactive we should monitor for deleting, do so
  if (!is.null(delete_hook)) {
    observeEvent(
      server[[delete_hook]](), {
        removeUI(selector = remove_selector)  # remove the ui
        update_values(values, name, NULL)  # remove the server from our values list
      },
      once = TRUE
    )
  }
}

ui <- fluidPage(
  actionButton("addModule", "Add Module"),
  verbatimTextOutput("wordsOutput")
)

server <- function(input, output, session){
  # can't use reactiveValues as can't remove items from them
  module_values <- reactiveVal(list())
  
  observeEvent(input$addModule, {
    id <- paste("module", input$addModule, sep = "-")
    insertUI(selector = "#addModule", where = "afterEnd", ui = textUI(id))
    add_module(
      module_values,
      name = id,
      server = textServer(id),
      delete_hook = "delete",
      remove_selector = paste0("#", NS(id, "ui"))
    )
  })
  
  words <- reactive({ 
    map(module_values(), ~.x$text())
  })
  
  output$wordsOutput <- renderPrint(words())
}

shinyApp(ui, server)