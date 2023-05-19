ui <- shinyUI(
  fluidPage(
    div(style="display: inline-block;vertical-align:top; width: 70px;height:10px",
        shiny::textInput('pricelow', 'Min price', value = 0)),
    div(style="display: inline-block;vertical-align:top; width: 70px;",
        shiny::textInput('pricehigh', 'Max price', value = 30)),
    
    ### Time Series Controls
    fluidRow(column(4, textInput('num_ts','No of TS', value = 2)),
             column(4, shiny::textInput('ts_pricelow', 'Min price', value = 0)),
             column(4, shiny::textInput('ts_pricehigh', 'Max price', value = 30))),
    fluidRow(column(3, 
                    column(3, actionButton("addInput","Add Input", style ='font-size:80%; padding:4px')),
                    column(3, actionButton("removeInput","Remove Input", style ='font-size:80%; padding:4px'))),
             
             #column(2, actionButton("resetInput","Reset Input", style ='font-size:80%; padding:4px')),
             column(3, checkboxInput(inputId = 'plotpoints', "MapView", value = FALSE)),
             column(3, checkboxInput(inputId = 'ts_facet', "Facet", value = FALSE)),
             column(3, checkboxInput(inputId = 'facet_free', "Y Free", value = FALSE))),
    
    
    
    uiOutput("inputs")
  )
)

server <- function(input, output, session){
  ids <- reactive({
    if (input$addInput == 0) return(NULL)
    
    if (input$addInput == 1){
      output <- 1
    }else{
      if(input$addInput > input$removeInput) {
        output <- 1:(input$addInput-input$removeInput)
      } else return(NULL)
      
    }
    return(output)
  })
  
  output$inputs <- renderUI({
    if (is.null(ids())) return(NULL)
    tagList(
      lapply(1:length(ids()),function(i){
        check_input_i <- paste0("cell_i_", ids()[i])
        check_input_j <- paste0("cell_j_", ids()[i])
        check_input_k <- paste0("cell_k_", ids()[i])
        if(is.null(input[[check_input_i]])){
          # Create a div that contains 3 new sub divs
          div(
            # Display option to provide I of Cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::textInput(paste0('cell_i_',ids()[i]), 'I index', value = 10)),
            # Option to provide J of cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::textInput(paste0("cell_j_",ids()[i]), 'J index', value = 34)),
            # Option to provide K of cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::selectInput(paste0("cell_k_",ids()[i]), 'Layer', c(1:10)))
          )
        } else {
          # Create a div that contains 3 existing sub divs
          div(
            # Display option to provide I of Cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::textInput(paste0('cell_i_',ids()[i]), 'I index', value = input[[check_input_i]])),
            # Option to provide J of cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::textInput(paste0("cell_j_",ids()[i]), 'J index', value = input[[check_input_j]])),
            # Option to provide K of cell
            div(style="display: inline-block;vertical-align:top; width: 80px",
                shiny::selectInput(paste0("cell_k_",ids()[i]), 'Layer', c(1:10), selected = input[[check_input_k]]))
          )
        }
        
      })
    )
  })
}

shinyApp(ui = ui, server = server)