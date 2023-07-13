# Load required libraries
library(shiny)

# Define UI for application
ui <- fluidPage(
  # Create dropdown menu for selecting burger ingredients
  selectizeInput("ingredients", "Choose your ingredients", 
                 choices = c("Ketchup", "Gherkins", "Cheese"),
                 multiple = TRUE),
  
  # Create a placeholder for UI elements
  uiOutput("ui_elements"),
  
  # Show selected ingredients
  verbatimTextOutput("selected_items")
)

# Define server logic required
server <- function(input, output, session) {
  # Create reactive variable for selected ingredients
  selected_ingredients <- reactive({
    input$ingredients
  })
  
  # Create UI elements for removing ingredients
  output$ui_elements <- renderUI({
    if (!is.null(selected_ingredients())) {
      tagList(lapply(selected_ingredients(), function(ingredient) {
        div(style = "display: inline-block; padding: 1px; margin: 1px",
            wellPanel(
              style = "padding: 5px; margin: 0px;", 
              ingredient,
              actionButton(paste0("remove_", gsub(" ", "", ingredient)), "X")
            )
        )
      }))
    }
  })
  
  # Show selected ingredients
  output$selected_items <- renderPrint({
    selected_ingredients()
  })
  
  # Observe for any remove action
  observe({
    if (!is.null(selected_ingredients())) {
      lapply(selected_ingredients(), function(ingredient) {
        observeEvent(input[[paste0("remove_", gsub(" ", "", ingredient))]], {
          updateSelectizeInput(session, "ingredients", 
                               selected = setdiff(selected_ingredients(), ingredient))
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
