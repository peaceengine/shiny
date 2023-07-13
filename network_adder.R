# Required libraries
library(shiny)
library(igraph)

# Define UI
ui <- fluidPage(
  titlePanel("Network Node and Edge Adder"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_node", "Add Node"),
      actionButton("add_edge_modal", "Add Edge"),
      actionButton("create_edge_list", "Create Edge List")
    ),
    mainPanel(
      plotOutput("networkPlot"),
      tableOutput("edgeListTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Our reactive graph
  rv <- reactiveValues(graph = g)
  
  # Add node modal
  observeEvent(input$add_node, {
    showModal(modalDialog(
      title = "Add a Node",
      textInput("node_name", "Name of the Node"),
      radioButtons("node_time", "Select Time", choices = c('A', 'B', 'C')),
      radioButtons("node_type", "Select Type", choices = c('Trend', 'Event')),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_node", "Add Node")
      )
    ))
  })
  
  observeEvent(input$save_node, {
    newNode <- input$node_name
    rv$graph <- add_vertices(rv$graph, 1, name = newNode, time = input$node_time, type = input$node_type)
    updateSelectInput(session, "from_node", choices = V(rv$graph)$name)
    updateSelectInput(session, "to_node", choices = V(rv$graph)$name)
    removeModal()
  })
  
  # Add edge modal
  observeEvent(input$add_edge_modal, {
    showModal(modalDialog(
      title = "Add an Edge",
      selectInput("from_node_modal", "Select From Node:", choices = V(rv$graph)$name),
      selectInput("to_node_modal", "Select To Node:", choices = V(rv$graph)$name),
      numericInput("edge_weight", "Weight:", value = 1, min = 0),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edge", "Add Edge")
      )
    ))
  })
  
  observeEvent(input$save_edge, {
    rv$graph <- add_edges(rv$graph, c(input$from_node_modal, input$to_node_modal))
    rv$graph <- set_edge_attr(rv$graph, "weight", index = E(rv$graph)[length(E(rv$graph))], value = input$edge_weight)
    removeModal()
  })
  
  # Create edge list
  observeEvent(input$create_edge_list, {
    edge_list <- get.data.frame(rv$graph, what = "edges")
    output$edgeListTable <- renderTable(edge_list)
  })
  
  output$networkPlot <- renderPlot({
    plot.igraph(rv$graph, edge.arrow.size=.5, vertex.label.dist=1, vertex.size=5, vertex.frame.color="#777777",
                vertex.label.color="#777777", vertex.label.cex=0.75, vertex.label.family="sans")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
