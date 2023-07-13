# Required libraries
library(shiny)
library(igraph)

# Define UI
ui <- fluidPage(
  titlePanel("Network Analyzer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("importance_measure", "Choose Importance Measure:", 
                  choices = c("degree", "betweenness", "in-degree", "out-degree")),
      sliderInput("cutoff", "Cutoff Value:", min = 0, max = 1, value = 0.5),
      sliderInput("k_value", "Maximum Path Length:", min = 1, max = 10, value = 2),
      actionButton("switch_view", "Switch View")
    ),
    mainPanel(
      textOutput("g2_nodes_count"),
      tableOutput("edge_table"),
      plotOutput("networkPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Our reactive graph
  g <- make_ring(10)  # Dummy graph for demo
  rv <- reactiveValues(graph = g, graph2 = NULL, view = "g")
  
  range_values <- reactiveValues(low = 0, high = 1)
  
  observeEvent(input$importance_measure, {
    mr <- get_measure_range(rv$graph, input$importance_measure)
    range_values$low <- mr[1]  
    range_values$high <- mr[2]  
    print(paste("Range values between ", range_values$low, " and ", range_values$high))
    
    output$slider_ui <- renderUI({
      sliderInput("cutoff", "Cutoff Value:", min = range_values$low, max = range_values$high, value = range_values$low)
    })
    
  })
  
  # Dynamically adjust slider range based on the selected measure
  
  # Recalculate graph2 and count of nodes reactively
  observeEvent(c(input$importance_measure, input$cutoff), {
    rv$graph2 <- create_simplified_graph(rv$graph, input$importance_measure, input$cutoff)
    
    output$g2_nodes_count <- renderText({
      if (is.null(rv$graph2)) {
        return("g2 is not yet created.")
      }
      paste("Number of nodes in g2:", length(V(rv$graph2)))
    })
  }, ignoreNULL = FALSE)
  
  # Switch view
  observeEvent(input$switch_view, {
    rv$view <- ifelse(rv$view == "g", "g2", "g")
  })
  
  output$networkPlot <- renderPlot({
    if (rv$view == "g") {
      plot.igraph(rv$graph)
    } else if (rv$view == "g2" & !is.null(rv$graph2)) {
      plot.igraph(rv$graph2)
    }
  })
  
#  output$edge_table <- renderTable({
#    if (!is.null(rv$graph2)) {
#      create_edge_table(rv$graph2, input$k_value)
#    }
#  }, rownames = TRUE)
}

# Dummy function to create simplified graph
create_simplified_graph <- function(g, importance_measure, cutoff) {
  v_delete <- V(g)[runif(length(V(g))) < cutoff]
  g2 <- delete.vertices(g, v_delete)
  return(g2)
}

# Function to get range of a given measure
get_measure_range <- function(g, measure) {
  if (measure == "degree") {
    degree_values <- degree(g)
    return(c(min(degree_values), max(degree_values)))
  }
  if (measure == "betweeness") {
    betweeness_values <- betweenness(g)
  }
  # Implement similar blocks for "betweenness", "in-degree", and "out-degree"
}

# Function to create edge table
create_edge_table <- function(g, k) {
  paths <- all_simple_paths(g, mode = "out")
  paths <- paths[lengths(paths) <= k+1]
  
  edge_table <- sapply(paths, function(p) {
    from <- V(g)[p[1]]$name
    to <- V(g)[p[length(p)]]$name
    steps <- length(p) - 1
    c(from, to, steps)
  })
  edge_table <- t(edge_table)
  colnames(edge_table) <- c("From", "To", "Steps")
  edge_table
}

# Run the application 
shinyApp(ui = ui, server = server)
