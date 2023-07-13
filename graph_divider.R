rm(list=ls())
# Required library
library(igraph)

## Functions for trimming graph ##

importance_vals <- function(g, importance_measure) {
  if (importance_measure == "Degree in") { vals <- V(g)$degree_in }
  if (importance_measure == "Degree out") { vals <- V(g)$degree_out }
  if (importance_measure == "Degree total") { vals <- V(g)$degree_total }
  if (importance_measure == "Betweenness") { vals <- V(g)$betweenness }
  if (importance_measure == "Closeness") { vals <- V(g)$closeness }
  if (importance_measure == "Eigencentrality") { vals <- V(g)$eigencentrality }
  return(vals)
}

get_g2_nodes <- function(g, importance_measure, importance_cutoff_value) {
  importance_values <- importance_vals(g, importance_measure)
  df <- cbind.data.frame(name = V(g)$name, val = importance_values)
  df2 <- df[df$val >= importance_cutoff_value, ]  
  key_nodes <- df2$name
  return(key_nodes)
}

get_g2_edges <- function(g, key_nodes, g2_max_links) {
  el <- NULL
  for (from_name in key_nodes) {
    from_node <- V(g)[which(V(g)$name == from_name)]
    for (to_name in key_nodes) {
      to_node <- V(g)[which(V(g)$name == to_name)]
      res <- all_simple_paths(g, from = from_node, to = to_node, mode = "out", cutoff = g2_max_links)  
      no_paths <- length(res) == 0 || length(res[[1]]) == 0
      if (!no_paths) {
#        pathways <- c()
        for (i in 1:length(res)) {
          path_dist <- length(res[[i]]) - 1
          el <- rbind.data.frame(el, cbind.data.frame(from=from_name, to=to_name, path_dist=path_dist, weight = 1/path_dist))
          
#          pathways <- c(pathways, length(res[[i]]))
#          weight <- sum(sapply(pathways, function(x) path_weightings$weighting[path_weightings$num_paths == x]))
        }
#        el <- rbind.data.frame(el, cbind.data.frame(from=from_name, to=to_name, weight=weight))
      }
    }
  }
  return(el)
}

make_g2 <- function(g, importance_measure, importance_cutoff_value, g2_max_links, must_have_node_names) {
  key_nodes <- get_g2_nodes(g, importance_measure, importance_cutoff_value)
  key_nodes <- unique(c(key_nodes, must_have_node_names))
  
  if (length(key_nodes) == 0) { return(NULL) }
  edges <- get_g2_edges(g, key_nodes, g2_max_links)
  g2 <- graph_from_data_frame(edges, directed = TRUE)
  
  for (v in V(g2)) {
    # Calculate a measure for how many ways a node is infuenced by different pathways
    incident_in_edges <- incident_edges(g2, V(g2)[v], mode = "in")
    path_dists <- E(g2)$path_dist[unlist(incident_in_edges)]
    V(g2)$in_influence[v] <- sum(1/path_dists)
    
    # Calculate a measure for how many ways a node influences others
    incident_out_edges <- incident_edges(g2, v, mode = "out")
    path_dists <- E(g2)$path_dist[unlist(incident_out_edges)]
    V(g2)$out_influence[v] <- sum(1/path_dists)
  }
  
  return(g2)
}

cut_edges_by_number_of_paths <- function(g2, display_pathways_cutoff) {
  # Identify edges with pathways > display_pathways_cutoff
  edges_to_remove <- E(g2)[E(g2)$path_dist > display_pathways_cutoff]
  # Remove these edges from the graph
  g3 <- delete_edges(g2, edges_to_remove)
  return(g3)
}

plot_simplified_graph <- function(g, importance_measure, 
                                  importance_cutoff_value, g2_max_links, 
                                  display_pathways_cutoff, must_have_node_names) {
  g2 <- make_g2(g, importance_measure, importance_cutoff_value, g2_max_links, must_have_node_names)
  if (is.null(g2)) { 
    print("No graph made")
  } else {
    g3 <- cut_edges_by_number_of_paths(g2, display_pathways_cutoff)
    plot(g3, 
         #edge.label = round(E(g3)$weight,2), 
         vertex.size = V(g3)$out_influence*2, 
         vertex.frame.width = V(g3)$in_influence*2,
         vertex.frame.color = "black", vertex.shape = "circle",
         edge.width = E(g3)$weight*3, 
         vertex.label = V(g3)$name, edge.arrow.size = 0.5)
  }
}


# Define nodes

edges <- c("ClimateChange", "Migration", 
           "Urbanization", "EconomicInequality",
           "AI", "EconomicInequality", 
           "Automation", "EconomicInequality",
           "5GTechnology", "Cybersecurity", 
           "AgingPopulation", "AI",
           "Pandemic", "RemoteWork", 
           "PoliticalPolarization", "EconomicInequality",
           "EconomicInequality", "PoliticalPolarization",
           "SpaceExploration", "AI", 
           "PoliticalPolarization", "Globalization",
           "AgingPopulation", "GeneEditing", 
           "Globalization", "Urbanization", 
           "Globalization", "Migration",
           "AgingPopulation", "Migration",
           "Migration","PoliticalPolarization",
           "ClimateChange", "EconomicInequality",
           "ClimateChange", "PoliticalPolarization",
           "RemoteWork","Migration",
           "Migration","5GTechnology",
           "AI","Cybersecurity",
           "AI", "Automation",
           "Globalization", "ClimateChange",
           "AI", "RemoteWork",
           "GeneEditing", "Pandemic",
           "AI", "GeneEditing",
           "5GTechnology", "RemoteWork"
)

# Create a directed graph
g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)

# Calculate measures of importance
V(g)$degree_in <- degree(g, mode = "in")
V(g)$degree_out <- degree(g, mode = "out")
V(g)$degree_total <- degree(g, mode = "all")
V(g)$betweenness <- betweenness(g, directed = TRUE)
V(g)$closeness <- closeness(g, mode = "all")

importance_measure <- "Betweenness"
importance_values <- importance_vals(g, importance_measure)
importance_cutoff_slider_range <- c(min(importance_values), max(importance_values))
importance_cutoff_value <- 1#mean(importance_values)

plot(g)
plot_simplified_graph(g, 
                      importance_measure = "Betweenness", 
                      importance_cutoff_value = 20, 
                      g2_max_links = 10, 
                      display_pathways_cutoff = 2,
                      must_have_node_names = c("Cybersecurity"))
