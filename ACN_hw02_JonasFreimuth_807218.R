# ACN homework


# Task 1 ------------------------------------------------------------------

getDegreesUndirected <- function(graph) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  degrees <- colSums(adj_mat)
  
  return(degrees)
  
}


# Task 2 ------------------------------------------------------------------

getNeighborsAdjacencySimple <- function (node, adj_mat, named = FALSE) {
  row <- adj_mat[node, ]
  
  if (named) {
    nb <- names(row[row != 0])
    
  } else {
    nb <- which(row != 0)
  }
  
  return(nb)
  
}

getNeighborsUndirected <- function(graph, node) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  nb <- getNeighborsAdjacencySimple(node, adj_mat)
  
  return(nb)
  
}


# Task 3 ------------------------------------------------------------------

getNthNeighborsUndirected <- function(graph, v, order, simplify = TRUE) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  nodes <- list()
  
  # initialize vector of already found neighbors with 0
  # empty vector causes problems at initial comparing
  anc_nds <- c(0)
  
  for (i in 1:order) {
    if (i == 1) {
      # handle case of i == 1, use original node
      prev_nodes <- v
      
    } else {
      # every other case: the previous nodes are in the previous list entry
      prev_nodes <- nodes[[i - 1]]
    }
    
    # get list of each of the previous neighbors neighbors
    # puts a list at pos i in the nodes list
    nodes[[i]] <- lapply(prev_nodes, getNeighborsAdjacencySimple, adj_mat)
    
    # convert output to a vector and filter out duplicates and nodes 
    # previously visited
    nodes[[i]] <- unique(unlist(nodes[[i]]))
    nodes[[i]] <- nodes[[i]][!(nodes[[i]] %in% anc_nds)]
    
    # update list of visited nodes
    anc_nds <- unlist(nodes)
  }
  
  # if requested, turn output into a vector
  if (simplify) {
    nodes <- unlist(nodes)
  }
  
  return(nodes)
  
}


# Task 4 ------------------------------------------------------------------




# Tests -------------------------------------------------------------------

# check if script is executed directly
if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, max = 100), runif(1))
  
  plot(rnd_graph)
  
  # task 1
  
  if (all(getDegreesUndirected(rnd_graph) == degree(rnd_graph))) {
    print("Task 1 works.")
  }
  
  # task 2
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, max = length(V(rnd_graph)))])
  
  gNU_neighbor <- getNeighborsUndirected(rnd_graph, rnd_vert)
  igraph_neighbor <- neighbors(rnd_graph, rnd_vert)
  
  if (all(gNU_neighbor %in% igraph_neighbor)) {
    print("Task 2 works.")
  }
  
  # task 3
  
  gNNU_neighbors <- getNthNeighborsUndirected(rnd_graph, rnd_vert, 2)
  igraph_nth_neighbor <- unlist(ego(rnd_graph, nodes = rnd_vert, order = 2))
  
  if (all(gNNU_neighbors %in% igraph_nth_neighbor)) {
    print("Task 3 works.")
  }
  
}
