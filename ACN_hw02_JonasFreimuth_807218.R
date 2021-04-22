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

getNeighborsUndirected <- function(graph, node) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  row <- adj_mat[node,]
  
  if (is_named(graph)) {
    nb <- names(row[row != 0])
    
  } else {
    nb <- which(row != 0)
  }
  
  return(nb)
  
}




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
  
  if (all(gNU_neighbor == igraph_neighbor)) {
    print("Task 2 works.")
  }
  
  
}
