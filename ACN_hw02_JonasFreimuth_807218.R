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

getNeighborsAdjacencySimple <- function (v, adj_mat, named = FALSE) {
  row <- adj_mat[v, ]
  
  if (named) {
    nb <- names(row[row != 0])
    
  } else {
    nb <- which(row != 0)
  }
  
  return(nb)
  
}

getNeighborsUndirected <- function(graph, v) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  nb <- getNeighborsAdjacencySimple(v, adj_mat, named = is_named(graph))
  
  return(nb)
  
}


# Task 3 ------------------------------------------------------------------

getNthNeighborsUndirected <- function(graph, v, order = 2, simplify = TRUE,
                                      full = FALSE, closed = TRUE) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  if (order < 1) {
    stop("0 or negative orders not permitted.")
  }
  
  adj_mat <- as.matrix(graph[])
  
  vrtcs <- list()
  
  # if the closed neighborhood is requested, add target vertex to ancestral
  # nodes, else add 0 so it is not NULL
  if (closed) { 
    anc_nds <- v
  } else {
    anc_nds <- c(0)
  }
  
  for (i in 1:order) {
    if (i == 1) {
      # handle case of i == 1, use original node
      prev_vrtcs <- v
      
    } else {
      # every other case: the previous vrtcs are in the previous list entry
      prev_vrtcs <- vrtcs[[i - 1]]
    }
    
    # get list of each of the previous neighbors neighbors
    # puts a list at pos i in the vrtcs list
    vrtcs[[i]] <- lapply(prev_vrtcs, getNeighborsAdjacencySimple, adj_mat,
                         named = is_named(graph))
    
    # convert output to a vector and filter out duplicates and vrtcs 
    # previously visited
    vrtcs[[i]] <- unique(unlist(vrtcs[[i]]))
    vrtcs[[i]] <- vrtcs[[i]][!(vrtcs[[i]] %in% anc_nds)]
    
    # update list of visited vrtcs
    anc_nds <- unlist(vrtcs)
    
    # if closed neighborhood is requested, also ensure target node is counted
    # as an ancestral node
    if (closed) {
      anc_nds <- c(anc_nds, v)
    }
  }
  
  # if requested, only return neighbors of the specified order instead of 
  # the entire neighborhood
  if (!full) {
    vrtcs <- vrtcs[[order]]
  }
  
  # if requested, turn output into a vector
  if (simplify) {
    vrtcs <- unlist(vrtcs)
  }
  
  return(vrtcs)
  
}


# Task 4 ------------------------------------------------------------------

getNodesHighestSimilarity <- function(graph) {
  
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_directed(graph)) {
    stop("This function can only deal with undirected graphs.")
  }
  
  adj_mat <- graph[]
  
  vrtcs <- as.vector(V(graph))
  
  pairs <- combn(vrtcs, 2, simplify = FALSE)
  
  nbs <- lapply(vrtcs, getNeighborsAdjacencySimple, adj_mat = adj_mat,
                named = is_named(graph))
  
  smlrty <- sapply(pairs, function(pair, nbs) {
    
    nbs_1 <- nbs[[pair[1]]]
    nbs_2 <- nbs[[pair[2]]]
    
    n_union <- length(union(nbs_1, nbs_2))
    
    n_shared <- sum(nbs_1 %in% nbs_2)
    
    smlrty <- n_shared / n_union
    
    return(smlrty)
    
  }, nbs = nbs)
  
  max_sim_idx <- which(smlrty == max(smlrty))
  
  return(pairs[[max_sim_idx]])
  
}


# Tests -------------------------------------------------------------------

# check if script is executed directly
if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, min = 1, max = 100), runif(1))
  
  plot(rnd_graph)
  
  # task 1
  
  if (all(getDegreesUndirected(rnd_graph) == degree(rnd_graph))) {
    print("Task 1 works.")
  }
  
  # task 2
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                            max = length(V(rnd_graph)))])
  
  gNU_neighbor <- getNeighborsUndirected(rnd_graph, rnd_vert)
  igraph_neighbor <- neighbors(rnd_graph, rnd_vert)
  
  if (all(gNU_neighbor %in% igraph_neighbor)) {
    print("Task 2 works.")
  }
  
  # task 3
  
  gNNU_neighbors <- getNthNeighborsUndirected(rnd_graph,
                                              rnd_vert,
                                              order = 2,
                                              full = FALSE)
  
  order_2 <- unlist(ego(rnd_graph, nodes = rnd_vert, order = 2))
  order_1 <- unlist(ego(rnd_graph, nodes = rnd_vert, order = 1))
  
  igraph_nth_neighbor <- order_2[!(order_2 %in% order_1)]
  
  if (all(gNNU_neighbors %in% igraph_nth_neighbor)) {
    print("Task 3 works.")
  }
  
  # task 4
  
  max_sim_gNHS <- getNodesHighestSimilarity(rnd_graph)
  
  sim_mat <- similarity(rnd_graph)
  
  max_sim <- max(sim_mat[upper.tri(sim_mat)])
  
  sim_pair <- which(sim_mat == max_sim, arr.ind = TRUE)[2, ]
  
  if (all(max_sim_gNHS %in% sim_pair)) {
    print("Task 4 works.")
  }
}
