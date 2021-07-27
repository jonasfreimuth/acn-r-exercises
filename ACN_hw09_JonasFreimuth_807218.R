
# Helper functions --------------------------------------------------------

# function to ensure all packages are available, and a given graph
# does not violate any of the assumptions a function makes

checkValid <- function (G) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("This algorithm does not work on",
               "weighted graphs or multigraphs."))
  }
}

# Task 1 ------------------------------------------------------------------

# function implementing qtClustering for networks, using pairwise shortest
# paths as a distance measure
qtClusterNetwork <- function(G, diam) {
  checkValid(G)
  
  dist_mat <- distances(G, mode = "out")
  
  # index corresponds to point (index of dist_mat)
  # value corresponds to group identity
  grp_vec <- rep(0, nrow(dist_mat))
  clust_no <- 1
  
  while (any(grp_vec == 0)) {
    
    max_clust <- 0
    
    for (p in 1:nrow(dist_mat)) {
      
      clust <- which(dist_mat[p, ] <= diam / 2 & grp_vec == 0)
      
      if (length(clust) >= max_clust) {
        max_clust <- length(clust)
        
        max_clust_vec <- clust
      }
    }
    
    grp_vec[max_clust_vec] <- clust_no
    clust_no <- clust_no + 1
  }
  
  return(grp_vec)
   
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1, 0.2, 0.9), directed = FALSE)
  
  # plot(rnd_graph)
  
  rnd_diam <- round(runif(1, 1, length(V(rnd_graph))))
  
  grp_vec <- qtClusterNetwork(rnd_graph, 2)
  
  print(grp_vec)
  
  plot(rnd_graph, vertex.color = grp_vec)  
  
}
