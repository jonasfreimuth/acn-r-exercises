
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

getModularity <- function(G, mem_vec) {
  
  degs <- degree(G)
  m <- ecount(G)
  n_clust <- length(unique(mem_vec))
  clust_vec <- unique(mem_vec)
  
  Q <- rep(0, n_clust)
  
  for (clust in clust_vec) {
    
    clust_nodes <- which(mem_vec == clust)
    E_clust <- ecount(induced.subgraph(G, clust_nodes))
    
    Q[clust] <- (E_clust / m) - (sum(degs[clust_nodes]) / (2 * m)) ^ 2
    
  }
  
  return( sum(Q) )
}


# Task 1 ------------------------------------------------------------------

# while modularity improves
# attempt to divide all clusters
# if any division provides an improvement, follow through
# if no divisions are made in a loop stop

spectralCLusterModularity <- function(G) {
  
  grp_vec <- rep(1, vcount(G))
  
  curr_mod <- getModularity(G, grp_vec)
  last_mod <- - 1
  
  while (curr_mod > last_mod) {
    
    clust_vec <- unique(grp_vec)
    
    for (clust in clust_vec) {
      
      clust_nodes <- which(grp_vec == clust)
      
      lap_mat <- laplacian_matrix(induced.subgraph(G, clust_nodes))
      
      # TODO: check for degeneracy (perhaps via clique)
      eigens <- eigen(lap_mat)
      eig_vals <- eigens$values
      
      # stupid construct to get the second smallest eigenval
      alg_conn <- min(eig_vals[-which(eig_vals == min(eig_vals))])
      
      # similarly weird construct to get the associated eigenvec
      eig_vec <- eigens$vectors[, which(eig_vals == alg_conn)]
      
      # grp_a <- which(eig_vec <  0), this is implicit
      grp_b <- which(eig_vec >= 0)
      
      # give the second division of the current cluster to the next overall 
      # cluster number
      # could maybe be done nicer, so split clusters have adjacent numbers
      grp_vec_tmp <- grp_vec
      grp_vec_tmp[grp_b] <- max(grp_vec) + 1
      
      # check if this offers an improvement
      if (getModularity(G, grp_vec_tmp) > last_mod) {
        grp_vec <- grp_vec_temp
      }
      
      last_mod <- curr_mod
      curr_mod <- getModularity(G, grp_vec)
      
    }
    
  }
  
  return(grp_vec)
  
}



# Tests -------------------------------------------------------------------

if (sys.nframe() == 0){
  library("igraph")
  
  rnd_graph <- sample_pa(runif(1, 1, 20),
                         out.dist = runif(round(runif(1, 0, 20))),
                         directed = FALSE)
  
  plot(rnd_graph)
  
  grp_vec <- spectralCLusterModularity(rnd_graph)
  
  plot(rnd_graph, vertex.colors = grp_vec)
}
