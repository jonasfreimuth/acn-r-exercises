
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
  
  if (is_directed(G)) {
    warning(paste("This algorithm will produce unexpected results on directed",
                  "graphs."))
  }
}

getModularity <- function(G, mem_vec) {
  
  if (ecount(G) < 1) {
    return(0)
  }
  
  degs <- degree(G)
  m <- ecount(G)
  n_clust <- length(unique(mem_vec))
  clust_vec <- unique(mem_vec)
  
  Q <- rep(0, n_clust)
  
  for (clust in clust_vec) {
    
    clust_nodes <- which(mem_vec == clust)
    
    # browser()
    
    E_clust <- ecount(induced.subgraph(G, clust_nodes))
    
    Q[clust] <- (E_clust / m) - (sum(degs[clust_nodes]) / (2 * m)) ^ 2
    
  }
  
  return( sum(Q) )
}


# Task 1 ------------------------------------------------------------------

# currently does not consider giving disconnected nodes to a separate cluster
# an improvement, not sure whether this is supposed to be so
spectralCLusterModularity <- function(G) {
  checkValid(G)
  
  # start with all connected components in one cluster
  # otherwise it doesnt work
  grp_vec <- components(G)$membership
  
  # browser()
  
  curr_mod <- getModularity(G, grp_vec)
  best_mod <- Inf
  
  # browser()
  
  while (best_mod > curr_mod) {
    
    clust_vec <- unique(grp_vec)
    
    curr_mod <- getModularity(G, grp_vec)
    best_mod <- curr_mod
    best_grp <- grp_vec
    
    # browser()
    
    for (clust in clust_vec) {
      
      # browser()
      
      clust_nodes <- which(grp_vec == clust)
      
      # check for degeneracy, should not be necessary
      # n_cl_nds <- length(clust_nodes)
      # 
      # # check if the current selection of nodes forms a clique
      # # (adjacency matrix has edges between all elements)
      # if (
      #   sum(G[clust_nodes, clust_nodes] > 0) >= (((n_cl_nds) ^ 2) - n_cl_nds)
      # ) {
      #   break
      # }
      
      # browser()
      
      sub_graph <- induced.subgraph(G, clust_nodes)
      lap_mat <- laplacian_matrix(sub_graph)
      
      eigvecs <- eigen(lap_mat)$vectors
      
      # get eigenvector associated with algebraic connectivity (second to last
      # eigenvalue)
      eig_vec <- eigvecs[, ncol(eigvecs) - 1]
      
      grp_a <- which(eig_vec <  0)
      grp_b <- which(eig_vec >= 0)
      
      # convert indices of eigenvector to vertex ids
      nds_grp_a <- clust_nodes[grp_a]
      nds_grp_b <- clust_nodes[grp_b]
      
      # give the second division of the current cluster to the next overall 
      # cluster number
      # could maybe be done nicer, so split clusters have adjacent numbers
      # while still having consecutive clust numbers
      grp_vec_tmp <- grp_vec
      grp_vec_tmp[nds_grp_a] <- max(grp_vec) + 1
      
      # sometimes this leads to a disconnected cluster. if this is the case, 
      # check if if brings better modularity if these disconnected components 
      # are considered separate clusters
      
      # browser()
      
      for (grp_nds in list(nds_grp_a, nds_grp_b)) {
        if (length(grp_nds) > 0) {
          # scale grp_nodes back so they match nodes in subgraph
          sub_nds <- 1:length(grp_nds)
          # get the components of the current new cluster
          comp <- components(induced.subgraph(sub_graph, sub_nds))
          
          # when the new cluster consists of disconnected components
          # record each component as new cluster
          if (comp$no > 1) {
            subclust_vec <- comp$membership
            grp_vec_tmp[grp_nds] <- subclust_vec + max(grp_vec_tmp)
            
          }
        }
      }
      
      # calculate modularity with this clustering
      mod_tmp <- getModularity(G, grp_vec_tmp)
      
      # browser()
      
      # check whether this gives a better modularity 
      if (mod_tmp >= best_mod) {
        best_mod <- mod_tmp
        best_grp <- grp_vec_tmp
      }
      
    }
    
    grp_vec <- best_grp
    
    # browser()
    
  }
  
  return(grp_vec)
  
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_pa(runif(1, 1, 20),
                         out.dist = runif(round(runif(1, 1, 20))),
                         directed = FALSE)
  
  plot(rnd_graph)
  
  grp_vec <- spectralCLusterModularity(rnd_graph)
  mod <- getModularity(rnd_graph, grp_vec)
  
  plot(rnd_graph, vertex.color = grp_vec, main = round(mod, 3))
}

