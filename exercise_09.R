
# Exc 1 -------------------------------------------------------------------

# currently gets stuck in infinite loop, aparently
qtCluster <- function (dist_mat, diam)  {
  
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


# Exc 2 -------------------------------------------------------------------

silhouetteIndex <- function (dist_mat, clust_vec) {
  
  n_clust <- length(unique(clust_vec))
  
  s <- rep(-2, nrow(dist_mat))
  
  for (obj in 1:nrow(dist_mat)) {
    in_clust <- which(clust_vec == clust_vec[obj])
    in_clust <- in_clust[in_clust != obj]
    
    out_clust_dist <- rep(0, n_clust - 1)
    
    for (clust in 1:(n_clust - 1)) {
      clust_objs <- which(clust_vec == clust)
      out_clust_dist[clust] <- mean(dist_mat[obj, clust_objs])
    }
    
    a_obj <- mean(dist_mat[obj, in_clust])
    
    b_obj <- min(out_clust_dist)
    
    s[obj] <- (b_obj - a_obj) / max(a_obj, b_obj)
  }
  
  return(mean(s))
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  
  data <- scale(as.matrix(mtcars))
  
  dist <- as.matrix(dist(data))
  
  grp_vec <- qtCluster(dist, 3)
  
  silhouetteIndex(dist, grp_vec)
}
