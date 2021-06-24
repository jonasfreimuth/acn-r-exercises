
# Exc 1 -------------------------------------------------------------------

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
  
  # if only one object exists
  if (length(clust_vec) <= 1) {
    return(0)
  }
  
  n_clust <- length(unique(clust_vec))
  
  s_vec <- rep(-2, nrow(dist_mat))
  
  for (obj in 1:nrow(dist_mat)) {
    in_clust <- which(clust_vec == clust_vec[obj])
    
    a_obj <- mean(dist_mat[obj, in_clust])
    
    out_clust_dist <- rep(0, n_clust - 1)
    
    out_clust_vec <- unique(clust_vec)
    out_clust_vec <- out_clust_vec[out_clust_vec != clust_vec[obj]]
    
    for (i in 1:length(out_clust_dist)) {
      clust <- out_clust_vec[i]
      
      clust_objs <- which(clust_vec == clust)
      out_clust_dist[i] <- mean(dist_mat[obj, clust_objs])
    }
    
    b_obj <- min(out_clust_dist)
    
    s_vec[obj] <- (b_obj - a_obj) / max(a_obj, b_obj)
    
    if (is.na(s_vec[obj])) {
      browser()
    }
  }
  
  return(mean(s_vec))
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  
  data <- scale(as.matrix(mtcars))
  
  dist <- as.matrix(dist(data))
  
  grp_vec <- qtCluster(dist, 3)
  
  silhouetteIndex(dist, grp_vec)
}

