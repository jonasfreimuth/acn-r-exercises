# THIS WORKS ONLY ON UNDIRECTED GRAPHS

threeNodeSub <- function(G) {
  nw_list <- list()
  
  for (u in V(G)) {
    nbs <- neighbors(G, u)
    
    pairs <- t(combn(nbs, 2))
    
    for (i in 1:nrow(pairs)) {
      nw_list[[length(nw_list) + 1]] <- c(u, pairs[i,])
      print(c(u, pairs[i,]))
    }  }
  
  return(nw_list)
}

if (sys.nframe() == 0) {
  library(igraph)
  
  rnd_graph <- sample_gnp(20, 0.2, directed = FALSE)
  
  nws <- threeNodeSub(rnd_graph)
  
  # check if each nw is counted once
  length(nws) == length(unique(nws))
  
}
