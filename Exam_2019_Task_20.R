# This works, assuming a subnetwork consists of the tuple of nodes 
#   V' = (v, u, w) and E' = ((v,u), (u,w))

# Pseudocode: Given a graph G
# threeNodeSub(G):
#   for each u in G do
#     for every in-neighbor v of u, v != w do
#       for every out-neighbor w of u, w != u, v do
#         print(v, u, w)
#       end for
#     end for
#   end for

threeNodeSub <- function(G) {
  # x <- rep(NA, factorial(vcount(G)))
  # nw_out <- data.frame(v = NULL, u = NULL, w = NULL)
  # i <- 1
  
  for (u in V(G)) {
    for (v in neighbors(G, u, "in")) {
      if (v == u) { next }
      for (w in neighbors(G, v, "out")) {
        if (w %in% c(v, u)) { next }
        print(c(u, v, w))
        
        # nw_out[i, ] <- c(v, u, w)
        # i <- i + 1
      }
    }
  }
  # return(na.omit(nw_list))
}

if (sys.nframe() == 0) {
  library(igraph)
  
  rnd_graph <- sample_gnp(5, 1, directed = TRUE)
  
  nws <- threeNodeSub(rnd_graph)
  
}
