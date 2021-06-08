library(igraph) 

rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1), directed = FALSE)

kCoreDecomp <- function (G) {
  
  n <- length(V(G))
  
  coreness <- rep(0, n)
  
  k <- 0
  
  degs = degree(G)
  
  while (any(degs[!is.na(degs)] > 0)) {
    
    rm_nodes <- which(degs <= k) 
    
    while (length(!is.na(rm_nodes)) != 0) {
      rm_nodes <- which(degs <= k)
      
      coreness[rm_nodes] <- k
      
      degs[rm_nodes] <- NA
      
      for (node in rm_nodes)  {
        nbs <- neighbors(G, node)
        degs[nbs] <- degs[nbs] - 1
      }
      
    }
    
    k <- k + 1
  }
  
  return (coreness)
}

plot(rnd_graph)

print(kCoreDecomp(rnd_graph))

