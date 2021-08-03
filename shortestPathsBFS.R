# shortestPathsBFS(G, u)
#   sp[1 .. n] <- Inf
#   mark[1 .. n] <- zeros
#   q <- queue
# 
#   mark[u] <- 1
#   sp[u] <- 0
#   enqueue(q, u)
#   
#   while q not empty do
#     u <- dequeue(q)
#     for every unmarked neighbor v of u do
#       mark[v] <- 1
#       enqueue(q, v)
#       sp[v] <- sp[u] + 1
#     end for
#   end while
#  return sp

shortestPathsBFS <- function(G, u) {
  n <- vcount(G)
  sp <- rep(Inf, n)
  mark <- rep(0, n)
  
  q <- queue()
  
  pushback(q, u)
  mark[u] <- 1
  sp[u] <- 0
  
  while (length(q) > 0) {
    u <- pop(q)
    for (v in as.numeric(neighbors(G, u))) {
      if (mark[v] > 0) {
        next
      }
      mark[v] <- 1
      pushback(q, v)
      sp[v] <- sp[u] + 1
      
    }
  }
  
  return(sp)
}

if (sys.nframe() == 0) {
  library(igraph)
  library(dequer)
  
  rnd_graph <- sample_gnp(20, 0.2, directed = FALSE)
  
  sps <- matrix(rep(Inf, vcount(rnd_graph) ^ 2),
                vcount(rnd_graph),
                vcount(rnd_graph))
  
  for (u in V(rnd_graph)) {
    sps[u, ] <- shortestPathsBFS(rnd_graph, u)
  }
  plot(rnd_graph)
  print(sps)
}
