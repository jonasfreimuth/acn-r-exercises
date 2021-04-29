
# Task 1 ------------------------------------------------------------------

pwDistUnweightSimple <- function(v1, v2, G) {
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("BFS algorithm to find pairwise shortest paths does not work on",
               "weighted graphs or multigraphs."))
  }
  
  if (v1 == v2) {
    return(0)
  }
  
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  order <- rep(NA, n_nodes)
  weight <- rep(0, n_nodes)
  
  # main function body
  Q <- queue()
  
  mark[v1] <- 1
  
  pushback(Q, v1)
  
  i <- 1
  j <- 1
  
  while (length(Q) != 0) {
    u <- pop(Q)
    
    order[i] <- u
    i <- i + 1
    
    for (w in neighbors(G, u)) {
      if (mark[w] != 1) {
        mark[w] <- 1
        pushback(Q, w)
        
        weight[w] <- weight[u] + 1
        
        if (w == v2) {
          return (weight[w])
        }
      }
    }
  }
  return(Inf)
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, min = 1, max = 100), runif(1))
  
  plot(rnd_graph)
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                            max = length(V(rnd_graph)))])
  
  rnd_vert_1 <- rnd_vert
  
  rnd_vert_2 <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                              max = length(V(rnd_graph)))])
  
  # task 1
  
  dist_pwDist <- pwDistUnweightSimple(rnd_vert_1, rnd_vert_2, rnd_graph)
  dist_shrtPath <- shortest.paths(rnd_graph, rnd_vert_1, rnd_vert_2)[1,1]
  
  if (dist_pwDist == dist_shrtPath) {
    print("Task 1 works")
  }
}

