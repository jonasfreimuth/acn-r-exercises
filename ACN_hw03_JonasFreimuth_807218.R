library(dequer)
library(igraph)


# Helper functions --------------------------------------------------------

top <- function(x) {
  t <- pop(x)
  push(x, t)
  return(t)
}

# DFS function ------------------------------------------------------------

depFS <- function(v, G, plot = TRUE) {
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  out <- rep(NA, n_nodes)
  
  # if edges are to be plotted, initialize vector for storage and counting
  # var for indexing
  if (plot) {
    preo_edges <- rep(NA, (n_nodes - 1) * 2)
    j <- 1
  }
  
  # main body
  P <- stack()
  
  mark[v] <- 1
  
  push(P, v)
  
  i <- 1
  
  while (length(P) != 0) {
    while(any(neighbors(G, top(P)) %in% which(mark != 1))) {
      w <- neighbors(G, top(P))[neighbors(G, top(P)) %in% which(mark != 1)][1]
      
      mark[w] <- 1
      
      if (plot) {
        preo_edges[j] <- top(P)
        preo_edges[j + 1] <- w
        
        j <- j + 2
      }
      
      push(P, w)
      
    }
    
    # print(top(P))
    
    out[i] <- pop(P)
    
    i <- i + 1
  }
  
  if (any(is.na(out))) {
    warning(paste("Node(s)", paste(which(is.na(out)), collapse = ", "),
                  "have/has not been visited, check if node", v,
                  "is part of their/its", "component."))
    
    # if there are unvisited nodes, we also need to prune the vector of 
    # traversed edges
    preo_edges <- preo_edges[!is.na(preo_edges)]
  }
  
  if (plot) {
    # save preorder as vertex attributes
    names(out) <- 1:length(out)
    V(G)$preorder <- names(sort(out))
    
    # save inclusion in spanning tree as edge attribute
    e_ids <- get.edge.ids(G, preo_edges)
    E(G)$color <- NA
    E(G)$color[e_ids] <- "gold"
    E(G)$color[is.na(E(G)$color)] <- "darkgrey"
    
    plot(G, label = V(G)$preorder)
  }
  
  return(out)
}

# BFS ---------------------------------------------------------------------

bredFS <- function(v, G, plot)  {
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  out <- rep(NA, n_nodes)
  
  # main function body
  Q <- queue()
  
  mark[v] <- 1
  
  pushback(Q, v)
  
  i <- 1
  
  while (length(Q) != 0) {
    u <- pop(Q)
    
    print(u)
    out[i] <- u
    i <- i + 1
    
    for (w in neighbors(G, u)) {
      if (mark[w] != 1) {
        mark[w] <- 1
        pushback(Q, w)
      }
    }
  }
  
  return(out)
  
}

# Tests -------------------------------------------------------------------


if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, min = 1, max = 100), runif(1))
  
  plot(rnd_graph)
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                            max = length(V(rnd_graph)))])
  
  # depth first search
  
  preorder_depFS <- depFS(rnd_vert, rnd_graph)
  preorder_dfs <- dfs(rnd_graph, rnd_vert, order.out = TRUE)$order.out
  
  if (all(preorder_depFS == preorder_dfs)) {
    print("DFS works")
  }
  
  # breadth first search
  
  postorder_bredFS <- bredFS(rnd_vert, rnd_graph)
  postorder_bfs <- bfs(rnd_graph, rnd_vert, order = TRUE)$order
  
  if (all(postorder_bredFS == postorder_bfs)) {
    print("BFS works")
  }
  
}

