library(dequer)
library(igraph)


# Helper functions --------------------------------------------------------

top <- function(x) {
  t <- pop(x)
  push(x, t)
  return(t)
}

plotSearchTree <- function(edge_vec, graph) {
  # save inclusion in spanning tree as edge attribute
  e_ids <- get.edge.ids(graph, edge_vec)
  
  E(graph)$color <- NA
  E(graph)$color[e_ids] <- "gold"
  E(graph)$color[is.na(E(graph)$color)] <- "darkgrey"
  
  plot(graph, label = V(graph)$preorder)
}

# DFS function ------------------------------------------------------------

depFS <- function(v, G, order.out = FALSE, warn = TRUE) {
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  order <- rep(NA, n_nodes)
  preo_edges <- rep(NA, (n_nodes - 1) * 2)
  
  # main body
  P <- stack()
  
  mark[v] <- 1
  
  push(P, v)
  
  i <- 1
  j <- 1
  
  while (length(P) != 0) {
    while(any(neighbors(G, top(P)) %in% which(mark != 1))) {
      w <- neighbors(G, top(P))[neighbors(G, top(P)) %in% which(mark != 1)][1]
      
      mark[w] <- 1
      
      preo_edges[j] <- top(P)
      preo_edges[j + 1] <- w
      j <- j + 2
      
      push(P, w)
    }
    order[i] <- pop(P)
    i <- i + 1
  }
  
  if (any(is.na(order))) {
    if (warn) {
      warning(paste("Node(s)", paste(which(is.na(order)), collapse = ", "),
                    "have/has not been visited, check if node", v,
                    "is part of their/its", "component."))
    }
    
    # if there are unvisited nodes, we also need to prune the vector of 
    # traversed edges
    preo_edges <- preo_edges[!is.na(preo_edges)]
  }
  
  # if requested, return a list containing both the preorder and the edges
  # of the search tree, otherwise return only the vector of the edges
  if (order.out) {
    out <- list(tree_edges = preo_edges,
                order.out = order)
  } else {
    out <- preo_edges
  }
  
  return(out)
}

# BFS ---------------------------------------------------------------------

bredFS <- function(v, G, plot, order.out = FALSE, warn = TRUE)  {
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  order <- rep(NA, n_nodes)
  posto_edges <- rep(NA, (n_nodes - 1) * 2)
  
  # main function body
  Q <- queue()
  
  mark[v] <- 1
  
  pushback(Q, v)
  
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
        
        posto_edges[j] <- u
        posto_edges[j + 1] <- w
        j <- j + 2
      }
    }
  }
  
  if (any(is.na(order))) {
    if (warn) {
      warning(paste("Node(s)", paste(which(is.na(order)), collapse = ", "),
                    "have/has not been visited, check if node", v,
                    "is part of their/its", "component."))
    }
    
    # if there are unvisited nodes, we also need to prune the vector of 
    # traversed edges
    posto_edges <- posto_edges[!is.na(posto_edges)]
  }
  
  # if requested, return a list containing both the postorder and the edges
  # of the search tree, otherwise return only the vector of the edges
  if (order.out) {
    out <- list(tree_edges = posto_edges,
                order.out = order)
  } else {
    out <- posto_edges
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
  
  depFS_edges <- depFS(rnd_vert, rnd_graph)
  plotSearchTree(depFS_edges, rnd_graph)
  
  preorder_depFS <- depFS(rnd_vert, rnd_graph,
                          order.out = TRUE, warn = FALSE)$order.out
  preorder_dfs <- dfs(rnd_graph, rnd_vert, order.out = TRUE)$order.out
  
  if (all(preorder_depFS == preorder_dfs)) {
    print("DFS works")
  } else {
    print("DFS failed")
  }
  
  # breadth first search
  
  bredFS_edges <- bredFS(rnd_vert, rnd_graph)
  plotSearchTree(bredFS_edges, rnd_graph)
  
  postorder_bredFS <- bredFS(rnd_vert, rnd_graph, order.out = TRUE)$order.out
  postorder_bfs <- bfs(rnd_graph, rnd_vert, order = TRUE)$order
  
  if (all(postorder_bredFS == postorder_bfs)) {
    print("BFS works")
  } else {
    print("BFS failed")
  }
  
}

