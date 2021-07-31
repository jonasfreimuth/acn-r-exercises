BFS <- function(G, root) {
  s <- queue()
  x <- rep(0, vcount(G))
  
  elist <- list()
  
  x[root] <- 1
  pushback(s, root)
  
  while (length(s) > 0) {
    
    u <- pop(s)
    
    for (v in as.numeric(neighbors(G, u))) {
      if (x[v] < 1) {
        
        x[v] <- 1
        pushback(s, v)
        
        elist[[length(elist) + 1]] <- c(u, v)
      }
    }
  }
  
  return (unlist(elist))
}

DFS <- function(G, root) {
  s <- stack()
  x <- rep(0, vcount(G))
  
  elist <- list()
  
  x[root] <- 1
  push(s, root)
  
  while (length(s) > 0) {
    
    u <- pop(s)
    
    for (v in as.numeric(neighbors(G, u))) {
      if (x[v] < 1) {
        
        x[v] <- 1
        push(s, v)
        
        elist[[length(elist) + 1]] <- c(u, v)
      }
    }
  }
  
  return (unlist(elist))
}

if (sys.nframe() == 0) {
  library("igraph")
  library("dequer")
  
  rnd_graph <- sample_gnp(20, 0.5)
  rnd_vert <- as.numeric(sample(V(rnd_graph), 1))
  
  opar <- par(mfrow = c(1, 2))
  
  dfs_elist <- BFS(rnd_graph, rnd_vert)
  dfs_e_ind <- get.edge.ids(rnd_graph, dfs_elist)
  
  col_vec <- rep("grey", ecount(rnd_graph))
  col_vec[dfs_e_ind] <- "gold"
  
  
  plot(rnd_graph, edge.color = col_vec, main = "BFS")
  
  
  
  dfs_elist <- DFS(rnd_graph, rnd_vert)
  dfs_e_ind <- get.edge.ids(rnd_graph, dfs_elist)
  
  col_vec <- rep("grey", ecount(rnd_graph))
  col_vec[dfs_e_ind] <- "gold"
  
  
  plot(rnd_graph, edge.color = col_vec, main = "DFS")
  
  par(opar)
  
}
