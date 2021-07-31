preordDFS <- function(G, root) {
  s <- stack()
  x <- rep(0, vcount(G))
  i <- 1
  
  preord <- x
  
  x[root] <- 1
  push(s, root)
  
  preord[root] <- i
  i <- i + 1
  
  while (length(s) > 0) {
    
    u <- pop(s)
    
    for (v in as.numeric(neighbors(G, u))) {
      if (x[v] < 1) {
        
        x[v] <- 1
        push(s, v)
        
        preord[v] <- i
        i <- i + 1
      }
    }
  }
  
  return (preord)
}

postordDFS <- function(G) {
  
}

articulationPoints <- function(G) {
  
}

if (sys.nframe() == 0) {
  
  library("igraph")
  library("dequer")
  library("RColorBrewer")
  source("DFS_BFS.R")
  
  blue_grad <- colorRampPalette(c("white", "black"))
  
  rnd_graph <- sample_gnp(20, 0.5)
  rnd_vert <- as.numeric(sample(V(rnd_graph), 1))
  
  graph_grad <- blue_grad(vcount(rnd_graph))
  
  preord <- preordDFS(rnd_graph, rnd_vert)
  
  dfs_elist <- DFS(rnd_graph, rnd_vert)
  DFS_tree <- make_graph(dfs_elist, directed = FALSE)
  
  plot(DFS_tree, vertex.color = preord, palette = graph_grad)
  
}
