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
        
      } } }
  
  return (preord)
}

postordDFS <- function(G, root) { 
  s <- stack()
  x <- rep(0, vcount(G))
  i <- 1
  
  postord <- x
  
  x[root] <- 1
  push(s, root)
  
  while (length(s) > 0) {
    
    u <- pop(s)
    
    for (v in as.numeric(neighbors(G, u))) {
      if (x[v] < 1) {
        
        x[v] <- 1
        push(s, v)
      }
    }
    
    postord[u] <- i
    i <- i + 1
  }
  
  # nodes should always be placed at the head of the list when all their 
  #   neighbors are colored. As the indices correspond to the nodes and the 
  #   values to the postorder, I need to reverse their ranks for the same 
  #   effect.
  return (rank(-postord))
  
}

# highest <- function(G, dfsTree, postord) {
#   postord_ind <- V(G)[postord]
#   highest <- rep(0, vcount(G))
#   
#   solid <- get.edgelist(dfsTree)
#   solid <- split(solid, 1:nrow(solid))
#   
#   full <- get.edgelist(G)
#   full <- split(full, 1:nrow(full))
#   
#   broken <- setdiff(full, solid)
#   
#   for (v in postord_ind) {
#     
#   }
# }

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
  postord <- postordDFS(rnd_graph, rnd_vert)
  
  dfs_elist <- DFS(rnd_graph, rnd_vert)
  DFS_tree <- make_graph(dfs_elist, directed = FALSE)
  
  opar <- par(mfrow = c(1, 2))
  
  plot(DFS_tree,
       main = "Preorder",
       vertex.color = preord,
       palette = graph_grad)
  
  plot(DFS_tree,
       main = "Postorder",
       vertex.color = postord,
       palette = graph_grad)
  
  par(opar)
  
}
