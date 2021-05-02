
# Helper functions --------------------------------------------------------

check_valid <- function (G) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("BFS algorithm to find pairwise shortest paths does not work on",
               "weighted graphs or multigraphs."))
  }
}

# Task 1 ------------------------------------------------------------------

pwDistUnweightSimple <- function (G, v1, v2) {
  check_valid(G)
  
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
    
    for (w in neighbors(G, u, mode = "out")) {
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
  return (Inf)
}

# Task 2 ------------------------------------------------------------------

# function to determine eccentricity of a node
getEccUnweightSimple <- function (G, v) {
  check_valid(G)
  
  weight <- rep(NA, length(V(G)))
  for (w in V(G)[-v]) {
    weight[w] <- pwDistUnweightSimple(G, v, w)
  }
  
  return (max(weight, na.rm = TRUE))
}

# function to give nodes belonging to center
getCenterUnweightSimple <- function (G) {
  check_valid(G)
  
  eccs <- sapply(V(G), getEccUnweightSimple, G = G)
  
  return (which(eccs == min(eccs)))
  
}

# function to plot graph with select nodes highlighted
plotHighlight <- function (G, vtcs) {
  V(G)$color <- NA
  V(G)$color[vtcs] <- "gold"
  V(G)$color[is.na(V(G)$color)] <- "darkgray"
  
  plot(G)
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
  
  dist_pwDist <- pwDistUnweightSimple(rnd_graph, rnd_vert_1, rnd_vert_2)
  dist_shrtPath <- shortest.paths(rnd_graph, rnd_vert_1, rnd_vert_2)[1,1]
  
  if (dist_pwDist == dist_shrtPath) {
    print("Task 1 works")
  }
  
  # task 2
  
  ecc_gEUS <- getEccUnweightSimple(rnd_graph, rnd_vert)
  ecc_ecc <- eccentricity(rnd_graph, rnd_vert)
  
  if (ecc_ecc == ecc_gEUS) {
    print("Task 2: Eccentricity determination works") 
  }
  
  ctr_gCUS <- getCenterUnweightSimple(rnd_graph)
  
  plotHighlight(rnd_graph, ctr_gCUS)
  
}

