
# Helper functions --------------------------------------------------------

# function to ensure all packages are available, and a given graph
# does not violate any of the assumptions a function makes

checkValid <- function (G) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("This algorithm does not work on",
               "weighted graphs or multigraphs."))
  }
}

# transitivity from hw_04

nTriangles <- function (graph, tri.out = FALSE) {
  checkValid(graph)
  
  is_dir <- is.directed(graph)
  
  # if requested, initialize matrix for vertex ids each triangles
  if (tri.out) { 
    triangs <- matrix(nrow = length(V(graph))^3,
                      ncol = 3) 
  }
  
  count <- 0
  
  # for every node u
  for (u in V(graph)) {
    # in case of directed graph, a triangle consists of the edges 
    # u -> v -> w -> u, with v an out-neighbor of u and w an in-neighbor of u
    out_nbs <- neighbors(graph, u, mode = "out")
    in_nbs <- neighbors(graph, u, mode = "in")
    
    # for every out-neighbor v of u
    for (v in out_nbs) {
      # for every in neighbor w of u
      for (w in in_nbs) {
        # if an edge connects v to w
        if (graph[v, w]) {
          count <- count + 1
          # print(c(u, v, w))
          if (tri.out) { triangs[count, ] <- c(u, v, w) }
        }
      }
      
      # delete the edge between u & v
      # both from graph and the neighborhoods
      graph[u, v] <- FALSE
      if (!is_dir) { graph[v, u] <- FALSE }
      out_nbs <- out_nbs[out_nbs != v]
      if (!is_dir) { in_nbs <- in_nbs[in_nbs != v] }
    }
  }
  
  # prepare output
  if (tri.out) {
    out <- list(n = count,
                triangles = triangs[!apply(triangs,
                                           1,
                                           function(x) any(is.na(x)))
                                    ,])
  } else {
    out <- count
  }
  
  return (out)
  
}

# 2-Paths

# for every node u
#   for every in-neighbor v of u
#     for every out-neighbor w of u
#       increase count

nTwoPath <- function(graph) {
  checkValid(graph)
  
  is_dir <- is.directed(graph)
  
  count <- 0
  
  # for every node u
  for (u in V(graph)) {
    out_nbs <- neighbors(graph, u, mode = "out")
    in_nbs <- neighbors(graph, u, mode = "in")
    
    # these formulas should always amount to the number of cycles in the for 
    # loops from the pseudocode
    if (is_dir) {
      count <- count + length(in_nbs) * length(out_nbs)
    } else {
      count <- count + length(in_nbs) * (length(in_nbs) - 1) / 2
    }
  }
  
  return (count)
}

# transitivity function

transtvty <- function (graph) {
  checkValid(graph)
  
  return (3 * nTriangles(graph) / nTwoPath(graph))
  
}


# Task 1 ------------------------------------------------------------------

bonacichCent <- function (G, alpha = 1, beta = 1) {
  checkValid(G)
  
  A <- as.matrix(G[])
  n <- nrow(A)
  
  I <- matrix(0, nrow = n, ncol = n)
  diag(I) <- 1
  
  one <- matrix(1, nrow = n, ncol = 1)
  
  P <- I - beta * A
  
  if (det(P) == 0) {
    warning ("Intermediated matrix not invertible, returning vector of NAs.")
    
    return (matrix(NA, nrow = n, ncol = 1))
  }
  
  bc <- (alpha * solve(P)) %*% A %*% one
  
  return(bc)
}

eigenvecCent <- function (G) {
  checkValid(G) 
  
  A <- as.matrix(G[])
  
  eigens <- eigen(A)
  
  if (Im(eigens$value[1]) != 0) {
    error("Leading eigenvalue not simple")
  }
  
  return(as.vector(as.numeric(eigens$vectors[,1])))
}


# Task 2 ------------------------------------------------------------------

erdosRenyiOrder <- function (G) {
  checkValid(G) 
  
  repeats <- 100
  
  n <- length(V(G))
  m <- length(E(G))
  is_dir <- is.directed(G)
  
  targ_trans <- transtvty(G)
  
  comp_trans <- rep(NA, repeats)
  
  for (i in 1:repeats) {
    comp_trans[i] <- transtvty(sample_gnm(n, m, is_dir))
  }
  
  comp_trans <- mean(comp_trans, na.rm = TRUE)
  
  return (targ_trans > comp_trans)
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library (igraph)
  
  # Task 1 ----------------------------------------------------------------
  
  redo <- TRUE
  
  while (redo) {
    rnd_graph1 <- sample_gnp(runif(1, 1, 20), runif(1), directed = TRUE)
    rnd_graph2 <- sample_gnp(runif(1, 1, 20), runif(1), directed = TRUE)
    rnd_graph3 <- sample_gnp(runif(1, 1, 20), runif(1), directed = TRUE)
    
    # ensure sufficient edges
    n_edges <- c(length(E(rnd_graph1)),
                 length(E(rnd_graph2)),
                 length(E(rnd_graph3)))
    
    # ensure connectedness
    degrees <- c(degree(rnd_graph1, mode = "out"),
                 degree(rnd_graph2, mode = "out"),
                 degree(rnd_graph3, mode = "out"))
    
    if (all(n_edges > 2) && !any(degrees == 0)) { redo <- FALSE }
    
    bonacichs <- list(bonacichCent(rnd_graph1),
                      bonacichCent(rnd_graph2),
                      bonacichCent(rnd_graph3))
    
    if (!any(is.na(do.call(rbind, bonacics)))) { redo <- FALSE }
  }
  
  eigens <- list(eigenvecCent(rnd_graph1),
                 eigenvecCent(rnd_graph2),
                 eigenvecCent(rnd_graph3))
  
  cors <- list(cor(bonacichs[[1]], eigens[[1]]),
               cor(bonacichs[[2]], eigens[[2]]),
               cor(bonacichs[[3]], eigens[[3]]))
  
  print(cors)
  
  # Answer: 
  # As Bonacich centrality is a generalization of eigenvector centrality, I
  # would have expected there to be consistent correlation or anti-correlation
  # between both. This does however not seem to be the case. The reason
  # here could lie in the choice of parameter for the Bonacich centrality,
  # or of course in the manner both centrality measures were implemented.
  
  # Task 2 ---------------------------------------------------------------
  
  time <- system.time(erdosOrder <- erdosRenyiOrder(rnd_graph1))[["elapsed"]]
  
  if (erdosOrder) {
    print("Graph has higher transitivity than the average Erdős–Rényi graph.")
  } else {
    print("Graph has lower transitivity than the average Erdős–Rényi graph.")
  }
  
  print(paste("It took", round(time), "seconds to find that out."))
  
}
