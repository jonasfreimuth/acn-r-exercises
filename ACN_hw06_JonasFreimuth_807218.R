
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


# Task 1 ------------------------------------------------------------------

bonacicCent <- function (G, alpha = 1, beta = 1) {
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


# Task 2 ------------------------------------------------------------------

fastTransitivity <- function (G) {
  return(NA)
}

erdosRenyiOrder <- function (G) {
  checkValid(G) 
  
  repeats <- 100
  
  n <- length(V(G))
  m <- length(E(G))
  is_dir <- is.directed(G)
  
  targ_trans <- fastTransitivity(G)
  
  comp_trans <- rep(NA, repeats)
  
  for (i in 1:repeats) {
    comp_trans[i] <- fastTransitivity(sample_gnm(n, m, is_dir))
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
    
    n_edges <- c(length(E(rnd_graph1)),
                 length(E(rnd_graph2)),
                 length(E(rnd_graph3)))
    
    if (all(n_edges > 2)) { redo <- FALSE }
  }
  
  bonacics <- list(bonacicCent(rnd_graph1),
                   bonacicCent(rnd_graph2),
                   bonacicCent(rnd_graph3))
  
  eigens <- list(eigen_centrality(rnd_graph1,
                                  directed = is.directed(rnd_graph1))$vector,
                 eigen_centrality(rnd_graph2,
                                  directed = is.directed(rnd_graph2))$vector,
                 eigen_centrality(rnd_graph3,
                                  directed = is.directed(rnd_graph3))$vector)
  
  cors <- list(cor.test(bonacics[[1]], eigens[[1]]),
               cor.test(bonacics[[2]], eigens[[2]]),
               cor.test(bonacics[[3]], eigens[[3]]))
  
  print(cors)
  
  # Answer: 
  # TODO
  
  # Task 2 ---------------------------------------------------------------
  
  
}
