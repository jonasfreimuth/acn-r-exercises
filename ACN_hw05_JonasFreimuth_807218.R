
# Helper functions --------------------------------------------------------

# function to ensure all packages are available, and a given graph
# does not 

checkValid <- function (G) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (!require("dequer", quietly = TRUE)) {
    stop("Package 'dequer' required but not installed.")
  }
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("BFS algorithm to find pairwise shortest paths does not work on",
               "weighted graphs or multigraphs."))
  }
}


# Task 1 ------------------------------------------------------------------

# function to obtain pairwise distance matrix and optionally a matrix of
#   nodes visited on that path using floyd's algorithm
#   adapted from exercise 5 solution
floyd <- function (G) {
  checkValid(G)
  
  D <- G[]
  D[D == 0] <- Inf
  diag(D) <- 0
  
  P <- matrix(0, nrow(D), ncol(D))
  
  n <- nrow(D)
  
  for (k in 1:n) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (D[i, j] > D[i, k] + D[k, j]) {
          D[i, j] <- D[i, k] + D[k, j]
          P[i, j] <- k
        }
      }
    }
  }
  
  return (list(dist = D, paths = P))
  
}

corrStressCentrEcc <- function (G) {
  checkValid(G)
  
  stress_centr <- rep(NA, length(V(G)))
  eccent <- stress_centr
  
  sp <- floyd(G)
  
  sp$dist[is.infinite(as.matrix(sp$dist))] <- 0
  
  for (v in V(G)) {
    stress_centr[v] <- sum(sp$paths == v) 
    eccent[v] <- max(sp$dist[v, ])
  }
  
  if (length(E(G)) > 1) {
    # TODO Deal with inf values
    corr <- cor.test(eccent, stress_centr)
  } else {
    warning(paste("Not enough vertices in Graph",
                  "to calculate correlation, NA generated."))
    corr <- list(estimate = NA, p.value = NA)
  }
  
  return (list(corr_coef = corr$estimate, pval = corr$p.value,
               stress_centrality = stress_centr, eccentricity = eccent))
}

corrRel <- function (coef, p.val) {
  if (any(is.na(c(coef, p.val)))) {
    return ("No calculation of relationship possible")
  }
  if (p.val > 0.05) {
    return ("No significant relationship")
  } else {
    if (coef > 0) {
      return ("Significant positive realtionship")
    } else if (coef < 0) {
      return ("Significant negative realtionship")
    } else {
      return ("Significant absence of any relationship")
    }
  }
}


# Task 2 ------------------------------------------------------------------




# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library ("igraph")
  
  # Task 1 ----------------------------------------------------------------
  
  reps <- 3
  
  for (i in 1:reps) {
    
    n <- runif(1, 1, 40)
    
    op <- par(mfrow = c(1, 2))
    
    barabasi_albert <- sample_pa(n, out.dist = runif(runif(1, max = 20)))
    erdos_renyi <- sample_gnm(n, length(E(barabasi_albert)), directed = TRUE)
    
    corr_ba <- corrStressCentrEcc(barabasi_albert)
    corr_er <- corrStressCentrEcc(erdos_renyi)
    
    plot(barabasi_albert, main = "Barabási–Albert",
         sub = paste(corrRel(corr_ba$corr_coef, corr_ba$pval),
                     paste("r =", round(corr_ba$corr_coef, 2)),
                     paste("p =", cut(corr_ba$pval, c(0, 0.05, 1),
                                      c("< 0.05", ">= 0.05"))),
                     sep = ", ")
         )
    
    plot(erdos_renyi, main = "Erdős–Rényi",
         sub = paste(corrRel(corr_er$corr_coef, corr_er$pval),
                     paste("r =", round(corr_er$corr_coef, 2)),
                     paste("p ", cut(corr_er$pval, c(0, 0.05, 1),
                                      c("< 0.05", ">= 0.05"))),
                     sep = ", ")
         )
  }
  
  par(op)
}

