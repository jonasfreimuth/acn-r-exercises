
# Helper functions --------------------------------------------------------

# function to ensure all packages are available, and a given graph
# does not 

checkValid <- function (G) {
  if (!require("igraph", quietly = TRUE)) {
    stop("Package 'igraph' required but not installed.")
  }
  
  if (is_weighted(G) || !is_simple(G)) {
    stop(paste("This algorithm does not work on",
               "weighted graphs or multigraphs."))
  }
}

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

# Task 1 ------------------------------------------------------------------

# Answer to question: See at end of script

corrStressCentrEcc <- function (G, floyd.res = NULL) {
  checkValid(G)
  
  stress_centr <- rep(NA, length(V(G)))
  eccent <- stress_centr
  
  # optionally take the result of a call to floyd, so as to reduce running
  # in case the algorithm has run on the same graph before
  if (is.null(floyd.res)) {
    sp <- floyd(G)
  } else {
    # do a rudimentary check whether the incoming result is a result of running 
    # floyd on the input graph
    if (!all(dim(floyd.res[]) == dim(G[]))) {
      stop("floy.res is not a result of running floyd on the input graph.")
    }
    sp <- floyd.res
  }
  
  if (any(is.infinite(as.matrix(sp$dist)))) {
    warning(paste("Some nodes are disconnected, setting their distance to 0."))
    # defining distance to disconnected nodes as 0
    # (what else should I do?)
    sp$dist[is.infinite(as.matrix(sp$dist))] <- 0
  }
  
  for (v in V(G)) {
    stress_centr[v] <- sum(sp$paths == v)
    eccent[v] <- max(sp$dist[v, ])
  }
  
  # in case G is not directed, stress centralities need to be halfed (as paths 
  # would be counted double)
  if (!is.directed(G)) {stress_centr <- stress_centr / 2}
  
  if (length(E(G)) > 1) {
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

# As Graphs which are not strongly disconnected are not excluded, I will be
# using the closeness centrality for networks not strongly connected, defined
# in Opsahl, T., Agneessens, F., Skvoretz, J., 2010. Node centrality in weighted
# networks: Generalizing degree and shortest paths. Social Networks 32 (3),
# 245-251, https://toreopsahl.com/2010/03/20/closeness-centrality-in-networks-with-disconnected-components/
# in cases where graphs are not strongly connected
closeCentrality <- function(G, u = NULL, floyd.res = NULL, raw = TRUE) {
  
  if (is.null(u)) {
    verts <- V(G)
  } else {
    verts <- c(u)
  }
  
  ccents <- rep(NA, length(verts))
  
  # optionally take the result of a call to floyd, so as to reduce running
  # in case the algorithm has run on the same graph before
  if (is.null(floyd.res)) {
    sp <- floyd(G)
  } else {
    # do a rudimentary check whether the incoming result is a result of running 
    # floyd on the input graph
    if (!all(dim(floyd.res[]) == dim(G[]))) {
      stop("floy.res is not a result of running floyd on the input graph.")
    }
    sp <- floyd.res
  }
  
  is_con <- is.connected(G, "strong")
  
  if (!is_con) {
    warning(paste("Graph is not strongly connected, alternative closeness",
                  "centrality formula (sum(1/d(u,v)) over all v != u) will",
                  "be used, results of it will be normalized between 0 and 1."))
  }
  
  for (v in verts) {
    dist_vec <- sp$dist[, v]
    if (is_con) {
      if (raw){
        ccents[v] <- 1 / sum(dist_vec[-v])
      } else {
        ccents[v] <- (length(V(G)) - 1) / sum(dist_vec[-v])
      }
    } else {
      ccents[v] <- sum(1 / dist_vec[-v])
    }
  }
  
  if (!is_con) {
    ccents <- ccents / sum(ccents)
  }
  
  return (ccents)
}


# Task 3 ------------------------------------------------------------------

pageRank <- function (G, alpha = 0.85, k = 1000, precision = 10) {
  if (!require ("igraph", quietly = TRUE)) {
    stop ("Package 'igraph' required but not installed.")
  }
  
  if (!is.simple(G)) {
    stop (paste("This algorithm does not work on",
                "multigraphs."))
  }
  
  if (!is.directed(G)) {
    warning (paste("Given graph is undirected,",
                   "an edge will be treated as both an in- and an out-edge."))
  }
  
  A <- G[]
  n <- nrow(A)
  pr_0 <- rep(1 / n, n)
  
  E <- matrix(1, nrow = nrow(A), ncol = ncol(A))
  
  D <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  diag(D) <- 1 / degree(G, mode = "out")
  
  P <- t(as.matrix(D %*% A))
  P_alpha <- alpha * P + 1 / n * (1 - alpha) * E
  
  P_old <- P_alpha
  P_new <- P_alpha %*% P_alpha
  
  i <- 2
  
  while (all(round(P_new, precision) != round(P_old, precision)) && i <= k) {
    P_old <- P_new
    P_new <- P_old %*% P_old
    i <- i + 1
  }
  
  return (P_new %*% pr_0)
  
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library ("igraph")
  
  reps <- 5
  direct <- TRUE
  
  res.vec <- rep(NA, reps)
  res <- data.frame(n = res.vec, m = res.vec, cor.bara = res.vec,
                    cor.erdos = res.vec, diff = res.vec, same.dist = res.vec,
                    close.match.er = res.vec, close.match.ba = res.vec) 
  
  # for loop for tasks 1 and 2
  for (i in 1:reps) {
    
    n <- round(runif(1, 1, 50))
    
    op <- par(mfrow = c(2, 2))
    
    barabasi_albert <- sample_pa(n,
                                 out.dist = runif(runif(1, min = 1, max = 20)),
                                 directed = direct)
    erdos_renyi <- sample_gnm(n, length(E(barabasi_albert)), directed = direct)
    
    floyd_ba <- floyd(barabasi_albert)
    floyd_er <- floyd(erdos_renyi)
    
    # Task 1: -------------------------------------------------------------
    
    corr_ba <- corrStressCentrEcc(barabasi_albert, floyd.res = floyd_ba)
    corr_er <- corrStressCentrEcc(erdos_renyi, floyd.res = floyd_er)
    
    if (any(is.na(c(corr_ba$pval, corr_ba$corr_coef)))) {
      sub_ba <- "No computation of correlation possible"
    } else {
      sub_ba <- paste(corrRel(corr_ba$corr_coef, corr_ba$pval),
                      paste("r =", round(corr_ba$corr_coef, 2)),
                      paste("p", cut(corr_ba$pval, c(0, 0.05, 1),
                                     c("< 0.05", ">= 0.05"))),
                      sep = ", ")
    }
    
    plot(barabasi_albert, main = "Barabási–Albert", arrow.size = 0.5,
         sub = sub_ba
    )
    
    if (any(is.na(c(corr_er$pval, corr_er$corr_coef)))) {
      sub_er <- "No computation of correlation possible"
    } else {
      sub_er <- paste(corrRel(corr_er$corr_coef, corr_er$pval),
                      paste("r =", round(corr_er$corr_coef, 2)),
                      paste("p", cut(corr_er$pval, c(0, 0.05, 1),
                                     c("< 0.05", ">= 0.05"))),
                      sep = ", ")
    }
    
    plot(erdos_renyi, main = "Erdős–Rényi", arrow.size = 0.5,
         sub = sub_er
    )
    
    res$n[i] <- n
    res$m[i] <- length(E(barabasi_albert))
    res$cor.bara[i] <- corr_ba$corr_coef
    res$cor.erdos[i] <- corr_er$corr_coef
    res$diff[i] <- abs(corr_ba$corr_coef - corr_er$corr_coef)
    
    
    # Task 2 --------------------------------------------------------------
    
    # Call to function for task 2 (Answers to questions at end of script)
    close_ba <- closeCentrality(barabasi_albert, floyd.res = floyd_ba, raw = F)
    close_er <- closeCentrality(erdos_renyi, floyd.res = floyd_er, raw = F)
    
    closeness_ba <- closeness(barabasi_albert, mode = "out")
    closeness_er <- closeness(erdos_renyi, mode = "out")
    
    if (!(all(is.nan(close_ba)) | all(is.nan(close_er)))) {
      
      ks_res <- ks.test(close_ba, close_er)
      
      res$same.dist <- cut(ks_res$p.value, breaks = c(0, 0.05, 1),
                           labels = c("yes", "no"), include.lowest = TRUE)
      
      hist(close_ba, col = "deepskyblue",
           main = "Histogram of closeness centralities")
      hist(close_er, col = "gold",
           main = "Histogram of closeness centralities")
      
    } else {
      plot.new()
      text(0.5, 0, paste("Graph has no edges,",
                       "no closeness centralitiy computation possible.",
                       sep = "\n"))
      plot.new()
      text(0.5, 0, paste("Graph has no edges,",
                       "no closeness centralitiy computation possible.",
                       sep = "\n"))
    }
    
    res$close.match.ba <- all(close_ba %in% closeness_ba)
    res$close.match.er <- all(close_er %in% closeness_er)
    
  }
  
  print(res)
  
  par(op)
  

  # Task 3 ----------------------------------------------------------------

  pr <- pageRank(erdos_renyi)
  
  print(pr[,1])
  
}


# Answer Task 1:  There seems to be no clear relationship of the
#                 eccentricity of a node with the number of shortest
#                 paths passing through it, neither for Barabási–Albert
#                 graphs, nor for Erdős–Rényi graphs of similar size 
#                 with respect to the number of nodes and edges.
# 
# Answer Task 2a: Closeness centralities appear to be distributed differently 
#                 between Erdős–Rényi- and Barabási–Albert-Graphs, with the 
#                 former having the closeness centralities generally around a mean
#                 while the latter had them distributed in a manner which lead to
#                 many nodes with low closeness and only a few ones of high
#                 closeness
# 
# Answer task 2b: The results of my own function for closenessCentrality 
#                 computation do not match the ones returned by the igraph
#                 function. I have no idea why, i don't recall any discussion
#                 of normalization constants.



