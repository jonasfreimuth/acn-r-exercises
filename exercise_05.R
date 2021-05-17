
# Helper funcs ------------------------------------------------------------

nShortestPaths <- function (G, v1, v2, dist.out = FALSE) {
  checkValid(G)
  
  # if both nodes are the same, distance is 0, no need for the rest of 
  # the algorithm
  if (v1 == v2) {
    if (dist.out) {
      out <- list(n = 0, dist = 0)
    } else {
      out <- list(n = 0)
    }
    return (out)
  }
  
  # additional prerequisites
  n_nodes <- length(V(G))
  mark <- rep(0, n_nodes)
  dist <- rep(Inf, n_nodes)
  count <- 0
  
  # main function body
  Q <- queue()
  
  mark[v1] <- 1
  dist[v1] <- 0
  
  pushback(Q, v1)
  
  while (length(Q) != 0) {
    # get first node
    u <- pop(Q)
    
    for (w in neighbors(G, u, mode = "out")) {
      # visit only unmarked nodes
      if (mark[w] != 1) {
        # if we visit a node here its distance can't be inf
        if (is.infinite(dist[w])) {
          dist[w] <- 0
        }
        
        mark[w] <- 1
        pushback(Q, w)
        dist[w] <- dist[u] + 1
      }
      
      # this may be executed more often than it needs to but
      # i didnt know where else to put it
      if (count > 0 && max(dist) > dist[v2]) {
        break
      }
      
      # if we found v2 again, increment count
      # if we found the node already, it would be marked, thats why this
      # is outside the check for marks
      if (w == v2) {
        count <- count + 1
      }
      
    }
  }
  
  # construct output, if no path between v1 and v2, dist is Inf
  if (dist.out) {
    out <- list(n = count, dist = dist[v2])
  } else {
    out <- list(n = count)
  }
  
  return (out)
}

# recursive square and multiply algorithm for matrices
recMatPower <- function(A, k) {
  
  if (k == 1) {
    return(A)
    
  } else {
    A_sq <- recMatPower(A, floor(k/2))
    
    if (k %% 2 == 0) {
      return(A_sq %*% A_sq)
      
    } else {
      return(A_sq %*% A_sq %*% A)
    }
  }
}

# wrapper function to check prerequisites
matrixPower <- function(A, k) {
  
  # if x is not already a matrix, check if it is coercible to one
  # (raises an error is x is not a matrix and not coercible)
  x <- as.matrix(A)
  
  # check if all dimensions are even equal
  if (!all(dim(A) == dim(A)[1])) {
    stop('Matrix is not symmetric!')
  }
  
  if (k == 0) {
    return(diag(nrow = nrow(A), ncol = ncol(A)))
  }
  
  A <- recMatPower(A, k)
  
  print(A)
  
  return(A)
}


# Task 1 ------------------------------------------------------------------

kWalksNaive <- function (G, k, u, v) {
  A <- as.matrix(G[])
  A_new <- A
  for (i in 2:k) {
    A_new <- A_new %*% A
  }
  
  return (A_new[u, v])
}


kWalks <- function (G, k, u, v) {
  A <- as.matrix(G[])
  
  A_new <- recMatPower(A, k)
  
  return (A_new[u, v])
}


# Task 2 ------------------------------------------------------------------

is.irreducible <- function (G) {
  sp <- shortest.paths(G)
  
  A <- as.matrix(G[])
  
  sp_vec <- c(sp[upper.tri(A)], sp[lower.tri(A)])
  
  if (any(sp_vec == 0)) {
    return (FALSE)
  } else {
    return (TRUE)
  }
}


# Task 3 ------------------------------------------------------------------

katzCentrality <- function (G, beta = 1) {
  A <- as.matrix(G[])
  
  I <- matrix(0, nrow = nrow(A), ncol = ncol(A))
  diag(I) <- 1
  
  B <- I - beta * t(A)
  cent <- (solve(B) - I) %*% matrix(1, nrow = nrow(A), ncol = 1)
  
  return (cent)
}

# Tests -------------------------------------------------------------------


if (sys.nframe() == 0) {
  library (igraph)
  rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1))
  
  plot(rnd_graph)
  
  rnd_v1 <- round(runif(1, 1, length(V(rnd_graph))))
  rnd_v2 <- round(runif(1, 1, length(V(rnd_graph))))
  
  system.time(k_walks_naive <- kWalksNaive(rnd_graph, 5, rnd_v1, rnd_v2))
  system.time(k_walks <- kWalks(rnd_graph, 5, rnd_v1, rnd_v2))
  
}
