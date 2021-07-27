library(igraph)

INIT <- function (G) {
  R <- c()
  P <- as.vector(V(G))
  X <- c()
  BC(R, P , X, G, 1)
}

BC <- function (R, P, X, G, nest) {
  if (length(P) == 0 && length(X) == 0) {
    print(paste("Maximal clique: ", paste(R, collapse = ", ")))
    return(R)
  }
  
  for (v in P) {
    N_v <- as.vector(neighbors(G, v))
    cat(rep("\t", nest), "Calling BC from ", nest, ".\n")
    BC(union(R, v),
       intersect(P, N_v),
       intersect(X, N_v),
       G,
       nest + 1)
    
    P <- P[which(P != v)]
    X <- c(X, v)
  }
}

G <- sample_gnp(20, 0.5)
# G <- make_graph(c(1,2, 1,4, 2,4, 2,3, 3,4), directed = F)
plot(G)

INIT(G)