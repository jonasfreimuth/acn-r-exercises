
# DISCLAIMER --------------------------------------------------------------

# I worked on this homework together with Franziska and Constanze

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

compCentCorr <- function (G) {
  G_comp <- complementer(G)
  
  cent_corr <- list()
  
  deg_cent <-cbind(degree(G), degree((G_comp)))
  cent_corr["degree"] <- cor(deg_cent[, 1], deg_cent[, 2])
  
  deg_cent <- cbind(eigen_centrality(G)$vector,
                    eigen_centrality(G_comp)$vector)
  cent_corr["eigen"] <- cor(deg_cent[, 1], deg_cent[, 2])
  
  deg_cent <- cbind(closeness(G), closeness(G_comp))
  cent_corr["closeness"] <- cor(deg_cent[, 1], deg_cent[, 2])
  
  return (cent_corr)
}


# Task 2 ------------------------------------------------------------------

minConn4Paths <- function (G) {
  
  n <- 0
  
  paths <- list()
  
  # adapted from Seiranas solution to homework 4
  for (u in V(G)) {
    for (v in V(G)) {
      if (G[u, v] == 1) {
        
        for (w in V(G)) {
          if (G[v, w] == 1 && 
              G[u, w] == 0 &&
              G[w, u] == 0) {
            
            for (x in V(G)) {
              if (G[w, x] == 1 &&
                  G[u, x] == 0 && G[v, x] == 0 &&
                  G[x, u] == 0 && G[x, v] == 0) {
                
                n <- n + 1
                paths[[n]] <- c(u, v, w, x)
              }
            }
            
          }
        }
        
      }
    }
  }
  
  return (list(n = n, paths = paths))
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library("igraph")
  
  direct <- FALSE
  
  n <- round(runif(1, 1, 20))
  
  barabasi_albert <- sample_pa(n,
                               out.dist = c(0.1, rep(0,8), 0.9),
                               # out.dist = runif(runif(1, min = 1, max = 20)),
                               directed = direct)
  erdos_renyi <- sample_gnm(n, length(E(barabasi_albert)), directed = direct)
  
  op <- par(mfrow = c(1, 2))
  
  plot(barabasi_albert, main = "Barabási–Albert")
  plot(erdos_renyi, main = "Erdős–Rényi")
  
  par(op)

  # Task 1 ----------------------------------------------------------------

  # There is an inverse relationship of the number of edges in a Graph G to the 
  # number of edges in its complement C: 
  # m_c = (n * (n - 1)) / 2 - m_G, with m_C = # Edges in C,
  # m_G = # Edges in G, and n = # Nodes in G and C. The term (n * (n - 1)) / 2
  # describes the number of edges in a clique with n nodes.
  # 
  # Also, in a clique with n nodes, the degree of each node is n - 1. This 
  # implies that the degree sequence of C is 
  # S_C = ( (n - 1) - s_n, (n - 1) - s_(n-1), ..., (n - 1) - s_1 ). 
  # 
  # From this inversion, it follows that any centrality measure having to do
  # with degrees will show strong negative correlation between the measure
  # applied to its graph and its complement.
  # 
  # As the degree distribution is derived from the degree sequence, we can 
  # tell that the degree distribution of a complement is the degree
  # distribution of its graph, but mirrored along the x-axis.
  
  cors <- compCentCorr(erdos_renyi)
  
  print(cors)

  # Task 2 ----------------------------------------------------------------

  # Due to the different ways they are generated, Erdős–Rényi and 
  # Barabási–Albert graphs show different numbers of minimally connected 4 
  # paths. In the Barabási–Albert model the graph is constructed iteratively
  # with new nodes being coninuously added and edges being drawn from old 
  # nodes to the new one with given probabilities, while in the Erdős–Rényi
  # model all nodes are present from the start and edges are being drawn at 
  # random. As there is much more interconnectedness in a Erdős–Rényi graph
  # than in a Barabási–Albert graph, the former will generally have less 
  # 4 paths than the former given the same number of nodes and edges in both
  # is the same.
  
  n4Paths_ba <- minConn4Paths(barabasi_albert)
  n4Paths_er <- minConn4Paths(erdos_renyi)
  
  print(paste("Number of 4-paths in the Barabási–Albert graph:",
              n4Paths_ba$n))
  print(paste("Number of 4-paths in the Erdős–Rényi graph:",
              n4Paths_er$n))
}
