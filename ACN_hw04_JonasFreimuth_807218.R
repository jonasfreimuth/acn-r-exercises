
# Helper functions --------------------------------------------------------

check_valid <- function (G) {
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


# Task 4 ------------------------------------------------------------------

# for every node u
#   for every neighbor v of u
#     for every other neighbor w of u
#       if an edge exists between v and w
#         increase count
#     delete the edge between u and v
# return count

nTriangles <- function (graph, tri.out = FALSE) {
  check_valid(graph)
  
  if (tri.out) { 
    triangs <- matrix(nrow = 2 * length(V(graph)),
                      ncol = 3) 
    }
  
  count <- 0
  
  # for every node u
  for (u in V(graph)) {
    out_nbs <- neighbors(graph, u, mode = "out")
    in_nbs <- neighbors(graph, u, mode = "in")
    
    # for every out-neighbor v of u
    for (v in out_nbs) {
      # for every in neighbor w of u
      for (w in in_nbs) {
        # if an edge connects v to w
        if (graph[v, w]) {
          count <- count + 1
          if (tri.out) { triangs[count, ] <- c(u, v, w) }
        }
      }
      
      # delete the edge between u & v
      graph[u, v] <- FALSE
      out_nbs <- out_nbs[out_nbs != v]
    }
  }
  
  if (tri.out) {
    out <- list(n = count,
                triangles = triangs[!apply(triangs, 1, function(x) any(is.na(x))),])
  } else {
    out <- count
  }
  
  return (out)
  
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, min = 1, max = 10),
                          runif(1), directed = TRUE)
  
  plot(rnd_graph)
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                            max = length(V(rnd_graph)))])
  
  rnd_vert_1 <- rnd_vert
  
  rnd_vert_2 <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                              max = length(V(rnd_graph)))])
  
  print(nTriangles(rnd_graph, tri.out = T))
  
}
