
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


# Task 4 ------------------------------------------------------------------

# Triangles:

# for every node u
#   for every neighbor v of u
#     for every other neighbor w of u
#       if an edge exists between v and w
#         increase count
#     delete the edge between u and v
# return count

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


# Task 2 ------------------------------------------------------------------


# from last week:
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


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  library ("igraph")
  
  rnd_graph <- sample_gnp(runif(1, min = 1, max = 20),
                          runif(1),
                          directed = TRUE
  )
  
  plot(rnd_graph)
  
  rnd_vert <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                            max = length(V(rnd_graph)))])
  
  rnd_vert_1 <- rnd_vert
  
  rnd_vert_2 <- as.numeric(V(rnd_graph)[runif(1, min = 1,
                                              max = length(V(rnd_graph)))])
  
  tris <- nTriangles(rnd_graph, tri.out = T)
  print(paste("Triangles:", tris$n))
  print(tris$triangles)
  print(paste("2-Paths:", nTwoPath(rnd_graph)))
  
  print("Transitivities if undirected:")
  print(paste("Igraph:", transitivity(rnd_graph)))
  print(paste("Own:", transtvty(as.undirected(rnd_graph))))
  print(paste(
    "Equal:", transitivity(rnd_graph) == transtvty(as.undirected(rnd_graph)))
  )
  
  print(paste("Own directed:", transtvty(rnd_graph)))
  
  nSP <- nShortestPaths(rnd_graph, rnd_vert_1, rnd_vert_2, dist.out = TRUE)
  print(paste("Distance between",
              rnd_vert_1, "and", rnd_vert_2,
              "is:", nSP$dist))
  
  print(paste("There exist",
              nSP$n, "paths of minimal length between them."))
  
}
