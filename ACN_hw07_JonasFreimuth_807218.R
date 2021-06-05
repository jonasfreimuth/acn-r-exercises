
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

# I'm sorry, person reading this. I just had to do everything pre-assigned and 
# pseudo-memory efficient, taking much longer to code this than I would have 
# liked
stubMatching <- function(degSeq, simple = TRUE, n_tries = 50) {
  
  if (is.unsorted(rev(degSeq))) {
    warning (paste("Input degSeq is not non-increasing,",
                   "sorting input in decreasing order."))
    degSeq <- sort(degSeq, decreasing = TRUE)
  }
  
  if (sum(degSeq) %% 2 != 0) {
    stop ("Degree sequence not graphic")
  }
  
  if (sum(degSeq) <= 0) {
    warning ("Resulting graph has no edges")
    
    G <- make_empty_graph(n = length(degSeq))
    return (G)
  }
  
  start_over <- TRUE
  k <- 1
  while (start_over && k < n_tries) {
    start_over <- FALSE
    
    stub_vec <- rep(NA, sum(degSeq))
    
    i <- 1
    j <- 1
    while (i <= length(degSeq)) {
      if (degSeq[i] < 1) {
        break
      }
      
      repl_ind <- j:(j + degSeq[i] - 1)
      stub_vec[repl_ind] <- rep(i, degSeq[i])
      
      j <- j + degSeq[i]
      i <- i + 1
    }
    
    stub_vec <- stub_vec[!is.na(stub_vec)]
    
    G <- make_empty_graph(length(degSeq), directed = FALSE)
    
    if (simple) {
      wrong_pairs <- list()
    }
    
    for (i in seq(1, sum(degSeq), 2)) {
      
      redo_pair <- TRUE
      while (redo_pair) {
        
        pair_indcs <- sample(which(!is.na(stub_vec)), 2)
        pair <- stub_vec[pair_indcs]
        
        # check if we want a simple graph
        if (simple) {
          
          # if we already stamped this pair as wrong discard it right away
          if (list(sort(pair)) %in% wrong_pairs) {
            next
          }
          
          # if so, check whether adding the current pair violates that
          if (!(get.edge.ids(G, pair) > 0 || pair[1] == pair[2])) {
            # if not, continue on
            redo_pair <- FALSE
          } else {
            wrong_pairs[[toString(i)]] <- sort(pair)
          }
          
          if (length(wrong_pairs) >= sum(!is.na(stub_vec)) ) {
            # sometimes this may get stuck on the last pair if that would
            # violate graph simplicity
            start_over <- TRUE
            k <- k + 1
            
            # break out of the redo loop
            break
          }
          
        } else {
          redo_pair <- FALSE
        }
      }
      
      if (start_over) {
        break
      }
      
      G <- add.edges(G, pair)
      
      stub_vec[pair_indcs] <- NA
    }
    
  }
  
  if (k >= n_tries) {
    warning (paste("Could not find a solution satisfying the simple",
                   "criterion, some nodes may be missing."))
  }
  
  return (G)
}

getAssortativity <- function (G) {
  degs <- rep(0, length(V(G)))
  avg_degs <- rep(0, length(V(G)))
  
  for (v in V(G)) {
    degs[v] <- degree(G, v)
    
    nbs <- neighbors(G, v)
    nb_degs <- degree(G, nbs)
    avg_degs[v] <- sum(nb_degs) / degs[v]
  }
  
  ass <- cor(degs, avg_degs)
  
  if (is.na(ass)) {
    return (0)
  }
  
  return (ass)
}


# Task 1 ------------------------------------------------------------------

assTest <- function (G, repl = 500) {
  checkValid(G)
  
  ass_obs <- getAssortativity(G)
  
  degSeq <- sort(degree(G), decreasing = TRUE)
  
  n_more_extreme <- 0
  
  for (i in 1:repl) {
    print(paste("repl no ", i))
    rnd_G <- stubMatching(degSeq, simple = TRUE)
    ass_perm <- getAssortativity(rnd_G)
    
    if (ass_perm > ass_obs) {
      n_more_extreme <- n_more_extreme + 1
    }
  }
  
  return ((n_more_extreme + 1) / (repl + 1))
}


# Task 2 ------------------------------------------------------------------

nAdjDegrees <- function (G, d1, d2) {
  checkValid(G)
  
  degs <- degree(G)
  
  d1_idcs <- which(degs == d1)
  d2_idcs <- which(degs == d2)
  
  red_adj <- G[d1_idcs, d2_idcs]
  
  return(sum(red_adj))
}

nAdjDegreesTest <- function (G, d1, d2, repl = 500) {
  checkValid(G)
  
  ass_obs <- nAdjDegrees(G, d1, d2)
  
  degSeq <- sort(degree(G), decreasing = TRUE)
  
  n_more_extreme <- 0
  
  for (i in 1:repl) {
    print(paste("repl no ", i))
    rnd_G <- stubMatching(degSeq, simple = TRUE)
    ass_perm <- nAdjDegrees(rnd_G, d1, d2)
    
    if (ass_perm > ass_obs) {
      n_more_extreme <- n_more_extreme + 1
    }
  }
  
  return ((n_more_extreme + 1) / (repl + 1))
}


# Tests -------------------------------------------------------------------

if (sys.nframe() == 0) {
  
  library("igraph")
  
  rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1), directed = FALSE)
  

  # Test - Task 1 ---------------------------------------------------------

  print(assTest(rnd_graph, repl = 10))
  

  # Test - Task 2 ---------------------------------------------------------

  print(nAdjDegreesTest(rnd_graph, round(runif(1, 1, 20)),
                    round(runif(1, 1, 20)), repl = 10))
  
}
