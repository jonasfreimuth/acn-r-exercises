
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
stubMatching <- function(degSeq, simple = TRUE) {
  if (sum(degSeq) %% 2 != 0) {
    stop ("Degree sequence not graphic")
  }
  
  if (sum(degSeq) <= 0) {
    warning ("Resulting graph has no edges")
    
    G <- make_empty_graph(n = length(degSeq))
    return (G)
  }
  
  start_over <- TRUE
  while (start_over) {
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
    
    for (i in seq(1, sum(degSeq), 2)) {
      
      redo_pair <- TRUE
      while (redo_pair) {
        
        pair_indcs <- sample(which(!is.na(stub_vec)), 2)
        pair <- stub_vec[pair_indcs]
        
        # check if we want a simple graph
        if (simple) {
          # if so, check whether adding the current pair violates that
          if (!(get.edge.ids(G, pair) > 0 || pair[1] == pair[2])) {
            # if not, continue on
            redo_pair <- FALSE
          } else {
            if (sum(!is.na(stub_vec)) <= 2) {
              # sometimes this my get stuck on the last pair if that would
              # violate graph simplicity
              start_over <- TRUE
              
              # break out of the redo loop
              break
            }
          }
        } else {
          redo_pair <- FALSE
        }
      }
      
      G <- add.edges(G, pair)
      
      stub_vec[pair_indcs] <- NA
    }
    
  }
  
  return(G)
}


# Task 1 ------------------------------------------------------------------


