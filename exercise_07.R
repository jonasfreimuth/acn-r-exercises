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

library(igraph)

opar <- par(mfrow = c(1, 2))

rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1), directed = FALSE)
plot(rnd_graph)

rnd_rnd_graph <- stubMatching(sort(degree(rnd_graph), decreasing = TRUE))
plot(rnd_rnd_graph)

par(opar)
