
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

# this here is an utter abomination, let this be a lesson to you who reads this:
# KEEP IT SIMPLE STUPID
# # I'm sorry, person reading this. I just had to do everything pre-assigned and 
# # pseudo-memory efficient, taking much longer to code this than I would have 
# # liked
# stubMatching <- function(degSeq, simple = TRUE, n_tries = 50) {
#   if (sum(degSeq) %% 2 != 0) {
#     stop ("Degree sequence not graphic")
#   }
#   
#   if (sum(degSeq) <= 0) {
#     warning ("Resulting graph has no edges")
#     
#     G <- make_empty_graph(n = length(degSeq))
#     return (G)
#   }
#   
#   start_over <- TRUE
#   k <- 1
#   while (start_over && k < n_tries) {
#     start_over <- FALSE
#     
#     stub_vec <- rep(NA, sum(degSeq))
#     
#     i <- 1
#     j <- 1
#     while (i <= length(degSeq)) {
#       if (degSeq[i] < 1) {
#         break
#       }
#       
#       repl_ind <- j:(j + degSeq[i] - 1)
#       stub_vec[repl_ind] <- rep(i, degSeq[i])
#       
#       j <- j + degSeq[i]
#       i <- i + 1
#     }
#     
#     stub_vec <- stub_vec[!is.na(stub_vec)]
#     
#     G <- make_empty_graph(length(degSeq), directed = FALSE)
#     
#     if (simple) {
#       wrong_pairs <- list()
#     }
#     
#     for (i in seq(1, sum(degSeq), 2)) {
#       
#       redo_pair <- TRUE
#       while (redo_pair) {
#         
#         pair_indcs <- sample(which(!is.na(stub_vec)), 2)
#         pair <- stub_vec[pair_indcs]
#         
#         # check if we want a simple graph
#         if (simple) {
#           
#           # if we already stamped this pair as wrong discard it right away
#           if (list(sort(pair)) %in% wrong_pairs) {
#             next
#           }
#           
#           # if so, check whether adding the current pair violates that
#           if (!(get.edge.ids(G, pair) > 0 || pair[1] == pair[2])) {
#             # if not, continue on
#             redo_pair <- FALSE
#           } else {
#             wrong_pairs[[toString(i)]] <- sort(pair)
#           }
#           
#           if (length(wrong_pairs) >= sum(!is.na(stub_vec)) ) {
#             # sometimes this may get stuck on the last pair if that would
#             # violate graph simplicity
#             start_over <- TRUE
#             k <- k + 1
#             
#             # break out of the redo loop
#             break
#           }
#           
#         } else {
#           redo_pair <- FALSE
#         }
#       }
#       
#       if (start_over) {
#         break
#       }
#       
#       G <- add.edges(G, pair)
#       
#       stub_vec[pair_indcs] <- NA
#     }
#     
#   }
#   
#   if (k >= n_tries) {
#     warning (paste("Could not find a solution satisfying the simple",
#                    "criterion, some nodes may be missing."))
#   }
#   
#   return (G)
# }

# actual working thing
# done after pedros idea of matching half of the stub_vector to the other half
stubMatching <- function(degSeq, simple = TRUE, n_tries = 100) {
  
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
    
    G <- make_empty_graph(n = length(degSeq), directed = FALSE)
    return (G)
  }
  
  stub_vec <- rep(0, sum(degSeq))
  
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
  
  stub_inds <- 1:length(stub_vec)
  
  redo <- TRUE
  k <- 1
  while (redo && k <= n_tries) {
    redo <- FALSE
    
    rnd_half <- sample(stub_inds, length(stub_vec) / 2)
    other_half <- setdiff(stub_inds, rnd_half)
    
    edges <- as.vector(rbind(stub_vec[rnd_half], stub_vec[other_half]))
    
    G <- make_graph(n = length(degSeq), edges = edges, directed = FALSE)
    
    if (!is.simple(G) && simple) {
      redo <- TRUE
      k <- k + 1
    } 
    
  }
  
  if (k >= n_tries) {
    warning (paste("Could not find a solution satisfying the simple",
                   "criterion."))
  }
  
  print(is_simple(G))
  
  return (G)
}



library(igraph)

opar <- par(mfrow = c(1, 2))

rnd_graph <- sample_gnp(runif(1, 1, 20), runif(1), directed = FALSE)
plot(rnd_graph)

rnd_rnd_graph <- stubMatching(sort(degree(rnd_graph), decreasing = TRUE))
plot(rnd_rnd_graph)

par(opar)
