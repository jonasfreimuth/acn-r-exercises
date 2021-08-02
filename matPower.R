matPower <- function(M, l) {
  if (l == 0) {
    return(diag(nrow(M)))
  }
  
  k <- floor(l / 2)
  B <- M
  for (i in 1:k) {
    B <- B %*% B
  }
  
  if (l %% 2 != 0) {
    B <- B %*% M
  }
  
  return(B)
}

if (sys.nframe() == 0) {
  M <- matrix(c(3, 2, 2, 0), 2, 2)
  
  print(M)
  print(matPower(M, 4))
}
