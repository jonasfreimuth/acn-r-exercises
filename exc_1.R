
# Task 1: Colwise matrix sums ---------------------------------------------

matrix_sum <- function(A) {
  
  # if x is not already a matrix, check if it is coercible to one
  # (raises an error is x is not a matrix and not coercible)
  x <- as.matrix(A)
  
  # apply sum function to all columns
  sums <- apply(A, 2, sum)
  
  return(sums)
  
}

# demonstrate function works

A <- matrix(rnorm(81), ncol = 9)

res_func <- matrix_sum(A)

res_rfunc <- colSums(A)

all(res_func == res_rfunc)

# Task 2: Check if matrix is symmetric ------------------------------------

matrix_is_symmetric <- function(A) {
  
  # if x is not already a matrix, check if it is coercible to one
  # (raises an error is x is not a matrix and not coercible)
  x <- as.matrix(A)
  
  # check if all dimensions are even equal
  if (!all(dim(A) == dim(A)[1])) {
    
    return(FALSE)
    
  }
  
  # check if the upper triangular is the same as the lower triangular
  is_sym <- all(Matrix::tril(A) == t(as.matrix(Matrix::triu(A))))
  
  return(is_sym)
  
}

# demonstrate function works

rand_res_func <- matrix_is_symmetric(A)

# construct symmetric matrix from A and check that
sym_res_func <- matrix_is_symmetric(Matrix::forceSymmetric(A))

rand_res_rfunc <- Matrix::isSymmetric(A)
sym_res_rfunc <- Matrix::isSymmetric(Matrix::forceSymmetric(A))

all(rand_res_func == rand_res_rfunc, sym_res_func == sym_res_rfunc)


# Matrix to a power -------------------------------------------------------

# recursive square and multiply algorithm for matrices
rec_mat_power <- function(A, k) {
  
  if (k == 1) {
    return(A)
    
  } else {
    A_sq <- rec_mat_power(A, floor(k/2))
    
    if (k %% 2 == 0) {
      return(A_sq %*% A_sq)
      
    } else {
      return(A_sq %*% A_sq * A)
      
    }
    
  }
  
}

# wrapper function to check prerequisites
matrix_power <- function(A, k) {
  
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
  
  A <- rec_mat_power(A, k)
  
  print(A)
  
  return(A)
  
}

