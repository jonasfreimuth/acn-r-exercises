
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

matrix_power <- function(A, k) {
  
  # if x is not already a matrix, check if it is coercible to one
  # (raises an error is x is not a matrix and not coercible)
  x <- as.matrix(A)
  
  # check if all dimensions are even equal
  if (!all(dim(A) == dim(A)[1])) {
    
    stop('Matrix is not symmetric!')
    
  }
  
  # matrix to the power of 0 gives its identity matrix
  if (k == 0) {
    
    return(diag(dim(A)[1]))
    
  }
  
  for (i in 0:floor(k / 2)) {
    
    if (i < 1) {
      
      M_sq <- A
      
    } else {
      
      M <- M_sq
      
      M_sq <- M %*% M
      
    }
    
  }
  
  if (k > 1 && k - i * 2 != 0) {
    
    print("here")
    
    M_sq <- M_sq %*% A
    
  }
  
  return(M_sq)
  
}

