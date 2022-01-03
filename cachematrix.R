## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Write a short comment describing this function
## n: Create an n*n matrix
install.packages("matlib")
library(matlib)
makeCacheMatrix <- function(n) {
  vec <- sample(1:100, n*n, replace=TRUE)
  A <- matrix(vec, nrow=n, ncol=n)
  if (det(A) != 0) {
    AI  <- inv(A)
    list_a_ai <- list(A=A,AI=AI) 
    return(list_a_ai)
  }else{
    print('the matrix is not invertible')
  }
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## inputlist: list contains matrix & inverse matrix form makeCacheMatrix
cacheSolve <- function(inputlist) {
  testA <- inputlist$A
  testAI  <- inputlist$AI
  if (all.equal(inv(testAI),testA,tolerance=1e-6)) {
    inverse_mat <- inputlist$AI
    return(inverse_mat)
  }else{
    print('the matrix has changed')
  }
}
