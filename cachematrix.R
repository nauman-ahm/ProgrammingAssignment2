## There are three functions, makeCacheMatrix and cacheSolve are do the same operations to matrix as
## the examples provided did to vectors. And inversecalculator simply calculates and returns the inverse of the provided matrix.

## makeCacheMatrix makes a list of 4 functions get, set, getinverse, and setinverse. 

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  set <- function(y)
  { 
    data <<- y
    inverse <<- NULL
    }
  get <- function()
  {
    return(data)
  }
  setinverse <- function(i)
  {
    inverse<<- i
  }
  getinverse <- function()
  {
    return(inverse)
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve takes input as a list of functions initialized from makeCacheMatrix, checks if there
## is any data in cache, if there is it will print it out, if not it will calculate the inverse and print it out.

cacheSolve <- function(x, ...) {
        temp <- x$getinverse()
  if (!is.null(temp))
  {
      message("getting cached data")
      return (temp)
  }
  else
  {
    temp <- x$get()
    inverse <- inversecalculator(temp)
    x$setinverse(inverse)
    inverse
  }
}

## inversecalculator takes a matrix input and returns the inverse of that matrix, it is used by cacheSolve

inversecalculator <- function (data = matrix)
{
  n = ncol(data)
  cofactors_matrix <- matrix( nrow = n, ncol = n)
  adjugate_matrix <- matrix (nrow = n, ncol = n)
  if (nrow(data) == ncol(data))
  {
    for (i in 1: ncol(data))
      for (j in 1: nrow(data))
      {
        if ((i+j) %% 2 == 0) {cofactors_matrix[i,j] <- det(data[-i,-j])}
        if ((i+j) %% 2 == 1) {cofactors_matrix[i,j] <- (det(data[-i,-j]))*-1}
      }
    for (i in 1: ncol(data))
      for (j in 1: nrow(data))
      {
      adjugate_matrix[i,j] <- cofactors_matrix[j,i]
      }
  }
 inverse_matrix <- (1/det(data))*adjugate_matrix
 return (inverse_matrix)
}
