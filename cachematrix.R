
#first function sets the value of the matrix
#2nd gets the value of the matrix
#3rd sets the value of the inverse of matrix
#4th gets the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(matrix1) mat <<- matrix1
  
  getmatrix <- function() mat
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

#first sees if inverse of matrix is there
#otherwise it gets the matrix and calculates its inverse
#and then caches its inverse value

cacheSolve <- function(x, ...) {
  mat <- x$getmatrix()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data)
  x$setmatrix(mat)
  mat
}
