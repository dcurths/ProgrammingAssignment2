# These functions are about caching the inverse of a matrix

#  Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	# create matrix
	m <- NULL
 	set <- function(y)	{
    x <<- y
    m <<- NULL
  }

  # compute inverse of matrix
	get <- function() x
 	setmatrix <- function(solve) m <<- solve
 	getmatrix <- function() m
 	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
 	m <- x$getmatrix()
 	# check if matrix is not null and return cached data in this case
 	if(!is.null(m))	{
   	message("getting cached data")
	return(m)
  }

	# compute and return data if cache is empty
  	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}
