## Put comments here that give an overall description of what your
## functions do
## I have written two functions. One takes the values of the matrix and 
## stores the inverse in cache. The other function first checks cache for inverse
## and gets the value from cache if inverse has already been computed, if not, 
## then it computes the inverse and stores the value in cache. 

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix which is a list containing 
## a function to -
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
		x <<- y
      		m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve function calculates the inverse of the special matrix, but first
## it checks if the inverse has already been calculated. If so, then it gets 
## the inverse from the cache and skips the computation. Otherwise it calculates
## the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
	 m <- x$getinverse()
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
