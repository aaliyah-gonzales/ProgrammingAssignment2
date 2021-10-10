

## The goal of this experiment is to develop two functions that cache the inverse of a matrix, "makeCacheMatrix" and "cacheSolve." 

## make CacheMatrix is a function that produces a special "matrix" object for the input that can cache its inverse (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function (y){
            x <<- y
            inv <<- NULL
      }
      get <- function()  {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The inverse of the special "matrix" supplied by makeCacheMatrix above is computed by cacheSolve. 

## The cachesolve should get the inverse from the cache if the inverse has previously been calculated (and the matrix has not changed).

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
        	inv <- x$getInverse()
     	if(!is.null(inv)){
             message("fetching cached data")
             return(inv)
	}
      	mat <- x$get()
      	inv <- solve(mat, ...)
      	x$setInverse(inv)
      	inv
}

