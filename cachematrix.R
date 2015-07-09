## Functions are used to cache the inverse of a matrix.

## This Function creates a list containing a function to set and get value of the matrix

makeCacheMatrix <- function(x = matrix()) {
     invr <- NULL
     set <- function(y) {
         x <<- y
         invr <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) invr <<- inverse
     getinverse <- function() invr
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
     invr <- x$getinverse()
     if(!is.null(invr)) {
         message("getting cached data.")
         return(invr)
     }
     data <- x$get()
     invr <- solve(data)
     x$setinverse(invr)
     invr
}

