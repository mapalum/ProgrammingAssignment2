## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


	makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(0, nrow=nrow(x),ncol=ncol(x))
        set <- function(y) {
                x <<- y
                m <<- matrix(0, nrow=nrow(x),ncol=ncol(x))
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
	}

	cacheSolve <- function(x, ...) {
        m <- x$getSolve()
	I <- diag(c(1),nrow=nrow(x),ncol=ncol(x))
        if (all.equal(x %*% m , I)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
	}
