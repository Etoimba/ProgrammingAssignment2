## Task: ProgrammingAssignment 2
## main idea to cache inversed matrix, so we dont need to inverse it again
## this will save us a lot of time (for big ones)

## makeCacheMatrix  returns list of functions to cache inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve - if we have cached inversed matrix than it will be returned,
## but if there is no cached matrix it will inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## i dont check if matrix invertible or not - due to task
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
