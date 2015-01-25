makeCacheMatrix <- function(x = matrix()) { ## This function creates a special "matrix" object that can cache its inverse.
        m <- NULL
        set <- function(y) {                ## Create a function to save a matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                 ## Create a function to get the matrix that has been set
        setinverse <- function(solve) {     ## Create a function to solve the matrix
                m <<- solve
        }
        getinverse <- function() m          ## Create a function to get the inverse of the matrix
        list(set = set, get = get,          ## Create a list to store the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {            ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
        m <- x$getinverse()                 ## Get the inverse matrix, if it has been already computed
        if(!is.null(m)) {                   ## Return the value of the cached matrix, if it isn't NULL
                message("getting cached data")
                return(m)
        }
        data <- x$get()                     ## Get a matrix
        m <- solve(data, ...)               ## Solve the inverse matrix
        x$setinverse(m)                     ## Set the inverse matrix
        m                                   ## return the inverse matrix
}
