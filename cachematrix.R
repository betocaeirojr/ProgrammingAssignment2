# R Programming - Assignment 2
## Alberto Caeiro

# Function Lists
## makeCacheMatrix creates a list containing both the original and the inversed matrix
## cacheSolve is responsible for inversing the matrix. In case is already in the cache, no math is done.

# makeCacheMatrix
## Creates an caching structure consisting of both 
## the original matrix, and
## the inversed matrix
## and both setters and getters
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversed <- function(inversed) m <<- inversed
    getinversed <- function() m
    list(set = set, get = get,
        setinversed = setinversed,
        getinversed = getinversed)
}

# cacheSolve 
## is responsible for inversing a matrix
## If the matrix x has already been inversed, it returns the value from the cache
## if not, it computes the inversed matrix, stores it into the cache and returns the solution
cacheSolve <- function(x, ...) {
    ## Check to see if the matrix has already been inversed, 
    ## so I can get the value from the cache
    m <- x$getinversed()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
    
    ## If not, compute the inversed matrix
    data <- x$get()
    m <- solve(data)
    x$setinversed(m)
    m  
}


