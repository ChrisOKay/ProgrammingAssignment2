# The following section create a cache for storing matrixes and their inverse,
# which is used to save computing time by accessing caches solution instead
# of recalculating it.

# The first function, makeCacheMatrix, creates a special "vector",
# which is really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# The second function calculates the inverse of the a given matrix.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data
# and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
