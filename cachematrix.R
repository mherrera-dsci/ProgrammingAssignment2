## Make a inverse of matrix 'x'
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

## Return a matrix that is the inverse of 'x': If the matrix has been resolved, it is not necessary to recalculate
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                print("getting cached data")
                return(m)
        } else {
                print("xxxxxxxxx")
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## For testing
# > test <- matrix(1:4, 2,2)
# > test
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m <- makeCacheMatrix(test)
# > s <- cacheSolve(m)
# [1] "xxxxxxxxx"
# > s <- cacheSolve(m)
# getting cached data
# [1] "getting cached data"
# > s <- cacheSolve(m)
# getting cached data
# [1] "getting cached data"
# > s
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# >
# }