## Caching the Inverse of a Matrix
## A pair of functions which stores and caches the inverse of a matrix

## Below is a function which creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                inver <- NULL
                set <- function(y) {
                        x <<- y
                        inver <<- NULL
                }
                get <- function() x
                setInverseMatrix <- function(inversed) inver <<- inverse
                getInverseMatrix <- function() inver
                list(set = set, get = get, 
                     setInverseMatrix = setInverseMatrix, 
                     getInverseMatrix = getInverseMatrix)
}


## Below is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverseMatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inver)
        }
        matrix <- x$get()
        inver <- solve(matrix, ...)
        x$setInverseMatrix(inver)
        inver
}
