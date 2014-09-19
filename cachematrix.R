## The 2 below functions help to cache the value of the inverse of a matrix,
## so that we do not need to recalculate the inverse of the same matrix anytime
## we need it again.

## The makeCacheMatrix function takes as input a numeric matrix. It returns a
## list that contains 4 elements. Each element in the list is a function. 

## The functions returned in the list are -
## 1. "set" - which can be used to set the value of the matrix in makeCacheMatrix.
## 2. "get" - which returns the value of the matrix that was set in  
## makeCacheMatrix environment.
## 3. "setinverse" - which takes the inverse of a matrix and assigns it to a 
## variable in makeCacheMatrix.
## 4. "getinverse" - which returns the cached value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix <<- inverse
    getinverse <- function() matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function that gets the inverse of a matrix making use of 
## makeCacheMatrix function. It firsts checks if the value returned by the
## makeCacheMatrix is null or not. If not null, it returns it. If the value is
## null, it calculates the inverse of the matrix stored in makeCacheMatrix
## and saves the inverse by calling setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}