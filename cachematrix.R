## The purpose of the below functions makeCacheMatrix and CacheSolve enable to 
## cache Inverse Matrix. Since Matrix computetation is costly there is great
## benefit to cache it and retrieve it rather than computing it repeatedly. 


## makeCacheMatrix fuction creates a special Matrix with the following list of 
## functions
## They are 1. Set Matrix  2. Get Matrix 3. setInverseMatrix 4.getInverseMatrix 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) s <<- solve
        getInverseMatrix <- function() s
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## The following function cacheSolve calculates the inverse of special matrix
## ,using the function solve, created in makecacheMatrix. It first checks 
## if inverse has already been calculated. If so, it gets from cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets it in the cache via setInverseMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverseMatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data) 
        x$setInverseMatrix(s)
        s      
}
