## Put comments here that give an overall description of what your
## functions do

##The purpose of the functions in this assignment is to write a pair of functions named 
##"makeCacheMatrix" and cacheSolve" that cache the inverse of a matrix. Matrix inversion is usually 
## a costly computation and therefore can be beneficial to cache the inverse of a matrix rather than computing it repeatedly. 

## Write a short comment describing this function 

## Below, I created "makeCacheMatrix", which is a function that creates a special object that stores the matrix and caches its inverse.
##The "makeCacheMatrix" creates a special "matrix" object that can cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<-NULL
        }
        get <- function() x 
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## The function "cacheSolve" below computes the inverse of the special "matrix" I created above
## called "makeCacheMatrix".  If the inverse has already been calculated, and the matrix has not changed,
## then it should compute the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached result")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}

