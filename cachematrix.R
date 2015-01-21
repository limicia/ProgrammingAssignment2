## Put comments here that give an overall description of what your
## functions do
## The first function "makeCasheMatrix()" take the input of a square invertable matrix, 
## calculate the inverse of the matrix, and creates a "matrix" object that contains a
## list of the functions defined in the makeVector() function's last "list" statement.

## The second function "cacheSolve" checks the cache and returns the inverse of the 
## matrix from cache when the cache is not empty, and computes the inverse of the matrix
## and sets the inverse matrix in the cache via the first function.
 
## Write a short comment describing this function

## x is the input parameter (matrix) when this fuction is called. The return value of 
## this function is an object rather than a plain matrix.

## The first time this function is called, it creates an environment to store the matrix,
## If the functions is called again, it would get a new environment/space.

## So everytime the function is called, m (which stores the inverse of the matrix x) is
## set to NULL at first place because the "<<-" sign gave x values globally.

## The solve() function gives the inverse of a matrix if it's square and invertible, and
## stores the inverse matrix in the variable m, or makeCacheMatrix()$getinverse.

## The list command at the end of the function returns a named list with the variables 
## (which point to the get/set functions) as values. Thus the names (characters before 
## the equal signs) are arbitrary and are set to the same as the function names just to 
## avoid any confusion.

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


## Write a short comment describing this function

## This function takes a list of functions as its argument (x) instead of a plain matrix
## itself. It is a list of values returned by the makeCacheMatrix() function. The return
## value of the function returns the inverse of the matrix that is returned by 
## x$getinverse().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m         
}
