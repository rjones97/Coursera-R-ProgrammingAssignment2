## cachematrix.R
## defines two functions:
##          makeCacheMatrix(x): stores x, and establishes functions for caching the inverse of x
##          cacheSolve(x):  returns the inverse of x (defined in makeCacheMatrix)
##                          cached (if available), or calculates inverse to return


## makeCacheMatrix(x)
## Input:
##      x: matrix
## Function:
##  This function creates a special "matrix object" that can cache its inverse.
##      inv: variable intended to store the inverse of the matrix x
##      set(a): allows an override of the matrix x; clears any existing cache values
##      get(): returns matrix x
##      setInverse(b): stores b; note: assumes this is the inverse of matrix x
##      getInverse(): retrieves and returns inv
## Returns: list of available functions: get, set, getInverse, setInverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL    # initialize "inv": placeholder for the inverse of "x"
    set <- function(a){
        message("Explicit Matrix over-write requested.  
                Clearing currently cached values.")
        inv <<- NULL
        x <<- a    
        return()
    }

    get <- function(){
        x          # return the matrix "x" that was provided 
    }
    
    setInverse <- function(b){
        # assuming the caller has correctly calculated the inverse of "x"
        # no validation that solve(x) == b
        inv <<- b  
    }

    getInverse <- function(){
        inv        # return the "inv" value stored in makeCacheMatrix
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(x)
## Input: 
##      x: output of makeCacheMatrix()
## Function: 
##      Using 'x', check if the inverse has already been calculated 
##      If already calculated, return cached value
##      Else, evaluate and return inverse of matrix in 'x' 
## Return:
##      inverse of matrix
cacheSolve <- function(x, ...) {
  
    if(!is.null( x$getInverse() )){
        message("Inverse already calculated. Retrieving cached value.")
        
        return(x$getInverse())
    } else{
        message("Calculating Inverse ...")
        x$setInverse( solve(x$get(), ...) )

       return( x$getInverse() )
    }
}
