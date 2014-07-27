## This function creates a list that contains a function to cache to inverse of
## a matrix

## makeCacheMatrix function will:
## Set values of a matrix
## Retrieve matrix values
## Set values of the inverse matrix
## Retireve values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # store matrix values in im
    im <- NULL
    
    # Set matrix
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    # Retrieve matrix
    ret <- function() x
    
    # Set inverse of matrix
    setinverse<- function(inverse) im <<-inverse
    
    # Return inverse
    returninverse <- function() im
    
    # List martix with functions
    list(set = set, ret = ret,
         setinverse = setinverse,
         returninverse = returninverse)
}


## Return a matrix that is the inverse of 'x'

## cacheSolve wil compute the inverse of a pre-calculated matrix or return 
## the cached inverse

cacheSolve <- function(x, ...) {
    im <- x$returninverse()
    
    # If inverse in calculated, return it
    if (!is.null(im)) {
        message("return cached inverse matrix")
        return(im)
    
    #Else calculate inverse cache and return
    } else {
        im <- solve(x$ret())
        x$setinverse(im)
        return(im)
    }
}

