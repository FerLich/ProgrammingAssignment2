
## makeCacheMatrix: creates special matrix object from argument and defines functions
## from argument x
## x.set - change value of matrix 
## x.get - return value of matrix
## x.setinverse - calculate and return inverse, if not cached
## x.getinverse - return inverse if cached, NULL if not


## Creates a global matrix object and defines functions to operate on that object
## When setting the value, inv is set to NULL to trigger a new calculation

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

## Define functions and place in list

        get <- function() x
        setinverse <- function(inversa)inv <<- inversa
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Checks if inverse has already been calculated
## If yes, returns the chached result
## If no, calculates and returns the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()

## Check if a previous result exists and return it

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
## Not cached, calculate and return inverse

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
