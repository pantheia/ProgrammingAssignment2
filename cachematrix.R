## _makeCacheMatrix_ creates an instance of the object with four functions:
## set, get, setinv, getinv
## and also saves those functions to a list so they can be called with "object%set" etc
## _cacheSolve_ uses the solve() function on the input, it can be applied
## to an instance of the makeCacheMatrix object

## make an instance of a makeCacheMatrix object:
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) #define a matrix
## myMatrix_object <- makeCacheMatrix(m1)
## after initialization, a new matrix can bet set by:
## n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## myMatrix_object$set(n2)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverseM) {
                inv <<- inverseM
        }
        getinv <- function() {
                inv
        }
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## computes the solve() of the input object, unless
## that value has already been calculated, in which
## case that inverse is pulled from memory
## cacheSolve(myMatrix_object)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        V <- x$getinv()
        if (!is.null(V) ) {
                
                message("getting cached data")
                return(V)
        }
        data <- x$get()
        V <- solve(data, ...)
        x$setinv(V)
        V
        
}
