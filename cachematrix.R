##  The purpose of this assignment was to highlight the use of lexical scoping
##  in R, and the concept of closures. Through the use of the double arrow 
##  assignment operator, we can define variables in the parent environment, such 
##  that the calculated values can be cached for use outside the function rather 
##  than needing to recalculate each time.

##  makeCacheMatrix: This function is responsible for initializing, storing, and  
##                   updating variables in the parent environment, defining the 
##                   get/set functions, and then storing them as named elements  
##                   in a list, allowing us to use the $ operator to call them.  
                   
makeCacheMatrix <- function(x = matrix()) {     
        
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) c <<- inverse
        get_inverse <- function() c
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve: This function checks the stored value of the inverse matrix, 
##             and if necessary, calculates a new value. If already available,
##             it returns the cached value to the parent environment and informs 
##             the user.

cacheSolve <- function(x, ...) {                
        
        c <- x$get_inverse()
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$set_inverse(c)
        c
}

# Test Function

B <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)

cacheSolve

