## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        #Our inverse matrix will be reset to null every time makeCacheMatrix is called        
        inverse_mat <- NULL
        
        #We can set the matrix to a different matrix if we want to. We would need to
        #reset the inverse to null to avoid calling the wrong solution. 
        
        set <- function(y) {
                x <<- y
                inverse_mat <<- NULL
        }

        #Returns the value of the initial vector x
        get <- function() { 
                x 
        }   
        
        #Accessed by cachesolve() during the first call and stores the result using
        #superassignment
        setsolve <- function(solve)  { 
                inverse_mat <<- solve
        }
        
        #This will return the cached value to cachesolve() after the first call
        getsolve <- function() { 
                inverse_mat
        }
        
        #We want to store the matrix and its functions in a list for 
        #easy access for when we run cacheSolve
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)     

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #First we access the object 'x' and gets the value of the inverse
        inverse_mat <- x$getsolve()
        
        #If the inverse has been cached, then we return it...
        if(!is.null(inverse_mat)) {
                message("getting cached data")
                return(inverse_mat)
        }
        
        #Otherwise we get the inverse, by first getting the initial matrix...
        data <- x$get()

        #...then finding the inverse of the matrix....
        inverse_mat <- solve(data, ...)
        #...and cache the result
        x$setsolve(inverse_mat)
        #...and print out the result
        inverse_mat
}
