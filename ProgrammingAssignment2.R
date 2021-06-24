
makeCacheMatrix  <- function(x = matrix()) {
        
        i <- NULL # Initialise the inverse property
        set <- function(y){ # set the matrix
                x <<- y
                i <<- NULL
        }   
        get <- function(){ # get the matrix
                x
        } 
        setInverse <- function(inverse){ #set the inverse of the matrix
                i <<- inverse
        } 
        getInverse <- function() i # Return a list of the methods
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse() #Return a matrix that is the inverse of 'x'
        #OR return the inverse if it is already set
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        } 
        data <- x$get()  # Get the matrix from our object   
        m <- solve(data,... ) # Calculate the inverse matrix 
        x$setInverse(m) # Set inverse to the object
  
        m  #Return the matrix   
}

