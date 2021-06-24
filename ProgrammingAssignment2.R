
makeCacheMatrix  <- function(x = matrix()) {
        
        i <- NULL # Initialise the inverse property
        
        set <- function(y){
                x <<- y
                i <<- NULL
        } # Method to set the matrix
        
        get <- function(){
                x
        } # Method to get the matrix

        setInverse <- function(inverse){
                i <<- inverse
        } # Method to set the inverse of the matrix
        
        getInverse <- function() i # Return a list of the methods
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse() ## Return a matrix that is the inverse of 'x'
        
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        } # Return the inverse if it is already set
        
        
        data <- x$get()  # Get the matrix from our object
        
        
        m <- solve(data,... ) # Calculate the inverse matrix 
        
        x$setInverse(m) # Set inverse to the object
        
        m  #Return the matrix
        
}



x <- makeCacheMatrix(matrix(rnorm(9) , 3 , 3)) 
#create a matrix as an argument to the new function


cacheSolve(x) #then pass the result of a makeCacheMatrix call to cacheSolve

cacheSolve(x) #it will return the cached value
