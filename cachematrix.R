## This function is used to cache the Inverse of a Matrix

## Creates a special matrix pbject that can cashe its revrese

makeCacheMatrix <- function(x = matrix()) {

	i<- NULL
	
	set <- function(matrix){
		m <<- matrix
		i <<- NULL
	}
	
	get <- function(){
		m
}	
	setInverse <- function(inverse){
   		i <<- inverse
   }
   
   getInverse <- function(){
   		
   		i
   }
   
   list(set = set,get = get, setInverse = serInverse, getInverse= getInverse)
}

## the "cacheSolve" function will retrive the inverse from the cache if the inverse has been computed

cacheSolve <- function(x, ...) {
	
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if (!is.null(m)) {
        			message("getting cached data")
        			return(m)
        }
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        m
}
