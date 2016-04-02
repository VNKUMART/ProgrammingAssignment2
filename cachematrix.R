## makeCacheMatrix function - creates a special matrix to store and matrix 
## object which can cache its inverse. Supports functions as following.
## 1. Set the value of the Matrix - setMatrix
## 2. Get the value of the Matrix - getMatrix
## 3. Inverse the Matrix to get the cached value - cacheInverse
## 4. Inverse the Matrix to get invere value - getInverse 

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize Cache and set to NULL
    
    cache = NULL
    
    ## 1. Set the value of the Matrix
    
    setMatrix <- function(newValue) { 
        x <<- newValue 
        
        ## Re-Initialize Cache and re-set it to NULL
        cache <<- NULL 
    } 
    
    ## 2. Get the value of the Matrix - getMatrix
    
    getMatrix <- function() { 
        x 
    }     
    
    ## 3. Inverse the Matrix to get the cached value - cacheInverse
    
    cacheInverse <- function(solve) { 
        cache <<- solve 
    } 
    
    ## 4. Inverse the Matrix to get invere value - getInverse 
    
    getInverse <- function() { 
        cache 
    } 
    
    # return a list. Each named element of the list is a function 
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         cacheInverse = cacheInverse, 
         getInverse = getInverse) 
}


## get the cached value 

cacheSolve <- function(x, ...) {
    
    ## Inverse of 'x'
    
    inverse <- y$getInverse() 
    
    ## Return cached value
    
    if(!is.null(inverse)) { 
        message("getting cached data") 
        return(inverse) 
    } 
    
    ## Caclulate the inverse 
    
    data <- y$getMatrix() 
    inverse <- solve(data) 
    y$cacheInverse(inverse) 
    
    ## return the inverse 
    inverse 
}