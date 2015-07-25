## Caching the Inverse of a Matrix:

## When running time consuming computation, it is good to cache the result 
## so that you can look them up later instead of computing them again.
## For example, Matrix Inversion is usually a costly computation especially 
## when running inside a loop. There may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.The two functions below 
## are used to calculate the inverse of a matrix and saves it to the cache 
## such that the next time the user attempts to calculate the matrix inverse, 
## the previously saved value is returned instead of repeating the calculation


## This function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inve


makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set<- function (y) {
        
          x   <<- y
          inv <<- NULL
      }
  
    get<- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse= setInverse,
         getInverse= getInverse)
  
    
}



## This function calcuates the inverse of the special "matrix" created by 
## makeCacheMatrix above. However, it first checks to see if the inverse 
## has already been calculated. If the inverse has already been calculated 
## and the matrix has not changed, then it will directly retrieve 
## the inverse from the cache.Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
    inv <- x$getInverse()
    if(!is.null(inv)){
            message("getting cached data")
            return(inv)
    }
    
    mat<- x$get()
    inv<- solve(mat,...)
    x$setInverse(inv)
    inv
  
}
