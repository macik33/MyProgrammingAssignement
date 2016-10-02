
##This pair of functions calculate and cache the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<-solve #set matrix inverse
        getinv <- function() inv #get matrix inverse
        list(set=set,get=get,setinv=setinv,getinv=getinv) 
}


## this function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
