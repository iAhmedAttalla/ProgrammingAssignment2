## First of all, We practice to caching matrix such as the example of caching
## vector. Make makematrix function to set, get, setinverse, and getinverse
## the matrix x so that we can do other function to check the caching
## this matrix and put the result if x was not a null but null, we will go to
## the data by get() function and store it variable to do the inverse function
##  on and return in the last by typing the name of it.

## This function takes a matrix and then cache it as the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set<- function(y){
                x <<- y
                inver <<- NULL
        }
        
        get<- function() x
        
        setinverse<- function(inverse) inver <<- inverse
        getinverse<- function() inver
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function checks if the above function caches the invers matrix correctly
## or not, if not, we will get and inverse it and then return the value.
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        
        if(!is.null(inver)){
                message("Getting Cache Matrix correctly.")
                return(inver)
        }
        
        getdata <- x$get()
        inver <- solve (getdata)
        x$setinverse(inver)
        inver
        ## Return a matrix that is the inverse of 'x'
}
