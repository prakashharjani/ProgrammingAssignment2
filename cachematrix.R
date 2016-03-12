## makeCacheMatrix will allow the following features:
#set will assign the matrix argument to storage
#get will retreive the matrix stored
#getinv will retrieve the inverse of the matrix stored
#setinv will store the matrix you calculate 
#note: setinv does not calculate the inverse, it is a placeholder to store it.

##First assign makeCacheMatrix to a variable to store the matrix and create the caches
##Then run cacheSolve on the varible to calculate the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        m<- NULL
        set <-function(y){
                x <<- y
                m<<- NULL
        }
        get <- function() x
        
        setinv<- function(sol) m<<- sol
        getinv <- function() m
        list(set = set, get= get, setinv =setinv, getinv= getinv)
}


## cacheSolve will accept a matrix as a argument and return it's inverse
## on multiple calls, if the matrix is not changed, it will return the cached inverse

cacheSolve <- function(x, ...) {
        m <-x$getinv()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        data <-x$get()
        m <- solve(data,...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}