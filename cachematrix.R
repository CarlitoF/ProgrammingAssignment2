
makeCacheMatrix <- function(x = matrix()) {
        m <-NULL  #m will be a free variable
        set <- function(y){
                x<<-y  #function which assigns local value (y) to x
                m<<-NULL  #m is reinitialized
        }
        get<- function() x  #original input value (x)
        setinverse <-function(solve) m<<-solve #setinverse performs solve() function on m
        getinverse <-function() m #getinverse swaps x and m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the special matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated then the cacheSolve should return the inverse from the cache

cacheSolve <- function(x, ...) {
        m<-x$getinverse()       #free variable (m) assigned to the getinverse() value of the matrix
        if(!is.null(m)){
                message("getting cached data") #if m has value then it is returned
                return(m)
        }
        data<-x$get()           #local variable data is assigned the original matrix value
        m<-solve(data, ...)     #free varialble, m, is assigned inverse value through solve
        x$setinverse(m)
        m
}