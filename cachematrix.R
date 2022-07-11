## Caching the Inverse of a Matrix
## Set the input x as a matrix

## Set the output i as a NULL

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <-function (y) {
                x<<-y
                i<<-NULL
        }
        get <-function ()x
        setinverse <- function (inverse) i <<-inverse
        getinverse <-function ()i
        list (set=set, get=get,
              setinverse=setinverse,
              getinverse=getinverse)
        
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        
        if(!is.null(i)){
                message ("getting inverse of the matrix")
                return (i)
        }
        
        data <- x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
