## This functions are meant to make a cache copy of the invert matrix of x


## This matrix creates the matrix list that is going to save the invert matrix

makeCacheMatrix <- function(x = matrix()) {
    ma<-NULL ## set to NULL the initializing matrix
    set<-function(y){  ## set the cache values to null
        x<<-y
        ma<<-NULL
    }
    get<-function()x
    setmatrix<-function(matrix) ma<<-matrix  ## this three functions get us the values
    getmatrix<-function() ma
    list( set = set , 
          get=get, 
          setmatrix=setmatrix,
          getmatrix=getmatrix)
    
}


## This functions difines the inverse matrix of the matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' x being a list returned by makeCacheMatrix
    ma<- x$getmatrix()
    if (!is.null(ma)){  ## if it's in the cache does this
        message("getting cache data")
        return(ma)
    }
    data<-x$get() ## if not creates a new one on the cache
    ma<-solve(data)
    x$setmatrix(ma)
    ma
}
