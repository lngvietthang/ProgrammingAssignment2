## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    
    get <- function() x
    
    setinvert <- function(invertMatrix) invert <<- invertMatrix
    
    getinvert <- function() invert
    
    list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invert <- x$getinvert()
    
    if(!is.null(invert)) {
        message("getting cache data")
        return(invert)
    }
    
    matrix <- x$get()
#     invert <- solve(matrix)
    invert <- tryCatch({
        solve(matrix)    
    }, warning = function(war) {
        print (paste("WARNING: ", war))
        return(NA)
    }, error = function(err) {
        print (paste("ERROR: ", err))
        return(NA)
    }, finally = {
        
    })
    x$setinvert(invert)
    invert
    
}
