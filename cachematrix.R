## function to calculate the inverse of a matrix
## inverse is only calculated if a squared matrix is passed
## and when the inverse hasn't been calculated yet

## creates the matrix and caches inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    ## checks if y is an invertible matrix
    if(dim(x)[1] == dim(x)[2]) {
    inv <- NULL;
    set <- function(y) {
        x <<- y;
        inv <<- NULL;
    }
    get <- function() {
        x;
    }
    setInv <- function(inverse) {
        inv <<- inverse;
    }
    getInv <- function() {
        inv;
    }
    ## 
    tmp <<- x;
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    }
    else {
        message("Only square matrices allowed!")
    }
}


## calculates the inverse of a squared matrix only when
## a matrix hasn't been changed
## b inverse of matrix hasn't been calculated yet and

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sol <- x$getInv();
    if(!identical(tmp, x$get())) {
        message("Matrix has been changed!");
    }
    else if(!is.null(sol)) {
        message("Getting cached data...");
        return(sol);
    }
    else {
        data <- x$get();
        sol <- solve(data);
        x$setInv(sol);
        sol;
    }
}
