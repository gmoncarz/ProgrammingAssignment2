# Module that handles a matrix with a cached inverse


# Execution example:
#   > source('cachematrix.R')
#   > test_matrix <- matrix(rnorm(100), 10, 10)
#   > cache_matrix <- makeCacheMatrix(test_matrix)
#     	# This time the inverse should be calculated
#   > cacheSolve(cache_matrix)
#       # This tiem the inverse should be read from cached one
#   > cacheSolve(cache_matrix)


# 
# makeCacheMatrix create a special matrix, which can store the inverse matrix
# into an internal cache avoiding calculate it several times
# 
# makeCacheMatrix returns a list with the following functions:
# 1. set the matix
# 2. Get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
    cache_matrix <- NULL

    set <- function(y)
    {
        x <<- y
        cache_matrix <- NULL
    }
    
    get <- function() x
    set_inverse <- function(inverse) cache_matrix <<- inverse
    get_inverse <- function() cache_matrix

    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse
        )
}


# Function that returns the inverse of the matrix using the cached one.
# If it was not calculated before, this function will perform the math 
# and store it within the cache
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    cache_inverse  <- x$get_inverse()
    if (!is.null(cache_inverse))
    {
        return(cache_inverse)
    }

    # There is nothing on cache. calculate
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)

    inverse
}
