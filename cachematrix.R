## cacheMatrix.R 
## Version 1.0 22 Aug 2014
## Functions in this file:
## 1) makeCacheMatrix(mat) -- creates a cacheMatrix "pseudo object" 
##                          that stores the values of the input matrix mat
##   On call via  myCacheMat <- makeCacheMatrix( inputMatrix ) it returns a list of pseudo methods 
##      myCacheMat$get() -- get the contents of original matrix
##      myCacheMat$set(newMat <- matrix()) -- set the contents of the pseudo object
##          to the values of the new matrix and initialize
##      myCacheMat$getInverse() -- retrieve the inverse of the matrix
##          NOTE: this will only be availble after an initial call to cacheSolve(myCacheMat).
##              after creation by makeCacheMatrix(mat). It will return NULL if
##              myCacheMat$getInverse() is invoked after creation by makeCacheMatrix(mat), but before
##              invocation of cacheSolve(myCacheMat)
## 2) cacheSolve(cacheMatrixPseudoObject) -- calculate or retrive a cached matrix inverse
## 
## 3) testCacheMatrix() -- a function to exercise the above functions and pseudo methods
## makeCacheMatrix() --  create a 'square matrix object' that can cache its own inverse via cacheSolve()
##
## To exercise the test, in the R console:
#  Change working directory to same directory as cacheMatrix.R
## Then type the following 2 commands at the prompt (shown here as >):
## > source cacheMatrix.R 
## > testCacheMatrix()
makeCacheMatrix <- function(mat = matrix()) {
    minv <- NULL
    set <- function(foreignMatrix) {
        mat <<- foreignMatrix
        minv <<- NULL
    }
    get <- function(){ mat}
    setInverse <- function(newInverse) {minv <<- newInverse}
    getInverse <- function(){
        if (is.null(minv)) {
            #             message( ' no inverse available  maybe use v$cachSolve() first')
        } 
        return(minv)
    }
    return(list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse))
}

## cacheSolve() -- calculate or remember cached matrix inverse
cacheSolve <- function(matCachePseudoObject, ...) {
    minv <- matCachePseudoObject$getInverse()
    if(!is.null(minv)) {
        message("getting cached inverse")
        return(minv)
    }
    tmat <- matCachePseudoObject$get()
    tminv <- solve(tmat)
    matCachePseudoObject$setInverse(tminv)
    return(tminv)
}   

testCacheMatrix <- function (){
    testmat <- matrix(c(10,2.96, 2.96, 1.97),2,2)
    message('Contents of testmat -- test input matrix:')
    print(testmat)
    inv_testmat <- solve(testmat)
    message('Contents of inv_testmat calcuted directly as' )
    message('"inv_testmat <- solve(testmat)"')
    message('Printing via "print(inv_testmat)"')
    print(inv_testmat)
    
    message('New matrix mmtest created as "mmtest  <-  makeCacheMatrix(testmat)"')
    mmtest  <-  makeCacheMatrix(testmat) 
    message( 'Printing values of stored matrix via "print(mmtest$get())" ')
    print(mmtest$get())
    
    message('Contents of inv_testmat calcuted  using cacheSolve via:' )
    message('"inv_mmtest <- cacheSolve(mmtest)"')
    inv_mmtest <- cacheSolve(mmtest)
    print(inv_testmat)

    
    message('Contents of inv_mmtest_a calcuted via use of getInverse()' )
    message('"inv_mmtest_a <-  mmtest$get(inverse)"')
    inv_testmat
    inv_mmtest <- cacheSolve(mmtest)
    print(inv_testmat)
    
    message ('Original values of elements of testmat calculated as')
    message( '"testmat_copy <-  mmtest$get() ; print(testmat_copy)"')
    testmat_copy <- mmtest$get()
    print(testmat_copy)
    message(' Another testmatrix called testmat2')
    testmat2 <- matrix(c(10.965143, 3.301190,  3.301190,  2.151795),2,2)
    print(testmat2)
    message( 'Changing the contents of mmtest via:')
    message( '"mmtest$set(testmat2)"')
    mmtest$set(testmat2)
    message ('Printing changed contents of closure mmtest and crudely displayed via "print(mmtest$get())')
    mmtest$get()
    message('Note that mmtest$getInverse() will return NULL until we re-invoke cacheSolve(mmtest)')
    message('Executing "print(mmtest$getInverse())"')
    print(mmtest$getInverse())
}
# testCacheMatrix()

