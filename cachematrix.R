# 第一个函数makeCacheMatrix创建了一个特殊的“向量”，
# 实际上它返回具有以下用途函数的列表
#   1. 设置矩阵的值
#   2. 获取矩阵的值
#   3. 设置矩阵的逆
#   4. 获取矩阵的逆

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set=set, get=get, 
         setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve函数计算了上述函数创建的特殊“矩阵”的逆。
# 但是，它会首先查看是否已经计算过了该矩阵的逆。
# 如果是这种情况，那么它会从缓存中获取该矩阵的逆，并跳过计算。
# 否则，它会计算矩阵的逆，并通过setinverse函数在缓存中设置平均值。

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
