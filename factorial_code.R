# Functional Programming
## Part 1 Factorial Function

# 1. Factorial Loop
# This program implements a for loop 
loopy_factorial <- function(n){
    stopifnot(n >= 0) # prevents entry of a negative number which will prevent an infinite loop
    number <- 1
    for(i in n:1){
        number <- i*number
    }
    if(number == 0){
        1
    } else {
        number
    }
}

# 2. Factorial Reduce
# This uses the reduce function
reducy_factorial <- function(n){
    stopifnot(n >= 0)
    if(n == 0){
        1
    } else {
        reduce(n:1, function(x,y){
            x*y  # x*y in reduce will actually provide a factorial of length n
        })
    }
}

# 3. Factorial Func
# This uses a recursive function, reverting back to the last term calculated
funky_factorial <- function(n){
    stopifnot(n >= 0)
    if(n == 0){
        1
    } else if (n == 1){
        1
    } else {
        n*funky_factorial(n-1) # this is the recursion line, multiplying n by the last multiple
    }
}

# 4. Factorial Mem
# This is applying memoization to the last function, the recursive function factorial, but it can easily be applied to any of the other functions by replacing the term in the line noted
memoizer <- function(){    
    res <- 1
    memoizy_factorial <- function(n){
        stopifnot(n >= 0)
        if (n == 0) return(1)
        if (n == 1) return (1)
        
        if (!is.na(res[n])) return(res[n])
        
        res[n] <<- n * funky_factorial(n-1)  #replace the function to be used here
        res[n]
    }
    memoizy_factorial
} 
memoizy_factorial <- memoizer()




#5. Benchmarking the functions with microbenchmark
bm <- microbenchmark(loopy_factorial(20), reducy_factorial(20), funky_factorial(20), memoizy_factorial(20))
print(bm)
