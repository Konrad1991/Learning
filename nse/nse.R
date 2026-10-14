f <- function(arg1, arg2) {
  substitute( # This substitute builds the code
    fun(x, y), list( # list contains the argument to replace x
      x = substitute(arg1), # This substitute prevents evaluation
      y = substitute(arg2) # This substitute prevents evaluation
    )
  )
}

fun <- function(a, b) a + b
a <- 1:3
b <- 4:6
code <- f(a, b)
code
eval(code)
