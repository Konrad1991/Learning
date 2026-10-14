same_object <- function(a, b) {
  tracemem(a) == tracemem(b)
}

a <- 1
b <- 1
same_object(a, b)
same_object(a, a)
b <- a
same_object(a, b)

x <- 1:10
tracemem(x)
x[1] <- 99 # copy

x[] <- 11:20
