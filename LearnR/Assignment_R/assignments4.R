# Lexical scoping
#
# If a variable is not defined inside the function
# the function is looking it up but where is the function actually looking?
a <- 10
f <- function() {
  return(a + 1)
}
f()
# f is defined in the global environment
# Therefore, f looks for a in this environment

# The lookup happens when f is called, not when f is defined
a <- 20
f() # 21, not 11
a <- 10

# In R functions capture the environment in which they are created.
# When functions are called and they have to look up a variable
# they search in the environment in which they are created
# not the one they are called from
f <- function() {
  a <- 12
  g <- function() {
    return(a + 2)
  }
  return(g)
}
h <- f()
h()
# Thus, here 'g' is created within the environment of f
# f is called and the result is assigned to h (basically h == g)
# Then, h is called. With dynamic scoping (e.g. bash) 'a' would be
# looked up where h is called (global environment) --> 10 + 2 = 12.
# But `h()` returns 14 so it looks up within the environment of 'f'
# and finds 'a' with a value of 12.
environment(h)$a # 12

# This mechanism makes it sometimes hard to predict which variable
# a function is using. Therefore, I pass almost all arguments to a
# function and do not rely on the lookup.

# Go environments up until it finds it
# If nothing found the variable is created in the
# global environment
# =================================================
a
f <- function() {
  a <- 1 # This assignment has no effect on the global `a`
}
f()
a
f <- function() {
  a <<- 1 # This assignment changes the value of `a`
}
f()
a
# I wouldn't recommend using it.
#   - function f has side effects in the second version
#   - better to pass everything as argument
#   - if you want to change a variable it is better to use
#     an environment

exists("z") # FALSE
f <- function() {
  z <<- 5
}
f()
z

# One can exploit this mechanism to store a value
create_counter <- function() {
  counter <- 0
  increment <- function() {
    counter <<- counter + 1
    counter
  }
  return(increment)
}
ctr <- create_counter()
ctr() # 1
ctr() # 2
ctr() # 3
ctr() # 4

# Lazy evaluation
# =================================================
# Be careful if the argument is not used in the function
# the expression in f is never evaluated
f <- function(arg) { # arg is never required in f here
  42
}
f(arg <- 3)
exists("arg") # FALSE
