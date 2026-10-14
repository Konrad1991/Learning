# Environments
# ===============================================
f <- function() {
  env <- environment()
  print(env)
  print(parent.env(env))
  e <- new.env()
  e
}
e <- f()
environment(f)
parent.env(e)

# get something
get("c")
get("e")
get("f")

# lets nest some environments
# ===============================================
outer <- new.env()
outer$a <- 100
inner <- new.env(parent = outer)
inner$b <- 10
inner_inner <- new.env(parent = inner)
inner_inner$c <- 34
inner_inner_inner <- new.env(parent = inner_inner)
inner_inner_inner$d <- 12

get("a", envir = inner_inner_inner)
# But what happens here actually
# ===============================================
get_own <- function(what, env) {
  if (identical(env, emptyenv())) {
    stop(sprintf("Cannot find object '%s'", what))
  }
  if (exists(what, envir = env, inherits = FALSE)) {
    return(env[[what]])
  }
  get_own(what, parent.env(env))
}
get_own("a", inner_inner_inner)
get_own("not_defined", inner_inner_inner)

# Environments are cool to modify something by reference
# ===============================================
e <- new.env(parent = emptyenv()) # good style to set the parent to emptyenv. Especially for just storing stuff
e$large_data <- 1:1e6
modify <- function(env) {
  env$large_data[] <- runif(1:1e6)
}
modify(e)
head(e$large_data)

# lets build our own class system
# ===============================================
AwesomeClass <- function(...) {
  data <- list(...)
  names <- names(data)
  self <- new.env(parent = parent.frame())
  self$self <- self
  for (i in seq_along(data)) {
    self[[names[[i]]]] <- data[[i]]
  }
  for (name in names) {
    if (is.function(self[[name]])) {
      environment(self[[name]]) <- self
    }
  }
  # Extra layer: user assignments (awesome$a <- 99) land in outer,
  # while the methods keep seeing the untouched values in self
  outer <- new.env(parent = self)
  attr(outer, "class") <- "Awesome"
  outer
}
awesome <- AwesomeClass(
  a = 1,
  b = 2,
  c = 1:5,
  d = "Hello",
  show = function() {
    print(self$a)
    print(self$b)
    print(self$c)
    print(self$d)
  }
)
with(awesome, show())
`$.Awesome` <- function(x, name) {
  x[[name]]
}
`[[.Awesome` <- function(x, i, ...) {
  get(i, envir = x, inherits = TRUE)
}
awesome$show()
awesome[["show"]]()

print.Awesome <- function(obj) {
  print(obj$a)
  print(obj$b)
  print(obj$c)
  print(obj$d)
}
print(awesome)

# A linked list
# ===============================================
# Create an instance of the class
node <- function(data) {
  head <- new.env(parent = emptyenv())
  attr(head, "class") <- c("LinkedList", "LinkedListNode")
  head$data <- data
  head$next_node <- NULL
  head$previous_node <- NULL
  head
}

push_node <- function(head, value) {
  stopifnot(inherits(value, "LinkedListNode"))
  while (!is.null(head$next_node)) {
    head <- head$next_node
  }
  head$next_node <- value
  value$previous_node <- head
}

# head can't be rebound in the caller's scope, so the new head is returned
push_front <- function(head, value) {
  stopifnot(inherits(value, "LinkedListNode"))
  value$next_node <- head
  head$previous_node <- value
  value
}

# Removes the first node holding value; returns head since it may change
remove_node <- function(head, value) {
  current <- head
  while (!is.null(current)) {
    if (identical(current$data, value)) {
      prev <- current$previous_node
      nxt <- current$next_node
      if (!is.null(prev)) prev$next_node <- nxt
      if (!is.null(nxt)) nxt$previous_node <- prev
      if (identical(current, head)) head <- nxt
      return(head)
    }
    current <- current$next_node
  }
  head
}

`print.LinkedList` <- function(x, ...) {
  print(x$data)
  if (!is.null(x$next_node)) {
    print(x$next_node)
  }
  invisible(NULL)
}

head <- node(100L)
push_node(head, node(42L))
push_node(head, node(44L))
print(head)
head$data # 100
head$previous_node$data # NULL
head$next_node$previous_node$data # 100
head$next_node$next_node$previous_node$data # 42

head <- push_front(head, node(1L))
print(head) # 1 100 42 44
head$next_node$previous_node$data # 1

head <- remove_node(head, 42L)
print(head) # 1 100 44
head <- remove_node(head, 1L)
print(head) # 100 44
head$previous_node # NULL
