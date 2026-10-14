# Lists
# ===============================================
# lists can hold anything, also mixed types
l <- list(1:3, TRUE, "Hello")
str(l)

# List can have names
l <- list(a = 1, b = TRUE, c = 3.4)
str(l)

# Alternative to set names
setNames(
  list(1, 2, 3, 4, 5),
  c("a", "b", "c", "d", "e")
)

# Lists are only containers, you cannot use them for example in math
l <- list(1, 2, 3)
# l + l # Error: non-numeric argument to binary operator

# Lists are vectors too
# ===============================================
# Atomic vectors hold one type, lists (generic vectors) hold anything
typeof(l) # list
is.vector(l) # TRUE
is.atomic(l) # FALSE
length(l) # 3

# Preallocate
# ===============================================
# Same as for atomic vectors in 00types.R
vector(mode = "list", length = 3) # 3 elements which are all NULL

# Nesting
# ===============================================
# A list can hold lists. Therefore, lists are called recursive vectors
nested <- list(1, list(2, list(3)))
str(nested)

# c() and coercion
# ===============================================
# The list is at the top of the coercion chain from 00types.R
str(c(list(1), "a", TRUE)) # List of 3. Everything became a list element

# unlist flattens a list and coerces silently
unlist(list(1L, 2.5)) # 1 2.5. double
unlist(list(1L, 2.5, "a")) # "1" "2.5" "a". Everything became character
unlist(nested) # 1 2 3. The nesting is gone

# Outlook: a data.frame is just a list with attributes
# ===============================================
df <- data.frame(x = 1:2, y = c("a", "b"))
typeof(df) # list
attributes(df) # $names "x" "y", $class "data.frame", $row.names 1 2
