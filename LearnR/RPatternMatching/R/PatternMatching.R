
setClass("MatchClass",
         slots = list(arg = "ANY")
)

#' Create an instance of the S4 class MatchClass for pattern matching
#'
#' This function creates an instance of the S4 class MatchClass, which can be used for pattern matching
#'
#' @param x An R object to be matched against
#' @return An instance of the MatchClass class
#'
#' @seealso \code{\link{when}}
#' @export
Match <- function(x) {
  return(new("MatchClass", arg = x))
}

#' Pattern matching using instances of Match
#'
#' This function takes instances of the Match class and checks for a specific matching case based on provided conditions.
#'
#' @param value An R object to be matched
#' @param test A condition (either a function or a direct value) to test against `value`
#' @param predicate A value or function to be returned or applied when `test` matches `value`
#' @param apply Logical, indicating whether to apply `predicate` to `value` if `predicate` is a function
#'
#' @details
#' The `test` parameter can be:
#'   - A function: If `test(value)` returns TRUE, `predicate` is returned or applied to `value` (if `apply` is TRUE).
#'   - A direct value: If `value` matches `test`, `predicate` is returned or evaluated.
#'
#' The `predicate` parameter can be:
#'   - A value: Directly returned when `test` matches `value`.
#'   - A function: Applied to `value` (if `apply` is TRUE) when `test` matches `value`.
#'
#' @examples 
#' result <- Match("+") |>
#'   when("-", "minus") |>
#'   when(is.numeric, "numeric") |> 
#'   when(is.character, "character") |> 
#'   when(NULL, "not found")
#' print(result)
#'
#' result <- Match(1L) |>
#'   when("-", "minus") |>
#'   when(is.character, "character") |> 
#'   when(NULL, "not found")
#' print(result)
#'
#' @export        
when <- function(value, test, predicate, apply = FALSE) {
  if (is.null(test) && inherits(value, "MatchClass")) {
    return(predicate)
  }
  if (!inherits(value, "MatchClass")) {
    return(value)
  }
  if (is.function(test)) {
    if (do.call(test, list(value@arg))) {
      if (apply) {
        if (is.function(predicate)) {
          return(do.call(predicate, list(value@arg)))
        } else {
          warning("apply is true but predicate is not a function. value is returned.")
          return(predicate)
        }
      } else {
        return(predicate)
      }
    } else {
      return(value)
    }
  } else if (identical(slot(value, "arg"), test)) {
    return(eval(predicate))
  } else {
    return(value)
  }
}


#' Pattern Matching in the Style of Rust
#'
#' This function performs pattern matching on a value `x`, inspired by Rust's `match` expression.
#' Each case is specified as a formula of the form `test ~ result`, where `test` can be:
#' - A value (compared using `identical()`)
#' - A function (called with `x` and expected to return `TRUE`/`FALSE`)
#' - `TRUE` as a default fallback (must come last)
#'
#' The function returns the `result` of the first match. If `result` is a call or function, it is evaluated.
#'
#' @param x An object to match against.
#' @param ... A series of pattern-result pairs in the form of `test ~ result`.
#'   Tests can be literal values (compared with `identical()`) or functions (e.g., `is.numeric`).
#'   The final pattern can be `TRUE ~ default` to act as a fallback.
#'
#' @return The result of the first matching case. If no case matches and no default is given, an error is thrown.
#'
#' @examples
#' match(1,
#'   0 ~ "zero",
#'   1 ~ "one",
#'   is.character ~ "a character",
#'   TRUE ~ "fallback"
#' )
#'
#' match(NULL,
#'   0 ~ "zero",
#'   is.null ~ "was NULL",
#'   TRUE ~ "fallback"
#' )
#'
#' # With function as result
#' match(list(1,2,3),
#'   is.list ~ function(x) paste("list of", length(x)),
#'   TRUE ~ "not a list"
#' )
#'
#' @export
match <- function(x, ...) {
  cases <- rlang::enexprs(...)
  for (case in cases) {
    stopifnot(length(case) == 3 && case[[1]] == as.name("~"))
    test <- eval(case[[2]])
    result <- case[[3]]
    matched <-
      if (is.function(test)) test(x)
      else identical(x, test)

    if (matched) {
      return(if (is.call(result) || is.function(result)) eval(result) else result)
    }
  }
  stop("No match found and no default provided.")
}
