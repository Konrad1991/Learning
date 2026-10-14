memo_fib <- (function() {
  cache <- new.env()
  function(n) {
    if (exists(as.character(n), envir = cache)) return(cache[[as.character(n)]])
    if (n <= 2) res <- 1 else res <- Recall(n - 1) + Recall(n - 2)
    cache[[as.character(n)]] <- res
    res
  }
})()
memo_fib(10)


