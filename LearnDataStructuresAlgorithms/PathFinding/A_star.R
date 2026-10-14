binary_heap <- R6::R6Class(
  "binary_heap",
  public = list(
    arr = NULL,  # list of list(node = ..., priority = ...) pairs
    pos = NULL,  # env: as.character(node) -> current index in arr

    initialize = function() {
      self$arr <- list()
      self$pos <- new.env(hash = TRUE)
    },

    swap = function(i, j) {
      temp <- self$arr[[i]]
      self$arr[[i]] <- self$arr[[j]]
      self$arr[[j]] <- temp
      self$pos[[as.character(self$arr[[i]]$node)]] <- i
      self$pos[[as.character(self$arr[[j]]$node)]] <- j
    },

    sift_up = function(i) {
      while (i > 1) {
        parent_idx <- i %/% 2
        if (self$arr[[i]]$priority >= self$arr[[parent_idx]]$priority) break
        self$swap(i, parent_idx)
        i <- parent_idx
      }
    },

    insert = function(node, priority) {
      self$arr[[length(self$arr) + 1L]] <- list(node = node, priority = priority)
      i <- length(self$arr)
      self$pos[[as.character(node)]] <- i
      self$sift_up(i)
    },

    decrease_key = function(node, new_priority) {
      key <- as.character(node)
      if (!exists(key, envir = self$pos, inherits = FALSE)) {
        stop("decrease_key: node not in heap")
      }
      i <- self$pos[[key]]
      if (new_priority > self$arr[[i]]$priority) {
        stop("decrease_key: new priority must not be larger than the current one")
      }
      self$arr[[i]]$priority <- new_priority
      self$sift_up(i)
    },

    extract_min = function() {
      if (length(self$arr) == 0L) stop("cannot extract_min from an empty heap")
      res <- self$arr[[1L]]
      rm(list = as.character(res$node), envir = self$pos)
      n <- length(self$arr)
      self$arr[[1L]] <- self$arr[[n]]
      self$arr <- self$arr[seq_len(n - 1L)]
      n <- n - 1L
      if (n > 0L) self$pos[[as.character(self$arr[[1L]]$node)]] <- 1L

      i <- 1L
      repeat {
        left <- 2L * i
        right <- 2L * i + 1L
        smallest <- i

        has_left <- left <= n
        has_right <- right <= n

        if (has_left) {
          if (self$arr[[left]]$priority < self$arr[[smallest]]$priority) smallest <- left
        }
        if (has_right) {
          if (self$arr[[right]]$priority < self$arr[[smallest]]$priority) smallest <- right
        }

        if (smallest == i) break
        self$swap(i, smallest)
        i <- smallest
      }
      res
    },

    print = function(...) {
      n <- length(self$arr)
      level <- 0
      idx <- 1
      while (idx <= n) {
        last <- min(2^(level + 1) - 1, n)
        entries <- vapply(self$arr[idx:last], function(e) paste0(e$node, ":", e$priority), character(1L))
        cat(entries, "\n")
        idx <- last + 1
        level <- level + 1
      }
      invisible(self)
    }
  )
)

tile_types <- c("Grass", "Road", "Water")
tile <- setClass(
  "tile",
  slots = c(
    type = "character",
    z = "integer",
    occupied = "integer"
  )
)
TileMap <- setClass(
  "TileMap",
  slots = c(
    tiles = "list",
    w = "integer",
    h = "integer"
  )
)

node_to_xy <- function(node, w) {
  list(x = ((node - 1L) %% w) + 1L, y = ((node - 1L) %/% w) + 1L)
}

get_connected_nodes <- function(node, w, h) {
  xy <- node_to_xy(node, w)
  candidates <- list(c(xy$x - 1, xy$y), c(xy$x + 1, xy$y),
                      c(xy$x, xy$y - 1), c(xy$x, xy$y + 1))
  lapply(candidates, function(c) {
    if (c[1] >= 1 && c[1] <= w && c[2] >= 1 && c[2] <= h) tile_index(c[1], c[2], w) else NA
  })
}

move_to_tile_costs <- function(node, map) {
  costs <- list(Grass = 2L, Road = 1L, Water = Inf)
  t <- map@tiles[node][[1L]]@type
  costs[[t]]
}

# Manhattan distance from node to end - admissible as long as every step
# costs at least 1 (true now that the cheapest tile, Road, costs 1)
heuristic <- function(node, end, w) {
  a <- node_to_xy(node, w)
  b <- node_to_xy(end, w)
  abs(a$x - b$x) + abs(a$y - b$y)
}

dijkstra <- function(map, start, end){
  # Step 1: initialization
  n <- length(map@tiles)
  env <- new.env()
  env$pi <- rep(Inf, n)
  env$E <- rep(-1, n)
  processed <- rep(FALSE, n)

  # Step 2: start node initialization - pushed like every other node,
  # no more special-cased neighbor setup needed
  env$pi[[start]] <- 0
  heap <- binary_heap$new()
  heap$insert(node = start, priority = heuristic(start, end, map@w))

  # Step 3: processing of the rest of nodes
  while (length(heap$arr) > 0) {
    # node with least f = g (pi) + h (heuristic) is popped in O(log n)
    index <- heap$extract_min()$node
    if (processed[index]) next
    processed[index] <- TRUE

    # E and pi are updated
    for (j in get_connected_nodes(index, map@w, map@h)) {
      if (is.na(j) || processed[j]) next
      cheaper_transition <- env$pi[[index]] + move_to_tile_costs(j, map) < env$pi[j]
      if (cheaper_transition) {
        env$pi[[j]] <- env$pi[[index]] + move_to_tile_costs(j, map)
        env$E[[j]] <- index
        f <- env$pi[[j]] + heuristic(j, end, map@w)
        if (exists(as.character(j), envir = heap$pos, inherits = FALSE)) {
          heap$decrease_key(node = j, new_priority = f)
        } else {
          heap$insert(node = j, priority = f)
        }
      }
    }
  }

  #preparation of the solution to return
  if (is.infinite(env$pi[end])) stop("No path found")
  solution_route <- end
  while (!(start %in% solution_route)) {
    solution_route <- c(solution_route, env$E[solution_route[length(solution_route)]])
  }

  return(list("solution" = rev(solution_route), "loss" = env$pi[end]))
}

grass <- tile_types[1]
road <- tile_types[2]
water <- tile_types[3]
tile_index <- function(x, y, w) (y - 1L) * w + x
w <- 10L
h <- 10L
tiles <- lapply(seq_len(w * h), function(x) {
  tile(type = grass, z = 0L, occupied = 0L)
})

x <- 1:10
y <- 4
tiles[tile_index(x, y, w)] <- c(tile(type = water, z = 0L, occupied = 0L))

x <- 5L
y <- 1:10
tiles[tile_index(x, y, w)] <- c(tile(type = road, z = 0L, occupied = 0L))

x <- 1:10
y <- 5
tiles[tile_index(x, y, w)] <- c(tile(type = road, z = 0L, occupied = 0L))

map <- new("TileMap", tiles = tiles, w = w, h = h)
matrix(vapply(map@tiles, function(t) t@type, character(1L)),
       nrow = h, ncol = w, byrow = TRUE)

start <- 1
end <- 50
res <- dijkstra(map, start, end)
res
sapply(map@tiles, \(x) x@type)[res$solution]
