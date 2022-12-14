source("helpers.R")
txt <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

txt <- readFile("input14.txt")

part2 <- TRUE

parts <- lapply(
  ustrsplit(txt, "\n"),
  function(line) {
    coords <- ustrsplit(line, " -> ")
    lapply(coords, function(coord) {
      as.integer(ustrsplit(coord, ","))
    })
  })

height <- max(sapply(parts, \(coords) max(sapply(coords, \(.) .[[2L]])))) + 1L
width <- 500L + height + 1L
cave <- if (part2) {
  height <- height + 2L
  parts <- c(parts, list(list(c(1L, height -1L), c(width, height -1L))))
  matrix(0L, height, width)
} else {
  matrix(0L, height + 1L, width)
}

source <- c(1L, 500L)

cave[source[[1L]], source[[2L]]] <- -1L

add_rocks <- function(cave, start, end) {
  if (start[[1L]] == end[[1L]]) {
    cave[(start[[2L]]:end[[2L]]) + 1L, start[[1L]]] <- 2L
  } else {
    cave[start[[2L]] + 1L, start[[1L]]:end[[1L]]] <- 2L
  }
  cave
}

print_cave <- function(cave) {
  for (i in 1:nrow(cave)) {
    row <- paste(cave[i, ], collapse="")
    row <- gsub("0", ".", gsub("-1", "S", gsub("2", "#", row)))
    catn(row)
  }
}

create_cave <- function(cave, parts) {
  for (part in parts) {
    if (length(part) == 1L) {
      cave[part[[1L]][[1L]], part[[1L]][[2L]]] <- 1L
    } else {
      start <- part[[1L]]
      for (i in 2:length(part)) {
        end <- part[[i]]
        cave <- add_rocks(cave, start, end)
        start <- end
      }
    }
  }
  cave
}

win_condition <- if (!part2) {
  function(cave, source) source[[1L]] >= height
} else {
  function(cave, source) cave[source[[1L]], source[[2L]]] == 1L
}

done <- FALSE
drop_sand <- function(cave, source) {
  position <- source
  while (cave[position[[1L]], position[[2L]]] <= 0L) {
    if (position[[1L]] > height) {
      done <<- TRUE
      return(cave)
    } else if (cave[position[[1L]] + 1L, position[[2L]]] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]])
    } else if (cave[position[[1L]] + 1L, position[[2L]] - 1L] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]] - 1L)
    } else if (cave[position[[1L]] + 1L, position[[2L]] + 1L] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]] + 1L)
    } else {
      cave[position[[1L]], position[[2L]]] <- 1L
    }
  }
  # if (win_condition(cave, source)) done <<- TRUE
  if (cave[source[[1L]], source[[2L]]] == 1L) done <<- TRUE

  cave
}

cave <- create_cave(cave, parts)

run <- function() {
  i <- 0L
  while (!done && i < 30000) {
    i <- i + 1L
    cave <- drop_sand(cave, source)
  }

  cat_solution(27, sum(cave == 1L))
}
