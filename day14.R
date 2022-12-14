source("helpers.R")
txt <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

txt <- readFile("input14.txt")

part2 <- FALSE

parts <- lapply(
  ustrsplit(txt, "\n"),
  function(line) {
    coords <- ustrsplit(line, " -> ")
    lapply(coords, function(coord) {
      as.integer(ustrsplit(coord, ","))
    })
  })

height <- max(sapply(parts, \(coords) max(sapply(coords, \(.) .[[2L]])))) + 1L
cave <- if (part2) {
  height <- height + 2L
  parts <- c(parts, list(list(c(1L, height -1L), c(1001L, height -1L))))
  matrix(0L, height, 1001L)
} else {
  matrix(0L, height + 1L, 1001L)
}

source <- c(1L, 500L)


cave[source[[1L]], source[[2L]]] <- -1L

add_rocks <- function(cave, start, end) {
  if (start[[1L]] == end[[1L]]) {
    for (i in start[[2L]]:end[[2L]]) {
      cave[i + 1L, start[[1L]]] <- 2L
    }
  } else {
    for (i in start[[1L]]:end[[1L]]) {
      cave[start[[2L]] + 1L, i] <- 2L
    }
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
cave <- create_cave(cave, parts)

drop_count <- 0L
done <- FALSE

win_condition <- if (!part2) {
  function(cave, source) source[[1L]] >= height
} else {
  function(cave, source) cave[source[[1L]], source[[2L]]] == 1L
}

drop_sand <- function(cave, source) {
  position <- source
  while (cave[position[[1L]], position[[2L]]] <= 0L) {
    if (position[[1L]] > height ||
        length(cave[position[[1L]] + 1L, position[[2L]]]) == -1L) {

      done <<- TRUE
      return(cave)
    } else    if (cave[position[[1L]] + 1L, position[[2L]]] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]])
    } else if (cave[position[[1L]] + 1L, position[[2L]] - 1L] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]] - 1L)
    } else if (cave[position[[1L]] + 1L, position[[2L]] + 1L] == 0L) {
      position <- c(position[[1L]] + 1L, position[[2L]] + 1L)
    } else {
      cave[position[[1L]], position[[2L]]] <- 1L
      drop_count <<- drop_count + 1L
    }
  }
  if (win_condition(cave, source)) done <<- TRUE
  cave
}

i <- 0L
while (!done && i < 30000) {
  i <- i + 1L
  # if (i %% 10000L == 0L) print_cave(cave)
  cave <- drop_sand(cave, source)
  # tryCatch(cave <- drop_sand(cave, source), error=function() done <<- TRUE)
}

cat_solution(27, drop_count)
