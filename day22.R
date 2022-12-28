source("helpers.R")

part2 <- FALSE

txt <- "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"

txt <- readFile("input22.txt")

c(map, moves) %<-% ustrsplit(txt, "\n\n")

steps <- as.integer(ustrsplit(moves, "R|L"))
turns <- ustrsplit(gsub("[[:digit:]]", "", moves), "")

rows <- ustrsplit(map, "\n")
rows <- strsplit(rows, "")
height <- length(rows)
width <- max(sapply(rows, length))
map <- matrix(0L, height, width)
for (i in 1:height) {
  map[i, which(rows[[i]] == ".")] <- 1L
  map[i, which(rows[[i]] == "#")] <- 2L
}

zimod <- function(e1, e2) {
  ((e1 - 1L) %% e2) + 1L
}

if (!part2) {
  find_next <- function(tiles, location, distance, direction_index, step = 1L) {
    test_location <- location
    for (i in 1:distance) {
      test_location <- zimod(test_location + step, length(tiles))
      while (tiles[[test_location]] == 0L) {
        test_location <- zimod(test_location + step, length(tiles))
      }
      if (tiles[[test_location]] == 2L) {
        return(list(location = location, direction_index = direction_index))
      }
      location <- test_location
    }
    list(location = location, direction_index = direction_index)
  }
}

direction_index <- 1L
directions <- list(
  "east" = \(location, direction_index, distance) {
    new_loc <- find_next(
      map[location$row,], location$column, distance, direction_index
    )
    list(
      location = list(row = location$row, column = new_loc$location),
      direction_index = direction_index
    )
  },
  "south" = \(location, direction_index, distance) {
    new_loc <- find_next(
      map[, location$column], location$row, distance, direction_index
    )
    list(
      location = list(row =new_loc$location, column = location$column),
      direction_index = direction_index
    )
  },
  "west" = \(location, direction_index, distance) {
    new_loc <- find_next(
      map[location$row,], location$column, distance, direction_index,
      step = -1L
    )
    list(
      location = list(row = location$row, column = new_loc$location),
      direction_index = direction_index
    )
  },
  "north" = \(location, direction_index, distance) {
    new_loc <- find_next(
      map[, location$column], location$row, distance, direction_index,
      step = -1L
    )
    list(
      location = list(row = new_loc$location, column = location$column),
      direction_index = direction_index
    )
  }
)

print_map <- function(map, location) {
  catn("    ", paste((1:width) %% 10L), sep = "")
  for (i in 1:nrow(map)) {
    line <- gsub("0", " ", paste(map[i,], collapse = ""))
    line <- gsub("1", ".", line)
    line <- gsub("2", "#", line)
    catn(sprintf("%03d", i), line)
  }
}

location <- list(row = 1, column = which.max(map[1, ] == 1L))
for (i in 1:length(steps)) {
  update <- directions[[direction_index]](location, direction_index, steps[[i]])
  location <- update$location
  direction_index <- update$direction_index
  if (i < length(turns)) {
    if (turns[[i]] == "L") {
      direction_index <- direction_index - 1L
    } else {
      direction_index <- direction_index + 1L
    }
    direction_index <- zimod(direction_index, length(directions))
  }
  # catn("Position:", location$row, ",", location$column)
}

cat_solution(
  43L,
  location$row * 1000L + location$column * 4L + direction_index - 1L
)
