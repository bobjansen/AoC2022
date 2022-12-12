source("helpers.R")

txt <- "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

translate <- c("S", letters, "E")
heights <- strsplit(ustrsplit(readFile("input12.txt"), "\n"), "")
heights <- sapply(heights, \(.) match(., translate) -1L)

directions <- list(c(1L, 0L), c(0L, -1L), c(-1L, 0L), c(0L, 1L))

point_in_bounds <- function(point) {
  point[[1L]] >= 1L && point[[2L]] >= 1L &&
    point[[1L]] <= nrow(heights) && point[[2L]] <= ncol(heights)
}

find_coords <- function(index) {
  row <- index %% nrow(heights)
  column <- index %/% nrow(heights) + 1L
  c(row, column)
}

update_shortest_path <- function(visited, point, distance) {
  key <- toString(point)
  current_distance <- visited[key]
  is_new <- is.null(current_distance)
  if (is_new || distance < current_distance) {
    visited[key] <- distance
  }
  is_new
}

start_at <- find_coords(which(heights == 0L))
end_at <- find_coords(which(heights == 27L))
heights[start_at[[1L]], start_at[[2L]]] <- 0L
heights[end_at[[1L]], end_at[[2L]]] <- 26L
catn("Starts at: ", start_at[[1L]], ", ", start_at[[2L]], sep="")
catn("Ends at: ", end_at[[1L]], ", ", end_at[[2L]], sep="")

add_destinations <- function(source, visited, destinations) {
  current_height <- heights[source[[1L]], source[[2L]]]
  distance_to_source <- visited[toString(source)]
  for (direction in directions) {
    destination <- source + direction
    if (point_in_bounds(destination)) {
      destination_height <- heights[destination[[1L]], destination[[2L]]]
      if (destination_height >= current_height - 1L) {
        is_new <- update_shortest_path(
          visited, destination, distance_to_source + 1L
        )
        if (is_new) destinations <- append(destinations, list(destination))
      }
    }
  }
  destinations
}

visited <- dictionary()
destinations <- list()

update_shortest_path(visited, end_at, 0L)
destinations <- add_destinations(end_at, visited, destinations)

while (length(destinations) > 0L) {
  test_destination <- destinations[[1L]]
  destinations <- if (length(destinations) >= 2L) {
    destinations[2:length(destinations)]
  } else {
    list()
  }
  destinations <- add_destinations(test_destination, visited, destinations)
}

print(visited[toString(start_at)])

sapply(which(heights == 1L), function(index) {
  point <- find_coords(index)
  browser()
  visited[toString(point)]
})
