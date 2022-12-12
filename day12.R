source("helpers.R")

txt <- "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

translate <- c("S", letters, "E")
heights <- strsplit(ustrsplit(readFile("input12.txt"), "\n"), "")
# heights <- strsplit(ustrsplit(txt, "\n"), "")
heights <- sapply(heights, \(.) match(., translate) -1L)
max_distance <- nrow(heights) * ncol(heights)
directions <- list(c(1L, 0L), c(0L, -1L), c(-1L, 0L), c(0L, 1L))

point_in_bounds <- function(point) {
  point[[1L]] >= 1L && point[[2L]] >= 1L &&
    point[[1L]] <= nrow(heights) && point[[2L]] <= ncol(heights)
}

start_at <- which(heights == 0L, arr.ind = TRUE)
end_at <- which(heights == 27L, , arr.ind = TRUE)
heights[start_at[[1L]], start_at[[2L]]] <- 1L
heights[end_at[[1L]], end_at[[2L]]] <- 26L

add_destinations <- function(source, visited, destinations) {
  current_height <- heights[source[[1L]], source[[2L]]]
  distance_to_source <- visited[source[[1L]], source[[2L]]]
  for (direction in directions) {
    destination <- source + direction
    if (point_in_bounds(destination)) {
      destination_height <- heights[destination[[1L]], destination[[2L]]]
      if (destination_height >= current_height - 1L) {
        old_distance <- visited[destination[[1L]], destination[[2L]]]
        if (distance_to_source + 1L < old_distance) {
          visited[destination[[1L]], destination[[2L]]] <-
            distance_to_source + 1L
          destinations <- c(destinations, list(destination))
        }
      }
    }
  }
  list(destinations = destinations, visited = visited)
}

bfs <- function() {
  visited <- matrix(max_distance, nrow(heights), ncol(heights))
  destinations <- list(end_at)
  visited[end_at[[1L]], end_at[[2L]]] <- 0L

  repeat {
    test_destination <- destinations[[1L]]
    lst <- add_destinations(
      test_destination, visited,
      destinations[seq_len(length(destinations) - 1L) + 1L]
    )
    destinations <- lst[["destinations"]]
    visited <- lst[["visited"]]
    if (length(destinations) == 0L) break
  }
  visited
}

visited <- bfs()

cat_solution(23L, visited[start_at[[1L]], start_at[[2L]]])

as <- which(heights == 1L, arr.ind = TRUE)
distances <- sapply(1:nrow(as), function(i) {
  point <- as[i,]
  visited[point[[1L]], point[[2L]]]
})
cat_solution(24L, min(unlist(distances)))
