source("helpers.R")

streams <- c(
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
)

find_marker <- function(stream, marker_size) {
  chars <- ustrsplit(stream, "")
  buffer <- c()
  for (i in seq_along(chars)) {
    char <- chars[[i]]
    index <- match(char, buffer, nomatch = 0L)
    buffer <- c(buffer, char)
    buffer <- buffer[(index + 1L):length(buffer)]
    if (length(buffer) == marker_size) {
      catn(paste0(buffer, collapse = ""))
      return(i)
    }
  }
  NA_integer_
}

# Puzzle 11
sapply(streams, find_marker, marker_size = 4L)
cat("\n")
cat_solution(11L, find_marker(readFile("input6.txt"), marker_size = 4L))
cat("\n")

# Puzzle 12
sapply(streams, find_marker, marker_size = 14L)
cat("\n")
cat_solution(12L, find_marker(readFile("input6.txt"), marker_size = 14L))
