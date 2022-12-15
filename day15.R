source("helpers.R")

txt <- "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"

target_row <- 10L

txt <- readFile("input15.txt")
target_row <- 2000000L

extract_numbers <- function(line) {
  matches <- regmatches(line, gregexpr("-?[[:digit:]]+", line))
  as.integer(unlist(matches))
}

sensors <- ustrsplit(txt, "\n")
positions <- lapply(sensors, extract_numbers)


overlap <- c()

beacon_hits <- dictionary()

for (position in positions) {
  md <- abs(position[[1L]] - position[[3L]]) +
    abs(position[[2L]] - position[[4L]])

  y_dist <- abs(position[[2L]] - target_row)
  delta <- md - y_dist
  # browser()
  if (delta > 0L) {
    if (position[[4L]] == target_row) {
      setValue(beacon_hits, toString(c(position[[3L]], position[[4L]])), TRUE)
    }

    catn("md:", md)
    catn("y_dist:", y_dist)
    catn("delta:", delta)

    overlap <- c(overlap, (position[[1L]] - delta):(position[[1L]] + delta))
  }
}

print(length(unique(overlap)) - size(beacon_hits))
