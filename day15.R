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
upper_bound <- 20L

txt <- readFile("input15.txt")
target_row <- 2000000L
upper_bound <- 4000000L

extract_numbers <- function(line) {
  matches <- regmatches(line, gregexpr("-?[[:digit:]]+", line))
  as.integer(unlist(matches))
}

sensors <- ustrsplit(txt, "\n")
positions <- lapply(sensors, extract_numbers)

beacons <- list()
for (pos in positions) {
  x <- pos[[3L]]
  y <- pos[[4L]]
  if (x >= 0 && x <= upper_bound && y >= 0 && y <= upper_bound) {
    x <- toString(x)
    y <- toString(y)
    if (!y %in% names(beacons)) {
      beacons[[y]] <- list()
    }
    beacons[[y]][[toString(x)]] <- TRUE
  }
}

join_intervals <- function(lefts, rights) {
  n_intervals <- length(lefts)
  the_order <- order(lefts)
  lefts <- lefts[the_order]
  rights <- rights[the_order]
  if (lefts[[1L]] > 0L || max(rights) < target_row) return(TRUE)

  right_bound <- rights[[1L]]

  for (i in seq_len(n_intervals - 1L) + 1L) {
    if (lefts[[i]] > right_bound) return(TRUE)
    right_bound <- max(right_bound, rights[[i]])
  }
  FALSE
}

scan_row <- function(positions, target_row) {
  overlap <- c()

  lefts <- c()
  rights <- c()

  for (pos in positions) {
    md <- abs(pos[[1L]] - pos[[3L]]) + abs(pos[[2L]] - pos[[4L]])

    y_dist <- abs(pos[[2L]] - target_row)
    delta <- md - y_dist

    if (delta > 0L) {
      # catn("md:", md)
      # catn("y_dist:", y_dist)
      # catn("delta:", delta)
      # overlap <- c(overlap, (pos[[1L]] - delta):(pos[[1L]] + delta))
      lefts <- c(lefts, pos[[1L]] - delta)
      rights <- c(rights, pos[[1L]] + delta)
    }
  }
  # if (target_row == 11) browser()
  join_intervals(lefts, rights)
}

print(length(unique(scan_row(positions, 10L))) -
        length(beacons[[toString(target_row)]]))

done <- FALSE
i <- 0L
while (!done && i < upper_bound) {
  i <- i + 1L
  if (i %% 100000L == 0L) catn("Row:", i)
  done <- scan_row(positions, i)
}
catn("Row: ", i)

