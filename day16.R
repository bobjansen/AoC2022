source("helpers.R")

txt <- "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"

txt <- readFile("input16.txt")

lines <- strsplit(ustrsplit(txt, "\n"), " ")

setClass("Valve", representation(
  ID = "character",
  flow = "integer",
  destinations = "character",
  open = "logical"
))

valve_ids <- c()
candidates <- 0L
create_valve <- function(line) {
  id <- line[[2L]]
  flow <- substr(line[[5L]], 6, nchar(line[[5L]]) - 1L)
  destinations <- gsub(",", "", line[10L:length(line)])
  flow <- as.integer(flow)

  candidates <<- candidates + (flow > 0L)

  valve_ids <<- c(valve_ids, id)

  new(
    "Valve",
    ID = id,
    flow = flow,
    destinations = destinations,
    open = FALSE
  )
}

valves <- sapply(lines, create_valve)
names(valves) <- valve_ids

max_flow <- 0L
location <- "AA"
total_time <- 30L

steps <- list()

set.seed(42L)

explore <- function(valves, location, budget, current_flow, open_valves, flow_per_time, steps) {

  if (open_valves == candidates) {
    return(current_flow)
  }
  if (current_flow + budget * (budget + 1L) * 11L < max_flow) {
    return(current_flow)
  }

  # if (runif(1L) > 0.999) {
  #   catn("Entering", location, "at", budget, "with current flow", current_flow)
  # }
  if (budget > 2L) {
    steps[[length(steps) + 1L]] <- list(
      location = location,
      flow_per_time = flow_per_time,
      open_valves = open_valves
    )

    for (destination in valves[[location]]@destinations) {
      if (length(steps) == 1L ||
          destination != steps[[length(steps) - 1L]]$location) {
        explore(
          valves,
          destination, budget - 1L, current_flow, open_valves, flow_per_time, steps)
      }
    }
  }

  if (!valves[[location]]@open && valves[[location]]@flow > 0L) {
    budget <- budget - 1L
    current_flow <- current_flow + valves[[location]]@flow * budget
    valves[[location]]@open <- TRUE

    steps[[length(steps) + 1L]] <- list(
      location = location,
      flow_per_time = flow_per_time,
      open_valves = open_valves
    )
    open_valves <- open_valves + 1L
    flow_per_time <- flow_per_time + valves[[location]]@flow

    if (current_flow > max_flow) {
      max_flow <<- current_flow
      catn("Max flow:", max_flow)
    }
    if (budget > 2L) {
      for (destination in valves[[location]]@destinations) {
        explore(
          valves,
          destination, budget - 1L, current_flow, open_valves, flow_per_time, steps)
      }
    }
  }

}

explore(valves, "AA", total_time, 0L, 0L, 0L, steps)
cat_solution(31L, max_flow)
