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
potential_flow <- 0L
create_valve <- function(line) {
  id <- line[[2L]]
  flow <- substr(line[[5L]], 6, nchar(line[[5L]]) - 1L)
  destinations <- gsub(",", "", line[10L:length(line)])
  flow <- as.integer(flow)

  potential_flow <<- potential_flow + flow
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

explore <- function(
    valves, location, budget, current_flow, flow_per_time, last_location
) {
  if (current_flow + (budget - 1L) * (potential_flow - flow_per_time) <= max_flow) {
    return(current_flow)
  }

  flow <- valves[[location]]@flow
  destinations <- valves[[location]]@destinations
  if (flow > 0L && !valves[[location]]@open) {
    valves[[location]]@open <- TRUE

    new_flow <- current_flow + flow * (budget - 1L)
    if (new_flow > max_flow) {
      max_flow <<- new_flow
      catn("Max flow:", max_flow)
    }
    if (potential_flow - flow_per_time == 0L) {
      return(current_flow)
    }
    if (budget > 2L) {
      for (destination in destinations) {
        explore(
          valves,
          destination,
          budget - 2L,
          new_flow,
          flow_per_time + flow,
          location)
      }
    }
  }

  if (current_flow +
      (budget - 2L) * (potential_flow - flow_per_time) > max_flow) {
    for (destination in destinations) {
      if (destination != last_location) {
        explore(
          valves,
          destination, budget - 1L, current_flow, flow_per_time, location)
      }
    }
  }
}

profvis::profvis(explore(valves, "AA", total_time, 0L, 0L, ""))
cat_solution(31L, max_flow)
