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

total_time <- 30L
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
    destinations = sort(destinations),
    open = flow == 0L
  )
}

valves <- sapply(lines, create_valve)
names(valves) <- valve_ids

state_scores <- matrix(-1L, length(valve_ids), total_time)
row.names(state_scores) <- valve_ids

max_flow <- 0L
location <- "AA"
no_hits <- 0L
hits <- 0L

explore <- function(
    valves, location, budget, current_flow, flow_per_time, last_location
) {
  if (current_flow + (budget - 1L) * (potential_flow - flow_per_time) <=
      max_flow) {
    return()
  }

  if (max(-1L, state_scores[location, seq_len(total_time - budget)]) >=
      current_flow) {
    hits <<- hits + 1L
    return()
  }
  no_hits <<- no_hits + 1L
  state_scores[location, total_time - budget + 1L] <- current_flow
  state_scores <<- state_scores

  flow <- valves[[location]]@flow
  destinations <- valves[[location]]@destinations
  if (flow > 0L && !valves[[location]]@open) {
    valves[[location]]@open <- TRUE

    new_flow <- current_flow + flow * (budget - 1L)
    if (new_flow > max_flow) {
      max_flow <<- new_flow
      catn("Max flow:", max_flow)
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

explore(valves, "AA", total_time, 0L, 0L, "")
catn("hits", hits)
catn("no_hits", no_hits)
cat_solution(31L, max_flow)


max_flow <- 2511L
# max_flow <-
total_time <- 26L
locations <- c("AA", "AA")
explorer_count <- length(locations)

states <- expand.grid(valve_ids, valve_ids, stringsAsFactors = FALSE)
states <- states[states$Var1 <= states$Var2,]
states <- paste(states$Var1, states$Var2, sep = "-")
# states <- valve_ids
state_scores <- matrix(-1L, length(states), total_time)
row.names(state_scores) <- states

update_state <- function(
    t, explorer_index, locations, current_flow, flow_per_time,
    open_valves, paths
) {
  if (t == total_time) return()

  if (current_flow + (total_time - t) * (potential_flow - flow_per_time) / 1.8 <=
      max_flow) {
    return()
  }

  is_final_explorer <- explorer_index == explorer_count
  next_explorer <- explorer_index %% explorer_count + 1L

  if (explorer_index == 1L) {
    state_string <- paste(sort(locations), collapse = "-")
    if (max(-1L, state_scores[state_string, seq_len(t)]) >= current_flow) {
      hits <<- hits + 1L
      return()
    }
    no_hits <<- no_hits + 1L
    state_scores[state_string, t + 1L] <- current_flow
    state_scores <<- state_scores
  }

  location <- locations[[explorer_index]]
  flow <- valves[[location]]@flow
  destinations <- valves[[location]]@destinations

  t <- t + is_final_explorer

  if (flow > 0L && !location %in% open_valves) {
    new_flow <- current_flow + flow * (total_time - t - (!is_final_explorer))
    if (new_flow > max_flow) {
      max_flow <<- new_flow
      if (max_flow == 1716) browser()
      catn("Max flow:", max_flow)
    }
    paths[[explorer_index]] <- c(
      paths[[explorer_index]], paste("open", location)
    )
    update_state(
      t, next_explorer, locations,
      new_flow, flow_per_time + flow, c(open_valves, location),
      paths)
      # c(path, paste("at", t, "open", location, "with explorer",
      #               explorer_index, "with flow", flow_per_time + flow,
      #               "score", new_flow)))
  }

  for (destination in destinations) {
    # browser()
    # if (length(paths[[explorer_index]]) == 2L) browser()
    if (length(paths[[explorer_index]]) == 1L ||
        paths[[explorer_index]][[length(paths[[explorer_index]]) - 1L]]
        != destination) {
      if (explorer_index == 1L ||
        locations[[1L]] != locations[[2L]] ||
        tail(paths[[explorer_index]], 1L) >= destination) {

        paths[[explorer_index]] <- c(paths[[explorer_index]], destination)
        locations[[explorer_index]] <- destination
        update_state(
          t, next_explorer, locations,
          current_flow, flow_per_time, open_valves, paths
          # c(path, paste(explorer_index, "to", destination)))
        )
      }
    }
  }
}

open_valves <- c()
update_state(
  0L, 1L, locations, 0L, 0L, open_valves, list(`1` = c("AA"), `2`= c("AA"))
)
catn("hits", hits)
catn("no_hits", no_hits)
cat_solution(32L, max_flow)


# explore2 <- function(
#     valves, locations, budget, current_flow, flow_per_time, last_locations
# ) {
#   if (current_flow + (budget - 1L) * (potential_flow - flow_per_time) <=
#       max_flow) {
#     return()
#   }
#
#   state_string <- paste(locations, collapse = "-")
#   if (max(-1L, state_scores[state_string, seq_len(total_time - budget)]) >=
#       current_flow) {
#     hits <<- hits + 1L
#     # return()
#   }
#   no_hits <<- no_hits + 1L
#   state_scores[state_string, total_time - budget + 1L] <- current_flow
#   state_scores <<- state_scores
#
#   location1 <- locations[[1L]]
#   location2 <- locations[[2L]]
#   flow1 <- valves[[location1]]@flow
#   flow2 <- valves[[location2]]@flow
#   destinations1 <- valves[[location1]]@destinations
#   destinations2 <- valves[[location2]]@destinations
#
#   # If you and elephant diverged
#   if (location1 != location2) {
#     # Is it an option to open either?
#     if ((flow1 > 0L && !valves[[location1]]@open) ||
#         (flow2 > 0L && !valves[[location2]]@open)) {
#
#       # Open both
#       valves[[location1]]@open <- TRUE
#       valves[[location2]]@open <- TRUE
#
#       new_flow <- current_flow + (flow1 + flow2) * (budget - 1L)
#       if (new_flow > max_flow) {
#         max_flow <<- new_flow
#         catn("Max flow:", max_flow)
#       }
#
#       if (budget > 2L) {
#         # Continue with both
#         for (destination1 in destinations1) {
#           for (destination2 in destinations2) {
#             explore2(
#               valves,
#               c(destination1, destination2),
#               budget - 2L,
#               new_flow,
#               flow_per_time + flow1 + flow2,
#               locations
#             )
#           }
#         }
#       }
#
#       # Try with second closed
#       if (flow1 > 0L && !valves[[location1]]@open) {
#         valves[[location1]]@open <- TRUE
#         valves[[location2]]@open <- FALSE
#         new_flow <- current_flow + flow1 * (budget - 1L)
#         if (budget > 2L) {
#           # Continue with both
#           for (destination1 in destinations1) {
#             for (destination2 in destinations2) {
#               explore2(
#                 valves,
#                 c(destination1, destination2),
#                 budget - 2L,
#                 new_flow,
#                 flow_per_time + flow1,
#                 locations
#               )
#             }
#           }
#         }
#       }
#
#       # Try with first closed
#       if (flow2 > 0L && !valves[[location1]]@open) {
#         valves[[location1]]@open <- FALSE
#         valves[[location2]]@open <- TRUE
#         new_flow <- current_flow + flow2 * (budget - 1L)
#         if (budget > 2L) {
#           # Continue with both
#           for (destination1 in destinations1) {
#             for (destination2 in destinations2) {
#               explore2(
#                 valves,
#                 c(destination1, destination2),
#                 budget - 2L,
#                 new_flow,
#                 flow_per_time + flow2,
#                 locations
#               )
#             }
#           }
#         }
#       }
#     }
#   }
#
#   valves[[location1]]@open <- FALSE
#   valves[[location2]]@open <- FALSE
#
#   # if (current_flow +
#   #     (budget - 2L) * (potential_flow - flow_per_time) > max_flow) {
#     for (destination1 in destinations1) {
#       # if (!destination1 %in% last_locations) {
#         for (destination2 in destinations2) {
#           # if (!destination2 %in% last_locations) {
#             explore2(
#               valves,
#               c(destination1, destination2),
#               budget - 2L,
#               current_flow,
#               flow_per_time + flow1 + flow2,
#               locations
#             )
#           }
#       #   }
#       # }
#     }
#   # }
# }
#
# max_flow <- 0L
# explore2(valves, c("AA", "AA"), total_time, 0L, 0L, c("", ""))
# catn("hits", hits)
# catn("no_hits", no_hits)
# cat_solution(32L, max_flow)
#
