library(data.table)


file <- "input3.csv"
txt <- readChar(file, file.info(file)$size)
lines <- strsplit(ustrsplit(txt, "\n"), "")

# Puzzle 5

index <- 0L
create_backpack <- function(items) {
  num_items <- length(items)
  index <<- index + 1L
  data.table::data.table(
    ElfID = index,
    Compartment1 = items[1:(num_items / 2L)],
    Compartment2 = items[(num_items / 2L + 1):num_items]
  )
}
backpacks <- rbindlist(lapply(lines, create_backpack))

left <- backpacks[, .(ElfID, Item = Compartment1)]
right <- backpacks[, .(ElfID, Item = Compartment2)]

overlaps <- unique(merge(left, right, by = c("ElfID", "Item")))

priority <- data.table::data.table(Item = c(letters, LETTERS))
priority <- priority[, Priority := .I]

overlaps <- merge(overlaps, priority, by = "Item")
cat("Puzzle 5:", overlaps[, sum(Priority)], "\n")

# Puzzle 6

backpacks <- unique(rbind(left, right))
groups = data.table::data.table(
  ElfID = 1:index,
  Group = rep(1:(index / 3L), each = 3L)
)
backpacks <- merge(backpacks, groups, by = "ElfID")

badges <- backpacks[, .N, .(Group, Item)][N == 3L]
badges <- merge(badges, priority, by = "Item")
cat("Puzzle 6:", badges[, sum(Priority)], "\n")
