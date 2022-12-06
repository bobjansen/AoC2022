# Puzzle 1
file <- "input1.txt"
txt <- readFile(file)
blocks <- ustrsplit(txt, "\n\n")

sum_calories <- function(block) {
  lines <- ustrsplit(block, "\n")
  sum(as.integer(lines))
}
calorie_counts <- sapply(blocks, sum_calories)
names(calorie_counts) <- paste("elf", seq(length(calorie_counts)))

cat("Puzzle 1:", max(calorie_counts), "\n")

# Puzzle 2
cat("Puzzle 2:", sum(sort(calorie_counts, decreasing = TRUE)[1:3]), "\n")
