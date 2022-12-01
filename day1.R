# Puzzle 1
file <- "input1.txt"
txt <- readChar(file, file.info(file)$size)
blocks <- strsplit(txt, "\n\n")[[1L]]

sum_calories <- function(block) {
  lines <- strsplit(block, "\n")[[1L]]
  sum(strtoi(lines, 10L))[[1L]]
}
calorie_counts <- sapply(blocks, sum_calories)
names(calorie_counts) <- paste("elf", seq(length(calorie_counts)))

cat("Puzzle 1:", max(calorie_counts), "\n")

# Puzzle 2
cat("Puzzle 2:", sum(-sort(-calorie_counts)[1:3]), "\n")
