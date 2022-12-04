library(data.table)

# Puzzle 3

translateThem <- function(them) {
  them[them == "A"] <- "Rock"
  them[them == "B"] <- "Paper"
  them[them == "C"] <- "Scissors"
  them
}

translateMe <- function(me) {
  me[me == "X"] <- "Rock"
  me[me == "Y"] <- "Paper"
  me[me == "Z"] <- "Scissors"
  me
}

winner <- function(them , me) {
  scores <- rep(0L, length(them)) # Initialize to loss
  scores[them == me] <- 3L
  scores[(them == "Rock" & me == "Paper") |
           (them == "Paper" & me == "Scissors") |
           (them == "Scissors" & me == "Rock")] <- 6L
  scores
}

dt <- data.table::fread('input2.csv', header = FALSE, sep = ' ')
names(dt) <- c("Them", "Me")
dt[, ':='(Them = translateThem(Them), Me = translateMe(Me))]

calculateScore <- function(dt) {
  dt[, ScoreResult := winner(Them, Me)]

  dt[, ScoreChoice := 1L]
  dt[Me == "Paper", ScoreChoice := 2L]
  dt[Me == "Scissors", ScoreChoice := 3L]

  dt[, Score := ScoreResult + ScoreChoice]
  dt[, sum(Score)]
}
cat("Puzzle 3:", calculateScore(dt), "\n")

# Puzzle 4

choices <- c("Rock", "Paper", "Scissors")
losesTo <- c("Scissors", "Rock", "Paper")
winsFrom <- c("Paper", "Scissors", "Rock")

fixResultColumn <- function(them, result) {
  selX <- result == "Rock"
  selY <- result == "Paper"
  selZ <- result == "Scissors"

  me <- rep("", length(them))
  me[selX] <- losesTo[match(them[selX], choices)]
  me[selY] <- them[selY]
  me[selZ] <- winsFrom[match(them[selZ], choices)]
  me
}

dt[, Me := fixResultColumn(Them, Me)]

cat("Puzzle 4:", calculateScore(dt), "\n")
