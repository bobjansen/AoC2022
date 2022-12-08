source("helpers.R")

txt <- readFile("input7.txt")

ls_mode <- FALSE
tree <- dictionary(lst = list(root = recollections::dictionary()))
path = c("?")

read_lines <- function(txt) {
  ustrsplit(txt, "\n")
}
lines <- read_lines(txt)

get_current_dir <- function(tree, path) {
  current_dir <- tree
  for (dir in path[seq_len(length(path))]) {
    if (dir != "") {
      current_dir <- getValue(current_dir, dir)
    } else {
      current_dir <- getValue(current_dir, "root")
    }
  }
  current_dir
}

run_command <- function(tree, command, arg) {
  ls_mode <<- FALSE
  if (command == "cd") {
    if (arg == "/") {
      path <<- c("")
    } else {
      if (arg == "..") {
        path <<- path[seq_len(length(path) - 1L)]
      } else {
        current_dir <- get_current_dir(tree, path)
        path <<- c(path, arg)
        if (!arg %in% keys(current_dir)) {
          setValue(current_dir, arg, recollections::dictionary())
        }
      }
    }
  } else if (command == "ls") {
    ls_mode <<- TRUE
  } else {
    stop("Unknown command: ", command)
  }
  tree
}

read_output <- function(tree, path, file_spec, name) {
  current_dir <- get_current_dir(tree, path)
  if (file_spec == "dir") {
    if (!name %in% keys(current_dir)) {
      setValue(current_dir, name, recollections::dictionary())
    }
  } else {
    setValue(current_dir, name, file_spec)
  }
}

for (line in lines) {
  parts <- ustrsplit(line, " ")
  if (parts[[1L]] == "$") {
    tree <- run_command(tree, parts[[2L]], parts[[3L]])
  } else if (ls_mode) {
    read_output(tree, path, parts[[1L]], parts[[2L]])
  } else {
    stop("Reading output but not in ls_mode")
  }
}

sizes <- recollections::dictionary()
get_dir_sizes <- function(subtree, path) {
  total_size <- 0L
  for (file_name in keys(subtree)) {
    file <- getValue(subtree, file_name)
    if (is(file, "Dictionary")) {
      size <- get_dir_sizes(file, paste(path, file_name, sep = "/"))
    } else {
      size <- as.integer(file)
    }
    total_size <- total_size + size
  }
  setValue(sizes, path, total_size)
  total_size
}
get_dir_sizes(getValue(tree, "root"), "")

sizes <- sort(unlist(recollections::toList(sizes)))
cat_solution(13, sum(sizes[sizes <= 100000L]))

disk_size <- 70000000
free <- disk_size - tail(sizes, 1)
needed <- 30000000 - free
cat_solution(14, sizes[sizes >= needed][[1L]])
