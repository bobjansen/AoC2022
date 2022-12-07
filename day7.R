source("helpers.R")

txt <- "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"

txt <- readFile("input7.txt")

ls_mode <- FALSE
tree <- dictionary(lst = list(root = dictionary()))
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
          setValue(current_dir, arg, dictionary())
        }
      }
    }
    if (length(path) == "1") {
      catn("/")
    } else {
      catn(paste(path, collapse = "/"))
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
      setValue(current_dir, name, dictionary())
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

sizes <- dictionary()

get_dir_sizes <- function(subtree, path) {
  total_size <- 0L
  for (file_name in keys(subtree)) {
    file <- getValue(subtree, file_name)
    if (class(file) == "Dictionary") {
      dir_size <- get_dir_sizes(
        file,
        paste(path, file_name, sep="/")
      )
      total_size <- total_size + dir_size
    } else {
      total_size <- total_size + as.integer(file)
    }
  }
  setValue(sizes, path, total_size)
  total_size
}

answer <- 0L
get_dir_sizes(getValue(tree, "root"), "")
for (key in keys(sizes)) {
  size <- getValue(sizes, key)
  catn(key, ": ", size, sep = "")
  if (size <= 100000L) {
    answer <- answer + size
  }
}

cat_solution(13, answer)

sizes <- sort(unlist(recollections::toList(sizes)))

disk_size <- 70000000
free <- disk_size - tail(sizes, 1)
needed <- 30000000 - free
cat_solution(13, sizes[sizes >= needed][[1L]])

