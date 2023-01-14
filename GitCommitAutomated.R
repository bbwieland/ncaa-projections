library(git2r)

add(repo = getwd(), path = "GitCommitAutomated.R")

commit(repo = getwd(), message = paste0("Data update at: ", Sys.time()))

push(object = getwd())
