# renv::install("markdown")
# renv::install("gert")
# renv::install("withr")
# utils::install.packages("stringi")
# utils::install.packages("ragg")
# utils::install.packages("languageserver")
renv::snapshot()
library("markdown")
library("gert")
library("withr")

proj <- normalizePath("/path/to/working_dir", mustWork = FALSE)

sapply(
  file.path(proj, c("data", "outputs", "docs", "scripts", "logs")),
  dir.create, showWarnings = FALSE, mode = "0775", recursive = TRUE
)

## create a project via Rstudio or manually create the .Rproj
## use rproj
withr::with_dir(proj, {
  file <- paste0(basename(proj), ".Rproj")
  if (!file.exists(file)) {
    writeLines(
      con = file,
      text = c(
        "Version: 1.0",
        "",
        "RestoreWorkspace: No",
        "SaveWorkspace: No",
        "AlwaysSaveHistory: No",
        "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8",
        "",
        "AutoAppendNewline: Yes",
        "LineEndingConversion: Posix",
        "",
        "QuitChildProcessesOnExit: Yes"
      )
    )
  } else {
    message(sprintf('"%s" already exists! Nothing was done!', file))
  }
})

## use rprofile
withr::with_dir(proj, {
  file <- ".Rprofile"
  if (!file.exists(file)) {
    writeLines(
      con = file,
      text = c(
        "Sys.umask(\"0002\")",
        "if (interactive() && file.exists(\"~/.Rprofile\")) source(\"~/.Rprofile\")"
      )
    )
  } else {
    message(sprintf("\"%s\" already exists! Nothing was done!", file))
  }
})

## copy the dev container json and dockerfile
dir.create(file.path(proj, ".devcontainer"), showWarnings = TRUE, mode = "0775", recursive = TRUE)

file.copy( # from my SB as example
  from = list.files("/Isiprod1/project/SB_mboissel/.devcontainer/", full.names = TRUE),
  to = file.path(proj, ".devcontainer/"), overwrite = TRUE
)

## -*-*-*-*-*-* STOP HERE *-*-*-*-*-*
## now open the newly created project and in its console do :

## use renv
renv::init(project = proj)
## Rstudio will automatically open the new project here

library("renv")

## use gitlab
proj <- "/path/to/working_dir"

renv::install("jsonlite")
renv::install("rlang")
renv::install("withr")
renv::install("gert")
git_repository <- "http://gitlab.egid.local/BioStats"
git_remote <- gsub("https*://(.*)/(.*)", "\\1:\\2", git_repository)

withr::with_dir(proj, {
  file <- ".gitignore"
  if (!file.exists(file)) {
    writeLines(
      con = file,
      text = c(
        ".Rproj.user",
        "**.Rproj",
        "**.Rhistory",
        "**.Rdata",
        "core"
      )
    )
  } else {
    message(sprintf('"%s" already exists! Nothing was done!', file))
  }
})


#### Either :con init git with R
# gert::git_init() # it creates 'master' instead of 'main'
#
# gert::git_add(files = "*")
# if (!gert::user_is_configured()) {
#   stop("\"user.name\" and/or \"user.email\" are not set locally or globally. See ?gert::git_config().")
# }
# gert::git_commit_all(message = "create project")
# gert::git_config_set(name = "core.sharedRepository", value = "0775")
#
# gert::git_push(
#   remote = sprintf("git@%s/%s.git", git_remote, basename(proj)),
#   set_upstream = sprintf("git@%s/%s.git", git_remote, basename(proj))
# )
# gert::git_remote_add(url = sprintf("git@%s/%s.git", git_remote, basename(proj)))

#### Not Initi Git in Bash ####
# commande dans Bash to do :
# cd /Isiprod1/project/<project>/
# git init
# git remote add origin git@gitlab.egid.local:BioStats/<project>.git
# git add --all
# git commit -m "create project"
## git push origin master # ? no main
# git push origin main
