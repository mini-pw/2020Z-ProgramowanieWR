library(archivist)

library(archivist)
cacheRepo <- tempfile()
createLocalRepo(cacheRepo)

library(ggplot2)
# objects of class ggplot for which the session_info was archvied
md5plots <- searchInRemoteRepo(
  pattern = c("class:ggplot", "session_info"), 
  intersect = TRUE, repo = "graphGallery", 
  user = "pbiecek", fixed = FALSE
)

plots <- lapply(md5plots, function(pl) {
  loadFromRemoteRepo(
    md5hash = pl, 
    repo = "graphGallery",
    user = "pbiecek",
    value = TRUE
  ) + 
    ggtitle(pl)
})

aread('MarcinKosinski/Museum/3374db20ecaf2fa0d070d')
