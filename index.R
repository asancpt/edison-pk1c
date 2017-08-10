#!/SYSTEM/R/3.3.2/bin/Rscript

# Library -----------------------------------------------------------------

if (Sys.info()['sysname'] == 'Linux') .libPaths("./lib")

mylib <- c('markdown', 'knitr')
lapply(mylib, library, character.only = TRUE) # if needed # install.packages(mylib, lib = localLibPath)

# Argument ----------------------------------------------------------------

Args <- commandArgs(trailingOnly = TRUE) # SKIP THIS LINE IN R if you're testing!
if (identical(Args, character(0))) Args <- c("-inp", "input.deck")

if (length(intersect(dir(), "result")) == 0) {
  system("mkdir result")
}

file_doc2 <- "documentation"
knitr::knit(paste0(file_doc2, ".Rmd"), paste0(file_doc2, ".md"))
markdownToHTML(paste0(file_doc2, ".md"), 
               "result/README.html", 
               options = c("toc", "mathjax")) #, stylesheet = "mycss.css")

system(paste0('rm ', file_doc2, '.md'))

