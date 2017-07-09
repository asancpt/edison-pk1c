#!/SYSTEM/R/3.3.2/bin/Rscript

# Library -----------------------------------------------------------------

if (sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)") .libPaths("./packrat/lib/x86_64-pc-linux-gnu/3.3.2/")

mylib <- c("ncar", "dplyr", "rmarkdown", "knitr", "markdown")
lapply(mylib, library, character.only = TRUE) # if needed # install.packages(mylib, lib = localLibPath)

# Argument ----------------------------------------------------------------

Args <- commandArgs(trailingOnly = TRUE) # SKIP THIS LINE IN R if you're testing!
if (identical(Args, character(0))) Args <- c("-inp", "input.deck")

if (length(intersect(dir(), "result")) == 0) {
  system("mkdir result")
}

file_doc2 <- "oral01"
knitr::knit(paste0(file_doc2, ".Rmd"), paste0(file_doc2, ".md"))
markdown::markdownToHTML(paste0(file_doc2, ".md"), 
                         "result/README.html", 
                         options = c("toc", "mathjax")) #, stylesheet = "mycss.css")

#browseURL("result/Report_Appendix.html")

