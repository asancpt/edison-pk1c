#!/SYSTEM/R/3.3.2/bin/Rscript
# setup ----

source('R/function.R', encoding = 'UTF-8')
localLibPath <- c("./lib", .libPaths())
if (Get_os() == 'linux') .libPaths(localLibPath)
.libPaths()

library(MASS)
library(tidyr)
library(dplyr)
library(tibble)
library(readr)
library(ggplot2)
library(markdown)
library(knitr)
# install.packages(mylib, lib = localLibPath)
# source('R/input.R', encoding = 'UTF-8')

output <- list()

# args ----------------------------------------------------------------

Args <- commandArgs(trailingOnly = TRUE) # SKIP THIS LINE IN R if you're testing!
if (identical(Args, character(0))) Args <- c("-inp", "data-raw/input.deck")
if (Args[1] == "-inp") InputParameter <- Args[2] # InputPara.inp

inputRaw <- tibble(raw = readLines(InputParameter)) %>% 
  separate(raw, into = c('param', 'value'), sep = '=') %>% 
  mutate(value = sub(' ;', '', value)) %>% 
  mutate_at(vars(param, value), trimws)
input <- as.list(inputRaw$value)
names(input) <- inputRaw$param

inputVec <- c('DH1', 'Time', 'FullCov')
inputLogical <- 'concLog'
inputNum <- names(input)[! names(input) %in% c(inputVec, inputLogical)]

# input[inputVec] <- lapply(input[inputVec], function(x) as.numeric(unlist(strsplit(x, split=","))))
input[inputNum] <- lapply(input[inputNum], function(x) as.numeric(x))
input[inputLogical] <- as.logical(input[inputLogical])

if (length(intersect(dir(), "result")) == 0) {
  system("mkdir result")
}

# calculation ----

Vars <- Init(input$nSubj, 
             input$CL, input$V, 
             input$DH1, input$FullCov, 
             input$Time, input$PropE, 
             input$AddE, input$Jitter)

output$concTable <- Vars$Conc

# plot ----

p <- ggplot(Vars$Conc, aes(x=TIME, y=CONC, group=SUBJ, color=SUBJ)) + 
  geom_line() +
  geom_point(size = 3)

if (input$concLog == FALSE) output$concTimePlot <- p else output$concTimePlot <- p + scale_y_log10()

p <- ggplot(Vars$Conc, aes(x=TIME, y=CONC, group=SUBJ, color=SUBJ)) + 
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~ SUBJ, ncol = 4)

if (input$concLog == FALSE) output$concTimeFacet <- p else output$concTimeFacet <- p + scale_y_log10()

lapply(c('concTimePlot', 'concTimeFacet'), function(x){
  ggsave(filename = paste0('result/', x, '.pdf'), plot = output[[x]], width = 8, height = 6, dpi = 600, units = 'in')
})

system('cd result;for f in *.pdf; do 
       convert -density 300 ./$f ./${f%.pdf}.jpg
       done;cd ..')


# final ----

save(input, inputRaw, output, file = 'result/output.Rdata')

file_doc2 <- "documentation"
knitr::knit(paste0(file_doc2, ".Rmd"), paste0(file_doc2, ".md"))
markdownToHTML(paste0(file_doc2, ".md"), 
               "result/README.html", 
               options = c("toc", "mathjax"), title = 'PK 1comp IV') #, stylesheet = "mycss.css")
               #options = c("toc", "mathjax"), stylesheet = "css/bootstrap.css")

#system(paste0('rm ', file_doc2, '.md'))

sessionInfo()
