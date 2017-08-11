input <- list()

input$nSubj <- 10

input$CL<- 30
input$V <- 100

input$Ka <- 2
input$BioA <- 1
#input$DH1 <- matrix(c(0, 100000), nrow=1, ncol=2, byrow=TRUE)
input$DH1 <- '0, 100000'
input$Time <- '0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24'
input$PropE <- 0.1
input$AddE <- 0.1 
input$LLoQ <- 0 
input$Jitter <- 1
input$FullCov <- '0.04, 0.03, 0.03, 0.04'

input$concLog <- FALSE
