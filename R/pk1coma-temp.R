




pk1coma <- function(CL, V, Ka, BioA=1, DosingHistory, Time, PropE=0, AddE=0, LLoQ=0, Jitter=0)

pk1coma(10, 5, 1, BioA = 1, input$DosingHistory, input$Time)

# chol(MakeFullCov(BaseCov, OccsCov))

x1 <- pk1coma(input$CL, 
              input$V, 
              input$Ka, 
              input$BioA, 
              input$DosingHistory, 
              input$Time, 
              input$PropE, 
              input$AddE, 
              input$LLoQ, 
              input$Jitter)

# windows()
# plot(x1[,"Time"], x1[,"Conc"], type="o")
sNCA(x1[,"Time"], x1[,"Conc"], dose=100000)

FullCov <- read_csv('CL1,CL2,V1,V2,Ka1,Ka2
0.06,0.025,0.025,0.02,0.025,0.02
0.025,0.06,0.02,0.025,0.02,0.025
0.025,0.02,0.09,0.025,0.025,0.02
0.02,0.025,0.025,0.09,0.02,0.025
0.025,0.02,0.025,0.02,0.16,0.025
0.02,0.025,0.02,0.025,0.025,0.16')

chol(FullCov)
