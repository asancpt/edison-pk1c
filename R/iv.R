
output$ncarTable <- renderTable({
  Vars <- Init(input$nSubj, input$CL, input$V, input$DH1, input$FullCov, input$Time, input$PropE, input$AddE, input$Jitter)
  resNCA = NonCompart::tabNCA(Vars$Conc, colSubj="SUBJ", colTime="TIME", colConc="CONC", dose=sum(Vars$DH1[,2]), adm="Bolus")
  return(resNCA)
})
