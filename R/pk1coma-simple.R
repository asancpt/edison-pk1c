# simOral1comp ----

pk1coma = function(CL, V, Ka, BioA=1, DosingHistory, Time, PropE=0, AddE=0, LLoQ=0, Jitter=0)
{
  nObs = length(Time)
  Conc = rep(0, nObs)
  ke = CL/V

  if (Jitter > 0) Time = round(jitter(Time), Jitter)
  Time[Time < 0] = 0

  TERM1 = BioA*Ka/(Ka - ke)/V
  DH = DosingHistory[DosingHistory[,2] > 0,,drop=FALSE]
  nAmt = nrow(DH)
  for (i in 1:nAmt) {
    TERM2 = DH[i, 2]*TERM1
    dTime = Time - DH[i, 1]
    dTime2 = dTime[dTime >= 0]
    Conc = Conc + c(rep(0, length(dTime) - length(dTime2)), TERM2*(exp(-ke*dTime2) - exp(-Ka*dTime2)))
  }

  Err1 = rnorm(nObs, mean=0, sd=PropE)
  Err2 = rnorm(nObs, mean=0, sd=AddE)

  Conc = Conc + Conc*Err1 + Err2
  Conc[Conc < LLoQ] = 0
  return(cbind(Time, Conc))
}


CL = 20
V = 100
Ka = 2
Time = c(0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24)

DH1 = matrix(c(0, 100000), nrow=1, ncol=2, byrow=TRUE)

x1 = pk1coma(CL, V, Ka, BioA=1, DH1, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2)
# windows()
# plot(x1[,"Time"], x1[,"Conc"], type="o")

library(ncar)
sNCA(x1[,"Time"], x1[,"Conc"], dose=100000)

