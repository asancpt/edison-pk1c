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
plot(x1[,"Time"], x1[,"Conc"], type="o")

library(ncar)
sNCA(x1[,"Time"], x1[,"Conc"], dose=100000)

DH2 = matrix(c(0, 100000, 12, 5000), nrow=2, ncol=2, byrow=TRUE)
x2 = pk1coma(CL, V, Ka, BioA=1, DH2, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2)
windows()
plot(x2[,"Time"], x2[,"Conc"], type="o")

## sim PK para
# setwd("D:/Rt/pksim")
FullCov = read.csv("data-raw/FullCov2-1coma.csv")
chol(FullCov)

mu = c(20, 20, 100, 100, 1.9, 2.1)

nSubj = 36
nTrt = 2

Time = c(0, 0.25, 0.5, 0.75, 1, 2, 3, 4, 6, 8, 10, 12, 24)
nObs = length(Time)
TRT = c("R", "T")
seqGRP = sample(rep(c("RT", "TR"), nSubj))

library(MASS)
rpk = mvrnorm(nSubj*nTrt, rep(0, 6), FullCov) ; rpk
iPK = matrix(rep(mu, nSubj*nTrt), nrow=nSubj*nTrt, byrow=TRUE) * exp(rpk) ; iPK

ColNames = c("SUBJ", "GRP", "PRD", "TRT", "nTIME", "TIME", "CONC")
Conc = matrix(nrow=nSubj*nTrt*nObs, ncol=length(ColNames))
colnames(Conc) = ColNames
Conc = as.data.frame(Conc)
Conc[,"SUBJ"] = sort(rep(1:nSubj, nTrt*nObs)) # Conc
Conc[,"GRP"] = seqGRP[Conc[,"SUBJ"]] # Conc
Conc[,"PRD"] = rep(sort(rep(1:2, nObs)), nSubj) # Conc
Conc[,"nTIME"] = rep(Time, nSubj*nTrt) # Conc
Conc[Conc$GRP=="RT" & Conc$PRD==1,"TRT"] = "R"
Conc[Conc$GRP=="RT" & Conc$PRD==2,"TRT"] = "T"
Conc[Conc$GRP=="TR" & Conc$PRD==1,"TRT"] = "T"
Conc[Conc$GRP=="TR" & Conc$PRD==2,"TRT"] = "R"
# Conc

for (i in 1:nSubj) {
  for (j in 1:nTrt) {
    cSUBJ = i
    cTRT = TRT[j]
    if (cTRT == "R") {
      cCL = iPK[i, 1]
      cV  = iPK[i, 3]
      cKa = iPK[i, 5]
      BA = 1
    } else {
      cCL = iPK[i, 2]
      cV  = iPK[i, 4]
      cKa = iPK[i, 6]
      BA = 0.95
    }
    iConc = pk1coma(cCL, cV, cKa, BioA=BA, DH1, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2)
    Conc[Conc$SUBJ == cSUBJ & Conc$TRT == cTRT, c("TIME", "CONC")] = round(iConc, 2)
  }
}

Conc
write.csv(Conc, "Conc.csv", row.names=FALSE, quote=FALSE)

missSubj = sample(1:nSubj, 3) ; missSubj

Conc = Conc[Conc$SUBJ != missSubj,]

unique(Conc[,"SUBJ"])

Conc[,"ID"] = paste0(Conc[,"SUBJ"], Conc[,"TRT"])

library(ncar)
resNCA = tabNCA(Conc, colSubj="ID", colTime="TIME", colConc="CONC", dose=100000)
#rtfNCA("NCAres.rtf", Conc, colSubj="ID", colTime="TIME", colConc="CONC", dose=100000)

Rand = unique(Conc[,c("ID", "SUBJ","GRP","PRD","TRT")])
resBE = merge(resNCA[,c("ID","TMAX","CMAX","AUCLST")], Rand, by="ID")
resBE = resBE[order(resBE[,"SUBJ"], resBE[,"PRD"]),] ; resBE

colnames(resBE) = c("ID", "Tmax", "Cmax", "AUClast", "SUBJ", "GRP", "PRD", "TRT")
write.csv(resBE[,c("SUBJ", "GRP", "PRD", "TRT", "AUClast", "Cmax", "Tmax")], "BE-NCA.csv", row.names=FALSE, quote=FALSE)

source("~/shanmdphd/belib/R/BELIB2.R")
kbe("BE-NCA.csv")


