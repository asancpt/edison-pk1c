pk1cima = function(CL, V, DosingHistory, Time, PropE=0, AddE=0, LLoQ=0, Jitter=0)
{
  nObs = length(Time)
  Conc = rep(0, nObs)
  ke = CL/V

  if (Jitter > 0) Time = round(jitter(Time), Jitter)
  Time[Time < 0] = 0

  DH = DosingHistory[DosingHistory[,2] > 0,,drop=FALSE]
  nAmt = nrow(DH)
  for (i in 1:nAmt) {
    cAmt = DH[i, 2]
    dTime = Time - DH[i, 1]
    dTime2 = dTime[dTime >= 0]
    Conc = Conc + c(rep(0, length(dTime) - length(dTime2)), cAmt/V*exp(-ke*dTime2))
  }

  Err1 = rnorm(nObs, mean=0, sd=PropE)
  Err2 = rnorm(nObs, mean=0, sd=AddE)

  Conc = Conc + Conc*Err1 + Err2
  Conc[Conc < LLoQ] = 0
  return(cbind(Time, Conc))
}

CL = 20
V = 100
Time = c(0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24)

DH1 = matrix(c(0, 100000), nrow=1, ncol=2, byrow=TRUE)
x1 = pk1cima(CL, V, DH1, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2) ; x1

plot(x1[,"Time"], x1[,"Conc"], type="o")
library(ncar)
sNCA(x1[,"Time"], x1[,"Conc"], dose=100000, adm="Bolus")

DH2 = matrix(c(0, 100000, 12, 5000), nrow=2, ncol=2, byrow=TRUE)
pk1cima(CL, V, DH2, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2)

# For multiple subject simulation
BaseCov = matrix(c(0.04, 0.02, 0.02, 0.04), nrow=2)
OccsCov = c(0.03, 0.03)

MakeFullCov = function(BaseCov, OccsCov)
{
  nRow = nrow(BaseCov)
  if (nRow != length(OccsCov)) stop("Make the length of OccsCov same to dim of BaseCov")

  DxOccs = diag(OccsCov)
  DxOccs[DxOccs==0] = OccsCov*OccsCov
  return(rbind(cbind(BaseCov, DxOccs), cbind(DxOccs, BaseCov)))
}
chol(MakeFullCov(BaseCov, OccsCov))

library(MASS)
#x2 = mvrnorm(100, mu=c(0,0), Sigma=BaseCov)
#x3 = mvrnorm(100, mu=c(0,0), Sigma=BaseCov)
#L = matrix(c(1, 0.75, 0, sqrt(1-0.75*0.75)), nrow=2)
#Y = cbind(x2[,1], x2[,2]) %*% L
#cov(Y)


FullCov = matrix(c(0.04, 0.03, 0.03, 0.04), nrow=2)
chol(FullCov) # Check for Positive Definite

x4 = mvrnorm(100, mu=rep(0, 2), Sigma=FullCov)
c5 = cov(x4)
chol(c5)

nSubj = 12
Time = c(0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24)
nObs = length(Time)
mu = c(20, 100)
rpk = mvrnorm(nSubj, rep(0, 2), FullCov) ; rpk
iPK = matrix(rep(mu, nSubj), nrow=nSubj, byrow=TRUE) * exp(rpk) ; iPK

ColNames = c("SUBJ", "nTIME", "TIME", "CONC")
Conc = matrix(nrow=nSubj*nObs, ncol=length(ColNames))
colnames(Conc) = ColNames
Conc = as.data.frame(Conc)
Conc[,"SUBJ"] = sort(rep(1:nSubj, nObs))
Conc[,"nTIME"] = rep(Time, nSubj) ; Conc

for (i in 1:nSubj) {
  cSubj = i
  cCL = iPK[i, 1]
  cV  = iPK[i, 2]
  iConc = pk1cima(cCL, cV, DH1, Time, PropE=0.1, AddE=1, LLoQ=0, Jitter=2)
  Conc[Conc$SUBJ == cSubj, c("TIME", "CONC")] = iConc
}

Conc[,"CONC"] = round(Conc[,"CONC"],3) ; Conc

library(lattice)
xyplot(CONC ~ TIME | as.factor(SUBJ), data=Conc, type="o")

library(ncar)
resNCA = tabNCA(Conc, colSubj="SUBJ", colTime="TIME", colConc="CONC", dose=100000, adm="Bolus")

write.csv(Conc, "1CompIV-SimConc.csv", row.names=FALSE, quote=FALSE)
write.csv(resNCA, "1CompIV-NCA-result.csv", row.names=FALSE, quote=FALSE)

# Hint from real data
be1 = read.csv("~/shanmdphd/belib/data-raw/Cmax.csv") # Where's the file?

x1 = be1[be1$TRT=="R" & be1$CMAX > 0, c("SUBJ","CMAX")] ; x1
x2 = be1[be1$TRT=="T" & be1$CMAX > 0, c("SUBJ","CMAX")] ; x2

x3 = merge(x1, x2, by = "SUBJ")
cor(log(x3$CMAX.x), log(x3$CMAX.y))
cov(log(x3$CMAX.x), log(x3$CMAX.y))
