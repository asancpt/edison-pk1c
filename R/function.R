Get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

MakeFullCov = function(BaseCov, OccsCov)
{
  nRow = nrow(BaseCov)
  if (nRow != length(OccsCov)) stop("Make the length of OccsCov same to dim of BaseCov")
  
  DxOccs = diag(OccsCov)
  DxOccs[DxOccs==0] = OccsCov*OccsCov
  return(rbind(cbind(BaseCov, DxOccs), cbind(DxOccs, BaseCov)))
}

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

pk1coma = function(CLo, Vo, Ka, DosingHistory, Time, PropE=0, AddE=0, LLoQ=0, Jitter=0)
{ # CLo=CL/F, Vo=V/F  
  nObs = length(Time)
  Conc = rep(0, nObs)
  ke = CLo/Vo
  
  if (Jitter > 0) Time = round(jitter(Time), Jitter)
  Time[Time < 0] = 0
  
  T1 = Ka/(Ka - ke)/Vo
  DH = DosingHistory[DosingHistory[,2] > 0,,drop=FALSE]
  nAmt = nrow(DH)
  for (i in 1:nAmt) {
    dTime = Time - DH[i, 1]
    dTime2 = dTime[dTime >= 0]
    Conc = Conc + c(rep(0, length(dTime) - length(dTime2)), DH[i,2]*T1*(exp(-ke*dTime2) - exp(-Ka*dTime2)))
  }
  Err1 = rnorm(nObs, mean=0, sd=PropE)
  Err2 = rnorm(nObs, mean=0, sd=AddE)
  
  Conc = Conc + Conc*Err1 + Err2
  Conc[Conc < LLoQ] = 0
  return(cbind(Time, Conc))
}

#nSubj <- 10
#CL<- 30
#V <- 100
#Ka <- 2
#BioA <- 1
##$DH1 <- matrix(c(0, 100000), nrow=1, ncol=2, byrow=TRUE)
#DH1 <- '0, 100000'
#Time <- '0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24'
#PropE <- 0.1
#AddE <- 0.1 
#LLoQ <- 0 
#Jitter <- 1
#FullCov <- '0.04, 0.03, 0.03, 0.04'

Init <- function(nSubj, CL, V, DH1, FullCov, Time, PropE, AddE, Jitter)
{
  Var <- list()
  
  mu <- c(CL, V)
  Time <-eval(parse(text = paste0("c(", Time, ")"))) 
  nObs <- length(Time)
  
  DH1 <- eval(parse(text = paste0("c(", DH1, ")")))
  DH1 <- matrix(DH1, ncol=2, byrow=TRUE) # c(0, 100000)
  
  FullCov <- eval(parse(text = paste0("c(", FullCov, ")")))
  FullCov <- matrix(FullCov, nrow=2) # c(0.04, 0.03, 0.03, 0.04)
  
  rpk <- MASS::mvrnorm(nSubj, rep(0, 2), FullCov)
  iPK = matrix(rep(mu, nSubj), nrow=nSubj, byrow=TRUE) * exp(rpk)
  
  ColNames = c("SUBJ", "nTIME", "TIME", "CONC")
  Conc = matrix(nrow=nSubj*nObs, ncol=length(ColNames))
  colnames(Conc) = ColNames
  Conc = as.data.frame(Conc)
  Conc[,"SUBJ"] = sort(rep(1:nSubj, nObs))
  Conc[,"nTIME"] = rep(Time, nSubj)
  
  for (i in 1:nSubj) {
    cSubj = i
    cCL = iPK[i, 1]
    cV  = iPK[i, 2]
    iConc = pk1cima(cCL, cV, DH1, Time, PropE=PropE, AddE=AddE, LLoQ=0, Jitter=Jitter)
    Conc[Conc$SUBJ == cSubj, c("TIME", "CONC")] = iConc
  }
  
  Conc[,"CONC"] = round(Conc[,"CONC"],3) 
  Conc[,"SUBJ"] = as.factor(Conc[,"SUBJ"]) 
  
  Var$DH1 <- DH1
  Var$Time <- Time
  Var$Conc <- Conc
  
  return(Var)
}
