# -----------------------------------------------------------------------------------------------------------
# Script: ACTE.R  
# Fits a multivariate Cholesky ACTE Decomposition
# ZYG: 			'MZ'=MZ;'DZ'=DZ
# RCOMP: 'Good' or 'Poor'
# ------------------------------------------------------------------------------------------------------------
#Load libraries and functions
require(OpenMx)
require(psych)
source("miFunctions.R")
source("GenEpiHelperFunctions.R")
mxOption(NULL,"Default optimizer","SLSQP")
#mxOption(NULL,"Default optimizer","CSOLNP")

for(i in 1){
# Select Variables for Analysis
vars <- c('PIQ','PercSpd','WM','VSTM','PSTM','Decode') # list of variables names
nv <- length(vars) # number of variables
fm <- 3 # family members, defined in dissertation.R file
ntv <- nv*fm # number of total variables
selVars <- paste0(rep(vars, fm), rep(1:fm, each = nv))

# Select Data for Analysis
mzData <- subset(subData, zyg=='MZ' & RCOMP=='Good', selVars)
dzData <- subset(subData, zyg=='DZ' & RCOMP=='Good', selVars)

mzcov <- cov(mzData,use="pairwise.complete")
dzcov <- cov(dzData,use="pairwise.complete")

# Set Starting Values
svPa <- sqrt(2*abs(mzcov[1,2]-dzcov[1,2]))
svPct <- sqrt(abs(2*dzcov[1,2]-mzcov[1,2]))
svPe <- sqrt(abs(mzcov[2,2]-mzcov[1,2]))
Stmean <- colMeans(mzData[,c(1:6,1:6,13:18)],na.rm=T)
lbPa <- 0.0001 # lower bound for path coefficient
lbPaD <- valDiagLU(lbPa,-10,NA,nv) # lower bound for diagonal, lower & upper elements of covariance matrix
mLabs	<- paste(paste0("m",selVars[c(1:6,1:6,13:18)]),sep="")

# -------------------------------------------------------------------------------------------------
# (model 2) Specify Cholesky ACTE Decomposition 
# -------------------------------------------------------------------------------------------------

nvalues <- (nv^2+nv)/2

# Matrices declared to store a, c, and e Path Coefficients
pathA  <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa, nv), labels=labLower("a", nv), name="a")
pathC	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPct, nv), labels=labLower("c", nv), name="c")
pathTw <- mxMatrix(type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPct, nv), labels=labLower("tw", nv), name="tw")
pathE	<- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe, nv), labels=labLower("e", nv), name="e")

# Matrices generated to hold A, C, T, and E computed Variance Components
covA	<- mxAlgebra( expression=a %*% t(a), name="A" )
covC	<- mxAlgebra( expression=c %*% t(c), name="C" )
covTw <- mxAlgebra( expression=tw %*% t(tw), name="Tw" )
covE	<- mxAlgebra( expression=e %*% t(e), name="E" )
covP	<- mxAlgebra( expression=A+C+Tw+E, name="V" )
StA	<- mxAlgebra( expression=A/V, name="h2" )
StC	<- mxAlgebra( expression=C/V, name="c2" )
StE	<- mxAlgebra( expression=E/V, name="e2" )
StTw <- mxAlgebra( expression=Tw/V, name="tw2")

# Algebra to compute Correlations
matI	<- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
invSD_chol <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")
a_std <- mxAlgebra(expression = iSD %*% a, name = "std_a")
c_std <- mxAlgebra(expression = iSD %*% c, name = "std_c")
tw_std <- mxAlgebra(expression = iSD %*% tw, name = "std_tw")
e_std <- mxAlgebra(expression = iSD %*% e, name = "std_e")

# Calculate genetic and environmental correlations
Rph <- mxAlgebra( expression=solve(sqrt(I*V))%&%V, name ="Phcor") #rP
Rg <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="Acor" ) #cov2cor() #tää on rG
Rc <- mxAlgebra( expression=solve(sqrt(I*C))%&%C, name ="Ccor" )
Rt <- mxAlgebra( expression=solve(sqrt(I*Tw))%&%Tw, name ="Twcor" )
Re <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="Ecor" )

# Algebra for expected Mean and Variance/Covariance Matrices in MZ & DZ twins

MeanMZ	<- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=Stmean, labels=mLabs, name="ExpMeanMZ" ) ### labels ottaa sisälleen yhtä monta kuin mitä perheenjäseniä on

covMZ	<- mxAlgebra( expression= rbind( cbind(A+C+Tw+E , A+C+Tw, 0.5%x%A+C),
                                       cbind(A+C+Tw , A+C+Tw+E, 0.5%x%A+C),
                                       cbind(0.5%x%A+C, 0.5%x%A+C, A+C+Tw+E)),		name="ExpCovMZ" )

covDZ	<- mxAlgebra( expression= rbind( cbind(A+C+Tw+E, 0.5%x%A+C+Tw, 0.5%x%A+C),
                                       cbind(0.5%x%A+C+Tw , A+C+Tw+E, 0.5%x%A+C),
                                       cbind(0.5%x%A+C, 0.5%x%A+C, A+C+Tw+E)),	name="ExpCovDZ" )

# Data objects for Multiple Groups
dataMZ	<- mxData( observed=mzData, type="raw" )
dataDZ	<- mxData( observed=dzData, type="raw" )

# Objective objects for Multiple Groups
objMZ	<- mxExpectationNormal( covariance="ExpCovMZ", means="ExpMeanMZ", dimnames=selVars )
objDZ	<- mxExpectationNormal( covariance="ExpCovDZ", means="ExpMeanMZ", dimnames=selVars )

fitFunction <- mxFitFunctionML()

# Create Confidence Interval Objects
cinames <- c('Phcor', 'Acor')
ciCor <- c() #correlations
for (n in cinames) {
  for (i in 1:6) {
    for (k in i:6) {
      ciCor <- append(ciCor, paste0(n,"[",i,",",k,"]"))
    }
  }
}

heritabilitynames <- c('h2', 'c2', 'tw2', 'e2')
ciHer <- c() #heritabilities
for (n in heritabilitynames) {
  for (i in 1:6) {
    ciHer <- append(ciHer, paste0(n,"[",i,",",i,"]"))
  }
}

ciStd <- c("std_a", "std_c", "std_tw", "std_e")

CIs <- mxCI(c(ciStd, ciCor, ciHer))

# Combine Groups
pars	<- list( pathA, pathC, pathTw, pathE, covA, covC, covE, covP, covTw, matI, invSD_chol, StA, StC, StE, StTw, a_std, c_std, tw_std, e_std, Rph, Rg, Rc, Re, Rt) 
modelMZ	<- mxModel( pars, covMZ, MeanMZ, dataMZ, objMZ, fitFunction, name="MZ" )
modelDZ	<- mxModel( pars, covDZ, MeanMZ, dataDZ, objDZ, fitFunction, name="DZ" )
multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
minus2ll<- mxAlgebra( expression=MZ.objective + DZ.objective, name="m2LL" )
obj	<- mxFitFunctionAlgebra( "m2LL" )
CholACTEModel<- mxModel( "CholACTE", pars, modelMZ, modelDZ, multi, minus2ll, obj, CIs)
#---------------------------------------------------------------------------------------------------------------------
# (model 2) RUN Cholesky Decomposition ACTE Model
#CholACTEModel <- mxOption(CholACTEModel, "Standard Errors", "Yes")
#CholACTEModel <- mxOption(CholACTEModel, "Calculate Hessian", "Yes")
CholACTEFit	<- mxRun(CholACTEModel, intervals=F)

# RUN SUBMODELS
# Run ACTE model with a Common Environmental Factor and a Common Twin Factor
ts <- labLower("tw", nv)[-c(1:nv)]
cs <- labLower("c", nv)[-c(1:nv)]

modelACTEhypo <- mxModel( CholACTEFit, name="ACTEhypo" )
modelACTEhypo <- omxSetParameters( modelACTEhypo, labels=c("a32","a42","a52",cs, ts), free=FALSE, values=0 )
mxOption(model= modelACTEhypo, key="Number of Threads", value= (omxDetectCores() - 1))
CholACTEhypoFit <- mxRun(modelACTEhypo, intervals=T)
}

#print(mxCompare( CholACTEFit, subs <- list(CholACTEhypoFit, CholAEFit)))
#print(summary(CholACTEhypoFit)$parameters)
#print(omxAkaikeWeights(models = c(CholACTEhypoFit, CholAEFit), type = c("AIC", "AICc"), conf.level = 0.95))

set.seed(666)
ACTEboot <- mxBootstrap(CholACTEhypoFit, replications = 2000, verbose = 2L)
ACTEstda <- mxBootstrapEvalByName(name = "std_a", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEstdc <- mxBootstrapEvalByName(name = "std_c", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEstdt <- mxBootstrapEvalByName(name = "std_tw", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEstde <- mxBootstrapEvalByName(name = "std_e", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEAcor <- mxBootstrapEvalByName(name = "Acor", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEPhcor <- mxBootstrapEvalByName(name = "Phcor", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEh2 <- mxBootstrapEvalByName(name = "h2", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEc2 <- mxBootstrapEvalByName(name = "c2", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEt2 <- mxBootstrapEvalByName(name = "tw2", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
ACTEe2 <- mxBootstrapEvalByName(name = "e2", model = ACTEboot, bq = c(0.05, 0.95), replications = 2000, verbose = 2L)
