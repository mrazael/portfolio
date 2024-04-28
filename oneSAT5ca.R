# ------------------------------------------------------------------------------
# Program: oneSAT5ca.R
# Author: Hermine Maes
# Date: 05 28 2022
#
# Twin Univariate Saturated model to estimate means and (co)variances across multiple groups
# Matrix style model - Raw data - Continuous data
# -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# Load Libraries & Options
require(OpenMx)
require(psych)
source("miFunctions.R")
source("GenEpiHelperFunctions.R")
mxOption(NULL,"Default optimizer","SLSQP")
# ----------------------------------------------------------------------------------------------------------------------
# PREPARE DATA
# Load Data
dim(twinData)

comparisons <- data.frame(chiPIQ = character(), pPIQ = character(),
                          chiPercSpd = character(), pPercSpd = character(),
                          chiWM = character(), pWM = character(),
                          chiVSTM = character(), pVSTM = character(),
                          chiPSTM = character(), pPSTM = character(),
                          chiDecode = character(), pDecode = character())

trueVars <- c('PIQ','PercSpd','WM','VSTM','PSTM','Decode')
k = 0

for(i in 1:length(trueVars)){
# Select Variables for Analysis
vars = trueVars[i]
nv <- length(vars) # number of variables
fm <- 3 # family members, defined in dissertation.R file
ntv <- nv*fm # number of total variables
selVars <- paste0(rep(vars, fm), rep(1:fm, each = nv))
zygs <- c('MZff', 'MZmm', 'DZff', 'DZmm', 'DZfm', 'DZmf')

# Select Data for Analysis
mzffData <- subset(subData, zyg=='MZFF', selVars)
mzmmData <- subset(subData, zyg=='MZMM', selVars)
dzffData <- subset(subData, zyg=='DZFF', selVars)
dzmmData <- subset(subData, zyg=='DZMM', selVars)
dzfmData <- subset(subData, zyg=='DZFM', selVars)
dzmfData <- subset(subData, zyg=='DZMF', selVars)

# Generate Descriptive Statistics
mzffM <- colMeans(mzffData[1:ntv],na.rm=TRUE)
mzmmM <- colMeans(mzmmData[1:ntv],na.rm=TRUE)
dzffM <- colMeans(dzffData[1:ntv],na.rm=TRUE)
dzmmM <- colMeans(dzmmData[1:ntv],na.rm=TRUE)
dzfmM <- colMeans(dzfmData[1:ntv],na.rm=TRUE)
dzmfM <- colMeans(dzmfData[1:ntv],na.rm=TRUE)
mzffCov <- cov(mzffData[1:ntv],use="complete")
mzmmCov <- cov(mzmmData[1:ntv],use="complete")
dzffCov <- cov(dzffData[1:ntv],use="complete")
dzmmCov <- cov(dzmmData[1:ntv],use="complete")
dzfmCov <- cov(dzfmData[1:ntv],use="complete")
dzmfCov <- cov(dzmfData[1:ntv],use="complete")

# Set Starting Values
svBe <- 0.01 # start value for regressions
svMe <- rep(0, nv) # start value for means
svVa <- rep(2.5, nv) # start value for variance
lbVa <- 0.0001 # start value for lower bounds

# Create Labels for means
mMZff <- labVars("mMZff",selVars)
mMZmm <- labVars("mMZmm",selVars)
mDZff <- labVars("mDZff",selVars)
mDZmm <- labVars("mDZmm",selVars)
mDZfm <- labVars("mDZfm",selVars)
mDZmf <- labVars("mDZmf",selVars)

mMZf <- labVars("mMZf", vars)
mDZf <- labVars("mDZf", vars)
mDZfo <- labVars("mDZfo", vars)
mZf <- labVars("mZf", vars)
mMZm <- labVars("mMZm", vars)
mDZm <- labVars("mDZm", vars)
mDZmo <- labVars("mDZmo", vars)
mZm <- labVars("mZm", vars)
mZo <- labVars("mZo", vars)
mZ <- labVars("mZ",vars)

#Labels for covariance matrices
cvMZff <- labLower("covMZff",ntv)
cvMZmm <- labLower("covMZmm",ntv)
cvDZff <- labLower("covDZff",ntv)
cvDZmm <- labLower("covDZmm",ntv)
cvDZfm <- labLower("covDZfm",ntv)
cvDZmf <- labLower("covDZmf",ntv)

# Diagonal variances, these will be used in "labels" to signify replacements
vMZff <- labDiag('covMZff', ntv)
vMZmm <- labDiag('covMZmm', ntv)
vDZff <- labDiag('covDZff', ntv)
vDZmm <- labDiag('covDZmm', ntv)
vDZfm <- labDiag('covDZfm', ntv)
vDZmf <- labDiag('covDZmf', ntv)

#Intersection of covariances/variances
cMZff <- cvMZff[!cvMZff %in% vMZff]
cMZmm <- cvMZmm[!cvMZmm %in% vMZmm]
cDZff <- cvDZff[!cvDZff %in% vDZff]
cDZmm <- cvDZmm[!cvDZmm %in% vDZmm]
cDZfm <- cvDZfm[!cvDZfm %in% vDZfm]
cDZmf <- cvDZmf[!cvDZmf %in% vDZmf]

vMZf <- paste0('vMZf', rep(vars))
vZf <- paste0('vZf', rep(vars))
vMZm <- paste0('vMZm', rep(vars))
vZm <- paste0('vZm', rep(vars))
vDZo <- paste0('vMZo', rep(vars))
vZo <- paste0('vZo', rep(vars))
vZ <- paste0('vZ', rep(vars))

cMZf <- paste0('cMZf', rep(vars))
cZf <- paste0('cZf', rep(vars))
cMZm <- paste0('cMZm', rep(vars))
cZm <- paste0('cZm', rep(vars))
cDZo <- paste0('cMZo', rep(vars))
cZo <- paste0('cZo', rep(vars))
cZ <- paste0('cZ', rep(vars))

# ----------------------------------------------------------------------------------------------------------------------
# PREPARE MODEL

# Create Algebra for expected Mean Matrices
meanMZff <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=mzffM, labels=mMZff, name="meanMZff" )
meanMZmm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=mzmmM, labels=mMZmm, name="meanMZmm" )
meanDZff <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=dzffM, labels=mDZff, name="meanDZff" )
meanDZmm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=dzmmM, labels=mDZmm, name="meanDZmm" )
meanDZfm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=dzfmM, labels=mDZfm, name="meanDZfm" )
meanDZmf <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=dzmfM, labels=mDZmf, name="meanDZmf" )
expMeanMZff<- mxAlgebra( expression= meanMZff, name="expMeanMZff" )
expMeanMZmm<- mxAlgebra( expression= meanMZmm, name="expMeanMZmm" )
expMeanDZff<- mxAlgebra( expression= meanDZff, name="expMeanDZff" )
expMeanDZmm<- mxAlgebra( expression= meanDZmm, name="expMeanDZmm" )
expMeanDZfm<- mxAlgebra( expression= meanDZfm, name="expMeanDZfm" )
expMeanDZmf<- mxAlgebra( expression= meanDZmf, name="expMeanDZmf" )

# Create Algebra for expected Variance/Covariance Matrices
covMZff <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvMZff, name="covMZff" )
covMZmm <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvMZmm, name="covMZmm" )
covDZff <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvDZff, name="covDZff" )
covDZmm <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvDZmm, name="covDZmm" )
covDZfm <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvDZfm, name="covDZfm" )
covDZmf <- mxMatrix( type="Symm", nrow=ntv, ncol=ntv, free=TRUE, values=valDiag(svVa,ntv), lbound=valDiag(lbVa,ntv), labels=cvDZmf, name="covDZmf" )

# Create Algebra for Maximum Likelihood Estimates of Twin Correlations
matI <- mxMatrix( type="Iden", nrow=ntv, ncol=ntv, name="I" )
corMZff <- mxAlgebra( solve(sqrt(I*covMZff)) %&% covMZff, name="corMZff" )
corMZmm <- mxAlgebra( solve(sqrt(I*covMZmm)) %&% covMZmm, name="corMZmm" )
corDZff <- mxAlgebra( solve(sqrt(I*covDZff)) %&% covDZff, name="corDZff" )
corDZmm <- mxAlgebra( solve(sqrt(I*covDZmm)) %&% covDZmm, name="corDZmm" )
corDZfm <- mxAlgebra( solve(sqrt(I*covDZfm)) %&% covDZfm, name="corDZfm" )
corDZmf <- mxAlgebra( solve(sqrt(I*covDZmf)) %&% covDZmf, name="corDZmf" )

# Create Data Objects for Multiple Groups
dataMZff <- mxData( observed=mzffData, type="raw" )
dataMZmm <- mxData( observed=mzmmData, type="raw" )
dataDZff <- mxData( observed=dzffData, type="raw" )
dataDZmm <- mxData( observed=dzmmData, type="raw" )
dataDZfm <- mxData( observed=dzfmData, type="raw" )
dataDZmf <- mxData( observed=dzmfData, type="raw" )

# Create Expectation Objects for Multiple Groups
expMZff <- mxExpectationNormal( covariance="covMZff", means="expMeanMZff", dimnames=selVars )
expMZmm <- mxExpectationNormal( covariance="covMZmm", means="expMeanMZmm", dimnames=selVars )
expDZff <- mxExpectationNormal( covariance="covDZff", means="expMeanDZff", dimnames=selVars )
expDZmm <- mxExpectationNormal( covariance="covDZmm", means="expMeanDZmm", dimnames=selVars )
expDZfm <- mxExpectationNormal( covariance="covDZfm", means="expMeanDZfm", dimnames=selVars )
expDZmf <- mxExpectationNormal( covariance="covDZmf", means="expMeanDZmf", dimnames=selVars )
funML <- mxFitFunctionML()

# Create Model Objects for Multiple Groups
modelMZff <- mxModel( meanMZff, expMeanMZff, covMZff, matI, corMZff, dataMZff, expMZff, funML, name="MZff" )
modelMZmm <- mxModel( meanMZmm, expMeanMZmm, covMZmm, matI, corMZmm, dataMZmm, expMZmm, funML, name="MZmm" )
modelDZff <- mxModel( meanDZff, expMeanDZff, covDZff, matI, corDZff, dataDZff, expDZff, funML, name="DZff" )
modelDZmm <- mxModel( meanDZmm, expMeanDZmm, covDZmm, matI, corDZmm, dataDZmm, expDZmm, funML, name="DZmm" )
modelDZfm <- mxModel( meanDZfm, expMeanDZfm, covDZfm, matI, corDZfm, dataDZfm, expDZfm, funML, name="DZfm" )
modelDZmf <- mxModel( meanDZmf, expMeanDZmf, covDZmf, matI, corDZmf, dataDZmf, expDZmf, funML, name="DZmf" )
multi <- mxFitFunctionMultigroup( c("MZff","MZmm","DZff","DZmm","DZfm","DZmf") )

# Create Confidence Interval Objects
ciCov <- mxCI( c('MZff.covMZff','MZmm.covMZmm','DZff.covDZff','DZmm.covDZmm','DZfm.covDZfm','DZmf.covDZmf' ))
ciMean <- mxCI( c('MZff.expMeanMZff','MZmm.expMeanMZmm','DZff.expMeanDZff','DZmm.expMeanDZmm','DZfm.expMeanDZfm','DZmf.expMeanDZmf' ))

# Build Saturated Model with Confidence Intervals
modelSAT <- mxModel( "multiSAT", modelMZff, modelMZmm, modelDZff, modelDZmm, modelDZfm, modelDZmf, multi, ciCov, ciMean )
# ----------------------------------------------------------------------------------------------------------------------
# RUN MODEL
# Run Saturated Model
#modelSAT <- mxOption(modelSAT, "Standard Errors", "No")
#modelSAT <- mxOption(modelSAT, "Calculate Hessian", "No")
fitSAT <- mxTryHard( modelSAT, intervals=F, extraTries = 5 )
(sumSAT <- summary( fitSAT ))
# ----------------------------------------------------------------------------------------------------------------------
# RUN SUBMODELS

###############################
########### MEANS #############
###############################

# Constrain expected Means to be equal across twins 1 and 2
for(i in 1){
modelEMO <- mxModel( fitSAT, name="EMO" )
modelEMO <- omxSetParameters( modelEMO, label=mMZff[-3], free=TRUE, values=mzffM[1:nv], newlabels=mMZf )
modelEMO <- omxSetParameters( modelEMO, label=mMZmm[-3], free=TRUE, values=mzmmM[1:nv], newlabels=mMZm )
modelEMO <- omxSetParameters( modelEMO, label=mDZff[-3], free=TRUE, values=dzffM[1:nv], newlabels=mDZf )
modelEMO <- omxSetParameters( modelEMO, label=mDZmm[-3], free=TRUE, values=dzmmM[1:nv], newlabels=mDZm )
modelEMO <- omxSetParameters( modelEMO, label=mDZfm[-3], free=TRUE, values=dzfmM[1:nv], newlabels=mDZfo)
modelEMO <- omxSetParameters( modelEMO, label=mDZmf[-3], free=TRUE, values=dzmfM[1:nv], newlabels=mDZmo)
#modelEMO <- mxAutoStart(modelEMO)
fitEMO <- mxTryHard( modelEMO, intervals=F, extraTries = 5)}

# Constrain expected Means to be equal across twins and siblings
for(i in 1){
modelEMS <- mxModel( fitEMO, name="EMS" )
modelEMS <- omxSetParameters( modelEMS, label=c(mMZf, mMZff[3]), free=TRUE, values=mzffM[1:nv], newlabels=mMZf )
modelEMS <- omxSetParameters( modelEMS, label=c(mMZm, mMZmm[3]), free=TRUE, values=mzmmM[1:nv], newlabels=mMZm )
modelEMS <- omxSetParameters( modelEMS, label=c(mDZf, mDZff[3]), free=TRUE, values=dzffM[1:nv], newlabels=mDZf )
modelEMS <- omxSetParameters( modelEMS, label=c(mDZm, mDZmm[3]), free=TRUE, values=dzmmM[1:nv], newlabels=mDZm )
modelEMS <- omxSetParameters( modelEMS, label=c(mDZfo, mDZfm[3]), free=TRUE, values=dzfmM[1:nv], newlabels=mDZfo)
modelEMS <- omxSetParameters( modelEMS, label=c(mDZmo, mDZmf[3]), free=TRUE, values=dzmfM[1:nv], newlabels=mDZmo)
#modelEMO <- mxAutoStart(modelEMO)
fitEMS <- mxTryHard( modelEMS, intervals=F, extraTries = 5)}

# Constrain expected Means to be equal across zygosity
for(i in 1){
  modelEMZ <- mxModel( fitEMS, name="EMZ" )
  modelEMZ <- omxSetParameters( modelEMZ, label=c(mMZf, mDZf), free=TRUE, values=svMe, newlabels=mZf )
  modelEMZ <- omxSetParameters( modelEMZ, label=c(mMZm, mDZm), free=TRUE, values=svMe, newlabels=mZm )
  modelEMZ <- omxSetParameters( modelEMZ, label=c(mDZfo, mDZmo), free=TRUE, values=svMe, newlabels=mZo )
  fitEMZ <- mxTryHard( modelEMZ, intervals=F, extraTries = 5 ) }

# Constrain expected Means to be equal across SS/OS families
for(i in 1){
  modelEMP <- mxModel( fitEMZ, name="EMP" )
  modelEMP <- omxSetParameters( modelEMP, label=c(mZf, mZm), free=TRUE, values=svMe, newlabels=mZf )
  modelEMP <- omxSetParameters( modelEMP, label=c(mZf,mZo), free=TRUE, values=svMe, newlabels=mZ )
  fitEMP <- mxTryHard( modelEMP, intervals=F, extraTries = 5 )}

###############################
######### VARIANCES ###########
###############################

# Constrain expected Variances to be equal across twin order
for(i in 1){
modelEVO <- mxModel( fitEMP, name="EVO" )
modelEVO <- omxSetParameters( modelEVO, label=vMZff[-3], free=TRUE, values=svVa, newlabels=vMZff[1:nv] )
modelEVO <- omxSetParameters( modelEVO, label=vMZmm[-3], free=TRUE, values=svVa, newlabels=vMZmm[1:nv] )
modelEVO <- omxSetParameters( modelEVO, label=vDZff[-3], free=TRUE, values=svVa, newlabels=vDZff[1:nv] )
modelEVO <- omxSetParameters( modelEVO, label=vDZmm[-3], free=TRUE, values=svVa, newlabels=vDZmm[1:nv] )
modelEVO <- omxSetParameters( modelEVO, label=vDZfm[-3], free=TRUE, values=svVa, newlabels=vDZfm[1:nv] )
modelEVO <- omxSetParameters( modelEVO, label=vDZmf[-3], free=TRUE, values=svVa, newlabels=vDZmf[1:nv] )
#modelEMVO <- omxAssignFirstParameters(modelEMVO) #produces error without this, multiple same lower bound values
fitEVO <- mxTryHard( modelEVO, intervals = F, extraTries = 5)}

# Constrain expected Variances to be equal across twins and siblings
for(i in 1){
modelEVS <- mxModel( fitEVO, name="EVS" )
modelEVS <- omxSetParameters( modelEVS, label=c(vMZff[1],vMZff[3]), free=TRUE, values=svVa, newlabels=vMZff[1:nv] )
modelEVS <- omxSetParameters( modelEVS, label=c(vMZmm[1],vMZmm[3]), free=TRUE, values=svVa, newlabels=vMZmm[1:nv] )
modelEVS <- omxSetParameters( modelEVS, label=c(vDZff[1],vDZff[3]), free=TRUE, values=svVa, newlabels=vDZff[1:nv] )
modelEVS <- omxSetParameters( modelEVS, label=c(vDZmm[1],vDZmm[3]), free=TRUE, values=svVa, newlabels=vDZmm[1:nv] )
modelEVS <- omxSetParameters( modelEVS, label=c(vDZfm[1],vDZfm[3]), free=TRUE, values=svVa, newlabels=vDZfm[1:nv] )
modelEVS <- omxSetParameters( modelEVS, label=c(vDZmf[1],vDZmf[3]), free=TRUE, values=svVa, newlabels=vDZmf[1:nv] )
fitEVS <- mxTryHard( modelEVS, intervals = F, extraTries = 5)}

# Constrain expected Variances to be equal across zygosity
for(i in 1){
  modelEVZ <- mxModel( fitEVS, name="EVZ" )
  modelEVZ <- omxSetParameters( modelEVZ, label=c(vMZff[1:nv], vDZff[1:nv]), free=TRUE, values=mzffCov[1], newlabels=vZf )
  modelEVZ <- omxSetParameters( modelEVZ, label=c(vMZmm[1:nv], vDZmm[1:nv]), free=TRUE, values=mzffCov[1], newlabels=vZm )
  modelEVZ <- omxSetParameters( modelEVZ, label=c(vDZfm[1:nv], vDZmf[1:nv]), free=TRUE, values=mzffCov[1], newlabels=vZo )
  fitEVZ <- mxTryHard( modelEVZ, intervals=F, extraTries = 5 ) }

# Constrain expected Variances to be equal across SS/OS families
for(i in 1){
  modelEVP <- mxModel( fitEVZ, name="EVP" )
  modelEVP <- omxSetParameters( modelEVP, label=c(vZf, vZm), free=TRUE, values=mzffCov[1], newlabels=vZf )
  modelEVP <- omxSetParameters( modelEVP, label=c(vZf,vZo), free=TRUE, values=mzffCov[1], newlabels=vZ )
  fitEVP <- mxTryHard( modelEVP, intervals=F, extraTries = 5 )}

###############################
######## COVARIANCES ##########
###############################

# Constrain expected covariances to be equal across MZ-Sib, DZ-Sib and DZ-DZ
for(i in 1){
  modelECS <- mxModel( fitEVP, name="ECS" )
  modelECS <- omxSetParameters( modelECS, label=c(cMZff[2],cMZff[3]), free=TRUE, values=0.5, lbound=lbVa, newlabels=cMZff[3] )
  modelECS <- omxSetParameters( modelECS, label=c(cMZmm[2],cMZmm[3]), free=TRUE, values=0.5, lbound=lbVa, newlabels=cMZmm[3] )
  # modelECS <- omxSetParameters( modelECS, label=c(cDZff[2],cDZff[3]), free=TRUE, values=dzffCov[3,2], lbound=lbVa, newlabels=cDZff[3] )
  # modelECS <- omxSetParameters( modelECS, label=c(cDZmm[2],cDZmm[3]), free=TRUE, values=dzmmCov[3,2], lbound=lbVa, newlabels=cDZmm[3] )
  # modelECS <- omxSetParameters( modelECS, label=c(cDZfm[2],cDZfm[3]), free=TRUE, values=dzfmCov[3,2], lbound=lbVa, newlabels=cDZfm[3] )
  # modelECS <- omxSetParameters( modelECS, label=c(cDZmf[2],cDZmf[3]), free=TRUE, values=dzmfCov[3,2], lbound=lbVa, newlabels=cDZmf[3] )
  modelECS <- omxSetParameters( modelECS, label=cDZff, free=TRUE, values=0.5, lbound=lbVa, newlabels=cDZff[3] ) ###
  modelECS <- omxSetParameters( modelECS, label=cDZmm, free=TRUE, values=0.5, lbound=lbVa, newlabels=cDZmm[3] ) ###
  modelECS <- omxSetParameters( modelECS, label=cDZfm, free=TRUE, values=0.5, lbound=lbVa, newlabels=cDZfm[3] ) ###
  modelECS <- omxSetParameters( modelECS, label=cDZmf, free=TRUE, values=0.5, lbound=lbVa, newlabels=cDZmf[3] )
  fitECS <- mxTryHard( modelECS, intervals = F, extraTries = 5)}

# Constrain expected covariances to be equal across DZ twins
# for(i in 1){
#   modelECDZ <- mxModel( fitECS, name="ECDZ" )
#   modelECDZ <- omxSetParameters( modelECDZ, label=c(cDZff[1],cDZff[3]), free=TRUE, values=dzffCov[3,2], lbound=lbVa, newlabels=cDZff[3] )
#   modelECDZ <- omxSetParameters( modelECDZ, label=c(cDZmm[1],cDZmm[3]), free=TRUE, values=dzmmCov[3,2], lbound=lbVa, newlabels=cDZmm[3] )
#   modelECDZ <- omxSetParameters( modelECDZ, label=c(cDZfm[1],cDZfm[3]), free=TRUE, values=dzfmCov[3,2], lbound=lbVa, newlabels=cDZfm[3] )
#   modelECDZ <- omxSetParameters( modelECDZ, label=c(cDZmf[1],cDZmf[3]), free=TRUE, values=dzmfCov[3,2], lbound=lbVa, newlabels=cDZmf[3] )
#   modelECDZ <- mxAutoStart(modelECDZ)
#   fitECDZ <- mxTryHard( modelECDZ, intervals = F, extraTries = 5)}

# Constrain expected Covariances be equal across SS/OS families
for(i in 1){
  modelECF <- mxModel( fitECS, name="ECF" )
  modelECF <- omxSetParameters( modelECF, label=c(cMZff[3],cMZmm[3]), free=TRUE, values=mzffCov[3,2], lbound=lbVa, newlabels=cZm )
  modelECF <- omxSetParameters( modelECF, label=c(cDZff[3],cDZmm[3],cDZfm[3],cDZmf[3]), free=TRUE, lbound=lbVa, values=dzffCov[3,2], newlabels=cZo )
  fitECF <- mxTryHard( modelECF, intervals=F, extraTries = 5 )}

# Constrain expected Covariances to be equal across zygosity
for(i in 1){
  modelECZ <- mxModel( fitECF, name="ECZ" )
  modelECZ <- omxSetParameters( modelECZ, label=c(cZm, cMZff[1], cMZmm[1], cZo), free=TRUE, values=mzffCov[3,2], lbound=lbVa, newlabels=cZ )
  fitECZ <- mxTryHard( modelECZ, intervals=F, extraTries = 5 ) }

# Constrain expected Covariances to be zero
for(i in 1){
  modelEC0 <- mxModel( fitECZ, name="EC0" )
  modelEC0 <- omxSetParameters( modelEC0, label=cZ, free=FALSE, values=0 )
  fitEC0 <- mxTryHard( modelEC0, intervals=F, extraTries = 5 ) }

# # Constrain expected covariances to be equal across twins and siblings
# for(i in 1){
#   modelECP <- mxModel( fitECDZ, name="ECP" )
#   modelECP <- omxSetParameters( modelECP, label=cMZff, strict = FALSE, free=TRUE, values=mzffCov[3,2], lbound=lbVa, newlabels=cMZff[1] )
#   modelECP <- omxSetParameters( modelECP, label=cMZmm, strict = FALSE, free=TRUE, values=mzmmCov[3,2], lbound=lbVa, newlabels=cMZmm[1] )
#   modelECP <- omxSetParameters( modelECP, label=cDZff, strict = FALSE, free=TRUE, values=dzffCov[3,2], lbound=lbVa, newlabels=cDZff[1] )
#   modelECP <- omxSetParameters( modelECP, label=cDZmm, strict = FALSE, free=TRUE, values=dzmmCov[3,2], lbound=lbVa, newlabels=cDZmm[1] )
#   modelECP <- omxSetParameters( modelECP, label=cDZfm, strict = FALSE, free=TRUE, values=dzfmCov[3,2], lbound=lbVa, newlabels=cDZfm[1] )
#   modelECP <- omxSetParameters( modelECP, label=cDZmf, strict = FALSE, free=TRUE, values=dzmfCov[3,2], lbound=lbVa, newlabels=cDZmf[1] )
#   modelECP <- mxAutoStart(modelECP)
#   fitECP <- mxTryHard( modelECP, intervals = F, extraTries = 5)}
# 
# # Constrain expected Covariances be equal across SS/OS families
# for(i in 1){
#   modelECF <- mxModel( fitECF, name="ECF" )
#   modelECF <- omxSetParameters( modelECF, label=c(cMZff[1],cMZmm[1]), free=TRUE, values=mzffCov[3,2], newlabels=cZm )
#   modelECF <- omxSetParameters( modelECF, label=c(cDZff[1],cDZmm[1],cDZfm[1],cDZmf[1]), free=TRUE, values=dzffCov[3,2], newlabels=cZo )
#   fitECF <- mxTryHard( modelECF, intervals=F, extraTries = 5 )}
# 
# # Constrain expected Covariances to be equal across zygosity
# for(i in 1){
#   modelECZ <- mxModel( fitECF, name="ECZ" )
#   modelECZ <- omxSetParameters( modelECZ, label=c(cZm, cZo), free=TRUE, values=mzffCov[3,2], newlabels=cZm )
#   fitECZ <- mxTryHard( modelECZ, intervals=F, extraTries = 5 ) }

# # Constrain expected covariances to be equal opposite-sex and same-sex families
# for(i in 1){
#   modelECS <- mxModel( fitEVP, name="ECS" )
#   modelECS <- omxSetParameters( modelECS, label=cDZff, free=TRUE, values=dzffCov[2], lbound=lbVa, newlabels=cDZff[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZmm, free=TRUE, values=dzmmCov[2], lbound=lbVa, newlabels=cDZmm[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZfm, free=TRUE, values=dzfmCov[2], lbound=lbVa, newlabels=cDZfm[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZmf, free=TRUE, values=dzmfCov[2], lbound=lbVa, newlabels=cDZmf[1:nv] )
#   modelECS <- mxAutoStart(modelECS)
#   fitECS <- mxTryHard( modelECS, intervals = F, extraTries = 5)}
# 
# # Constrain expected Covariances be equal across SS/OS families asdasdasdsa
# for(i in 1){
#   modelECP <- mxModel( fitECZ, name="ECP" )
#   modelECP <- omxSetParameters( modelECP, label=c(cZf, cZm), free=TRUE, values=mzffCov[2], newlabels=cZf )
#   modelECP <- omxSetParameters( modelECP, label=c(cZf,cZo), free=TRUE, values=mzffCov[2], newlabels=cZ )
#   fitECP <- mxTryHard( modelECP, intervals=F, extraTries = 5 )}
# 
# # Constrain expected covariances to be equal across twins and siblings
# for(i in 1){
#   modelECS <- mxModel( fitEVP, name="ECS" )
#   modelECS <- omxSetParameters( modelECS, label=cMZff, free=TRUE, values=mzffCov[2], lbound=lbVa, newlabels=cMZff[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cMZmm, free=TRUE, values=mzmmCov[2], lbound=lbVa, newlabels=cMZmm[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZff, free=TRUE, values=dzffCov[2], lbound=lbVa, newlabels=cDZff[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZmm, free=TRUE, values=dzmmCov[2], lbound=lbVa, newlabels=cDZmm[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZfm, free=TRUE, values=dzfmCov[2], lbound=lbVa, newlabels=cDZfm[1:nv] )
#   modelECS <- omxSetParameters( modelECS, label=cDZmf, free=TRUE, values=dzmfCov[2], lbound=lbVa, newlabels=cDZmf[1:nv] )
#   modelECS <- mxAutoStart(modelECS)
#   fitECS <- mxTryHard( modelECS, intervals = F, extraTries = 5)}

# Constrain expected Covariances to be equal across zygosity
# for(i in 1){
#   modelECZ <- mxModel( fitECF, name="ECZ" )
#   modelECZ <- omxSetParameters( modelECZ, label=c(cMZff[1], cDZff[1]), free=TRUE, values=mzffCov[3,2], newlabels=cZf )
#   modelECZ <- omxSetParameters( modelECZ, label=c(cMZmm[1], cDZmm[1]), free=TRUE, values=mzffCov[3,2], newlabels=cZm )
#   modelECZ <- omxSetParameters( modelECZ, label=c(cDZfm[1], cDZmf[1]), free=TRUE, values=mzffCov[3,2], newlabels=cZo )
#   fitECZ <- mxTryHard( modelECZ, intervals=F, extraTries = 5 ) }

# Constrain expected Covariances be equal across SS/OS families
# for(i in 1){
# modelECP <- mxModel( fitECZ, name="ECP" )
# modelECP <- omxSetParameters( modelECP, label=c(cZf, cZm), free=TRUE, values=mzffCov[2], newlabels=cZf )
# modelECP <- omxSetParameters( modelECP, label=c(cZf,cZo), free=TRUE, values=mzffCov[2], newlabels=cZ )
# fitECP <- mxTryHard( modelECP, intervals=F, extraTries = 5 )}

comptables <- mxCompare( fitSAT, subs <- list(fitEMO, fitEMS, fitEMZ, fitEMP,
                                              fitEVO,fitEVS, fitEVZ, fitEVP,
                                              fitECS, fitECF, fitECZ, fitEC0) )

# Print Comparative Fit Statistics
pvalues <- noquote(format(round(mxCompareMatrix(c(fitSAT,
                                                  fitEMO, fitEMS, fitEMZ, fitEMP,
                                                  fitEVO, fitEVS, fitEVZ, fitEVP,
                                                  fitECS, fitECF, fitECZ, fitEC0)),3)))

for(j in 1:12){
comparisons[j, k+1] <- sprintf("%.2f", as.numeric(pvalues[j+1,j+1])-as.numeric(pvalues[j,j]))
comparisons[j, k+2] <- sprintf("%.3f", as.numeric(pvalues[j+1,j]))
}
k = k+2
}

write.csv(comparisons, "comparisons.csv")
