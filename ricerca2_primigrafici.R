# 21 gennaio 2016
# 5 gennaio 2015, a partira da:
# 15 novembre 2015 e 20 dicembre in Polonia
rm(list = ls())

# setwd('F:/AAA_DATA/PROGETTI/2015/01_MARS/AAA_ELABORAZ_NOV2015/Dati')
# setwd('/Volumes/NO NAME/01_MARS/ANALISI_Novembre2015')
setwd("~/Documenti/03-PUBBetCV/InFieri/Puletti/CAIviaMARS3")

library("latticeExtra")

source("Crea_mdata.R")

bpwm <- function(Measure, Group, ..., ord=meds){
  #produce BoxPlot, con medie "*", ordinate per mediana crescente
  meds <- by(Measure, Group, median)
  levls <- names(meds)[order(-ord)]
  Group <- factor(Group,levls,ordered=T)
  boxplot(Measure~Group, col = "lightgray",pch=20,cex=.5,
          las=1,notch=T,horizontal=T, ...)
  box(lwd=2) 
  # Get the group means
  means <- by(Measure, Group, mean)
  nl <- length(unique(Group))
  points(means, 1:nl, pch = 8, cex = 0.75,
       bg = "red")
  # Now label the means
  lbls<-suppressWarnings(formatC(means,format="f",digits=1))
  #text(means,1:nl,labels=lbls,pos=3,cex=0.9,col="red")
  grid()
}

#PRESENTAZIONE Materiali

#OBIETTIVO - Predictand
bpwm(idata2$incr_cm_y*10,idata2$ForCat, ylab="Forest type", xlab="dbh increment [mm]")
#moderately right skewed distributions, clearly distinct position indexes
bpwm(mdata$CAI,mdata$ForCat,log="x", ylab="Forest type", xlab="log(CAI)")
#severely right skewed distributions, still clearly distinct position indexes, different ordering

#SUPPORTO - Predictors
# 1 - Categorical
sort(table(mdata$ForCat))
{mde <- with(mdata, by(elevation, ForCat, median))
levls <- names(mde)[order(mde)]
fc.e <- factor(mdata$ForCat,levls,ordered=T)
plot(table(fc.e, mdata$aspect),main="",xlab="ForCat",ylab="Aspect",las=1)}
mngmt <- factor(mdata$management, unique(mdata$management)[c(2,1,3)],ordered=T)
plot(table(mngmt, mdata$ForCat),main="",xlab="Management",ylab="ForCat",las=1)
med_sv <- with(mdata, aggregate(Standing_Volume,list(Management=management,ForCat=ForCat), median))
barchart(x ~ as.factor(ForCat), med_sv, groups = Management, type = "a", lex=3,
         xlab="ForCat", ylab="median standing vol [m^3*ha^-1]",
         auto.key = list(corner=c(.95,.9)))
# NON CAPISCO: come è possibile che le mediane per "Coppice" non sono tutte come per QU
#              intorno alla metà di quelle per "High forest"?
#              Particolarmente AB (che ha pochi Coppice?!) ha mediana fuori scala

# 2 - Quantitaive
# 2.1 - Site
bpwm(mdata$elevation,mdata$ForCat, ylab="Forest Category", xlab="elevation [m]", las=2)
bpwm(mdata$slope,mdata$ForCat, ylab="Forest Category", xlab="slope [deg]", las=2)

# 2.2 - Stand
# export next two: 280*220
with(mdata[mdata$Age<250,], bpwm(Age, ForCat, ylab="Forest Category", xlab="age [years]", las=2) )
with(mdata[mdata$BasalArea<100,], bpwm(BasalArea, ForCat, xlab="BasalArea", ylab="Forest Category", las=2))
# sostituito dai successivi bpwm(mdata$Standing_Volume, mdata$ForCat, xlab="standing volume", ylab="Forest Category", las=2)
# export next two: 280*400
xyplot(Standing_Volume~BasalArea | ForCat, data=mdata, xlim=c(0,100),
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       }
)
xyplot(Standing_Volume~Age | ForCat, data=mdata, xlim=c(0,250),
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       }
)
stripplot(ForCat~jitter(CanopyCover,1,.5), data=mdata, jitter.data=T, xlab="Canopy cover %", ylab="Forest Category", las=2)
stripplot(ForCat~jitter(DEFOGLIAZIONE,1,.5), jitter.data=T, data=mdata, xlab="Defogliazione %", ylab="Forest Category", las=2)

# 2.3 - Tree
bpwm(mdata$meanDBH,mdata$ForCat, ylab="Forest Category", xlab="mean dbh [cm]", las=2)
bpwm(mdata$Mean_Height,mdata$ForCat, ylab="Forest Category", xlab="mean height [m]", las=2)
xyplot(Mean_Height~meanDBH | ForCat, data=mdata, xlim=c(5,75),
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       }
)
bpwm(mdata$domDBH,mdata$ForCat, ylab="Forest Category", xlab="dominant dbh [cm]", las=2)
xyplot(domDBH~meanDBH | ForCat, data=mdata, xlim=c(5,75),
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
         panel.abline(a=0, b=1, col="grey")
       }
)
#ATTENZIONE: per alcune UdC dDOM è inferiore dMED!!!
xyplot(domDBH~meanDBH | ForCat, data=mdata[mdata$domDBH<mdata$meanDBH,],
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.abline(a=0, b=1, col="grey")
       }
)
bpwm(mdata$TH, mdata$ForCat, xlab="TH", ylab="Forest Category", las=2)
xyplot(TH~Mean_Height | ForCat, data=mdata,
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
         panel.abline(a=0, b=1, col="grey")
       }
)
# ATTENZIONE: per molte UdC TH è inferiore a Mean_Height
xyplot(TH~Mean_Height | ForCat, data=mdata[mdata$TH<mdata$Mean_Height,],
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
         panel.abline(a=0, b=1, col="grey")
       }
)
# Grafici di studio
mdata$n_fusti <- with(mdata, BasalArea*400/(pi*meanDBH^2))
bpwm(mdata$n_fusti, mdata$ForCat, xlab="Tn_fusti", ylab="Forest Category", las=2)
xyplot(n_fusti~Age| management*ForCat, data=mdata, xlim=c(0, 250),
grid=T)
# la distribuzione è ragionevole: verificare che BA è in [m^2*ha*-1] e DBH in [cm]

mdata$meanF <- with(mdata, Standing_Volume/(BasalArea*Mean_Height))
bpwm(mdata$meanF, mdata$ForCat, xlab="meanF", ylab="Forest Category", las=2)
xyplot(meanF~Age| ForCat, data=mdata, xlim=c(0, 250), ylim=c(0,1), grid=T)
# l'ordine di grandezza c'è ma, a parte CA, comunque è parecchio elevato!!

Ages <- equal.count(mdata$Age, number=3, overlap=.1)
xyplot(Mean_Height~meanDBH | Ages*ForCat, data=mdata, xlim=c(0,80))
xyplot(Mean_Height~meanDBH | Ages*ForCat, data=mdata, xlim=c(0,80),
       strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Mean DBH", ylab = "Mean height",
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       },
)

xyplot(Standing_Volume~meanDBH | Ages*ForCat, data=mdata, xlim=c(0,80),
       strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Mean DBH", ylab = "Mean height",
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       },
)
# fine grafici di studio


# MODEL
library(earth)
set.seed(143); earth.m0 <- earth(CAI~., data=mdata, minspan=30, nk=15, degree=3,
                                nfold=10); earth.m0
summary(earth.m0)
# write.csv(as.data.frame(coefficients(earth.m)), 'mars_coeff.csv')
evimp(earth.m0)

selected.vars <- unlist(dimnames(evimp(earth.m0))[1])

set.seed(143); earth.m <- earth(CAI~., data=mdata[,c("CAI", selected.vars)], minspan=30, nk=15, degree=3,
                                 nfold=10); earth.m

CAIp <- predict(earth.m)
obs_meds <- by(mdata$CAI, mdata$ForCat, median)
# log(CAI predicted)
bpwm(CAIp[CAIp>0],idata2$ForCat[CAIp>0],log="x", ylab="Forest type", xlab="log(CAI predicted)", ord=obs_meds)

plotres(earth.m) # not used
fc<-as.factor(mdata$ForCat)
fcn<-as.numeric(fc)
plotres(earth.m, which=3, pch=fcn, col=fcn) # not used

res<-mdata$CAI-CAIp
bwplot(fc ~ res, horizontal=T, notch=T, pch="|", grid=F) # not used
# residual
bpwm(as.numeric(res), fc, ylab="ForCat", xlab="residual")
# residual 2, approfondimento
xyplot(res~CAIp | fc,
       layout=c(1,NA), strip=F, strip.left=T,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "fitted", ylab = "residual",
       panel = function(x, y) {
         panel.xyplot(x, y, grid=T)
         panel.loess(x, y, col="black")
       },
)

# relazioni di base
plotmo(earth.m,degree2=F,all1=T)

earth.shingles <- function(df, varn, model){
  cuts <- unique(model$cuts[model$dirs[,varn]!=0,varn])
  int0 <- c(min(df[,varn]),cuts,max(df[,varn]))
  n <- length(int0)
  interv <- matrix(int0[c(1,2:(n-1),2:n)],n-1,2)
  shingle(df[,varn], interv)
}
earth.cuts <- function(df, varn, model){
  cuts <- unique(model$cuts[model$dirs[,varn]!=0,varn])
  int0 <- c(min(df[,varn]), cuts,max(df[,varn]))
  cut(df[,varn], int0, include.lowest=T, ordered_result=T)
}

SV.s <- earth.shingles(mdata, "Standing_Volume", earth.m)
mD.s <- earth.shingles(mdata, "meanDBH", earth.m)
mH.s <- earth.cuts(mdata, "Mean_Height", earth.m)
pf <- function(x,y, ...){
  panel.xyplot(x, y, col=c("grey", "blue"), ...)
  panel.xyplot(x, y, type="smooth", lwd=2, ...)  }
xyplot(CAI+CAIp~Age|meanDBHs*StandingVolumes, mdata,
       par.settings=list(superpose.symbol=list(pch=c(1,4)),
                         superpose.line=list(col=c(NA, "black"))),
       panel=pf, xlim=c(0,250), ylim=c(0,50), 
       auto.key=list(corner=c(.96,.92), lines=T, border=T))
pf <- function(x,y, ...){
  panel.xyplot(x, y, col=c("grey", "blue"), ...)
  panel.xyplot(x, y, type="smooth", lwd=2, ...)  }
xyplot(CAI+CAIp~Age, data=mdata, groups=meanHs,
       par.settings=list(superpose.symbol=list(pch=c(1,4)),
                         superpose.line=list(col=c(NA, "black"))),
       panel=panel.superpose, panel.groups=pf, xlim=c(0,250), ylim=c(0,50), 
       auto.key=list(corner=c(.96,.92), lines=T, border=T))

Age.c <- cut(mdata$Age, c(0,seq(5,205,10),max(mdata$Age)), 
             include.lowest=T, ordered_result=T)
SV.c <- earth.cuts(mdata, "Standing_Volume", earth.m)
mD.c <- earth.cuts(mdata, "meanDBH", earth.m)
mH.c <- earth.cuts(mdata, "Mean_Height", earth.m)
library(plyr)
agg <- ddply(cbind(mdata,CAIp=as.numeric(CAIp)),c("Age.c","SV.c", "mD.c", "mH.c"), summarise, 
             N=length(Age), 
             mAge=median(Age), minAge=min(Age), maxAge=max(Age), 
             Standing_Volume=median(Standing_Volume),
             meanDBH=median(meanDBH),
             Mean_Height=median(Mean_Height),
             mCAI=mean(CAI),
             mCAIp=mean(CAIp)
             )
xyplot(mCAIp~mAge,agg,groups=as.factor(paste(SV.c, mD.c, mH.c)), type="o", auto.key=list(space="right"))

# studio 0
agg1 <- rbind(data.frame(c="min",Age=agg[,7],agg[,-c(6:8)]),
              data.frame(c="max",Age=agg[,8],agg[,-c(6:8)]),
              data.frame(c="mid",Age=rowMeans(agg[,7:8]),agg[,-c(6:8)]))
agg1$CAIp <- as.numeric(predict(earth.m, agg1[,selected.vars]))
agg1<-agg1[order(agg1$SV.c, agg1$mD.c, agg1$mH.c, agg1$Age.c, agg1$Age),]
xyplot(CAIp~Age,agg1,groups=as.factor(paste(SV.c, mD.c, mH.c)), auto.key=list(space="right"),type="smooth")
xyplot(CAIp~Age|SV.c*mD.c*mH.c, data=agg1, auto.key=list(space="right"),type="o")
# studio 0 fine
# studio 1
agg1 <- data.frame(Age=rowMeans(agg[,7:8]),agg)
agg1$CAIp <- as.numeric(predict(earth.m, agg1[,selected.vars]))
agg1<-agg1[order(agg1$SV.c, agg1$mD.c, agg1$mH.c, agg1$Age.c, agg1$Age),]
xyplot(CAIp~Age,agg1,groups=as.factor(paste(SV.c, mD.c, mH.c)), auto.key=list(space="right"),type="o")
xyplot(CAIp~Age|SV.c*mD.c*mH.c, data=agg1, auto.key=list(space="right"),type="o")


with(agg1, plot(agg1[as.numeric(SV.c)==1 & as.numeric(mD.c)==1 & as.numeric(mH.c)==1,c("Age","meanDBH")]))



