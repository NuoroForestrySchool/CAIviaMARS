# 21 gennaio 2016
# 5 gennaio 2015, a partira da:
# 15 novembre 2015 e 20 dicembre in Polonia

rm(list = ls())
srcf <- "Crea_mdata.R"
ghrepo <- "NuoroForestrySchool/CAIviaMARS/master/"
sf <- ifelse(file.exists(srcf), srcf, 
             paste(sep="","https://raw.githubusercontent.com/",
                   ghrepo,srcf))
source(sf)

library("latticeExtra")

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
# omissis ...
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
agg1 <- data.frame(Age=rowMeans(agg[,7:8]),agg)
agg1$CAIp <- as.numeric(predict(earth.m, agg1[,selected.vars]))
agg1<-agg1[order(agg1$SV.c, agg1$mD.c, agg1$mH.c, agg1$Age.c, agg1$Age),]
xyplot(CAIp~Age,agg1,groups=as.factor(paste(SV.c, mD.c, mH.c)), auto.key=list(space="right"),type="o")
xyplot(CAIp~Age|SV.c*mD.c*mH.c, data=agg1, auto.key=list(space="right"),type="o")

with(agg1, plot(agg1[as.numeric(SV.c)==1 & as.numeric(mD.c)==1 & as.numeric(mH.c)==1,c("Age","meanDBH")]))

mdata$ForCat <- as.factor(mdata$ForCat)
set.seed(143); 
earth.im <- earth(y=with(mdata, data.frame(Standing_Volume, meanDBH, Mean_Height)), 
                  x=with(mdata, data.frame(ForCat, Age)),
                  minspan=30, nk=15, degree=3, nfold=10)
summary(earth.im)
xyplot(predict(earth.im)[,1]~Age, groups=ForCat, mdata, auto.key=T)
xyplot(predict(earth.im)[,2]~Age, groups=ForCat, mdata, auto.key=T)
xyplot(predict(earth.im)[,3]~Age, groups=ForCat, mdata, auto.key=T)

aggFC <- ddply(mdata, .(ForCat), summarise, 
             N=length(Age), 
             mAge=median(Age), minAge=min(Age), maxAge=max(Age)
)
p.grid<-data.frame()
for(i in 1:nrow(aggFC)) p.grid<-rbind(p.grid, with(aggFC, 
                             data.frame(ForCat=levels(ForCat)[i],
                                        Age=seq(round(minAge[i]/10)*10,maxAge[i], 10))))
xyplot(predict(earth.im,p.grid)[,1]~p.grid$Age, groups=p.grid$ForCat, auto.key=T)
xyplot(predict(earth.im,p.grid)[,2]~p.grid$Age, groups=p.grid$ForCat, auto.key=T)
xyplot(predict(earth.im,p.grid)[,3]~p.grid$Age, groups=p.grid$ForCat, auto.key=T)

xyplot(predict(earth.m,data.frame(p.grid,predict(earth.im,p.grid)))~p.grid$Age, groups=p.grid$ForCat, auto.key=T,type="o")
xyplot(predict(earth.m,data.frame(p.grid,predict(earth.im,p.grid)))~p.grid$Age|p.grid$ForCat, 
       auto.key=list(points=F,lines=T,corner=c(.9,.9)),type="smooth",
       xlim=c(0,250),ylab="CAIp",xlab="Age", ylim=c(0,10))
xyplot(CAI~Age|ForCat, data=mdata, auto.key=list(points=F,lines=T,corner=c(.9,.9)),type="p",
       xlim=c(0,250),ylab="CAIp",xlab="Age", ylim=c(0,30))
library(latticeExtra)
# Estimated CAI vs Age distribution, by ForCat
# - general average, estimated using general average distributions of the other predictors vs Age, by ForCat
xyplot(CAI~Age|ForCat, data=mdata, auto.key=list(points=F,lines=T,corner=c(.9,.9)),type="p",
       xlim=c(0,250),ylab="CAIp",xlab="Age", ylim=c(0,30)) +
  as.layer(xyplot(predict(earth.m,data.frame(p.grid,predict(earth.im,p.grid)))~p.grid$Age|p.grid$ForCat, 
                  auto.key=list(points=F,lines=T,corner=c(.9,.9)),type="smooth", col="grey",lwd=3))
#########
# - 1&3 quartile averages, estimated by a chain of models: 
#     CAIp = f(Age, f_DBH(Age, ForCat), f_Height(f_DBH, ForCat), f_SV(Age, f_DBH, f_Height, ForCat))
# ** level 1 - only DBH estimate is split on the two quartile levels, subsequent are central fits

# BUILDING MODELS
# examining Standing_Volume distributions => f_SV
set.seed(143); 
earth.imv.sv <- earth(Standing_Volume~., 
                   data=with(mdata, 
                             data.frame(ForCat, Age, meanDBH, Mean_Height, Standing_Volume)), 
                  minspan=30, nk=15, degree=3, nfold=3, ncross=30, varmod.method="power")
summary(earth.imv.sv)
evimp(earth.imv.sv)
##             nsubsets   gcv    rss
## Mean_Height        9 100.0  100.0
## ForCatCA           7  52.3   52.9
## Age                7  52.3   52.9
## ForCatLC           5  31.7   32.7
## meanDBH            4  25.3   26.4
## ForCatQU           3  19.6   20.7

# examining Mean_Height distributions => f_Height
set.seed(143); 
earth.imv.mh <- earth(Mean_Height~., 
                      data=with(mdata, 
                                data.frame(ForCat, Age, meanDBH, Mean_Height)), 
                      minspan=30, nk=15, degree=3, nfold=3, ncross=30, varmod.method="power")
summary(earth.imv.mh)
evimp(earth.imv.mh)
##           nsubsets   gcv    rss
##  meanDBH         9 100.0  100.0
##  ForCatCA        7  35.3   35.9
##  ForCatLC        6  30.4   30.9
##  ForCatPS        5  24.9   25.5
##  ForCatQU        4  16.9   17.7
##  ForCatAF        3  12.4   13.1

# examining meanDBH distributions => f_DBH
set.seed(143); 
earth.imv.md <- earth(meanDBH~., 
                      data=with(mdata, 
                                data.frame(ForCat, Age, meanDBH)), 
                      minspan=30, nk=15, degree=3, nfold=3, ncross=30, varmod.method="power")
summary(earth.imv.md)
evimp(earth.imv.md)
##          nsubsets   gcv    rss
## Age             8 100.0  100.0
## ForCatCA        7  25.8   26.5
## ForCatFG        4  15.5   16.2
## ForCatQU        1   5.0    5.5

#verify f_DBH
xyplot(predict(earth.imv.md)~mdata$Age, groups=mdata$ForCat, ylab=paste(dimnames(predict(earth.imv.md))[[2]],"- center"), xlab="Age", xlim=c(0,250))
plotmo(earth.imv.md, degree1=2, degree2=F, pt.col=1, level=.5, xlim=c(0,250))

#start building prediction grid (estimating coordinates of data points for 'quartile' CAI predictions)
# add upr and lwr .5 DBH prediction limits
p.grid1<-data.frame(p.grid,predict(earth.imv.md,p.grid, interval="pint", level=.5))
i <- which(names(p.grid1)%in%c("fit","lwr","upr"))
yvn <- dimnames(predict(earth.imv.md))[[2]]
names(p.grid1)[i] <- paste(yvn,names(p.grid1)[i],sep="_")
xyplot(meanDBH_upr~Age|ForCat, data=p.grid1, ylab=yvn, xlim=c(0,250))+
  as.layer(xyplot(meanDBH_lwr~Age|ForCat, data=p.grid1 ))

# for each DBH series, predict Heigth - (as distinct columns)
yvn <- dimnames(predict(earth.imv.mh))[[2]]
p.grid2 <- data.frame(p.grid1,predict(earth.imv.mh,rename(p.grid1, replace=c("meanDBH_lwr"="meanDBH"))))
p.grid2 <- rename(p.grid2, replace=c("Mean_Height" = "Mean_Height.lwrDBH"))
p.grid2 <- data.frame(p.grid2,predict(earth.imv.mh,rename(p.grid1, replace=c("meanDBH_upr"="meanDBH"))))
p.grid2 <- rename(p.grid2, replace=c("Mean_Height" = "Mean_Height.uprDBH"))
xyplot(Mean_Height.uprDBH~Age|ForCat, data=p.grid2, ylab=yvn, xlim=c(0,250))+
  as.layer(xyplot(Mean_Height.lwrDBH~Age|ForCat, data=p.grid2 ))

# for upr and lwr, predict Standing Volume
yvn <- dimnames(predict(earth.imv.sv))[[2]]
p.grid3 <- data.frame(p.grid2,predict(earth.imv.sv,
                                      rename(p.grid2, replace=c("meanDBH_lwr"="meanDBH", "Mean_Height.lwrDBH"="Mean_Height"))))
p.grid3 <- rename(p.grid3, replace=c("Standing_Volume" = "Standing_Volume.lwrDBH"))
p.grid3 <- data.frame(p.grid3,predict(earth.imv.sv,
                                      rename(p.grid2, replace=c("meanDBH_lwr"="meanDBH", "Mean_Height.uprDBH"="Mean_Height"))))
p.grid3 <- rename(p.grid3, replace=c("Standing_Volume" = "Standing_Volume.uprDBH"))
xyplot(Standing_Volume.uprDBH~Age|ForCat, data=p.grid3, ylab=yvn, xlim=c(0,250))+
  as.layer(xyplot(Standing_Volume.lwrDBH~Age|ForCat, data=p.grid3 ))

