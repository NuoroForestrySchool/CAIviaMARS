# 21 gennaio 2016
# 5 gennaio 2015, a partira da:
# 15 novembre 2015 e 20 dicembre in Polonia

# - 1&3 quartile averages, estimated by a chain of models: 
#     CAIp = f(Age, f_DBH(Age, ForCat), f_Height(f_DBH, ForCat), f_SV(Age, f_DBH, f_Height, ForCat))
# ** level 2 - only SV estimate is split on the two quartile levels, others are central fits


rm(list = ls())

# setwd('F:/AAA_DATA/PROGETTI/2015/01_MARS/AAA_ELABORAZ_NOV2015/Dati')
# setwd('/Volumes/NO NAME/01_MARS/ANALISI_Novembre2015')
setwd("~/Documenti/03-PUBBetCV/InFieri/Puletti/CAIviaMARS3")

library("latticeExtra")
library(earth)
library(dplyr)
library(latticeExtra)
source("Crea_mdata.R")

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

# THE MODEL
## >selected.vars <- unlist(dimnames(evimp(earth.m0))[1])
## >selected.vars
##[1] "Standing_Volume"
##[2] "meanDBH"        
##[3] "Age"            
##[4] "Mean_Height"  
selected.vars <- c("Standing_Volume", "meanDBH", "Age", "Mean_Height")
set.seed(143); earth.m <- earth(CAI~., data=mdata[,c("CAI", selected.vars)], minspan=30, nk=15, degree=3,
                                nfold=10); earth.m

#start building prediction grid (estimating coordinates of data points for 'quartile' CAI predictions)
aggFC <- ddply(mdata, .(ForCat), summarise, 
               N=length(Age), 
               mAge=median(Age), minAge=min(Age), maxAge=max(Age)
)
aggFC$ForCat <- factor(aggFC$ForCat)
p.grid<-data.frame()
for(i in 1:nrow(aggFC)) p.grid<-rbind(p.grid, with(aggFC, 
                                                   data.frame(ForCat=levels(ForCat)[i],
                                                              Age=seq(round(minAge[i]/10)*10,maxAge[i], 10))))

# add DBH central predictions
yvn <- dimnames(predict(earth.imv.md))[[2]]
p.grid1<-data.frame(p.grid,predict(earth.imv.md,p.grid))
xyplot(meanDBH~Age|ForCat, data=p.grid1, ylab=yvn, xlim=c(0,250))

# predict Heigth
yvn <- dimnames(predict(earth.imv.mh))[[2]]
p.grid2<-data.frame(p.grid1, predict(earth.imv.mh, p.grid1))
xyplot(Mean_Height~Age|ForCat, data=p.grid2, ylab=yvn, xlim=c(0,250))

# predict upr and lwr Standing Volume 
yvn <- dimnames(predict(earth.imv.sv))[[2]]
p.grid3<-data.frame(p.grid2,predict(earth.imv.sv,p.grid2, interval="pint", level=1/3))
i <- which(names(p.grid3)%in%c("fit","lwr","upr"))
names(p.grid3)[i] <- paste(yvn,names(p.grid3)[i],sep="_")
xyplot(Standing_Volume_upr~Age|ForCat, data=p.grid3, ylab=yvn, xlim=c(0,250))+
  as.layer(xyplot(Standing_Volume_lwr~Age|ForCat, data=p.grid3 ))

# add CAI predictions
yvn <- dimnames(predict(earth.imv.sv))[[2]]

####
p.grid4 <- data.frame(p.grid3,predict(earth.m, 
                                      plyr::rename(p.grid3, 
                                                   replace=c("Standing_Volume_lwr"="Standing_Volume"))))

p.grid5 <- data.frame(p.grid4,predict(earth.m, 
                                      plyr::rename(p.grid4, 
                                                   replace=c("Standing_Volume_upr"="Standing_Volume"))))
#%%%%%
p.grid6 <- data.frame(p.grid5,predict(earth.m, 
                                      plyr::rename(p.grid5, 
                                                   replace=c("Standing_Volume_fit"="Standing_Volume"))))
colnames(p.grid6)[8:10] <- c('CAI.lwr','CAI.upr', 'CAI.fit')
head(p.grid6)

xyplot(CAI~Age|ForCat,data=mdata,pch=20, cex=.5, col='grey90', ylab='CAI', xlim=c(-10,150), ylim=c(0,20))+
  as.layer(xyplot(CAI.upr~Age|ForCat, data=p.grid6, type='l'))+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid6, type='l',col='red'))+
  as.layer(xyplot(CAI.fit~Age|ForCat, data=p.grid6, type='l',col='black', lty=2))
#%%%%%
colnames(p.grid5)[8:9] <- c('CAI.lwr','CAI.upr')
head(p.grid5)

xyplot(CAI~Age|ForCat, data=mdata, ylab=yvn, xlim=c(-10,250), cex=.5, 
       layout=c(1,NA), strip=F, strip.left=T)+
  as.layer(xyplot(CAI.upr~Age|ForCat, data=p.grid5, type='o'))+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid5, type='o'))+
##########
xyplot(CAI.upr~Age|ForCat, data=p.grid5, ylab=yvn, xlim=c(10,150), type='o')+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid5, type='o'))

xyplot(CAI.upr~Age|ForCat, data=p.grid5, ylab='CAI', xlim=c(10,150), type='o')+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid5, type='o'))
xyplot(CAI~Age|ForCat,data=mdata,pch=20, cex=.5, col='grey', ylab='CAI', xlim=c(-10,150))+
  as.layer(xyplot(CAI.upr~Age|ForCat, data=p.grid5, type='l'))+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid5, type='l',col='red'))

xyplot(CAI~Age|ForCat,data=mdata,pch=20, cex=.5, col='grey90', ylab='CAI', xlim=c(-10,150), ylim=c(0,40))+
  as.layer(xyplot(CAI.upr~Age|ForCat, data=p.grid5, type='l'))+
  as.layer(xyplot(CAI.lwr~Age|ForCat, data=p.grid5, type='l',col='red'))


           