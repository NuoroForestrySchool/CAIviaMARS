# 21 gennaio 2016
# 5 gennaio 2015, a partira da:
# 15 novembre 2015 e 20 dicembre in Polonia

# Opzione utilizzabile se non è necessario criptare il CSV. Non è necessario creare file!
# require(RCurl)
# x <- getURL("https://raw.githubusercontent.com/NuoroForestrySchool/CAIviaMARS/master/IFR_Piemonte_2.csv")
# indata <- read.csv(text = x)[,-c(5,15:16,24:26)]#tolte info accessorie

# Se invece deve essere criptato
tf <- tempfile()
td <- dirname(tf)
# se il CSV non è nella WorkingDir
csvzipfile <- "inventario.csv.zip"
if(!file.exists(csvzipfile)) {
  # recuperalo da GitHub
  d.url <- "https://raw.githubusercontent.com/NuoroForestrySchool/CAIviaMARS/master/inventario.csv.zip"
  system2("wget", args=c(paste("--output-document",tf), d.url))
  csvzipfile <- tf
}
psw <- readline("Password: ")
system2("unzip", args=c(paste("-o -d",td), paste("-P",psw), csvzipfile, "inventario.csv"))
rm(psw)
if (file.exists(tf)) file.remove(tf)
indata.file <- paste(td,"inventario.csv",sep="/")
indata <- read.csv(indata.file)[,-c(5,15:16,24:26)]#tolte info accessorie
file.remove(indata.file)
head(indata)

# Calcolo CAI = StandingVolume * pV
#          pV = {[vol(DBH2, (H+.2)) / vol(DBH, H)] - 1  )
indata$vol_i_1a <- pi*indata$sH*.5*(indata$sDBH/200)^2
indata$vol_i_2a <- pi*(indata$sH+.2)*.5*(indata$sDBH2/200)^2
indata$pV_h <- (indata$vol_i_2a/indata$vol_i_1a)-1
indata$CAIv <- indata$GS_ha * indata$pV_h
summary(indata)

#idata <- indata[,-c(12, 13:15,21:23)]
idata <- indata[,-c(13:15,21:23)] # keep "incr_cm_y"

# Selezione ed aggregazione delle ForCat
idata$ForCat <- as.character(idata$ForCat)
table(idata$ForCat)
idata2 <- idata[idata$ForCat %in% c('AB','AF','CA','FG','LC','PN','PM','PS','QV','QR','QC'),]
table(idata2$ForCat)
idata2$ForCat[idata2$ForCat %in% c('QC','QR','QV')]<-'QU'
idata2$ForCat[idata2$ForCat %in% c('PN','PM','PS')]<-'PS'  # Diventa tutto 'Pino silvestre'?

idata2$FTita <- NULL
idata2$FTita[idata2$ForCat=='AB'] <- 'Abetine'
idata2$FTita[idata2$ForCat=='AF'] <- 'Acero_tiglio_frassineti'
idata2$FTita[idata2$ForCat=='AN'] <- 'Alneti_planiziali_montani'
idata2$FTita[idata2$ForCat=='AS'] <- 'Arbusteti_planiziali_collinari_montani'
idata2$FTita[idata2$ForCat=='BS'] <- 'Boscaglie_pioniere'
idata2$FTita[idata2$ForCat=='CA'] <- 'Castagneti'
idata2$FTita[idata2$ForCat=='CE'] <- 'Cerrete'
idata2$FTita[idata2$ForCat=='FG'] <- 'Faggete'
idata2$FTita[idata2$ForCat=='LC'] <- 'Lariceti_cembreti'
idata2$FTita[idata2$ForCat=='OS'] <- 'Orno_ostrieti'
idata2$FTita[idata2$ForCat=='OV'] <- 'Arbusteti_subalpini'
idata2$FTita[idata2$ForCat=='PE'] <- 'Peccete'
idata2$FTita[idata2$ForCat=='PM'] <- 'Pinete_PMarittimo'
idata2$FTita[idata2$ForCat=='PN'] <- 'Pinete_PMontano'
idata2$FTita[idata2$ForCat=='PS'] <- 'Pinete_PSilvestre'
idata2$FTita[idata2$ForCat=='QC'] <- 'Querceti'
idata2$FTita[idata2$ForCat=='QR'] <- 'Querceti'
idata2$FTita[idata2$ForCat=='QV'] <- 'Querceti'
idata2$FTita[idata2$ForCat=='RB'] <- 'Robinieti'
idata2$FTita[idata2$ForCat=='RI'] <- 'Rimboschimenti'
idata2$FTita[idata2$ForCat=='SP'] <- 'Saliceti_Pioppeti_ripari'

idata2$Management <- NA
idata2$Management[idata2$ASSETTO=='CC'] <- 'Coppice'
idata2$Management[idata2$ASSETTO=='CI'] <- 'Coppice'
idata2$Management[idata2$ASSETTO=='CM'] <- 'Coppice'
idata2$Management[idata2$ASSETTO=='CS'] <- 'Coppice'
idata2$Management[idata2$ASSETTO=='FC'] <- 'High forest'
idata2$Management[idata2$ASSETTO=='FU'] <- 'High forest'
idata2$Management[idata2$ASSETTO=='IN'] <- 'new'
idata2$Management[idata2$ASSETTO=='RI'] <- 'Plantations'
idata2$Management[idata2$ASSETTO=='SG'] <- 'unmanaged'
idata2 <- idata2[idata2$Management %in% c('Coppice', 'High forest', 'unmanaged'),]

vl <-  c("ForCat", "elevation", "aspect", "slope", "CanopyCover",
         "nDeadTrees","DEFOGLIAZIONE","meanDBH","Mean_Height","Age",
         "domDBH", "TH","BasalArea", "Standing_Volume", "CAI", "management")
pos <- match(vl,names(idata2))
r_o <- cbind(names(idata2[1:length(vl)]),pos,vl)
r_o[8,2] <- 10
r_o[9,2] <- 11
r_o[11,2] <- 14
r_o[13,2] <- 16
r_o[14,2] <- 17
r_o[15,2] <- 18
r_o[16,2] <- 20

mdata <- idata2[,as.numeric(r_o[!is.na(r_o[,2]),2])]
names(mdata) <- vl

# parametri per i grafici
op <- par()
#on.exit((par(op)))
par(omd=c(0.1,0.9,0.1,0.9),mar=c(3,3,0,0),mgp=c(1.8,.6,0), las=2)

