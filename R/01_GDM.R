if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)

tif <- list.files("~/WorldClim/wc2/" , patt = ".tif")
tif
pres <- grep("30s", tif, value = T)
setwd("~/WorldClim/wc2/")
var_env <-stack(pres)
plot(var_env[[1]])

bacia <- readOGR("~/Occurrence-data/data/shapefile/Bacia/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))
env_crop <- crop(var_env, bacia)
plot(env_crop[[1]])
env<- mask(env_crop, bacia)
plot(env[[1]])

# extract values of cells
env_v <- values(env)
env_v <- na.omit(env_v)


dir.create("analise_selecao_variaveis")
setwd("analise_selecao_variaveis")
dir.create("correlacao")
dir.create("../vif")
######################correlacao
setwd("correlacao")
corr <- cor(env_v)
abs(round(corr, 2)) # funçao abs, transforma em módulo ( tira o negativo)
ifelse(corr >= 0.7, "sim", "nao")
ifelse(corr >= 0.7, 1, 0)
write.table(abs(round(corr, 2)), "cor_pres.xls", row.names = T, sep = "\t")
write.table(ifelse(corr >= 0.7, "sim", "nao"), "cor_pres_afirmacao.xls",
            row.names = T, sep = "\t")

##################VIF

setwd("~/Dropbox/scripts")
source("vif_conc.R")
setwd("~/analise_selecao_variaveis/vif")
vif<-vif_func(env_v[,-c(1:2)], thresh=10, trace=T)
vif
# significado das bios
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio

vif
lista <- c(2, 3, 5, 14, 18)
setwd("~/WorldClim/wc2/")

for(i in lista){
  writeRaster(env[[i]], ifelse(i < 10, paste0("wc_bio0", i, " .tif"),
                               paste0("wc_bio", i, " .tif")), format = "GTiff")}


#########################################

bio <- read.delim("~/download_occ/0054453-200613084148143.csv")
bacia <- readOGR("~/Occurrence-data/data/shapefile/Bacia/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))

tif <- list.files("~/WorldClim/wc2/" , patt = ".tif")
wc <- grep("wc_bio", tif, value = T)
setwd("~/WorldClim/wc2/")
wc <-stack(wc)
plot(wc[[2]])

bio<-bio[,c("decimalLongitude","decimalLatitude", "species")]
colnames(bio)[1:2] <- c("lon", "lat")
bio$lon<-as.numeric(bio$lon)
bio$lat<-as.numeric(bio$lat)

data_tudo<-extract(wc, bio[,1:2], cellnumbers=T)
data_tudo <- cbind(bio, data_tudo)
dups2 <- duplicated(data_tudo[, c("cells", "species")])
sum(dups2)
data_tudo <- data_tudo[!dups2, ]
data_tudo<-na.omit(data_tudo)

setwd("~/GDM/")
write.table(data_tudo,"ocor.csv", sep=";", dec=",",row.names = F)
plot(wc$wc_bio02_)
points(data_tudo$lon, data_tudo$lat, pch = 20, cex=0.5)
plot(bacia, add = T, border = "dark grey")

teste<-data_tudo
lista<-count(teste$species)
b<-lista$freq > 1
sp<- lista[!b, ]
s <- which(teste$species %in% sp$x)
newdata<-teste[-s,]
write.table(newdata,"newdata.csv", sep=";", dec=",",row.names = F)
newdata<-read.csv2("newdata.csv", h=T, sep= ";", dec=",")

newdata-> gdmExpData
envTab <- gdmExpData[, c(5:ncol(gdmExpData))]
sppTab <- gdmExpData[, c("species", "cells", "lon", "lat")]
gdmTab.rast <- formatsitepair(sppTab, bioFormat=2, XColumn="lon",                                         YColumn="lat", sppColumn="species",
                              siteColumn="cells", predData=wc,
                              sppFilter=3)
gdmTab.rast<-na.omit(gdmTab.rast)
gdm.1 <- gdm(gdmTab.rast, geo=T)


## get idea of number of panels
length(gdm.1$predictors)
quartz(title="gdm")
plot(gdm.1, plot.layout=c(3,3))
dev.off()

summary(gdm.1)

# porcentagem de explicação do GDM. Figure X. Fitted functions of observed turnover in composition of species for a Generalized Dissimilarity Model using 4 environmental variables.
#The maximum height reached by each function provides an indication of the total amount of compositional turnover associated with that variable, holding all other variables constant.
#The slope of each function provides an indication of the rate of compositional turnover and how this rate varies along the gradient.


quartz(title="gdm")
plotUncertainty(gdmTab.rast,  sampleSites=0.7, bsIters=5,
                geo=T, spline=NULL,
                knots=NULL, splineCol="blue", errCol="grey80",
                plot.linewidth=2.0, plot.layout=c(3,2))
dev.off()

##Figure X: Predicted spatial variation in species composition.
#Grid cells mapped in similar colors are predicted to have similar species composition, while cells mapped in very different colors are predicted to be highly dissimilar in composition.
# Colors represent gradients in species composition derived from transformed environmental predictors.
# Locations with similar colors are expected to contain similar communities.


envRast<-wc
rastTrans <- gdm.transform(gdm.1, envRast)
plot(rastTrans)

rastDat <- na.omit(getValues(rastTrans))
#rastDat <- sampleRandom(rastTrans, 50000) # can use if rasters are large
pcaSamp <- prcomp(rastDat)
# note the use of the 'index' argument
pcaRast <- predict(rastTrans, pcaSamp, index=1:3)
# scale rasters
pcaRast[[1]] <- (pcaRast[[1]]-pcaRast[[1]]@data@min) /
  (pcaRast[[1]]@data@max-pcaRast[[1]]@data@min)*255
pcaRast[[2]] <- (pcaRast[[2]]-pcaRast[[2]]@data@min) /
  (pcaRast[[2]]@data@max-pcaRast[[2]]@data@min)*255
pcaRast[[3]] <- (pcaRast[[3]]-pcaRast[[3]]@data@min) /
  (pcaRast[[3]]@data@max-pcaRast[[3]]@data@min)*255

par(mfrow=c(1,1))
plotRGB(pcaRast, r=1, g=2, b=3)
dev.off()

par(mfrow=c(1,1))
plotRGB(pcaRast, r=2, g=3, b=1,stretch='hist', axes=F, main="rgb231-hist", colNA="gray", bgalpha=100)
#axis(1, pos=c(-34))
#box()


###mesmo gráfico com outro padrão de cor
#é tudo o mesmo resultado, só muda aparência (combinação de cores). Escolher qual fica melhor de interpretar


#testando
par(mfrow=c(3,2))
plotRGB(pcaRast, r=2, g=3, b=1,stretch='hist')
plotRGB(pcaRast, r=2, g=1, b=3,stretch='hist')
plotRGB(pcaRast, r=3, g=2, b=1,stretch='hist')
plotRGB(pcaRast, r=3, g=1, b=2,stretch='hist')
plotRGB(pcaRast, r=1, g=2, b=3,stretch='hist')
plotRGB(pcaRast, r=1, g=3, b=2,stretch='hist')

quartz(title="gdm")
plotRGB(pcaRast, r=2, g=3, b=1,stretch='hist', axes=T, main="rgb231-hist", colNA="gray", bgalpha=100)
dev.off()



#teste bem demorado!!!!! plota a importância relativa de cada variável
modTest <- gdm.varImp(gdmTab.rast, geo=T, nPerm=50, fullModelOnly=T)
par(mfrow=c(1,1))
barplot(sort(modTest[[2]][,1], decreasing=T))



