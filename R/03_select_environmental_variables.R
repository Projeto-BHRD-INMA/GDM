if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)

tif <- list.files("./Data/Abiotic/" , patt = ".tif")
tif
pres <- grep("2.5m", tif, value = T)
setwd("./Data/Abiotic/")
var_env <-stack(pres)
plot(var_env[[1]])
setwd("~/GDM")

bacia <- readOGR("./Data/Abiotic/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))
env_crop <- crop(var_env, bacia)
plot(env_crop[[1]])
env<- mask(env_crop, bacia)
plot(env[[1]])

# extract values of cells
env_v <- values(env)
env_v <- na.omit(env_v)



######################correlacao
corr <- cor(env_v)
abs(round(corr, 2)) # funçao abs, transforma em módulo ( tira o negativo)
ifelse(corr >= 0.7, "sim", "nao")
ifelse(corr >= 0.7, 1, 0)
write.table(abs(round(corr, 2)), "./Results/Selected_variables/correlacao/cor_pres.xls", row.names = T, sep = "\t")
write.table(ifelse(corr >= 0.7, "sim", "nao"), "./Results/Selected_variables/correlacao/cor_pres_afirmacao.xls",
            row.names = T, sep = "\t")

##################VIF


source("./R/vif_conc.R")
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
lista <- c(2, 5, 9, 12, 17, 18)
setwd("./Data/Abiotic/")

for(i in lista){
  writeRaster(env[[i]], ifelse(i < 10, paste0("wcbio_0", i, ".tif"),
                               paste0("wcbio_", i, ".tif")),
              format = "GTiff", overwrite=TRUE)}

setwd("~/GDM")

tif <- list.files("./Data/Abiotic/" , patt = ".tif")
wc <- grep("wcbio_", tif, value = T)
setwd("./Data/Abiotic/")
wc <-stack(wc)
plot(wc[[2]])
writeRaster(wc,"wc_vif.grd", format = "raster", overwrite=TRUE)
setwd("../../")

