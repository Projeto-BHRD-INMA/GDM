if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)

library(readr)
bio <- read_delim("./Data/Biotic/gbif_download.csv",
                   "\t", escape_double = FALSE, trim_ws = TRUE)
bacia <- readOGR("~./Data/Abiotic/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))

wc <-stack("./Data/Abiotic/wc_vif.grd")
plot(wc[[2]])


#selecionando so colunas long, lat e species
head(bio)
bio<-bio[,c("decimalLongitude","decimalLatitude", "species")]
colnames(bio)[1:2] <- c("lon", "lat")

#para tirar duplicadas de cada pixel/cell
data_tudo<-extract(wc, bio[,1:2], cellnumbers=T)
data_tudo <- cbind(bio, data_tudo)
dups2 <- duplicated(data_tudo[, c("cells", "species")])
sum(dups2)
data_tudo <- data_tudo[!dups2, ]
data_tudo<-na.omit(data_tudo)

png(filename="./Results/map.png")
plot(wc[[1]])
points(data_tudo2$lon, data_tudo2$lat, pch = 20, cex=0.5)
plot(bacia, add = T, border = "dark grey")
dev.off()

#pra ver as ocorrencias e as UCs####
uc <- readOGR("./Data/UC_todas/crop_all_bhrd.shp")
uc <- spTransform(uc, CRS("+proj=longlat +datum=WGS84"))

png(filename="./Results/map_2.png")
plot(wc[[1]])
points(data_tudo2$lon, data_tudo2$lat, pch = 20, cex=0.5)
plot(uc, add = T, border = "red")
dev.off()


# write table para a planilha ja croped e sem duplicates
write.table(data_tudo,"./Data/Biotic/species1.csv", sep=";", dec=",",row.names = F)
### ==== para tirar especies da lista que so ocorrem uma vez ===== ####
teste<-data_tudo
lista<-count(teste$species) #qual frequencia de ocorrencia de cada ssp
b<-lista$freq > 1
sp<- lista[!b, ] #lista ssp q ocorrem so uma vez
s <- which(teste$species %in% sp$x)
newdata<-teste[-s,] #cria nova tabela tirando as spp q ocorrem so uma vez
write.table(newdata,"./Data/Biotic/species.csv", sep=";", dec=",",row.names = F)



