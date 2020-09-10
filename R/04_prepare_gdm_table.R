if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)


bio <- read.delim("./Data/Biotic/gbif_download.csv")
bacia <- readOGR("./Data/Abiotic/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))

wc <-stack("./Data/Abiotic/wc_vif.grd")
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

#mapa das ocorrencias com uma das camadas das variaveis climpaticas, a camada [1] que é a bio_2.
png(filename="GDM/Results/map1.png")
plot(wc[[1]])
points(data_tudo$lon, data_tudo$lat, pch = 20, cex=0.5)
plot(bacia, add = T, border = "dark grey")
dev.off()

#pra ver as ocorrencias e as UCs####
uc <- readOGR("./Data/UC_todas/crop_all_bhrd.shp")
uc <- spTransform(uc, CRS("+proj=longlat +datum=WGS84"))

png(filename="./Results/map_2.png")
plot(wc[[1]])
points(data_tudo$lon, data_tudo$lat, pch = 20, cex=0.5)
plot(uc, add = T, border = "red")
dev.off()

### ==== para tirar especies da lista que so ocorrem uma vez ===== ####
teste<-data_tudo
lista<-count(teste$species) #qual frequencia de ocorrencia de cada ssp
b<-lista$freq > 1
sp<- lista[!b, ] #lista ssp q ocorrem so uma vez
s <- which(teste$species %in% sp$x)
newdata<-teste[-s,] #cria nova tabela tirando as spp q ocorrem so uma vez
write.table(newdata,"./Data/Biotic/species.csv", sep=";", dec=",",row.names = F)



