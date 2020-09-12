if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)

library(readr)
bio <- read_delim("./Data/Biotic/gbif_download.csv",
                   "\t", escape_double = FALSE, trim_ws = TRUE)
bacia <- readOGR("~./Data/Abiotic/munic_BHRD.shp")
bacia <- spTransform(bacia, CRS("+proj=longlat +datum=WGS84"))

wc <-stack("./Data/Abiotic/wc_vif.grd")
plot(wc[[2]])

bio<-bio[,c("decimalLongitude","decimalLatitude", "species")]
colnames(bio)[1:2] <- c("lon", "lat")


data_tudo<-extract(wc, bio[,1:2], cellnumbers=T)
data_tudo <- cbind(bio, data_tudo)
dups2 <- duplicated(data_tudo[, c("cells", "species")])
sum(dups2)
data_tudo <- data_tudo[!dups2, ]
data_tudo<-na.omit(data_tudo)

png(filename="./Results/map.png")
plot(wc[[1]])
points(data_tudo$lon, data_tudo$lat, pch = 20, cex=0.5)
plot(bacia, add = T, border = "dark grey")
dev.off()

teste<-data_tudo
lista<-count(teste$species)
b<-lista$freq > 1
sp<- lista[!b, ]
s <- which(teste$species %in% sp$x)
newdata<-teste[-s,]
write.table(newdata,"./Data/Biotic/species.csv", sep=";", dec=",",row.names = F)



