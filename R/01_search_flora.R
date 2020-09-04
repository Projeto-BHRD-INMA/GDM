#install.packages( "devtools")
#devtools::install_github("saramortara/rocc")
library (rocc)

mg<-search_flora(domain = NULL,
             state = "MG",
             lifeform = "Árvore")

est<-rep("MG",nrow(mg))
sp_mg<-cbind(est, mg)

es<-search_flora(domain = NULL,
                     state = "ES",
                     lifeform = "Árvore")

est<-rep("es",nrow(es))
sp_es<-cbind(est, es)

lista<- rbind(sp_mg, sp_es)

dups2 <- duplicated(lista[, "id"])
sum(dups2)
lista2 <- lista[!dups2, ]

write.table(lista2, "GDM/Data/Biotic/lista_BHRD.csv", row.names=F, sep=";", dec=",")

