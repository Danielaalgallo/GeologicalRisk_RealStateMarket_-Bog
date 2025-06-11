###Necesito trasponer la matriz de forma que cada codigo de manzana salga una vez pero 
###muestre el diferente valor de referencia

library(tidyverse)
library(dplyr)
library(plyr)

setwd("~/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")

db <- read.csv("Valor_Ref_AIMM_Estrato_Barrio_CT1.csv", header = TRUE, sep= ";", dec =",")

### para dejar el aÒo solo
db$ANO1 <- substring(db$ANO, 6, 10)
db$TipAmenaza <- substring(db$TipAmenaza,1, 18)

db1 <- db %>%
  select(MANCODIGO, Area_Manzana, V_REF, ANO1, Tipmenaza,FECHA_ACTO,DESCRIPCIO, TipAmenaza, AMENAZA, A√.O
         , ESTRATO,SCACODIGO, SCANOMBRE, cod_ct, riesgo, a√.o_1, )

###excluir datos del 2020 por el COVID

db1<- db1[db1$ANO1 != "2020 " ,]

db1 <- rename(db1, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))

######Agregar por codigo manzana el crecimiento anual promedio##

db1 <- subset(db1,!is.na(codigo_manzana))

dif_prom <-matrix(ncol=8)
manzanas <- unique(na.omit(db1$codigo_manzana))
colnames(dif_prom) <-  c("codigo_manzana","anos", "num_anos" ,"promanual_2019","promanual_2015",
                         "porc_2020", "porc_2015", "diferencia_neta")

for (l in c(1:length(manzanas))){
  manzana <- db1[db1$codigo_manzana == manzanas[l],]
  manzana <- subset(manzana, !is.na(codigo_manzana))
  manzana <- manzana[order(manzana$ANO1) ,]
  b <- count(manzana$V_REF > 0) 
  for (y in c(1:length(manzana[, 1]))){
    if ((length(manzana[, 1])> 2) && (manzana$V_REF[y] != 0) && (b$freq > 2) ){
        n1 <- y
        n <- length(manzana[, 1])
        anos1 <- paste(manzana$ANO1[n1],"a ",manzana$ANO1[n], sep="")
        num_anos1 <- n-(y-1)
        promanual2019 <- ((manzana$V_REF[n] - manzana$V_REF[n1])/manzana$V_REF[n1])*100
        v_ref_2015 <- manzana[manzana$ANO1 == "2015 ", "V_REF"]
        if (v_ref_2015 == 0){
          promanual2015 <- 0
        }
          else {
            promanual2015 <- ((v_ref_2015 - manzana$V_REF[n1])/manzana$V_REF[n1])*100
            porc2015 <- promanual2015/n-(y-1)
        }
        
        porc2020 <- promanual2020/(n-(y-1))
        
        dif <- manzana$V_REF[n] - manzana$V_REF[n1]
        
        vector = data.frame(codigo_manzana = manzana$codigo_manzana[1],
                            anos = anos1,
                            num_anos = num_anos1,
                            promanual_2019 = promanual2019,
                            promanual_2015 = promanual2015,
                            porc_2020 = porc2020,
                            porc_2015 = porc2015,
                            diferencia_neta = dif)
        
        dif_prom <- rbind(dif_prom,vector)
    }
    break
  }
}
  
  



dif_prom = dif_prom[-1,]

##trasponer la matriz

db2 <- db1 %>%
  pivot_wider(id_cols = c(codigo_manzana, Tipmenaza,FECHA_ACTO, DESCRIPCIO,TipAmenaza,
                          AMENAZA, A√.O, ESTRATO, codigo_barrio, SCANOMBRE, cod_ct, riesgo, a√.o_1), names_from =c(ANO1), values_from = c(V_REF), values_fn = mean)

db3 <- drop_na(db2, codigo_manzana)

db4 <- merge(x = db3, y = dif_prom, by.x= "codigo_manzana", by.y = "codigo_manzana", all =FALSE)

db4$promanual_2019 <- round(db4$promanual_2019,2)
db4$promanual_2015 <- round(db4$promanual_2015,2)
db4$porc_2020 <- round(db4$porc_2020,2)
db4$porc_2015 <- round(db4$porc_2015,2)

col_order <- c("codigo_manzana", "Area_Manzana", "Tipmenaza","FECHA_ACTO", "DESCRIPCIO","TipAmenaza",
               "AMENAZA", "A√.O", "ESTRATO", "codigo_barrio", "SCANOMBRE", "cod_ct", "riesgo", "a√.o_1",
               "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019","2020", "anos", "aumento_prom_anual", "diferencia_neta" )
db5 <- db4[, col_order]
db4 <- as.data.frame(db4)


write.csv(db4, file="manzanasBogotaRA.csv")
