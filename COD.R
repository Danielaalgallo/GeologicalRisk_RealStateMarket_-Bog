#### fijar como carpeta de trabajo la carpeta donde se encuentra la base de datos
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")

### cargar la informaciÛn 
bd <- read.table("Barrios_Alta_Amenaza_ValorRef.txt",sep =";", header =TRUE, dec= ",")

#### hay que hacer algunos controles para saber si la base de datos funciona,
#### estos controles podrÌan ser
##Los barrios que solo intersectan con su extremo exterior shapes de valor de referencia,
### que informaciÛn tienen
### cada barrio incluye informacion para todos los aÒos de todas las manzanas que incluye?
sum(bd$SCACODIGO==008307)
#la respuesta debe ser cero
sum(bd$SCACODIGO==009112)
#la respesta debe ser 116
l <- bd[(bd$SCACODIGO== 009112), ]
unique(l$MANCODIGO) 
#13 manzanas en ese barrio

## LIBRERO PARA TABLAS
library(data.table)
BARRIOS <- setkey(setDT(bd))[,
                                                    list(SCACODIGO, ANO_1), by= list(MANCODIGO, SCANOMBRE, V_REF)]


###librero para varias osas utiles por ejemplo cambair el nombre de las columnas
library(tidyverse)
library(dplyr)
library(plyr)

bd <- rename(bd, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))
    

barrios <- unique(bd$codigo_barrio)
bd <- matrix(ncol=7)
colnames(bd) <- c("barrio","codigo_barrio","ano","promedio_val_m2","maximo_val_m2","minimo_val_m2", "manzanas_totales")
 
for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  anos <- unique(barrio$ANO_1)
  manzanas_tot <- length(unique(barrio$codigo_manzana))
  for (h in c(1:length(anos))){
    ano <- barrio[barrio$ANO_1 == anos[h] ,]
    promedio <- mean(ano$V_REF)
    valor_maximo <- max(ano$V_REF)
    valor_minimo <- min(ano$V_REF)
###Construir la base de datos, esta base de datos me permite ver el valor de referencia
## pomedio por barrio por aÒo
    
    matrixSD = data.frame(barrio = as.character(barrio$SCANOMBRE[1]),
                          codigo_barrio = as.character(barrio$codigo_barrio[1]),
                          ano = ano$ANO_1[1],
                          promedio_val_m2 = promedio,
                          maximo_val_m2 = valor_maximo,
                          minimo_val_m2 = valor_minimo,
                          manzanas_totales = manzanas_tot)
                      
    #### construir la base de datos
    bd = rbind(bd, matrixSD)
  }
 plot
}

bd = bd[-1,]
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
write.csv(bd, file = "Valor_ref_promedio_barrio_base1.csv")

#### BASE 2 #####
##Lo que me interesa de esta base es ver el cambio en el tiempo del precio
### esto podria hacerse con un for que recorra cada uno de los aÒos para los cuales hay valor de referencia y vaya sumando o restando dependiendo de 
## del lugar en el que va, tendria que hacer los nombres variables tambien
database <-matrix(ncol=26)
barrios <- unique(na.omit(bd$codigo_barrio))
total<-data.frame()
colnames(database) <-  c("nombre", "code",   "nos" ,   "a"  ,    "dif" ,   "nos",    "a"  ,    "dif" ,   "nos",   
 "a"  ,    "dif"  ,  "nos"  ,  "a"  ,    "dif" ,   "nos" ,   "a",      "dif" ,   "nos" ,  
 "a" ,     "dif"  ,  "nos" ,   "a"    ,  "dif"  ,  "nos" ,   "a"   ,   "dif")


for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  barrio <- subset(barrio, !is.na(codigo_barrio))
  barrio <- barrio[order(barrio$ano) ,]
  nombre <- barrio$barrio[1]
  n <- length(barrio$barrio)
  code <- barrio$codigo_barrio[1]
  total <- data.frame(nombre,code)
  ##grafico de comportamiento por barrio del valor de referencia
  #gg <-ggplot(data=barrio, aes(x=ano, y=promedio_val_m2, group=1)) +
   # geom_line()+
  #  geom_point( size =3) + labs(title= paste(barrio$barrio), x= "aÒo", y = "valor promedio m2") + theme_gray() + 
   # theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13,  family= "Times New Roman"))  +  
  #  scale_y_continuous(breaks = pretty(barrio$promedio_val_m2, n=5))
  ##guardar la imagen del plot
#  setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/graficos")
 # png(filename = paste(barrio$barrio,"_",barrio$codigo_barrio,".png", sep=""))
  #plot(gg)
  #dev.off()
  for(j in (1:length(barrio[, 1]))){
   if (j < n){
     a<-ifelse(barrio$promedio_val_m2[j] > barrio$promedio_val_m2[j+1], "disminuyo", 
                                                               ifelse(barrio$promedio_val_m2[j] == barrio$promedio_val_m2[j+1],"se mantuvo", 
                                                                      ifelse(barrio$promedio_val_m2[j] < barrio$promedio_val_m2[j+1], "aumento")))
     dif <- barrio$promedio_val_m2[j+1] - barrio$promedio_val_m2[j] 
     nos <- paste(barrio$ano[j],"a",barrio$ano[j+1], sep="")
     
     add <- data.frame(nos,a,dif)
     total <- cbind(total,add)
     
     }
     dif_total <- barrio$maximo_val_m2 - barrio$minimo_val_m2
  }
  if (length(total) < 26){
    completar <- (26- length(total))
    agregar <- matrix(rep("NA",completar),ncol= completar, nrow=1)
    colnames(agregar)<-rep(c("nos","a","dif"),completar/3)
    total <- cbind(total,agregar)
    
    
  } 
  database <- rbind(database,total)
}

database = database[-1,]
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
write.csv(database, file = "diferencia_relativa_barrio_base2.csv")

#### BASE 3 #####
##Lo que me interesa de esta base es ver el cambio en el tiempo del precio
### esto podria hacerse con un for que recorra cada uno de los aÒos para los cuales hay valor de referencia y vaya sumando o restando dependiendo de 
## del lugar en el que va, tendria que hacer los nombres variables tambien
dif_minymax_ano <-matrix(ncol=5)
barrios <- unique(na.omit(bd$codigo_barrio))
colnames(dif_minymax_ano) <-  c("barrio_nombre", "codigo_barrio",   "anos" ,   "cambio_relativo"  ,    "diferencia" )


for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  barrio <- subset(barrio, !is.na(codigo_barrio))
  barrio <- barrio[order(barrio$ano) ,]
  nombre <- barrio$barrio[1]
  n <- length(barrio$barrio)
  code <- barrio$codigo_barrio[1]
  
            a<-ifelse(barrio$promedio_val_m2[1] > barrio$promedio_val_m2[n], "disminuyo", 
                      ifelse(barrio$promedio_val_m2[1] == barrio$promedio_val_m2[n],"se mantuvo", 
                             ifelse(barrio$promedio_val_m2[1] < barrio$promedio_val_m2[n], "aumento")))
            dif <- barrio$promedio_val_m2[n] - barrio$promedio_val_m2[1] 
            anos1 <- paste(barrio$ano[1],"a",barrio$ano[n], sep="")
            
            add <- data.frame(nos,a,dif)
            total <- cbind(total,add)
            vector = data.frame(barrio_nombre = as.character(barrio$barrio[1]),
                                  codigo_barrio = as.character(barrio$codigo_barrio[1]),
                                  anos = anos1,
                                  cambio_relativo = a,
                                diferencia = dif)
            
            dif_minymax_ano <- rbind(dif_minymax_ano,vector)
 
}

dif_minymax_ano = dif_minymax_ano[-1,]
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
write.csv(dif_minymax_ano,file = "valor_ano_minymax_base3.csv")

#### Ahora quisiera analizar estos comportamientos respecto al del resto de la ciudad
##para eso debe importar la base de datos de los valores de referencia de toda la ciudad
library(sf)
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/geodatos/valor de referencia no modificado")

val_ref <- st_read("Valor_Ref_M.shp")

###Homogenizar el formato de aÒo respecto al resto de variables
library(stringr)
val_ref$ANO_1 <- str_sub(val_ref$ANO,3,4)
anos <- unique(val_ref$ANO_1)

comp_general <- matrix(ncol=2)
colnames(comp_general) <- c("aÒo","promedio")
## oara ver el comportamiento de toda la ciudad
for (p in c(1:length(anos))){
 
    ano <- val_ref[val_ref$ANO_1 == anos[p] ,]
    prom_ano <- mean(ano$V_REF)
    info = data.frame(aÒo =ano$ANO_1[1],promedio =prom_ano)
    comp_general <- rbind(comp_general, info)
}
comp_general = comp_general[-1 ,]
comp_general <- comp_general[order(comp_general$aÒo) ,]

## para ver el comportamiento de las zonas de amenaza
comp_amenaza <- matrix(ncol=2)
colnames(comp_amenaza) <- c("aÒo","promedio")
anos <- unique(val_ref$ANO_1)

for (o in c(1:length(anos))){
  
  ano <- bd[bd$ano == anos[o] ,]
  prom_ano <- mean(ano$promedio_val_m2)
  info = data.frame(aÒo =ano$ano[1],promedio =prom_ano)
  comp_amenaza <- rbind(comp_amenaza, info)
}
comp_amenaza = comp_amenaza[-1 ,]
comp_amenaza <- comp_amenaza[order(comp_amenaza$aÒo) ,]

### PLOTS

library("ggpubr")

 general <- ggplot(data=comp_general, aes(x=aÒo, y=promedio, group = 1)) +
   geom_line(color = "#E46726", size =0.5)+
    geom_point( size =2.5, color = "#E46726") + labs(title= paste("Comportamiento general del m2 en Bogot·"), x= "aÒo", y = "valor promedio m2") + 
   theme_gray() + 
   theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13))  +  
    scale_y_continuous(breaks = pretty(comp_general$promedio, n=8))
 
library(scales)
 
amenaza <- ggplot(data=comp_amenaza, aes(x=aÒo, y=promedio, group = 1)) +
   geom_line(color = "#E46726", size =0.5)+
   geom_point( size =2.5, color = "#E46726") + labs(title= paste("Comportamiento en zonas de Amenaza
   del m2 en Bogot·"), x= "aÒo", y = "valor promedio m2") + 
   theme_gray() + 
  scale_y_continuous(label = unit_format() , breaks = pretty(comp_amenaza$promedio, n=8)) +
   theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13))  
  

figure <- ggarrange(general, amenaza,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)


###### Ahora vamos a hacer esta misma comparaciÛn pero por localidades
#################INFORMACI”N LOCALIDADES #########################

setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
library(scales)
library(ggplot2)
library("ggpubr")

Valor_General <- read.table("Valor_Ref_General.txt",sep =";", header =TRUE, dec= ",")
Valor_Amenaza <- read.table("Valor_Ref_Zonas_Amenaza.txt",sep =";", header = TRUE, dec = ",")

localidades <- unique(Valor_Amenaza$LocCodigo)

for (t in c(1:length(localidades))){
  comp_general <- matrix(ncol=2)
  colnames(comp_general) <- c("aÒo","promedio")
  
  comp_amenaza <- matrix(ncol=2)
  colnames(comp_amenaza) <- c("aÒo","promedio")
  
  localidad_general <- Valor_General[Valor_General$LocCodigo == localidades[t ] ,]
  localidad_general <- localidad_general[!(is.na(localidad_general$OBJECTID)) ,]
  localidad_amenaza <- Valor_Amenaza[Valor_Amenaza$LocCodigo == localidades[t ] ,]
  anos <- unique(localidad_general$ANO_1)
  anos_1 <- unique(localidad_amenaza$ANO_1)
  n <- length(localidad_general[, 1])
  n_1 <- length(localidad_amenaza[, 1])
  
  
  for (p in c(1:length(anos))){
    
    ano <- localidad_general[localidad_general$ANO_1 == anos[p] ,]
    prom_ano <- mean(ano$V_REF, na.rm= TRUE)
    std <- round(sd(ano$V_REF, na.rm= TRUE),2)
    maxi <- max(ano$V_REF, na.rm= TRUE)
    mini <- min(ano$V_REF, na.rm= TRUE)
    info = data.frame(aÒo =ano$ANO_1[1],promedio =prom_ano)
    comp_general <- rbind(comp_general, info)
  }
  for (g in c(1:length(anos_1))){
    ano_1 <- localidad_amenaza[localidad_amenaza$ANO_1 == anos_1[g] ,]
    prom_ano_1 <- mean(ano_1$V_REF, na.rm = TRUE)
    std_1 <- round(sd(ano_1$V_REF, na.rm = TRUE),2)
    maxi_1 <- max(ano_1$V_REF, na.rm = TRUE)
    mini_1 <- min(ano_1$V_REF, na.rm = TRUE)
    info_1 = data.frame(aÒo= ano_1$ANO_1[1],promedio =prom_ano_1)
    comp_amenaza <- rbind(comp_amenaza, info_1)
  }
  
  comp_general = comp_general[-1 ,]
  comp_general <- comp_general[order(comp_general$aÒo) ,]
  comp_amenaza = comp_amenaza[-1 ,]
  comp_amenaza <- comp_amenaza[order(comp_amenaza$aÒo) ,]
  
  
  general <- ggplot(data=comp_general, aes(x=aÒo, y=promedio, group = 1)) +
    geom_line(color = "#E46726", size =0.5)+
    geom_point( size =2.5, color = "#E46726") + labs(title= paste("Comportamiento general del m2 en ", localidad_general$LocNombre[1], sep = " "),
                                                     x= "aÒo", y = "valor promedio m2", subtitle = paste("numero de manzanas -",n, "desviaciÛn estandar -",
     std, "
     m·ximo -", maxi, "mÌnimo -", mini, sep =" ")) + 
    theme_gray() + 
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13))  +  
    scale_y_continuous(label= unit_format(), breaks = pretty(comp_general$promedio, n=8))
  
  amenaza <- ggplot(data=comp_amenaza, aes(x=aÒo, y=promedio, group = 1)) +
    geom_line(color = "#E46726", size =0.5)+
    geom_point( size =2.5, color = "#E46726") + labs(title= paste("Comportamiento en zonas de Amenaza
   del m2 en", localidad_amenaza$LocNombre[1], sep = " "), x= "aÒo", y = "valor promedio m2", subtitle = paste("numero de manzanas -",n_1, "desviaciÛn estandar -",
    std_1, "
    m·ximo -", maxi_1, "mÌnimo -", mini_1, sep =" ")) + 
    theme_gray() + 
    scale_y_continuous(label = unit_format() , breaks = pretty(comp_amenaza$promedio, n=8)) +
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13))  
  
  figure <- ggarrange(general, amenaza,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
  
  setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/graficos")
  png(filename = paste(localidad_amenaza$LocNombre[1],"_comp",".png", sep=""))
  plot(figure)
  dev.off()
  
}
##############################################################################
#########################################################################
### PROCESAMIENTO DE CONCEPTOS T…CNICOS ###################################7
### E l objetivo de esta secciÛn es ver si hay correlaciÛn del precio con el 
###aÒo en el que se emite el concepto tÈcnico øsube, baja?
library(dplyr)
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
CT<- read.table("CT_ValorRef_Barrios.txt",sep =";", header =TRUE, dec= ",")

CT <- CT  %>% 
  select("MANCODIGO","V_REF", "ANO", "ANO_1","cod_ct", "riesgo","localidad","barmanpre", "fecha_ct",
         "a√.o", "SCACODIGO", "SCANOMBRE")

CT <- CT %>%
  rename(ANO_CT = a√.o )

library(ggplot2)
library(scales)
library("ggpubr")
library(gridExtra)

barrios <- unique(CT$SCACODIGO)

alto <- CT[CT$riesgo == "A" | CT$riesgo == "ANM" | CT$riesgo == "SPPR" ,]
CT <- alto

CT <- CT  %>% 
  select("MANCODIGO","V_REF", "ANO", "ANO_1","cod_ct", "riesgo","localidad","barmanpre", "fecha_ct",
         "a√.o", "SCACODIGO", "SCANOMBRE")

for (b in c(1:length(barrios))){
  barrio <- CT[CT$SCACODIGO == barrios[b] ,]
  
  barrio_ct_vref <- matrix(ncol=2)
  colnames(barrio_ct_vref) <- c("aÒo","promedio")
  anos <- unique(barrio$ANO_1)
  for (g in c(1:length(anos))){
    ano <- barrio[barrio$ANO_1 == anos[g] ,]
    prom_ano <- mean(ano$V_REF, na.rm= TRUE)
    
    anos_CT <- unique(ano$ANO_CT)
    anos_CT1 <- substr(unique(ano$ANO_CT), start = 3, stop = 4)
    
    riesgo_CT <- unique(as.character(ano$riesgo))
    
    CT1 <- unique(as.character(ano$cod_ct))
   
    
    std <- round(sd(ano$V_REF, na.rm = TRUE),2)
    maxi <- max(ano$V_REF, na.rm = TRUE)
    mini <- min(ano$V_REF, na.rm = TRUE)
    info = data.frame(aÒo= ano$ANO_1[1],promedio =prom_ano)
    barrio_ct_vref <- rbind(barrio_ct_vref, info)
  }
  barrio_ct_vref = barrio_ct_vref[-1 ,]
  barrio_ct_vref <- barrio_ct_vref[order(barrio_ct_vref$aÒo) ,]
  
  grafica <- ggplot(data=barrio_ct_vref, aes(x=aÒo, y=promedio, group = 1)) +
    geom_line(color = "#E46726", size =0.5)+
    geom_point(aes(x=aÒo, y=promedio, group = 1, color = ifelse(aÒo %in% anos_CT1, "se emitio CT", "no se emitio CT")), size =2.5) + 
    labs( title= paste("Comportamiento del m2 en", barrio$SCANOMBRE[1], sep = " "),
                                                     x= "aÒo", y = "valor promedio m2", subtitle = paste("desviaciÛn estandar -",
                                                                                                         std, "
    m·ximo -", maxi, "mÌnimo -", mini, sep =" ")) + 
    labs(color = "EMISI”N CT")+
    theme_gray() + 
    theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13), plot.caption = element_text(hjust = 0.5))  +  
    scale_y_continuous(label= unit_format(), breaks = pretty(barrio_ct_vref$promedio, n=8)) 
    
    

  ##### Generar gr·ficos
  setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/graficos/CT alto riesgo")  
  png(filename = paste(barrio$SCANOMBRE[1],"_CT",".png", sep=""))
  plot(grafica)
  dev.off()
}




################### QIERO VER EL COMPORTAMIENTO DE TODA LA CIUDAD RESPECTO
### A CUANTO CRECIO EL PRECIO ENTRE EL A—O MAYOR Y EL MENOR QUE EXISTA POR BARRIO

##LEER LA BASE DE DATOS GENERAL
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
bd <- read.table("Valor_Ref_Barrios_TodaBogota.txt",sep =";", header =TRUE, dec= ",")
library(tidyverse)
library(dplyr)
library(plyr)

bd <- rename(bd, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))


barrios <- unique(bd$codigo_barrio)

##Crear una base de datos que haga este calculo tomando todos los valores de referencia que
## est·n por manzana y transformarlo a valores por barrio con promedios

bd <- matrix(ncol=7)
colnames(bd) <- c("barrio","codigo_barrio","ano","promedio_val_m2","maximo_val_m2","minimo_val_m2", "manzanas_totales")
bd$ANO_1 <- str_sub(bd$ANO, 8,9)


for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  anos <- unique(barrio$ANO_1)
  manzanas_tot <- length(unique(barrio$codigo_manzana))
  for (h in c(1:length(anos))){
    ano <- barrio[barrio$ANO_1 == anos[h] ,]
    promedio <- mean(ano$V_REF)
    valor_maximo <- max(ano$V_REF)
    valor_minimo <- min(ano$V_REF)
    ###Construir la base de datos, esta base de datos me permite ver el valor de referencia
    ## pomedio por barrio por aÒo
    
    matrixSD = data.frame(barrio = as.character(barrio$SCANOMBRE[1]),
                          codigo_barrio = as.character(barrio$codigo_barrio[1]),
                          ano = ano$ANO_1[1],
                          promedio_val_m2 = promedio,
                          maximo_val_m2 = valor_maximo,
                          minimo_val_m2 = valor_minimo,
                          manzanas_totales = manzanas_tot)
    
    #### construir la base de datos
    bd = rbind(bd, matrixSD)
  }
  plot
}

bd = bd[-1,]
setwd("~/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
write.csv(bd, file = "Valor_ref_promedio_barrio_base1.csv")


dif_minymax_ano <-matrix(ncol=5)
barrios <- unique(na.omit(bd$codigo_barrio))
colnames(dif_minymax_ano) <-  c("barrio_nombre", "codigo_barrio",   "anos" ,   "cambio_relativo"  ,    "diferencia" )


for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  barrio <- subset(barrio, !is.na(codigo_barrio))
  barrio <- barrio[order(barrio$ano) ,]
  nombre <- barrio$barrio[1]
  n <- length(barrio$barrio)
  code <- barrio$codigo_barrio[1]
  
  a<-ifelse(barrio$promedio_val_m2[1] > barrio$promedio_val_m2[n], "disminuyo", 
            ifelse(barrio$promedio_val_m2[1] == barrio$promedio_val_m2[n],"se mantuvo", 
                   ifelse(barrio$promedio_val_m2[1] < barrio$promedio_val_m2[n], "aumento")))
  dif <- barrio$promedio_val_m2[n] - barrio$promedio_val_m2[1] 
  anos1 <- paste(barrio$ano[1],"a",barrio$ano[n], sep="")
  
#  add <- data.frame(anos1,a,dif)
 # total <- cbind(total,add)
  vector = data.frame(barrio_nombre = as.character(barrio$barrio[1]),
                      codigo_barrio = as.character(barrio$codigo_barrio[1]),
                      anos = anos1,
                      cambio_relativo = a,
                      diferencia = dif)
  
  dif_minymax_ano <- rbind(dif_minymax_ano,vector)
  
}

dif_minymax_ano = dif_minymax_ano[-1,]
dif_minymax_ano$diferencia <- round(as.numeric(as.character(dif_minymax_ano$diferencia)),1)
dif_minymax_ano$codigo_barrio <- as.numeric(as.character(dif_minymax_ano$codigo_barrio))
setwd("D:/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")
write.csv(dif_minymax_ano,file = "valor_ano_minymax_bogota.csv")


#########################################################################################
### ANALISIS DE PRECIOS DEL SUELO AL LADO DE VARIABLES COMO: NIVELES DISTINTOS DE AMENAZA
### Y ESTRATO
#Lo primero es cargar las bases de datos con la informaciÛn y los librerios importantes

library(tidyverse)
library(dplyr)
library(plyr)

setwd("~/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")

db <- read.csv("Valor_Ref_AIMM_Estrato_Barrio_CT1.csv", header = TRUE, sep= ";", dec =",")

### para dejar el aÒo solo
db$ANO1 <- substring(db$ANO, 6, 10)

db1 <- db %>%
  select(MANCODIGO, Area_Manzana, V_REF, ANO1, Tipmenaza,FECHA_ACTO,DESCRIPCIO, TipAmenaza, AMENAZA, A√.O
         , ESTRATO,SCACODIGO, SCANOMBRE, cod_ct, riesgo, a√.o_1, )


#### Necesito homogenizar la columna de amenaza

db1 <- db1 %>%
  
  mutate(nivelAmenaza =case_when(DESCRIPCIO == ("Amenaza Baja")~ "Amenaza Baja",
                                 DESCRIPCIO == ("Amenaza Media")~ "Amenaza Media",
                                 DESCRIPCIO == ("Amenaza Alta")~ "Amenaza Alta",
                                 AMENAZA == ("Amenaza Media")~ "Amenaza Media",
                                 AMENAZA == ("Amenaza Alta")~ "Amenaza Alta",
                                 AMENAZA == ("Amenaza Baja")~ "Amenaza Baja",
                                 DESCRIPCIO == ("Ampliaci√≥n del cauce" ) ~ "Cauce de rio o quebrada",
                                 DESCRIPCIO == ("Cauce oficial") ~ "Cauce de rio o quebrada",
                                 DESCRIPCIO == ("Zonas de amortiguamiento propuestos") ~ "zonas de amortiguamiento"
                                 ))

db1 <- db1 %>%
  
  mutate(nivelRiesgo =case_when(riesgo == ("SPPR")~ "riesgo SPPR",
                                    riesgo == ("ANM")~ "riesgo ARNM",
                                    riesgo == ("M")~"riesgo medio",
                                    riesgo == ("B" )~"riesgo bajo",
                                    riesgo == ("A")~ "riesgo alto" ))

library(data.table)

nivelesA <- unique(db1$nivelAmenaza)
nivelesR <- unique(db1$nivelRiesgo)

###Es necesario filtrar la informaciÛn de solo un aÒo porque o sino me salen un monton de hectareas que no tengo que tener

db2 <- subset(db1, db1$ANO1 =="2018 ")

amenaza <- setkey(setDT(db2), nivelAmenaza)[,list(hectareas = round(sum(Area_Manzana),2), manzanas = length(MANCODIGO)) ,by=list(nivelAmenaza, Tipmenaza, TipAmenaza)]
riesgo <- setkey(setDT(db2), nivelRiesgo)[,list(hectareas = round(sum(Area_Manzana),2), manzanas = length(MANCODIGO)) ,by=list(nivelRiesgo)]

riesgo$hectareas <- format(riesgo$hectareas, scientific= FALSE)

amenaza$porcentajeManzanas <- round((amenaza$manzanas * 100)/sum(amenaza$manzanas),2)
amenaza$porcentajeHectareas <- round((amenaza$hectareas * 100)/sum(amenaza$hectareas),2)

riesgo$porcentajeManzanas <- round((riesgo$manzanas * 100)/sum(riesgo$manzanas),2)
riesgo$porcentajeHectareas <- round((riesgo$hectareas * 100)/sum(riesgo$hectareas),2)


write.csv(amenaza, file="amenaza_stadistics.csv")
write.csv(riesgo, file="riesgo_stadistics.csv")

########### Quiero sumar la diferencia entre el primer aÒo de registro y el ultimo
##para cada manzana de Bogot· para hacer el mapa comparativo###


bd <- rename(db1, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))


barrios <- unique(bd$codigo_barrio)
bd <- matrix(ncol=7)
colnames(bd) <- c("barrioname","codigo_barrio","ano","promedio_val_m2","maximo_val_m2","minimo_val_m2", "manzanas_totales")
bd$SCANOMBRE <-bd$SCANOMBRE(words = unlist(words))

for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  anos <- unique(barrio$ANO1)
  manzanas_tot <- length(unique(barrio$codigo_manzana))
  for (h in c(1:length(anos))){
    ano <- barrio[barrio$ANO1 == anos[h] ,]
    promedio <- mean(ano$V_REF)
    valor_maximo <- max(ano$V_REF)
    valor_minimo <- min(ano$V_REF)
    ###Construir la base de datos, esta base de datos me permite ver el valor de referencia
    ## pomedio por barrio por aÒo
    
    matrixSD = data.table(barrioname = as.character(barrio$SCANOMBRE[1]),
                          codigo_barrio = as.character(barrio$codigo_barrio[1]),
                          ano = ano$ANO1[1],
                          promedio_val_m2 = promedio,
                          maximo_val_m2 = valor_maximo,
                          minimo_val_m2 = valor_minimo,
                          manzanas_totales = manzanas_tot)
    
    #### construir la base de datos
    bd = rbind(bd, matrixSD)
  }
  plot
}

##esta secciÛn del codigo solo se puede correr una vez
bd = bd[-1,]
## Entonces esta base de datos creada bd, contiene informaciÛn de todos los barrios, aÒos
## su codigo, su valor promedio de referencia, maximo, minimo y numero de manzanas

write.csv(bd, file= "ValRefPromedio_Barrio_Bogota.csv")



## ahora esta es para calcular la diferencia entre el menor aÒo y el mayor para ponerlo  por
## todos los barrios y hacer mapas comparativos

dif_minymax_ano <-matrix(ncol=5)
barrios <- unique(na.omit(bd$codigo_barrio))
colnames(dif_minymax_ano) <-  c("barrio_nombre", "codigo_barrio",   "anos" ,   "cambio_relativo"  ,    "diferencia" )


for (l in c(1:length(barrios))){
  barrio <- bd[bd$codigo_barrio == barrios[l],]
  barrio <- subset(barrio, !is.na(codigo_barrio))
  barrio <- barrio[order(barrio$ano) ,]
  nombre <- barrio$barrio[1]
  n <- length(barrio$barrio)
  code <- barrio$codigo_barrio[1]
  
  a<-ifelse(barrio$promedio_val_m2[1] > barrio$promedio_val_m2[n], "disminuyo", 
            ifelse(barrio$promedio_val_m2[1] == barrio$promedio_val_m2[n],"se mantuvo", 
                   ifelse(barrio$promedio_val_m2[1] < barrio$promedio_val_m2[n], "aumento")))
  dif <- barrio$promedio_val_m2[n] - barrio$promedio_val_m2[1] 
  anos1 <- paste(barrio$ano[1],"a ",barrio$ano[n], sep="")
  
  #add <- data.frame(anos1,a,dif)
  #total <- cbind(total,add)
  vector = data.frame(barrio_nombre = as.character(barrio$barrioname[1]),
                      codigo_barrio = as.character(barrio$codigo_barrio[1]),
                      anos = anos1,
                      cambio_relativo = a,
                      diferencia = dif)
  
  dif_minymax_ano <- rbind(dif_minymax_ano,vector)
  
}

dif_minymax_ano = dif_minymax_ano[-1,]

##Esta base de datos lo que hace es tomar el aÒo mas reciente y el aÒo mas antiguo y restarlos
## para analizar el diferencial y pode comparar donde el precio a crecido m·s y donde ha crecido menos

write.csv(dif_minymax_ano, file = "dif_ValRefProm_Bogota.csv")

##### Ahora con la primera base de datos de esta secciÛn db2 que tiene la informaciÛn
##para el aÒo 2018, osea pre covid, osea valiosa donde ademas hay varias CT emitidos que son utiles
## Esta informaciÛn esta por manzanas y eso me interesa

db2 <- subset(db1, db1$ANO1 =="2018 ")

var_estratos <-matrix(ncol=5)
db2 <- rename(db2, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))

barrios <- unique(na.omit(db2$codigo_barrio))
colnames(dif_minymax_ano) <-  c("Estrato", "Valor Ref. promedio", "Amenaza o riesgo" ,  "cambio_relativo"  ,    "diferencia" )


estrato <- setkey(setDT(db2), ESTRATO)[,list(Val_Ref_promedio = mean(V_REF), manzanas = length(codigo_manzana), hectareas=sum(Area_Manzana)) ,by=list(ESTRATO)]

estratoamenaza <- setkey(setDT(db2), ESTRATO)[,list(Val_Ref_promedio = mean(V_REF), manzanas = length(codigo_manzana), hectareas=sum(Area_Manzana)) ,by=list(ESTRATO, nivelAmenaza,nivelRiesgo )]

estrato1 <- subset(db2, db2$ESTRATO == "1")

# verificamos que todo este bien con un ejemplo
#mean(estrato1$V_REF)
#[1] 466421
#> sum(estrato$manzanas)
#[1] 43902
#> sum(estrato1$Area_Manzana)
#[1] 2353.283

### tabla con informaciÛn por estrats sin amenaza
write.csv(estrato, file= "estratos_valrefpromedio.csv")
write.csv(estratoamenaza, file= "estratos_valrefpromedio_RyA.csv")

######Ahora vamos a hacr el tema de la diferencia maxima pero no 
###por barrio sino por manzana
library(tidyverse)
library(dplyr)
library(plyr)


bd <- rename(db1, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))


dif_minymax_ano <-matrix(ncol=8)
manzanas <- unique(na.omit(bd$codigo_manzana))
colnames(dif_minymax_ano) <-  c("barrio_nombre", "codigo_manzana",   "anos" ,   "cambio_relativo"  ,    "diferencia", "porc", "val1", "val2" )

for (l in c(1:length(manzanas))){
  manzana <- bd[bd$codigo_manzana == manzanas[l],]
  manzana <- subset(manzana, !is.na(codigo_manzana))
  manzana <- manzana[order(manzana$ANO1) ,]
  nombre <- manzana$SCANOMBRE[1]
  n <- length(manzana$codigo_manzana)
  code <- manzana$codigo_manzana[1]
  
  a<-ifelse(manzana$V_REF[1] > manzana$V_REF[n], "disminuyo", 
            ifelse(manzana$V_REF[1] == manzana$V_REF[n],"se mantuvo", 
                   ifelse(manzana$V_REF[1] < manzana$V_REF[n], "aumento")))
  dif <- manzana$V_REF[n] - manzana$V_REF[1] 
  anos1 <- paste(manzana$ANO1[1],"a ",manzana$ANO1[n], sep="")
  val11 <- manzana$V_REF[1]
  val21 <- manzana$V_REF[n]
  porce <- round((val21 *100)/val11, 1)
  
  
  #add <- data.frame(anos1,a,dif)
  #total <- cbind(total,add)
  vector = data.frame(barrio_nombre = nombre,
                      codigo_manzana = code,
                      anos = anos1,
                      cambio_relativo = a,
                      diferencia = dif,
                      porc= porce ,
                      val1 =  val11,
                      val2 = val21  )
  
  dif_minymax_ano <- rbind(dif_minymax_ano,vector)
  
}
 

dif_minymax_ano = dif_minymax_ano[-1,]



setwd("~/Google Drive/Master Governance of Risk and Resources/tesis/bases de datos")

write.csv(dif_minymax_ano, file="diferenciaxmanzana.csv")

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

db1 <- rename(db1, c("SCACODIGO" = "codigo_barrio","MANCODIGO" = "codigo_manzana"))

######Agregar por codigo manzana el crecimiento anual promedio##

db1 <- subset(db1,!is.na(codigo_manzana))

dif_prom <-matrix(ncol=4)
manzanas <- unique(na.omit(db1$codigo_manzana))
colnames(dif_prom) <-  c("codigo_manzana","anos" ,"aumento_prom_anual","diferencia_neta")

for (l in c(1:length(manzanas))){
  manzana <- db1[db1$codigo_manzana == manzanas[l],]
  manzana <- subset(manzana, !is.na(codigo_manzana))
  manzana <- manzana[order(manzana$ANO1) ,]
  for (y in c(1:length(manzana[, 1]))){
    if (manzana$V_REF[y] != 0){
      n1 <- y
      n <- length(manzana[, 1])
      anos1 <- paste(manzana$ANO1[n1],"a ",manzana$ANO1[n], sep="")
      promanual <- (manzana$V_REF[n] - manzana$V_REF[n1])/ n
      dif <- manzana$V_REF[n] - manzana$V_REF[n1]
      break
    }
  }

  vector = data.frame(codigo_manzana = manzana$codigo_manzana[1],
                      anos = anos1,
                      aumento_prom_anual = promanual,
                      diferencia_neta = dif)
  
  dif_prom <- rbind(dif_prom,vector)
}


dif_prom = dif_prom[-1,]

##trasponer la matriz

db2 <- db1 %>%
  pivot_wider(id_cols = c(codigo_manzana, Tipmenaza,FECHA_ACTO, DESCRIPCIO,TipAmenaza,
                          AMENAZA, A√.O, ESTRATO, codigo_barrio, SCANOMBRE, cod_ct, riesgo, a√.o_1), names_from =c(ANO1), values_from = c(V_REF), values_fn = mean)

db3 <- drop_na(db2, codigo_manzana)

db4 <- merge(x = db3, y = dif_prom, by.x= "codigo_manzana", by.y = "codigo_manzana", all =FALSE)


col_order <- c("codigo_manzana", "Area_Manzana", "Tipmenaza","FECHA_ACTO", "DESCRIPCIO","TipAmenaza",
               "AMENAZA", "A√.O", "ESTRATO", "codigo_barrio", "SCANOMBRE", "cod_ct", "riesgo", "a√.o_1",
               "2012", "2013", "2014", "2015", "2016", "2017", "2018","2019","2020", "anos", "aumento_prom_anual", "diferencia_neta" )
db5 <- db4[, col_order]
db4 <- as.data.frame(db4)


write.csv(db4, file="manzanasBogotaRA.csv")

####### Encontrar conceptos tecnicos interesantes apra solicitarlos por medio de
## de derecho de peticiÛn

##primero elegimos los que tienen conceptos tecnicos emitidos

db2 <- drop_na(db1, codigo_manzana)
db3 <- db2[!(db2$cod_ct =="") ,]
db3 <- db3 %>%
  
  mutate(nivelAmenaza =case_when(DESCRIPCIO == ("Amenaza Baja")~ "Amenaza Baja",
                                 DESCRIPCIO == ("Amenaza Media")~ "Amenaza Media",
                                 DESCRIPCIO == ("Amenaza Alta")~ "Amenaza Alta",
                                 AMENAZA == ("Amenaza Media")~ "Amenaza Media",
                                 AMENAZA == ("Amenaza Alta")~ "Amenaza Alta",
                                 AMENAZA == ("Amenaza Baja")~ "Amenaza Baja",
                                 DESCRIPCIO == ("Ampliaci√≥n del cauce" ) ~ "Cauce de rio o quebrada",
                                 DESCRIPCIO == ("Cauce oficial") ~ "Cauce de rio o quebrada",
                                 DESCRIPCIO == ("Zonas de amortiguamiento propuestos") ~ "zonas de amortiguamiento"
  ))


db4 <- db3[(db3$riesgo =="M")|(db3$riesgo =="A")|(db3$riesgo =="ANM") ,]  
db6 <- db4[(db4$nivelAmenaza == "Amenaza Baja") ,]
db6 <- db6[(db6$ESTRATO == "1") ,]

db5 <- db4[(db4$ESTRATO == "3")|(db4$ESTRATO == "4")|(db4$ESTRATO == "5") | (db4$ESTRATO == "6") ,]
 
manzanas <- unique(db5$codigo_manzana)  
manzanas <- na.omit(manzanas)

for (i in c (1:length(manzanas))){
  manzana <- db5[db5$codigo_manzana == manzanas[i] ,]
  manzana <- drop_na(manzana, ANO1)
  manzana <- manzana[order(manzana$ANO1) ,]
  anos_CT <- unique(manzana$a√.o_1)
  if (mean(manzana$V_REF != 0)){
  
  grafica <- ggplot(data=manzana, aes(x=ANO1, y=V_REF, group = 1)) +
  geom_line(color = "#E46726", size =0.5)+
  geom_point(aes(x=ANO1, y=V_REF, group = 1, color = ifelse(ANO1 %in% anos_CT, "se emitio CT", "no se emitio CT")), size =2.5) + 
  labs( title= paste("Comportamiento del m2 en", manzana$SCANOMBRE[1], sep = " "),
        x= "aÒo", y = "valor promedio m2") + 
  labs(color = "EMISI”N CT")+
  theme_gray() + 
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=13), plot.caption = element_text(hjust = 0.5)) 
  plot(grafica)
  }
}

db4 <- db3[(db3$riesgo =="A") ,]  
db6 <- db4[(db4$nivelAmenaza == "Amenaza Baja") ,]
db6 <- db6[(db6$ESTRATO == "1") ,]
           