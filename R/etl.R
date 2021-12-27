library(dplyr)

data <- read.csv("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
View(data)
data1 <- data[order(data[,11],data[,1],data[,2]), ]

data1 <- data1 %>%
  filter(F_UNIDADESVENTA>0.000001)

data1 <- data1 %>%
  mutate(
    MONTOVENTA_UNIDAD= case_when(
      F_UNIDADESVENTA >=1 ~ F_MONTOVENTA/F_UNIDADESVENTA,
      F_UNIDADESVENTA >=0 | F_MONTOVENTA <1  ~ F_MONTOVENTA
    )
  )

data1 <- data1 %>%
  mutate(
    COSTOVENTA_UNIDAD= case_when(
      F_UNIDADESVENTA >=1 ~ F_COSTOVENTA/F_UNIDADESVENTA,
      F_UNIDADESVENTA >=0 | F_COSTOVENTA <1  ~ F_COSTOVENTA
    )
  )

#Agregamos columna de la semana de una fila
data1['SEMANA'] <- strftime(data1[,1], format = "%V")


data2 <- data1[,c("SEMANA","DESC_LOCALFISICO","CLASE","COD_SKU","DESC_SKU","F_UNIDADESVENTA","F_MONTOVENTA","MONTOVENTA_UNIDAD","F_COSTOVENTA","COSTOVENTA_UNIDAD")]

data3 <- data2 %>% group_by(SEMANA,DESC_LOCALFISICO,CLASE,COD_SKU, DESC_SKU)  %>%
  summarise(TOTAl_F_UNIDADESVENTA = sum(F_UNIDADESVENTA),
            TOTAl_F_MONTOVENTA = sum(F_MONTOVENTA),
            VENTA_PROMEDIO = mean(MONTOVENTA_UNIDAD),
            CV_VENTA_PROMEDIO = ifelse(is.na(round(sd(MONTOVENTA_UNIDAD)/mean(MONTOVENTA_UNIDAD),2)),0,round(sd(MONTOVENTA_UNIDAD)/mean(MONTOVENTA_UNIDAD),2)),
            TOTAl_F_COSTOVENTA = sum(F_COSTOVENTA),
            COSTOVENTA_PROMEDIO = mean(COSTOVENTA_UNIDAD),
            .groups = 'drop')

clases_global <- distinct(data3[,c("CLASE")])
locales <- distinct(data3[,c("DESC_LOCALFISICO")])

for (clase in clases_global){
  #datos de determinada clase de todos los locales
  data_global <- data3 %>%
    filter(CLASE==clase) #Elaborados
  
  #semanas de datos
  semanas <- distinct(data_global[,c("SEMANA")])
  #skus de los productos de la clase
  skus <- distinct(data_global[,c("COD_SKU")])
  
  n_skus <- nrow(skus)
  n_weeks <- nrow(semanas)
  n_locales <-nrow(locales)
  
  etl_completa <-matrix(data=NA,nrow=1,ncol=n_skus+4)
  for (k in 1:n_locales){
    #data de un local en especifico para calcular su matriz de precios
    data_local <- data_global %>%
      filter(DESC_LOCALFISICO==as.character(locales[k,]))
    
    matriz_precios <- matrix(data=NA,nrow=n_weeks,ncol=n_skus)
    
    for (i in 1:n_weeks){
      for (j in 1:n_skus){
        consulta <- data_local %>%
          filter(COD_SKU==as.numeric(skus[j,]), SEMANA==as.numeric(semanas[i,]))
        matriz_precios[i,j] <- as.numeric(consulta["VENTA_PROMEDIO"])
      }
    }
    
    matriz_precios <- cbind(semanas,matriz_precios)
    n_precios <-nrow(matriz_precios)
    
    matriz_demanda_precios <- matrix(data=NA,nrow=1,ncol=n_skus+4)
    for (i in 1:n_skus){
      for (j in 1:n_weeks){
        consulta2 <- data_local %>%
          filter(COD_SKU==as.numeric(skus[i,]), SEMANA==as.numeric(semanas[j,]))
        demanda <- as.numeric(consulta2["TOTAl_F_UNIDADESVENTA"])
        
        temp1<- c(locales[k,],skus[i,],demanda,matriz_precios[j,])
        matriz_demanda_precios <- rbind(matriz_demanda_precios,temp1)
      }
    }
    
    matriz_demanda_precios <- matriz_demanda_precios[1:(n_weeks*n_skus)+1,]
    rownames(matriz_demanda_precios) <- NULL
    
    etl_completa <- rbind(etl_completa,matriz_demanda_precios)
    print(k)
  }
  etl_completa <- etl_completa[2:(n_weeks*n_skus*n_locales)+1,]
  
  nombres_columnas = c("DESC_LOCALFISICO","COD_SKU","DEMANDA","SEMANA")
  p_sku_nombres <- c()
  for (sku in skus){
    p_sku_nombres <- c(p_sku_nombres,sku)
  }
  
  colnames(etl_completa) <- c(nombres_columnas,paste("PrecioSKU",p_sku_nombres,sep=""))
  View(etl_completa)
  nombre_archivo <- paste("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/",clase,".csv", sep="")
  write.csv(nombre_archivo, row.names = FALSE)  
}


