  rm(list = ls(all = TRUE)); 
  setwd("/home/lio/Documentos/Python/Programa Big Data/TP3")
  library(data.table)
  library(readxl)
  library(ggplot2)
  library(WriteXLS)
  library(plyr)
  library(dplyr)
  library(glmnet)
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(rattle)
  library(RGtk2)
  ###PARTE 1
  
  # Descargamos de la página del INDEC los microdatos de la EPH para el primer trimestre de 2019 en formato xls. 
  dataset <- read_xls("usu_individual_t119.xls")
  
  # Creamos  una columna llamada "ninios" que tenga la  cantidad de niños por hogar (a saber: los miembros de un mismo hogar comparten código CODUSU), donde llamamos niño a todo individuo de 10 años o menos. 
  dataset$menor  <- ifelse(dataset$CH06 <= 10, 1, 0)
  df1 = aggregate(dataset$menor, by=list(CODUSU=dataset$CODUSU), FUN=sum)
  dataset <- merge(dataset,df1,by="CODUSU")
  dataset$menor <- NULL
  dataset <- rename(dataset, ninios = x)
  
  # Descartamos de la base a todas las personas menores a 16 años y aquellas que no hayan contestado la encuesta individual.
  dataset <- dataset[!(dataset$CH06 < 16), ]
  dataset <- dataset[!(dataset$ESTADO == 0), ]
  
  # En dunción de la dimensión ESTADO creamos una variable binaria llamada trabaja que tome valor 1 si la persona trabaja y valor 0 en caso contrario
  dataset$trabaja  <- ifelse(dataset$ESTADO == 1, 1, 0)
  
  # Partimos la base dataset en dos en función del sexo
  dataset_varones <- dataset[(dataset$CH04 == 1), ]
  dataset_mujeres <- dataset[(dataset$CH04 == 2), ]
  
  #Vamos a analizar los NAs
  # filtramos NAs de IPCF
  for(j in setdiff(names(dataset), "trabaja")){
    dataset[, j] <- ifelse(dataset[, j] == 0, NA, dataset[, j])
  }
  
  #filtramos todos los valores donde sí hay missings de IPCF y no hay missings de PP07G4 
  d = dataset[is.na(dataset$IPCF) & !is.na(dataset$PP07G4), 
              c("IPCF", "PP07G4")]
  
  #filtramos todos los valores donde no hay missings de IPCF  ni de  PP07G4  
  d_normal = dataset[!is.na(dataset$IPCF)  & !is.na(dataset$PP07G4), 
                     c("IPCF", "PP07G4")]
  
  # para los datos que no tienen missings de uno y otro lado calculamos la proporción de las personas que tienen descuentos de obra social y que no lo tienen
  prop_PP07G4 = table(d_normal$PP07G4) / nrow(d_normal)
  title = sprintf("Relacion entre missings de IPCF y PP07G4")
  plot(prop_PP07G4, type="b", 
       col="blue", main=title)
  
  # Ahora calculamos la proporción pero para los datos que tienen missings de IPCF y que no tienen missings en PP07G4
  prop_PP07G4_missing = table(d$PP07G4)[1:2] / nrow(d)
  lines(prop_PP07G4_missing, type="l", col="red")
  
  #repetimos el mismo proceso con la variable = "CAT_OCUP"
  d = dataset[is.na(dataset$IPCF) & !is.na(dataset$CAT_OCUP), 
              c("IPCF", "CAT_OCUP")]
  
  d_normal = dataset[!is.na(dataset$IPCF) & !is.na(dataset$CAT_OCUP), 
                     c("IPCF", "CAT_OCUP")]
  prop_CAT_OCUP = table(d_normal$CAT_OCUP) / nrow(d_normal)
  title = sprintf("Relacion entre missings de IPCF y CAT_OCUP")
  plot(prop_CAT_OCUP, type="b", 
       col="blue", main=title)
  
  prop_CAT_OCUP_missing = table(d$CAT_OCUP) / nrow(d)
  lines(prop_CAT_OCUP_missing, type="l", col="red")
  
  prop_CAT_OCUP
  prop_CAT_OCUP_missing
  
  # repetimos análisis con nivel educativo
  ggplot(dataset, aes(x= as.factor(NIVEL_ED), y=IPCF)) + geom_boxplot() + ylim(0,60000)
  
  d = dataset[is.na(dataset$IPCF) & !is.na(dataset$NIVEL_ED), 
              c("IPCF", "NIVEL_ED")]
  d_normal = dataset[!is.na(dataset$IPCF) & !is.na(dataset$NIVEL_ED), 
                     c("IPCF", "NIVEL_ED")]
  prop_NIVEL_ED = table(d_normal$NIVEL_ED)/ nrow(d_normal)
  title = sprintf("Relacion entre missings de IPCF y NIVEL EDUCATIVO")
  plot(prop_NIVEL_ED, type="b", 
       col="blue", main=title)
  
  prop_NIVEL_ED_missing = table(d$NIVEL_ED) / nrow(d)
  lines(prop_NIVEL_ED_missing, type="l", col="red")
  
  #Relación no lineal de edad e ingresos
  ggplot(dataset, aes(x= CH06, y= IPCF)) + geom_point(alpha=0.01) + geom_smooth()+ ylim(c(0,50000))
  
  #histograma de IPCF
  ggplot(dataset, aes(IPCF))+  geom_histogram() + xlim(c(1,60000))
  
  #proporción de missings en CAT_OCUP
  sum(is.na(dataset$CAT_OCUP))/nrow(dataset)
  
  #decidimos reemplazar missings de IPCF con la media
  
  dataset$IPCF[is.na(dataset$IPCF)] <- round(mean(dataset$IPCF, na.rm = TRUE))
  
  #7
  #Ingreso Promedio
  ingresov <- aggregate( IPCF ~ trabaja, dataset_varones, mean )
  ingresov$sexo <- "varones"
  ingresom <- aggregate( IPCF ~ trabaja, dataset_mujeres, mean )
  ingresom$sexo<- "mujeres"
  itotal <- rbind(ingresom,ingresov)
  
  ggplot(itotal, aes(x=as.factor(trabaja), y=IPCF)) + 
      geom_point(aes(colour =factor(trabaja))) + 
      scale_colour_manual(values = c("red", "blue"), labels = c("No trabaja", "Trabaja")) +
      facet_grid(. ~ sexo) + labs(title ="Ingreso promedio según sexo y estado", x = "", y = "Ingreso")
  
  # Edad promedio
  edadv <- aggregate( CH06 ~ trabaja, dataset_varones, mean )
  edadv$sexo <- "varones"
  edadm <- aggregate( CH06 ~ trabaja, dataset_mujeres, mean )
  edadm$sexo<- "mujeres"
  total_edad <- rbind(edadm,edadv)
  
  ggplot(total_edad, aes(x=as.factor(trabaja), y=CH06)) + 
    geom_point(aes(colour =factor(trabaja))) + 
    scale_colour_manual(values = c("red", "blue"), labels = c("No trabaja", "Trabaja")) +
    facet_grid(. ~ sexo)+ labs(title ="Edad promedio según sexo y estado", x = "", y = "Edad") 
  
  # Cantidad de Niños por hogar
  niniosv <- aggregate( ninios ~ trabaja, dataset_varones, mean )
  niniosv$sexo <- "varones"
  niniosm <- aggregate( ninios ~ trabaja, dataset_mujeres, mean )
  niniosm$sexo<- "mujeres"
  total_ninios <- rbind(niniosm,niniosv)
  
  ggplot(total_ninios, aes(x=as.factor(trabaja), y=ninios)) + 
    geom_point(aes(colour =factor(trabaja))) + 
    scale_colour_manual(values = c("red", "blue"), labels = c("No trabaja", "Trabaja")) +
    facet_grid(. ~ sexo)+ labs(title ="Promedio de niños en hogares según sexo y estado de los padres", x = "", y = "Niños") 
  
  # Inactivos
  amos_de_casa= c(nrow(dataset_varones[dataset_varones$CAT_INAC == 4 , ]), nrow(dataset_mujeres[dataset_mujeres$CAT_INAC == 4 , ]))
  jubilados = c(nrow(dataset_varones[dataset_varones$CAT_INAC == 1 , ]) ,nrow(dataset_mujeres[dataset_mujeres$CAT_INAC == 1 , ])) 
  inactivos <- data.frame (amos_de_casa, jubilados,"Sexo" = c("varones", "mujeres") )
  inactivos$amos_de_casa <- inactivos$amos_de_casa/sum(inactivos$amos_de_casa)
  inactivos$jubilados <- inactivos$jubilados/sum(inactivos$jubilados)
  inactivos
  
  ###PARTE 2
  dataset_orig <- dataset
  dataset$trabaja <- factor(dataset$trabaja, labels = c("0", "1"))
  
  #pasamos todos los NAs a numérica negativa para que lo considere como otro valor y pueda hacer un split por los valores NA
  for(j in setdiff(names(dataset), "trabaja")){
    dataset[, j] <- ifelse(is.na(dataset[, j]), -9, dataset[, j])
  }
  
  #Seleccionamos las variables que  estan vinculadas a participación en el trabajo o que no son relevantes
  eliminadas= c( "CODUSU","COMPONENTE", "PONDERA", "NRO_HOGAR", "ANO4", "TRIMESTRE", "NRO_HOGAR","CH05","CH15_COD", "CH16_COD", "H15", "V1", "V2", "V21", "V22", "V3", "V4", "V5", "ESTADO", "CAT_INAC", "CAT_OCUP", "PP02C1", "PP02C2", "PP02C3", "PP02C4", "PP02C5", "PP02C6", "PP02C7", "PP02C8", "PP02E", "PP02H", "PP02I", "PP03C", "PP03D","PP03G","PP03H", "PP3E_TOT", "PP3F_TOT", "PP03I", "PP03J", "INTENSI", "PP04A", "PP04B_CAES","PP04B_COD", "PP04B1", "PP04B2", "PP04B3_MES", "PP04B3_ANO", "PP04B3_DIA", "PP04C", "PP04C99", "PP04D_COD", "PP04G", "PP05B2_MES", "PP05B2_ANO", "PP05B2_DIA", "PP05C_1", "PP05C_2", "PP05C_3", "PP05E", "PP05F", "PP05H", "PP06A", "PP06C", "PP06D", "PP06E", "PP06H", "PP07A", "PP07C", "PP07D", "PP07E", "PP07F1", "PP07F2", "PP07F3", "PP07F4", "PP07F5", "PP07G1", "PP07G2", "PP07G3", "PP07G4", "PP07G_59", "PP07H", "PP07I", "PP07J", "PP07K", "PP08D1", "PP08D4", "PP08F1", "PP08F2", "PP08J1", "PP08J2", "PP08J3", "PP09A", "PP09A_ESP", "PP09B", "PP09C", "PP09C_ESP", "PP10A", "PP10C", "PP10D", "PP10E", "PP11A", "PP11B_CAES", "PP11B_COD", "PP11B1", "PP11B2_MES", "PP11B2_ANO", "PP11B2_DIA", "PP11C", "PP11C99", "PP11C99", "PP11D_COD", "PP11G_ANO", "PP11G_MES", "PP11G_DIA", "PP11L", "PP11L1", "PP11M", "PP11N", "PP11O", "PP11P", "PP11Q", "PP11R", "PP11S", "PP11T", "P21", "DECOCUR", "IDECOCUR", "RDECOCUR", "GDECOCUR", "PDECOCUR", "ADECOCUR", "TOT_P12", "P47T", "DECINDR", "IDECINDR", "RDECINDR", "GDECINDR", "PDECINDR", "ADECINDR", "V2_M", "V3_M", "V4_M", "V5_M", "V11_M", "V21_M", "T_VI", "DECIFR", "IDECIFR", "RDECIFR", "GDECIFR", "PDECIFR", "ADECIFR", "DECCFR", "IDECCFR", "RDECCFR", "GDECCFR", "PDECCFR", "ADECCFR", "IDIMPP", "PONDII", "PONDIH", "PONDIIO", "ITF", "IMPUTA", "V19_AM")
  vars = setdiff(names(dataset), eliminadas)
  dataset <- dataset[, vars]
  explicativas= setdiff(vars, "trabaja")
  
  #convertimos a factor las variables categoricas
  num_vars= c("CH06", "V8_M", "V9_M", "V10_M", "V12_M", "V18_M", "IPCF")
  for(j in setdiff(names(dataset), c(num_vars, "trabaja"))){
    dataset[, j] <- as.factor(dataset[, j])
  }
  
  #volvemos a separar la base entre varones y mujeres
  dataset_varones <- dataset[(dataset$CH04 == 1), ]
  dataset_mujeres <- dataset[(dataset$CH04 == 2), ]
  
  #Arboles
  # train/test mujeres
  inx_sample = sample(nrow(dataset_mujeres), nrow(dataset_mujeres)*.7)
  train <- dataset_mujeres[inx_sample,]
  test <- dataset_mujeres[-inx_sample,]
  
  X_test <- test[, explicativas]
  y_test <- test$trabaja
  
  tree.mujeres <- rpart(trabaja ~ ., train, method = "class", 
                        control = rpart.control(cp = 0.005))
  fancyRpartPlot(tree.mujeres,tweak = 2, main= "Mujeres")
  summary(tree.mujeres)
  
  pred_test_mujeres = as.vector(predict(tree.mujeres, X_test)[,2])
  pred_test_mujeres = ifelse(pred_test_mujeres > 0.5, 1, 0)
  
  tbl_mujeres = table(y_test, pred_test_mujeres)
  table(y_test, pred_test_mujeres)
  
  # la matriz de confusion se presenta así
  #           y_pred
  # y_test    1   0
  #      1   2217 881 
  #      0  1054  2985
  
  accuracy_mujeres = c(2217 + 2945) / sum(tbl_mujeres)
  # 0.72
  
  # train/test varones
  inx_sample = sample(nrow(dataset_varones), nrow(dataset_varones)*.7)
  train <- dataset_varones[inx_sample,]
  test <- dataset_varones[-inx_sample,]
  
  X_test <- test[, explicativas]
  y_test <- test$trabaja
  
  tree.varones <- rpart(trabaja ~ ., train, method = "class", 
                        control = rpart.control(cp = 0.005))
  fancyRpartPlot(tree.varones,tweak = 1.5, main= "Hombres")
  summary(tree.varones)
  pred_test_varones = as.vector(predict(tree.varones, X_test)[,2])
  pred_test_varones = ifelse(pred_test_varones > 0.5, 1, 0)
  tbl_varones = table(y_test, pred_test_varones)
  table(y_test, pred_test_varones)
  accuracy_varones = c(3726 + 1520) / sum(tbl_varones)
  # 0.82
  
  #           pred_test_varones
  # y_test    1  0 
  #      1  3726 411 
  #      0  738 1520 

#5)

# convertimos a NAs de vuelta los -9
for(j in num_vars){
  dataset_mujeres[, j] <- ifelse(dataset_mujeres[, j] == -9, NA, dataset_mujeres[, j])
  dataset_varones[, j] <- ifelse(dataset_varones[, j] == -9, NA, dataset_varones[, j])
}

for(j in num_vars){
  dataset_mujeres[, j] <- ifelse(is.na(dataset_mujeres[, j]), 
                                 median(dataset_mujeres[, j], na.rm=T), 
                                 dataset_mujeres[, j])
  dataset_varones[, j] <- ifelse(is.na(dataset_varones[, j]), 
                                 median(dataset_varones[, j], na.rm=T), 
                                 dataset_varones[, j])
}


# lasso / mujeres

dataset_mujeres$CH04 <- NULL
xmm <- model.matrix(trabaja ~ ., data=dataset_mujeres)
y = as.numeric(as.character(dataset_mujeres$trabaja))

inx_sample = sample(nrow(xmm), nrow(xmm) * .7)
X_train <- xmm[inx_sample,]
X_test <- xmm[-inx_sample,]
y_test <- y[-inx_sample]

cv_lasso = cv.glmnet(x=X_train, y= y[inx_sample], alpha=1, 
                     family='binomial', type.measure='auc')
plot(cv_lasso)
bestlam = cv_lasso$lambda.min
bestlam

pred_test_mujeres = as.vector(predict(cv_lasso, X_test, type='response')[,1])
pred_test_mujeres = ifelse(pred_test_mujeres > 0.5, 1, 0)
tbl_mujeres = table(y_test, pred_test_mujeres)

# pred_test_mujeres
# y_test    0    1
#      0 1858 1271
#      1  909 3099

accuracy_lasso_mujeres = sum(diag(tbl_mujeres)) / sum(tbl_mujeres)
# 0.6945495

# lasso / varones
dataset_varones$CH04 <- NULL
xmm <- model.matrix(trabaja ~ ., data=dataset_varones)
y = as.numeric(as.character(dataset_varones$trabaja))

inx_sample = sample(nrow(xmm), nrow(xmm) * .7)
X_train <- xmm[inx_sample,]
X_test <- xmm[-inx_sample,]
y_test <- y[-inx_sample]

cv_lasso = cv.glmnet(x=X_train, y= y[inx_sample], alpha=1, 
                     family='binomial', type.measure='auc')
plot(cv_lasso)
bestlam = cv_lasso$lambda.min
bestlam

pred_test_varones = as.vector(predict(cv_lasso, X_test, type='response')[,1])
pred_test_varones = ifelse(pred_test_varones > 0.5, 1, 0)
tbl_varones = table(y_test, pred_test_varones)

# pred_test_varones
# y_test    0    1
#      0 3720  440
#      1  930 1305

accuracy_lasso_varones = sum(diag(tbl_varones)) / sum(tbl_varones)
# 0.7857

# para interpretar
coefficients(cv_lasso, s=0.02)

# lasso / varones


# 6
num_vars= c("CH06", "V8_M", "V9_M", "V10_M", "V12_M", "V18_M", "V19_AM", "IPCF")
for(j in setdiff(names(dataset), c(num_vars, "trabaja"))){
  dataset[, j] <- as.factor(dataset[, j])
}

tree_all <- rpart(trabaja ~ ., dataset, method = "class", 
                      control = rpart.control(cp = 0.005))
fancyRpartPlot(tree_all,tweak = 1.5, main = "TODOS")
summary(tree_all)


