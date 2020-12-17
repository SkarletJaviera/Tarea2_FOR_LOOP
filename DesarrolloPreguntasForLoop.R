
install.packages("readxl")
install.packages("ggplot2")
library(readr)

########################   PREGUNTA 1   #####################################

G_chile <- data.frame(read.csv("grandes_chile.csv",header = TRUE, sep = ";" ),tamanio ="grande")
G_peru <- data.frame(read.csv("grandes_peru.csv", sep = ";" ),tamanio ="grande")
G_colombia <- data.frame(read.csv("grandes_colombia.csv", sep = ";" ),tamanio ="grande")
me_chile <- data.frame(read.csv("medianas_chile.csv", sep = ";" ),tamanio ="mediana")
me_peru <- data.frame(read.csv("medianas_peru.csv", sep = ";" ),tamanio ="mediana")
me_colombia <- data.frame(read.csv("medianas_colombia.csv", sep = ";" ),tamanio ="mediana")
p_chile <- data.frame(read.csv("pequena_chile.csv", sep = ";" ),tamanio ="pequena")
p_peru <- data.frame(read.csv("pequena_peru.csv", sep = ";" ),tamanio ="pequena")
p_colombia <- data.frame(read.csv("pequena_colombia.csv", sep = ";" ),tamanio ="pequena")
mi_chile <- data.frame(read.csv("micro_chile.csv", sep = ";" ),tamanio ="micro")
mi_peru <- data.frame(read.csv("micro_peru.csv", sep = ";" ),tamanio ="micro")
mi_colombia <- data.frame(read.csv("micro_colombia.csv", sep = ";" ),tamanio ="micro")


names(G_chile) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(G_colombia) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(G_peru) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(me_chile) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(me_colombia) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(me_peru) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(p_chile) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(p_colombia) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(p_peru) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(mi_chile) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(mi_colombia) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(mi_peru) = c("fecha", "pais", "ingresos", "costos", "porcentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")


######################   PREGUNTA 2   #######################################


Empresas <- rbind(G_chile,G_colombia,G_peru,me_chile,me_colombia,me_peru,mi_chile,mi_colombia,mi_peru,p_chile,p_colombia,p_peru)

Empresas  <-transform(
  Empresas,
  
  fecha = as.Date.character(fecha, format="%d-%m-%Y"),
  ingresos =as.numeric(gsub(",",".",ingresos)),
  costos =as.numeric(gsub(",",".",costos)),
  porcentaje_mujeres = as.numeric((gsub(",",".",porcentaje_mujeres))),
  exportaciones =as.numeric((gsub(",",".",exportaciones))),
  importaciones =as.numeric((gsub(",",".",importaciones))),
  endeudamiento =as.numeric((gsub(",",".",endeudamiento))),
  morosidad =as.numeric((gsub(",",".",morosidad))),
  reservas =as.numeric((gsub(",",".",reservas))),
  spread=as.numeric((gsub(",",".",spread))),
  tasa_interes=as.numeric((gsub(",",".",tasa_interes))))

###################### PREGUNTA 6 ############################################33



for (i in 1:nrow(Empresas)) {
  
  ingreso =((Empresas[i, "ingresos"]))
  tasa_interes =(Empresas[i, "tasa_interes"])
  exportaciones =round((Empresas[i, "exportaciones"]), digits=1)
  
  if(exportaciones>2.1){
    Empresas[i,"exportaciones"]=1
    
  }else if(exportaciones<2.1){
    Empresas[i,"exportaciones"]=2
    
  }else if(exportaciones==2.1)  {
    Empresas[i,"exportaciones"]=3