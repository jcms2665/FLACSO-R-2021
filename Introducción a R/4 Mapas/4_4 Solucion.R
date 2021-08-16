
#--------------------------------------------------------------------------------
# Tema:       Ejercicio - 3
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      11-08-2021
# Datos:      Shape de Estados, INEGI.
#             CONACYT (casos diarios):
#             https://datos.covid-19.conacyt.mx/#DownZCSV
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:      Se recomienda seguir el procedimiento paso por paso     


# Objetivo:   Hacer un mapa con 4 rangos y personalizarlo

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign); library(ggplot2); library(dplyr); library(tidyverse); library(sf)


#2.  Directorio de trabajo (SHAPE)
setwd(".../Shape")


#3.  Visualizar la base de datos asociada shape (No es el shape)
base_mx=read.dbf("00ent.dbf")


base_mx=select(base_mx, -c("rango","SUMA"))

base_mx=select(base_mx, -c("rango"))

#-----------------------------------
# NOTA:
# Necesitamos identificar una variable 
# que sirva como LLave para unir dos
# tablas.
#-----------------------------------

#4.   Crear la variable a graficar

      #4.1 Buscar la base de datos del numero de casos de covid
      setwd(".../Datos")
      covid=read.csv("Casos_Diarios_Estado_Nacional_Confirmados_20210809.csv")
      
            #4.1.1 Crear la variable SUMA
            covid=covid%>%mutate(SUMA=rowSums(.[4:542]))    
            
            #4.1.2 Quitar el total
            
            covid=covid%>%filter(cve_ent>=1)
            
            #4.1.3 Seleccionar la variable de estado y SUMA
            covid=covid%>%select(cve_ent, SUMA)
      
      #4.2 Agregar un 0 para tener el mismo formato
      covid=covid%>%mutate(cve_ent=formatC(cve_ent, width = 2, format = "d", flag = "0"))
      covid=covid%>%data.frame()
      
      #4.3 Dividir la muestra en terciles
      quantile(covid$SUMA, probs = c(.25,.50,.75,1))
      
      
      #4.4 Crear 3 rangos: Bajo, Medio y Alto
      covid$rango=""
      covid$rango[covid$SUMA<=38916]<-"S1"
      covid$rango[covid$SUMA>38916 & covid$SUMA<=57287]<-"S2"
      covid$rango[covid$SUMA>57287 & covid$SUMA<=87741]<-"S3"
      covid$rango[covid$SUMA>87741]<-"S4"
      
      #4.5 De la base covid solo nos quedamos con cve_ent y rango
      covid=covid %>% select("cve_ent", "rango", "SUMA")
      
      
      
      #4.6 Unir tabla de rangos con el dbf del shape
      # Indicar la columna Llave en ambos casos
      base_mx=base_mx%>%left_join(covid,
                                  by = c("CVE_ENT" = "cve_ent"))
      
      #4.7 Guardamos la base base_mx en donde esta el shp
      #-----------------------------------
      # NOTA:
      # Con este procedimiento se sustituye 
      # la base original.
      #-----------------------------------
      
      
      setwd(".../Shape")
      write.dbf(base_mx,"00ent.dbf")
      rm("base_mx","covid")
      
      #5. Cargar el SHAPE
      
      
      mx=st_read("00ent.shp")
      
      #6. Agrega una capa llamada "mapa"
      mapa=ggplot(mx) + geom_sf() 
      mapa
      
      
      
      #7. Mapa con variable discreta
      mx%>%
        ggplot(aes(fill = rango)) +
        geom_sf(colour = "white", size = 0.07) +
        labs(title = "Covid-19 en MX") +
        scale_fill_brewer("Total de casos", palette = "Reds") +
        theme_bw()
      