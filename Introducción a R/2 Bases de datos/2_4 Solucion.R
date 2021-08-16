
#--------------------------------------------------------------------------------
# Tema:       Solucion del Ejercicio - 1
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      11-08-2021
# Datos:      I Trimestre ENOE nueva, INEGI.
#             https://www.inegi.org.mx/programas/enoe/15ymas/#Microdatos
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:      Se recomienda seguir el procedimiento paso por paso     


# Objetivo:   Obtener el tabulado de la Posicion en la ocupacion con su CV de
#             de los hombres

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign);library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo
setwd(".../Datos")


#3.  Importar datos
enoe<-read.dbf("ENOEN_SDEMT121.dbf")%>%data.frame()


#4. Tabular la variable POS_OCU (sin ponderar)
wtd.table(enoe$POS_OCU)


#5. Crear una nueva base y nombrarla enoe.3
    #   Filtrar: Hombres ocupados (SEX==1 & CLASE2==1) 
    #   Seleccionar variables: "SEX", "EDA","POS_OCU","FAC_TRI", "UPM", "EST_D_TRI","R_DEF","C_RES"
    #   Ayuda: ver punto 6.3
enoe.3 <- enoe%>%filter(SEX==1 & CLASE2==1)%>%select("SEX", "EDA","POS_OCU","FAC_TRI", "UPM", "EST_D_TRI","R_DEF","C_RES")


#6. Convertir las variables R_DEF, C_RES y EDA numericas
#   Ayuda: ver punto 7.1.1
enoe.3$R_DEF <-as.numeric(as.character(enoe.3$R_DEF))
enoe.3$C_RES <-as.numeric(as.character(enoe.3$C_RES))
enoe.3$EDA <-as.numeric(as.character(enoe.3$EDA))


#8. Filtrar casos validos para la ENOE para crear la base SD 
#   Ayuda: ver punto 7.1.2
SD<-enoe.3%>%filter(R_DEF==0)%>%filter(C_RES==1 | C_RES==3)%>%filter(EDA>=15 & EDA<=98)


#8.1 Definir el esquema de muestreo
#   Ayuda: ver punto 7.2
ds<-svydesign(id=~UPM, strata=~EST_D_TRI, weight=~FAC_TRI, data=SD, nest=TRUE)


#8.2 Colapsar estratos (en caso de tener 1 UPM en los estratos)
#   Ayuda: ver punto 7.3
options(survey.lonely.psu="adjust")


#8.3 Obtener la Estimacion
#   Ayuda: ver punto 7.4
estimacion<-svytotal(~factor(POS_OCU), ds, deff=TRUE)
estimacion

#8.4 Calcular el coeficiente de variacion
#   Ayuda: ver punto 7.5
coef_var<-cv(estimacion)*100


#8.5 Visualizar el resultados
#   Ayuda: ver punto 7.6
coef_var

