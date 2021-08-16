
#--------------------------------------------------------------------------------
# Tema:       Ejercicio - 2
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      11-08-2021
# Datos:      Encuesta Telefonica sobre COVID-19 y Mercado Laboral, INEGI.
#             https://www.inegi.org.mx/investigacion/ecovidml/2020/#Microdatos
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:      Se recomienda seguir el procedimiento paso por paso     


# Objetivo:   Hacer una grafica para comparar las horas trabajadas por 
#             mujeres y hombres

#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias
library(foreign);library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo
setwd(".../Datos")


#3.  Importar datos
encovid<-read.dbf("ecovid0720.dbf")%>%data.frame()


#5. Filtrar la base: solo interesa los ocupados (CLASE2==1)
encovid=encovid%>%filter(CLASE2==1) 


#6. Etiquetar los datos de la variable sexo (PB1) en la misma variable. 
#   Los codigos son: (1=Hombre, 2=Mujer)
encovid$PB1 <- factor(encovid$PB1,levels = c(1,2),labels = c("Hombre","Mujer"))


#7. Tabula la variable PB1 para comprobar el etiquetado
wtd.table(encovid$PB1) 


#7.1 Tabula la variable horas trabajadas (PE10_1) para ver su comportamiento
wtd.table(encovid$PE10_1) 


#8. Dado que la variable es factor y necesitamos que sea continua, hay que cambiar
#   de formato la variable con la funcion as.numeric
encovid$PE10_1=as.numeric(encovid$PE10_1)


#9. Con la función class() verificamos que la variable PE10_1 es numerica
class(encovid$PE10_1)


#10. Cargamosr la base de datos e indicar las dos variables que vamos 
#    a graficar. Para guardar esto, crea un objeto llamado g (Ver código 4.4)  
g<-ggplot(encovid, aes(PE10_1, colour=PB1))     


#10. Al objeto "g" agrega la capa geom_freqpoly()
g+geom_freqpoly()


#11. Agrega un titulo, asi como nombre a los ejes
g+geom_freqpoly()+
  ggtitle("Horas trabajadas por sexo")+
  xlab("Horas trabajadas")+
  ylab("Sexo")+
  theme(plot.title =                           
          element_text(hjust = 0.5))



