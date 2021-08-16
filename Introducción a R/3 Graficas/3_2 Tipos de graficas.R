
#--------------------------------------------------------------------------------
# Tema:       Graficas con variables discretas y continuas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      10-08-2021
# Datos:      Encuesta Telefonica sobre COVID-19 y Mercado Laboral, INEGI.
#             https://www.inegi.org.mx/investigacion/ecovidml/2020/#Microdatos
# Github:     https://github.com/jcms2665/Tools-for-Demography/tree/main/R
# Notas:       

# Contenido

#       0. Preparar entorno de trabajo
#       1. Cargar librerias
#       2. Directorio de trabajo
#       3. Importar datos
#           3.1 Etiquetar variables
#           3.2 Etiquetar datos
#       4. Graficas (ggplot)
#           4.1 Con una variable discreta
#           4.2 Con dos variables discretas
#           4.3 Con una variable continua
#           4.4 Una variable continua y una discreta

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign); library(questionr); library(ggplot2); library(dplyr) 


#2.  Directorio de trabajo

setwd(".../3_Datos")



#3.  Importar datos


encovid=read.dbf("ecovid0720.dbf")%>%data.frame()


    # 3.1 Subconjuntos de datos

    # CLASE2=Condicion de ocupacion
    # Codigos: 1=Ocupado, 2=Desocupado, 3=Disponible, 4=No disponible
    
    # Convertir la variable a dato numerico
    encovid$CLASE2=as.numeric(encovid$CLASE2)
    
    # Filtrar la base, necesitamos a los ocupados CLASE2=1
    ocupados=encovid%>%filter(CLASE2==1)

    # 3.2 Etiquetar datos
        
    # PB1=SEXO
    # Codigos: 1=Hombre, 2=Mujer
    ocupados$PB1 <- factor(ocupados$PB1,levels = c(1,2),labels = c("Hombre","Mujer"))
    
    # PB1=Posicion en la ocupacion
    # Codigos: 1=Suborninado, 2=Independiente, 3=Disponible, 0=No aplica
    ocupados$POS_OCU <- factor(ocupados$POS_OCU,levels = c(0,1,2,3),labels = c("No Aplica","Suborninado","Independiente","Disponible"))
    
    #---------------------------------------
    # NOTA:
    # Estamos recodificando en la misma
    # variable, entonces se pierden los 
    # valores originales.
    #---------------------------------------
    
    

#4. Graficas (ggplot)
    
    #4.1 Con 1 variable discreta
    
    # Comentario:
    class(ocupados$PB1)    
    ggplot(ocupados,aes(PB1))+geom_bar(fill="blue")
    
    
    # Comentario:
    ggplot(ocupados,aes(PB1))+geom_bar(fill="blue")+
        xlab("Sexo")+
        ylab("Personas")+
        ggtitle("Comparacion entre hombres y mujeres")


    #4.2 Con 2 variables discretas
    
    g1<-ggplot(ocupados, aes(POS_OCU))            # Base de datos y variable
    g1+geom_bar(fill="yellowgreen")+
      facet_wrap(~ PB1)+                          # facet_wrap divide a la pantalla
      ggtitle("Ocupacion por sexo")+
      xlab("Posicion de la ocupacion")+
      ylab("Personas")
    
    
    g2<-ggplot(ocupados,aes(x=POS_OCU,fill=PB1))   # Base de datos y variable
    g2+geom_bar(aes(weights=ocupados$FAC_PER))+    # weights pondera la base para tener datos reales
      ggtitle("Ocupacion por sexo")+
      xlab("Posicion de la ocupacion")+
      ylab("Personas")
    
    
    g3<-ggplot(ocupados,aes(x=POS_OCU,fill=PB1))   # Base de datos y variable
    g3+geom_bar(aes(weights=ocupados$FAC_PER),
                position = "dodge")+
      ggtitle("Ocupacion por sexo")+              # position ajusta la posicion de las barras
      xlab("Posicion de la ocupacion")+
      ylab("Personas")



    #4.3 Con 1 variable continua
    
    class(ocupados$PB2)                            
    ocupados$PB2<-as.numeric(ocupados$PB2)
    class(ocupados$PB2)
    
    # PB2=Edad
    g4<-ggplot(ocupados, aes(PB2))                 # Base de datos y variable
    
    
    # Agregamos diferentes tipos de graficas
    g4+geom_area(stat="bin")
    
    g4+geom_freqpoly()
    
    g4+geom_histogram(binwidth = 5)
    
    #---------------------------------------
    # NOTA:
    # g4 guarda la base de datos y la variable
    # en un objeto, al cual lo podemos 
    # usar varias veces.
    #---------------------------------------
    

    #4.4 Una variable continua y una discreta
    
    g5<-ggplot(ocupados, aes(PB2, colour=PB1))     # colour indica que hay grupos (2 en este caso)
    
    g5+geom_freqpoly()
    
    g5+geom_freqpoly()+
      ggtitle("Edad por sexo")+
      xlab("Edad")+
      ylab("Personas")+
      theme(plot.title =                           # theme ajusta el centrado
              element_text(hjust = 0.5))
                                                  

