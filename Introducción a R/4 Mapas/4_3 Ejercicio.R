
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



#1. Cargar librerias



#2.  Directorio de trabajo (SHAPE)



#3.  Visualizar la base de datos asociada shape (No es el shape)
# OJO: Quizas ya existan las variables que necesitamos, antes de continuar conviene 
#      mirar la base de datos






#4.   Crear la variable a graficar

      #4.1 Buscar la base de datos del numero de casos de covid


      
            #4.1.1 Crear la variable SUMA

            
            #4.1.2 Quitar el total
            

            
            #4.1.3 Seleccionar la variable de estado y SUMA

      
      #4.2 Agregar un 0 para tener el mismo formato


      
      #4.3 Dividir la muestra en terciles

      
      
      #4.4 Crear 3 rangos: Bajo, Medio y Alto





      
      #4.5 De la base covid solo nos quedamos con cve_ent y rango

      
      
      
      #4.6 Unir tabla de rangos con el dbf del shape
      # Indicar la columna Llave en ambos casos


      
      #4.7 Guardamos la base base_mx en donde esta el shp
      #-----------------------------------
      # NOTA:
      # Con este procedimiento se sustituye 
      # la base original.
      #-----------------------------------
      
      



      
      #5. Cargar el SHAPE
      
      

      
      #6. Agrega una capa llamada "mapa"


      
      
      
      #7. Mapa con variable discreta




      