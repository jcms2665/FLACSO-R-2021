
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



#1. Cargar librerias



#2.  Directorio de trabajo



#3.  Importar datos



#5. Filtrar la base: solo interesa los ocupados (CLASE2==1)



#6. Etiquetar los datos de la variable sexo (PB1) en la misma variable. 
#   Los codigos son: (1=Hombre, 2=Mujer)



#7. Tabula la variable PB1 para comprobar el etiquetado



#7.1 Tabula la variable horas trabajadas (PE10_1) para ver su comportamiento



#8. Dado que la variable es factor y necesitamos que sea continua, hay que cambiar
#   de formato la variable con la funcion as.numeric



#9. Con la función class() verificamos que la variable PE10_1 es numerica



#10. Cargamosr la base de datos e indicar las dos variables que vamos 
#    a graficar. Para guardar esto, crea un objeto llamado g (Ver código 4.4)  



#10. Al objeto "g" agrega la capa geom_freqpoly()



#11. Agrega un titulo, asi como nombre a los ejes




