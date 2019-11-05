
#Librerias complementaria con las cuales se importaran los paquetes para hacer consultas de los cubos de datos SQl Server 

library(odbc)
library(dplyr)
library(xlsx)
library(rJava)
library(datasets)


#Se genera la conexi�n con la base de datos
  
con <- dbConnect(odbc::odbc(), dsn="Metrocali", UID = rstudioapi::askForPassword("Ingrese su Usuario"), PWD =rstudioapi::askForPassword("Ingrese la Contrasena"))

#Muestra el listado de las tablas que tienen la conexi�n a las cuales se les puede hacer la consulta

dbListTables(con)

#Muestra los atributos o las variables de las tablas

dbListFields(con,"T510_OPERACION" )
dbListFields(con,"T513_BUSES" )
dbListFields(con,"T503_TIPO_DIA")
dbListFields(con,"T511_FECHAS")

#Se especifican las tablas que se van a consultar de todas las que se alojan en el servidor
datos <- dbGetQuery(con,"SELECT F510_FECHA,
                                F510_USOS,
                                F510_INTEGRACIONES,
                                F510_USOS_NO_PAGOS,
                                F510_TASKID,
                                F510_TRIPID,
                                F510_BUSID,
                                F510_CONDUCTOR,
                                F513_OPERADOR,
                                F510_ORIENTACION,
                                F510_FECHA_INICIO_REAL,
                                F510_FECHA_FIN_REAL,
                                datepart(hour,F510_FECHA_INICIO_REAL)*3600+
                                datepart(minute,F510_FECHA_INICIO_REAL)*60+
                                datepart(SECOND,F510_FECHA_INICIO_REAL),
                                datepart(hour,F510_FECHA_FIN_REAL)*3600+
                                datepart(minute,F510_FECHA_FIN_REAL)*60+
                                datepart(SECOND,F510_FECHA_FIN_REAL)
                                FROM T510_OPERACION inner join T513_BUSES on F513_ID=F510_BUSID 
                                inner join T511_FECHAS on F511_FECHA=F510_FECHA
                                WHERE F510_FECHA between '20190801' and '20190818'")

#Crea las otras dos tablas

tabla1 <- data.frame(datos)

#str(tabla1)

#Renombre las columnas de la tabla 1

tb1 <- rename(tabla1,   Fecha=F510_FECHA,
              Usos=F510_USOS,
              Integraciones=F510_INTEGRACIONES,
              Usos_No_Pagos=F510_USOS_NO_PAGOS,
              TareaID=F510_TASKID,
              ViajeID=F510_TRIPID,
              BUSID=F510_BUSID,
              Conductor=F510_CONDUCTOR,
              Operador=F513_OPERADOR,
              Orientaci�n=F510_ORIENTACION,
              Fecha_de_Inicio_Real=F510_FECHA_INICIO_REAL,
              Fecha_de_Fin_Real=F510_FECHA_FIN_REAL)

#se Exporta la tabla en el formato CSV

write.csv(tb1, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\Usos.csv", row.names = FALSE)

#visualiza la consulta generada

View(tb1)

#Desconecta de la base de datos de SQL server 

#dbDisconnect(tb1)

