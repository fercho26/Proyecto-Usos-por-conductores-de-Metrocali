
library(odbc)
library(dplyr)
library(xlsx)
library(rJava)
library(datasets)
library(readxl)
library(fdth)
library(stats)
library(readr)
library(ggplot2)
library(data.table)
library(lubridate)

dbListTables(con)


dbListFields(con,"T510_OPERACION" )
dbListFields(con,"T513_BUSES" )
dbListFields(con,"T503_TIPO_DIA")
dbListFields(con,"T511_FECHAS")
dbListFields(con,"T520_RUTAS")



#Muestra los tipos de dï¿½a por cada fecha


datos2 <- dbGetQuery(con,"SELECT F510_FECHA,
                                F510_TASKID,
                                F510_TRIPID,
                                F510_LINEID,
                                F514_NOMBRE,
                                F510_CONDUCTOR,
                                F510_TIPO_VIAJE,
                                F510_USOS,
                                F510_INTEGRACIONES,
                                F510_USOS_NO_PAGOS,
                                F510_TASKID,
                                F510_TRIPID,
                                F510_BUSID,
                                F510_CONDUCTOR,
                                F510_ORIENTACION,
                                F510_FECHA_INICIO_REAL,
                                F510_FECHA_FIN_REAL,
                                datepart(year,F510_FECHA)+
                                datepart(month,F510_FECHA)+
                                datepart(day,F510_FECHA)+
                                datepart(hour,F510_FECHA_INICIO_REAL)*3600+
                                datepart(minute,F510_FECHA_INICIO_REAL)*60+
                                datepart(SECOND,F510_FECHA_INICIO_REAL),
                                datepart(hour,F510_FECHA_FIN_REAL)*3600+
                                datepart(minute,F510_FECHA_FIN_REAL)*60+
                                datepart(SECOND,F510_FECHA_FIN_REAL)
                                FROM T510_OPERACION 
                                inner join T514_RUTAS on F514_ID=f510_LINEID
                             
                                WHERE F510_FECHA between '20190801' and '20190831'")


    #Genera la tabla para hacer los limites

tabla2 <- data.frame(datos2)

lm <- select(tabla2,F510_FECHA,F510_TASKID,F510_TRIPID,F510_LINEID,F510_FECHA_INICIO_REAL,F510_USOS,F510_INTEGRACIONES,F514_NOMBRE,F510_CONDUCTOR,F510_ORIENTACION)

#Renomre de las columnas

Proximobi <- rename(lm,       Fecha=F510_FECHA,
                              Tarea=F510_TASKID,
                              Viaje=F510_TRIPID,
                              LineID=F510_LINEID,
                              Fecha_Inicio=F510_FECHA_INICIO_REAL,
                              Nombre=F514_NOMBRE,
                              Conductor=F510_CONDUCTOR,
                              Orientación=F510_ORIENTACION,
                              Usos=F510_USOS,
                              Integraciones=F510_INTEGRACIONES)

#Se extrae las rutas que no se analizaran

Proximobi = Proximobi[-grep("E21",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("E31",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("E37",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("E52",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("E41",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("T31",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("T47B",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("T57A",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("T50",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("T40",Proximobi$Nombre),]
Proximobi = Proximobi[-grep("U22",Proximobi$Nombre),]

View(Proximobi)

#guarda el Csv de proximobi
write.csv(Proximobi, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\Proximobi.csv", row.names = FALSE)

#Opciï¿½n de ampliar la memoria de almacenamiento de las tablas
  summary(Proximobi)
memory.limit()
memory.limit(size=40000)

#Lee el XLSX que tiene las horas y tipo de dï¿½a

require(readxl)

diatipo <- read_excel("C:/FernandoM/Proyectos en R/Cubo Usos/BD_in/Diatipo.xlsx", 
                   sheet = "Tipo")
colnames(diatipo) <- c("Fecha","Dia tipo")

class(diatipo$Fecha)

  attach(diatipo)

diatipo$Fecha=as.Date(diatipo$Fecha)

Proximobi <- merge(Proximobi,diatipo, by = c("Fecha"))
class(Proximobi$Fecha_Inicio)

  attach(Proximobi)

Proximobi <- mutate(Proximobi, Horas = hour(Fecha_Inicio))
    Proximobi=Proximobi[,-c(5)]

head(Proximobi)

#Filtra las horas requeridas
  
Proximobi$Hora = as.numeric(Proximobi$Hora)

Proximobi = filter(Proximobi,4< Hora & Hora <23)

Proximobi = filter(Proximobi,10000<= Conductor & Conductor <=50000)

Proximobi =as.data.frame(Proximobi)

#Ordena de forma decreciente los Usos 

Proximobi <- Proximobi[with(Proximobi, order(Proximobi$Usos)), ]

Proximobihab <- subset(Proximobi,Proximobi$`Dia tipo`=="HAB")

Proximobihab <- Proximobihab[with(Proximobihab, order(Proximobihab$Usos)), ]

Proximobihab$Sum_Total <- Proximobihab$Usos + Proximobihab$Integraciones

write.csv(Proximobihab, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\Proximobihab.csv", row.names = FALSE)
Proximobihab <- read.csv("c:/FernandoM/Proyectos en R/Cubo Usos/BD_out/Para Analisis/Proximobihab.csv")

#Agrupa los factores a los que se aplicaran los indices de tencencia estadistica
  
ybi1=aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
               FUN="summary","quantile","sd","sum")[,c(1,2,3)]

 ybi2=aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
               FUN="summary")$x[,c(1,2,3,4,5,6)]

ybi3=aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
               FUN="sd")$x

 ybi4=aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
               FUN="sum")$x


#Realizar el calculo del coeficiente de variaciï¿½n???????!!!

#Crea los limites por factores agrupados

ybi2=as.data.frame(ybi2)
RIC=ybi2[,5]-ybi2[,2]

limites=cbind(ybi2[,2]-1.5*RIC,ybi2[,5]+1.5*RIC)
colnames(limites)=c("LimiteInferior","LimiteSuperior")
head(limites)
todotipobihab=data.frame(ybi1,ybi2,ybi3,ybi4,limites)
head(todotipobihab)

#Nombra las columnas de todotipobi

todotipobihab <- select(todotipobihab,2,1,3,4,5,6,7,8,9,10,11,12,13)
colnames(todotipobihab) <- c('Nombre','Hora','Orientación','Min','1er.Quar','Mediana','Media','3er.Quar','Max','Desv.','Sum Total','LimiteInferior','LimiteSuperior')

#para eliminar columnas
#todotipobi=todotipobi[,-c(5,6)]

#ajuste de los decimales de la media

   ls()

 # Ajuste de decimales a la Media
 
 todotipobihab$Media = as.numeric(todotipobihab$Media)
 todotipobihab$Media = round(todotipobihab$Media,digits = 2)
 
 #Ajuste de decimales a la Desviación estandar
 
 todotipobihab$Desv. = as.numeric(todotipobihab$Desv.)
 todotipobihab$Desv. = round(todotipobihab$Desv.,digits = 2)
 
 #Ajuste de decimales al Limite Superior
 
 todotipobihab$LimiteSuperior = as.numeric(todotipobihab$LimiteSuperior)
 todotipobihab$LimiteSuperior = round(todotipobihab$LimiteSuperior,digits = 2)
 
 
 #Ajuste de decimales al Limite Inferior
 
 todotipobihab$LimiteInferior = as.numeric(todotipobihab$LimiteInferior)
 todotipobihab$LimiteInferior = round(todotipobihab$LimiteInferior,digits = 2)

 
#Genera el archivo de factores con los limites generados de los usos

write.csv(todotipobihab, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\Todotipobihab.csv", row.names = FALSE)

attach(todotipobihab)

View(todotipobihab)

#__________________________________________________________________________________________________________________________________________

#showExtends(todotipobi)
#Ordena de forma decreciente los Usos 

todotipobihab <- todotipobihab[with(todotipobihab, order(todotipobihab$`Sum Total`)), ]

#Ajuste de decimales a percentiles

colnames(todotipobihab) <- c('Nombre','Hora','Orientación','Min','1er.Quar','Mediana','Media','3er.Quar','Max','Desv.','Sum Total','LI','LS')

#Cuenta el numero de celdas vacias por columna

sapply(todotipobihab, function(x) sum(is.na(x)))

#todotipobi <- na.omit(todotipobi)

write.csv(todotipobihab, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\Todotipobihab.csv", row.names = FALSE)

# Calculo de los percentiles 1 y 5

todotipobihab$Fecha = as.Date(todotipobihab$Fecha)

Percentil1 = aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
                       function(x) quantile(x,probs = c(0.01)))[,c(1,2,3,4)]
colnames(Percentil1) <- c('Hora','Nombre','Orientación','Percentil1')


Percentil5 = aggregate(x=Proximobihab$Sum_Total,by=list(Proximobihab$Hora,Proximobihab$Nombre,Proximobihab$Orientación),
                       function(x) quantile(x,probs = c(0.05)))[,c(1,2,3,4)]
colnames(Percentil5) <- c('Hora','Nombre','Orientación','Percentil5')

Percentil1 = as.data.frame(Percentil1)
Percentil5 = as.data.frame(Percentil5)
      
# Uniones entre Percentiles

Percentiles <- merge(Percentil1,Percentil5,by = c('Hora','Nombre','Orientación'))
colnames(Percentiles) <- c('Hora','Nombre','Orientación','Percentil1','Percentil5')

# Creaión de la tabla todotipibihab con los percentiles

todotipobihab <- merge(Percentiles,todotipobihab,by = c('Hora','Nombre','Orientación'))
todotipobihab=select(todotipobihab,1,2,3,12,15,4,5)
colnames(todotipobihab) <- c('Hora','Nombre','Orientación','Desv.','Ls','Percentil1','Percentil5')

# Unión de la tabla todotipibihab con el data frame Proximobihab que esta desagregado

todotipobihab <- merge(todotipobihab,Proximobihab,by = c("Nombre","Hora","Orientación"))
todotipobihab=select(todotipobihab,8,14,1,2,3,4,9,10,12,13,17,5,6,7)

###########################################  TODOTIPOBI CON LIMITES PERCENTILES  ########################################################################
#________________________________________________________________________________________________________________________________________________________________________________________________________________________-

#Limite inferior con el percentil 1%

attach(todotipobihab)

P1 <- select(todotipobihab,3,2,4,5,6,7,8,9,10,11,13,12)

colnames(P1) <- c("Nombre","Conductor","Hora","Orientación","Desv.","Tarea","Viaje","Usos","Integraciones","Total","LI","LS")

write.csv(P1, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\LimitesP1%.csv", row.names = FALSE)


#Limite inferior con el percentil 5%

P5 <- select(todotipobihab,3,2,4,5,6,7,8,9,10,11,14,12)

colnames(P5) <- c("Nombre","Conductor","Hora","Orientación","Desv.","Tarea","Viaje","Usos","Integraciones","Total","LI","LS")

write.csv(P5, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\LimitesP5%.csv", row.names = FALSE)

write.csv(todotipobihab, file = "c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Para Analisis\\Todotipohab.csv", row.names = FALSE)

