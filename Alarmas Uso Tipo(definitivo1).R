library(readxl)
library(ggplot2)
library(lattice)
library(readr)
library(readxl)
library(scales)

#lee el archivo generado de la consulta llamado TodoTipobi el cual muestra tipo de dia, hora, estacion, liniteinferior, limitesuperior con el limite inferior del 1%
P1 <- read.csv("c:/FernandoM/Proyectos en R/Cubo Usos/BD_out/LimitesP1%.csv")

#Opción de utilizar el percentil 5% como limite inferior de los datos

P5 <- read.csv("c:/FernandoM/Proyectos en R/Cubo Usos/BD_out/LimitesP5%.csv")

#Eliminacion de la columna sobrante

#merge1=merge1[,-c()]

P1 <- select(P1,1,2,3,4,6,7,9,10,11)

P5 <- select(P5,1,2,3,4,6,7,9,10,11)
attach(P1)
attach(P5)

# Sin Criterio de seleccion para obtener atipicos

P1 <- mutate(P1,Atipicos=ifelse(LI>P1$Total,print("1"),print("0")))

P5 <- mutate(P5,Atipicos=ifelse(LI>P5$Total,print("1"),print("0")))

# Criterio de selección de registros atipicos

P1c <- mutate(P1,Atipicos=ifelse(LI>1 & LI>P1$Total,print("1"),print("0")))

P5c <- mutate(P5,Atipicos=ifelse(LI>1 & LI>P5$Total,print("1"),print("0")))
  

P1$Atipicos = as.numeric(P1$Atipicos)
P5$Atipicos = as.numeric(P5$Atipicos)

P1c$Atipicos = as.numeric(P1c$Atipicos)
P5c$Atipicos = as.numeric(P5c$Atipicos)
class(P1$Atipicos)

write.csv(P1,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje1%.csv")
write.csv(P5,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje5%.csv")

write.csv(P1c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Criterio1 1%.csv")
write.csv(P5c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Criterio1 5%.csv")

# Calculo de totales por conductor

Valor <- c("1")

P1 <- mutate(P1,Valor)
P5 <- mutate(P5,Valor)
P1 = as.data.frame(P1)
P5 = as.data.frame(P5)
class(P5)
P1c <- mutate(P1c,Valor)
P5c <- mutate(P5c,Valor)
P1c = as.data.frame(P1c)
P5c = as.data.frame(P5c)

P1$Valor <- as.numeric(P1$Valor)
P5$Valor <- as.numeric(P5$Valor)

P1c$Valor <- as.numeric(P1c$Valor)
P5c$Valor <- as.numeric(P5c$Valor)

write.csv(P1,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje1%.csv")
write.csv(P5,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje5%.csv")

write.csv(P1c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Criterio1 1%.csv")
write.csv(P5c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Criterio1 5%.csv")

# calculo atipicos sin el criterio

Total_Viajes1=aggregate(x=P1$Valor,by=list(P1$Conductor),
                       FUN="sum")[,c(1,2)]

Total_Viajes5=aggregate(x=P5$Valor,by=list(P5$Conductor),
                       FUN="sum")[,c(1,2)]

colnames(Total_Viajes1) <- c("Conductor","Total Viajes")
colnames(Total_Viajes5) <- c("Conductor","Total Viajes")

Conduc_Atipicos1=aggregate(x=P1$Atipicos,by=list(P1$Conductor),
               FUN="sum")[,c(1,2)]

Conduc_Atipicos5=aggregate(x=P5$Atipicos,by=list(P5$Conductor),
                           FUN="sum")[,c(1,2)]

colnames(Conduc_Atipicos1) <- c("Conductor","Total Bajos")
colnames(Conduc_Atipicos5) <- c("Conductor","Total Bajos")

# Calculo de atipicos con el criterio

Total_Viajes1c=aggregate(x=P1c$Valor,by=list(P1c$Conductor),
                        FUN="sum")[,c(1,2)]

Total_Viajes5c=aggregate(x=P5c$Valor,by=list(P5c$Conductor),
                        FUN="sum")[,c(1,2)]

colnames(Total_Viajes1c) <- c("Conductor","Total Viajes")
colnames(Total_Viajes5c) <- c("Conductor","Total Viajes")

Conduc_Atipicos1c=aggregate(x=P1c$Atipicos,by=list(P1c$Conductor),
                           FUN="sum")[,c(1,2)]

Conduc_Atipicos5c=aggregate(x=P5c$Atipicos,by=list(P5c$Conductor),
                           FUN="sum")[,c(1,2)]

colnames(Conduc_Atipicos1c) <- c("Conductor","Total Bajos")
colnames(Conduc_Atipicos5c) <- c("Conductor","Total Bajos")

# Unión de tablas para obtener los porcentajes de Atipicos Bajos

BajosP1 <- merge(Conduc_Atipicos1,Total_Viajes1)

BajosP5 <- merge(Conduc_Atipicos5,Total_Viajes5)

BajosP1c <- merge(Conduc_Atipicos1c,Total_Viajes1c)

BajosP5c <- merge(Conduc_Atipicos5c,Total_Viajes5c)

# Porcentajes de Bajos para percentil 1% y percentil 5%
attach(BajosP1)
attach(BajosP5)
attach(BajosP1c)
attach(BajosP5c)

BajosP1 <- mutate(BajosP1,Porcentaje = percent(1-(`Total Bajos`/`Total Viajes`)))
BajosP5 <- mutate(BajosP5,Porcentaje = percent(1-(`Total Bajos`/`Total Viajes`)))

BajosP1c <- mutate(BajosP1c,Porcentaje = percent(1-(`Total Bajos`/`Total Viajes`)))
BajosP5c <- mutate(BajosP5c,Porcentaje = percent(1-(`Total Bajos`/`Total Viajes`)))

write.csv(BajosP1,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Bajos 1%.csv")
write.csv(BajosP5,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Bajos 5%.csv")

write.csv(BajosP1c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Bajos Criterio 1%.csv")
write.csv(BajosP5c,"c:\\FernandoM\\Proyectos en R\\Cubo Usos\\BD_out\\Porcentaje Bajos Criterio 5%.csv")

#############################  GRAFICAS LINEALES  ###################################################
#_______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

attach(BajosP1)
attach(BajosP5)
attach(BajosP1c)
attach(BajosP5c)
attach(G1)
attach(f1)

# Graficas de tendencias

G1 <- select(P1,1,2,3,4,10,11)

f1 <- subset(P1,Nombre=="A02" & Orientación=="0")
class(f1)

ggplot(data = f1,aes(Hora,Total)) + geom_col()
ggplot(data = f1,aes(Hora)) + geom_density(kernel = "gaussian")

# Graficos de atipicidad

ggplot(G1, aes(x = factor(Orientación), y = Total)) + geom_boxplot() 

# Graficas de Conductores con registros de bajos

BajosP1c$Conductor = as.character(BajosP1c$Conductor)
BajosP5c$Conductor = as.character(BajosP5c$Conductor)

BajosP5c = filter(BajosP5c,`Total Bajos` > 0)
BajosP5c = filter(BajosP5c,Porcentaje > 60 & Porcentaje < 85)

ggplot(data = BajosP5c,aes(Conductor,Porcentaje)) + geom_col()
