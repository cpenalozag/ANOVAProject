library(psych)
library(ggplot2)
library(magrittr)
library(stringr)
library(lubridate)
library(scales)
library(testthat)
library(readxl)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(onewaytests)
library(lawstat)
library(stats)
library(knitr)

# ANOVA Project: Flames with magnifying glass


datos <- read.csv("Desktop/ANOVA Project/Datos.csv", header=TRUE, sep=",")
print(head(datos))

#  Brown-Forsythe test for distance
# 1. Data transformations to test distance
m1 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='10 cm'])
m1
m2 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='20 cm'])
m2
m3 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='30 cm'])
m3

medians<-c(rep(m1,27),rep(m2,27),rep(m3,27))
print(medians)
datos$z=abs(datos$TIEMPO..Seg.-medians)
print(datos$z)

# 2. The test
bf=aov(z~datos$DISTANCIA,data=datos)
summary(bf)

#  Brown-Forsythe test for angle
# 1. Data transformations to test distance
m1g = median(datos$TIEMPO..Seg.[datos$ANGULO=='30 grados'])
m1g
m2g = median(datos$TIEMPO..Seg.[datos$ANGULO=='60 grados'])
m2g
m3g = median(datos$TIEMPO..Seg.[datos$ANGULO=='90 grados'])
m3g

mediansg<-c(rep(m1g,27),rep(m2g,27),rep(m3g,27))
datos$z=abs(datos$TIEMPO..Seg.-mediansg)

# 2. The test
bf=aov(z~datos$ANGULO,data=datos)
summary(bf)

#  Brown-Forsythe test for material
# 1. Data transformations to test distance
m1m = median(datos$TIEMPO..Seg.[datos$MATERIAL=='Papel bond'])
m1m
m2m = median(datos$TIEMPO..Seg.[datos$MATERIAL=='Carton'])
m2m
m3m = median(datos$TIEMPO..Seg.[datos$MATERIAL=='Papel periodico'])
m3m

mediansm<-c(rep(m1m,27),rep(m2m,27),rep(m3m,27))
datos$z=abs(datos$TIEMPO..Seg.-mediansm)

# 2. The test
bf=aov(z~datos$MATERIAL,data=datos)
summary(bf)


# Descriptive Statistics
groupingVariables <- list(datos$ANGULO,datos$DISTANCIA,datos$MATERIAL)
DSp<-describeBy(datos$TIEMPO..Seg.,group=groupingVariables, mat=TRUE, digits= 2)
print(kable(DSp, 'latex'))
print((DSp)%>%kable())
datos$TIEMPO <- datos$TIEMPO..Seg.
g1 <- ggplot(datos, aes(x = ANGULO, y = TIEMPO,fill=DISTANCIA)) +
  geom_boxplot()+facet_grid(.~MATERIAL)+theme_bw() + theme(text=element_text(family = "Tahoma", size = 18),
       axis.title = element_text())
g1

# Triple Interaction
datos%>%group_by(ANGULO, DISTANCIA,MATERIAL)%>% 
  summarise(y= mean(TIEMPO)) %>% ggplot(aes(x =ANGULO, y = y ,colour = DISTANCIA, 
                                       group = DISTANCIA)) + geom_point() + geom_line()+theme_bw()+facet_grid(.~MATERIAL)+
  theme(
        text=element_text(family = "Tahoma", size = 18),
        axis.title = element_text(face="bold"))

datos%>%group_by(ANGULO, DISTANCIA,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =ANGULO, y = y,colour = MATERIAL, 
                                       group = MATERIAL)) + geom_point() + geom_line()+theme_bw()+facet_grid(.~DISTANCIA)
+theme(text=element_text(family = "Tahoma", size = 18),
    axis.title = element_text(face="bold"))
 

# Double Interactions
# Angle vs Distance
datos%>%group_by(DISTANCIA,ANGULO,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =ANGULO, y = y,colour = DISTANCIA, 
                                       group = DISTANCIA)) + geom_point() + geom_line()+theme_bw()

# Distance vs Material
datos%>%group_by(DISTANCIA,ANGULO,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =DISTANCIA, y = y,colour = MATERIAL, 
                                       group = MATERIAL)) + geom_point() + geom_line()+theme_bw()

# Angle vs Material
datos%>%group_by(DISTANCIA,ANGULO,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =ANGULO, y = y,colour = MATERIAL, 
                                       group = MATERIAL)) + geom_point() + geom_line()+theme_bw()


# ANOVA
# Whith triple interaction
fit<-aov(TIEMPO~MATERIAL*ANGULO*DISTANCIA, data=datos)
summary(fit)

# Whith double interaction
fit2<-aov(TIEMPO~MATERIAL*ANGULO + MATERIAL*DISTANCIA + ANGULO*DISTANCIA, data=datos)
summary(fit2)

# Correct model
fit3<-aov(TIEMPO~MATERIAL+ANGULO+DISTANCIA+ ANGULO*DISTANCIA, data=datos)
summary(fit3)


#Comparaciones Multiples

library(multcomp)

#Experimento con interacciones
TukeyHSD(fit3)
TukeyHSD(fit3, "MATERIAL")
TukeyHSD(fit3, "DISTANCIA")
TukeyHSD(fit3, "ANGULO")
TukeyHSD(fit3, "ANGULO:DISTANCIA")


