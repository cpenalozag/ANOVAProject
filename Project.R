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

# ANOVA Project: Flames with magnifying glass


datos <- read.csv("Desktop/ANOVA Project/Datos.csv", header=TRUE, sep=",")
print(head(datos))

#  Brown-Forsythe test
# 1. Data transformations to test distance
m1 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='10 cm'])
m2 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='20 cm'])
m3 = median(datos$TIEMPO..Seg.[datos$DISTANCIA=='30 cm'])

medians<-c(rep(m1,27),rep(m2,27),rep(m3,27))
print(medians)
datos$z=abs(datos$TIEMPO..Seg.-medians)
print(datos$z)

# 2. The test
bf=aov(z~datos$DISTANCIA,data=datos)
summary(bf)


# Descriptive Statistics
groupingVariables <- list(datos$ANGULO,datos$DISTANCIA,datos$MATERIAL)
DSp<-describeBy(datos$TIEMPO..Seg.,group=groupingVariables, mat=TRUE, digits= 2)
print(DSp)
datos$y <- datos$TIEMPO..Seg.
g1 <- ggplot(datos, aes(x = ANGULO, y = y,fill=DISTANCIA)) +
  geom_boxplot()+facet_grid(.~MATERIAL)+theme_bw()
g1

# Triple Interaction
datos%>%group_by(ANGULO, DISTANCIA,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =ANGULO, y = y,colour = DISTANCIA, 
                                       group = DISTANCIA)) + geom_point() + geom_line()+theme_bw()+facet_grid(.~MATERIAL)

datos%>%group_by(ANGULO, DISTANCIA,MATERIAL)%>% 
  summarise(y= mean(y)) %>% ggplot(aes(x =ANGULO, y = y,colour = MATERIAL, 
                                       group = MATERIAL)) + geom_point() + geom_line()+theme_bw()+facet_grid(.~DISTANCIA)

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
fit<-aov(y~MATERIAL*ANGULO*DISTANCIA, data=datos)
summary(fit)

# Whith double interaction
fit2<-aov(y~MATERIAL*ANGULO + MATERIAL*DISTANCIA + ANGULO*DISTANCIA, data=datos)
summary(fit2)


#Comparaciones Multiples

library(multcomp)

#Experimento con interacciones
TukeyHSD(fit)
TukeyHSD(fit, "MATERIAL")
TukeyHSD(fit, "DISTANCIA")
TukeyHSD(fit, "ANGULO")


