library(readxl)
Wikipedia <- read_excel("Downloads/Wikipedia Views.xlsx") 
View(Wikipedia)

genero<-factor(Wikipedia$GENERO)
genero<-relevel(genero, ref="0") #Base es que sea hombre 

facultad<-factor(Wikipedia$FACULTAD)
facultad<-relevel(facultad, ref="6") #Base es que sea otros 

loginwiki<-factor(Wikipedia$LOGINWIKI)
loginwiki<-relevel(loginwiki, ref="0") #Base es que sea no

phd<-factor(Wikipedia$PhD)
phd<-relevel(phd, ref="0") #Base es que sea no

cargo<-factor(Wikipedia$CARGO)
cargo<-relevel(cargo, ref="6") #Base es que sea adjunto 

modeloinicial<-lm(VISTAS~.+cargo+facultad+loginwiki+phd+genero-CARGO-FACULTAD-GENERO-PhD-LOGINWIKI, data=Wikipedia)
graphics.off()
par("mar")
head(Wikipedia)
par(mar=c(1,1,1,1))
pairs(~VISTAS+EDAD+genero+facultad+loginwiki+phd+cargo+SALARIO+PEU1+PEU2+ENJ1+ENJ2+QU+VIS+IM+SA, data=Wikipedia)

summary(modeloinicial)

library(car)
vif(modeloinicial)

#Corregir el modelo quitando ENJ2 
matriz<-data.frame(Wikipedia[,3:25])
cor(matriz)

modelocorregido<-lm(VISTAS~.+cargo+facultad+loginwiki+phd+genero-CARGO-FACULTAD-GENERO-PhD-LOGINWIKI-ENJ1-ID, data=Wikipedia)
summary(modelocorregido)

vif(modelocorregido)

library(lmtest)

#Autocorrelacion 
res<-matrix(residuals(modelocorregido))
head(res)
res1<-matrix(c(res[2:910,],0))
plot(res,res1)
dwtest(modelocorregido)
#No hay 

#revisar hererocedasticidad del modelo 
plot(modelocorregido)

bptest(modelocorregido)

#Si hay un problema entonces es necesario aislar la o las variables que lo producen 
bptest(VISTAS~EDAD, data=Wikipedia)
bptest(VISTAS~genero, data=Wikipedia)
bptest(VISTAS~facultad, data=Wikipedia)
bptest(VISTAS~phd, data=Wikipedia)
bptest(VISTAS~EXPERIENCIA, data=Wikipedia)
bptest(VISTAS~cargo, data=Wikipedia)
bptest(VISTAS~SALARIO, data=Wikipedia)
bptest(VISTAS~loginwiki, data=Wikipedia)
bptest(VISTAS~PEU1, data=Wikipedia)
bptest(VISTAS~PEU2, data=Wikipedia)
bptest(VISTAS~ENJ2, data=Wikipedia)
bptest(VISTAS~QU, data=Wikipedia)
bptest(VISTAS~VIS, data=Wikipedia)
bptest(VISTAS~IM, data=Wikipedia)
bptest(VISTAS~SA, data=Wikipedia)
bptest(VISTAS~USE1, data=Wikipedia)
bptest(VISTAS~USE2, data=Wikipedia)
bptest(VISTAS~USE3, data=Wikipedia)
bptest(VISTAS~PF, data=Wikipedia)
bptest(VISTAS~EXP1, data=Wikipedia)
bptest(VISTAS~EXP2, data=Wikipedia)
bptest(VISTAS~EXP3, data=Wikipedia)

#YA SE CONOCEN CUALES SON. MIRAR EL COMPORTAMIENTO 
res<-matrix(residuals(modelocorregido))
plot(loginwiki, res^2)
plot(Wikipedia$ENJ2, res^2)
plot(Wikipedia$VIS, res^2)
plot(Wikipedia$IM, res^2)
plot(Wikipedia$SA, res^2)
plot(Wikipedia$USE1, res^2)
plot(Wikipedia$USE2, res^2)
plot(Wikipedia$USE3, res^2)
plot(Wikipedia$EXP1, res^2)
plot(Wikipedia$EXP2, res^2)

Wikicorregido<-Wikipedia/((Wikipedia$IM)^(1/2))
Wikicorregido$GENERO<-genero
Wikicorregido$LOGINWIKI<-loginwiki
Wikicorregido$PhD<-phd
Wikicorregido$FACULTAD<-facultad
Wikicorregido$CARGO<-cargo

View(Wikicorregido)

modelocompleto<-lm(VISTAS~.-ENJ1-IM+cargo+facultad+loginwiki+phd+genero-CARGO-FACULTAD-GENERO-PhD-LOGINWIKI-ID, data=Wikicorregido)
bptest(modelocompleto)
summary(modelocompleto)

plot(modelocompleto)
dwtest(modelocompleto)




modReducido <- lm(VISTAS~.-ENJ1-IM+cargo+facultad+loginwiki+phd+genero-CARGO-FACULTAD-GENERO-PhD-LOGINWIKI-ID-VIS-USE2-USE3-PF-EXP3-facultad-loginwiki-phd, data=Wikicorregido)
summary(modReducido)
anova(modelocompleto, modReducido)

modeloSinCargo <- lm (VISTAS~.-ENJ1-IM+cargo+facultad+loginwiki+phd+genero-CARGO-FACULTAD-GENERO-PhD-LOGINWIKI-ID-VIS-USE2-USE3-PF-EXP3-facultad-loginwiki-phd-cargo, data=Wikicorregido)
summary(modeloSinCargo)
anova(modReducido, modeloSinCargo)
#No hay diferencia para las visitas de ningun cargo de ninguna facultad. 

linearHypothesis(modReducido, c("cargo1=0","cargo2=0","cargo3=0","cargo4=0","cargo5=0"),test="F")

