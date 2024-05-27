library(readxl)
GLM_Datos_sargazo <- read_excel("/Users/nancha/Desktop/manuscrito sargazo y erizos/GLM Datos sargazo.xlsx")
View(GLM_Datos_sargazo)
GLM_Datos_sargazo$EFECTO <- GLM_Datos_sargazo$`EFECTO SARGAZO (SIN=O, CON=1)` 
glmsargazo <-glm(d15N~factor(EFECTO)+factor (sitio) + factor (TIPO),data= GLM_Datos_sargazo)
summary(glmsargazo)

erizo<- subset(GLM_Datos_sargazo, TIPO=="ERIZO") 
alga<- subset(GLM_Datos_sargazo, TIPO=="ALGA") 

glmerizo <-glm(d15N~factor(EFECTO)+factor (sitio)+factor(EFECTO):factor (sitio), data= erizo)
summary(glmerizo)

glmerizo <-glm(d15N~factor(EFECTO)*factor (sitio)+factor(EFECTO):factor (sitio), data= erizo)
summary(glmerizo)

glmerizo <-glm(d15N~EFECTO*sitio, data= erizo)
summary(glmerizo)

glmalga <-glm(d15N~factor(EFECTO)*factor (sitio) + EFECTO*sitio, data= alga)
summary(glmalga)

anova1<- lm(d15N~factor(EFECTO), data=alga)


anovaalga<- anova (glmalga)
summary (anovaalga)

anova2<- lm(d15N~factor(EFECTO)*factor(sitio), data=erizo)
summary(anova2)


#### anova de dos vías##### ##d15N NITRÓGENO
anova3<- aov(d15N~EFECTO*sitio, data=erizo)
summary (anova3)
anovaerizo<- anova (glmerizo)
summary (anovaerizo)
summary (anova3)

anova4<- aov(d15N~EFECTO*sitio, data=alga)

anovaalga<- anova (glmalga)
summary (anovaalga)
summary (anova4)

#### anova de dos vías##### ##d13C CARBONO
anova5<- aov(d13C~EFECTO*sitio, data=erizo)
summary (anova5)
anovaerizo<- anova (glmerizo)
summary (anovaerizo)
summary (anova5)

anova6<- aov(d13C~EFECTO*sitio, data=alga)

anovaalga<- anova (glmalga)
summary (anovaalga)
summary (anova6)



library (ggplot2)

ggplot(alga,aes(x=sitio,y=d15N))+geom_boxplot()+facet_grid(EFECTO~.)


###PARTE II####

library(readxl)
GLMoxigeno <- read_excel("/Users/nancha/Desktop/manuscrito sargazo y erizos/GLMoxigeno.xlsx")

library (ggplot2)
#Gráfica para oxígeno
ggplot(GLMoxigeno, aes(x=DISTANCIA, y=OD))+geom_point()+geom_smooth(method=lm)+facet_wrap(~Localidad)

### Gráfica para pH
ggplot(GLMoxigeno, aes(x=DISTANCIA, y=pH))+geom_point()+geom_smooth(method=lm)+facet_wrap(~Localidad)


cor(GLMoxigeno$DISTANCIA,GLMoxigeno$OD)

Mahahual<- subset(GLMoxigeno, Localidad=="Mahahual") 
cor(Mahahual$DISTANCIA,Mahahual$OD)
