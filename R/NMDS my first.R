### llamar archivo
# setwd("/Users/nancycabanillasteran/Desktop/RETENCIÓN")
### llamar paquetería vegan
library(vegan)
library (ggplot2)
### crear variables: llamar a los factores, o sea ponerles nombre y llamarlos 
factores <-read.csv("data/FACTORES.csv", header = T, sep = ",")

### en este caso los datos son reclutas, se asigna el nombre que se quiera para crear una variable
reclutas <-read.csv("data/DATOS.csv", header = T, sep = ",")
row.names(reclutas)<-factores$Site[1:12]
### llamar a la variable de datos### recordar poner el nombre de la hoja csv entrecomillada

### para hacer el NMDS###
## este es el q se pone por default metaMDS(varespec, try = 999) ##### try 999 es para que hace esas iteraciones, en ete caso nancha NMDS es el objeto dd se guardará la info

nanchaNMDS <- metaMDS(reclutas,try = 999, distance = "bray")
plot(nanchaNMDS)
plot(nanchaNMDS, type = "n")
points(nanchaNMDS, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nanchaNMDS, display = "sites", cex = 0.8, pch=21, col="green", bg="yellow")
text(nanchaNMDS, display = "spec", cex=0.7, col="blue")


ordipointlabel(nanchaNMDS)




