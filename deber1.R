# INTEGRANTES

# JORGE CHIMARRO
# DAYANARA PEÑAFIEL
# GUIDO OCHOA
# DIEGO CAJAMARCA

library(readr)
datos <- read_csv("C:/Users/JOCHA/Desktop/MAESTRIA/MODULO 8 - HERRAMIENTAS PARA CONTROL DE LA PRODUCCIÓN/deber 1/segmentation_data.csv")
#View(datos)

datos <- datos[,-c(1),drop=FALSE]
View(datos)

#ground = datos$frequency

######################################
######## MATRIZ DE DISTANCIAS ########
######################################

d=dist(datos,method = "euclidean")

######################################
########### CORRELACIONES ############ 
######################################

c=cor(datos)
#install.packages("corrplot")
library(corrplot)
corrplot(c)


######################################
### ESCALAMIENTO MULTIDIMENSIONAL  ###
######################################

fit = cmdscale(d,eig=TRUE, k=2)     #k es el numero de dimensiones

x = fit$points[,1] 
y = fit$points[,2]
plot(x,y,col=c("red","green3", "blue", "black"), main = "grupos Original")
#text(x, y, labels = row.names(iris), cex=1)


## K-Means ##

grupos = kmeans(datos,4)
g1 = grupos$cluster
g2 = grupos$size
plot(x,y, col=c("red","green3","blue", "black")[g1], main = "grupos K-Means")

######################################
######## EXPORTACION DE DATOS ########
######################################

comparacion <-cbind(datos,g1)
View(comparacion)
setwd("C:/Users/JOCHA/Desktop")
#install.packages("xlsx")
library(xlsx)
write.xlsx(comparacion,"datosd1.xlsx")

######################################
############ MODELO DHC  #############
######################################

library("dendextend")
hc = hclust(d, method = "complete" )
clus3 = cutree(hc, 4)
dend = as.dendrogram(hc)
dend = color_branches(dend, 4)
colors = c("red", "green3","blue", "black")
plot(dend, fill = colors[clus3], cex = 0.1 , main = "grupos DHC")

######################################
######### DIAGRAMA DE ELBOW ##########
######################################

wi = c()
for (i in 1:10) 
{
  g = kmeans(datos,i) 
  wi[i] = g$tot.withinss
}
plot((1:length(wi)),wi, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

######################################
########### VALIDACIÓN ###############
######################################

library(cluster)
library(clValid)
du1 = dunn(d,g1)
du2 = dunn(d,clus3)

sil1 = silhouette(g1,d)
plot(sil1,col=1:4, border=NA)
sil2 = silhouette(clus3,d)
plot(sil2,col=5:8, border=NA)


library(aricode)
library(plyr)
ARI1= ARI(g1,g1)
ARI2= ARI(g1,clus3)
AMI1= AMI(g1,g1)
AMI2= AMI(g1,clus3)
NMI1= NMI(g1,g1,variant = c("joint"))
NMI2= NMI(g1,clus3,variant = c("joint"))


######################################
#### Identificación de las Clases ####
######################################
cliente = as.factor(comparacion$g1)         #GROUND

plot(x,y,col=c("red","green3","blue", "black")[cliente], main = "clientes Dataset Original")


######################################
######### Validación Externa #########
######################################
# ARI, AMI, NMI
#install.packages("aricode")

library(aricode)
library(plyr)
ground = comparacion$g1
ARI1_C= ARI(ground,g1)
ARI2_C= ARI(ground,clus3)
AMI1_C= AMI(ground,g1)
AMI2_C= AMI(ground,clus3)
NMI1_C= NMI(ground,g1,variant = c("joint"))
NMI2_C= NMI(ground,clus3,variant = c("joint"))
ENT_C = entropy(ground,clus3)
