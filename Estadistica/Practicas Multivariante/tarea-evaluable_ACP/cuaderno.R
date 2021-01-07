# -------------------------------------- #
# PASO 0: Carga y estandarización        #
# -------------------------------------- #

library(foreign)
datos<-read.spss("empresas.sav", to.data.frame = TRUE, reencode="utf-8")
round(datos,4) # Pone que hay 13 empresas y hay 14 filas
# La fila 14 tiene valores extraños (20023 en X1) y datos perdidos, así que la eliminamos
summary(datos) # Observamos que summary indica si hay datos perdidos
datos<-datos[-14,]
datos
summary(datos) # Análisis de la escala y variabilidad de cada variable, también vemos que no hay ningún dato perdido

boxplot(datos,main="Análisis exploratorio de datos",
        xlab="Indicadores económicos",
        ylab="Distribución de valores",
        col=c(1:15))

# Compruebo si los datos están estandarizados (media 0 y varianza 1)
round(colMeans(datos),4)
round(apply(datos, 2, sd),4)
# No están estandarizados, lo hacemos nosotros
datos_pca<-scale(datos)
datos_pca
round(colMeans(datos_pca),4)
round(apply(datos_pca, 2, sd),4)

# -------------------------------------- #
# PASO 1: ¿Tiene sentido un ACP?         #
# -------------------------------------- #

round(cor(datos_pca),4) # Matriz de correlación

# Test de Bartlett
install.packages("psych")
library(psych)
cortest.bartlett(cor(datos_pca),n=13)

# ------------------------------------------ #
# Paso 2: Limpieza de outliers               #
# ------------------------------------------ #

datos_originales<-datos_pca # Guardamos los datos para comparar con los valores perdidos

# Función para limpiar los outliers
outlier<-function(data){
  continue<-TRUE
  while(continue){
    H<-1.5*IQR(data)
    data[data<quantile(data,0.25,na.rm = T)-H]<-NA
    data[data>quantile(data,0.75, na.rm = T)+H]<-NA
    continue<-any(is.na(data))
    data[is.na(data)]<-mean(data,na.rm=T)
  }
  data
}

# La aplicamos a cada una de las variables que presenta outliers
datos_pca[,4]<-outlier(datos_pca[,4])
datos_pca[,5]<-outlier(datos_pca[,5])

# Comparamos los datos originales y los arreglados 
par(mfrow=c(1,2))
# Boxplot de los datos originales
boxplot(datos_originales,main="Datos originales",
        xlab="Indicadores económicos",
        ylab="z-values",
        col=c(1:15))
# Boxplot de los datos corregidos.
boxplot(datos_pca,main="Datos sin outliers",
        xlab="Indicadores económicos",
        ylab="z-values",
        col=c(1:15))

# ----------- #
# Paso 3: ACP #
# ----------- #

# ACP con la función prcomp
PCA<-prcomp(datos_pca, scale=T, center = T)
# Matriz de cada rotación, con los pesos de cada una de las variables
# en cada una de las componentes principales.
round(PCA$rotation,4)
# Desviación típica de cada componente principal
PCA$sdev
# Proporción de la varianza explicada y acumulada de cada componente
summary(PCA)

install.packages("ggplot2")
library("ggplot2")
# Proporción de varianza explicada
prop_var<- PCA$sdev^2 / sum(PCA$sdev^2)
ggplot(data = data.frame(prop_var, pc = 1:8),
       aes(x = pc, y = prop_var, fill=prop_var )) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,0.6)) + theme_bw() +
  labs(x = "Componente principal", y= " Proporción de varianza explicada")

# Proporción acumulada de varianza explicada
cum_var<-cumsum(prop_var)
ggplot( data = data.frame(cum_var, pc = 1:8),
        aes(x = pc, y = cum_var ,fill=cum_var )) +
  geom_col(width = 0.5) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Proporción de varianza acumulada")

# -------------------------------------------------------------- #
# Paso 4: Selección del número óptimo de componentes principales #
# -------------------------------------------------------------- #

# Variables cuya varianza supera a la media de las varianzas
round(PCA$sdev^2,4)
round(mean(PCA$sdev^2),4)

# Método del codo
ggplot( data = data.frame(cum_var, pc = 1:8),
        aes(x = pc, y = cum_var ,fill=cum_var )) +
  geom_line(size = 1.5) +
  geom_point(size=3, fill="black") +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Proporción de varianza acumulada")

# -------------------------------------------------------------- #
# Paso 5: Representación gráfica de las componentes principales  #
# -------------------------------------------------------------- #

install.packages("factoextra")

library("factoextra")

fviz_pca(PCA, axes=c(1,2),
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()

fviz_pca(PCA,axes=c(1,3),
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()

fviz_pca(PCA,axes=c(2,3),
         alpha.ind ="contrib", col.var = "cos2",col.ind="seagreen",
         gradient.cols = c("#FDF50E", "#FD960E", "#FD1E0E"),
         repel=TRUE,
         legend.title="Distancia")+theme_bw()

t(round(PCA$x[,1:3],3))
