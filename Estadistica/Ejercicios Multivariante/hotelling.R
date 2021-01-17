mu0=matrix(c(70,170), nrow=2, ncol=1) # Valor de mu para la hipótesis nula
X=matrix(c(71.45, 164.7), nrow = 2, ncol = 1) # Vector de medias muestral
Sn=matrix(c(14.576,128.88,128.88,1441.2653),nrow=2,ncol=2) # Matriz de cuasi-covarianzas muestral
p=2
N=20
n=N-1
r12=0.889 # Coeficiente de correlación muestral

# Estadístico de contraste para Sigma desconocida:
t=20*t(X-mu0)%*%solve(Sn)%*%(X-mu0)
t

# Valores de comparación teóricos bajo H0 para distintos niveles de significación
f01=qf(0.1, 2, 18, lower.tail = FALSE)*38/18 # alpha=0.1
f005=qf(0.05, 2, 18, lower.tail = FALSE)*38/18 # alpha=0.05
f001=qf(0.01, 2, 18, lower.tail = FALSE)*38/18 # alpha=0.01

f01
f005
f001

library("ellipse")

# Regiones de confianza (elipses) para los distintos niveles de significación
e1<-ellipse(x=Sn/N, centre=as.vector(mu0), t=sqrt(f01))
plot(e1, type='l', xlim=c(67,73), ylim=c(140,200), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.9")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

e2<-ellipse(x=Sn/N, centre=as.vector(mu0), t=sqrt(f005))
plot(e2, type='l', xlim=c(67,73), ylim=c(140,200), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.95")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

e3<-ellipse(x=Sn/N, centre=as.vector(mu0), t=sqrt(f001))
plot(e3, type='l', xlim=c(67,73), ylim=c(140,200), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.99")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

# Representaciones para Sigma conocida
Sigma=matrix(c(20,100,100,1000),nrow = 2, ncol = 2)

# Estadístico de contraste para Sigma conocida
u=20*t(X-mu0)%*%solve(Sigma)%*%(X-mu0)
u

# Valores de comparación teóricos bajo H0 para distintos niveles de significación
c01=qchisq(0.1,2,lower.tail=FALSE)
c005=qchisq(0.05,2,lower.tail=FALSE)
c001=qchisq(0.01,2,lower.tail=FALSE)

c01
c005
c001

# Regiones de confianza (elipses) para los distintos niveles de significación
e1<-ellipse(x=Sigma/N, centre=as.vector(mu0), t=sqrt(c01))
plot(e1, type='l', xlim=c(67,73), ylim=c(145,195), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.9")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

e2<-ellipse(x=Sigma/N, centre=as.vector(mu0), t=sqrt(c005))
plot(e2, type='l', xlim=c(67,73), ylim=c(145,195), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.95")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

e3<-ellipse(x=Sigma/N, centre=as.vector(mu0), t=sqrt(c001))
plot(e3, type='l', xlim=c(67,73), ylim=c(145,195), col="red", xlab="Altura", ylab="Peso", main="Región de confianza para nivel 0.99")
points(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pch=20)
text(x=c(as.vector(mu0)[1], as.vector(X)[1]), y=c(as.vector(mu0)[2], as.vector(X)[2]), pos=2, offset=0.3, labels=c('mu0', 'x'), cex=0.7, font=2)

