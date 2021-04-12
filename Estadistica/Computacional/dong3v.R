
dong3.v=function(numero = 100)
{
  x = vector(mode = "numeric", length = numero)
  y = vector(mode = "numeric", length = numero)
  x[1] = 1
  y[1] = 1
  
  XX=c(0.0, 0.85, 0.2, -0.15)
  XY=c(0.0, 0.04, -0.26, 0.28)
  XK=c(0.0, 0.0, 0.0, 0.0)
  YY=c(0.25, 0.85, 0.26, 0.26)
  YX=c(0.0, -0.04, 0.22, 0.24)
  YK=c(0.0, 1.6, 0.0, 1.0)
  
  for(i in 2:numero){
    a = sample(4,1,F,c(1,85,7,7))
    x[i]=XX[a]*x[i-1]+XY[a]*y[i-1]+XK[a]
    y[i]=YX[a]*x[i-1]+YY[a]*y[i-1]+YK[a]
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}

kk=dong3.v(2000)
plot(kk)
