# David Cabezas

# La probabilidad de obtener cara es tres veces la de obtener cruz, imponiendo
# que sumen 1 obtenemos P(H)=0.75 y P(T)=0.25; donde H=cara y T=cruz.

# La variable toma los valores 1 si sale (H,T) y 2 si sale (T,H); según el que salga antes.
# Puesto que los lanzamientos son independientes, la probabilidad de (H,T) es 0.75*0.25=0.1875,
# mientras que la de (T,H) es 0.25*0.75=0.1875. Los dos sucesos, (H,T) y (T,H) son igual de probables,
# luego ambos tienen igual probabilidad de ocurrir primero. Por tanto, X toma los valores 1 y 2 con
# igual probabilidad.

# Aunque la variable aleatoria esté descrita de una forma complicada, su
# función de densidad es muy simple. Simulamos la función de densidad de la forma
# descrita en el enunciado para comprobar nuestros cálculos.

X <- function(){ # Simulamos la variable aleatoria
  x=0
  while(x==0){
    monedas=sample(c('H','T'),2,T,c(0.75,0.25))
    if (all(monedas==c('H','T'))){
      x = 1 
    }
    else if (all(monedas==c('T','H'))){
      x = 2
    }
  }
  return(x)
}

X()

# 15000 repeticiones
repeticiones = 15000

veces=c(0,0) # Veces que sale 1 y 2 respectivamente

for (i in 1:repeticiones){
  x=X()
  veces[x]=veces[x]+1
}

# Frecuencias relativas
veces[1]/repeticiones # Obtengo en mi simulación 0.5011333
veces[2]/repeticiones # Obtengo en mi simulación 0.4988667
# Las frecuencias relativas son bastante cercanas a las probabilidades teóricas que calculamos
