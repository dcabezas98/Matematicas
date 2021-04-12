# David Cabezas

# Al lanzar dos monedas al aire (las numeramos como moneda 1 y moneda 2) tenemos 4 resultados posibles:
# HH, HT, TH, TT
# donde H significa cara (Heads) y T cruz (Tails); la primera letra es el
# resultado de la moneda 1 y la segunda de la 2.
# Suponiendo que las monedas están equilibradas, los 4 resultados son equiprobables. Por tanto, la probabilidad
# de cada suceso puede calcularse como casos favorables entre casos posibles.
# a) La probabilidad de dos caras es 1/4=0.25.
# b) La probabilidad de dos cruces es 1/4=0.25.
# c) La probabilidad de una cara y una cruz es 2/4=1/2=0.5.

# Función que simula una realización del experimento.
# Es más cómodo trabajar con datos numéricos pongamos que el 0 es cara y el 1 es cruz
monedas <- function(){
  return( sample(c(0,1),2,T,c(1,1)) )  # Devuelve un vector con los resultados de los dos lanzamientos
}
# Ya que conocemos la probabilidad de cada suceso, podríamos simplemente extraer un resultado.
# sample(c('HH','TT','HT'),1,F,c(1,1,2))

monedas()

# Si la suma del vector es 0, tenemos doble cara; si es 2, tenemos doble cruz; y si es 1, cara y cruz (o cruz y cara).
# Repetimos el experimento 15000 veces y observamos el número de veces que se obtiene cada resultado.

resultados=c(0,0,0) # Para contar el número de veces que se da cada resultado

repeticiones=15000

for (i in 1:repeticiones) {
  i = sum(monedas())+1 # Posición 1 para doble cara, posición 2 para una cara y una cruz y posición 3 para doble cruz
  resultados[i]=resultados[i]+1
}

resultados                      # En mi simulación: 3741 7496 3763
sum(resultados) # Deben sumar 15000

# Proporción de dobles caras:
resultados[1]/repeticiones      # En mi simulación: 0.2494
# Proporción de dobles cruzes:
resultados[3]/repeticiones      # En mi simulación: 0.2508667
# Proporción de veces que obtenemos una cara una cruz:
resultados[2]/repeticiones      # En mi simulación: 0.4997333

# Los resultados obtenidos difieren ligeramente de las probabilidades teóricas, pero se asemejan mucho.