# David Cabezas

# Función que simula una realización del experimento, suponiendo que los seis números del dado tienen
# la misma probabilidad de salir.
dado2 <- function(){
  return( sample(6,2,T) )  # Devuelve un vector con los resultados de los dos lanzamientos
}

dado2()

# Repetimos el experimento 100000 veces almacenando el número de veces que se da cada resultado en una matriz,
# la fila indica el resultado del primer lanzamiento y la columna el del segundo.

resultados=matrix(data=rep(0,len=6*6), nrow=6, ncol=6) # Comenzamos con 0 en el contador de cada posible resultado

repeticiones=100000

for (i in 1:repeticiones) {
  res=dado2()
  resultados[res[1],res[2]]=resultados[res[1],res[2]]+1
}

resultados # Frecuencia de cada posible resultado
sum(resultados) # Deben sumar 100000

# Frecuencias de cada posible resultado
for ( i in 1:6){
  for (j in 1:6){
    cat(resultados[i,j]) # Frecuencia del resultado (i,j), valor i en el primer lanzamiento y valor j en el segundo
    cat(' ')
    }
  cat('\n')
}

# Superficie con las frecuencias
persp(x=1:6,y=1:6,resultados, xlab='Dado 1', ylab='Dado 2', zlab='', r=4,
      axes=T, ticktype='detailed', col='yellow', theta=30, phi=30, shade=0.2)

# Suponiendo que el dado no está cargado, los sucesos elementalos son equiprobables.
# Para calcular la probabilidad de que la suma de ambas puntuaciones sea mayor
# que 9, basta dividir el número de casos favorables entre el número de posibles.
# Para que los resultados sumen 10 o más (mayor que 9) hay 6 sucesos favorables:
# (4,6), (5,5), (5,6), (6,4), (6,5), (6,6)
# Por tanto, la probabilidad es 6/36=1/6.

# Resultados favorables en la simulación:
favorables=0
for ( i in 4:6){
  for (j in 4:6){
    if( i+j>9){
      favorables=favorables+resultados[i,j]
    }
  }
}

favorables # 16727
# Frecuencia relativa
favorables/repeticiones # 0.16727; 1/6 = 0.1666667
# La frecuencia relativa difiere ligeramente de la probabilidad real, pero
# es muy similar
