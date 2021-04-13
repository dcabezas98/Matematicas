# David Cabezas

# Las probabilidades de obtener cada cara son inversamente proporcionales al valor:
# Probabilidad de sacar:
# 1: k
# 2: k/2
# 3: k/3
# 4: k/4
# 5: k/5
# 6: k/6

# Imponiendo que las probabilidades deben de sumar 1, obtenemos el valor de k
# y por tanto las probabilidades reales.
# 1 = k (1+1/2+1/3+1/4+1/5+1/6) = k 49/20
# Concluimos que k=20/49. La probabilidad de obtener cada número es:
# 1: 20/49
# 2: 20/98
# 3: 20/147
# 4: 20/196
# 5: 20/245
# 6: 20/294

# Probabilidad de obtener 6 en un lanzamiento: 20/294
# Probabilidad de obtener impar (1, 3 ó 5) en un lanzamiento: 20/49+20/147+20/245=92/147

# Función que simula dos lanzamientos de este dado:
dadoCargado2 <- function(){
  return( sample(6,2,T,c(1,1/2,1/3,1/4,1/5,1/6)) )  # Devuelve un vector con los resultados de los dos lanzamientos
}

dadoCargado2()

# Repetimos el experimento 100000 veces almacenando el número de veces que se da cada resultado en una matriz,
# la fila indica el resultado del primer lanzamiento y la columna el del segundo.

resultados=matrix(data=rep(0,len=6*6), nrow=6, ncol=6) # Comenzamos con 0 en el contador de cada posible resultado

repeticiones=100000

for (i in 1:repeticiones) {
  res=dadoCargado2()
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
      axes=T, ticktype='detailed', col='yellow', theta=30, phi=20, shade=0.2)

# Los sucesos no son equiprobables, la superficie es mucho más alta donde los
# valores son más bajos. La ecuación teórica de la superficie sería 100000*k^2/(xy)=16659.725/(xy)

# Probabilidad de que la suma sea 9 o más
# Los casos favorables son: (4,6), (5,5), (5,6), (6,4), (6,5), (6,6)
# Ahora que el dado está cargado, tenemos que calcular por separado la probabilidad
# de cada uno: (los últimos valores son aproximaciones)
# P(4,6)=k/4*k/6=k^2/24 = 0.00694155
# P(5,5)=k^2/25         = 0.00666389
# P(5,6)=k^2/30         = 0.00555324
# P(6,4)=P(4,6)         = 0.00694155
# P(6,5)=P(5,6)         = 0.00555324
# P(6,6)=k^2/36         = 0.00462770
# Sumando todos, obtenemos la probabilidad de que la suma sea 9 o más:
0.00694155*2+0.00555324*2+0.00666389+0.00462770
# 0.03628117

# Ahora veamos la frecuencia relativa en la simulación

# Resultados favorables en la simulación:
favorables=0
for ( i in 4:6){
  for (j in 4:6){
    if( i+j>9){
      favorables=favorables+resultados[i,j]
    }
  }
}

favorables # 3558
# Frecuencia relativa
favorables/repeticiones # 0.03558;
# La frecuencia relativa difiere ligeramente de la probabilidad real, pero es similar
