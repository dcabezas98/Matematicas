# David Cabezas

# El juego acabará con seguridad en los próximos 4 lanzamientos, puede que antes.
# Podemos escribir posibles resultados de los próximos 4 lanzamientos como una palabra con
# los símbolos A y B, según quien gane. Por ejemplo: AAAB (gana A), ABBA (gana A), ABBB (gana B).
# El número de palabras de longitud 4 que pueden formarse con 2 símbolos es 2^4=16.

# En los 16 casos que acabamos de describir, el jugador B gana en los casos con 3 o más Bs.
# Como todos los casos son equiprobables, basta contar el número de casos en los que gana B.
# B gana en BBBB, y en todos los que tienen 3Bs y una A, que son 4 (hay 4 posiciones posibles para la A).
# Por tanto la probabilidad de que gane B es 5/16, y la de A es 9/16. De modo que A se llevará 9/16 de
# la apuesta y B se llevará 5/16.

# Los puntos en la interrupción y el total de lanzamientos acertados para ganar son arbitrarios
# Notemos que el juego está obligado a terminar en un número finito de rondas
simular <- function(A=0, B=0, lim=6){ 
  puntos=c(A,B)
  while(puntos[1]<lim && puntos[2]<lim){ # Hasta que uno gane
    ganador = sample(2,1,T)
    puntos[ganador]=puntos[ganador]+1 # 1 punto para el ganador de la ronda
  }
  if (puntos[1]==lim){ # Gana A, jugador 1
    return(1)
  }
  else{ # Gana B, jugador 2
    return(2)
  }
}

simular(4,1)

repetir <- function(n=10000, lim=6){ # Simula n veces
  ganadas=c(0,0)
  for (i in 1:n){
    ganador=simular(A=4,B=3, lim=lim)
    ganadas[ganador]=ganadas[ganador]+1 # Una partida ganada para el jugador 1 (A) o 2 (B)
  }
  return(ganadas)
}

partidas=98765
ganadas=repetir(partidas)
ganadas
ganadas[1]/partidas # Proporción de partidas ganadas por A, obtengo 0.6887663
ganadas[2]/partidas # Proporción de partidas ganadas por B, obtengo 0.3112337

# 5/16 = 0.3125, la proporción de partidas que gana B es similar a la probabilidad que calculamos

