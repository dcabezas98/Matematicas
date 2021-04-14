# David Cabezas

# Suponemos los dados no cargados.

# La probabilidad de sacar un 6 en un lanzamiento de dado es de 1/6, utilizamos una binomial
# de parámetros N=4, p=1/6 para calcular la probabilidad requerida. La probabilidad de sacar
# al menos un 6 en 4 lanzamientos será 1 menos la de no sacar ningún 6: 1-(5/6)^4=0.517747.
# Es ventajoso apostar a que se consigue al menos un 6.

seises <- function(n=4){
  n_seises=0
  for (i in 1:n){
    dado=sample(6,1,T)
    if(dado==6){
      n_seises=n_seises+1
    }
  }
  return(n_seises)
}

seises()

# j veces la simulación
repetirSeises <- function(n=4, j=10000){
  veces=rep(0,n+1) # Veces que se obtiene cada resultado
  for (i in 1:j){
    n_seises=seises(n)
    veces[n_seises+1]=veces[n_seises+1]+1 # 0 seises en pos 1, 1 seis en pos 2, etc.
  }
  return(veces)
}

veces=repetirSeises()
veces

# Proporción de casos con al menos 1 seis
1-veces[1]/10000 # Obtengo 0.5145. Difiere ligeramente de la prob. teórica, pero es muy similar


# La probabilidad de sacar un doble 6 en un lanzamiento con par de dados es 1/36. Hacemos lo mismo
# que antes pero ahora con los parámetros N=24, p=1/36. La probabilidad de sacar al menos un doble 6
# es 1-(35/36)^24=0.491404. No es ventajoso apostar a que se consigue al menos un doble 6.

doblesSeises <- function(n=24){
  n_doblesSeises=0
  for (i in 1:n){
    dados=sample(6,2,T) # Tiro dos dados
    if(dados[1]+dados[2]==12){ # Si son dos seises ...
      n_doblesSeises=n_doblesSeises+1
    }
  }
  return(n_doblesSeises)
}

doblesSeises()

# j veces la simulación
repetirDoblesSeises <- function(n=24, j=100000){
  veces=rep(0,n+1) # Veces que se obtiene cada resultado
  for (i in 1:j){
    n_dobles_seises=doblesSeises(n)
    veces[n_dobles_seises+1]=veces[n_dobles_seises+1]+1 # 0 dobles seises en pos 1, 1 doble seis en pos 2, etc.
  }
  return(veces)
}

veces=repetirDoblesSeises()
veces

# Proporción de casos con al menos 1 doble seis
1-veces[1]/100000 # Obtengo 0.49071. Difiere ligeramente de la prob. teórica, pero es muy similar
