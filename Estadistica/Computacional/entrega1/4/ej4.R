# David Cabezas

# La urna contiene 5 bolas rojas (R), 8 blancas (B) y 3 negras(N)

# En los sucesos a y b entiendo sacar exactamente esas bolas, no más.

# Extracción sin reemplazamiento de 5 bolas
extract5 <- function(){
  return( sample(c(rep('R',5),rep('B',8),rep('N',3)), 5, F) )
}

# Extracción con reemplazamiento de 5 bolas
extract5Reemp <- function(){
  return( sample(c('R','B','N'), 5, T, c(5, 8, 3)) )
}

extract5()
extract5Reemp()

# Programaremos una función que compruebe si ocurre un suceso en una extracción

sucesos<-function(extracciones){
  blancas=sum(extracciones=='B')
  rojas=sum(extracciones=='R')
  negras=sum(extracciones=='N')
  return( list( dosBlancas=(blancas==2), dosBlancas1roja=(blancas==2 && rojas==1),
                dosNegrasAlMenos=(negras>=2) ) )
}

x=extract5()
x
sucesos(x)

x=extract5Reemp()
x
sucesos(x)

# Con reemplazamiento

# La probabilidad de sacar una bola blanca en una extraccion es de 8/(8+5+3)=1/2
# Al ser con reemplazamiento, esta probabilidad no cambia entre extracciones,
# luego nos encontramos ante una binomial con N=5 y p=0.5,
# la probabilidad de obtener el valor 2 es 0.3125=5C2*0.5^2*0.5^3

# La probabilidad de obtener 2 blancas y una roja se obtiene por la fórmula que
# relaciona la probabilidad condicionada con la intersección.
# P(2 blancas y 1 roja) = P(2 blancas)*P(1 roja / 2 blancas)
# Falta calcular la probabilidad de obtener una bola roja condicionada a que salen
# dos blancas.
# Suponiendo que salen dos blancas, en las otras tres bolas solo pueden salir negras y rojas.
# Como hay 5 rojas y 3 negras, y es con reemplazamiento, la probabilidad de sacar
# roja en cada una de las 3 extracciones es 5/(5+3)=5/8=0.625. Por tanto, estamos 
# en una binomial con N=3, p=0.625, y la probabilidad de obtener el valor 1 es
# 3C1*0.625^1*(1-0.625)^2=0.263672. Multiplicando esa probabilidad por la de obtener
# dos bolas blancas, concluimos que la probabilidad de sacar dos blancas y una roja
# es de 0.0823975 (redondeando).

# La probabilidad de sacar bola negra no cambia entre extracciones, así que estamos
# en una binomial N=5, p=3/16=0.1875. La probabilidad de obtener 2 o más es 1 menos
# la de obtener 1 ó 0:
1-5*0.1875*(1-0.1875)^4-(1-0.1875)^5 # 0.237339

# Ahora realizamos la simulación
repeticiones=15000

dosBlancas=0
dosBlancas1roja=0
dosNegrasAlMenos=0
for (i in 1:repeticiones){
  x <- extract5Reemp()
  res <- sucesos(x)
  dosBlancas=dosBlancas+res$dosBlancas
  dosBlancas1roja=dosBlancas1roja+res$dosBlancas1roja
  dosNegrasAlMenos=dosNegrasAlMenos+res$dosNegrasAlMenos
}

# Frecuencias relativas

dosBlancas/repeticiones # Prob teórica = 0.3125, obtengo 0.3166
dosBlancas1roja/repeticiones # Prob teórica = 0.0823975, obtengo 0.08553333
dosNegrasAlMenos/repeticiones # Prob teórica = 0.237339, obtengo 0.2358667

# Como siempre, las probabilidades difieren de las frecuencias relativas,
# pero son bastante similares

# Sin reemplazamiento

# La probabilidad de sacar dos bolas blancas en 5 extracciones sin reemplazamiento en una urna
# con 16 bolas de las cuales 8 son blancas se cualcula usando la función de densidad de una
# hipergeométrica. Los parámetros son N=16, N1=8, n=5. Obtenemos 8C2*8C3/16C5=14/39=0.358974.

# La probabilidad de obtener 2 blancas y una roja se obtiene por la fórmula que
# relaciona la probabilidad condicionada con la intersección.
# P(2 blancas y 1 roja) = P(2 blancas)*P(1 roja / 2 blancas)
# Falta calcular la probabilidad de obtener una bola roja condicionada a que salen
# dos blancas.
# Suponiendo que salen exactamente dos blancas, en las otras tres bolas solo pueden salir negras y rojas.
# Como hay 5 rojas y 3 negras, y es sin reemplazamiento, nos encontramos otra vez ante una
# hipergeométrica con N=8, N1=5, n=3; y la probabilidad de obtener una bola roja es
# 5C1*3C2/8C3=15/56=0.267857.
# Por tanto, multiplicando esa probabilidad por la de obtener dos bolas blancas, concluimos
# que la probabilidad de sacar dos blancas y una roja es de 0.0961537 (redondeando).

# Para las bolas negras, tenemos una hipergeométrica de N=16, N1=3, n=5. La probabilidad de obtener
# dos o más bolas negras es la de obtener 2 más la de obtener 3 (no hay más opciones, puesto que sólo hay
# 3 bolas negras). Por tanto, obtnemos 3C2*13C3/16C5+3C3*13C2/16C5=3/14=0.2142857.

# Ahora realizamos la simulación
repeticiones=15000

dosBlancas=0
dosBlancas1roja=0
dosNegrasAlMenos=0
for (i in 1:repeticiones){
  x <- extract5()
  res <- sucesos(x)
  dosBlancas=dosBlancas+res$dosBlancas
  dosBlancas1roja=dosBlancas1roja+res$dosBlancas1roja
  dosNegrasAlMenos=dosNegrasAlMenos+res$dosNegrasAlMenos
}

extract5()

# Frecuencias relativas

dosBlancas/repeticiones # Prob teórica = , obtengo
dosBlancas1roja/repeticiones # Prob teórica = , obtengo
dosNegrasAlMenos/repeticiones # Prob teórica = , obtengo

# Como siempre, las probabilidades difieren de las frecuencias relativas,
# pero son bastante similares