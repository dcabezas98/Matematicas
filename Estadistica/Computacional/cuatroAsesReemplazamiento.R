cuatroAsesReemplazamiento <- function(maximo=1000, verbose=FALSE){
  extracciones=0
  cartas <- sample(52,maximo,T)
  ases<-seq(1,52,13) # Ases que quedan por sacar
  
  while(length(ases)>0 && extracciones<maximo){
    extracciones=extracciones+1
    carta=cartas[extracciones]
    ases=ases[! ases == carta] # Se elimina la carta si era un As si quedaba por sacar
  }
  
  if (verbose){
    if(length(ases)==0){
      cat("He necesitado",extracciones,"extracciones para obtener los 4 Ases :)\n")
    }
    else{
    cat("No he podido obtener los 4 Ases en", extracciones, "extracciones :(\n")
    }
  }
  if(length(ases)==0){
    return(list(extracciones=extracciones,sucesion=cartas[1:extracciones]))
  }
  else{
    return(NA)
  }
}
