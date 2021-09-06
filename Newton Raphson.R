# METODO DE NEWTON - RAPHSON ----

# DEF: El metodo de newton raphson nos permitira hallar la raices de una funcion
# siempre que en el intervalo [a,b] donde se encuentra la raiz tal funcion sea
# diferenciable.

# DESARROLLO DE ALGORITMO
# ELEMENTOS:
# f: funcion diferenciable
# df: derivada de la funcion f
# p0: aproximacion inicial
# tol: error maximo
# N: numero maximo de iteraciones

newton.raphson <- function (f,df,p0,tol,N) {
  
  # Paso 1: Defino variables auxiliares
  i <- 1
  p <- p0-(f(p0)/df(p0))
  
  # Paso 2: Defino la cantidad de iteraciones
  while (i <= N) {
    
    # Paso 3: Genero "p"
    p <- p0-(f(p0)/df(p0))
    
    # Paso 4: Evaluo p
    if(abs(p-p0) < tol) {
      return(p)
      break
    }
    
    # Paso 5: Genero el contador de iteraciones (Suma iteraciones)
    i <- i+1
    
    # Paso 6: Asigno nuevo p0 en caso de no alcanzar la tol
    p0 <- p
  }
  
  #Paso 7: Arrojar resultado en caso de no alcanzar la tol
  return(paste("El metodo fracaso despues de",N,"iteraciones"))
}
