# METODO DE BISECCION ----

# DEF: El metodo de biseccion nos permitira hallar raices de una ecuacion a partir
# de una funcion que se encuentra definida en el intervalo [a,b] donde f(a) y f(b)
# poseen signos distintos, pues por el teorema de valor intermedio esto implica que
# existe al menos una raiz en dicho intervalo. 
# Este metodo consiste en disminuir el intervalo [a,b] haciendo pruebas en intervalos
# distintos, tal intervalo lo encontraremos dividiendo entre 2 (dos) el intervalo
# previamente dado y verificando el signo que arroja la funcion evaluado en (a+b/2)

# DESARROLLO DE ALGORITMO
# ELEMENTOS:
# f: funcion a evaluar
# a: extremo inferior de la funcion (intervalo)
# b: extremo superior de la funcion (intervalo)
# tol: error maximo
# N: numero maximo de iteraciones

biseccion <- function (f,a,b,tol,N) {
  
  # Paso 1: Defino variables auxiliares
  i <- 1 # Variable contadora de iteracion
  p <- a+((b-a)/2) # Variable de evaluacion f(p) donde p = (a+b)/2
  fp <- f(p) # Evaluo f(p)
  
  # Paso 2: Defino la cantidad de iteraciones
  while (i <= N) {
    
    # Paso 3: Genero y evaluo f(p)
    p <- a+((b-a)/2)
    fp <- f(p)
    
    # Paso 4: Evaluo finalizacion del bucle
    if (abs(fp) < tol) {
      return(p) # Arraja el resultado si alcanzo la tol
      break # Frena el Bucle
    }
    
    # Paso 5: Genero el contador de iteraciones (Suma iteraciones)
    i <- i+1
    
    # Paso 6: Asigno nuevo a o b en caso de que fp > tol
    if (f(a)*f(p) > 0) {
      a <- p
    } else {
      b <- p
    }
  }
  
  # Paso 7: Arrojar resultado en caso de no alcanzar la tol
  return(paste("El metodo fracaso despues de",N,"iteraciones"))
}
