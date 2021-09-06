# METODO SECANTE ----

# DEF: El metodo secante nos permitira nos permitira encontrar las raices de
# una funcion f(p) dadas dos aproximaciones de la raiz.

# DESARROLLO DE ALGORITMO
# ELEMENTOS
# f: funcion a evaluar
# p0: punto de aproximacion inferior
# p1: punto de aproximacion superior
# tol: error maximo
# N: numero maximo de iteraciones

secante <- function (f,p0,p1,tol,N) {
  
  # Paso 1: Defino variables auxiliares
  i <- 2
  p <- p1 - ( (f(p1)*(p1-p0)) / (f(p1)-f(p0)) )
  
  # Paso 2: Defino cantidad de iteraciones
  while(i <= N) {
    
    # Paso 3: Genero p
    p <- p1 - ( (f(p1)*(p1-p0)) / (f(p1)-f(p0)) )
    
    # Paso 4: Evaluo p
    if(abs(p-p1) < tol) {
      return(p)
      break
    }
    
    # Paso 5: Genero contador de iteraciones (Suma de iteraciones)
    i <- i + 1
    
    # Paso 6: Asigno nuevo p0 y p1 en caso de no alcanzar la tol
    p0 <- p1
    p1 <- p
  }
  
  #Paso 7: Arrojar resultado en caso de no alcanzar la tol
  return(paste("El metodo fracaso despues de",N,"iteraciones"))
}