# METODO DE PUNTO FIJO ----

# DEF: El metodo de punto fijo nos permitira hallar raices en una ecuacion
# f(p) = 0, cuando la misma se pueda expresar de la forma g(p) = p

# DESARROLLO DE ALGORITMO
# ELEMENTOS:
# g: funcion de prueba g(p)
# p0: aproximacion inicial de p (Numero de prueba)
# tol: error maximo
# N: numero maximo de iteraciones

puntofijo <- function (g,p0,tol,N) {
  
  # Paso 1: Defino variables auxiliares
  i <- 1 # Variable contadora de iteracion
  p <- g(p0) # Variable de prueba para evaluacion de igualdad
  
  # Paso 2: Defino la cantidad de iteraciones
  while (i <= N) {
    
    # Paso 3: Evaluo la igualdad g(p) = p
    p <- g(p0)
    
    # Paso 4: Verifico igualdad
    if (is.nan(abs(p-p0))) { # Genero verificacion "is.nan" para evitar errores de comparacion por numeros > Inf
      break
    }
    
    if (abs(p-p0) < tol) {
      return(p)
      break
    }
    
    # Paso 5: Genero el contador de iteraciones (Suma iteraciones)
    i <- i+1
    
    # Paso 6: Asigno nuevo p0 en caso de no alcanzar tol
    p0 <- p
  }
  
  # Paso 7: Arrojar resultado en caso de no alcanzar la tol
  return(paste("El metodo fracaso despues de",N,"iteraciones"))
}
