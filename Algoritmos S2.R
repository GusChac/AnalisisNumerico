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

# DERIVAR CON R BASE ---- 

##           ¿Como encontrar la derivada con funcionces base de R?           ##
## A traves de la funciones 1) D, 2) expression y 3) eval las podemos hallar ##
## Estas funciones la contiene R base por lo cual no sera necesario instalar ##
## una libreria para poder hallar la derivada.                               ##
##                                                                           ##
## A continuacion un ejemplo para derivar cos(x).                            ##
##   ###   ###   ###   ###   ###   ###    ###   ###   ###   ###   ###   ###  ##

# Paso 1: Generar funcion a derivar a traves de la funcion expression.
f <- expression(cos(x))

# Paso 2: Obtener la expression de la derivada con respecto a "x".
df.e <- D(f,"x")

# Paso 3: Evaluar la funcion en un punto x dado.
df.r <- function (x) eval(df)

# NOTESE que df.e nos arrojara la expresion de la derivada y df.r el resultado

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

#### METODO FALSA POSICION ----

# DEF