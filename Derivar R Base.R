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
