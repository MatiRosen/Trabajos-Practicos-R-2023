#-------------------------------------------------------------------------------
# Ejercicio 1) 
# Genere 3 vectores aleatorios de 10 componentes x, y, z. Use la función RUNIF(n)

# runif() sirve para crear numeros aleatorios entre 0 y 1. El '10' le indica que genere 10 numeros.
# A este numero 10 se le llama argumento. Un argumento es un valor que se le pasa
# a una función a través de los paréntesis.
# c() sirve para crear un vector con los números que contiene entre parentesis, separados
# por coma. Como runif genera 10 elementos, va a ser un vector de 10 elementos.

# Uso ; para separar el comportamiento. Es lo mismo que poner la x en el renglon de abajo.
# Al escribir la x, y o z nos muestra en pantalla el valor de estas variables.
x = c(runif(10)); x
y = c(runif(10)); y
z = c(runif(10)); z
#-------------------------------------------------------------------------------
# Ejercicio 2)
# Genere 3 vectores aleatorios x, y, z de 10 componentes, pero con distribución
# U(-100,100) usando RUNIF(n). Vea luego RUNIF(n, mín, máx)


# Uso set.seed(10), que permite generar siempre los mismos numeros aleatorios.
# Esto lo uso para comprobar que el caso 1 y caso 2 sean iguales.

# Caso 1
set.seed(10)
# Genero 10 numeros aleatorios, y a cada uno lo multiplico por 200 y le resto 100.
# Si a un numero entre 0 y 1 lo multiplicamos por 200, obtenemos un numero entre
# 0 y 200. Si le restamos 100, obtenemos un numero entre -100 y 100
x = c(runif(10)*200-100); x
y = c(runif(10)*200-100); y
z = c(runif(10)*200-100); z

# Caso 2
set.seed(10)
# Si a runif le ponemos mas parametros, definimos los limites inferiores y superiores
# entre los que se generaran los numeros random.
x = c(runif(10, -100, 100)); x
y = c(runif(10, -100, 100)); y
z = c(runif(10, -100, 100)); z
#-------------------------------------------------------------------------------
# Ejercicio 3)
# Realice las siguientes operaciones con los vectores obtenidos en el caso anterior:

#   a) u= x+ y+ z
u = x + y + z ; u

#                     10
#   b)  k = <x, y> =  ∑ x𝑖*𝑦𝑖
#                    𝑖=1 
#   (Producto escalar, tendrá que transponer uno de los vectores)

# Con %*% obtenemos el producto escalar.
k = x %*% y ; k

#   c) v =𝑥/ ‖𝑥‖
#   (Vector unitario en la dirección de X, use NORM(vea help) y AS.MATRIX)

# Convierto el vector x en matriz y obtengo su norma.
v = x / norm(as.matrix(x)) ; v

#                 10 
#   d) 𝑤 =|𝑥| / ∑ 𝑥𝑖
#                𝑖=1
#     (Vector de probabilidad a partir de X, use ABS y SUM)

# Con abs(x) obtengo el valor absoluto. con sum(x) sumo todos los elementos del vector
w = abs(x) / (sum(x)) ; w
#-------------------------------------------------------------------------------
# Ejercicio 4)          n
# Calcule 𝑥𝑚 = (1/n)* ∑𝑥𝑖
#                      𝑖=1

# Con length(x) obtengo la longitud del vector x.
xm = sum(x) / length(x); xm
mean(x) # Compruebo con la función mean(x) que el resultado anterior esté bien.
#-------------------------------------------------------------------------------
# Ejercicio 5)
# Calcule el vector dx de desviaciones de 𝑥 en el cual cada componente dx = 𝑥𝑖−𝑥𝑚.
# Puede generar un vector de unos y multiplicarlo por 𝑥𝑚 y luego restarlo de𝑥. 
# Utilice el cálculo del ejercicio anterior para 𝑥𝑚 y intente incluirlo en una sola línea. (Explore
# la función MATRIX para vectores y matrices). Verifique que la suma de los componentes de dx es nula.

# Genero un vector de unos, usando rep(), que repite n veces el primer parametro,
# siendo n el segundo parametro.
vector_unos = c(rep(1, length(x)))

# Se respeta el orden de multiplicacion y suma. Primero el por, despues la resta...
dx = x - vector_unos * xm ; dx
sum(dx)
#-------------------------------------------------------------------------------
# Ejercicio 6)                                                    𝑛
# Calcule la varianza de los elementos de x como Var(x) = (1/n-1)* ∑ (𝑥𝑖−𝑥𝑚)^2
#                                                                 𝑖=1

#Trabaje con vectores y para la sumatoria use SUM. Recuerde que para obtener un
#vector cuyas componentes sean el cuadrado de las componentes originales debe
#plantear u = v ^ 2. Verifique con COV.

varx = sum(dx^2) / (length(x) - 1) ; varx
var(x) # Compruebo que esté bien el resultado anterior.
#-------------------------------------------------------------------------------
# Ejercicio 7)
# Calcule la covarianza de las componentes de los vectores x e y con la fórmula:
#                      𝑛
# Cov(x, y) = (1/n-1)* ∑(𝑥𝑖−𝑥𝑚)(𝑦𝑖−𝑦𝑚)
#                     𝑖=1
# Trabaje con los vectores dx y dy y use SUM. Verifique con COV.

ym = sum(y) / length(y); ym
dy = y - c(rep(1, length(y))) * ym
covxy = sum(dx * dy) / (length(x) - 1) ; covxy
cov(x, y) # Compruebo que esté bien el resultado anterior.
#-------------------------------------------------------------------------------        
# Ejercicio 8)
# Calcula Var(x+y) y verifique que Var(x+y) = Var(x) + Var(y) + 2 Cov (x,y). Utilice
# las soluciones a los ejercicios anteriores para la verificación. Trabaje con vectores
# como lo viene haciendo.

vary = sum(dy^2) / (length(y) - 1)
varxy = varx + vary + 2 * covxy ; varxy
var(x+y) # Compruebo que esté bien el resultado anterior.
#-------------------------------------------------------------------------------
# Ejercicio 9)
# Genere 3 matrices con componentes aleatorios enteros U (-100,100). Utilice redondeo.
# Dimensiones: A: 5x3 B: 4x5 C: 5x5

# Con matrix generamos matrices, donde el primer parametro o argumento es un vector,
# el segundo parametro es el numero de filas y el tercero el numero de columnas.
# Tambien podemos usar byrow = true para ordenar el vector por filas en la matriz.
A = matrix(round(runif(15, -100, 100)), 5, 3) ; A
B = matrix(round(runif(20, -100, 100)), 4, 5) ; B
C = matrix(round(runif(25, -100, 100)), 5, 5) ; C
#-------------------------------------------------------------------------------
# Ejercicio 10)
#  Calcule: a) C*A b) B*C*A c) A^T*C
#   a)

# Con %*% obtenemos el producto escalar.
C %*% A
#   b)

B %*% C %*% A
#   c)

# Con t() obtenemos la traspuesta de una matriz.
t(A) %*% C
#-------------------------------------------------------------------------------
# Ejercicio 11)
# a) Verifique que [C*A]^t = A^t*C^t (Reste [C*A]^t-A^t*C^t y verifique que el resultado
# sea la matriz nula) b) Ídem para [B*C*A]^t = A^t*C^t*B^t
#   a)

t(C %*% A) - t(A) %*% t(C)
#   b)

t(B %*% C %*% A) - t(A) %*% t(C) %*% t(B)
#-------------------------------------------------------------------------------
# Ejercicio 12)
# a) Verifique que 𝐴*𝐴^t es simétrica (Debe ser 𝐴*𝐴^t = [A*A^t]^t
# b) Ídem para 𝐴^t*A
#   a)

A %*% t(A)
A %*% t(A) == t(A %*% t(A))
#   b)

t(A) %*% A
t(A) %*% A == t(t(A) %*% A)
#-------------------------------------------------------------------------------
# Ejercicio 13) 
# Calcule los autovalores y los autovectores de 𝐶,𝐴*𝐴^t 𝑦𝐴^t*A
# (Explore la función EIGEN con help)

# eigen devuelve los autovalores y autovectores.
eigen(C)
eigen(A %*% t(A))
eigen(t(A) %*% A)
#-------------------------------------------------------------------------------
# Ejercicio 14)
# Calcule la matriz inversa de C y verifique que 𝐶^(−1)*𝐶 = 1

# Primero verificamos si tiene determinante.
det(C)

# Con solve() obtenemos la inversa.
solve(C)

# Con round() redondeamos los valores.
round(solve(C) %*% C)
#-------------------------------------------------------------------------------
# Ejercicio 15) 
# Genere una matriz identidad de 5x5, una matriz de 6x6 con todos sus componentes
# iguales a 1, una matriz nula de 4x4, y una matriz diagonal con sus elementos no nulos
# iguales a las componentes de un vector x dado. (Explore MATRIX Y DIAG)


# Para generar una matriz identidad puedo usar diag, o puedo multiplicar 
# cualquier matriz por su inversa...
identidad = diag(5) ; identidad
todo1 = matrix(rep(1, 36), 6) ; todo1
nula = matrix(rep(0, 16), 4) ; nula
x= seq(10, 100, 25) ; x
diag(x, length(x))

