# Ejercicio 1
# Generar un número aleatorio con distribución normal estándar con media 4 y
# varianza 5. Si el valor generado es menor a la media, mostrar en la pantalla la
# palabra “inferior” (string de caracteres) y si es superior, indicar “superior”.

x = rnorm(1,4,sqrt(5)) ; x
if (x < 4){
  print("inferior")
} else if (x > 4){
  print("superior")
} else print("Iguales")
#-------------------------------------------------------------------------------
# Ejercicio 2
# Generar un vector aleatorio v de 10 elementos, con distribución normal estándar,
# con media 4 y varianza 5. Redondee los elementos a 2 decimales. Calcular la media
# de los elementos del vector. Luego, recorrer el vector y si el valor del elemento es
# menor a la media calculada, reemplazar el elemento por un cero. Si es mayor,
# reemplazar el elemento por un uno.

v = round(rnorm(10, 4, sqrt(5)), 2); v
media = mean(v) ; media
for (i in 1:length(v)){
  if (v[i] < media){
    v[i] = 0
  } else if (v[i] > media){
    v[i] = 1
  }
}
v
#-------------------------------------------------------------------------------
# Ejercicio 3
# Ídem anterior pero ahora generar un vector auxiliar w con caracteres “S” si el
# elemento correspondiente de v es superior a la media o “I” si elemento
# correspondiente de v es inferior a la media.
v = round(rnorm(10, 4, sqrt(5)), 2); v
media = mean(v) ; media
w = c()
for (i in 1:length(v)){
  if (v[i] < media){
    w[i] = "I"
  } else if (v[i] > media){
    w = append(w, "S")
  }
}
w
#-------------------------------------------------------------------------------
# Ejercicio 4
# Generar un vector aleatorio con distribución binomial (n: 77, p: 0.368) de 17
# elementos. Verificar si la suma de sus elementos es mayor (2), menor (0) o igual
# (1) a 481.712 (E(S)). Imprimir el resultado (2, 1 o 0).

v = rbinom(17, 77, 0.368) ; v
suma = sum(v)
if (suma > 481.712){
  print(2)
} else if(suma == 481.712){
  print(1)
} else{
  print(0)
}
#-------------------------------------------------------------------------------
# Ejercicio 5
# Generar una matriz A de 3*4 con números aleatorios enteros con distribución
# normal (µ: 37, σ: 9). Con las funciones 'for' e 'if', analizar si los valores de cada
# celda de la matriz A son mayores o iguales a 35 o no y expresar el resultado en una
# nueva matriz (1 si es mayor o igual; 0 si es menor).

nums = round(rnorm(12, 37, 9), 0)
A = matrix(nums, 3, 4) ; A
B = matrix(1:12, 3, 4)
for (i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    if (A[i, j] >= 35){
      B[i, j] = 1
    } else{
      B[i, j] = 0
    }
  }
}
B
#-------------------------------------------------------------------------------
# Ej 6
# Generar una matriz de 20x9 con números aleatorios redondeados generados con
# distribución exponencial de parámetro 𝜆=0.007. Analizar si la suma de los
# componentes de cada columna es mayor a 1800 y expresar el resultado en un nuevo
# vector (de 0s y 1s). Realizar la misma operación, pero analizando si la suma de cada
# fila es mayor a 9000.

nums = round(rexp(20*9, 0.007))
C = matrix(nums, 20, 9) ; C
v = c()

for (i in 1:ncol(C)){
  if (sum(C[, i]) > 1800){
    v[i] = 1
  } else{
    v[i] = 0
  }
}
v
v = c()
for (i in 1:nrow(C)){
  if (sum(C[i,]) > 900){
    v[i] = 1
  } else{
    v[i] = 0
  }
}
v
#-------------------------------------------------------------------------------
# Ejercicio 7
# Generar un valor aleatorio 'k' (k ~ U(76, 245)). Sumar, comenzando en cero,
# números aleatorios obtenidos a través de distribuciones uniformes (0, 1),
# deteniendo finalmente en la iteración que haga que la suma de ellos sea mayor a ‘k’
# Tip: genere un objeto vacío 'contador' y un objeto 'suma' con valor cero. Utilice la
# e indique cuántas iteraciones fueron requeridas.
# función while. 

k = runif(1, 76, 245) ; k
suma = 0
contador = 0
while (suma < k){
  suma = suma + runif(1)
  contador = contador + 1
}
suma ; contador
#-------------------------------------------------------------------------------
#Ejercicio 8
# Repetir el ejercicio anterior, pero condicionando la suma a: si el valor generado para
# cada iteración es mayor o igual a 0.7, entonces se suma tal cual se obtuvo. Si el
# valor está entre 0.7 y 0.5, se suma 0.45 y si el valor es menor a 0.5, no se suma
# nada. 
k = runif(1, 76, 245) ; k
suma = 0
contador = 0
while (suma < k){
  valor = runif(1)
  if (valor >= 0.7){
     suma = suma + valor
  } else if (valor < 0.7 && valor >= 0.5){
    suma = suma + 0.45
  }
 
  contador = contador + 1
}
suma ; contador
