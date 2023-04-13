# A esta guía le agregué ejercicios de una guía anterior. 
# Estos ejercicios son el 3, 11, 22 y 23.

# Ejercicio 1
# Escriba un programa – función cuya salida sea el elemento n-ésimo de la
# sucesión de Fibonacci. La sucesión de Fibonacci tiene como sus dos primeros
# elementos el 0 y el 1. Los restantes términos se calculan como la suma de los dos
# anteriores.

fibonacci <- function(n){
  if (!is.numeric(n)) return("El argumento debe ser un numero")
  
  # Los primeros 2 elementos de la sucesión son 0 y 1, por lo tanto, lo "hardcodeamos"
  # para facilitarnos el trabajo más tarde.
  if (n == 1) return(0) 
  else if (n == 2) return(1)
  
  # Ahora estamos en la posicion 2 de la serie. El numero actual es el 1 y el anterior el 0.
  actual = 1
  anterior = 0
  
  # Recorremos todos los numeros entre 3 y n. Empezamos en 3, ya que las primeras 2
  # posiciones ya están resueltas anteriormente.
  for (i in 3:n){
    aux = anterior
    anterior = actual
    actual = aux + actual
  }
  
  return(actual)
}

fibonacci(25)

# Otra manera mas corta (pero muchisimo mas lenta y menos eficiente, no usar en numeros grandes).
fibonacci <- function(n){
  if (!is.numeric(n)) return("El argumento debe ser un numero")
  
  if (n == 1) return(0)
  else if (n == 2) return(1)
  
  else return(fibonacci(n-1) + fibonacci(n-2))
}

fibonacci(25)
#-------------------------------------------------------------------------------
# Ejercicio 2
# Escriba un programa (función) que determine si un número entero positivo
# (argumento) es par o impar. Recuerde que puede hacerlo comprobando si el número
# es divisible exactamente por 2. La salida deberá ser 1 si es par y 0 si no es par. Luego,
# utilizando ese programa como esclavo, escriba una nueva función que cuente los
# número pares e impares de una matriz de tamaño arbitrario que contiene elementos
# enteros positivos. La salida de este programa será una matriz cuyo primer elemento
# sea la cantidad de valores pares en A y el segundo elemento la cantidad de valores
# impares en A.

esPar <- function(x){
  if (!is.numeric(x)) return("El argumento debe ser un numero")
  
  # Si al dividirlo por 2 el resto es 0, es par y devolvemos 1. Sino devolvemos 0 porque es impar.
  if (x %% 2 == 0) return(1)
  
  else return(0)
}


paresMatriz = function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz")
  
  # Creamos una matriz de 2 filas y una columna. Sus valores al principio son (0, 0)
  B = matrix(0, 2, 1)
  
  # Recorremos todos los elementos de la matriz A.
  for (i in 1:length(A)){
    if (!is.numeric(A[i])) return("Todos los elementos de la matriz deben ser numeros")
    
    # Si el elemento de la matriz actual es par, sumamos uno en la primera fila de 
    # la matriz B. Sino, sumamos uno en la segunda fila.
    if (esPar(A[i]) == 1){
        B[1,1] = B[1,1] + 1
      } else{
        B[2,1] = B[2,1] + 1
      }
  }
  
  return(B)
}
matriz = matrix(round(runif(15)*100), 3); matriz
paresMatriz(matriz)
#-------------------------------------------------------------------------------
# Ejercicio 3
# Escriba un programa – función que reporte una aproximación del número pi
# sumando “n” términos de la serie de Leibnitz.
# 𝑗
# ∑((-1)^n)/(2n+1) =𝜋/4
#𝑛=0
# Verificar que, al aumentar “n”, disminuye el error. 

pi_leibnitz <- function(n){
  if (!is.numeric(n)) return("El argumento debe ser un numero")
  
  suma = 0
  
  # Hacemos n iteraciones simulando la sumatoria.
  for (i in 0:(n-1)){
    suma = suma + (((-1)^i)/(2*i+1))
  }
  return(suma*4)
}
pi_leibnitz(200000)
#-------------------------------------------------------------------------------
# Ejercicio 4
# Escriba un programa (función) que calcule el número pi mediante simulación de
# ‘lluvia’ al azar, usando n observaciones (argumento de la función).
# El proceso es el siguiente:
# Se generan puntos al azar (x, y) con x, y pertenecientes a [0, 1] y se verifica si
# pertenecen o no al interior de un cuarto de círculo de radio uno (Es decir, se
# comprueba si x^2 + y^2 < 1). La proporción de puntos en el interior del círculo
# converge en probabilidad al área de ese cuarto de círculo a medida que se usan más
# puntos. Nota: El área de un cuarto de círculo de radio 1 es A=pi/4.

pi_lluvia <- function(n){
  if (!is.numeric(n)) return("El argumento debe ser un numero")
  
  cant = 0
  # Realizamos n iteraciones. En cada iteración verificamos si la suma de
  # 2 numeros aleatorios entre 0 y 1 es menor a 1. En caso de que lo sean,
  # sumamos uno al contador. Luego dividimos el contador por n y lo multiplicamos por 4.
  for (i in 1:n){
    if (runif(1)^2 + runif(1)^2 < 1){
      cant = cant + 1
    }
  }
  
  return(cant*4/n)
}

pi_lluvia(50000)
#-------------------------------------------------------------------------------
# Ejercicio 5
# Escriba una función que devuelva el mínimo elemento del vector x (argumento).

minimo_elemento <- function(v){
  if (!is.vector(v)){
    return("El argumento debe ser un vector")
  }
  
  # Inicializamos una variable en NULL, ya que si lo hacemos en 0, este podría ser considerado
  # el minimo valor del vector, por mas que no esté presente en el mismo.
  min = NULL
  
  # Recorremos todos los elementos del vector
  for (i in 1:length(v)){
    if (!is.numeric(v[i])) return("Todos los elementos del vector deben ser numeros")
    
    # Si el minimo es nulo, significa que estamos en el primer elemento del vector.
    # Por esto, le asignamos este valor. Si el minimo no es nulo, pero el elemento
    # actual es mas chico que el minimo hasta ahora, entonces le asignamos este valor.
    if (is.null(min) || v[i] < min){
      min = v[i]
    }
  }
  return(min)
}

v = round(runif(10, 1, 100)); v
minimo_elemento(v)
#-------------------------------------------------------------------------------
# Ejercicio 6
# Escriba una función que, dado un vector, calcule la varianza, pudiendo indicar
# como argumento de la función, si se busca una varianza muestral o poblacional.

calcular_Varianza <- function(v, tipo = "muestral"){
  if (!is.vector(v)) return("El argumento debe ser un vector")
  
  # La diferencia entre la varianza muestral y poblacional es que en la poblacional
  # el divisor es la longitud del vector, mientras que en la muestral es la 
  # longitud - 1. Por eso inicializamos el valor en la longitud, y si el tipo es
  # muestral, entonces le restamos uno.
  divisor = length(v)
  
  if (tipo == "muestral"){
    divisor = divisor - 1
  } else if (tipo != "poblacional"){
    # Si el tipo no es muestral ni poblacional, avisamos que está mal el argumento.
    return("El tipo debe ser 'muestral' o 'poblacional'")
  }
  
  return(sum((v-mean(v))^2)/divisor)
}

v=round(runif(5)*100) ; v
var(v) # Comprobamos el valor de la varianza segun R.
calcular_Varianza(v, "muestral")
#-------------------------------------------------------------------------------
# Ejercicio 7
# Escriba una función que se aplique sobre un escalar x (asumimos entero y
# positivo) y determine si x es primo o no. Un número primo es aquel que sólo es
# divisible por 1 y por sí mismo. Para saberlo deberá dividir el número por todos los
# otros entre 1 y x y ver si surge algún resto cero. Si el número es primo el programa
# retorna un valor 1. Si no lo es retorna un valor 0.

esPrimo <- function(x){
  if (!is.numeric(x)){
    print("El argumento debe ser un numero")
    return(0)
  } 
  
  # El numero 2 es primo, pero como iteramos entre 2 y x-1, si el argumento x es 2,
  # tendríamos una iteracion entre 2 a 1, lo cual no tiene sentido. Por eso decimos
  # que si es 2, termine el programa devolviendo un 1
  if (x == 2) return(1)
  
  # Iteramos todos los valores entre 2 y (x-1), y si x es divisible por alguno de
  # estos valores, entonces el numero no es primo y devolvemos 0.
  for (i in 2:(x-1)){
    if (x %% i == 0) return(0)
  }
  
  return(1)
}

esPrimo(29)
#-------------------------------------------------------------------------------
# Ejercicio 8
# Escriba una función en R que aplique el programa anterior como esclavo para
# determinar si los elementos de una matriz o vector y de enteros positivos son primos.
# El programa deberá retornar un vector o matriz de igual dimensión que y, pero con 1´s
# donde los elementos correspondientes de y son primos y 0´s donde los elementos de y
# son no primos.

sonPrimos <- function(v){
  if (!is.vector(v) && !is.matrix(v)) return("El argumento debe ser un vector o una matriz")
  
  # Iteramos el vector o matriz y reemplazamos el valor de cada elemento por 1 o 0,
  # dependiendo de lo que devuelve la funcion esPrimo().
  for (i in 1:length(v)){
    v[i] = esPrimo(v[i])
  }
  return(v)
}

vector = c(5:18) ; vector
matriz = matrix(vector, 2, byrow = T) ; matriz
sonPrimos(vector)
sonPrimos(matriz)
#-------------------------------------------------------------------------------
# Ejercicio 9
# Escriba un programa tipo función que genere una matriz A de dimensión m x n.
# Dicha matriz deberá tener como elementos números primos entre 0 y 101.

generadorDePrimos <- function(m, n){
  if (!is.numeric(m) || !is.numeric(n)) return("El argumento debe ser un numero")
  
  # Generamos la matriz de dimensión m*n, con todos sus elementos en 0.
  A = matrix(0, m, n)
  
  # Generamos 2 variables auxiliares iniciadas en 1.
  contador = 1
  primos = 1
  
  # Hacemos un bucle mientras la variable 'primos' sea menor al tamaño de la matriz.
  # Esto lo hacemos para poder llenar todos los elementos de la matriz con numeros primos.
  while (primos <= m*n){
    if (esPrimo(contador) == 1){
      A[primos] = contador
      primos = primos + 1

    }
    
    # Como queremos tener numeros primos entre 1 y 101, si llegamos al numero 101, 
    # comenzamos de nuevo con el valor 1
    if (contador > 101){
        contador = 1
    }
    contador = contador + 1
  }
  return(A)
}

generadorDePrimos(4, 8)
#-------------------------------------------------------------------------------
# Ejercicio 10
# Escriba una función en R tal que tome a un vector x como argumento de entrada
# y devuelva un vector y cuyos elementos surgen de ordenar x de menor a mayor
# mediante el siguiente procedimiento ("Método de la Burbuja" o “bubble sort”): Se
# recorre todo el vector x comparando cada elemento con el anterior. Si están en orden
# incorrecto se permutan y se continúa avanzando, comparando y si es necesario,
# permutando. Una vez que se llega al final de x se vuelve a comenzar. El proceso
# termina cuando, ante un recorrido completo en x no se realiza ninguna permutación

bubble_sort <- function(x){
  if (!is.vector(x)) return("El argumento debe ser un vector")
  
  # Iniciamos un valor booleano en TRUE. Lo usaremos para terminar el ciclo while
  permutacion = TRUE
  
  # Seguimos iterando hasta que no se haya hecho ninguna permutación.
  while (permutacion){
    # En un principio, no se hace ninguna permutación en cada iteración.Por eso le ponemos
    # FALSE a la variable.
    permutacion = FALSE
    
    # Obtenemos el primer elemento del vector.
    anterior = x[1]
    
    # Recorremos todos los elementos del vector desde el segundo elemento.
    for (i in 2:length(x)){
      if (!is.numeric(anterior)) return("Todos los elementos del vector deben ser numeros")
      
      # Si el elemento actual es menor al anterior, lo permutamos.
      if (x[i] < anterior){
        x[i-1] = x[i]
        x[i] = anterior
        permutacion = TRUE
      }
      anterior = x[i]
    }
  }
  return(x)
}
bubble_sort(c(9, 8, 2, 3, 5, 1, -2, 5, 4, 33, 6, 7, 3, 2))
#-------------------------------------------------------------------------------
# Ejercicio 11
# Escriba una función similar a la anterior pero que admita un segundo argumento,
# tal que si vale 0 ordena de menor a mayor, mientras que si vale 1 ordena de
# mayor a menor.

bubble_sort <- function(x, orden = 0){
  if (!is.vector(x)) return("El argumento debe ser un vector")
  
  permutacion = TRUE
  while (permutacion){
    permutacion = FALSE
    anterior = x[1]
    for (i in 2:length(x)){
      if (!is.numeric(anterior)) return("Todos los elementos del vector deben ser numeros")
      
      # Si el orden es 0, comprobamos que el numero actual sea menor al anterior.
      # Si es 1, comprobamos que el numero actual sea mayor al anterior.
      if (orden == 0 && x[i] < anterior || orden == 1 && x[i] > anterior){
        x[i-1] = x[i]
        x[i] = anterior
        permutacion = TRUE
      }
      anterior = x[i]
    }
  }
  return(x)
}
bubble_sort(c(9, 8, 2, 3, 5, 1, -2, 5, 4, 33, 6, 7 ,3 ,2), 1)
#-------------------------------------------------------------------------------
# Ejercicio 12
# Escriba un programa con formato de función que realice lo siguiente: Encontrar
# el número más pequeño en una matriz dada y reportarlo, así como su posición en la
# matriz. Si se repite, reportar todas las posiciones en que se encuentra. El input deberá
# ser una matriz arbitraria de m x n y el output, el escalar correspondiente al valor
# mínimo, así como el vector con la posición del número encontrado (todas las
# posiciones, si hubiera más de una).

numMasChico <- function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz")
  
  # Hacemos una variable inicializada en NULL, que representará el valor mas bajo de la matriz
  masChico = NULL
  
  # Creamos la variable posiciones, donde guardaremos las posiciones en las que se
  # encuentra el valor mas bajo de la matriz.
  posiciones = c()
  
  for (i in 1:length(A)){
    elementoActual = A[i] # Elemento actual de la recorrida a la matriz.
    if (!is.numeric(elementoActual)) return("Todos los elementos de la matriz deben ser numeros")
    
    # Si masChico es null, significa que estamos en la primera iteración. Le asignamos valor.
    if (is.null(masChico) || elementoActual <= masChico){
      
      # Si masChico es null o el elementoActual es mas chico que el valor guardado
      # en la variable masChico, entonces le asignamos a masChico el valor de 
      # elementoActual, y como sobreescribimos esta variable, reiniciamos el vector
      # de posiciones, ya que se deben borrar las posiciones del elemento masChico anterior.
      if (is.null(masChico) || elementoActual < masChico){
        masChico = elementoActual
        posiciones = c()
      }
      # Sin importar si el elementoActual es mas chico o igual al elemento masChico anterior, 
      # guardamos su posición en el vector de posiciones.
      posiciones = append(posiciones, i)
    }
  }
  
  cat("El numero mas chico es ", masChico, ", encontrado en las posiciones: ", posiciones, "\n")
  return(c(masChico, posiciones))
}

numMasChico(matrix(c(5, 3, 4, 3, 7, 8, 5, 6, 7, 6, 3, 4)))
#-------------------------------------------------------------------------------
# Ejercicio 13
# En una lista de números enteros consecutivos desde “a” hasta “b” encontrar
# aquellos que son divisibles por “c”. Reportar un vector con los números que cumplan
# la condición. Los argumentos deberán ser un vector y el escalar por el cual se quiere
# dividir.

# Hacemos una función para saber si los numeros de un vector son consecutivos.

sonConsecutivos <- function(v){
  if (!is.vector(v)){
    print("El argumento debe ser un vector")
    return(FALSE)
  } 
  
  # Recorremos todos los elementos del vector.
  for (i in 1:length(v)){
    if (!is.numeric(v[i])){
      print("Todos los elementos del vector deben ser numeros")
      return(FALSE)
    } 
    
    # Si sumamos i al elemento numero 1 y le restamos uno, obtenemos el valor que
    # debe haber en la posicion actual para que el vector sea consecutivo. Entonces,
    # si no es igual, devolvemos FALSE.
    # Ejemplo: Si el vector va de 5 a 10, en la primera iteración tenemos: 
    # 1 + 5 - 1 = 5, que es el valor que debe haber. En la segunda iteración:
    # 2 + 5 - 1 = 6, que es el valor que debe haber para que sea consecutivo.
    if (v[i] != (i + v[1] - 1)){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

divisiblesPor <- function(v, k){
  if (!is.numeric(k)) return("El segundo argumento debe ser un numero")
  if (!sonConsecutivos(v)) return("Los elementos no son consecutivos.")
  
  
  x = c()
  for (i in 1:length(v)){
    # Si el elemento actual del vector dividido por k tiene resto 0, significa que
    # es divisible, y por lo tanto lo agregamos al vector x.
    if (v[i] %% k == 0){
      x = append(x, v[i])
    }
  }
  
  return(x)
}
divisiblesPor(c(10:50), 7)
#-------------------------------------------------------------------------------
# Ejercicio 14
# Dada una matriz dato genere una función que retorne la imagen espejada de la
# original respecto del eje vertical.

espejarMatriz <- function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz.")
  
  # Recorremos cada fila de la matriz.
  for (i in 1:nrow(A)){
    # Obtenemos la fila de la matriz, la cual es un vector que llamamos v.
    v = A[i,]
    
    # Ahora recorremos las columnas de la matriz, espejando los valores
    for (j in 1:(ncol(A))){
      # ncol(A) - (j-1) significa: El numero de columnas - (la posicion actual - 1)
      # Ejemplo: Si la matriz tiene 5 columnas, y estamos en la primera iteración
      # de la fila i, entonces: 5 - (1-1) = 5. En esa posicion, agregamos el valor
      # que esta en el vector en la posición j.
      # En la segunda iteración: 5 - (2-1) = 4.
      # En la quinta iteración: 5 - (5 - 1) = 1. Terminamos poniendo en la posicion 1
      # el valor que estaba antes en la posicion 5 y almacenamos en el vector v.
      A[i, ncol(A) - (j-1)] = v[j]
    }
  }
  return(A)
}
A = matrix(c(1:25), 5); A
espejarMatriz(A)
#-------------------------------------------------------------------------------
# Ejercicio 15
# Escribir un programa que recorra los elementos de un vector v y encuentre los
# que son divisibles por un número dato x.
# Reporte una copia del vector donde los elementos divisibles se reemplacen por "div".
# Ejemplo: v=(1:9); w=[1 2 DIV 4 5 DIV 6 7 DIV]
divisibles <- function(v, x){
  if (!is.numeric(x)) return("El segundo argumento debe ser un numero")
  if (!is.vector(v)) return("El primer argumento debe ser un vector")
  
  w = v
  for (i in 1:length(v)){
    if (!is.numeric(v[i])) return("Todos los elementos del vector deben ser numeros")
    if (v[i] %% x == 0){
      w[i] = 'DIV'
    }
  }
  
  return(w)
}

divisibles(1:9, 2)
#-------------------------------------------------------------------------------
# Ejercicio 16
# Programe una función que detecte si un número entero positivo es o no un
# cuadrado perfecto: cuadrado(x). Utilizando la función anterior como "esclavo"
# programe una función "maestro" que tenga como argumento una matriz cualquiera de
# orden m x n.
cuadrado <- function(a){
  if (!is.numeric(a) || round(a) != a || a < 0){
    print("Debe ser un numero entero.")
    return(FALSE)
  } 
  
  return(sqrt(a) == round(sqrt(a)))
}

matrizCuadradoPerfecto <- function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz")
  
  for (i in 1:length(A)){
    if (!is.numeric(A[i])) return("Todos los elementos de la matriz deben ser numeros")
    A[i] = cuadrado(A[i])
  }
  return(A)
}
matriz = matrix(c(1:15), 3); matriz
matrizCuadradoPerfecto(matriz)
#-------------------------------------------------------------------------------
# Ejercicio 17
# Esta función detecta(A) debe detectar los cuadrados perfectos de la matriz de
# entrada y devolver una matriz similar pero con dichos cuadrados perfectos
# reemplazados por CP’.

detecta <- function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz")
  B = A
  for (i in 1:length(A)){
    if (!is.numeric(A[i])) return("Todos los elementos de la matriz deben ser numeros")
    
    if (cuadrado(A[i])){
      B[i] = 'CP'
    }
  }
  return(B)
}
matriz = matrix(c(1:15), 3); matriz
detecta(matriz)
#-------------------------------------------------------------------------------
# Ejercicio 18
# Dado un vector de números v (dato) eliminar los elementos repetidos generando
# un nuevo vector w. con los elementos de este vector construir un nuevo vector u que
# contenga todos los productos posibles formados por 2 elementos distintos de w.
# Reportar el vector u.
contieneElemento <- function(v, k){
  if (length(v) == 0) return(FALSE)
  for (i in 1:length(v)){
    if (v[i] == k){
      return(TRUE)
    }
  }
  return(FALSE)
}

eliminarRepetidos <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector.")
  
  w = c()
  for (i in 1:length(v)){
    if (!contieneElemento(w, v[i])){
      w = append(w, v[i])
    }
  }
  return(w)
}

productosPosibles <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector.")
  w = eliminarRepetidos(v)
  
  u = c()
  for (i in 1:length(w)){
    if (!is.numeric(w[i])) return("Todos los argumentos del vector deben ser numeros")
    for (j in i:length(w)){
      if (j != i){
        u = append(u, w[i] * w[j])
      }
    }
  }
  
  return(u)
}

productosPosibles(c(1,2,3,3, 4))
#-------------------------------------------------------------------------------
# Ejercicio 19
# Dada una matriz A de n x 2, donde cada fila representa las coordenadas (x , y) de
# un punto en el plano. Determinar cuáles de dichos puntos se encuentran dentro de la
# zona determinada por los ejes cartesianos y la recta y = a - b x , con a y b positivos
# dados por el vector "param". Reportar las filas que representan puntos dentro de la
# zona.

filaDentroDe <- function(v, a, b){
  if (b == 0) return(FALSE)
  x = v[1]
  y = v[2]
  
  if (x < 0 || y < 0) return(FALSE)
  #if (y > a-b*x + 1e-10) return(FALSE)
  if (y > a-b*x) return(FALSE)
  
  return(TRUE)
}

dentroDe <- function(A, param){
  if (!is.matrix(A) || ncol(A) != 2) return("El primer argumento debe ser una matriz de n*2")
  if (!is.vector(param) || length(param) != 2) return("El segundo argumento debe ser un vector con 2 elementos")
  
  for (i in 1:nrow(A)){
    if (filaDentroDe(A[i,], param[1], param[2])){
      print(A[i, ])
    }
  }
}
matriz = matrix(c(-1,2,3,16,5, 6, 1, 3, 0, 12, 2, 0, 2.1, 0, 0, 12.1, 0, 11.9, 1.6, 2, 1, 6), ncol=2, byrow = TRUE); matriz
dentroDe(matriz, c(12, 6))
#-------------------------------------------------------------------------------
# Ejercicio 20
# Escriba un programa que detecte en un vector dato los números que llamaremos
# "contiguos", que resultan del producto de dos enteros seguidos, por ejemplo 6 = 2 x 3
# ó 56 = 7 x 8. El reporte del programa será una matriz de n x 3 donde en la primera
# columna sea la posición del número contiguo en el vector y las otras dos sean los
# factores en que se descompone el contiguo.
# Sugerencia: pruebe con los enteros cercanos a la raíz del número.

contiguos <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector.")
  
  w = c()
  for (i in 1:length(v)){
    raiz = floor(sqrt(v[i]))
    if (raiz * (raiz+1) == v[i]){
      w = append(w, c(i, raiz, raiz+1))
    }
  }
  
  return(matrix(w, length(w)/3, 3, TRUE))
}

contiguos(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16, 18, 20, 56))
#-------------------------------------------------------------------------------
# Ejercicio 21
# Programar una función cuyo desempeño sea generar un vector con elementos
# enteros positivos aleatorios entre 0 y 100, llamado "a", de longitud "n" (argumento).
# Con los elementos de dicho vector "a" generar otro vector "b" cuyo primer elemento
# sea 1 y los elementos siguientes se calculan sumando y restando alternativamente los
# números del vector a. La cantidad de elementos del vector b deberá ser, entonces n+1.

generadorDeVectoresAleatorios <- function(n){
  a = round(runif(n, 0, 100))
  print(a)
  return(a)
}

generarVector <- function(n){
  a = generadorDeVectoresAleatorios(n)
  b = c(1)
  for (i in 1:length(a)){
    # Conociendo si i es par o impar puedo alternar entre sumar y restar.
    if (i %% 2 == 0){
      b = append(b, b[i] + a[i])
    } else{
      b = append(b, b[i] - a[i])
    }
   
  }
  return(b)
}

generarVector(5)


# Agrego 2 ejercicios de la guia 3 antigua.

#-------------------------------------------------------------------------------
# Ejercicio 22
# Generar una función que, para cada elemento de una matriz de n*m, determine
# si cada elemento es primo y/o par, y exprese los resultados en una sola matriz. Si
# el número fuera primo, en la matriz de output se debería ver 1, si fuera entero, se
# debería ver 1, y si fuera ambas, se debería ver el número 2. 

esPrimo <- function(x){
  if (!is.numeric(x)) return(F)
  
  if (x == 2) return(T)
  # Recorro todos los elementos entre 2 y x-1 y divido x por estos numeros. 
  # Si el resto es 0 en algun resultado, el numero no es primo
  for (i in 2:(x-1)){
    if (x %% i == 0) return(F)
  }
  
  return(T)
}

esPar <- function(x){
  if (!is.numeric(x)) return(F)
  
  # Si el resto de dividir el numero por 2 es 0, retorna true. Sino false.
  return(x %% 2 == 0)
}

esPrimoOPar <- function(A){
  if (!is.matrix(A)) return("El argumento debe ser una matriz.")
  
  # Recorro todos los elementos de la matriz y genero un valor auxiliar que comienza en 0.
  # Si el numero es primo, le sumo uno al auxiliar. Si es par, sin importar si 
  # era primo o no, le sumo uno al auxiliar.
  for (i in 1:length(A)){
    aux = 0
    if (esPrimo(A[i])){
      aux = aux + 1
    }
    
    if (esPar(A[i])){
      aux = aux + 1
    }
    
    A[i] = aux
  }
  
  return(A)
}
A = matrix(c(1:18), 3); A
esPrimoOPar(A)

#-------------------------------------------------------------------------------
# Ejercicio 23
# Generar una función que, dado un escalar que indique el número de caras de un
# dado, itere tiradas hasta alcanzar una de las dos siguientes condiciones: el
# número de tiradas “n” (argumento de la función) o el valor “s” (argumento de la
# función) de la suma de las tiradas. Se recomienda usar la función sample.
# Recuerde que dos condiciones pueden incluirse utilizando el símbolo “&”. 

tirarDado <- function(k, n, s){
  cantTiradas = 0
  sumaTiradas = 0
  while(cantTiradas < n && sumaTiradas < s){
    sumaTiradas = sumaTiradas + sample(k, 1)
    cantTiradas = cantTiradas + 1
  }
  
  sprintf("Se sumó %d en %d tiradas", sumaTiradas, cantTiradas)
}

tirarDado(6, 10, 30)