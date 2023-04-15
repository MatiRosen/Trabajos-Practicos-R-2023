# La guía es hasta el ejercicio 15. Sin embargo, se agregaron los ejercicios de una guia anterior al final.

# Ejercicio 1)
# Dado un vector de números llamado "base" de enteros positivos, buscar en otro vector
# v cualquiera de números enteros positivos la aparición de los elementos de "base".
# Cada vez que aparezca un elemento en v que ya exista en "base", reemplazar dicho
# elemento en v por el número 0. Reportar el vector w.
# base=[2145] v=[3456792385] w=[3006790380]

# Vemos si el vector es entero y positivo.
esEnteroYPositivo <- function(v){
  if (!is.vector(v)) return(FALSE)
  
  for (i in 1:length(v)){
    # Para saber si el numero es entero, nos fijamos que sea igual al redondeo del mismo.
    if (round(v[i]) != v[i] || v[i] <= 0){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Buscamos si el vector v contiene el elemento k
contieneElemento <- function(v, k){
  if (!is.vector(v)){
    print("El primer parametro debe ser un vector")
    return(FALSE)
  }
  
  # Recorremos el vector v
  for (i in 1:length(v)){
    # Si lo encontramos devuelve true.
    if (v[i] == k){
      return(TRUE)
    }
  }
  
  # Si llegamos hasta aca no encontramos el valor k en el vector v. Devuelve False.
  return(FALSE)
}

buscarElementos <- function(v, base){
  if (!is.vector(v) || !is.vector(base)) return("Los argumentos deben ser vectores")
  if (!esEnteroYPositivo(v) || !esEnteroYPositivo(base)) return("Los vectores deben ser enteros positivos.")
  
  w = v
  # Recorremos el vector, y en caso que el elemento este en la base, lo reemplazamos por 0
  for (i in 1:length(v)){
    if (contieneElemento(base, v[i])){
      w[i] = 0
    }
  }
  
  return(w)
}

base = c(2,1,4,5)
v = c(3, 4, 5, 6, 7, 9, 2, 3, 8, 5)
buscarElementos(v, base)
#-------------------------------------------------------------------------------
# Ejercicio 2)
# Programe una función que reconozca si un número "x" es par o impar. Llamele par(x).
# Con dicho programa como esclavo programe una función maestro llamado
# "seleccion" que recorra un vector "v" #(dato) de enteros positivos y verifique si los
# elementos impares son números impares y si los elementos pares son números pares.
# El programa debe dejar inalterados los elementos que cumplan las condiciones y debe
# reemplazar por el valor 0 los elementos que no las cumplan.
# Ejemplo: v=[7, 12, 4, 6, 8, 5, 9, 20]
# seleccion(v) -> [7, 12, 0, 6, 0, 0, 9, 20]

par <- function(x){
  if (!is.numeric(x)) return(FALSE)
  
  # Si al dividir x por 2 nos da resto 0, es par.
  return(x %% 2 == 0)
}

seleccion <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector")
  
  for (i in 1:length(v)){
    # Si (i es par pero el elemento es impar) o (i es impar y el elemento es par)
    # entonces lo reemplazamos por 0.
    if ((par(i) && !par(v[i])) || (!par(i) && par(v[i]))){
      v[i] = 0
    }
  }
  
  return(v)
}

v= c(7, 12, 4, 6, 8, 5, 9, 20)
seleccion(v)
#-------------------------------------------------------------------------------
# Ejercicio 3)
# Escriba una función "mayor_que" que tenga como argumentos dos variables reales x
# y a. si el número x es mayor que a el programa debe retornar un 1 (verdadero), sino
# debe retornar un 0 (falso). Escriba un programa detecta que tenga como argumentos
# un vector v de dimensión libre y un escalar h. Utilizando el programa esclavo, se
# deberá analizar si cada elemento de v es mayor que el número h. Como salida debe
# generar un vector w de la misma longitud que v, que tenga 1s en las posiciones de los
# elementos mayores que h, y 0 en las menores o iguales.

mayor_que <- function(x, a){
  if (!is.numeric(x) || !is.numeric(a)) return("Los argumentos deben ser numeros")
  
  if (x > a){
    return(1)
  } else{
    return(0)
  }
}

detecta <- function(v, h){
  if (!is.vector(v)) return("El primer argumento debe ser un vector")
  if (!is.numeric(h)) return("El segundo argumento debe ser un numero")
  
  w = v
  # Recorro el vector.
  for (i in 1:length(v)){
    # Como mayor_que devuelve 1s y 0s, no hace falta usar ifs para asignar los valores...
    w[i] = mayor_que(v[i], h)
  }
  
  return(w)
}

detecta(c(1,3,2,4,5,6,8,7,6,5), 5)
#-------------------------------------------------------------------------------
# Ejercicio 4)
# Escriba una función "elementos" que tenga como argumentos una matriz cuadrada A
# y un escalar positivo b. El programa debe crear una nueva matriz c que tenga en su
# diagonal principal los valores de a multiplicados por b y fuera de su diagonal principal
# los valores de a divididos por b. La matriz c será la salida del programa.

elementos <- function(A, b){
  # Compruebo que el numero de filas sea igual al numero de columnas para que sea matriz cuadrada.
  if (!is.matrix(A) || ncol(A) != nrow(A)) return("El primer argumento debe ser una matriz cuadrada.")
  if (!is.numeric(b) || b <= 0) return("El segundo argumento debe ser un numero positivo.")
  
  C = A
  # Primero recorro filas y luego columnas.
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (!is.numeric(A[i, j])) return("Todos los elementos de la matriz deben ser numeros.")
      
      # Si 'i' y 'j' son iguales, entonces es un elemento de la diagonal principal.
      if (i == j){
        C[i, j] = A[i, j] * b
      } else{
        C[i, j] = A[i, j] / b
      }
    }
  }
  
  return(C)
}
A = matrix(c(1:9), 3); A
elementos(A, 2)
#-------------------------------------------------------------------------------
# Ejercicio 5)
# Generar un programa esclavo divisible(x,v) que deberá determinar si un número "x"es
# divisible por alguno de los elementos de un vector dado "v". La salida será TRUE o 1
# si es divisible y FALSE o 0 si no lo es. Con ese esclavo generar una función
# Divisible(A) que deberá determinar si los elementos de una matriz cuadrada son
# divisibles por alguno de los elementos de su diagonal principal. En caso de que lo
# sean se deberán reemplazar por "d", si no lo son se reemplazarán por "nd".

divisible <- function(x, v){
  if (!is.numeric(x)) return("El primer argumento debe ser un numero")
  if (!is.vector(v)) return("El segundo argumento debe ser un vector")
  
  for (i in 1:length(v)){
    # Si la division entre x y el elemento tiene resto 0, devuelve true.
    if (x %% v[i] == 0) return(TRUE)
  }
  
  return(FALSE)
}

Divisible <- function(A){
  if (!is.matrix(A) || ncol(A) != nrow(A)) return("El primer argumento debe ser una matriz cuadrada.")
  
  # Primero creamos el vector diagonal y lo rellenamos con los valores de la diagonal de la matriz.
  # Se podría usar la funcion diag(A) para esto, pero lo hacemos "manualmente"
  diagonal = c()
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      if (i == j){
        diagonal = append(diagonal, A[i, j])
      }
    }
  }
  
  # Ahora comprobamos para cada elemento si es divisible o no por un elemento de la diagonal,
  # y lo reemplazamos por 'd' o 'nd' respectivamente.
  B = A
  for (i in 1:length(A)){
    if (divisible(A[i], diagonal)){
      B[i] = "d"
    } else{
      B[i] = "nd"
    }
  }
  
  return(B)
}
A = matrix(seq(3, 27, 1), 5, byrow = T); A
Divisible(A)
#-------------------------------------------------------------------------------
# Ejercicio 6)
# PARTE 1:
#  La secuencia de Collatz de un número entero se construye de la siguiente forma:
#  *Si el número es par, se lo divide por dos;
#  *Si es impar, se le multiplica tres y se le suma uno;
#  *La sucesión termina al llegar a uno.
#  Dado un número entero positivo n, cree una función cuyo resultado sea su secuencia
#  de Collatz

esPar <- function(n){
  if (!is.numeric(n)) return(FALSE)
  
  # Si el numero es divisible por 2, devuelve true, sino false.
  return(n %% 2 == 0)
}

collatz <- function(n){
  if (!is.numeric(n) || round(n) != n || n <= 0) 
    return("El argumento debe ser un numero entero positivo")
  
  v = c(n)
  # Mientras n no sea 1, seguimos iterando.
  while(n != 1){
    if (esPar(n)){
      n = n/2
    } else{
      n = n*3+1
    } 
    
    v = append(v, n)
  }
  
  return(v)
}

collatz(11)
# PARTE 2:
#  Dado un vector, cree una matriz cuya primera columna sea el vector, y en las filas, a
#  continuación de cada elemento, la secuencia de Collatz correspondiente. En caso que
#  no coincida la longitud, completar con 0 una vez terminada la misma

matriz_collatz <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector.")
  
  # Primero obtenemos cual es la mayor secuencia de collatz de los numeros del vector.
  mayorSecuencia = 0
  for (i in 1:length(v)){
    if (!is.numeric(v[i])) return("Todos los elementos del vector deben ser numeros.")
    
    actual = length(collatz(v[i]))
    if (actual > mayorSecuencia){
      mayorSecuencia = actual
    }
  }
  
  # Ahora podemos armar una matriz, donde la cantidad de filas son la longitud de v,
  # y la cantidad de columnas es la longitud de la mayor secuencia de collatz.
  A = matrix(0, length(v), mayorSecuencia)
  
  # Aca recorremos el vector v, y obtenemos para cada elemento su secuencia de collatz.
  # Despues nos fijamos cuantos 0s debo poner para rellenar la fila de la matriz correspondiente.
  for (i in 1:length(v)){
    u = collatz(v[i])
    cantCeros = mayorSecuencia - length(u)
    if (cantCeros > 0){
      u = append(u, replicate(cantCeros, 0))
    }
    A[i,] = u
  }
  
  return(A)
}

matriz_collatz(c(6, 10, 11))
#-------------------------------------------------------------------------------
# Ejercicio 7)
# Diseña un programa que, dados cinco puntos en el plano, determine cuál de los cuatro
# últimos puntos es más cercano al primero. Un punto se representará con dos variables:
#   una para la abcisa y otra para la ordenada. La distancia entre dos puntos (x1, y1) y
# (x2, y2) es RAIZ[(x1 − x2)^2 + (y1 − y2)^2].

distancia <- function(v, w){
  if (!is.vector(v) || !is.vector(w)) return("Los argumentos deben ser vectores")
  
  # Obtengo los puntos a partir de los vectores.
  x1 = v[1]
  y1 = v[2]
  
  x2 = w[1]
  y2 = w[2]
  
  if (!is.numeric(x1) || !is.numeric(x2) || !is.numeric(y1) || !is.numeric(y2))
    return("Los elementos de los vectores deben ser numeros.")
  
  # Calculo la distancia...
  return(sqrt((x1 - x2)^2 + (y1-y2)^2))
}

mas_cercano <- function(a, b, c, d, e){
  if (!is.vector(a) || !is.vector(b) || !is.vector(c) || !is.vector(d) || !is.vector(e))
    return("Los argumentos deben ser vectores")
  
  # Obtengo la abcisa y ordenada del punto a.
  abcisa = a[1]
  ordenada = a[2]
  
  # Establezco, inicialmente, que el punto mas cercano es el b.
  vectorMasCercano = b
  menorDistancia = distancia(a, vectorMasCercano)
  
  # Creo una matriz con el resto de puntos.
  A = matrix(c(c, d, e), 3, 2, byrow = TRUE)
  
  # Recorro todos los puntos para obtener el mas cercano utilizando la funcion distancia.
  for (i in 1:nrow(A)){
    distanciaAux = distancia(a, A[i, ])
    if (distanciaAux < menorDistancia){
      menorDistancia = distanciaAux
      vectorMasCercano = A[i, ]
    }
  }
  
  cat("El vector mas cercano es ", vectorMasCercano, "\n")
  return(vectorMasCercano)
}

mas_cercano(c(1,2), c(1,5), c(10,1), c(9,5), c(3, 6))
#-------------------------------------------------------------------------------
# Ejercicio 8)
# Sabiendo que en R existe un vector "letters" cuyos elementos son las letras del
# abecedario, indique una función que, dado un vector de números enteros positivos
# hasta el 26, "decodifique" el mensaje oculto, retornando un vector cuyos elementos
# sean las letras correspondientes. Indicar cantidad de vocales y consonantes.

es_vocal <- function(x){
  vocales = c('a', 'e', 'i', 'o', 'u')
  
  # Si el vector vocales contiene el elemento x devuelve true. Sino false.
  for (i in 1:length(vocales)){
    if (x == vocales[i]) return(TRUE)
  }
  
  return(FALSE)
}

mensaje_oculto <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector")
  if (!esEnteroYPositivo(v) ) return("el vector debe ser entero y positivo.")
  
  w = c()
  cantVocales = 0
  
  # Recorremos el vector y obtenemos que letra representa cada numero, usando el vector
  # letters. Ademas comprobamos que el numero sea menor o igual a 26
  for (i in 1:length(v)){
    if (v[i] > 26) return("El numero debe ser menor a 26.")
    w[i] = letters[v[i]]
    if (es_vocal(w[i])) cantVocales = cantVocales + 1
  }
  
  cat("Cantidad de vocales: ", cantVocales, ". Cant de consonantes: ", length(w) - cantVocales, "\n")
  return(w)
}

mensaje_oculto(c(13, 1, 20, 9, 18, 15, 19, 5, 14))
#-------------------------------------------------------------------------------
# Ejercicio 9)
# Una de las técnicas de criptografía más rudimentarias consiste en sustituir cada uno de
# los caracteres por otro situado "n" posiciones más a la derecha. Si n = 2, por ejemplo,
# sustituiremos la "a" por la "c", la "c" por la "e", y así sucesivamente. El problema que
# aparece en las últimas n letras del alfabeto tiene fácil solución: en el ejemplo, la letra
# "y" se sustituirá por la "a" y la letra "z" por la "b". La sustitución debe aplicarse a las
# letras y a los dígitos (el 0 se sustituye con el 2, el 9 con el 1,..).
# Diseña un programa que lea un vector de letra / número y el valor de
# "n" y muestre su versión criptográfiada.

#-------------------------------------------------------------------------------
# Ejercicio 10)
# Define una función que devuelva el número de días que tiene un año determinado.
# Ten en cuenta que un año es bisiesto si es divisible por 4 y no divisible por 100,
# excepto si es también divisible por 400, en cuyo caso es bisiesto.
# Ejemplos: El número de días de 2002 es 365: el número 2002 no es divisible por 4, así
# que no es bisiesto. El año 2004 es bisiesto y tiene 366 días: el número 2004 es
# divisible por 4, pero no por 100, así que es bisiesto. El año 1900 es divisible por 4,
# pero no es bisiesto porque es divisible por 100 y no por 400. El año 2000 si es
# bisiesto: el número 2000 es divisible por 4 y, aunque es divisible por 100, también lo
# es por 400).

#-------------------------------------------------------------------------------
# Ejercicio 11)

# Primera parte:
# Se pide escribir una función "bin_to_int(b)" que transforme un número binario "b" (de
# base 2) en un número entero.
# Recuerde que así como un número con base decimal tiene unidades, decenas,
# centenas, etc. que son las potencias de 10, el número con base 2 utiliza la potencias de
# 2. Así el número "1 1 0 1" indica (de derecha a izquierda)
# 1 * 2^0 + 0 * 2^1 + 1 * 2^2 +1 * 2^3 = 1*0+0*2+1*4+1*8 = 1 + 0 + 4 + 8 = 13
# Para simplificar el número binario vendrá indicado como un vector con un elemento
# por número.
# Ejemplo:
# b=c(1,1,0,1) bin_to_int(b)=13
# b=c(1,0,1,0,0,1,1) bin_to_int(b)=83
# b=c(1,1,0,1,1,0,1) bin_to_int(b)=109



# Segunda Parte (sacada de la guía anterior)
# Idem anterior pero ahora la función se llamará "int_to_bin(n)" y deberá transformar
# un número entero positivo en un número binario. Se adjunta el link para un video
# explicativo de la metodología, que usted deberá programar en su función
# https://www.youtube.com/watch?v=-4rUKlNeCEs
# Ejemplos: Los mismos de la parte I pero invertidos
# b=c(1,1,0,1) bin_to_int(b)=13
# b=c(1,0,1,0,0,1,1) bin_to_int(b)=83
# b=c(1,1,0,1,1,0,1) bin_to_int(b)=109


#-------------------------------------------------------------------------------
# Ejercicio 12)
# Dado un vector de números enteros positivos v (dato) eliminar los elementos
# repetidos generando un nuevo vector w.
# Note que cada vez que se elimina un elemento la longitud de vector cambia, por lo
# tanto se sugiere reemplazar en un primer paso todos los repetidos por 0 y luego
# eliminar los 0s.
# v = [3 2 2 14 3] w = [3 2 14] 

#-------------------------------------------------------------------------------
# Ejercicio 13)
# Escriba un programa "espejo" que genere a partir de una matriz cualquiera a otra
# matriz cuyos elementos sean una imagen a espejo de la original. La imagen a espejo
# será respecto de un eje vertical si el argumento eje = 1 y respecto de un eje horizontal
# si el argumento eje = 2. Si encuentra un comando para hacerlo, no lo use. 

#-------------------------------------------------------------------------------
# Ejercicio 14)
# Escribir un programa "índices" que extraiga de una matriz todos los elementos que
# tengan la suma de sus dos índices (de fila y columna) múltiplos de a. El reporte
# deberá ser un vector indicando fila y columna para cada valor extraído.


#-------------------------------------------------------------------------------
# Ejercicio 15)
# Escribir un programa "intervalo" que tenga como argumento una matriz A de enteros
# positivos de dimensión cualquiera. El programa deberá encontrar los elementos de la
# matriz que son mayores e iguales que un número a y menores o iguales que un
# número b, ambos argumentos del mismo. El reporte deberá ser un vector indicando
# para cada valor encontrado, fila y columna. Si hay valores repetidos incluir todos.


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Ejercicios de la guia anterior que no estaban en esta:
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Ejercicio 16)
# Un tablero de ajedrez es una grilla de ocho filas y ocho columnas, numeradas de 1 a 8.
# Dos de las piezas del juego de ajedrez son el alfil y la torre. El alfil se desplaza en
# diagonal, mientras que la torre se desplaza horizontal o verticalmente. Una pieza
# puede ser capturada por otra si está en una casilla a la cual la otra puede desplazarse.
# Escriba una función que reciba como argumento las posiciones en el tablero de un
# alfil y de una torre, e indique cuál pieza captura a la otra.

#-------------------------------------------------------------------------------
# Ejercicio 17)
# Diseña una función que, dada una lista de números enteros, devuelva el número de
# "series" que hay en ella. Llamamos "serie" a todo tramo de la lista con valores
# idénticos.
# Por ejemplo, el vector [1, 1, 8, 8, 8, 8, 0, 0, 0, 2, 10, 10] tiene 5 "series" (tener en
# cuenta que el 2 forma parte de una "serie" de un solo elemento)

#-------------------------------------------------------------------------------
# Ejercicio 18)

# Primera Parte
# Se desea verificar si el generador de números aleatorios de R funciona bien. Para
# probarlo se debe escribir una función prob(n,k). La función debe generar con el
# comando runif() un vector de n números enteros 1 y 10 (redondee para arriba). Con
# ese vector se verificará que la frecuencia de aparición de números x <= k sea la que
# indica la distribución uniforme.
# Ejemplo: prob(100000,5) aprox. 0.5

# Segunda Parte
# Idem anterior pera ahora se desea verificar el comando rchisq. Para probarlo se debe
# escribir una función prob2(n,gl,k).
# El vector v será el que surja de la aplicación directa del comando para "gl" grados de
# libertad. Con ese vector se verificará que la frecuencia de aparición de números x <=
# k sea la que indica la distribución chi cuadrado con gl grados de libertad.
# Ejemplo: prob2(100000,5,7) aprox 0.7793597

#-------------------------------------------------------------------------------
# Ejercicio 19)
# En el programa R, para eliminar un elemento se pone un signo menos en la posición
# del elemento a eliminar. Por ejemplo v = [3 2 2 14] w = v[-c(3)] = [3 2 14]
# Dado un vector de números enteros positivos v (dato) eliminar los elementos
# repetidos generando un nuevo vector w.
# Note que cada vez que se elimina un elemento la longitud de vector cambia, por lo
# tanto se sugiere reemplazar en un primer paso todos los repetidos por 0 y luego
# eliminar los 0s.
# v = [3 2 2 14 3] w = [3 2 14]

#-------------------------------------------------------------------------------
# Ejercicio 22)
# Escriba un programa "cercano" cuyo argumento sea una matriz de n filas y 2
# columnas que representan coordenadas x e y en el plano cartesiano. El otro argumento
# del programa será un par ordenado v representando las coordenadas de un punto en el
# plano.
# El programa debe detectar cuál de los puntos de la matriz está más cercano al punto v.
# Para calcular las distancias recuerde la fórmula: puntos=(x1, y1) y (x2, y2)
# distancia=RAIZ CUADRADA[(x1 − x2)^2 + (y1 − y2)^2].
# Para encontrar la mínima distancia de las calculadas pruebe los comandos min() y
# which.min()
# La salida debe ser un vector que contenga el j indicando la fila en la matriz A del
# punto más cercano, los valores x e y del punto y la distancia de dicho punto al punto
# dato v.

#-------------------------------------------------------------------------------
# Ejercicio 24)
# Escribir un programa "diagonales" que extraiga de una matriz A cuadrada cualquiera
# los vectores formados por sus diagonales secundarias y por la contradiagonal. Si
# encuentra un comando para hacerlo, no lo use.