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
  
  # Recorremos el vector y obtenemos que letra representa a cada numero, usando el vector
  # letters. Ademas comprobamos que el numero sea menor o igual a 26, ya que solo hay
  # 26 letras.
  for (i in 1:length(v)){
    if (v[i] <= 0 || v[i] > 26) return("El numero debe ser mayor a 0 y menor a 26.")
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
obtenerPosLetra <- function(x){
  for (i in 1:length(letters)){
    if (letters[i] == x){
      return(i)
    }
  }
  
  return(0)
}

obtenerValorOculto <- function(x, n){
  if (!is.numeric(n)) return("El segundo argumento debe ser un numero")
  
  # Si x es un numero, entonces le sumamos n.
  if (is.numeric(x)){
    x = x + n
    # En caso de que el numero obtenido sea mayor a 9, le restamos 10, ya que queremos
    # que sea un numero entre 0 y 9.
    if (x > 9){
      x = x - 10
    }
    
    # Si no es un numero y es una letra, obtenemos la posicion de la letra.
  } else if (is.character(x)){
    pos = obtenerPosLetra(x)
    
    # A la posicion le sumamos n, y en caso de que sea mayor a la cantidad de letras,
    # le restamos esta cantidad.
    pos = pos + n
    if (pos > length(letters)){
      pos = pos - length(letters)
    }
    
    # Le asignamos a x la letra en la posicion pos.
    x = letters[pos]
  }
  return(x)
}

# obtenerValorOculto(9, 2)

criptografia <- function(v, n){
  if (!is.vector(v)) return("El primer argumento debe ser un vector")
  if (!is.numeric(n)) return("El segundo argumento debe ser un numero")
  
  w = c()
  # Recorremos el vector y le sumamos al vector w el elemento oculto
  for (i in 1:length(v)){
    w = append(w, obtenerValorOculto(v[i], n))
  }
  
  return(w)
}

criptografia(c("h", "o", "l", "w"), 4)
criptografia(c(0,1,2,3,9), 2)
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

# Primero comprobamos si el año 'x' es divisible por el numero 'y'
es_divisible <- function(x, y){
  if (!is.numeric(x) || !is.numeric(y)) return("Los argumentos deben ser numeros")
  
  # Si el resto es 0, entonces es divisible. Sino no.
  return(x%%y == 0)
}

# Aca comprobamos si el año es bisiesto.
es_bisiesto <- function(x){
  if (!is.numeric(x)) return("El argumento debe ser un numero (año)")
  
  # Si es divisible por 4 y no es divisible por 100, devuelve true. Si es divisible
  # por 4 y es divisible por 400, entonces tambien devuelve true. Sino, devuelve false
  return(es_divisible(x, 4) && (!es_divisible(x, 100) || es_divisible(x, 400)))
}

numero_dias <- function(x){
  if (!is.numeric(x)) return("El argumento debe ser un numero (año)")
  
  # Los años bisiestos tienen 366 dias, mientras que los no bisiestos tienen 365.
  if (es_bisiesto(x)){
    return(366)
  } else return(365)
}

numero_dias(1900)
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

# Damos vuelta el vector para poder trabajar como se hace con los numeros binarios.
# Existe el metodo rev() para esto, pero lo hacemos asi para ver como se haría.
voltear_vector <- function(b){
  if (!is.vector(b)) return("El argumento debe ser un vector.")
  
  
  # Creamos un vector auxiliar que va a ser igual al vector b.
  v = b
  # Recorremos el vector b
  for (i in 1:length(b)){
    # Si por ejemplo i es 1, y el vector tiene 4 elementos, en la posicion 1
    # ponemos el elemento 4. Pero esto debe ser generalizado. Si estamos en
    # la posicion 2, i es 2, por lo tanto hay que reemplazar i por 3. 
    # Para obtener el valor por el cual hay que reemplazar, usamos el tamaño del
    # vector, le restamos i y le sumamos 1.
    v[i] = b[length(b) - i + 1]
  }
  
  return(v)
}

bin_to_int <- function(b){
  if (!is.vector(b)) return("El argumento debe ser un vector")
  
  # Los numeros binarios se leen de derecha a izquierda, pero nuestro vector va de
  # izquierda a derecha. Por lo tanto, o damos vuelta el vector, o trabajamos 
  # al revés. Voy a optar por la primera opcion.
  b = voltear_vector(b)
  
  num = 0
  # Recorremos el vector
  for (i in 1:length(b)){
    if (!is.numeric(b[i])) return("Todos los elementos del vector deben ser numeros")
    
    # Multiplicamos el elemento actual por 2 elevado a la posicion - 1 y se 
    # lo sumamos al numero guardado
    potencia = i - 1
    num = num + b[i] * 2^potencia
  }
  
  return(num)
}

b=c(1,1,0,1) ; bin_to_int(b)
b=c(1,0,1,0,0,1,1) ; bin_to_int(b)
b=c(1,1,0,1,1,0,1) ; bin_to_int(b)

# Segunda Parte (sacada de la guía anterior)
# Idem anterior pero ahora la función se llamará "int_to_bin(n)" y deberá transformar
# un número entero positivo en un número binario. Se adjunta el link para un video
# explicativo de la metodología, que usted deberá programar en su función
# https://www.youtube.com/watch?v=-4rUKlNeCEs
# Ejemplos: Los mismos de la parte I pero invertidos
# b=c(1,1,0,1) bin_to_int(b)=13
# b=c(1,0,1,0,0,1,1) bin_to_int(b)=83
# b=c(1,1,0,1,1,0,1) bin_to_int(b)=109

int_to_bin <- function(n){
  if (!is.numeric(n)) return("El argumento debe ser un numero")
  
  # Creamos un vector donde pondremos los resultados de la division redondeados
  # sin decimales hacia abajo. El primer elemento siempre sea n
  v = c(n)
  
  # Dividimos mientras n sea mayor a 1
  while(n > 1){
    aux = floor(n/2)
    n = aux
    v = append(v, aux)
  }
  
  # Ahora que ya tenemos los enteros, reemplazamos los pares por 0 y los impares
  # por 1.
  for(i in 1:length(v)){
    if (v[i] %% 2 == 0){
      v[i] = 0
    } else{
      v[i] = 1
    }
  }
  
  # Y recordamos que los numeros binarios se escriben de derecha a izquierda, asi
  # que volteamos el vector y lo devolvemos.
  
  return(voltear_vector(v))
}

int_to_bin(109)
#-------------------------------------------------------------------------------
# Ejercicio 12)
# En el programa R, para eliminar un elemento se pone un signo menos en la posición
# del elemento a eliminar. Por ejemplo v = [3 2 2 14] w = v[-c(3)] = [3 2 14]

# Dado un vector de números enteros positivos v (dato) eliminar los elementos
# repetidos generando un nuevo vector w.
# Note que cada vez que se elimina un elemento la longitud de vector cambia, por lo
# tanto se sugiere reemplazar en un primer paso todos los repetidos por 0 y luego
# eliminar los 0s.
# v = [3 2 2 14 3] w = [3 2 14] 

es_entero_positivo <- function(v){
  if (!is.vector(v)){
    print("El argumento debe ser un vector")
    return(FALSE)
  } 
  
  # Si el elemento no es un numero, o es menor igual a 0, o no es entero, devuelve false
  for (i in 1:length(v)){
    if (!is.numeric(v[i]) || v[i] <= 0 || v[i] != round(v[i])) return(FALSE)
  }
  
  return(TRUE)
}

contiene_elemento <- function(v, n){
  if (!is.vector(v)) return(F)
  
  # Recorremos el vector y chequeamos si el elemento ya estaba. Devolvemos true si es asi.
  for (i in 1:length(v)){
    if (v[i] == n) return(TRUE)
  }
  
  # Si llegamos hasta aca es porque no se repite, devuelve false.
  return(FALSE)
}

eliminar_repetidos <- function(v){
  if (!es_entero_positivo(v)) return("El argumento debe ser un vector de numeros enteros positivos")
  
  # Creamos un vector w vacio.
  w = c()
  
  # Recorremos el vector v.
  for (i in 1:length(v)){
    elemento_actual = v[i]
    # Si el vector w no tiene el elemento actual, lo agregamos. Sino no
    if (!contiene_elemento(w, elemento_actual)){
      w = append(w, elemento_actual)
    }
  }
  
  return(w)
}

eliminar_repetidos(c(3, 2, 2, 14, 3))
#-------------------------------------------------------------------------------
# Ejercicio 13)
# Escriba un programa "espejo" que genere a partir de una matriz cualquiera a otra
# matriz cuyos elementos sean una imagen a espejo de la original. La imagen a espejo
# será respecto de un eje vertical si el argumento eje = 1 y respecto de un eje horizontal
# si el argumento eje = 2. Si encuentra un comando para hacerlo, no lo use. 

# En este ejercicio voy a usar la funcion voltear_vector() creado en el ejercicio 11.
# Por defecto eje es 1.
espejo <- function(A, eje = 1){
  if (!is.matrix(A)) return("El primer argumento debe ser una matriz")
  if (eje != 1 && eje != 2) return("El segundo argumento debe ser 1 o 2")
  
  # Generamos una matriz con todos 0 del mismo tamaño que A.
  B = matrix(0, nrow(A), ncol(A))
  
  # Si eje es 1, recorremos por fila. Si es 2, por columna
  if (eje == 1){
    for (i in 1:nrow(A)){
      # En la fila i, ponemos el vector dado vuelta
      B[i,] = voltear_vector(A[i,])
    }
  } else if(eje == 2){
    for (i in 1:ncol(A)){
      # En la columna i, ponemos el vector dado vuelta
      B[, i] = voltear_vector(A[, i])
    }
  }
  
  return(B)
}

M = matrix(c(1:12), 4); M
espejo(M, 1)
#-------------------------------------------------------------------------------
# Ejercicio 14)
# Escribir un programa "índices" que extraiga de una matriz todos los elementos que
# tengan la suma de sus dos índices (de fila y columna) múltiplos de a. El reporte
# deberá ser un vector indicando fila y columna para cada valor extraído.

# Hacemos una funcion que nos dice si a es multiplo de b
es_multiplo <- function(a, b){
  if (!is.numeric(a) || !is.numeric(b)) return("Los argumentos deben ser numeros")
  
  # Si a es multiplo de b, entonces debe ser divisible por b
  return(a %% b == 0)
}

indices <- function(A, a){
  if (!is.matrix(A)) return("El primer argumento debe ser una matriz")
  if (!is.numeric(a)) return("El segundo argumento debe ser un numero")
  
  v = c()
  
  # Recorremos la matriz primero por fila y luego por columna
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      # Nos fijamos si la suma del indice 'i' y el indice 'j' es multiplo de a.
      # Si lo es, agregamos el elemento, su fila y su columna al vector.
      if(es_multiplo(i + j, a)){
        v = append(v, c(A[i, j], i, j))
      }
    }
  }
  
  return(v)
}
M = matrix(c(1:12), 4); M
# Devolvera primero el elemento de la matriz cuyos indices son multiplos de 3, seguido
# por la fija y la columna de ese elemento.
indices(M, 3)
#-------------------------------------------------------------------------------
# Ejercicio 15)
# Escribir un programa "intervalo" que tenga como argumento una matriz A de enteros
# positivos de dimensión cualquiera. El programa deberá encontrar los elementos de la
# matriz que son mayores e iguales que un número a y menores o iguales que un
# número b, ambos argumentos del mismo. El reporte deberá ser un vector indicando
# para cada valor encontrado, fila y columna. Si hay valores repetidos incluir todos.

# Verificamos que todos los elementos de la matriz sean numeros enteros y positivos.
# Para eso, usamos la funcion es_entero_positivo() del ejercicio 12.
matriz_entero_positivo <- function(A){
  if (!is.matrix(A)){
    print("El argumento debe ser una matriz")
    return(FALSE)
  } 
  
  for (i in 1:nrow(A)){
    # Si la fila no es entera positiva, devolvemos false
    if (!es_entero_positivo(A[i, ])) return(FALSE)
  }
  
  # Si el programa llega hasta aca, es true.
  return(TRUE)
}

intervalo <- function(A, a, b){
  if (!is.matrix(A) || !matriz_entero_positivo(A)) 
    return("El primer argumento debe ser una matriz entera positiva")
  
  if (!is.numeric(a) || !is.numeric(b)) return("El segundo y tercer argumento deben ser numeros")
  
  v = c()
  
  # Recorremos la matriz por fila y columna
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      elemento_actual = A[i, j]
      # Si el elemento actual cumple los requisitos, lo agregamos al vector v, 
      # junto a su indice fila y columna.
      if (elemento_actual >= a && elemento_actual <= b){
        v = append(v, c(elemento_actual, i, j))
      }
    }
  }
  
  return(v)
}
M = matrix(c(1:12), 3, byrow = TRUE); M
intervalo(M, 3, 8)
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


ajedrez <- function(alfil, torre){
  if (!is.vector(alfil) || !is.vector(torre)) return("Los argumentos deben ser vectores")
  
  # Obtenemos los indices en el que se encuentra cada pieza
  fila_alfil = alfil[1]
  col_alfil = alfil[2]
  
  fila_torre = torre[1]
  col_torre = torre[2]
  
  # Verificamos que los indices estren entre 1 y 8
  if (fila_alfil < 1 || fila_alfil > 8 || col_alfil < 1 || col_alfil > 8) 
    return("El alfil no entra en el tablero")
  
  if (fila_torre < 1 || fila_torre > 8 || col_torre < 1 || col_torre > 8) 
    return("La torre no entra en el tablero")
  
  # Verificamos que no esten en la misma posicion
  if (fila_alfil == fila_torre && col_torre == col_alfil) return("Las piezas estan en el mismo lugar")
  
  # Genero una matriz simulando el ajedrez para poder verlo graficamente.
  A = matrix(".", 8, 8)
  A[fila_alfil, col_alfil] = "A"
  A[fila_torre, col_torre] = "T"
  print(A)
  
  # La torre se puede comer al alfil si alguno de sus indices coincide.
  if (fila_alfil == fila_torre || col_torre == col_alfil) return("La torre se come al alfil")
  
  # El alfil se come a la torre si está en diagonal. Para saber si está en diagonal, el indice
  # de la torre debe ser (n, n) veces el del alfil. Pero tambien puede ser (n, -n) veces.
  # Por lo tanto, lo importante es el modulo de la resta, y no la resta en si.
  if (abs(fila_alfil - fila_torre) == abs(col_alfil - col_torre))
    return("El alfil se come a la torre")
  
  return("Ninguna se come")
}

ajedrez(c(5, 3), c(2, 3)) # La torre se come al alfil porque estan en la misma columna
ajedrez(c(5, 3), c(5, 8)) # La torre se come al alfil porque estan en la misma fila

ajedrez(c(5, 3), c(6, 4)) # El alfil se come a la torre
ajedrez(c(8, 8), c(1, 1)) # El alfil se come a la torre
ajedrez(c(5, 4), c(3, 6)) # El alfil se come a la torre
ajedrez(c(2, 3), c(3, 2)) # El alfil se come a la torre

ajedrez(c(3, 6), c(2, 4)) # Ninguno se come
#-------------------------------------------------------------------------------
# Ejercicio 17)
# Diseña una función que, dada una lista de números enteros, devuelva el número de
# "series" que hay en ella. Llamamos "serie" a todo tramo de la lista con valores
# idénticos.
# Por ejemplo, el vector [1, 1, 8, 8, 8, 8, 0, 0, 0, 2, 10, 10] tiene 5 "series" (tener en
# cuenta que el 2 forma parte de una "serie" de un solo elemento)

series <- function(v){
  if (!is.vector(v)) return("El argumento debe ser un vector")
  
  # Iniciamos una variable en 1
  cant = 0
  
  # Recorremos el vector
  for (i in 1:length(v)){
    # Si estamos en el primer elemento, sumamos uno a la serie ya que siempre sera
    # una serie.
    if (i == 1){
      cant = cant + 1
    } else{
      # Si no es uno, comprobamos que si elemento actual es igual al anterior.
      # Si no lo es, significa que es otra serie.
      if (v[i] != v[i-1]){
        cant = cant + 1
      }
    }
  }
  
  return(cant)
}

series(c(1, 1, 8, 8, 8, 8, 0, 0, 0, 2, 10, 10))
#-------------------------------------------------------------------------------
# Ejercicio 18)

# Primera Parte
# Se desea verificar si el generador de números aleatorios de R funciona bien. Para
# probarlo se debe escribir una función prob(n,k). La función debe generar con el
# comando runif() un vector de n números enteros 1 y 10 (redondee para arriba). Con
# ese vector se verificará que la frecuencia de aparición de números x <= k sea la que
# indica la distribución uniforme.
# Ejemplo: prob(100000,5) aprox. 0.5

prob <- function(n, k){
  if (!is.numeric(n) || !is.numeric(k)) return("Los argumentos deben ser numeros")
  # Redondeamos para arriba con ceiling
  v = ceiling(runif(n, 1, 10))
  
  apariciones = 0
  # Recorremos el vector para buscar apariciones de numeros menores a k
  for (i in 1:length(v)){
    if (v[i] <= k){
      apariciones = apariciones + 1
    }
  }
  
  # Devolvemos la cantidad de apariciones dividido la cantidad de numeros.
  return(apariciones / n)
}

prob(100000,5)
# Segunda Parte
# Idem anterior pera ahora se desea verificar el comando rchisq. Para probarlo se debe
# escribir una función prob2(n,gl,k).
# El vector v será el que surja de la aplicación directa del comando para "gl" grados de
# libertad. Con ese vector se verificará que la frecuencia de aparición de números x <=
# k sea la que indica la distribución chi cuadrado con gl grados de libertad.
# Ejemplo: prob2(100000,5,7) aprox 0.7793597

prob2 <- function(n, gl, k){
  if (!is.numeric(n) || !is.numeric(k) || !is.numeric(gl)) return("Los argumentos deben ser numeros")
  # Redondeamos para arriba con ceiling
  v = rchisq(n, gl)
  
  apariciones = 0
  # Recorremos el vector para buscar apariciones de numeros menores a k
  for (i in 1:length(v)){
    if (v[i] <= k){
      apariciones = apariciones + 1
    }
  }
  
  # Devolvemos la cantidad de apariciones dividido la cantidad de numeros.
  return(apariciones / n)
}

prob2(100000,5,7)
#-------------------------------------------------------------------------------
# Ejercicio 19)
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

cercano <- function(A, v){
  if (!is.matrix(A) || ncol(A) != 2) return("El primer argumento debe ser una matriz n * 2")
  if (!is.vector(v) || length(v) != 2) return("El segundo argumento debe ser un vector de 2 elementos.")
  
  # Obtengo los puntos x e y del vector v

  x = v[1]
  y = v[2]
  
  # Creo un vector donde la minima distancia y los datos seran los del primer elemento.
  # La distancia será guardada en v[4], la cual será usada mas tarde.
  v = c(1, A[1, 1], A[1, 2], sqrt((x - A[1, 1])^2 + (y - A[1, 2])^2))
  
  # Si la longitud del vector es 1, devuelve el vector encontrado.
  if (nrow(A) == 1) return(v)
  # Recorro desde la segunda fila, ya que la primera ya está puesta.
  for (i in 2:nrow(A)){
    # Para cada fila, obtengo los puntos y los guardo en las variables auxiliares
    x_aux = A[i, 1]
    y_aux = A[i, 2]
    
    # Obtengo la distancia entre el punto actual y el punto pasado por parametro
    distancia_actual = sqrt((x - x_aux)^2 + (y - y_aux)^2)
    
    # No hace falta, pero vamos a poner que la distancia minima es la menor entre
    # la distancia actual y la minima anterior, usando el comando min()
    minima_distancia = min(distancia_actual, v[4])
    
    # Si la minima distancia es la distancia actual, debo sobre-escribir el vector.
    if (minima_distancia == distancia_actual){
      v = c(i, x_aux, y_aux, minima_distancia)
    }
  }
  
  return(v)
}

M = matrix(1:16, ncol = 2) ; M
cercano(M, c(5,10))
#-------------------------------------------------------------------------------
# Ejercicio 20)
# Escribir un programa "diagonales" que extraiga de una matriz A cuadrada cualquiera
# los vectores formados por sus diagonales secundarias y por la contradiagonal. Si
# encuentra un comando para hacerlo, no lo use.

# Vamos a suponer que las diagonales secundarias son las que no son la principal...
diagonales <- function(A){
  if (!is.matrix(A) || nrow(A) != ncol(A)) return("El argumento debe ser una matriz cuadrada")
  
  # Creamos una matriz donde pondremos las diagonales.La matriz será de 3 x la cantidad
  # de filas de A
  B = matrix(0, 3, nrow(A))
  
  # Vamos a rotar la matriz 3 veces para obtener la diagonal principal.
  # Para eso usamos la funcion espejo() del ej 13.
  for (i in 1:3){
    if (i == 2){
      A = espejo(A, 2)
    } else{
      A = espejo(A, 1)
    }
    
    # Ahora obtenemos la diagonal principal y lo guardamos en la matriz B.
    for (j in 1:nrow(A)){
      for (k in 1:ncol(A)){
        if (j == k){
          B[i, j] = A[j, k]
        }
      }
    }
  }
  
  
  return(B)
}

M = matrix(1:16,4, byrow = T) ; M
diagonales(M)

# Ejercicio 21)
# Primera Parte
# Se pide escribir una función "multiplo(b,k)" que verifique si un número x y los
# números que surgen del mismo son múltiplos de un número "k" dado (entero
# positivo).
# Por números que surgen de x se entiende los números que se forman tomando los
# dígitos de x en forma secuencial y acumulativa de izquierda a derecha.
# Por ejemplo del número 3948743 surgen los números: 3, 39, 394, 3948, 39487,
# 394874 y 3948743
# Para encontrar dichos números hay 2 alternativas:
# Dividir x por las potencias crecientes de x comenzando de la potencia 0 y tomar el
# redondeo del resultado hacia abajo
# floor(3948743/1) = 3948743
# floor(3948743/10) = 394874
# floor(3948743/100) = 39487
# ...
# floor(3948743/1000000) = 3
# floor(3948743/10000000) = 0 - > este no se usa
# Otra posibilidad es ingresar el número como vector de dígitos
# x = c(3, 9, 4, 8, 7, 4, 3)
# Ejemplo:
#  multiplo(3948743,2)= c(394874, 3948, 394)

# Hago una funcion que me convierta numeros a un vector como pide el enunciado.
num_a_vector <- function(b){
  if (!is.numeric(b)){
    print("El argumento debe ser un numero.")
    return(b)
  }
  
  v = c()
  
  contador = 0
  x = b
  # Repito, mientras b sea mayor a 0, la logica que dice el enunciado.
  while(b > 0){
    b = floor(x / 10^contador)
    if (b != 0){
      v = c(b, v)
    }
    contador = contador + 1
  }
  return(v)
}

multiplo <- function(b, k){
  if (!is.numeric(b) || !is.numeric(k)) return("Los argumentos deben ser numeros.")
  
  # Obtengo un vector a partir del numero b.
  v = num_a_vector(b)
  w = c()
  
  # Recorro el vector
  for (i in 1:length(v)){
    # Si el elemento actual es multiplo de k, lo agrego a w
    if (v[i] %% k == 0){
      w = append(w, v[i])
    }
  }
  return(w)
}

multiplo(3948743,2)

#Segunda Parte
# Idem anterior pero ahora la función se llamará "multiplo2(b,k)" y deberá considerar
# como números que surgen de x a los números que se forman tomando los dígitos de x
# en forma secuencial y acumulativa de izquierda a derecha y de derecha a izquierda.
# Por ejemplo para el número 3948743 surgen los números:
# 3, 39, 394, 3948, 39487, 394874, 3948743, 3, 43, 743, 8743, 48743, 948743, 3948743
# (no importa si aparecen repetidos)
# Ejemplos:
#  multiplo2(3948743,4)= c(3948)

# Hago una funcion para separar el numero en varios numeros.
separar_numeros <- function(x){
  if (!is.numeric(x)){
    print("El argumento debe ser un numero.")
    return(x)
  }
  
  digitos = c()
  
  # Itero mientras x sea mayor a 0
  while (x > 0) {
    # Guardo el resto de dividir x por 10 en la primera posicion del vector.
    # Por ejemplo, si el numero es 423, lo divido por 10 y me da modulo 3. Este 3
    # lo pongo en la posicion 1 del vector digitos.
    # De esta manera puedo obtener el ultimo numero del numero...
    digitos = c(x %% 10, digitos)
    
    # Ahora a x le resto el numero que agregue al vector. Quedaría 42, y empieza
    # el ciclo nuevamente
    x = x %/% 10
  }
  
  return(digitos)
}

revertir_numero <- function(b){
  if (!is.numeric(b)){
    print("El argumento debe ser un numero.")
    return(b)
  }
  
  # Separo los numeros y los guardo en el vector v.
  v = separar_numeros(b)
  
  # Ahora doy vuelta el vector
  w = c()
  for (i in 1:length(v)){
    w[i] = v[length(v) - i + 1]
  }
  
  # Y junto los numeros del vector
  # La cantidad de digitos es la longitud de w. Voy a multiplicar cada numero
  # por 10 elevado a la longitud menos la posicion en la que se encuentra, y sumarle el resto de numeros.
  # Por ejemplo, en el vector (4, 5), multiplico 4 por 10, que da 40. Luego multiplico
  # 5 por 1, que da 5. Luego los sumo y obtengo 45.
  num = 0
  for (i in 1:length(w)){
    num = num + w[i] * 10^(length(w) - i) 
  }
  
  return(num)
}


obtener_vector_der_izq <- function(x){
  if (!is.numeric(x)){
    print("El argumento debe ser un numero.")
    return(x)
  } 
  
  # Doy vuelta el numero
  x = revertir_numero(x)
  # Obtengo un vector a partir del numero
  v = separar_numeros(x)
  w = c()
  
  # Recorro el vector v
  for (i in 1:length(v)){
    # Hago una variable num que empieza en 0.
    num = 0
    # Recorro los elementos del vector desde 1 hasta i. Entonces a num le sumo
    # el numero en la posicion i-j+1. Por ejemplo:
    # Si el numero es 234, entonces hace esto:
    # Cuando i es 1, le sumo a num 0 + (2*10^0) = 2.
    # Cuando i es 2, sumo: 0 + (3*10^1 + 2*10^0) = 32
    # Cuando i es 3, sumo 0 + (4 * 10^2 + 3 * 10^1 + 2 * 10 ^0) = 432
    for (j in 1:i){
      num = num + v[i-j+1] * 10^(i - j) 
     
    } 
    
    # Cada iteracion voy sumando los valores al vector w.
    w = append(w, num)
  }
  
  return(w)
}


multiplo2 <- function(b, k){
  if (!is.numeric(b) || !is.numeric(k)) return("Los argumentos deben ser numeros.")
  
  # Obtengo un vector a partir del numero b.
  v = num_a_vector(b)
  
  # Ahora le tengo que agregar al vector, lo mismo pero de derecha a izquierda.
  v = append(v, obtener_vector_der_izq(b))
  w = c()
  # Recorro el vector
  for (i in 1:length(v)){
    # Si el elemento actual es multiplo de k, lo agrego a w
    if (v[i] %% k == 0){
      w = append(w, v[i])
    }
  }
  return(w)
}

multiplo2(3948743,4)



# Extra: Como obtener cantidad de digitos de un numero:
obtener_cant_digitos <- function(x){
  if (x == 0) return(1)
  return(floor(log10(x)+1))
}
obtener_cant_digitos(100)
