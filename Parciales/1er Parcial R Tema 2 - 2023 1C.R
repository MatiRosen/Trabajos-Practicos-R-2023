# Ejercicio 1
# Dada una matriz "Datos" de 3 x n, siendo la primer fila el número de registro 
# del alumno, la segunda el número de curso (hay 3 cursos posibles) y la tercera
# la nota del alumno, se pide crear una función "Reporte" que añada a dicha matriz
# las siguientes filas: 
#   * Nota promedio del curso (en coincidencia con la ocurrencia de cada curso)
#   * Número de alumnos por curso (en coincidencia con la ocurrencia de cada curso)

# Ejemplo: Datos: 122 145 235 234 112 236 231 215 135 233
#                  1   1   3   2   2   3   3   2   1   2
#                  9   5   7   5   8   9   6   7   4   5

# Reporte: Registro  122  145  235  234  112  236  231  215  135  233
#          Curso      1    1    3    2    2    3    3    2    1    2
#          Nota       9    5    7    5    8    9    6    7    4    5
#          Prom_Nota  6    6   7.33 6.25 6.25 7.33 7.33 6.25  6   6.25
#          Cant_Alum  3    3    3    4    4    3    3    4    3    4


# Usamos esta función para obtener el promedio. El mismo lo obtenemos a partir
# del vector notas, del vector cursos, y del numero de curso que queremos obtener
# el promedio.
obtener_promedio <- function(notas, cursos, curso){
  total = 0
  cantidad = 0
  # Recorremos el vector de notas, ya que es de ahí de donde obtendremos el promedio.
  for (i in 1:length(notas)){
    # Si el curso en la posición i, es el curso que estamos calculando el promedio,
    # entonces a la variable 'total' le agregamos la nota del indice i, y a la cantidad
    # le agregamos uno.
    if (cursos[i] == curso){
      total = total + notas[i]
      cantidad = cantidad + 1
    }
    # Si el curso en la posición i es otro curso del cual no nos interesa el promedio,
    # entonces lo ignoramos. 
  }
  
  # Devolvemos el promedio.
  return(round(total/cantidad, 2))
}

# En esta función vamos a devolver cuantos alumnos hay en el curso que buscamos
obtener_total <- function(cursos, curso){
  cant = 0
  
  # Como queremos saber cuantos alumnos hay en el curso 'curso', recorremos solo
  # el vector de cursos, y vemos cuantas veces se repite el numero del curso.
  for (i in 1:length(cursos)){
    # Si el curso en la posición i es el curso que buscamos, entonces significa
    # que hay un alumno mas en ese curso.
    if (cursos[i] == curso){
      cant = cant + 1
    }
  }
  
  return(cant)
}

reporte <- function(Datos){
  # Creo una matriz auxiliar que va a ser de 5 x n, ya que ahora tengo 5 filas.
  # n es la cantidad de columnas de la matriz Datos. Ordeno la matriz por fila.
  A = matrix(0, 5, ncol(Datos), T)
  
  # Obtengo los vectores registros, cursos y notas a partir de la matriz Datos.
  registros = Datos[1, ]
  cursos = Datos[2, ]
  notas = Datos[3, ]
  
  # Los inserto en la matriz A.
  A[1, ] = registros
  A[2, ] = cursos
  A[3, ] = notas
  
  # Ahora se deben completar la cuarta y quinta filas de la matriz A. Para eso
  # necesitamos 2 vectores más: el vector Prom_Nota y el vector Cant_Alum. 
  # Para obtener estos 2 vectores, hay que ir completandolo según el indice
  # del curso. Por eso, recorremos el vector curso, y para cada curso, completamos
  # la matriz en el indice especifico con los datos obtenidos.
  for (i in 1:length(cursos)){
    # Completamos en la fila 4 con el promedio de notas obtenido de la funcion en cada
    # indice
    A[4, i] = obtener_promedio(notas, cursos, cursos[i])
    
    # Completamos la fila 5 con el total de alumnos en el curso del indice i.
    A[5, i] = obtener_total(cursos, cursos[i])
  }
  
  return(A)
}


# Generamos la matriz Datos. Para armarla, vamos a crear 3 vectores: El de registros,
# el de notas y el de cursos.
registros = c(122, 145, 235, 234, 112, 236, 231, 215, 135, 233)
cursos = c(1, 1, 3, 2, 2, 3, 3, 2, 1, 2)
notas = c(9, 5, 7, 5, 8, 9, 6, 7, 4, 5)

# Ahora generamos la matriz con todos los datos, de 3xN, ordenando por filas.
Datos = matrix(c(registros, cursos, notas), 3, length(registros), TRUE) ; Datos

# Y ahora mostramos usamos la matriz Datos en la función reporte
reporte(Datos)




# Ejercicio 2
# Idem anterior pero ahora la única fila que se añadirá será una que señale la 
# mejor nota de cada curso (en coincidencia con el alumno, el elemento valdrá 1
# si dicha nota es la mejor para dicho curso, 0 si no.)

# La nota promedio del curso y la cantidad de alumnos del mismo se imprimirán en
# una matriz llamada "Resumen"

# Ejemplo: Datos (Idem anterior)
# Resumen: Curso      1    2    3
#          Prom_nota  6   6.25 7.33
#          Cant_Alum  3    4    3

# Reporte: 
#   Registro   122 145 235 234 112 236 231 215 135 233
#   Curso       1   1   3   2   2   3   3   2   1   2
#   Nota        9   5   7   5   8   9   6   7   4   5
#   Max_Nota    1   0   0   0   1   1   0   0   0   0

# Para verificar si la mayor nota de un curso es la nota que tenemos, necesitamos 
# recorrer todas las notas y verificar si hay alguna mas alta.
mayor_nota <- function(cursos, notas, curso, nota){
  # Recorremos el vector de notas
  for (i in 1:length(notas)){
    # Como el vector de notas y cursos tienen el mismo indice, podemos usar i para
    # ambos vectores. Entonces, podemos verificar, si el curso del indice i es el
    # curso del cual estamos buscando la nota mas alta. Si lo es, podemos comparar la nota
    # recibida por parametro con la nota que recorremos actualmente.
    if (cursos[i] == curso){
      if (notas[i] > nota){
        # Si la nota es las alta que la recibida por parametro, significa que
        # esta nota no era las mas alta, y por lo tanto devuelve false.
        return(FALSE)
      }
    }
  }
  
  # Si llegamos hasta aca, es porque nunca se devolvió FALSE, entonces efectivamente
  # era la nota mas alta y devolvemso TRUE.
  return(TRUE)
}

reporte2 <- function(Datos){
  # Creo una matriz auxiliar que va a ser de 4 x n, ya que ahora tengo 4 filas.
  # n es la cantidad de columnas de la matriz Datos. Ordeno la matriz por fila.
  A = matrix(0, 4, ncol(Datos), T)
  
  # Obtengo los vectores registros, cursos y notas a partir de la matriz Datos.
  registros = Datos[1, ]
  cursos = Datos[2, ]
  notas = Datos[3, ]
  
  # Los inserto en la matriz A.
  A[1, ] = registros
  A[2, ] = cursos
  A[3, ] = notas
  
  # Ahora necesitamos, para cada curso, poner la mejor nota en la fila 4. Entonces
  # vamos a recorrer el vector notas:
  for (i in 1:length(notas)){
    # Y para cada elemento, si la nota es las mas alta, insertamos un 1 en la fila
    # 4 y columna i.
    if (mayor_nota(cursos, notas, cursos[i], notas[i])){
       A[4, i] = 1
    }
  }
  
  # Antes de terminar, imprimimos la matriz Resumen.
  print(Resumen(Datos))
  return(A)
}

contiene_elemento <- function(v, k){
  # Si el vector está vacio, no contiene el elemento
  if (length(v) == 0) return(FALSE)
  
  
  # Recorremos el vector v
  for (i in 1:length(v)){
    # Si encontramos el valor k, devolvemos true
    if (v[i] == k){
      return(TRUE)
    }
  }
  
  # Si llegamos hasta aca no encontramos el valor k en el vector v. Devuelve False.
  return(FALSE)
}

resumen_cursos <- function(cursos){
  # Creamos un vector v, con los cursos sin repetirse
  v = c()
  
  # Recorremos todos los cursos
  for (i in 1:length(cursos)){
    # Si el curso no esta en el vector v, lo agregamos. 
    if (!contiene_elemento(v, cursos[i])){
      v = append(v, cursos[i])
    }
  }
  
  # Ordenamos de menor a mayor, (ya que la salida del ejemplo está así)
  return(sort(v))
}

Resumen <- function(Datos){
  # Obtengo los vectores registros, cursos y notas a partir de la matriz Datos.
  registros = Datos[1, ]
  cursos = Datos[2, ]
  notas = Datos[3, ]
  
  # Obtenemos los cursos sin repetidos.
  cursos_sin_rep = resumen_cursos(cursos)
  # Hacemos una matriz que sea de 3 x la cantidad de cursos sin repetirse.
  A = matrix(0, 3, length(cursos_sin_rep), TRUE)
  
  # La primera fila de la matriz son los cursos, de menor a mayor.
  A[1, ] = cursos_sin_rep
  
  # Ahora, para cada curso, ponemos el promedio y la cantidad de alumnos.
  # Estos datos ya lo tenemos en la matriz del punto 1, por lo que lo podemos usar.
  # Para eso, recorremos la fila 1 de la matriz, buscando el curso que estamos recorriendo. 
  # Una vez obtenido el curso, obtenemos los datos de esa columna. 
  Matriz_Datos_1 = reporte(Datos)
  for (i in 1:length(cursos_sin_rep)){
    for (j in 1:ncol(Matriz_Datos_1)){
      # Si el curso de la matriz es el mismo que el curso del vector de cursos,
      # entonces usamos los datos de esa columna para rellenar la matriz A.
      # Recordemos que el curso estaba en la segunda fila
      if (Matriz_Datos_1[2, j] == cursos_sin_rep[i]){
        # Recordemos que la nota promedio estaba en la cuarta fila.
        A[2, i] = Matriz_Datos_1[4, j]
        # Recordemos que la cant de alumnos estaba en la quinta fila.
        A[3, i] = Matriz_Datos_1[5, j]
      }
    }
  }
 
  
  return(A)
}

# Usamos la misma matriz Datos de antes
Datos
reporte2(Datos)








