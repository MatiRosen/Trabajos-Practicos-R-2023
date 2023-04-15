# Ejercicio 1
# Dada una matriz cuadrada "A" dato, de orden "n x n" cualquiera, cuyos elementos son todos ceros y un vector "v" de longuitud (n^2-n), 
# programar una función "relleno(n,v), que rellene columna por columna la matriz "A" con los elementos del vector "v" en orden inverso, salvo en 
# la diagonal principal, que seguirá teniendo elementos igual a cero. La salida será la matriz A así modificada.
# Ejemplo:
# v=c(1,2,3,4,5,6)
# Comando = relleno(3,v)
# A =          [,1] [,2] [,3]
#        [1,]    0    4    2
#        [2,]    6    0    1
#        [3,]    5    3    0

relleno = function(n,v){
  A = matrix(0,n,n)
  v = rev(v)
  contador=0
  for (j in 1:ncol(A)){
    for (i in 1:nrow(A)){
      if (i != j){
        contador=contador+1
        A[i,j]=v[contador]
      }
    }
  }
  return(A)
}

v = c(1:6)
relleno(3,v)

# Ejercicio 2
# Programe una función "multiplos(v,x,z)" que dado un vector "v" de números enteros, encuentre aquellos que son múltiplos de "x" y/o "z".
# La función deberá tener como salida el vector de los múltiplos encontrados de cada número (x ó z) y deberá imprimir el número de elemento
# en el vector "v" de cada múltiplo encontrado. 
# El reporte print deberá ser por separado para los múltiplos de x y para los múltiplos de z.
# Ejemplo: 
# v=c(64,83,75,21,57,38,68,30,70,84)
# Comando = multiplos(v,x=7,z=5)
# Resultado:
# Múltiplos de x:
# 21 70 84
# Ubicación en v de los múltiplos de x:
# 4 9 10
# Múltiplos de z:
# 75 30 70
# Ubicación en v de los múltiplos de z:
# 3 8 9

v=c(64,83,75,21,57,38,68,30,70,84)
multiplos = function(v,x,z){
  multiplos_x = c()
  index_x = c()
  multiplos_z = c()
  index_z = c()
  for (i in 1:length(v)){
    if (v[i]/x == round(v[i]/x)){
      multiplos_x = c(multiplos_x,v[i])
      index_x = c(index_x,i)
    }
    if (v[i]/z == round(v[i]/z)){
      multiplos_z = c(multiplos_z,v[i])
      index_z = c(index_z,i)
    }
  }
  print("Múltiplos de x:")
  print(multiplos_x)
  print("Ubicación en v de los múltiplos de x:")
  print(index_x)
  print("Múltiplos de z:")
  print(multiplos_z)
  print("Ubicación en v de los múltiplos de z:")
  print(index_z)
}

multiplos(v,7,5)




