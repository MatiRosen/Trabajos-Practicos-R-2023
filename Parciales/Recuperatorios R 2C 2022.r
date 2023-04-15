# Ejercicio 1 (1er Recuperatorio 2C 2022)
# Dado un vector de números enteros positivos "v" de longitud arbitraria, escriba una función transforma(v) que al primer elemento de "v"
# le sume 2, al 2do le calcule la raíz cuadrada, al tercero lo use como exponente del número "e" y al cuarto lo eleve al cubo.
# Así sucesivamente con el resto de los elementos, en ese orden.
# La salida debe ser una matriz "A" cuya primera columna sea el vecotr "v" y la segunda el vector "v" transformado.

# Ejemplo: 
#      v          vt
# [1,] 1    3.000000
# [2,] 2    1.414214
# [3,] 3   20.085537
# [4,] 4   64.000000
# [5,] 5    7.000000
# [6,] 6    2.449490
# [7,] 7 1096.633158

# OPCION 1
v = c(1:7)
transforma = function(v){
  s = seq(from=1,to=length(v),by=4)
  vt = c()
  for (i in s){
    vt = c(vt,v[i]+2,v[i+1]**0.5,exp(v[i+2]),v[i+3]**3)
  }
  vt=vt[1:length(v)]
  A=cbind(v,vt)
  return(A)
}

transforma(v)



# OPCION 2
v = c(1:7)
transforma = function(v){
  vt = c()
  for (i in 1:length(v)){
    if (i%%4 == 1){
      vt = c(vt,v[i]+2)
    } else if (i%%4 == 2){
      vt = c(vt, v[i]**0.5)
    } else if (i%%4 == 3){
      vt = c(vt, exp(v[i]))
    } else{
      vt = c(vt, v[i]**3)
    }
  }
  A=cbind(v,vt)
  return(A)
}

transforma(v)

################################################################################

# Ejercicio 2 (1er Recuperatorio 2C 2022)
# Idem anterior, pero agregar una tercera columna tal que, si el elemento "v" es primo, reemplaza al valor correspondiente en "vt" por el
# elemento + 1.
# Ejemplo:
#      v          vt       v3
# [1,] 1    3.000000  3.00000
# [2,] 2    1.414214  3.00000
# [3,] 3   20.085537  4.00000
# [4,] 4   64.000000 64.00000
# [5,] 5    7.000000  6.00000
# [6,] 6    2.449490  2.44949
# [7,] 7 1096.633158  8.00000

es_primo <- function(x){
  if (x == 2) {
    1
  } else if (any(x %% 2:(x-1) == 0)) {
    return(0)
  } else { 
    return(1)
  }
}

v = c(1:7)

transforma = function(v){
  s = seq(from=1,to=length(v),by=4)
  vt = c()
  for (i in s){
    vt = c(vt,v[i]+2,v[i+1]**0.5,exp(v[i+2]),v[i+3]**3)
  }
  vt=vt[1:length(v)]
  
  v3 = vt
  
  for (i in 2:length(v)){
    if (es_primo(v[i])==1){
      v3[i] = v[i]+1
    }
  }
  
  A=cbind(v,vt,v3)
  return(A)
}

transforma(v)

################################################################################

# (3er Recuperatorio 2C 2022)
# Programe una función "borde" que recorra el borde de una matriz dato "A" de elementos enteros con filas > 2 y columnas > 2.
# Se debe sumar 2 al elemento del borde si es impar y dejar sin cambios a los pares. Los elementos que no están en el borde de la matriz
# deben quedar inalterados.

A = matrix(c(7,3,10,13,16,4,3,5,17,7,11,10,19,11,1,7,13,5,17,3),nrow=4,ncol=5,byrow=T); A

borde = function(A){
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      if ((i==1 | j==1 | j==ncol(A) | i==nrow(A)) & (A[i,j]/2!=round(A[i,j]/2))){
        A[i,j]=A[i,j]+2
      }
      else{
        A[i,j]=A[i,j]
      }
    }
  }
  return(A)
}

borde(A)



