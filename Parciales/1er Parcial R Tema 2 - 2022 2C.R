# Ejercicio 1
# Usted trabaja para una consultora económica. Su jefe le pide escribir un programa "inflación_acumulada" que tenga como argumento una matriz
# A de dimensión 4x3; donde las filas representan lso 4 trimestres del año y las columnas los respectivos meses de cada trimestre. 
# Cada elemento de la matriz indicará (en porcentaje) la inflación del respectivo mes.
# Reportar la inflación acumulada del año en porcentaje.
# Ejemplo:
# Si la matriz de la inflación mensual es 
#    [,1] [,2] [,3]
# [1,]    5    4    3
# [2,]    6    6    4
# [3,]    5    4    3
# [4,]    5    6    3
# La inflación acumulada es 69.47
# Recordar fórmula de inflación acumulada: ( 1 + Inflación de Ene) * ( 1 + Inflación de Feb) * ... * ( 1 + Inflación de Dic) 

inflaciones = c(5,4,3,6,6,4,5,4,3,5,6,3)
A = matrix(inflaciones, byrow = T, ncol=3, nrow = 4); A

inflacion_acumulada1 = function(A){
  dato=1
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      dato=dato*(1+A[i,j]/100)
    }
  }
  return(round((dato-1)*100,2))
}

inflacion_acumulada1(A)

# Ejercicio 2
# Idem anterior pero reportar una matriz de salida donde cada elemento sea la inflación acumulada hasta dicho mes. Si la matriz de datos mensuales
# de inflación es la misma del ejercicio 1, la matriz pedida será:
#        [,1]     [,2]     [,3]
# [1,] 1.050000 1.092000 1.124760
# [2,] 1.192246 1.263780 1.314332
# [3,] 1.380048 1.435250 1.478308
# [4,] 1.552223 1.645356 1.694717

inflacion_acumulada2 = function(A){
  dato=1
  B=matrix(1,ncol=3,nrow=4)
  for (i in 1:nrow(A)){
    for (j in 1:ncol(A)){
      dato=dato*(1+A[i,j]/100)
      B[i,j]=dato
    }
  }
  return(B)
}

inflacion_acumulada2(A)
