library(matlib)

A = matrix(c(4,-3,1,1,5,-2,2,2,3), nrow = 3, ncol = 3, byrow=T);A
B = matrix(c(7,10,17));B


if(det(A)!=0){
  inv(A) %*% B
}else
  print("Matriks A tidak memiliki invers")