library(matrixcalc)

A = matrix(c (1, 2, -3, -1, 0, -3, 2, 6, -3, -1, 3, 1, 2, 3, 2, -1), nrow=4, byrow=T);A
B = matrix(c ( 0,-8, 0, -8), nrow=1, byrow=T);B

faktorisasi_LU = function(x)
{
  if (!is.square.matrix(x))
    stop("X bukan matriks segi")
  if (!is.numeric(x))
    stop("X bukan numerik")
  n = nrow(x)
  L = matrix(0, nrow=n, ncol=n)
  U = matrix(0, nrow=n, ncol=n)
  diag(L) = rep(1, n)
  for (i in 1:n) {
    ip1 = i + 1
    im1 = i - 1
    for (j in 1:n) {
      U[i,j] = x[i,j]
      if (im1 > 0) {
        for (k in 1:im1) {
          U[i,j] = U[i,j] - L[i,k] * U[k,j]
        }
      }
    }
    if (ip1 <= n) {
      for (j in ip1:n) {
        L[j,i] = x[j,i]
        if (im1 > 0) {
          for (k in 1:im1) {
            L[j,i] = L[j,i] - L[j,k] * U[k,i]
          }
        }
        if (U[i,i] == 0)
          stop("X adalah matriks singular")
        L[j,i] = L[j,i] / U[i,i]
      }    
    }
  }
  result = list(L=L, U=U)
  return(result)
}

substitusi_maju = function (A,B){
  n = nrow(A)
  B = matrix(B,n,1,byrow=TRUE)
  y = matrix(rep(0,n),n,1)
  x = matrix(rep(0,n),n,1)
  y[1,1] = B[1,1]
  for (k in 2:n){
    sum = 0
    for(c in 1:(k-1)){
      sum = sum+(A[k,c]*y[c,1])
    }
    y[k,1] = B[k,1]-sum
  }
  return (y)
}

substitusi_balik = function(A,B){
  n = nrow(A)
  B = matrix(B,n,1,byrow=TRUE)
  x = matrix(rep(0,n),n,1)
  x[n,1] = B[n,1]/A[n,n]
  for(k in (n-1):1){
    sum = 0
    for(j in (k+1):n){
      sum = sum+(A[k,j]*x[j,1])
    }
    x[k,1] = (B[k,1]-sum)/A[k,k]
  }
  return(x)
}

LU = faktorisasi_LU(A)
L = LU$L
U = LU$U

C = substitusi_maju(L,B)
C = t(C)
hasil = substitusi_balik(U,C)
print(hasil)

