A = matrix(c(1,2,-3,-1,0,-3,2,6,-3,-1,3,1,2,3,2,-1), nrow = 4, ncol = 4, byrow=T);A
B = matrix(c(0,-8,0,-8));B

p = nrow(A)
AB = cbind(A,B)

AB[1,] = AB[1,]/AB[1,1]

i = 2
while (i < p+1) {
  j = i
  while (j < p+1) {
    AB[j, ] = AB[j, ] - AB[i-1, ] * AB[j, i-1]
    j = j+1
  }
  while (AB[i,i] == 0) {
    AB = rbind(AB[-i,],AB[i,])
  }
  AB[i,] = AB[i,]/AB[i,i]
  i = i+1
}

for (i in p:2){
  for (j in i:2-1) {
    AB[j, ] = AB[j, ] - AB[i, ] * AB[j, i]
  }
}
AB