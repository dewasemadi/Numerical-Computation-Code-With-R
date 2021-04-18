# Solusi SPL dengan iterasi gauss-seidel
x = c(1,2,2) # tebakan awal
epsilon  = 1e-6
max_iter = 100
n = 3 #ukuran matriks n*n
a = matrix(c(3,1,-1,2,4,1,1,-1,4), nrow = n, ncol = n, byrow = T); a
b = matrix(c(3,7,4)); b
m = cbind(a,b); m #gabung matriks a & b

# cek kondisi kekonvergenan solusi dengan syarat cukup dominan secara diagonal
cek = 0
for(i in 1:n){
    sum = 0
    for(j in 1:n){
        if(i!=j)
            sum = sum + abs(a[i,j])
    }
    if(abs(a[i,i]) > sum)
        cek = cek + 1
}

# proses gauss seidel
if(cek == n){
    print("solusi aproksimasi hasil iterasi dijamin konvergen")
    for(iter in 1:max_iter){
        final_err = 0
        for(i in 1:n){
            sum = 0
            for(j in 1:n){
                if(i != j)
                    sum = sum + (m[i,j] * x[j])
            }
            temp = (m[i,n+1] - sum)/m[i,i]

            #cek error relatif
            err_rel = abs((temp - x[i])/temp)
            if(final_err < err_rel)
                final_err = err_rel

            x[i] = temp
        }
        
        print(paste("iterasi ke-", iter, "x:", x[1], "y:", x[2], "z:", x[3], "err_rel:", final_err));
        if(final_err <= epsilon) {
            cat("\nSolusi aproksimasi SPL\n")
            for(i in x)
                print(paste(i))
            break(0)
        }
    }
}else{
    print("tidak syarat cukup dominan secara diagonal, sehingga hasil iterasi tidak dijamin konvergen")
}
