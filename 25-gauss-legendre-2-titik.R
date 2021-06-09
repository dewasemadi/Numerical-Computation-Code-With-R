f = function(x) 
  return (3*x*exp(x)+2)

lagendre = function(f, batas_atas, batas_bawah){
    b = batas_atas
    a = batas_bawah
    if(a == -1 && b == 1){
        return (f(1/sqrt(3)) + f(-1/sqrt(3)))
    } else {
        # transformasi
        x1 = ((a+b) + (b-a) * (1/sqrt(3)))/2
        x2 = ((a+b) + (b-a) * (-1/sqrt(3)))/2
        
        res1 = f(x1)
        res2 = f(x2)

        dx = (b-a)/2

        return (dx * (res1+res2))
    }
}

lagendre (f, 3, 0)