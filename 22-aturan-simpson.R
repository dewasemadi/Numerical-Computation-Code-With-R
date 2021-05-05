f = function(x)
    (x^2+1)/(2*x);

simpson = function(f, N, batasAtas, batasBawah){
    h = (batasAtas - batasBawah)/N
    inc = batasBawah
    sumFiGanjil = 0
    sumFiGenap = 0
    for(i in 0:N){
        fxi = f(inc)
        if(i == 0)
            f0 = fxi
        else if(i%%2!=0 && i<=N-1)
            sumFiGanjil = sumFiGanjil + fxi
        else if(i%%2==0 && i<=N-2)
            sumFiGenap = sumFiGenap + fxi
        else
            fn = fxi
        inc = inc + h
        
    } 
    return (h/3*(f0 + 4*sumFiGanjil + 2*sumFiGenap + fn))
}

x = simpson(f, 8, 3, 1); x
