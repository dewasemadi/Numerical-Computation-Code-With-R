f = function(x)
    2*exp(x^2);

trapesium = function(f, N, batasAtas, batasBawah){
    h = (batasAtas - batasBawah)/N
    inc = batasBawah
    sumFi = 0
    for(i in 0:N){
        fxi = f(inc)
        if(i == 0)
            f0 = fxi
        else if(i<=N-1)
            sumFi = sumFi + fxi
        else 
            fn = fxi
        inc = inc + h
    } 
    return (h/2*(f0 + 2*sumFi + fn))
}

x = trapesium(f, 8, 1, 0); x