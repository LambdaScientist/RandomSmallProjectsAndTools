--http://www.parsonsmatt.org/2016/10/26/grokking_fix.html

fix :: (a -> a) -> a
fix f = 
    let x = f x 
     in x

weird = fix (\f x -> if x >= 0 then do print x; f (x - 1) else pure x) 4


cosList = fix (\f x -> if x == cos x then [x] else cos x : f (cos x)) 