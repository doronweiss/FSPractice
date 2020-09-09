module ExpertF3

let rec fact n = 
  if n<=1 then 1 
  else n * fact(n-1)

let funcsq x:float =
  x*x

let funcadd x:float =
  x + x

