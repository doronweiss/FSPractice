﻿module dwfsutils


let rec gcd x y =
  if y = 0 then x else gcd y (x % y)