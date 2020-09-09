// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module program
module dwu =
  open dwfsutils

open System
open System.Text.RegularExpressions
open Tut2
open ExpertF3

 
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

let rec DWFibo x=
  match x with
    |0->0
    |1->1
    |_-> DWFibo (x-1) + DWFibo (x-2)

let rec factorial = function
    | 0 | 1 -> 1
    | n -> n * factorial (n - 1)

let rec gcd x y =
    printfn "in GCD: %A %A" x y
    if y = 0 then x
    else gcd y (x % y)

let safediv x y =
    match y with
    | 0.0 -> None
    | _ -> Some(x/y)
    
let containsNegativeNumbers inList =
  let isNeg x = x < 0
  let fltrd = List.filter isNeg inList
  if List.length fltrd>0
  then Some(fltrd)
  else None;


let functionalSum numbers = 
    numbers 
    |> Seq.map (fun(a:float)->a*a)
    //|> Seq.map (fun (a:float)->a+3.0)
    |> Seq.sum


let mult a b=
  a*b

let mult5 = mult 5

let (===) str (regex : string) =
    Regex.Match(str, regex).Success


let sumOfSqrs x = 
  [1..x]|>List.map (fun z->z*z) |> List.sum

let rec List22 l =   
  printfn "Element: %A" (l)
  match l with
  | [] -> None
  | _::tail -> List22 tail

let rec doAll f list =
  match list with
  | [] -> []
  | head :: tail -> f head :: doAll f tail

let optionPatternMatch input =
  match input with
  | None -> printfn "input is missing"
  | Some s -> printfn "input is a %A" s

let rec b2iw xs m = 
  match xs with
  | [] -> 0
  | head :: [] -> (head * m) 
  | head :: tail -> (head * m) + (b2iw tail (m*2))

let b2i xs =  b2iw (List.rev  xs) 1

let addlist (xs : double List ) =
  let a::b::_ = xs in
    if List.length xs = 2 then a+b
    else 0.0

//let impJunk x  =
//  for i = 100 downto 1 do
//    if i*i<x then break
//  0

let rec canCompose M x =
  if Set.isEmpty M then false else
  let biggest, smallest = Set.maxElement M, Set.minElement M
  let sum = biggest + smallest
  if   sum > x then canCompose (Set.remove biggest M)  x
  elif sum < x then canCompose (Set.remove smallest M) x
  elif sum = x then true
  else failwith "Encountered failure to compare numbers / NaN"

type myrec = {
  name : String
  id : int}

let getPesr (x:int) = 
  {name = "Doron"; id = x}

let rec accusumh (n:int) xs =
  match xs with
    | [] -> []
    | z::zs -> 
      let s = n+z in 
        s :: (accusumh s zs)

let accusum xs =
  accusumh 0 xs

let add1 = (+) 1

let add2 = add1 2

[<EntryPoint>]
let main argv = 
  //let validValue = Some(99.5)
  //let invalidValue = None
  //optionPatternMatch validValue
  //optionPatternMatch invalidValue

  //formatResult NoHit;;
  //Tut2.formatResult (Tut2.Hit("bob", 0));; 
  //formatResult (Hit("alice", 123));; 
  //List22 [1;2;3;4;5]
  //printfn "doall %A " (doAll (fun n -> n + n) [1; 2; 3])
  //printfn ""
  //let x = ExpertF3.fact 5
  //printfn "ef3=>%A" x
  //
  //let comb = ExpertF3.funcadd >> ExpertF3.funcsq
  //let comb2 = ExpertF3.funcsq >> ExpertF3.funcadd
  //let a = comb2 5.0
  //printfn "Comb = %A" a
  //
  let l = Seq.init 5 (fun x -> x) //(fun x -> x*3)
  let diff x =
    x |> Seq.pairwise |> Seq.map (fun (x, y) -> y - x)
  printfn "Diff=> %A" (diff l)
  for x in l do
    printfn "%d" x
  printfn "B2I => %A" (b2i [1;0;0;1;0;0;1;1])
  printfn "addlist => %A" (addlist [1.0;0.0;0.0;1.0])
  printfn "addlist => %A" (addlist [1.0;3.5])
  printfn "Can compose: %A" <| canCompose (set [1;2;3;4;5;6;7;8]) 12  
  let p = getPesr 056789
  printfn "getPesr: %A" <| p
  printfn "getPesr: %A" <| p.GetType()
  printfn "accumsum: %A" (accusum [1;2;3;4;5;6;7;8;9;10])
  // doc
  [-10..10]
  |> List.filter (fun x -> x>0)
  |> List.map (fun x -> x*2)
  |> printfn "Test: %A"
  0
