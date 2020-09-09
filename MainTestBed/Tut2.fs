module Tut2

type SearchResult =
  | NoHit
  | Hit of string * int
 
let formatResult sr =
  match sr with
  | NoHit -> "no record found"
  | Hit (_, 0) -> "no good match found"
  | Hit (str, score) -> sprintf "found %s with score %d" str score