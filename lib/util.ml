(* TODO Doesn't appear to be tail-call optimized, overflows the stack for
   large value of (j - i). Switch to using Streams? *)
let ( -- ) (x : int) (y : int) =
  let rec f i j =
    if i > j then
      [] 
    else if i = j then
      [i]
    else
      i :: f (i + 1) j in
  f x y

let repeat f n = List.map f (1 -- n)

let repeat_and_split f n = repeat f n |> List.split

type tuple = float * float

type triple = float * float * float

type quadruple = float * float * float * float

type sextuple = float * float * float * float * float * float
