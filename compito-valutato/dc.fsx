(* Esercizio 2 - DC.fsx *)

#r "FsCheck"
open FsCheck



(*
  i. Declare suitable types "names" and "childDes" to model the above.
     (Hint: a type abbreviation is enough for names, a tagged type is
     suitable for the other)
 *)

type Category =
    | Daycare
    | Nursery
    | Recreation

type names = string

type child = {
  surname: names;
  category: Category
}


(* 
  ii. declare a function 

        number : category * (childDes list) -> int

      which counts the number of children that belong to a certain
      category
 *)

let rec number (cat, list) =
  match list with
  | [] -> 0
  | x :: xs -> 
    if x.category = cat
      then number(cat, xs) + 1
      else number(cat, xs)

let listochild = [{surname = "antonio"; category = Daycare};{surname = "caloggero"; category = Daycare}; {surname = "pippo"; category = Nursery}]

printfn "Number in Daycare: %d" (number (Daycare,listochild))
printfn "Number in Nursery: %d" (number (Nursery,listochild))
printfn "Number in Recreation: %d" (number (Recreation,listochild))

(*
  ii' Write a property that (partially) validates the number function and
      then quickcheck it. For example you could relate the output to the
      lenght of childDes list
 *)

let prop_NumberTest (ls:child list) =
  let nNurs = number (Nursery, ls)
  let nDayc = number (Daycare, ls)
  let nRecr = number (Recreation, ls)

  List.length ls = nNurs + nDayc + nRecr

do Check.Quick prop_NumberTest

(*
  Parents are charged monthly:
    - $225 for Daycare, 
    - $116 for Nursery,
    - $110 for Recreation.

  However large families have a discount: if a family has more than 
  one child attending the day-care, the first child pays full charge, 
  the other siblings half.

  iii. Write a function 

        pay  : name * childDes list -> float

      that computes how much a family named "name" pays given a list of childDes 

        - you can assume that childDes list is sorted according to categories,
          so it lists first Daycare attendees, then Nursery etc 

        - you may want to define a function, say remove, which determines if a
          child of a given surname is in the list; if so, 
          this is the child that pays full price, while his siblings in 
          the rest of the list will pay half.
 *)

let rec pay


let orderedList = [
  {
    surname = "caloggero";
    category = Daycare
  };
  {
    surname = "Pippo";
    category = Daycare
  };
  {
    surname = "Pluto";
    category = Daycare
  };
  {
    surname = "caloggero";
    category = Nursery
  };
  {
    surname = "Pippo";
    category = Nursery
  };
  {
    surname = "caloggero";
    category = Recreation
  };
  {
    surname = "Pluto";
    category = Recreation
  }
]






















