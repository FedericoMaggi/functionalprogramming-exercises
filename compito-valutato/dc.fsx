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

let cost c = 
  match c with
  | Daycare -> 225
  | Nursery -> 116
  | Recreation -> 110

let pay (fname, clist) =
  
  let rec remove (fname, clist) =
    match clist with
    | [] -> None
    | x :: xs -> 
      if x.surname = fname
        then Some x
        else remove (fname, xs)

  let rec calcCost (mchild, clist) =
    match clist with
    | [] -> 0
    | x :: xs -> 
      if x.surname = mchild.surname
        then 
          if x.category = mchild.category
          then ((cost x.category) + calcCost (mchild, xs))
          else (((cost x.category)/2) + calcCost (mchild, xs))
        else calcCost (mchild, xs)

  let fullPayer = remove (fname, clist)

  calcCost (fullPayer.Value, clist)

(*
  iv. Write some tests and run your program.
 *)















