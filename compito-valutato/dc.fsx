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

let prop_costFunctionTest (cat:Category) =
  if cat = Daycare
    then 225 = cost cat
    else 
      if cat = Nursery
        then 116 = cost cat
        else 
          if cat = Recreation
            then  110 = cost cat
            else
              false

do Check.Quick prop_costFunctionTest

(* Examples *)
let daycareArchive = [
  {
    surname = "socrate";
    category = Daycare
  };
  {
    surname = "anassimene";
    category = Daycare
  };
  {
    surname = "talete";
    category = Daycare
  };
  {
    surname = "anassimandro";
    category = Daycare
  };
  {
    surname = "platone";
    category = Nursery
  };
  {
    surname = "aristotele";
    category = Nursery
  };
  {
    surname = "plauto";
    category = Nursery
  };
  {
    surname = "socrate";
    category = Recreation
  };
  {
    surname = "talete";
    category = Recreation
  };
  {
    surname = "aristotele";
    category = Recreation
  };
  {
    surname = "virgilio";
    category = Recreation
  };
  {
    surname = "platone";
    category = Recreation
  }
]

printfn "number for Daycare: %d" ( number (Daycare, daycareArchive))
printfn "number for Nursery: %d" ( number (Nursery, daycareArchive))
printfn "number for Recreation: %d" ( number (Recreation, daycareArchive))

printfn "pay aristotele: %d" (pay ("aristotele",daycareArchive))
printfn "pay anassimene: %d" (pay ("anassimene",daycareArchive))
printfn "pay socrate: %d" (pay ("socrate",daycareArchive))
printfn "pay talete: %d" (pay ("talete",daycareArchive))
printfn "pay plauto: %d" (pay ("plauto",daycareArchive))
printfn "pay platone: %d" (pay ("platone",daycareArchive))
