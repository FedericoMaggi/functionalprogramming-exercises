
#r "FsCheck"
open FsCheck

(* Esercizio 1 *)
(*
  Consideriamo le definizioni

  let f = fun x -> x + 1 ;;
  let g = fun x -> x  +1 ;;

  Le funzioni f e g sono uguali?
  Cosa calcola g?

  Dare qualche esempio di termine t tale per cui l'applicazione

    g t

  ha senso.
*)

let f = fun x -> x + 1
let g = fun x -> x  +1 (* g: int -> 'a *)

printfn "f 5: %d" (f 5)
printfn "g 5: %d" (g f)

(* Esercizio 2 *)
(*
  2.1) Definire la funzione ricorsiva map tale che, data una funzione f e una lista ls aventi tipo

    f : 'a -> 'b      ls : 'a list

    il valore di
 
      map f ls

    è la lista di tipo 'b list ottenuta applicando a ogni elemento x di ls la funzione f.

    Quindi:

      map  f [ x0 ; x1 ; .... ; xn ]   =   [ f x0 ; f x1 ; ... ; f xn ]

    Il tipo di map e':

      map : ('a -> 'b) -> 'a list -> 'b list

    Notare che map e' una funzione higher-order.
*)

let rec map f ls = 
  match ls with
  | [] -> []
  | x :: xs -> (f x) :: (map f xs)

let theList = [1;2;3;4;5]

let compList = map (fun x -> x + 1) theList

printfn "compList: %A" compList

(*
  2.2) Sia l1 la lista contenente i numeri da 1 a 10.

      Applicando map a una opportuna funzione f e alla lista l1 costruire le seguenti liste
      (in entrambi i casi, scrivere la funzione f come funzione anonima):

      l2 =  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100] // quadrati dei primi 10 numeri

      l3 = [(1, "dispari"); (2, "pari"); (3, "dispari"); (4, "pari"); (5, "dispari"); (6, "pari"); 
            (7, "dispari"); (8, "pari"); (9, "dispari"); (10, "pari")]
*)

let l1 = [1 .. 10]

printfn "quadrati: %A" (map (fun x -> x * x) l1)

let isEvenOdd x =
  match x % 2 with
  | 0 -> "pari"
  | _ -> "dispari"

printfn "dispari/pari: %A" (map (fun x -> (x, isEvenOdd x)) l1)

(*
  2.3) Consideriamo la lista

      let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

    Applicando map a una opportuna funzione e alla lista names, costruire la lista
   
      names1 =  ["Dott. Mario Rossi"; "Dott. Anna Maria Verdi"; "Dott. Giuseppe Di Gennaro"]
*)

let names = [("Mario","Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")]

let dottList = map (fun (x,y) -> "Dott. " + x + " " + y ) names

printfn "%A" dottList


(* QUICKCHECK *)
(*
  i) prop_map f (ls : int list)

    map e List.map calcolano gli stessi valori
    (piu' precisamente, le applicazioni 'map f ls' e 'List.map f ls' producono la stessa lista).
*)

let prop_map f ls =
  map f ls = List.map f ls

do Check.Quick prop_map
(*
  ii) prop_map_pres_len f (ls :int list)

    La lista  'map f ls' ha la stessa lunghezza di ls
    (per calcolare la lunghezza di una lista si puo' usare List.length) 
*)

let prop_map_pres_len f ls = 
  (List.length (map f ls)) = (List.length ls)

do Check.Quick prop_map_pres_len

(* Esercizio 3 *)
(*
  3.1) Definire la funzione ricorsiva filter tale che, 
       data una funzione pred (predicato)  e una lista ls  aventi tipo

      pred : 'a -> bool     ls :  'a list

  il  valore di 
      filter pred ls
     
  è la lista di tipo 'a list contenente gli elementi di ls che verificano pred.
  La lista risultante contiene quindi gli elementi x di ls tali che pred x e' true  
  (pred funge da filtro).

  Il tipo di filter e':

     filter: ('a -> bool) -> 'a list -> 'a list

  ed e'  una funzione higher-order
*)

let rec filter pred ls =
  match ls with
  | [] -> []
  | x :: xs -> 
    if pred x 
      then x :: (filter pred xs)
      else filter pred xs

(*
  3.2) Usando fiter, definire la funzione

    mult3 : int  -> int list

  che costruisce la lista dei multipli di 3 compresi fra 1 e n
  (applicare in modo opportuno filter sulla lista [1 .. n]).
*)

let mult3 n =
  filter (fun x -> x % 3 = 0) [1 .. n]

printfn "%A" (mult3 15)

(* QUICKCHECK *)

(*
  i) prop_filter pred (ls : int list) 

    filter e List.filter calcolano gli stessi valori.
*)

let prop_filter pred ls =
  (filter pred ls) = (List.filter pred ls)

do Check.Quick prop_filter

(*
  ii)  prop_filter_len pred (ls :int list)

    La lista   'filter pred ls' non puo' essere piu' lunga della lista ls.
*)

let prop_filter_len pred ls =
  (List.length (filter pred ls)) <= (List.length ls)

do Check.Quick prop_filter_len

(* Esercizio 4 *)
(*
  4.1) Definire la funzione ricorsiva filter1 analoga a filter in cui pero'

     filter1 pred ls = ( lsTrue, lsFalse )

    dove:

      - lsTrue    contiene gli elementi di ls che verificano pred
      - lsFalse   contiene gli elementi di ls che non verificano pred

    Il tipo di filter1 e':

      ('a -> bool) -> 'a list -> 'a list * 'a list
*)






















