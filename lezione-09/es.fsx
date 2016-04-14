
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

let filter1 pred ls =
  
  let rec filt (pred, ls, ok, ko) =
    match ls with
    | [] -> ok, ko
    | x :: xs -> 
      if pred x 
        then (filt (pred, xs, ok @ [x], ko))
        else (filt (pred, xs, ok, ko @ [x]))

  filt (pred, ls, [], [])

(*
  4.2) Usando filter1 e le definizioni nell'esercizio precedente, costruire le coppie di liste

    p1 =  ( [3; 6; 9; 12; 15; 18] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16; 17; 19; 20] )
     //  ( multipli di 3 , non-multipli di 3 ) 
*)

let mult3_bis n =
  filter1 (fun x -> x % 3 = 0) [1 .. n]

printfn "%A" (mult3_bis 20)

(*
  4.3) Usando filter1, definire la funzione
    
    multNonmult : int -> int list * int list

  che, dato un intero n, partiziona la lista [1 .. n] 
  nella coppia di liste  

      ( multipli di 3 , non-multipli di 3 ) 

  Ad esempio:

     multNonmult 16 =   ( [3; 6; 9; 12; 15] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16] )
*)

let multNonmult n =
  filter1 (fun x -> x % 3 = 0) [1 .. n]

(* QUICKCHECK *)
(*
  i) prop_filter1_len pred (ls : int list)

    Sia  (xs,ys) il risultato di 'filter1 pred ls'.
    Allora concatenando xs e ys si ottiene una lista avente la stessa lunghezza di ls.
*)

let prop_filter1_len pred ls =
  let xs, ys = filter1 pred ls

  List.length ls = (List.length xs) + (List.length ys)

do Check.Quick prop_filter1_len

(*
  ii) t prop_filter1_app pred (ls :int list)

    Sia  (xs,ys) il risultato di 'filter1 pred ls'.
    Allora, concatenando xs e ys si ottiene una lista avente gli stessi elementi di ls.
*)


(* Esercizio 5 *)
(*
  Definire la funzione 

       divisori : int -> int list

  che, dato un intero n > 0, restituisce la lista dei suoi divisori
  (usare opportunamente la funzione filter).
*)

let divisori n = 
  filter (fun x -> n % x = 0) [1 .. n]

printfn "divisori di 10: %A" (divisori 10)
printfn "divisori di 1: %A" (divisori 1)
printfn "divisori di 3: %A" (divisori 3)

(*
  Usando la funzione divisori, definire la funzione isPrime che determina se un intero  e' primo.
  Notare che e' sufficiente scrivere una espressione booleana.
*)

let isPrime n =
  List.length(divisori n) <= 2

printfn "prime 2: %b" (isPrime 2)
printfn "prime 3: %b" (isPrime 3)
printfn "prime 10: %b" (isPrime 10)
printfn "prime 15: %b" (isPrime 15)
printfn "prime 17: %b" (isPrime 17)
printfn "prime 19: %b" (isPrime 19)

