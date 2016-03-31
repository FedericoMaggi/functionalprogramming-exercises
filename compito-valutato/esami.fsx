(* Esercizio 1 - Esami.fsx *)

#r "FsCheck"
open FsCheck

(* 

  Una valutazione numerica e' una record con campi studente e voto
  e una valutazione a giudizio è una record  con campi studenti e
  giudizio, dove:

    - studente e giudizio sono stringhe;
    - voto un intero.
 *)

(*
  0) Dare delle type definitions dei record
 *)

type valV = {
  nome: string;
  voto: int
}

type valG = {
  studente: string;
  giudizio: string
}

(*
  1) Definire una funzione

     valuta :  valV  ->   valG

  che, dato il nome di uno studente e il voto, restituisce il record con
  campi studente e giudizio, dove il giudizio e' definita secondo la seguente tabella:
   
   meno di 18   --->        insufficiente   
   da 18 a 22   --->        sufficiente  
   da 23 a 27   --->        buono 
   piu' di 27   --->        ottimo
 *)

let valuta e = 
  match e.voto with
  | k when k < 18 -> {studente = e.nome; giudizio = "insufficiente"}
  | k when k >= 18 && k < 22 -> {studente = e.nome; giudizio = "sufficiente"}
  | k when k >= 22 && k < 27 -> {studente = e.nome; giudizio = "buono"}
  | k when k >= 27 -> {studente = e.nome; giudizio = "ottimo"}
  | _ -> {studente = e.nome; giudizio = "insufficiente"}

(* esempio *)
let bianchi = { nome = "bianchi"; voto = 28 }
printfn "Bianchi: %A" (valuta bianchi)

(*

  2)  Definire la funzione

      valutaList : valV list -> valG list

  Che trasforma una lista di valutazioni numeriche
  in una lista di valutazioni a giudizio

  In questo caso si richiede di *NON* usare le funzioni nella
  collection List. 

  Si usi nel testing questa property:

  let ``valutaList è una map di valuta`` (xs : valV list)  =
    List.map valuta xs = valutaList xs;;
  do Check.Quick  ``valutaList è una map di valuta``

 *)

let rec valutaList list =
  match list with
  | [] -> []
  | x :: xs -> (valuta x) :: (valutaList xs)


let ``valutaList è una map di valuta`` (xs : valV list)  =
  List.map valuta xs = valutaList xs;;
do Check.Quick  ``valutaList è una map di valuta``

(*

  3)  Definire  la funzione

    creaValList : (string list * int list) -> valV list

  che, data una lista di studenti e una lista di voti, crea la lista dei record
  delle valutazioni (studente,voto).  Se le liste non hanno la stessa
  lunghezza, la parte in eccedenza non viene considerata.

 *)

let rec creaValList (names, grads) =
  match names,grads with
  | ([],[]) -> []
  | (_, []) -> []
  | ([], _) -> []
  | (n :: nl, v :: vl) -> {nome = n; voto = v} :: creaValList(nl, vl)

(*
  4) Definire la funzione

    media : valV list -> float

  che data una lista di record di valutazioni calcola la media dei voti.

  La funzione può usare la definizione (possibilmente locale, cioè al
  suo interno) di una funzione 

    sommaAndConta : valV list  -> int * int

  che, data una lista vList di record di valutazione studente e voto, calcola
  **simultaneamente** (cioe' in una unica passata) la coppia

    (sommaVoti,count)

  dove sommaVoti e' la somma dei voti in vList e count il numero di
  valutazioni, cioe' la lunghezza in vList.
 *)

let media list =
  let rec sommaAndConta ll =
    match ll with
    | [] -> (0,0)
    | x :: xs -> 
        let (voti,n) = sommaAndConta (xs) 
        (voti + x.voto, n + 1)

  let (sum,count) = sommaAndConta list
  printfn "sum: %d | count: %d" sum count

  float ((float sum) / (float count))


(*
  5) Definire la funzione 

    separa : valV list -> valV list * valV list

  che, data una lista di valutazioni numeriche, crea due liste di
  valutazioni numeriche, la lista dei bocciati (voto < 18) e la lista dei promossi
  (voto>= 18).

    - Definire una proprietà FsCheck che controlla che i record in
      questione sono preservati da "separa". Hint: usare List.sort

  let ``due liste risultato hanno stessi elementi di vs`` () =
  ...

  do Check.Quick ``due liste risultato hanno stessi elementi di vs`` 
 *)

let separa list =
  let rec sepcore (list, bocciati, promossi) =
    match list with
    | [] -> (bocciati,promossi)
    | x :: xs -> if x.voto < 18 
                  then sepcore (xs, {nome= x.nome; voto= x.voto} :: bocciati, promossi)
                  else sepcore (xs, bocciati, {nome= x.nome; voto= x.voto} :: promossi)

  sepcore (list, [], [])

let ``due liste risultato hanno stessi elementi di vs`` (xs : valV list)  =
  let (b,p) = separa xs
  List.length xs = List.length p + List.length b

do Check.Quick ``due liste risultato hanno stessi elementi di vs`` 














