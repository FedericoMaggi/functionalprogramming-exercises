printfn "Esercizio 2 - IVA\n"

(*

i) Definire la funzione

    iva : char -> int

che data una categoria (un char) determina la sua aliquota IVA (un int).

Ad esempio:

iva 'a' ;;
// val it: int = 4

iva 'B' ;;  
// val it: int = 10

iva 'x' ;;
// val it: int = 22

iva 'Z' ;;
// val it: int = 22

*)


let iva = function
  | 'a'|'A' -> 4
  | 'b'|'B' -> 10
  | _       -> 22

printfn "iva a: %d\t|\tiva A: %d" (iva 'a') (iva 'A')
printfn "iva b: %d\t|\tiva B: %d" (iva 'b') (iva 'B')
printfn "iva x: %d\t|\tiva X: %d" (iva 'x') (iva 'X')
printfn "iva z: %d\t|\tiva Z: %d" (iva 'z') (iva 'Z')


(*
	Percent function
*)
let percent fl = (1.0 + fl / 100.0)

(*
ii) Definire la funzione piuIva che calcola il costo con IVA di un prodotto.
Piu' precisamente, la funzione ha tipo

  piuIva :  char * float -> float
   
Data  la coppia (categoria,costoBase), che  specifica la categoria e il costo base di un prodotto,
la fuzione calcola il costo del prodotto IVA inclusa.

Verificare che, se il costo base del prodotto e' 1000 euro e la categoria e' 'A', 'B' o 'C',
il costo con IVA e' calcolato correttamente.

Nelle operazioni aritmetiche, attenzione ai tipi!

*)

let piuIva (cat, basecost) = 
  basecost + ((basecost * (float (iva cat))) / float 100)

printfn "piuIva"
printfn "cat: a, basecost: 1000 => %f" (piuIva ('a',(float 1000)))
printfn "cat: b, basecost: 1000 => %f" (piuIva ('b',(float 1000)))
printfn "cat: z, basecost: 1000 => %f" (piuIva ('z',(float 1000)))

let piuIvaWithPercent (cat, basecost) =
  basecost * percent (iva cat |> float)

printfn "piuIvaWithPercent"
printfn "cat: a, basecost: 1000 => %f" (piuIvaWithPercent ('a',(float 1000)))
printfn "cat: b, basecost: 1000 => %f" (piuIvaWithPercent ('b',(float 1000)))
printfn "cat: z, basecost: 1000 => %f" (piuIvaWithPercent ('z',(float 1000)))


let menoIva (cat, ivacost) =
  ivacost / ((float (100 + (iva cat))/ (float 100)))

printfn "menoIva"
printfn "cat: a, ivacost: 1040 => %f" (menoIva ('a',(float 1040)))
printfn "cat: b, ivacost: 1100 => %f" (menoIva ('b',(float 1100)))
printfn "cat: z, ivacost: 1220 => %f" (menoIva ('z',(float 1220)))


let menoIvaWithPercent (cat, ivacost) =
  ivacost / percent (iva cat |> float)

printfn "menoIvaWithPercent"
printfn "cat: a, ivacost: 1040 => %f" (menoIvaWithPercent ('a',(float 1040)))
printfn "cat: b, ivacost: 1100 => %f" (menoIvaWithPercent ('b',(float 1100)))
printfn "cat: z, ivacost: 1220 => %f" (menoIvaWithPercent ('z',(float 1220)))

let piuIvaWithPiping (cat, basecost) =
  basecost * (cat |> iva |> float |> percent)

let menoIvaWithPiping (cat, ivacost) =
  ivacost / (cat |> iva |> float |> percent)

printfn "piping"
printfn "cat: a, basecost: 1000 => menoIvaWithPiping('a',piuIvaWithPiping('a',1000) => %f" (menoIvaWithPiping('a',piuIvaWithPiping('a',(float 1000))))
printfn "cat: b, basecost: 1000 => menoIvaWithPiping('b',piuIvaWithPiping('b',1000) => %f" (menoIvaWithPiping('b',piuIvaWithPiping('b',(float 1000))))
printfn "cat: z, basecost: 1000 => menoIvaWithPiping('z',piuIvaWithPiping('z',1000) => %f" (menoIvaWithPiping('z',piuIvaWithPiping('z',(float 1000))))
