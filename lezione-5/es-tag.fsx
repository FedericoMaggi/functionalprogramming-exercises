printfn "Esercizio 4 - Tag & Union"

type figura =
  | Rettangolo  of float * float
  | Quadrato    of float
  | Triangolo   of float * float


printfn "\n> Area figura"

let areaOpt fig =
  match fig with
  | Rettangolo(b,h) -> 
    if (b < 0. || h < 0.) 
      then None
      else Some (b * h)
  | Quadrato l -> 
    if l < 0. 
      then None
      else Some (l * l)
  | Triangolo(b,h) ->
    if (b < 0. || h < 0.)
      then None
      else Some ((b * h) / 2.)

let printArea fig =
  match (areaOpt fig) with
  | None -> "Error"
  | Some a -> "Area: " + string(a)

printfn "Area Quadrato(5): %A" (areaOpt (Quadrato 5.))
printfn "Quadrato: %s" (printArea (Quadrato 5.))
printfn "Area Quadrato(-5): %A" (areaOpt (Quadrato -5.))
printfn "Quadrato: %s" (printArea (Quadrato -5.))

printfn "Area Rettangolo(5,2): %A" (areaOpt (Rettangolo (5.,2.)))
printfn "Rettangolo: %s" (printArea (Rettangolo (5.,2.)))
printfn "Area Rettangolo(-5,2): %A" (areaOpt (Rettangolo (-5.,2.)))
printfn "Rettangolo: %s" (printArea (Rettangolo (-5.,2.)))

printfn "Area Triangolo(5,2): %A" (areaOpt (Triangolo (5.,2.)))
printfn "Triangolo: %s" (printArea (Triangolo (5.,2.)))
printfn "Area Triangolo(5,-2): %A" (areaOpt (Triangolo (5.,-2.)))
printfn "Triangolo: %s" (printArea (Triangolo (-5.,2.)))


(* Somma area *
printfn "\n> Somma area"

let sommaAree (fig1, fig2) =
  match (fig1, fig2) with
  | (Some f1, Some f2) ->
      ((areaOpt f1) + (areaOpt f2))

*)
