printfn "Esercizio 4 - Liste"

(* Remove even *)
printfn "\n>Remove even"

let rec rmEven list =
  match list with
  | [] -> []
  | i :: ls -> if i % 2 = 0 
                then rmEven ls
                else i :: rmEven ls

let fullList = [1 .. 10]
printfn "fullList: %A" fullList

let oddList = rmEven fullList
printfn "oddNumberList: %A" oddList

(* Remove odd position *)
printfn "\n>Remove odd position"

let rec rmOddPosition list = 
  match list with
  | [] -> []
  | j :: i :: ls -> j :: rmOddPosition ls
  | i :: ls -> rmOddPosition ls

let notOddList = rmOddPosition fullList
printfn "notOddList: %A" notOddList

(* Split *)
printfn "\n>Split list"

let rec splitter (ls, even, odd) = 
  match ls with
  | [] -> even,odd
  | j :: i :: l -> splitter (l, j::even, i::odd)
  | i :: l -> splitter (l, even, i::odd)

let split list = 
  splitter (list, [], [])  

let (l1,l2) = split fullList
printfn "l1: %A, l2: %A" l1 l2

(* Length test *)
printfn "\n> Length test"

let rec lengthTest (l1,l2) =
  match (l1,l2) with
  | ([],[]) -> 0
  | ([], b) -> -1
  | ( a,[]) -> 1
  | ( a::xa, b::xb) -> lengthTest(xa,xb)


let list1 = [1]
let list2 = [2;3]
let list3 = [3;4;5]
let list4 = [4;5]
let list5 = [5]


printfn "l1 | l2: %d" (lengthTest (list1, list2)) 
printfn "l1 | l3: %d" (lengthTest (list1, list3)) 
printfn "l1 | l5: %d" (lengthTest (list1, list5)) 
printfn "l4 | l2: %d" (lengthTest (list2, list4)) 
printfn "l3 | l1: %d" (lengthTest (list3, list1))
printfn "[] | []: %d" (lengthTest ([],[]))


(* Remove *)
printfn "\n> Remove element"

let rec rmElement(a,list) =
  match list with
  | [] -> []
  | x :: ls -> if not (x = a)
                then x :: rmElement(a,ls)
                else rmElement(a,ls)

let listToRemove  = [1 .. 10]
let listToRemove2 = [1;3;4;4;5;10]

printfn "remove 1:\t%A" (rmElement (1,listToRemove))
printfn "remove 2:\t%A" (rmElement (2,listToRemove))
printfn "remove 5:\t%A" (rmElement (5,listToRemove))
printfn "remove 10:\t%A" (rmElement (10,listToRemove))
printfn "remove 15:\t%A" (rmElement (15,listToRemove))
printfn "remove 4 from listToRemove2: %A" (rmElement (4,listToRemove2))

let fruits = ["apples";"bananas";"oranges";"apples";"bananas";"kiwi";"strawberry"]
printfn "remove bananas from fruits (i don't like bananas): %A" (rmElement("bananas",fruits))

(* Remove duplicates * TODO
printfn "\n> Remove duplicates"

let rec rmDuplicates list =
  match list with
  | [] -> []
  | x::ls -> if 

*)

(* Down and Up *)
printfn "\n> Down & Up"

let rec downToZero n =
  match n with
  | 0 -> [0]
  | x -> n :: (downToZero (n-1))

printfn "downToZero10: %A" (downToZero 10)
printfn "downToZero5: %A" (downToZero 5)

let rec upFromZero n =
  match n with
  | 0 -> [0]
  | x -> (upFromZero (n-1)) @ [x]

printfn "upTo10: %A" (upFromZero 10)
printfn "upTo5: %A" (upFromZero 5)

