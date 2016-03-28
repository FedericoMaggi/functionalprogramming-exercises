printfn "Esercizio 3 - Ricorsione"

(* Fattoriale *)
printfn "\n> Factorial"

let rec fact x =
  match x with
  | 0 -> 1
  | x -> x * fact (x-1)

printfn "fact 0: %d" (fact 0)
printfn "fact 1: %d" (fact 1)
printfn "fact 2: %d" (fact 2)
printfn "fact 3: %d" (fact 3)
printfn "fact 4: %d" (fact 4)
printfn "fact 5: %d" (fact 5)


(* Esponenziale *)
printfn "\n> Exponential"

let rec power (b, e) =
  match e with
  | 0 -> 1
  | _ -> b * power(b, e-1)

printfn "power 1^2: %d" (power (1,2))
printfn "power 2^2: %d" (power (2,2))
printfn "power 2^3: %d" (power (2,3))
printfn "power 3^4: %d" (power (3,4))


(* Generatore di stringhe numeriche *)
printfn "\n> Numeric string generator"

let rec makestr = function
  | 0 -> "0"
  | n -> makestr (n - 1) + " " + (string n)

printfn "makestr 0: [%s]" (makestr 0)
printfn "makestr 1: [%s]" (makestr 1)
printfn "makestr 2: [%s]" (makestr 2)
printfn "makestr 5: [%s]" (makestr 5)
printfn "makestr 10: [%s]" (makestr 10)


(* Massimo Comun Divisore *)
printfn "\n> MCD"

let rec mcd a b = 
  match b with
  | 0 -> a
  | _ -> mcd b (a % b)

printfn "MCD(1,1): %d" (mcd 1 1)
printfn "MCD(2,2): %d" (mcd 2 2)
printfn "MCD(2,3): %d" (mcd 2 3)
printfn "MCD(9,4): %d" (mcd 9 4)
printfn "MCD(24,18): %d" (mcd 24 18)


(* Semplificazione di frazioni *)
printfn "\n> simplify"

let simplify (a,b) = 
  let x = mcd a b
  if not (x = a && x = b)
    then ((a/x) , (b/x))
  else (a,b)

printfn "simplify(15,9): %d %d" <|| (simplify (15,9))
printfn "simplify(30,7): %d %d" <|| (simplify (30,7))
printfn "simplify(35,7): %d %d" <|| (simplify (35,7))


(* Sommatoria *)
printfn "\n> Sum from 0"

let rec sum1 a =
  match a with
  | 0 -> 0
  | a -> a + sum1 (a-1)

printfn "sum1(1) : %d" (sum1 1)
printfn "sum1(2) : %d" (sum1 2)
printfn "sum1(4) : %d" (sum1 4)
printfn "sum1(5) : %d" (sum1 5)


(* Sommatoria tra numeri compresi *)
printfn "\n> Sum between"

let rec sum2 a b =
  if not (a > 0 && b > 0 && a <= b) 
    then 0
  else b + (sum2 a (b-1))

printfn "sum2 2 5: %d" (sum2 2 5)
printfn "sum2 3 4: %d" (sum2 3 4)
printfn "sum2 2 15: %d" (sum2 2 15)


(* Esponenziale ottimizzato *)
printfn "\n> Optimised exponential"

let rec optpower (b, e) =
  match e with
  | 1 -> b
  | _ -> if ((e % 2) = 0) 
          then optpower ((b*b), (e/2))
          else b * optpower ((b*b), (e/2))

printfn "optpower 1^2: %d" (optpower (1,2))
printfn "optpower 2^2: %d" (optpower (2,2))
printfn "optpower 2^3: %d" (optpower (2,3))
printfn "optpower 3^4: %d" (optpower (3,4))


(* Fibonacci *)
printfn "\n> Fibonacci"

let rec fib n = 
  match n with
  | 0|1 -> 1
  | n -> fib(n-2) + fib(n-1)

printfn "fibonacci(2): %d" (fib 2)
printfn "fibonacci(3): %d" (fib 3)
printfn "fibonacci(5): %d" (fib 5)
printfn "fibonacci(8): %d" (fib 8)
printfn "fibonacci(10): %d" (fib 10)
