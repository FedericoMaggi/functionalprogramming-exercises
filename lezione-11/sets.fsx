(* COMPUTING WITH RELATIONS (via SETS)
  We see a relation from a set A to a set B as a subset of A * B. 
  A relation r is called finite if it is a finite subset
  of A * B. Assuming that the sets A and B are represented by F# types
  ’a and ’b we can represent a finite relation r by a value of type
  Set<’a * ’b>.
*)
type rel<'a,'b when 'a: comparison and 'b : comparison> = 
  Set<'a * 'b>;;

(*
  i.
  Give as a **concrete** example the "less" relation over the set
  {1,2,3}. Do not write the function, just list the elements as a set.
*)

let theRelation = Set.ofList [(1,2);(1,3);(2,3);(5,10)]

printfn "theRelation: %A" theRelation

(*
  ii. 
  The *domain* dom r of a relation r from A in B is the set of elements a in A
  where there exists an element b in B such that (a, b) ∈ r. Write an F#
  declaration expressing the domain function.  

  The *range* r of a relation r is the set of elements b in B where
  there exists an element a in A such that (a, b) ∈ r. Write an F#
  declaration expressing the range function.
*)

let dom (R: rel<'a,'b>) = Set.map (fun (x,y) -> x) R

printfn "domain: %A" (dom theRelation)

let range (R: rel<'a,'b>) = Set.map (fun (x,y) -> y) R

printfn "range: %A" (range theRelation)

(*
  iii. 
  The identity relation on a set S is the relation associating every s ∈ S 
  to itself.  
  Write an F# declaration computing the identity relation from a given S.
*)

let identity s = Set.map (fun (x) -> (x,x)) s // Receive a SET and create an identity relation

printfn "identity for set {'a';'b';'c'}: %A" (Set.ofList ['a';'b';'c'] |> identity)

(*
  iv.
  A relation r from a set A to the same set is said to be symmetric
  if (a1 , a2 ) ∈ r implies (a2 , a1 ) ∈ r for any elements a1 and a2 in A.
  
  The symmetric closure of a relation r is the smallest symmetric
  relation containing r. Declare an F# function to compute the symmetric
  closure.
*)

let symmetric R = Set.map (fun (x,y) -> (y,x)) R

printfn "symmetric of theRelation: %A" (symmetric theRelation)

let symmetricClosure R = symmetric R |> Set.union R

printfn "symmetricClosure: %A" (symmetricClosure theRelation)

(*
  v. 
  If r is a finite relation from A to B and a is an element of A , 
  then the application of r to a , apply r a, is the set 
  of elements b in B such that (a, b) ∈ r . 
  Write an F# declaration expressing the apply function.
*)

let apply R a = Set.filter (fun (x,y) -> x = a) R |> range // Applies the relation on the element a from dom

printfn "Apply theRelation on 1: %A" (apply theRelation 1)
printfn "Apply theSymmetricRelation on 1: %A" (apply (symmetric theRelation) 3)

(*
  5. 
  The relation composition r ◦◦ s of a relation r from a set A to a set B 
  and a relation s from B to a set C is a relation from A to C . 

  It is defined as the set of pairs (a, c) where there exist
  an element b in B such that (a, b) ∈ r and (b, c) ∈ s . 

  Declare an F# function to compute the relational composition.
*)

let composition R1 R2 =
  

(*
  6. 
  A relation r from a set A to the same set A is said to be transitive if (a 1 , a 2 ) ∈ r and
(a 2 , a 3 ) ∈ r implies (a 1 , a 3 ) ∈ r for any elements a 1 , a 2 and a 3 in A . The transitive closure
of a relation r is the smallest transitive relation containing r . If r contains n elements, then
the transitive closure can be computed as the union of the following n relations:
r ∪ (r ◦◦ r) ∪ (r ◦◦ r ◦◦ r) ∪ · · · ∪ (r ◦◦ r
*)




