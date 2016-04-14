#r "FsCheck"
open FsCheck

// Standard Binary Tree
type 'a binTree =   // '
  | Leaf        // empty tree
  | Node of 'a * 'a binTree * 'a binTree;; // ' // Node (root, left, right)

let t1 = Node (2, Leaf, Leaf)
let t2 = Node (2, Leaf, Node (4, Leaf, Leaf))
let t7 = Node (7, Leaf, Node (10, Leaf, Node (13, Leaf, Leaf)))
let t8 = Node (8, Node (11, Leaf, Leaf), Leaf)
let t5 = Node (5, t7, t8)


(* Int To Float *)
printfn "tree: %A" t5

// Int -> Float recursive
let rec intToFloatTree btree =
  match btree with
  | Leaf -> Leaf
  | Node (r, left, right) ->
      Node (float r, intToFloatTree left, intToFloatTree right)

printfn "float Tree: %A" (intToFloatTree t5)

// Int -> Float HO
let rec mapTree f btree=
  match btree with
  | Leaf -> Leaf
  | Node (r, left, right) ->
      Node (f r, mapTree f left, mapTree f right)

printfn "float Tree (HO): %A" (mapTree float t5)

let prop_intToFloat (ts: int binTree) =
  intToFloatTree ts = mapTree float ts

do Check.Quick prop_intToFloat

(* Search *)
let rec search (x, btree)=
  match btree with
  | Leaf -> false
  | Node (r, left, right) -> 
      if x = r 
        then true
        else search (x, left) || search (x, right)


printfn "search 7 in t5: %b" (search (7,t5))
printfn "search 17 in t5: %b" (search (17,t5))

(* Exists HO -> applies p on the whole btree *)
let rec existsTree p btree =
  match btree with
  | Leaf -> false
  | Node (r, left, right) ->
    p r || existsTree p left || existsTree p right

let prop_existsTree x (ts:int binTree) =
  search (x,ts) = existsTree (fun n -> x = n) ts

do Check.Quick prop_existsTree

(* Filter (inorder) *)
let rec filterToList pred btree =
  match btree with
  | Leaf -> []
  | Node (r, left, right) ->
    let filtLeft = filterToList pred left
    let filtRight= filterToList pred right

    if pred r 
      then filtLeft @ [r] @ filtRight
      else filtLeft @ filtRight

printfn "evenfilter: %A" (filterToList (fun x -> x % 2 = 0) t5) 


let rec filterTreeOpt pred btree =
  match btree with
  | Leaf -> Leaf
  | Node (r, left, right) ->
    let filtLeft = filterTreeOpt pred left
    let filtRight = filterTreeOpt pred right

    if pred r
      then Node (Some r, filtLeft, filtRight)
      else Node (None, filtLeft, filtRight)

printfn "evenfilterOpt: %A" (filterTreeOpt (fun x -> x % 2 = 0) t5)
printfn "gtzero: %A" (filterTreeOpt (fun x -> x > 5) t5)

(* Sum, Count, Depth *)

let rec sum btree =
  match btree with 
  | Leaf -> 0
  | Node (r, left, right) ->
      r + sum left + sum right

let rec count btree =
  match btree with
  | Leaf -> 0
  | Node (r, left, right) ->
      1 + count left + count right

let rec depth btree = 
  match btree with
  | Leaf -> 0
  | Node (r, left, right) ->
      1 + (max (depth left) (depth right)) 

// the FOLD combinator  for trees
let rec fold_tree f_node f_leaf tree = 
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) -> f_node x (fold_tree f_node f_leaf left) (fold_tree f_node f_leaf right)

// sum of elements
let sumbtf = fold_tree (fun x l r -> x + l + r) 0 

// # of elements
let countf ts = fold_tree (fun _ l r -> 1 + l + r) 0 ts

let c = countf t2

// depth of tree
let depthtf ts = fold_tree (fun _ l r ->  1 + (max l  r))  0 ts

let d = depthtf t2

// inorder traversal

let inOrder tr = fold_tree (fun x l r -> l @ [x] @ r) []  tr

// defining filterOpt
let filterTreeOptF p tr =
  fold_tree (fun x l r -> if p x then Node(Some x,l,r) else Node(None,l,r) ) Leaf tr 















