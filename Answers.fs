module Answers

let print x = printfn "%A" x;x

(*
Q1. How many values does the q1Type type have? int8 is an 8 bit numeric type
(Q1 is a function that must return the answer)
*)
let q1() : int = 1


(*
Q2. Property-based testing is - return the most correct answer:
0 = better than unit testing
1 = more complete than unit testing
2 = more able to find unexpected corner cases than unit testing
(Q2 is a function that must return the correct answer)
*)
let q2() : int = 2



(*
Q3. The Option type is preferable to using Null pointers because
A = It has a better implementation
B = It provides type protection
C = It documents program behaviour
0 = None of above
1 = A
2 = B
3 = C
4 = A & B
5 = A & C
6 = B & C
7 = A & B & C
(Q3 is a function that must return the correct answer)
*)
let q3 (lst: int list) : int list =
    lst
    |>List.collect (fun i-> [i;i])



(*
Q4. Write a function (see below for signature) that returns a list whose nth
element is the nth element of lst added on the head of the nth element of lsts.
You may assume that lst and lsts have the same length.
Do not used indexes or List.item.
*)
let q4 (lsts: int list list) : int =
    lsts
    |> List.collect (fun i -> i)
    |> List.reduce (+)

(*
Q6. Write a function that takes two ordered integer lists (ordered by increasing values),
    and return one list, which contains the values of both lists, and is also ordered by increasing values.
    You may only use the List.rev function from the list library, which takes a list and reverses its order.
    You may not use array functions.
    You should consider using a recursive function.
*)
let q5 (lst: int list): int=
    let sorting =
        lst
        |> List.groupBy id
        |> List.map (fun (x,y)-> (x,List.length y))
        |> List.sortByDescending snd
    match sorting with
    | (id, cnt)::_ ->
        let modes = List.takeWhile (fun (x,cnt') -> cnt' = cnt) sorting
        List.map (fun (i,j)-> abs(i)) modes
        |>List.sort
        |> List.head
    | [] -> failwithf "Keep the compiler smiling"

(*
Q7 return the number of sets in setOfSets that el is an element of
*)
let q6 (lst: int list): int list =
    match lst.Length with
    | n when n % 2 = 0 ->
        List.init (n/2) (fun i -> lst.[2*i] * lst.[2*i+1])
    | n ->
        lst.[0..n-2] @ [lst.[n-1] * lst.[n-1]]
