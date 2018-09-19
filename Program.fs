let ran = new System.Random(12345)

let genlist N = 
  let L = List.init N (fun i -> ran.Next(512000))
  L

//
// printfn a list: if the list is long, we print first 3 ... last 3:
//
let doprintFirst3 L = 
  printf "%A; %A; %A;" (List.item 0 L) (List.item 1 L) (List.item 2 L)
  
let doprintLast3 L = 
  let len = List.length L
  printf "%A; %A; %A" (List.item (len-3) L) (List.item (len-2) L) (List.item (len-1) L)

let printfnList msg L = 
  printf msg
  if (List.length L) <= 10 then
    printfn "%A" L
  else
    printf "["
    doprintFirst3 L
    printf " ...; "
    doprintLast3 L
    printfn "]"


//
// merge two sorted lists into one:
//

let rec _merge L1 L2 = 
  match L1, L2 with
  | [], [] -> []
  | [], _  -> L2
  | _,  [] -> L1
  | hd1::tl1, hd2::tl2 ->
    if hd1 <= hd2 then
      hd1 :: _merge tl1 L2
    else
      hd2 :: _merge L1 tl2


let rec merge L1 L2 acc = 
 if L1 = [] && L2 = [] then acc
 elif L1 = [] then List.rev (acc) @ (L2)
 elif L2 = [] then List.rev (acc) @ (L1)
 elif List.head (L1) <= List.head (L2) then merge(List.tail L1)(L2)(List.head (L1) :: acc) 
 else merge(L1)(List.tail L2)(List.head (L2) :: acc)


//
// mergesort:
//
let rec mergesort L = 
  match L with
  | []    -> []
  | e::[] -> L
  | _     -> 
    let mid = List.length L / 2
    let (L1, L2) = List.splitAt mid L
    merge (mergesort L1) (mergesort L2)([])


//
// Input list size N:
//
let N = System.Convert.ToInt32(System.Console.ReadLine())
let L = genlist N
printfnList "L: " L
//
let R = mergesort L
printfnList "Sorted: " R

