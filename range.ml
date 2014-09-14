(*let rec range a b =
  if a > b 
  then []
  else a :: range (a + 1) b;;

let rec range1 a b acc = 
  if a = b 
  then acc
  else range1(a+1) b (a::acc);;

let sum xs = List.fold_left (+) 0 xs;;


let range2 a b = 
  let rec range1 a b acc = 
    if a > b 
    then acc
    else range1 a  (b - 1) (b::acc) 
  in range1 a  b [];;
 
let int_range a b =
  let rec int_range_rec l a b =
    if a > b then l
    else int_range_rec (b :: l) a (b - 1)
  in (int_range_rec [] a b);;

range2 1 3;;
  *)

let range ?a:(a=0) b =
 
  let rec range1 a b acc =
    if a > b 
    then acc
    else range1 a (b - 1) (b::acc) in
  let rec range2 a b acc = 
    if a > b
    then acc
    else range2 (a + 1)  b (a::acc) in
  let g = b - 1 in 
   if  a <  b then range1 a  g [] else range2 b (a - 1) []
   
  
    ;;

let a = List.map ~f:(fun x -> x + 1);;

type args = 
  |A of int
  |B of int*int
