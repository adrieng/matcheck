let uncurry f (a, b) = f a b
let rec repeat n a = match n with | 0 -> [] | n -> a :: (repeat (n - 1) a)
let concat_with sep l =
  List.fold_left (fun a b -> if a = "" then b else a ^ sep ^ b) "" l;;
let sort_uniq l =
  let rec add l a = match l with
    | [] -> [a]
    | h :: t -> if a < h then a :: l else (if a > h then h :: add t a else l) in
  List.fold_left add [] l
let rec combine3 l1 l2 l3 = match (l1, l2, l3) with
  | ([], [], []) -> []
  | (a :: l1', b :: l2', c :: l3') -> (a, b, c) :: combine3 l1' l2' l3'
  | _ -> invalid_arg "combine3"
let rec split3 l = match l with
  | [] -> ([], [], [])
  | (a, b, c) :: l' ->
      let (l1, l2, l3) = split3 l' in
      (a :: l1, b :: l2, c :: l3)
let rec cut_at n l =
  let rec f acc n l = match n with
    | 0 -> (acc, List.hd l, List.tl l)
    | n -> f (List.hd l :: acc) (n - 1) (List.tl l) in
  if n < 1 || n > List.length l then invalid_arg "cut_at" else f [] n l
let rec range n m = if n > m then [] else n :: (range (n + 1) m)
let gather l = List.fold_left (fun l a ->
                                 match a with
                                   | None -> l
                                   | Some e -> e :: l) [] l
let separate n l =
  let rec f acc n l = match (n, l) with
    | (0, _) -> (acc, l)
    | (n, h :: t) -> f (h :: acc) (n - 1) t
    | _ -> assert false in
  if n > List.length l
  then invalid_arg "separate"
  else
    let (p, l') = f [] n l in
    (List.rev p, l')
let rec keep l n = match (l, n) with
  | (_, 0) -> invalid_arg "keep"
  | (h :: _, 1) -> h
  | (_ :: t, n) -> keep t (n - 1)
  | _ -> invalid_arg "keep"
let rec drop l n = match (l, n) with
  | (_, 0) -> invalid_arg "drop"
  | (_ :: t, 1) -> t
  | (h :: t, n) -> h :: (drop t (n - 1))
  | _ -> invalid_arg "drop"
module SSet = Set.Make(struct
                         type t = string
                         let compare s1 s2 = compare s1 s2
                       end)
let raise_exc f e = try ignore (f e); false with _ -> true
