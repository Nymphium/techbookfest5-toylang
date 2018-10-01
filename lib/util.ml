let take i xs =
  let rec work i acc = function
    | _ when i = 0 -> List.rev @@ acc
    | x :: xs -> work (i - 1) (x :: acc) xs
    | _ -> invalid_arg "take"
  in work i [] xs

let split_by_index i xs =
  let rec work acc j = function
    | xs when j = 0 -> List.rev acc, xs
    | x :: xs -> work (x :: acc) (j - 1) xs
    | _ -> invalid_arg "split_by_index"
  in work [] i xs

let take_range i n xs =
  let _, r = split_by_index i xs in
  take n r

let swap_by_index i xs e =
  let l, r = split_by_index (i + 1) xs in
  let l' = take i l in
  l' @ (e :: r)

let rec drop i = function
  | _ when i < 0 -> invalid_arg "drop"
  | [] -> invalid_arg "drop"
  | xs when i = 0 -> xs
  | _ :: xs -> drop (i - 1) xs

let find_opt_with_index p xs =
  let rec work i = function
    | x :: _ when p x -> Some(x, i)
    | _ :: xs -> work (i + 1) xs
    | [] -> None
  in work 0 xs

let extract_by_index xs i =
  let rec work i acc = function
    | x :: xs when i = 0 -> x, List.rev acc @ xs
    | x :: xs -> work (i - 1) (x :: acc) xs
    | _ -> invalid_arg "remove_by_index"
  in work i [] xs

