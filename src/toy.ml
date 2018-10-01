open Hvm

let prompt = ">> "
let print_prompt () =
  print_string prompt |> flush_all

let print_splash_screen () =
  print_endline "\
Welcome to toylang REPL
check detail information: https://techbookfest.org/event/tbf05/circle/45010003
Ctrl-C to exit" |> flush_all

let print_cresult t c result =
  Printf.printf
    "\
input term: %s
compiled term:\n%s
computation resut: %s
"
    (Term.show t)
    (Irepr.show_cl c)
    (Irepr.show_regv result)
  |> flush_all

let parse () =
  let lexbuf = Lexing.from_channel stdin in
  let open Parsing in
  Parse.parse Lex.token lexbuf

let comprun t =
  let c = Compile.compile t in
  let result = Vm.run c in
  (c, result)

let _ =
  let () = Sys.(set_signal sigint @@ Signal_handle (fun _ -> print_endline "\nBye" |> flush_all; exit 0)) in
  let () = print_splash_screen () in
  let rec main () =
    let () = print_prompt () in
    let () =
      try
        let t = parse () in
        let (c, result) = comprun t in
        print_cresult t c result
      with
      | Parsing.Parse.Error ->
        print_endline "failed to parse" |> flush_all
    in main ()
  in main ()

