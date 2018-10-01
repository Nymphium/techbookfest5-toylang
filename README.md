Toylang interpreter
===

refer to 『つくってかんたん Virtual Machine-based Interpreter』 of 『[Dragon University 技術書典5](https://techbookfest.org/event/tbf05/circle/45010003)』

```shell-session
$ opam switch 4.06.1
$ eval $(opam env)
$ opam install -y ppx_deriving menhir
$ jbuilder exec src/toy.exe
Welcome to toylang REPL
check detail information: https://techbookfest.org/event/tbf05/circle/45010003
Ctrl-C to exit
>> let rec fib n = if n < 2 then n else fib(n - 1) + fib(n - 2) in fib 20;;
input term: (LetRecFun ("fib", "n",
   (If ((Lt ((Var "n"), (Int 2))), (Var "n"),
      (Add ((App ((Var "fib"), (Sub ((Var "n"), (Int 1))))),
         (App ((Var "fib"), (Sub ((Var "n"), (Int 2)))))))
      )),
   (App ((Var "fib"), (Int 20)))))
compiled term:
(Q ([(Clos (1, 0, 1)); (Move (2, 1)); (Load (3, 0)); (TailCall (2, 3))],
   [(RInt 20)],
   [(Q (
       [(Move (1, 0)); (Load (2, 0)); (Lt (1, 2)); (SetBool (3, 1, 1));
         (SetBool (3, 0, 0)); (Test (3, 0)); (Jump 3); (Move (4, 0));
         (Move (13, 4)); (Jump 12); (Upval (5, 0)); (Move (6, 0));
         (Load (7, 1)); (Sub (8, 6, 7)); (Call (5, 8)); (Upval (9, 0));
         (Move (10, 0)); (Load (11, 0)); (Sub (12, 10, 11)); (Call (9, 12));
         (Add (13, 5, 9)); (Move (13, 13)); (Return 13)],
       [(RInt 2); (RInt 1)], [], []))
     ],
   []))
computation resut: (Vint 6765)
>> ^C
Bye
```

# LICENSE
MIT

