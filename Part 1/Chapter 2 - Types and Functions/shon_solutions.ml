(** {1 Notes on Chapter 2}

    For a runnable version of these notes, see
    https://github.com/shonfeder/CTfP/tree/master/lib/memo.ml *)

(** {2 2.7 Challenges} *)

(** Exercise 1

    Define a higher-order function that takes a pure function [f] as an argument
    and returns a function that behaves like [f] except that it only calls the
    function once for every dstinct argument, and stores the result internally,
    returning the stored result instead of invoking [f] again if the same argument
    is provided in subsequent calls. *)

let memo f =
  let ht = Hashtbl.create 100 in
  fun x ->
    match Hashtbl.find_opt ht x with
    | Some y -> y
    | None   ->
        let y = f x in
        Hashtbl.add ht x y;
        y

let%expect_test "memo memoizes (morally) pure functions" =
  let f x =
    let result = Int.to_string x in
    print_endline "Computing...";
    result
  in
  let memo_f = memo f in
  let check n = memo_f n |> Printf.printf "Value: %s\n" in
  check 2;
  [%expect {|
    Computing...
    Value: 2 |}];
  check 3;
  [%expect {|
    Computing...
    Value: 3 |}];
  (* The value is not recomputed when the input has been seen before *)
  check 2;
  [%expect {|
    Value: 2 |}]

let%test "memoization of pure functions" =
  let open QCheck in
  Aux.run_qcheck
    [ Test.make
        ~name:"memoized function are equal to the function"
        (pair (fun1 Observable.string int) string)
        (fun (f, x) ->
          let memo_f = memo (Fn.apply f) in
          Fn.apply f x = memo_f x)
    ]

(** Exercise 2
    Try to memoize a function from your standard library that you normally use
    to produce random numbers. Does it work? *)

let%test "memoization of non-deterministic functions" =
  let open QCheck in
  Aux.run_qcheck
    [ (* NOTE: This test is non-determinstic, and could fail occasionally *)
      Test.make
        ~name:"Random.int gives different results on repeated calls"
        (int_range 100 10000)
        (fun bound ->
          assume (bound > 0);
          Random.int bound != Random.int bound)
    ; Test.make
        ~name:
          "memoized Random.int doesn't give different results on repeated \
           calls "
        (int_range 100 10000)
        (fun bound ->
          let memo_rand = memo Random.int in
          not (memo_rand bound != memo_rand bound))
    ]

(** Exercise 3
    Implement a function that takes a seed, calls a seeded random number
    generator, and returns the result. Memoize that function. Does it work? *)

let init_rand seed =
  Random.init seed;
  Random.int 10000

let%test _ =
  let open QCheck in
  Aux.run_qcheck
    [ Test.make
        ~name:"init_rand is equal to its memoized version"
        int
        (fun seed ->
          let memo_init_rand = memo init_rand in
          init_rand seed = memo_init_rand seed)
    ]

(** Exercise 4

    (a) Pure and equal to its memoized version.
    (b) Impure and unequal to its memoized version.
    (c) Impure but benign, and equal to its memoized version.
    (d) Impre and unequal to its memoized version (due to shared, accumulating
        state in the static y)
*)

(** Exercise 5

    How many different functions are there from bool to bool? Implement them all. *)
module Bool_to_bool = struct
  type t = bool -> bool

  let id : t = Fun.id

  let to_true : t = fun _ -> true

  let to_false : t = fun _ -> false

  let not : t = not
end

(** Exercise 6

    Draw a picture of a category whose only objects are the types bottom, unit,
    and bool; with arrows corresponding to all possible functions between these
    types. Label the arrows with the names of the functions.

    See {{: ./ch-2-ex-6-diagram.png } the diagram} *)
