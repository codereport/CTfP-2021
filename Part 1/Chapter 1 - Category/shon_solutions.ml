(** {1} Notes on Chapter 1

    For a runnable version of these answers, see
    https://github.com/shonfeder/CTfP/blob/master/lib/composition.ml *)

(** {2} 1.4 Challenges *)

(** Exercise 1
    Implement the identify function. *)

let id : 'a. 'a -> 'a  =
  fun x -> x

(** Exercise 2
    Implement the composition function. *)

let (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c =
  fun g f x -> g (f x)

(** Exercise 3
    >  Write a program that tries to test that your composition function respects identity *)

let%test_module "composition" = (module struct
  open QCheck

  let%test _ =
    Aux.run_qcheck
      [
        Test.make ~name:"composition respects left identity"
          (pair (fun1 Observable.string int) string)
          (fun (f, x) -> Fn.apply f x = ((id : int -> int) % Fn.apply f) x)
      ; Test.make ~name:"composition respects right identity"
          (pair (fun1 Observable.string int) string)
          (fun (f, x) -> Fn.apply f x = (Fn.apply f % (id : string -> string)) x)
      ]
end)

(** Exercise 4

    > Is the world-wide web a category in any sense? Are links morphisms?

    Links hyperlinks are not morphisms between pages, because most pages don't
    have a hyperlink pointing to themselves, and hyperlinks are not transitive:
    if page A has a link to page B and page B a link to page C, there is no
    reason to think there will also be a hyperlink from A to C. I.e., the
    hyperlink connection doesn't compose.

    However, I think the www is a category if the objects are pages and the
    morphisms are the the relation of "navigable from". Each page is navigable
    from itself by reloading. One page is navigable from another another if
    there is a hyperlink, and you can compose these morphisms by first following
    one link then another.
*)

(** Exercise 5

    > Is Facebook a category, with people as objects and friendships as morphsisms?

    Facebook might be a category in some senses, but not with this assignment. I
    don't think people are friends with themselves, and friendship is not transitive.
*)

(** Exercise 6

    > When is a directed graph a category?

    A digraph is a category where the nodes are objects and the edges are
    morphisms when

    - every node has a loop
    - there is an edge for every possile connection, reflecting the transitive
      closure of the "connected to" relation.

    If we let the morphisms just be the "connected to" relation, and consider
    this relationship to hold trivially for nodes even if they don't have edges
    back to themselves, then maybe every digraph is a category? *)
