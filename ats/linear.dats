#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// compile with -DATS_MEMALLOC_LIBC to use the normal libc functions (malloc
// and free) to manage memory

datatype foo (a: t@ype) =
  | foo_nil (a) of ()
  | foo_cons (a) of (a, foo a)

fun{a,b:t@ype} map (f: a -> b, l: foo a): foo b =
  case+ l of
    | foo_nil  ()      => foo_nil
    | foo_cons (x, xs) => foo_cons (f (x), map<a,b> (f, xs))

fun print_int_list (l: foo int): void =
  let fun loop (l: foo (int)): void = case+ l of
                                      | foo_nil  ()      => ()
                                      | foo_cons (x, xs) => begin print x;
                                                                  loop xs; end
  in begin print "["; loop l; print "]"; end
  end

implement main0 () = print_int_list (map<int,int> (lam (x: int): int => x * 2,
                                                   (foo_cons (1,
                                                    foo_cons (2,
                                                    foo_cons (3,
                                                    foo_nil))))))
