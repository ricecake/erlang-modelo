-module(test).

-fields([a, b, c, d]).

-join([
  {hasmany, other, z},
  {hasa, other, b}
]).
