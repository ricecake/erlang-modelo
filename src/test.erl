-module(test).

-fields([a, b, c, d]).

-join([
  {has_many, other, z},
  {belongs_to, other, b}
]).
