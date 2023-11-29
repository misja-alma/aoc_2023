let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | x -> fib (x-1) + fib(x-2)

