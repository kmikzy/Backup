let npi exp = 
  let rec f = function 
    |([],acc)->List.hd acc
    |("+"::xs,a::b::ys) -> f (xs,(a+b::ys)) 
    |("-"::xs,a::b::ys) -> f (xs,(a-b::ys)) 
    |("*"::xs,a::b::ys) -> f (xs,(a*b::ys)) 
    |("/"::xs,a::b::ys) -> f (xs,(a/b::ys)) 
    |(n::xs, acc) -> f (xs,int_of_string  n :: acc)
  in f (Str.split (Str.regexp " ") exp, [])
