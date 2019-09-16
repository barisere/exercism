let leap_year = function
  | n when Int.rem n 400 = 0 -> true
  | n when Int.rem n 4   = 0 -> not(Int.rem n 100 = 0)
  | _                        -> false
;;
