open Base;;

type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus
;;

let planet_orbital_period planet =
  let earth_year = 31557600.0 in
  match planet with
  | Mercury -> earth_year *. 0.2408467
  | Venus   -> earth_year *. 0.61519726
  | Earth   -> earth_year
  | Mars    -> earth_year *. 1.8808158
  | Jupiter -> earth_year *. 11.862615
  | Saturn  -> earth_year *. 29.447498
  | Uranus  -> earth_year *. 84.016846
  | Neptune -> earth_year *. 164.79132

let age_on planet seconds =
  let orbital_period = planet_orbital_period planet in
  Float.of_int seconds /. orbital_period
;;
