use std::fmt::{Display, Formatter, Result};

const LOOKUP_TABLE: [(u16, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

fn divide_in_roman(decimal_n: u16, (place_value, string_value): &(u16, &str)) -> (u16, String) {
    let (quot, rem) = (decimal_n / place_value, decimal_n % place_value);
    if quot == 0 {
        (decimal_n, "".to_owned())
    } else {
        (rem, string_value.repeat(quot as usize))
    }
}

pub struct Roman {
    value: String,
}

impl Display for Roman {
    fn fmt(&self, _f: &mut Formatter<'_>) -> Result {
        _f.write_str(&self.value)
    }
}

impl From<u16> for Roman {
    fn from(num: u16) -> Self {
        let roman_string = LOOKUP_TABLE
            .iter()
            .fold((num, "".to_owned()), |(n, roman_s), v| {
                let (next_n, n_in_roman) = divide_in_roman(n, v);
                (next_n, format!("{}{}", roman_s, n_in_roman))
            });
        Roman {
            value: roman_string.1.to_string(),
        }
    }
}
