const BASE_NUMBERS: [(u8, &str); 28] = [
    (0, "zero"),
    (1, "one"),
    (2, "two"),
    (3, "three"),
    (4, "four"),
    (5, "five"),
    (6, "six"),
    (7, "seven"),
    (8, "eight"),
    (9, "nine"),
    (10, "ten"),
    (11, "eleven"),
    (12, "twelve"),
    (13, "thirteen"),
    (14, "fourteen"),
    (15, "fifteen"),
    (16, "sixteen"),
    (17, "seventeen"),
    (18, "eighteen"),
    (19, "nineteen"),
    (20, "twenty"),
    (30, "thirty"),
    (40, "forty"),
    (50, "fifty"),
    (60, "sixty"),
    (70, "seventy"),
    (80, "eighty"),
    (90, "ninety"),
];

fn units_and_tens(num: u8) -> String {
    if num == 0 {
        return "".to_owned();
    }
    if num < 20 {
        return BASE_NUMBERS[num as usize].1.to_string();
    }
    let (quot, remainder) = (num / 10 * 10, num % 10);
    if remainder == 0 {
        return BASE_NUMBERS[quot as usize].1.to_string();
    }
    let suffix = format!("-{}", BASE_NUMBERS[remainder as usize].1);
    let idx = 20 + (quot as usize) / 10 - 2;
    format!("{}{}", BASE_NUMBERS[idx as usize].1, suffix)
}

fn hundreds(num: u16) -> String {
    if num == 0 {
        return "".to_owned();
    }
    let (quot, remainder) = (num / 100, num % 100);
    let prefix = if quot == 0 {
        "".to_owned()
    } else {
        format!("{} hundred", BASE_NUMBERS[quot as usize].1)
    };
    let suffix = if remainder == 0 {
        "".to_owned()
    } else {
        format!(" {}", units_and_tens(remainder as u8))
    };
    prefix + &suffix
}

fn div_hundreds_with_remainder(num: u64, div: u64) -> (String, u64) {
    let (quot, remainder) = (num / div, num % div);
    if quot == 0 {
        ("".to_owned(), remainder)
    } else {
        (hundreds(quot as u16), remainder)
    }
}

fn prefix_place_value(amount: &str, order: &str) -> String {
    if amount.is_empty() {
        "".to_owned()
    } else {
        format!("{} {}", amount, order)
    }
}

fn n_in_words(n: u64) -> String {
    let (quintillion, quadrillion_rem) = div_hundreds_with_remainder(n, 1_000_000_000_000_000_000);
    let (quadrillion, trillion_rem) =
        div_hundreds_with_remainder(quadrillion_rem, 1_000_000_000_000_000);
    let (trillion, billion_rem) = div_hundreds_with_remainder(trillion_rem, 1_000_000_000_000);
    let (billion, million_rem) = div_hundreds_with_remainder(billion_rem, 1_000_000_000);
    let (million, thousand_rem) = div_hundreds_with_remainder(million_rem, 1_000_000);
    let (thousand, hundred_rem) = div_hundreds_with_remainder(thousand_rem, 1_000);
    let hundred = hundreds(hundred_rem as u16);
    [
        prefix_place_value(&quintillion, "quintillion"),
        prefix_place_value(&quadrillion, "quadrillion"),
        prefix_place_value(&trillion, "trillion"),
        prefix_place_value(&billion, "billion"),
        prefix_place_value(&million, "million"),
        prefix_place_value(&thousand, "thousand"),
        hundred,
    ]
    .iter()
    .fold("".to_owned(), |acc, v| {
        format!("{} {}", acc.trim(), v.trim())
    })
    .trim()
    .to_string()
}

pub fn encode(number: u64) -> String {
    if number < 20 {
        BASE_NUMBERS[number as usize].1.to_string()
    } else {
        n_in_words(number)
    }
}
