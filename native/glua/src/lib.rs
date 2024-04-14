#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn owo(a: i64, b: i64) -> i64 {
    a - b
}

rustler::init!("Elixir.Glua", [add, owo]);
