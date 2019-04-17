#![feature(custom_inner_attributes, proc_macro_hygiene)]

use std::time::Duration;

fn main() {
    std::env::set_var("RUST_LOG", "TRACE");
    env_logger::init();
    foo::fibonacci(30);
    foo::fibonacci_iterative(30);
    foo::sum(1000, 1000);
    foo::exp(2, 31);
    foo::add_generic(2, 2);
    foo::wait_duration(Duration::from_secs(1));
    foo::wait_duration(Duration::from_millis(1));
    foo::wait_duration(Duration::from_micros(1));
    foo::wait_duration(Duration::from_nanos(1));
}

mod foo {
    #![autolog::autolog(blacklist = "fibonacci_impl", flame = true)]

    pub fn fibonacci(n: u32) -> u32 {
        fibonacci_impl(n)
    }

    fn fibonacci_impl(n: u32) -> u32 {
        match n {
            0 => 1,
            1 => 1,
            _ => fibonacci_impl(n - 1) + fibonacci_impl(n - 2),
        }
    }

    pub fn fibonacci_iterative(n: u32) -> u64 {
        if n == 0 || n == 1 {
            1
        } else {
            (1..n).fold((1, 1), |(last, curr), _| (curr, last + curr)).1
        }
    }

    pub fn sum(a: u32, b: u32) -> u32 {
        a + b
    }

    pub fn exp(a: u32, x: u32) -> u64 {
        let mut temp = a as u64;
        for _ in 0..x {
            temp *= a as u64
        }
        temp
    }

    pub fn add_generic<T: Into<i32>>(a: T, b: T) -> i32 {
        a.into() + b.into()
    }

    pub fn wait_duration(d: std::time::Duration) {
        std::thread::sleep(d);
    }
}
