use criterion::{black_box, criterion_group, criterion_main, Criterion};
use scheme_ish::lexer::*;

pub fn benchmark_one(c: &mut Criterion) {
    c.bench_function("aaa", |b| {
        b.iter(|| {
            black_box({
                let mut l = Lexer::new();
                l.tokenize("'(#t #f)")
                    .unwrap()
                    .tokenize("(10 2)")
                    .unwrap()
                    .tokenize("(\'2\'")
                    .unwrap()
                    .tokenize("(\"a 2 j()\"")
                    .unwrap();
            })
        })
    });
}

criterion_group!(benches, benchmark_one);
criterion_main!(benches);
