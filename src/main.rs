mod lexer;
use lexer::Lexer;
mod pipeline;

fn main() {
    println!("Hello, world!");

    let file_path = std::env::args().nth(1).unwrap();
    let mut pipeline = pipeline::Pipeline::new(file_path);
    pipeline.solve();
}