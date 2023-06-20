mod lexer;
mod parser;
mod sema;

mod pipeline;
mod utils;

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    
    let mut pipeline_options = pipeline::PipelineOptions::default();

    for arg in std::env::args().skip(2) {
        let (minus_minus, arg_strip) = arg.split_at(2);
        if minus_minus != "--" {
            panic!("argument \"{arg}\" not recognized")
        }

        if arg_strip == "write_tokens" { pipeline_options.write_tokens = true; }
        else if arg_strip == "write_ast" { pipeline_options.write_ast = true; }
        else if arg_strip == "write_sym_table" { pipeline_options.write_sym_table = true; }
        else if arg_strip == "write_types_sema" { pipeline_options.write_types_sema = true; }
        else { panic!("argument \"{arg}\" not recognized") }
    }

    let mut pipeline = pipeline::Pipeline::new(file_path, pipeline_options);
    pipeline.solve();
}