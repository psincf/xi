pub struct CGeneratorOptions {
    pub out_path: String,
    pub bin_file_name: String
}

pub struct CCodeGenerator {
    options: CGeneratorOptions,
    output_file: std::fs::File,
}

impl CCodeGenerator {
    pub fn new(options: CGeneratorOptions) -> Self {
        let mut file = std::fs::File::create(options.out_path.clone() + options.bin_file_name.as_str()).unwrap();

        Self {
            options,
            output_file: file
        }
    }

    pub fn gen(&mut self) {
        //todo!()
    }
}