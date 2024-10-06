use clap::Parser;
use std::fs;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File path to compile
    #[arg(short, long)]
    file_path: String,
}

fn main() {
    let args = Args::parse();

    let input = fs::read_to_string(args.file_path).expect("Should have been able to read the file");

    let (parse_result, parse_errors) = frame::parser::parse(&input);

    for parse_error in parse_errors {
        let report = frame::parser::to_report(&parse_error);

        // print errors to stdout
        let _ = report.eprint(ariadne::Source::from(input.clone()));
    }

    let input_expr = frame::parser::parse_block_to_expr(parse_result).expect("parsing expr");

    let result = frame::typecheck::infer(&input_expr, &mut vec![]);

    match result {
        Ok(_) => {
            println!("OK!");
            std::process::exit(0)
        }
        Err(type_error) => {
            let report = frame::typecheck::to_report(&type_error);

            // print errors to stdout
            let _ = report.eprint(ariadne::Source::from(input));

            std::process::exit(1)
        }
    }
}
