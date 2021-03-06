# Students Generator

A simple tool to generate input for the students program.

```
SHORT USAGE:
    students-gen-rust --correct <CORRECT> --incorrect <INCORRECT> --output <OUTPUT_FILE>
```

It uses the excellent `clap` library for parsing and providing neatly formatted usage hints and tips. We get the following
from help:

```
Students Data Generator 1.0
Mariusz fom PROG2006
The program generates data file for testing student error handling implementations

USAGE:
    students-gen-rust --correct <CORRECT> --incorrect <INCORRECT> --output <OUTPUT_FILE>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -c, --correct <CORRECT>        How many correct students to generate
    -i, --incorrect <INCORRECT>    How many incorrect students to generate
    -o, --output <OUTPUT_FILE>     Output filename to generate the data INTO. [default: output.txt]
```

It also uses the excellent `anyhow` library to produce superior error messages with context to help the user. It has a 
generic type for `Error` combined with `Result` that makes using the `?` operator with multiple error types very natural.