# `students` example


## Data validation

* Name
   * must be a single string that starts with capital letter
   * can contain only letters ['a'..'Z']
   * must contain at least 2 characters
* Surname
   * same as Name, BUT, must be at least 4 characters
* Age
   * must be a natural number, between 18 and 130

# Objectives

* a simple demo for error handling
* the expected data should fulfill the specification requirements
* there are several possible validation procedures
    * `A` - simplest, where we do not care about errors, just want to ignore them and get nothing in return
    * `B` - simple validation, in which we want to know the FIRST error, and we do not care about anything else
    * `C` - more advanced validation, where we want to know ALL the errors
    * `D` exceptions? When do we use them?
      * Programming languages with Exceptions: Java (Checked/Runtime), Python, C++, ..., Haskell (library thing)
      * Programming languages WITHOUT Exceptions: C, Go (do not use panics to pretend they are like exceptions)
    * `E` panics? When do we use them?
      * With implicit panics: Java (RuntimeExceptions), Go, Rust, ...,


## Types of errors

* user errors, input validation
* external errors: network, IO, files, resources, etc
  * can recover? who? how?
  * cannot recover
* internal errors: programmer's errors, bugs, invariants or preconditions are broken
  * can recover? who? how?
  * cannot recover



## Small/non-scientific performance test results

| impl | time | factor |
| --- | --- | --- |
| **C**  | 3.95 | **1** |
| **go** | 14.5 | **3.7** |
| rust release | 17.2 | 4.4 |
| python3 | 26.7 | 6.8 |
| C++     | 26.8 | 6.8 |
| **Haskell** | 29.1 | **7.4** |
| python  | 54.6 | 13.8 |
| rust debug | 56.7 | 14.4 |


## Compiler options

* C++
   * `/usr/local/bin/g++-10 -std=gnu++14 -o students -Ofast -pedantic -Wall -Wextra -fomit-frame-pointer -march=native -flto students.cpp`
* C
   * `/usr/local/bin/gcc-10 -std=c11 -o students -Ofast -Wall -pedantic -fomit-frame-pointer -march=native  main.c stud_db.c`
* Go
   * `go build -ldflags "-s -w" .`



## Concluding remarks


* Think in Abstract (using tools/concepts from your abstractions toolbox)
* Then code it in a given language
* Most of the time, algorithm beats optimisations

