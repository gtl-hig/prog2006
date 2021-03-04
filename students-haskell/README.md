# `students` example

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


## Student

* Name
    * must be a single string that starts with capital letter
    * can contain only letters
    * must contain at least 2 characters
* Surname
    * same as Name, BUT, must be at least 4 characters
* Age
    * must be a natural number, between 18 and 130



## Concluding remarks


Think in Abstract (using tools/concepts from your abstractions toolbox)
Then code it in a given language
