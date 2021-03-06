use std::fmt::{Display, Formatter};

#[derive(Debug)]
enum StudentError {
    MustBeCapitalized(String),
    OnlyLettersAllowed(String),
    TooShort(String),
    AgeOutOfRange,
    AgeNotANumber,
    MissingField,
}

#[derive(Debug)]
struct Student {
    name: String,
    surname: String,
    age: i32,
}

impl Display for Student {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Student {{{} {} {}}}", self.name, self.surname, self.age)
    }
}

#[derive(Debug)]
struct Slice<'a, T> {
    data: &'a[T]
}

impl <T: Display> Display for Slice<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for s in self.data {
            write!(f, "{}\n", s)?;
        }
        Ok(())
    }
}

/*
Note We cannot do that:

// CANNOT do that
impl Display for &[Student] {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for s in self {
            write!(f, "{}\n", s)?;
        };
        Ok(())
    }
}
*/


fn pure<E>(e: E) -> Vec<E> {
    let mut res = Vec::new();
    res.push(e);
    res
}

fn combine_results<A, B, E>(r1: Result<A, Vec<E>>, r2: Result<B, Vec<E>>) -> Result<B, Vec<E>> {
    match r1 {
        Ok(_) => {
            match r2 {
                Ok(v2) => Ok(v2),
                Err(e2) => Err(e2),
            }
        },
        Err(e1) => {
            match r2 {
                Ok(_) => Err(e1),
                Err(e2) => {
                    let mut both = Vec::new();
                    both.extend(e1);
                    both.extend(e2);
                    Err(both)
                }
            }
        }
    }
}

impl Student {
    pub fn new(name: &str, surname: &str, age_s: &str) -> Result<Self, Vec<StudentError>> {

        let age = combine_results(combine_results(is_valid_name(name),
        is_valid_surname(surname)),
        is_valid_age(age_s))?;

        Ok(Self {
            name: name.to_string(),
            surname: surname.to_string(),
            age,
        })
    }
}

struct StudentDB {
    students: Vec<Student>,
}

impl StudentDB {
    pub fn new() -> Self {
        Self {
            students: Vec::new(),
        }
    }

    pub fn add_student(&mut self, student: Student) {
        self.students.push(student);
    }

    pub fn students(&self) -> &[Student] {
        self.students.as_slice()
    }
}




fn is_only_letters(s: &str) -> bool {
    s.chars().all(|c| c.is_alphabetic())
}

fn is_valid_name(name: &str) -> Result<(), Vec<StudentError>> {
    let mut errs = Vec::new();
    if name[0..1] != name[0..1].to_uppercase() {
        errs.push(StudentError::MustBeCapitalized("Name".to_owned()));
    }
    if name.len() < 2 {
        errs.push(StudentError::TooShort("Name".to_owned()));
    }
    if !is_only_letters(name) {
        errs.push(StudentError::OnlyLettersAllowed("Name".to_owned()));
    }
    if errs.len() > 0 {
        Err(errs)
    } else {
        Ok(())
    }
}

fn is_valid_surname(surname: &str) -> Result<(), Vec<StudentError>> {
    let mut errs = Vec::new();
    if surname[0..1] != surname[0..1].to_uppercase() {
        errs.push(StudentError::MustBeCapitalized("Surname".to_owned()));
    }
    if surname.len() < 4 {
        errs.push(StudentError::TooShort("Surname".to_owned()));
    }
    if !is_only_letters(surname) {
        errs.push(StudentError::OnlyLettersAllowed("Surname".to_owned()));
    }

    if errs.len() > 0 {
        return Err(errs)
    } else {
        Ok(())
    }
}

fn is_valid_age(age_s: &str) -> Result<i32, Vec<StudentError>> {
    let age = age_s.parse::<i32>().map_err(|_| pure(StudentError::AgeNotANumber))?;
    if age < 18 || age > 130 {
        Err(pure(StudentError::AgeOutOfRange))
    } else {
        Ok(age)
    }
}

// TODO: Simplefy this
fn parse_student_info<'a>(
    mut words: impl Iterator<Item = &'a str>,
) -> Result<Student, Vec<StudentError>> {
    let name = words.next().ok_or(pure(StudentError::MissingField))?;
    let surname = words.next().ok_or(pure(StudentError::MissingField))?;
    let age = words
        .next()
        .ok_or(pure(StudentError::MissingField))?;

    Student::new(name, surname, age)
}

fn exec_commands(db: &mut StudentDB) {
    let stdin = std::io::stdin();

    loop {
        let mut input = String::new();
        stdin
            .read_line(&mut input)
            .expect("Failed to read input from stdin");

        let mut words = input.split_whitespace();
        let command = words.next().unwrap_or_default();

        match command {
            "" => continue,
            "new" => {
                let student = parse_student_info(words);
                match student {
                    Ok(student) => db.add_student(student),
                    Err(err) => println!("Errors: {:?}", err),
                };
            }
            "list" => {
                let students = db.students();
                println!("{}", &Slice { data: &students });
                // println!("{:?}", &Slice { data: &students });
                // println!("{:#?}", students);
            }
            "end" => break,
            _ => {
                println!("Unknown command");
                continue
            },
        }
        input.clear();
    }
}

fn main() {
    let mut db = StudentDB::new();
    exec_commands(&mut db);
}
