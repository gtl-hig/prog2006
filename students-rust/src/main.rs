#[derive(Debug)]
enum StudentError {
    NotCapitalized(String),
    NotAlphabetical(String),
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

impl Student {
    pub fn new(name: &str, surname: &str, age: i32) -> Result<Self, StudentError> {
        is_valid_name(name)?;
        is_valid_surname(surname)?;
        is_valid_age(age)?;

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

fn is_valid_name(name: &str) -> Result<(), StudentError> {
    if name[0..1] != name[0..1].to_uppercase() {
        Err(StudentError::NotCapitalized("Name".to_string()))
    } else if name.len() < 2 {
        Err(StudentError::TooShort("Name".to_string()))
    } else if !is_only_letters(name) {
        Err(StudentError::NotAlphabetical("Name".to_string()))
    } else {
        Ok(())
    }
}

fn is_valid_surname(surname: &str) -> Result<(), StudentError> {
    if surname[0..1] != surname[0..1].to_uppercase() {
        Err(StudentError::NotCapitalized("Surname".to_string()))
    } else if surname.len() < 4 {
        Err(StudentError::TooShort("Surname".to_string()))
    } else if !is_only_letters(surname) {
        Err(StudentError::NotAlphabetical("Surname".to_string()))
    } else {
        Ok(())
    }
}

fn is_valid_age(age: i32) -> Result<(), StudentError> {
    if age < 18 || age > 130 {
        Err(StudentError::AgeOutOfRange)
    } else {
        Ok(())
    }
}

// TODO: Simplefy this
fn parse_student_info<'a>(
    mut words: impl Iterator<Item = &'a str>,
) -> Result<Student, StudentError> {
    let name = words.next().ok_or(StudentError::MissingField)?;
    let surname = words.next().ok_or(StudentError::MissingField)?;
    let age = words
        .next()
        .ok_or(StudentError::MissingField)
        .and_then(|s| s.parse::<i32>().map_err(|_| StudentError::AgeNotANumber))?;

    Student::new(name, surname, age)
}

fn exec_commands(db: &mut StudentDB) {
    let mut input = String::new();
    let stdin = std::io::stdin();

    loop {
        stdin
            .read_line(&mut input)
            .expect("Failed to read input from stdin");

        let mut words = input.split_whitespace();
        let command = words.next().unwrap_or_default();

        match command {
            "new" => {
                let student = parse_student_info(words);
                match student {
                    Ok(student) => db.add_student(student),
                    Err(err) => println!("Error occured while creating new student: {:?}", err),
                };
            }
            "list" => {
                let students = db.students();
                println!("Students: {:#?}", students);
            }
            "end" => break,
            _ => break,
        }
        input.clear();
    }
}

fn main() {
    let mut db = StudentDB::new();
    exec_commands(&mut db);
}
