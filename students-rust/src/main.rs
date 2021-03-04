#[derive(Debug)]
enum StudentError {
    NotCapitalized,
    NotAlphabetical,
    TooShort,
    AgeOutOfRange,
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
        Err(StudentError::NotCapitalized)
    } else if name.len() < 2 {
        Err(StudentError::TooShort)
    } else if !is_only_letters(name) {
        Err(StudentError::NotAlphabetical)
    } else {
        Ok(())
    }
}

fn is_valid_surname(surname: &str) -> Result<(), StudentError> {
    if surname[0..1] != surname[0..1].to_uppercase() {
        Err(StudentError::NotCapitalized)
    } else if surname.len() < 4 {
        Err(StudentError::TooShort)
    } else if !is_only_letters(surname) {
        Err(StudentError::NotAlphabetical)
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

fn exec_commands(db: &mut StudentDB) {
    let mut input = String::new();
    let stdin = std::io::stdin();

    loop {
        stdin
            .read_line(&mut input)
            .expect("Failed to read input from stdin");

        let mut words = input.split(' ');
        let command = words.next().map(str::trim).unwrap_or("end");

        match command {
            "new" => {
                let name = words.next().unwrap();
                let surname = words.next().unwrap();
                let age = words.next().unwrap().trim();
                let student = Student::new(name, surname, age.parse().unwrap()).unwrap();
                db.add_student(student);
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
