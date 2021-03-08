// class for single student
class Student {
    constructor(firstname, surname, age) {
        this.firstname = firstname;
        this.surname = surname;
        this.age = age;
    }
}

// "database"
class Students {
    constructor() {
        this.students = []
    }
    addStudent(firstname, surname, age) {
        const student = new Student(firstname, surname, age)
        this.students.push(student)
    }
    getAllStudents() {
        return this.students
    }
}

// validating and creating student
const newStudent = (input) => {
    let errorMessages = []

    // validate format
    if (input.length !== 4) {
        errorMessages.push("Should provide 3 arguments: Name Surname Age");
        return errorMessages;
    }

    firstname = input[1]
    surname = input[2]
    age = input[3]

    // validate firstname
    if (firstname.length < 2) {
        errorMessages.push("Name must be at least 2 characters long")
    }
    if (firstname.charAt(0) !== firstname.charAt(0).toUpperCase()) {
        errorMessages.push("Name must have first character Capitalized")
    }
    if (/[^a-zA-Z]/.test(firstname)) {
        errorMessages.push("Name can only have letters")
    }

    // validate surname
    if (surname.length < 4) {
        errorMessages.push("Surname must be at least 4 characters long")
    }
    if (surname.charAt(0) !== surname.charAt(0).toUpperCase()) {
        errorMessages.push("Surname must have first character Capitalized")
    }
    if (/[^a-zA-Z]/.test(surname)) {
        errorMessages.push("Surname can only have letters")
    }

    // validate age
    if (isNaN(age)) {
        errorMessages.push("Age must be a number")
    } else if (age < 18 || 130 < age) {
        errorMessages.push("Age outside the range")
    }

    if (errorMessages.length > 0) { // if errors
        let finalMessage = ""
        errorMessages.forEach((message, idx) => {
            finalMessage += message
            if(idx !== errorMessages.length-1){
                finalMessage += ", "
            }
        })
        process.stdout.write(finalMessage+"\n");
    } else { // if no errors
        students.addStudent(firstname, surname, age);
    }
}

// show all students
const showAllStudents = () => {
    const allStudents = students.getAllStudents();
    allStudents.forEach((student, idx) => {
        process.stdout.write(`${student.firstname}, ${student.surname}, ${student.age}\n`)
    })
}

// main
process.stdin.resume();
process.stdin.setEncoding('ascii');

students = new Students();
var readline = require('readline');
var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function(d){
    const input = d.toString().trim().split(" ");
    if (input[0] === "new") {
        newStudent(input);
    } else if (input[0] === "list") {
        showAllStudents();
    } else if (input[0] === "end") {
        // TODO because of async write, the command below
        // quits the program BEFORE it finishes printing
        // process.exit(1)
    } else {
        process.stdout.write("unknown command\n")
    }
});

// TODO: think of a better solution for the timing
// this is just a hack to approx. the actual time
// setTimeout(function(){process.exit(1);}, 15900);
