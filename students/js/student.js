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
    firstname = input[1]
    surname = input[2]
    age = input[3]

    // validate format
    if (input.length !== 4) {
        console.error("Should provide 3 arguments: Name Surname Age");
        return;
    }

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
    if (/[^a-zA-Z]/.test(firstname)) {
        errorMessages.push("Surname can only have letters")
    }

    // validate age
    if (isNaN(age)) {
        errorMessages.push("age needs to be a number")
    } else if (age < 0 || 100 < age) {
        errorMessages.push("age needs to be a number between 0 and 100")
    }

    if (errorMessages.length > 0) { // if errors
        let finalMessage = ""
        errorMessages.forEach((message, idx) => {
            finalMessage += message
            if(idx !== errorMessages.length-1){
                finalMessage += ", "
            }
        })
        console.error(finalMessage);
    } else { // if no errors
        students.addStudent(firstname, surname, age);
    }
}

// show all students
const showAllStudents = () => {
    const allStudents = students.getAllStudents();
    allStudents.forEach((student, idx) => {
        console.log(`firstname: ${student.firstname}, surname: ${student.surname}, age: ${student.age}`)
    })
}

// main
const stdin = process.openStdin();
students = new Students();
stdin.addListener("data", (d) => {
    const input = d.toString().trim().split(" ");
    if (input[0] === "new") {
        newStudent(input);
    } else if (input[0] === "list") {
        showAllStudents();
    } else if (input[0] === "end") {
        process.exit(1)
    } else {
        console.log("unknown command")
    }
});