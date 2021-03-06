import sys
from collections import namedtuple

Student = namedtuple("Student", "name surname age")

StudentsDB = []


def validateName(prefix, name):
    errors = []
    if len(name) == 0:
        errors.append(prefix + " cannot be empty")
        return errors
    if len(name) < 2:
        errors.append(prefix + " too short")
    if not name.isalpha():
        errors.append(prefix + " can only have letters")
    if not name[0].isupper():
        errors.append(prefix + " must be Capitalized")
    return errors


def validateAge(ageS):
    errors = []
    try:
        age = int(ageS)
    except Exception:
        errors.append("Age not a number!")
        return 0, errors
    if age < 18 or age > 130:
        errors.append("Age outside the range")
    return age, errors


def newStudent(words):
    errors = []
    nameErrors = validateName("Name", words[0])
    errors.extend(nameErrors)
    name = words[0]

    surnameErrors = validateName("Surname", words[1])
    errors.extend(surnameErrors)
    surname = words[1]

    age, ageErrors = validateAge(words[2])
    errors.extend(ageErrors)

    return Student(name=name, surname=surname, age=age), errors


def printStudents(studs):
    for s in studs:
        print(s)


def processor():
    while True:
        line = sys.stdin.readline()
        words = line.split()
        if len(words) == 0:
            continue
        elif 'end' == words[0]:
            break
        elif 'new' == words[0]:
            stud, errors = newStudent(words[1:])
            if len(errors) > 0:
                print(errors)
            else:
                StudentsDB.append(stud)
        elif 'list' == words[0]:
            printStudents(StudentsDB)
        else :
            print("unknown command")



processor()
