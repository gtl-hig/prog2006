package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type Student struct {
	name    string
	surname string
	age     int
}

type StudentDB interface {
	Init()
	AddStudent(s Student)
	GetAllStudents() []Student
}

type MemDB struct {
	data []Student
}

// Init initialise the DB
func (db *MemDB) Init() {
	db.data = make([]Student, 0)
}

// GetAllStudents returns all students
func (db *MemDB) GetAllStudents() []Student {
	return db.data
}

// AddStudent adds new student to the DB
func (db *MemDB) AddStudent(s Student) {
	db.data = append(db.data, s)
}

func OnlyLetters(s string) bool {
	for _, r := range s {
		if !unicode.IsLetter(r) {
			return false
		}
	}
	return true
}

func validateName(newName string) []error {
	errs := make([]error, 0)
	if strings.ToUpper(newName[:1]) != newName[:1] {
		errs = append(errs, errors.New("Name must be Capitalized"))
	}
	if len(newName) < 2 {
		errs = append(errs, errors.New("Name too short"))
	}
	if !OnlyLetters(newName) {
		errs = append(errs, errors.New("Name only letters allowed"))
	}
	return errs
}

func validateSurname(newName string) []error {
	errs := make([]error, 0)
	if strings.ToUpper(newName[:1]) != newName[:1] {
		errs = append(errs, errors.New("Surname must be Capitalized"))
	}
	if len(newName) < 4 {
		errs = append(errs, errors.New("Surname too short"))
	}
	if !OnlyLetters(newName) {
		errs = append(errs, errors.New("Surname only letters allowed"))
	}

	return errs
}

func validateAge(newAgeS string) (int, error) {
	newAge, err := strconv.Atoi(newAgeS)
	if err != nil {
		return -1, err
	}
	if newAge < 18 || newAge > 130 {
		return -1, errors.New("Age outside the range")
	}
	return newAge, nil
}

// execCmd commands processor
func execCmd(db StudentDB) {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		cmd := scanner.Text()
		words := strings.Split(cmd, " ")
		switch words[0] {
		case "":
			break
		case "new":
			stud, err := createStudent(words[1:])
			if len(err) != 0 {
				fmt.Println(err)
			} else {
				db.AddStudent(*stud)
			}
		case "list":
			for _, s := range db.GetAllStudents() {
				fmt.Println(s)
			}
		case "end":
			os.Exit(0)
		default:
			fmt.Println("unknown command")
		}
	}
}

func createStudent(words []string) (*Student, []error) {
	errs := make([]error, 0)

	if len(words) != 3 {
		errs = append(errs, errors.New("should provide 3 arguments: Name Surname Age"))
		return &Student{}, errs
	}
	err := validateName(words[0])
	if err != nil {
		errs = append(errs, err...)
	}
	err = validateSurname(words[1])
	if err != nil {
		errs = append(errs, err...)
	}
	age, errA := validateAge(words[2])
	if errA != nil {
		errs = append(errs, errA)
	}
	if len(errs) == 0 {
		return &Student{words[0], words[1], age}, nil
	}

	return &Student{words[0], words[1], age}, errs
}

func main() {
	db := new(MemDB)
	db.Init()
	execCmd(db)
}
