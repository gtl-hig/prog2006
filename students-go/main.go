package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
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

func validateName(newName string) error {
	if strings.ToUpper(newName[:0]) != newName[:0] || len(newName) < 2 {
		// here ADD 3-4 line for a FOR loop
		return errors.New("invalid Name: must be at least 2 characters and first Cap")
	}
	return nil
}

func validateSurname(newName string) error {
	if strings.ToUpper(newName[:0]) != newName[:0] || len(newName) < 4 {
		return errors.New("invalid Surname: must be at least 4 characters and first Cap")
	}
	return nil
}

func validateAge(newAgeS string) (int, error) {
	newAge, err := strconv.Atoi(newAgeS)
	if err != nil { return -1, err }
	if newAge < 18 || newAge > 130 {
		return -1, errors.New("Age should be between 18 and 130")
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
		case "": break
		case "new":
			stud, err := createStudent(words[1:])
			if err != nil {
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

func createStudent(words []string) (*Student, error) {
	if len(words) != 3 {
		return &Student{}, errors.New("should provide 3 arguments: Name Surname Age")
	}
	err := validateName(words[0])
	if err != nil {
		return &Student{}, err
	}
	err = validateSurname(words[1])
	if err != nil {
		return &Student{}, err
	}
	age, err := validateAge(words[2])
	if err != nil {
		return &Student{}, err
	}
	return &Student{words[0], words[1], age}, nil
}


func main() {
	db := new(MemDB)
	db.Init()
	execCmd(db)
}