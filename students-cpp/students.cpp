#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iterator>
#include <valarray>

class Student{
    public:
        std::string name;
        std::string surname;
        uint8_t age;

    Student(std::string name, std::string surname, uint8_t age){
        this->name = name;
        this->surname = surname;
        this->age = age;
    }
};

void executeCommands(std::vector<Student*> &students);
void printStudents(std::vector<Student*> &students);
Student* createStudent(std::vector<std::string> &cmd);

void executeCommands(std::vector<Student*> &students){
    while (1) {
        std::string in;  // Declare and read input
        getline (std::cin, in);

        if(!in.size()) continue; // Do nothing if empty

        std::istringstream iss(in);
        std::vector<std::string> cmd((std::istream_iterator<std::string>(iss)), std::istream_iterator<std::string>());

        if(cmd[0].compare("new") == 0){
            students.push_back(createStudent(cmd));
        }
        else if(cmd[0].compare("list") == 0){
            printStudents(students);
        }
        else if(cmd[0].compare("end") == 0){
            break;
        }
        else {
            std::cout << "\nUnknown command\n";
        }
    }
}

void printStudents(std::vector<Student*> &students){
    for (Student* &s : students){
        std::cout << s->name << ", " << s->surname << ", " << std::to_string(s->age) << "\n";
    }
}

std::vector<std::string> validateName(std::string name){
    std::vector<std::string> errors;
    return errors;
}

std::vector<std::string> validateSurname(std::string surname){
    std::vector<std::string> errors;
    return errors;
}

std::vector<std::string> validateAge(std::string age){
    std::vector<std::string> errors;
    return errors;
}

Student* createStudent(std::vector<std::string> &cmd){

    return new Student(cmd[1], cmd[2], std::stoi(cmd[3]));
}

int main(){
    std::vector<Student*> students;

    // FOR DEBUG PURPOSES //
    students.push_back(new Student("Test", "Testington", 30));
    students.push_back(new Student("John", "Doe", 50));

    executeCommands(students);
    return 0;
}
