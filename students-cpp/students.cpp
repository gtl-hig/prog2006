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
std::vector<std::string> validateName(const std::string &name);
std::vector<std::string> validateSurname(const std::string &name);
std::vector<std::string> validateAge(const std::string &name);
bool checkForErrors(const std::vector<std::string> &cmd);

void executeCommands(std::vector<Student*> &students){
    while (1) {
        std::string in;  // Declare and read input
        getline (std::cin, in);

        if(!in.size()) continue; // Do nothing if empty

        std::istringstream iss(in);
        std::vector<std::string> cmd((std::istream_iterator<std::string>(iss)), std::istream_iterator<std::string>());

        if(cmd[0].compare("new") == 0){
            if(checkForErrors(cmd)) students.push_back(new Student(cmd[1], cmd[2], std::stoi(cmd[3])));
        }
        else if(cmd[0].compare("list") == 0){
            printStudents(students);
        }
        else if(cmd[0].compare("end") == 0){
            break;
        }
        else {
            std::cout << "\nUnknown command\n\n";
        }
    }
}

void printStudents(std::vector<Student*> &students){
    for (Student* &s : students){
        std::cout << s->name << ", " << s->surname << ", " << std::to_string(s->age) << "\n";
    }
}

std::vector<std::string> validateName(const std::string &name){
    std::vector<std::string> errors;
    return errors;
}

std::vector<std::string> validateSurname(const std::string &surname){
    std::vector<std::string> errors;
    return errors;
}

std::vector<std::string> validateAge(const std::string &age){
    std::vector<std::string> errors;
    return errors;
}

bool checkForErrors(const std::vector<std::string> &cmd){
    std::vector<std::string> errors;

    // If the list of commands contains 4 elements:
    if(cmd.size() == 4){
        // Append any eventual errors from each error handler to errors vector
        for (std::string e : validateName(cmd[1])) errors.push_back(e);
        for (std::string e : validateSurname(cmd[2])) errors.push_back(e);
        for (std::string e : validateAge(cmd[3])) errors.push_back(e);
    } else { // If there are not 4 elements in command string, append this error
        errors.push_back("3 arguments needed: Name Surname Age");
    }

    // Print all accumulated errors, if any
    for (std::string error : errors) std::cout << error << " ";

    // Return true if there are no errors
    return (errors.empty());
}

int main(){
    std::vector<Student*> students;

    // FOR DEBUG PURPOSES //
    students.push_back(new Student("Test", "Testington", 30));
    students.push_back(new Student("John", "Doe", 50));

    executeCommands(students);
    return 0;
}
