#include <algorithm>
#include <cctype>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <valarray>
#include <vector>

/**
 * Class to hold data for a single student
 */
struct Student
{
    std::string name{};
    std::string surname{};
    std::uint8_t age{};
};

void executeCommands(std::vector<Student>& students);
void printStudents(const std::vector<Student>& students);
bool isEntirelyAlphabetic(const std::string& word);
void validateName(const std::string& name, std::vector<std::string>& errors);
void validateSurname(const std::string& name, std::vector<std::string>& errors);
void validateAge(const std::string& name, std::vector<std::string>& errors);
bool checkForErrors(const std::vector<std::string>& cmd);

/**
 *  Reads user input, checks for errors, and
 *  responds accordingly
 *  @param students std::vector<Student*>&
 */
void executeCommands(std::vector<Student>& students)
{
    while (true)
    {
        std::string in{}; // Declare and read input
        getline(std::cin, in);

        if (in.empty())
        {
            continue;
        }

        std::istringstream iss{in};
        auto cmd = std::vector<std::string>(std::istream_iterator<std::string>(iss), std::istream_iterator<std::string>());

        if (cmd[0] == "new")
        {
            if (checkForErrors(cmd))
            {
                students.emplace_back(Student{cmd[1], cmd[2], static_cast<std::uint8_t>(std::stoi(cmd[3]))});
            }
        }
        else if (cmd[0] == "list")
        {
            printStudents(students);
        }
        else if (cmd[0] == "end")
        {
            break;
        }
        else
        {
            std::cout << "Unknown command\n";
        }
    }
}

/**
 *  Uses the validation-functions to check
 *  command string for errors, returns true if no errors
 *  @param students std::vector<Student*>&
 */
void printStudents(const std::vector<Student>& students)
{
    for (const auto& s : students)
    {
        std::cout << s.name << ", " << s.surname << ", " << std::to_string(s.age) << "\n";
    }
}

/**
 *  Checks that the given word consists only of alphabetical characters
 *  @param word
 *  @return bool
 */
bool isEntirelyAlphabetic(const std::string& word)
{
    return std::all_of(word.cbegin(), word.cend(), [](auto&& letter) { return isalpha(letter); });
}

/**
 *  Checks that the given name is in the valid format
 *  and returns a vector of any errors that might occur
 *  @param name std::string&
 *  @param errors
 */
void validateName(const std::string& name, std::vector<std::string>& errors)
{
    // Check if name is capitalized
    if (!isupper(name[0]))
    {
        errors.push_back("Name must be Capitalized");
    }

    // Check if name is longer than two letters
    if (name.size() < 2)
    {
        errors.push_back("Name too short");
    }

    // Check if name only contains letters
    if (!isAlphabetic(name))
    {
        errors.push_back("Name can only have letters");
    }
}

/**
 *  Checks that the given surname is in the valid format
 *  and returns a vector of any errors that might occur
 *  @param surname std::string&
 *  @param errors
 */
void validateSurname(const std::string& surname, std::vector<std::string>& errors)
{
    // Check if surname is capitalized
    if (!isupper(surname[0]))
    {
        errors.push_back("Surname must be Capitalized");
    }

    // Check if surname is longer than two letters
    if (surname.size() < 4)
    {
        errors.push_back("Surname too short");
    }

    // Check if surname only contains letters
    if (!isAlphabetic(surname))
    {
        errors.push_back("Surname only letters allowed");
    }
}

/**
 *  Checks that the given age is in the valid format
 *  and returns a vector of any errors that might occur
 *  @param age std::string&
 *  @return std::vector<std::string>
 */
void validateAge(const std::string& age, std::vector<std::string>& errors)
{
    int iage;

    // Check if parsing to int is possible
    try
    {
        iage = std::stoi(age);
    }
    catch (...)
    {
        errors.push_back("Age must be a number");
    }

    // Check if age is between 18 and 130
    if (iage < 18 || iage > 130)
    {
        errors.push_back("Age outside valid range");
    }
}

/**
 *  Uses the validation-functions to check
 *  command string for errors, returns true if no errors
 *  @param cmd std::vector<std::string>&
 *  @return bool
 */
bool checkForErrors(const std::vector<std::string>& cmd)
{
    std::vector<std::string> errors;

    // If the list of commands contains 4 elements:
    if (cmd.size() == 4)
    {
        // Append any eventual errors from each error handler to errors vector
        validateName(cmd[1], errors);
        validateSurname(cmd[2], errors);
        validateAge(cmd[3], errors);
    }
    else
    { // If there are not 4 elements in command string, append this error
        errors.push_back("[3 arguments needed: Name Surname Age]");
    }

    // Print all accumulated errors, if any
    if (!errors.empty())
    {
        std::cout << "[";
        for (const auto& error : errors)
        {
            std::cout << error << (error == errors.back() ? "" : ", ");
        }
        std::cout << "]\n";
    }

    // Return true if there are no errors
    return (errors.empty());
}

/**
 *  Program entry point
 */
int main()
{
    std::vector<Student> students;

    executeCommands(students);
    return 0;
}
