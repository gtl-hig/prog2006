#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iterator>

void executeCommand();

int main(){
    executeCommand();
    return 0;
}

void executeCommand(){
    std::string in;  /*Declare and read input*/   getline (std::cin, in);

    if(!size(in)) return; // Return if empty

    std::istringstream iss(in);
    std::vector<std::string> cmd((std::istream_iterator<std::string>(iss)), std::istream_iterator<std::string>());

    if(cmd[0].compare("new") == 0){

    }
    else if(cmd[0].compare("list") == 0){
        
    }
    else if(cmd[0].compare("end") == 0){
        
    }
    else {
        std::cout << "\nUnknown command\n";
    }
}