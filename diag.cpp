#include "diag.h"

#include <string>
#include <iostream>

using namespace glaz;

Diagnostics::Diagnostics() {
    maxErrors = 10; // just pick a default value...
}

bool Diagnostics::error(const char *str) {
    std::cout << str << std::endl;
    return maxErrors > 0 && ++errorCount >= maxErrors;
}

bool Diagnostics::error(const std::string &str) {
    std::cout << str << std::endl;
    return maxErrors > 0 && ++errorCount >= maxErrors;
}

void Diagnostics::warning(const char *str) {
    std::cout << str << std::endl;
}

void Diagnostics::warning(const std::string &str) {
    std::cout << str << std::endl;
}

