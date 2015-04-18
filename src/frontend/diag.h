#ifndef DIAG_H
#define DIAG_H

#include <string>

namespace glaz {

class Diagnostics {
    int maxErrors;
    int errorCount;
    
public:
    // default to using stdout
    Diagnostics();
    
    bool shouldExit() { return errorCount >= maxErrors; }
        
    // returns whether we should continue (have max errors been reached?)
    bool error(const char *str);
    bool error(const std::string &str);
    
    void warning(const char *str);
    void warning(const std::string &str);
};

} // namespace glaz

#endif        //  #ifndef DIAG_H

