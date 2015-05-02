#ifndef GLAZ_H
#define GLAZ_H

#include <assert.h>
#include <stdio.h>
#include <string>

#ifdef _MSC_VER
# define EXPORTFUNC __declspec(dllexport)
#elif defined(__GNUC__)
# define EXPORTFUNC __attribute__((visibility("default")))
#endif

namespace glaz {
    void setIdLookupCaseConversion(bool convertCase);
    bool getIdLookupCaseConversion();

    // Transforms an identifier into a standardized case format for lookups.
    // In case sensitive mode, this returns an identical copy of the string.
    // In case insensitive mode, it returns a lower case copy.
    std::string getIdLookupString(const std::string &str);

    // Compares strings for id lookup - if case sensitive mode is on, returns
    // true only if the strings are identical. If case insensitive mode is on,
    // returns true if the strings are equal ignoring case.
    bool strEqualIdLookup(const std::string &str1, const std::string &str2);
}

#endif        //  #ifndef GLAZ_H

