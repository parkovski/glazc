#include "glaz.h"
#include <unordered_map>
#include <string>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/predicate.hpp>

namespace glaz {

namespace {
std::unordered_map<std::string, int> *commands_map;
}

extern "C" {
    void commands_put(const char *word, int id) {
        std::string lword(getIdLookupString(std::string(word)));
        if (!commands_map) {
            commands_map = new std::unordered_map<std::string, int>();
            commands_map[0][lword] = id;
        } else {
            if (commands_map->find(lword) == commands_map->end())
                commands_map[0][lword] = id;
        }
    }
    
    int commands_get(const char *word) {
        if (!commands_map)
            return 0;

        std::string lword(getIdLookupString(std::string(word)));
        
        std::unordered_map<std::string, int>::const_iterator
            entry = commands_map->find(lword);
        if (entry == commands_map->end())
            return 0;
        return entry->second;
    }
    
    int commands_get_from_range(const char *wordbegin, const char *wordend) {
        if (!commands_map)
            return 0;
        
        std::string word(getIdLookupString(std::string(wordbegin, wordend)));

        std::unordered_map<std::string, int>::const_iterator
            entry = commands_map->find(word);
        if (entry == commands_map->end())
            return 0;
        
        return entry->second;
    }
}

void globalCleanup() {
    delete commands_map;
}

static bool caseInsensitiveMode = true;

void setCaseInsensitiveMode(bool caseInsensitive) {
    caseInsensitiveMode = caseInsensitive;
}

bool getCaseInsensitiveMode() {
    return caseInsensitiveMode;
}

std::string getIdLookupString(const std::string &str) {
    if (caseInsensitiveMode) {
        return boost::to_lower_copy(str);
    }
    return str;
}

bool strEqualIdLookup(const std::string &str1, const std::string &str2) {
    if (caseInsensitiveMode) {
        return boost::iequals(str1, str2);
    }
    return str1 == str2;
}

} // end namespace glaz

