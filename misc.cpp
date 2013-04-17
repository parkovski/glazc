#include "glaz.h"
#include <unordered_map>
#include <string>
#include <boost/algorithm/string/case_conv.hpp>

namespace glaz {

namespace {
std::unordered_map<std::string, int> *commands_map;
}

extern "C" {
    void commands_put(const char *word, int id) {
        std::string lword(word);
        boost::to_lower(lword);
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

        std::string lword(word);
        boost::to_lower(lword);
        
        std::unordered_map<std::string, int>::const_iterator
            entry = commands_map->find(lword);
        if (entry == commands_map->end())
            return 0;
        return entry->second;
    }
    
    int commands_get_from_range(const char *wordbegin, const char *wordend) {
        if (!commands_map)
            return 0;
        
        std::string word(wordbegin, wordend);
        boost::to_lower(word);

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

} // end namespace glaz

