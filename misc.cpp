#include "glaz.h"
#include <boost/unordered_map.hpp>
#include <string>

namespace glaz {

namespace {
boost::unordered_map<std::string, int> *commands_map;
}

extern "C" {
    void commands_put(const char *word, int id) {
        if (!commands_map) {
            commands_map = new boost::unordered_map<std::string, int>();
            commands_map[0][word] = id;
        } else {
            if (commands_map->find(word) == commands_map->end())
                commands_map[0][word] = id;
        }
    }
    
    int commands_get(const char *word) {
        if (!commands_map)
            return 0;
        
        boost::unordered_map<std::string, int>::const_iterator
            entry = commands_map->find(word);
        if (entry == commands_map->end())
            return 0;
        return entry->second;
    }
    
    int commands_get_from_range(const char *wordbegin, const char *wordend) {
        if (!commands_map)
            return 0;
        
        std::string word(wordbegin, (size_t)(wordend-wordbegin));

        boost::unordered_map<std::string, int>::const_iterator
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

