#include "glaz.h"
#include "ast.h"
#include <boost/algorithm/string/case_conv.hpp>

using namespace glaz;

Sub::~Sub() {
    for (InOrderNode *stmt = first, *temp; stmt; stmt = temp) {
        temp = stmt->next;
        delete stmt;
    }
}

bool Sub::addParamOrLocal(const std::string name, Var *var) {
    std::string lower = boost::to_lower_copy(name);
    var_map::const_iterator entry = vars.find(lower);
    if (entry != vars.end())
        return false;
        
    vars[lower] = var;
    varlist.push_back(var);
    return true;
}

Var *Sub::getVar(const std::string name) const {
    std::string lower = boost::to_lower_copy(name);
    
    // Is it in locals/params?
    var_map::const_iterator entry = vars.find(lower);
    if (entry != vars.end())
        return entry->second;
    
    // Not found
    return 0;
}

Var *Sub::getVar(unsigned int index) const {
    if (index >= varlist.size())
        return 0;
    
    return varlist[index];
}

bool Sub::setVarName(std::string oldname, std::string newname) {
    std::string oldlower = boost::to_lower_copy(oldname);
    std::string newlower = boost::to_lower_copy(newname);
    
    var_map::iterator entry = vars.find(oldlower);
    if (entry == vars.end())
        return false;
    if (vars.find(newlower) != vars.end())
        return false;
        
    Var *var = entry->second;
    vars.erase(entry);
    var->setName(newname);
    vars[newlower] = var;
    
    return true;
}

bool Sub::addLabel(const std::string name) {
    std::string lower = boost::to_lower_copy(name);
    label_map::const_iterator entry = labels.find(lower);
    if (entry != labels.end())
        return false;
    
    labels[lower] = new Label(name);
    return true;
}

Label *Sub::getLabel(const std::string name) const {
    label_map::const_iterator entry =
        labels.find(boost::to_lower_copy(name));
    if (entry == labels.end())
        return 0;
    
    return entry->second;
}

bool Sub::enterContainer(Container *c) {
    if (c->getParent() != current_container)
        return false;
    current_container = c;
    return true;
}

bool Sub::exitContainer() {
    if (!current_container)
        return false;
    current_container = current_container->getParent();
    return true;
}

void Sub::addStatement(InOrderNode *stmt) {
    if (current_container) {
        current_container->addStatement(stmt);
        return;
    }
    
    if (!first) {
        first = last = stmt;
    } else {
        last = last->next = stmt;
    }
}

