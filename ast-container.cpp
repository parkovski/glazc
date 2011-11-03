#include "glaz.h"
#include "ast.h"

using namespace glaz;

// Need somewhere to put the implementation for the pure virtual destructor...
InOrderNode::~InOrderNode() {
}

IfBlock::~IfBlock() {
    InOrderNode *node, *temp;
    
    node = first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
    
    node = else_first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}

void IfBlock::addStatement(InOrderNode *node) {
    if (nested) {
        nested->addStatement(node);
        return;
    }
    
    if (in_else) {
        if (!else_first)
            else_first = else_last = node;
        else
            else_last = else_last->next = node;
    } else {
        if (!first)
            first = last = node;
        else
            last = last->next = node;
    }
}

bool IfBlock::enterElse() {
    if (nested) {
        return nested->enterElse();
    }
    
    if (in_else)
        return false;
    in_else = true;
    return true;
}

bool IfBlock::enterElseIf(IfBlock *elseif) {
    if (nested) {
        bool result = nested->enterElseIf(elseif);
        nested = elseif;
        return result;
    }
    
    assert(!in_else && "elseif shouldn't come before else");
    
    in_else = true;
    else_first = else_last = nested = elseif;
    return true;
}

CaseBlock::~CaseBlock() {
    InOrderNode *node, *temp;
    
    node = first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}

void CaseBlock::addStatement(InOrderNode *node) {
    if (first) {
        last = last->next = node;
    } else {
        first = last = node;
    }
}

SelectBlock::~SelectBlock() {
    CaseBlock *node, *temp;
    
    node = first_case;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}
    
void SelectBlock::addStatement(InOrderNode *node) {
    assert(last_case && "you can only insert stuff when there's a case block");
    last_case->addStatement(node);
}

bool SelectBlock::enterCase(CaseBlock *c) {
    in_default = false;
    
    // TODO: check if c's expression is constant or not.
    
    if (first_case) {
        last_case = last_case->next = c;
    } else {
        first_case = last_case = c;
    }
    
    ++nr_of_cases;
    return true;
}

bool SelectBlock::enterDefault(CaseBlock *c) {  
    in_default = true;
    
    if (first_case) {
        last_case = last_case->next = c;
    } else {
        first_case = last_case = c;
    }
    
    ++nr_of_cases;
    return true;
}

ForBlock::~ForBlock() {
    InOrderNode *node, *temp;
    
    node = first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}

void ForBlock::addStatement(InOrderNode *node) {
    if (first) {
        last = last->next = node;
    } else {
        first = last = node;
    }
}

WhileBlock::~WhileBlock() {
    InOrderNode *node, *temp;
    
    node = first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}

void WhileBlock::addStatement(InOrderNode *node) {
    if (first) {
        last = last->next = node;
    } else {
        first = last = node;
    }
}

DoBlock::~DoBlock() {
    InOrderNode *node, *temp;
    
    node = first;
    while (node) {
        temp = node->next;
        delete node;
        node = temp;
    }
}

void DoBlock::addStatement(InOrderNode *node) {
    if (first) {
        last = last->next = node;
    } else {
        first = last = node;
    }
}

