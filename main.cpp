#include <iostream>
#include <fstream>
#include <getopt.h>
#include <string.h>

#include "scanner.h"
#include "parser.h"
#include "ast.h"

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/PassManager.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/LLVMContext.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Target/TargetSelect.h>
#include <llvm/Target/TargetData.h>

using namespace std;
using namespace glaz;

// from misc.cpp
namespace glaz {
extern void globalCleanup();
}

extern void llvmtest();
void printtree(Token*);
void printnode(Token*);

void printhelp() {
    cout << "usage: glazc <options> <input files>\n\n"
        "\tinput files = *.gb, *.iba, *.cba\n\n"
        "valid options:\n"
        "-h\t\t\tprint help\n"
        "-emit:<option>\t\tset emitter. valid options are: parsetree, ast,\n"
        "\t\t\tcomponent, llvm, asm\n"
        "-component:<filename>\timport a component into this program\n"
        ;
}

enum EmitterMode {
    PARSETREE,
    AST,
    COMPONENT,
    LLVM,
    ASM
};

int main(int argc, char *argv[]) {
    const char *filename = 0;
    
    EmitterMode emitter = ASM;
    
    for (int i = 1; i < argc; i++) {
        int len = strlen(argv[i]);
        
        if (argv[i][0] == '-') {
            if (!strcmp(argv[i], "-h")) {
                printhelp();
                return 0;
            } else if (len > 6 && !memcmp(argv[i], "-emit:", 6)) {
                if (len == 15 && !memcmp(&argv[i][6], "component", 9))
                    emitter = COMPONENT;
                else if (len == 10 && !memcmp(&argv[i][6], "llvm", 4))
                    emitter = LLVM;
                else if (len == 15 && !memcmp(&argv[i][6], "parsetree", 9))
                    emitter = PARSETREE;
                else if (len == 9 && !memcmp(&argv[i][6], "ast", 3))
                    emitter = AST;
                else if (len == 9 && !memcmp(&argv[i][6], "asm", 3))
                    emitter = ASM;
                else
                    cout << "invalid argument to -emit: `" <<
                        &argv[i][6] << "'\n";
            } else {
                cout << "invalid option: " << argv[i] << "\n";
            }
        } else if (len > 3 &&
            (!memcmp(argv[i] + len - 3, ".gb", 3) ||
            !memcmp(argv[i] + len - 4, ".iba", 4) ||
            !memcmp(argv[i] + len - 4, ".cba", 4))) {
            
            filename = argv[i];
        } else {
            cout << "invalid option or unrecognized filetype: " <<
                argv[i] << endl;
        }
    }
    
    if (!filename) {
        //cout << "error: required input file" << endl;
        //return 1;
        filename = "../../test.gb";
    }
        
    Parser *parser = Parser::create(filename);
    if (!parser) {
        cout << "couldn't create parser for " << filename << "\n";
        return 1;
    }
    
    Token *tree = parser->parse();
    
    if (parser->failed()) {
        token_free_all(tree);
    } else {
        Token *node = tree;
        
        if (emitter == PARSETREE) {
            while (node) {
                printnode(node);
                cout << endl;
                node = node->next;
            }
        }
        
        Component *c = Component::fromTree(tree, true);
        
        if (c) {
            llvm::Module *mod = c->getLlvmModule();
            
            llvm::Function *main_f = llvm::cast<llvm::Function>(
                c->getMain()->getLlvmFunction(c->getLlvmContext(), mod)
            );
        
            // Run the program
            llvm::verifyModule(*mod, llvm::PrintMessageAction);
            
            llvm::InitializeNativeTarget();
            llvm::ExecutionEngine *engine = llvm::EngineBuilder(mod).create();
            
            llvm::PassManager pm;
            pm.add(new llvm::TargetData(*engine->getTargetData()));
            pm.add(llvm::createBasicAliasAnalysisPass());
            pm.add(llvm::createConstantPropagationPass());
            pm.add(llvm::createFunctionInliningPass());
            pm.add(llvm::createDeadCodeEliminationPass());
            pm.add(llvm::createReassociatePass());
            pm.add(llvm::createGVNPass());
            pm.add(llvm::createInstructionCombiningPass());
            pm.add(llvm::createCFGSimplificationPass());
            pm.add(llvm::createConstantMergePass());
            pm.add(llvm::createPrintModulePass(&llvm::outs()));
            pm.run(*mod);
            
            void *fptr = engine->getPointerToFunction(main_f);
            
            void (*pmain)() = (void(*)())(fptr);
            
            // alright, here it goes...
            cout << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n";
            pmain();
            
            // have to flush stdout for now.
            cout << endl;
            
            delete engine;
        }
        
        delete c;
    }
        
    delete parser;
    
    globalCleanup();
    
    return 0;
}

void printnode(Token *node) {
    if (node->child) {
        cout << "(" << node->text << " ";
        printtree(node->child);
        cout << ")";
    } else {
        cout << node->text;
    }
}

void printtree(Token *node) {
    while (node) {
        if (node->child) {
            cout << "(";
            cout << node->text << " ";
            printtree(node->child);
            cout << ")";
        } else {
            cout << node->text;
        }
        node = node->next;
        if (node)
            cout << " ";
    }
}

