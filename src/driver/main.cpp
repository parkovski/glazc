#include <iostream>
#include <fstream>
#include <cstring>
#include <memory>
#include <direct.h>

#include "frontend/scanner.h"
#include "frontend/parser.h"
#include "frontend/ast.h"
#include "frontend/glaz.h"

/*
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
#include <llvm/Support/TargetSelect.h>
#include <llvm/DataLayout.h>
*/

using namespace std;
using namespace glaz;

// from misc.cpp
namespace glaz {
extern void globalCleanup();
}

void printtree(Token*);
void printnode(Token*);

void printhelp() {
    cout << "usage: glazc <options> <input files>\n\n"
        "\tinput files = *.gb, *.iba, *.cba\n\n"
        "valid options:\n"
        "-h\t\t\tprint help\n"
        "-emit:<option>\t\tset emitter. valid options are: parsetree, ast,\n"
        "\t\t\tcomponent, exe\n"
        "-component:<filename>\timport a component into this program\n"
        ;
}

enum EmitterMode {
    RUN,       // Runs the program with the LLVM JIT.
    PARSETREE, // Prints the parse tree (lisp style) and quits.
    AST,       // Prints the AST and quits.
    COMPONENT, // Outputs a component that can be used in other programs.
    EXE        // Output a fully compiled program.
};

// Exports to get the command line arguments from a GB program.
extern "C" {

static int global_argc;
static char **global_argv;

EXPORTFUNC
int GetArgCount() {
    return global_argc;
}

EXPORTFUNC
char *GetArg(int n) {
    if (n >= global_argc) {
        return "";
    }
    return global_argv[n];
}

}

int main(int argc, char *argv[]) {
    global_argc = argc;
    global_argv = argv;

    const char *filename = 0;
    EmitterMode emitter = RUN;
    bool dump_llvm = false;
    bool prompt_filename = false;
    string inp_filename;
    
    for (int i = 1; i < argc; i++) {
        int len = strlen(argv[i]);
        
        if (argv[i][0] == '-') {
            if (!strcmp(argv[i], "-h")) {
                printhelp();
                return 0;
            } else if (!strcmp(argv[i], "-ll")) {
                dump_llvm = true;
            } else if (!strcmp(argv[i], "-getfn")) {
                prompt_filename = true;
            } else if (len > 6 && !memcmp(argv[i], "-emit:", 6)) {
                if (len == 15 && !memcmp(&argv[i][6], "component", 9))
                    emitter = COMPONENT;
                else if (len == 15 && !memcmp(&argv[i][6], "parsetree", 9))
                    emitter = PARSETREE;
                else if (len == 9 && !memcmp(&argv[i][6], "ast", 3))
                    emitter = AST;
                else if (len == 9 && !memcmp(&argv[i][6], "exe", 3))
                    emitter = EXE;
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

    if (prompt_filename) {
        std::unique_ptr<char, decltype(std::free)*> cwd(_getcwd(nullptr, 0), std::free);
        cout << "current directory: " << cwd.get() << "\n";
        cout << "input filename: ";
        cin >> inp_filename;
        if (!inp_filename.empty()) {
            filename = inp_filename.c_str();
        }
    }
    
    if (!filename) {
        cout << "error: required input file" << endl;
        return 1;
    }
        
    std::unique_ptr<Parser> parser(Parser::create(filename));
    if (!parser) {
        cout << "couldn't create parser for " << filename << "\n";
        return 1;
    }
    
    Token *tree = parser->parse();
    
    if (parser->failed()) {
        token_free_all(tree);
    } else {
        
        if (emitter == PARSETREE) {
            Token *node = tree;
            while (node) {
                printnode(node);
                cout << endl;
                node = node->next;
            }
        }
        else if (emitter == AST) {
            std::unique_ptr<Component> c(Component::fromTree(tree, true));
            cout << c->toString() << endl;
        }
        else {
            cout << "sorry this emitter isn't currently supported!" << endl;
        }

        //Component *c = Component::fromTree(tree, true);
        //delete c;
        /*
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
            pm.add(llvm::createBasicAliasAnalysisPass());
            pm.add(llvm::createConstantPropagationPass());
            pm.add(llvm::createFunctionInliningPass());
            pm.add(llvm::createDeadCodeEliminationPass());
            pm.add(llvm::createReassociatePass());
            pm.add(llvm::createGVNPass());
            pm.add(llvm::createInstructionCombiningPass());
            pm.add(llvm::createCFGSimplificationPass());
            pm.add(llvm::createConstantMergePass());
            if (dump_llvm) {
                pm.add(llvm::createPrintModulePass(&llvm::outs()));
            }
            pm.run(*mod);
            
            void *fptr = engine->getPointerToFunction(main_f);
            
            void (*pmain)() = (void(*)())(fptr);
            
            // alright, here it goes...
            if (dump_llvm) {
                cout << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n";
            }
            pmain();
            
            // have to flush stdout for now.
            cout << endl;
            
            delete engine;
        }
        
        delete c;*/
    }
        
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

