
#headers := scanner.h diag.h parser.h ast.h glaz.h
cppobjs := main.o diag.o misc.o parser.o ast-build.o ast-sub.o ast-type.o \
ast-pass2.o
cobjs := scanner.o

CC := clang
CXX := clang++

CFLAGS := -I/opt/local/include -D_GNU_SOURCE -D__STDC_LIMIT_MACROS \
-D__STDC_CONSTANT_MACROS  -fno-common

CXXFLAGS := -I/opt/local/include -D_GNU_SOURCE -D__STDC_LIMIT_MACROS \
-D__STDC_CONSTANT_MACROS -fno-common -Woverloaded-virtual -Wcast-qual

# for now
DEBUG = 1

ifeq ($(DEBUG),1)
	CFLAGS += -g
	CXXFLAGS += -g
else
	CFLAGS += -DNDEBUG
	CXXFLAGS += -DNDEBUG
endif

LLVM_LIBS := -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen \
-lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMMCParser -lLLVMX86AsmPrinter \
-lLLVMX86Info -lLLVMJIT -lLLVMExecutionEngine -lLLVMCodeGen -lLLVMScalarOpts \
-lLLVMInstCombine -lLLVMTransformUtils -lLLVMipa -lLLVMAnalysis -lLLVMTarget \
-lLLVMMC -lLLVMCore -lLLVMSupport -lLLVMSystem

all: glazc

# source file and header dependencies
scanner.c : scanner.re scanner.h
	re2c -o scanner.c scanner.re

#ast.h : scanner.h
#diag.h :
#glaz.h :
#parser.h : scanner.h diag.h
#scanner.h :

main.o : scanner.h parser.h scanner.h ast.h
diag.o : diag.h
misc.o : glaz.h
parser.o : parser.h scanner.h
ast-build.o : glaz.h ast.h scanner.h
ast-pass2.o : glaz.h ast.h scanner.h
ast-sub.o : glaz.h ast.h scanner.h
ast-type.o : glaz.h ast.h scanner.h

$(cobjs) : %.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(cppobjs) : %.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# add $(LLVM_LIBS) when needed.
glazc : $(cppobjs) $(cobjs)
	$(CXX) -o glazc $^

.PHONY : clean
clean :
	rm -f glazc
	rm -f *.o

