
#headers := scanner.h diag.h parser.h ast.h glaz.h
cppobjs := main.o diag.o misc.o parser.o ast-build.o ast-sub.o ast-type.o \
ast-pass2.o ast-expr.o ast-container.o ast-gen-llvm.o
cobjs := scanner.o

CC := clang
CXX := clang++

VPATH := ../..

CFLAGS := -I/Users/parkovski/Projects/llvm-2.9/include \
-I/Users/parkovski/Projects/llvm/include \
-I/Users/parkovski/Projects/boost_1_47_0 -D_GNU_SOURCE -fno-common

CXXFLAGS := -I/Users/parkovski/Projects/llvm-2.9/include \
-I/Users/parkovski/Projects/llvm/include \
-I/Users/parkovski/Projects/boost_1_47_0 \
-D_GNU_SOURCE -D__STDC_LIMIT_MACROS \
-D__STDC_CONSTANT_MACROS -fno-common -Woverloaded-virtual -Wcast-qual \
-fno-rtti -fno-exceptions

# for now
DEBUG := 1

ifeq ($(DEBUG),1)
	CFLAGS += -g
	CXXFLAGS += -g
else
	CFLAGS += -DNDEBUG -O3
	CXXFLAGS += -DNDEBUG -O3
endif

LLVM_LIBS := -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen \
-lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMMCParser -lLLVMX86AsmPrinter \
-lLLVMX86Utils -lLLVMX86Info -lLLVMJIT -lLLVMExecutionEngine -lLLVMCodeGen \
-lLLVMScalarOpts -lLLVMInstCombine -lLLVMTransformUtils -lLLVMipa \
-lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMCore -lLLVMSupport -lLLVMipo


all: glazc

# source file and header dependencies
../../scanner.c : scanner.re
	re2c -o $@ $<

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
ast-sub.o : glaz.h ast.h
ast-type.o : glaz.h ast.h 
ast-xml.o : glaz.h ast.h
ast-expr.o : glaz.h ast.h ast-conv.h
ast-container.o : glaz.h ast.h
ast-gen-llvm.o : glaz.h ast.h
#ast-conv.o : glaz.h ast-conv.h

$(cobjs) : %.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(cppobjs) : %.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

glazc : $(cppobjs) $(cobjs)
	$(CXX) -o glazc $^ -L~/Projects/llvm/lib/Debug $(LLVM_LIBS)

.PHONY : clean cleanall
clean :
	rm -f glazc
	rm -f *.o
	
cleanall : clean
	rm -f ../../scanner.c

