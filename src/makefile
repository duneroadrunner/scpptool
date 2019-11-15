TARGET=scpptool
SHELL=bash
SHELL?=bash
CC=gcc
CC?=gcc
CFLAGS=-fpic -std=c11
CXX=g++
CXX?=g++
CXX_FLAGS=-fpic
CXX_EXTRA?=
CTAGS_I_PATH?=./
LD_FLAGS=
EXTRA_LD_FLAGS?=
ADD_SANITIZERS_CC= -g -fsanitize=address -fno-omit-frame-pointer
ADD_SANITIZERS_LD= -g -fsanitize=address
MEM_SANITIZERS_CC= -g -fsanitize=memory -fno-omit-frame-pointer
MEM_SANITIZERS_LD= -g -fsanitize=memory
UB_SANITIZERS_CC= -g -fsanitize=undefined -fno-omit-frame-pointer
UB_SANITIZERS_LD= -g -fsanitize=undefined
COV_CXX= -fprofile-instr-generate -fcoverage-mapping
COV_LD= -fprofile-instr-generate
# BUILD_MODES are=RELEASE(default), DEBUG,ADDSAN,MEMSAN,UBSAN
BUILD_MODE?=RELEASE
OBJ_LIST:=$(patsubst %.cpp, %.o, $(wildcard *.cpp))
ASM_LIST:=$(patsubst %.cpp, %.dis, $(wildcard *.cpp))

LLVM_CONF?=llvm-config
LLVM_CXX_FLAGS=$(shell $(LLVM_CONF) --cxxflags)
LLVM_CXX_FLAGS+=-I$(shell $(LLVM_CONF) --src-root)/tools/clang/include\
 -I$(shell $(LLVM_CONF) --obj-root)/tools/clang/include\
 -std=c++17
# -stdlib=libstdc++ -std=c++17 -frtti -fexceptions
ifeq ($(CXX), clang++)
LLVM_CXX_FLAGS+=-stdlib=libstdc++
endif
LLVM_LD_FLAGS=-Wl,--start-group -lclangAST -lclangAnalysis -lclangBasic\
 -lclangDriver -lclangEdit -lclangFrontend -lclangFrontendTool\
 -lclangLex -lclangParse -lclangSema -lclangASTMatchers\
 -lclangRewrite -lclangRewriteFrontend -lclangStaticAnalyzerFrontend\
 -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore\
 -lclangSerialization -lclangToolingCore -lclangTooling -lstdc++\
 -lLLVMRuntimeDyld -lm -Wl,--end-group
LLVM_LD_FLAGS+=$(shell $(LLVM_CONF) --ldflags --libs --system-libs)

CXX_FLAGS+=$(LLVM_CXX_FLAGS)
LD_FLAGS+=$(LLVM_LD_FLAGS)

#MAKEFLAGS+=--warn-undefined-variables
ifeq ($(BUILD_MODE), ADDSAN)
ifeq ($(CXX), g++)
$(error This build mode is only useable with clang++.)
endif
CXX_EXTRA+=$(ADD_SANITIZERS_CC)
EXTRA_LD_FLAGS+=$(ADD_SANITIZERS_LD)
endif

ifeq ($(BUILD_MODE), MEMSAN)
ifeq ($(CXX), g++)
$(error This build mode is only useable with clang++.)
endif
CXX_EXTRA+=$(MEM_SANITIZERS_CC)
EXTRA_LD_FLAGS+=$(MEM_SANITIZERS_LD)
endif

ifeq ($(BUILD_MODE), UBSAN)
ifeq ($(CXX), g++)
$(error This build mode is only useable with clang++.)
endif
CXX_EXTRA+=$(UB_SANITIZERS_CC)
EXTRA_LD_FLAGS+=$(UB_SANITIZERS_LD)
endif

ifeq ($(BUILD_MODE), DEBUG)
CXX_EXTRA+=-g
endif

SRCS:=$(wildcard *.cpp)
CXX_FLAGS+=$(CXX_EXTRA)
LD_FLAGS+=$(EXTRA_LD_FLAGS)

.DEFAULT:all

.PHONY:all clean help ASM SO TAGS

all:$(TARGET)

everything:$(TARGET) ASM SO A $(TARGET)-dbg TAGS $(TARGET)-cov

depend:.depend

.depend:$(SRCS)
	rm -rf .depend
	$(CXX) -MM $(CXX_FLAGS) $^ > ./.depend
	echo $(patsubst %.o:, %.odbg:, $(shell $(CXX) -MM $(CXX_FLAGS) $^)) | sed -r 's/[A-Za-z0-9\_\-]+\.odbg/\n&/g' >> ./.depend
	echo $(patsubst %.o:, %.ocov:, $(shell $(CXX) -MM $(CXX_FLAGS) $^)) | sed -r 's/[A-Za-z0-9\_\-]+\.ocov/\n&/g' >> ./.depend

-include ./.depend

.cpp.o:
	$(CXX) $(CXX_FLAGS) -c $< -o $@

%.odbg:%.cpp
	$(CXX) $(CXX_FLAGS) -g -c $< -o $@

%.ocov:%.cpp
	$(CXX) $(CXX_FLAGS) $(COV_CXX) -c $< -o $@

$(TARGET): $(TARGET).o ./utils1.o
	$(CXX) $^ $(LD_FLAGS) -o $@
$(TARGET)-static: $(TARGET).o ./utils1.o
	$(CXX) $^ $(LD_FLAGS) -static -o $@

$(TARGET)-dbg: $(TARGET).odbg ./utils1.odbg
	$(CXX) $^ $(LD_FLAGS) -g -o $@

$(TARGET)-cov: $(TARGET).ocov ./utils1.ocov
	$(CXX) $^ $(LD_FLAGS) $(COV_LD) -o $@

cov:
	@llvm-profdata merge -sparse ./default.profraw -o ./default.profdata
	@llvm-cov show $(TARGET)-cov -instr-profile=default.profdata

covrep:
	@llvm-profdata merge -sparse ./default.profraw -o ./default.profdata
	@llvm-cov report $(TARGET)-cov -instr-profile=default.profdata

ASM:$(ASM_LIST)

SO:$(TARGET).so

A:$(TARGET).a

TAGS:tags

tags:$(SRCS)
	$(shell $(CXX) -c $(shell $(LLVM_CONF) --cxxflags) -I$(shell $(LLVM_CONF) --src-root)/tools/clang/include -I$(shell $(LLVM_CONF) --obj-root)/tools/clang/include -I $(CTAGS_I_PATH) -M $(SRCS)|\
		sed -e 's/[\\ ]/\n/g'|sed -e '/^$$/d' -e '/\.o:[ \t]*$$/d'|\
		ctags -L - --c++-kinds=+p --fields=+iaS --extra=+q)

%.dis: %.o
	objdump -r -d -M intel -S $< > $@

$(TARGET).so: $(TARGET).o
	$(CXX) $^ $(LD_FLAGS) -shared -o $@

$(TARGET).a: $(TARGET).o
	ar rcs $(TARGET).a $(TARGET).o

clean:
	rm -f *.o *.dis *.odbg *.ocov *~ $(TARGET) $(TARGET).so $(TARGET)-static $(TARGET)-dbg $(TARGET).a $(TARGET)-cov

deepclean:
	rm -f *.o *.dis *.odbg *.ocov *~ $(TARGET) $(TARGET).so tags $(TARGET)-static $(TARGET)-dbg $(TARGET).a $(TARGET)-cov FILE*.cpp FILE*.hpp
	rm .depend

help:
	@echo "--all is the default target, runs $(TARGET) target"
	@echo "--everything will build everything"
	@echo "--SO will generate the so"
	@echo "--ASM will generate assembly files"
	@echo "--TAGS will generate tags file"
	@echo "--$(TARGET) builds the dynamically-linked executable"
	@echo "--$(TARGET)-dbg will generate the debug build. BUILD_MODE should be set to DEBUG to work"
	@echo "--$(TARGET)-static will statically link the executable to the libraries"
	@echo "--$(TARGET)-cov is the coverage build"
	@echo "--cov will print the line coverage report"
	@echo "--covrep will print the coverage report"
	@echo "--A will build the static library"
	@echo "--TAGS will build the tags file"
	@echo "--clean"
	@echo "--deepclean will clean almost everything"
