#!/bin/bash

if [ -z "$LLVM_CONF" ]
then
    if [ -f /usr/bin/llvm-config ]
    then
        LLVM_CONF=/usr/bin/llvm-config
    else
        echo "The LLVM_CONF environment variable needs to be set to the llvm-config pathname. "
        echo "If you're using the clang+llvm pre-built binaries, then LLVM_CONF should be set to {the clang+llvm pre-built binaries directory}/bin/llvm-config. "
        exit
    fi
else
    echo " "
fi
echo "LLVM_CONF is set to '$LLVM_CONF'"

# While historically you could reliably use (even somewhat dated versions of) gcc/g++ to build programs that use the clang libraries, 
# recently it seems to have been more reliable to use the clang/clang++ (and ld.lld) executables included with the 
# clang+llvm pre-built binaries that we're linking with

CXX="$($LLVM_CONF --obj-root)/bin/clang++"
EXTRA_LD_FLAGS="-fuse-ld=$($LLVM_CONF --obj-root)/bin/ld.lld"

CXXFLAGS="$($LLVM_CONF --cxxflags)"
LDFLAGS1="$($LLVM_CONF --ldflags)"
LDFLAGS2="$($LLVM_CONF --libs --system-libs)"

set -x

$CXX $CXXFLAGS -fexceptions -std=c++17 -I./yaml-cpp/include -O0 -g3 -Wall -c -fmessage-length=0  -fvisibility-inlines-hidden -Wno-unused -Wno-attributes -Wno-deprecated-declarations -fPIC -MMD -MP -MF"scpptool.d" -MT"scpptool.o" -o "scpptool.o" "scpptool.cpp"

$CXX $CXXFLAGS -fexceptions -std=c++17 -O0 -g3 -Wall -c -fmessage-length=0  -fvisibility-inlines-hidden -Wno-unused -Wno-attributes -Wno-deprecated-declarations -fPIC -MMD -MP -MF"utils1.d" -MT"utils1.o" -o "utils1.o" "utils1.cpp"

$CXX $EXTRA_LD_FLAGS scpptool.o utils1.o  $LDFLAGS1 -Wl,--start-group -lclangAPINotes -lclangAST -lclangAnalysis -lclangBasic\
 -lclangDriver -lclangEdit -lclangFrontend -lclangFrontendTool\
 -lclangLex -lclangParse -lclangSema -lclangASTMatchers\
 -lclangRewrite -lclangRewriteFrontend -lclangStaticAnalyzerFrontend\
 -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore\
 -lclangSerialization -lclangToolingCore -lclangTooling -lclangToolingSyntax -lstdc++\
 -lLLVMRuntimeDyld -lm -Wl,--end-group $LDFLAGS2  -lclangSupport -lyaml-cpp  -o scpptool 

