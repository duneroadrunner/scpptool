#!/bin/bash

if [ -z "$LLVM_CONF" ]
then
    if [ -f /usr/bin/llvm-config ]
    then
        LLVM_CONF=/usr/bin/llvm-config
    else
        echo "The LLVM_CONF environment variable needs to be set to the llvm-config pathname."
        exit
    fi
else
    echo " "
fi
echo "LLVM_CONF is set to '$LLVM_CONF'"

CXXFLAGS="$($LLVM_CONF --cxxflags)"
LDFLAGS1="$($LLVM_CONF --ldflags)"
LDFLAGS2="$($LLVM_CONF --libs --system-libs)"

set -x

g++ $CXXFLAGS -fexceptions -std=c++17 -I./yaml-cpp/include -O0 -g3 -Wall -c -fmessage-length=0  -fvisibility-inlines-hidden -Wno-unused -Wno-attributes -Wno-deprecated-declarations -fPIC -MMD -MP -MF"scpptool.d" -MT"scpptool.o" -o "scpptool.o" "scpptool.cpp"

g++ $CXXFLAGS -fexceptions -std=c++17 -O0 -g3 -Wall -c -fmessage-length=0  -fvisibility-inlines-hidden -Wno-unused -Wno-attributes -Wno-deprecated-declarations -fPIC -MMD -MP -MF"utils1.d" -MT"utils1.o" -o "utils1.o" "utils1.cpp"

g++ scpptool.o utils1.o  $LDFLAGS1 -Wl,--start-group -lclangAST -lclangAnalysis -lclangBasic\
 -lclangDriver -lclangEdit -lclangFrontend -lclangFrontendTool\
 -lclangLex -lclangParse -lclangSema -lclangASTMatchers\
 -lclangRewrite -lclangRewriteFrontend -lclangStaticAnalyzerFrontend\
 -lclangStaticAnalyzerCheckers -lclangStaticAnalyzerCore\
 -lclangSerialization -lclangToolingCore -lclangTooling -lstdc++\
 -lLLVMRuntimeDyld -lm -Wl,--end-group $LDFLAGS2  -lyaml-cpp  -o scpptool 

