// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


/*Standard headers*/
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <algorithm>
#include <locale>

#include <cstdio>
#include <memory>
#include <array>

#include <fstream>

/*static */std::string g_mse_namespace_str = "mse";

#include "scpptool.h"
#include "utils1.h"

#include "checker.h"

//define EXCLUDE_CONVERTER_MODE1
#ifndef EXCLUDE_CONVERTER_MODE1
#include "converter_mode1.h"
#endif //!EXCLUDE_CONVERTER_MODE1

/*Clang Headers*/
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/AST/ASTImporter.h"
#include "clang/AST/ASTDiagnostic.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include "clang/Basic/SourceManager.h"

/*LLVM Headers*/
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"
/**********************************************************************************************************************/
/*used namespaces*/
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
/**********************************************************************************************************************/
static llvm::cl::OptionCategory MatcherSampleCategory("TBD");

cl::opt<bool> CheckSystemHeader("SysHeader", cl::desc("process system headers also"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> MainFileOnly("MainOnly", cl::desc("process the main file only"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> ConvertToSCPP("ConvertToSCPP", cl::desc("translate the source to a (memory) safe subset of the language"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> CTUAnalysis("CTUAnalysis", cl::desc("cross translation unit analysis"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> EnableNamespaceImport("EnableNamespaceImport", cl::desc("enable importing of namespaces from other translation units"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> SuppressPrompts("SuppressPrompts", cl::desc("suppress prompts before replacing source files"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> DoNotReplaceOriginalSource("DoNotReplaceOriginalSource", cl::desc("prevent replacement/modification of the original source files"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<std::string> MergeCommand("MergeCommand", cl::desc("specify an alternate merge tool to be used"), cl::init(""), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<bool> DoNotResolveMergeConflicts("DoNotResolveMergeConflicts", cl::desc("prevent the automatic resolution of merge conflicts (by heuristic guessing)"), cl::init(false), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);
cl::opt<std::string> ConvertMode("ConvertMode", cl::desc("specify the code conversion technique to use: \n"
  "\t Dual \t- The resulting code can be compiled as either safe C++ or (potentially faster) unsafe 'plain' C or C++. \n"
  "\t SlowAndFlexible \t- (Default) \n"
  "\t FasterAndStricter \t- The resulting (safe) code should be faster, but code that is not of 'good form' may not translate properly. \n"
  ), cl::init(""), cl::cat(MatcherSampleCategory), cl::ZeroOrMore);

/**********************************************************************************************************************/

class MyDiagConsumer : public DiagnosticConsumer {
  void anchor() {}

  void HandleDiagnostic(DiagnosticsEngine::Level DiagLevel, const Diagnostic &Info) override {
    llvm::SmallVector<char, 128> message;
    Info.FormatDiagnostic(message);
    llvm::errs() << message << '\n';
  }
};

/**********************************************************************************************************************/
/*Main*/
int main(int argc, const char **argv) 
{
  CommonOptionsParser op(argc, argv, MatcherSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());

  std::shared_ptr<DiagnosticConsumer> diag_consumer_shptr(new MyDiagConsumer());
  //Tool.setDiagnosticConsumer(diag_consumer_shptr.get());

  int retval = -1;

  if (true) {
    checker::Options options = {
          CheckSystemHeader,
          MainFileOnly,
          CTUAnalysis,
          EnableNamespaceImport,
          SuppressPrompts
      };
    retval = checker::buildASTs_and_run(Tool, options);
  }

  if (ConvertToSCPP.getValue()) {
#ifndef EXCLUDE_CONVERTER_MODE1

    /* The "checker" pass, among other things, determined which regions of the code are indicated
    to be excluded from the checks. The "convert" pass also needs this information. Rather than
    re-compute it, we'll copy it from the stored "states" of the checker pass. */
    for (const auto& checker_state : checker::g_final_tu_states) {
      convm1::CTUState convm1_state;
      convm1_state.m_suppress_check_region_set = checker_state.m_suppress_check_region_set;
      convm1::g_prepared_initial_tu_states.push_back(convm1_state);
    }

    convm1::Options options = {
          CheckSystemHeader,
          MainFileOnly,
          ConvertToSCPP,
          CTUAnalysis,
          EnableNamespaceImport,
          SuppressPrompts,
          DoNotReplaceOriginalSource,
          MergeCommand,
          DoNotResolveMergeConflicts,
          ConvertMode
      };
    retval = convm1::buildASTs_and_run(Tool, options);
#endif //!EXCLUDE_CONVERTER_MODE1
  }

  return retval;
}
/*last line intentionally left blank*/

