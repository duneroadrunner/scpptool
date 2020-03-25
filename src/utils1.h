// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef __UTILS1_H
#define __UTILS1_H

/*Standard headers*/
#include <string>
#include <iostream>
#include <string>
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

/* Execute a shell command. */
std::pair<std::string, bool> exec(const char* cmd);

clang::SourceRange nice_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite);
clang::SourceRange instantiation_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite);

/* not necessarily a proper subset */
bool first_is_a_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second);

bool filtered_out_by_location(const clang::SourceManager &SM, clang::SourceLocation SL);

bool filtered_out_by_location(const clang::ast_matchers::MatchFinder::MatchResult &MR, clang::SourceLocation SL);

std::string with_whitespace_removed(const std::string& str);

std::string with_newlines_removed(const std::string& str);

/* No longer used. This function extracts the text of individual declarations when multiple
 * pointers are declared in the same declaration statement. */
std::vector<std::string> f_declared_object_strings(const std::string& decl_stmt_str);

std::string tolowerstr(const std::string& a);

bool string_begins_with(const std::string& s1, const std::string& prefix);
bool string_ends_with(const std::string& s1, const std::string& suffix);


/* This function returns a list of individual declarations contained in the same declaration statement
 * as the given declaration. (eg.: "int a, b = 3, *c;" ) */
std::vector<const clang::DeclaratorDecl*> IndividualDeclaratorDecls(const clang::DeclaratorDecl* DD, clang::Rewriter &Rewrite);


class COrderedSourceRange : public clang::SourceRange {
	public:
	typedef clang::SourceRange base_class;
	using base_class::base_class;
	COrderedSourceRange(const clang::SourceRange& src) : base_class(src.getBegin(), src.getEnd()) {}
};
inline bool operator==(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	return static_cast<const COrderedSourceRange::base_class &>(LHS) == static_cast<const COrderedSourceRange::base_class &>(RHS);
	//return LHS.getBegin() == RHS.getBegin();
}
inline bool operator!=(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	return !(LHS == RHS);
}
inline bool operator<(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	return (LHS.getBegin() < RHS.getBegin()) || ((LHS.getBegin() == RHS.getBegin()) && (LHS.getEnd() < RHS.getEnd()));
}
class CSuppressCheckRegionSet : public std::set<COrderedSourceRange> {
	public:
	typedef std::set<COrderedSourceRange> base_class;
	using base_class::base_class;
	bool contains(const clang::SourceRange& SR) const {
		for (auto it = (*this).cbegin(); (*this).cend() != it; it++) {
			if (first_is_a_subset_of_second(SR, *it)) {
				return true;
			}
		}
		return false;
	}
};

#endif //__UTILS1_H
