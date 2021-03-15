// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#include "utils1.h"
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
/*used namespaces*/
using namespace llvm;
using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::driver;
using namespace clang::tooling;
/**********************************************************************************************************************/

/* Execute a shell command. */
std::pair<std::string, bool> exec(const char* cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::shared_ptr<FILE> pipe(popen(cmd, "r"), pclose);
    //if (!pipe) SCPPT_THROW( std::runtime_error("popen() failed!"));
    if (!pipe) { return std::pair<std::string, bool>(result, true); }
    while (!feof(pipe.get())) {
        if (fgets(buffer.data(), 128, pipe.get()) != nullptr)
            result += buffer.data();
    }
    return std::pair<std::string, bool>(result, false);
}

clang::SourceRange instantiation_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite)
{
	SourceLocation SL = sr.getBegin();
	SourceLocation SLE = sr.getEnd();
	SL = Rewrite.getSourceMgr().getFileLoc(SL);
	SLE = Rewrite.getSourceMgr().getFileLoc(SLE);
	if ((SL == SLE) && (sr.getBegin() != sr.getEnd())) {
		int q = 5;
	}
	return SourceRange(SL, SLE);
}

clang::SourceRange nice_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite)
{
	SourceLocation SL = sr.getBegin();
	SourceLocation SLE = sr.getEnd();

	if (SL.isMacroID() && SLE.isMacroID() && (!filtered_out_by_location(Rewrite.getSourceMgr(), SL))) {
		/* If the start and end locations are macro (instantiation) locations, then we'll presume that
		they are part of the same macro, and we'll attempt to return the corresponding range within the
		macro definition. */
		IF_DEBUG(std::string debug_source_location_str = SL.printToString(Rewrite.getSourceMgr());)
		IF_DEBUG(std::string text1 = Rewrite.getRewrittenText({SL, SLE});)
		auto SL5 = Rewrite.getSourceMgr().getSpellingLoc(SL);
		auto SLE5 = Rewrite.getSourceMgr().getSpellingLoc(SLE);
		clang::SourceRange SR5 = { SL5, SLE5 };

		if ((!(SLE5 < SL5)) && (SR5.isValid())) {
			auto FLSL5 = Rewrite.getSourceMgr().getFileLoc(SL5);
			if (!filtered_out_by_location(Rewrite.getSourceMgr(), FLSL5)) {
				IF_DEBUG(std::string text5 = Rewrite.getRewrittenText(SR5);)
				return SR5;
			}
		} else {
			int q = 5;
		}
	}

	SL = Rewrite.getSourceMgr().getFileLoc(SL);
	SLE = Rewrite.getSourceMgr().getFileLoc(SLE);
	clang::SourceRange retSR = { SL, SLE };

#ifndef NDEBUG
	if ((!(SLE < SL)) && (retSR.isValid())) {
		std::string text6 = Rewrite.getRewrittenText({SL, SLE});
		int q = 5;
	} else {
		int q = 5;
	}
#endif /*!NDEBUG*/

	return retSR;
}

bool first_is_a_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second) {
	bool retval = true;
	if ((first.getBegin() < second.getBegin()) || (second.getEnd() < first.getEnd())) {
		retval = false;
	}
	return retval;
}

bool first_is_a_proper_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second) {
	bool retval = true;
	if ((!first_is_a_subset_of_second(first, second)) || (second == first)) {
		retval = false;
	}
	return retval;
}

bool filtered_out_by_filename(const std::string &filename) {
	bool retval = false;

	static const std::string mse_str = "mse";
	static const std::string built_in_str = "<built-in>";
	if (0 == filename.compare(0, mse_str.size(), mse_str)) {
		retval = true;
	} else if (built_in_str == filename) {
		retval = true;
	}

	return retval;
}

bool filtered_out_by_location(const SourceManager &SM, SourceLocation SL) {
	bool retval = false;

	if (!(SL.isValid())) {
		retval = true;
	} else if (SM.isInSystemHeader(SL)) {
		retval = true;
	/*
	} else if (MainFileOnly && (!(SM.isInMainFile(SL)))) {
		retval = true;
	*/
	} else {
		bool filename_is_invalid = false;
		std::string full_path_name = SM.getBufferName(SL, &filename_is_invalid);
		if ("" == full_path_name) {
			full_path_name = SL.printToString(SM);

			/*
			static const std::string spelling_prefix("<Spelling=");
			auto last_spelling_pos = full_path_name.rfind(spelling_prefix);
			if (last_spelling_pos + spelling_prefix.length() + 1 < full_path_name.size()) {
				full_path_name = full_path_name.substr(last_spelling_pos + spelling_prefix.length());
			}
			*/

			auto last_colon_pos = full_path_name.find_first_of(':');
			if (last_colon_pos + 1 < full_path_name.size()) {
				full_path_name = full_path_name.substr(0, last_colon_pos);
			} else {
				int q = 7;
			}
		}
		const auto lib_clang_pos = full_path_name.find("/lib/clang/");
		if (std::string::npos != lib_clang_pos) {
			retval = true;
		} else {
			std::string filename = full_path_name;
			const auto last_slash_pos = full_path_name.find_last_of('/');
			if (std::string::npos != last_slash_pos) {
				if (last_slash_pos + 1 < full_path_name.size()) {
					filename = full_path_name.substr(last_slash_pos+1);
				} else {
					filename = "";
				}
			}
			if (filtered_out_by_filename(filename)) {
				retval = true;
			}
		}
	}
	return retval;
}

bool filtered_out_by_location(const ast_matchers::MatchFinder::MatchResult &MR, SourceLocation SL) {
  ASTContext *const ASTC = MR.Context;
  const SourceManager &SM = ASTC->getSourceManager();
  return filtered_out_by_location(SM, SL);
}

std::string with_whitespace_removed(const std::string_view str) {
	std::string retval;
	retval = str;
	retval.erase(std::remove_if(retval.begin(), retval.end(), isspace), retval.end());
	return retval;
}

std::string with_newlines_removed(const std::string_view str) {
	std::string retval;
	retval = str;
	auto riter1 = retval.rbegin();
	while (retval.rend() != riter1) {
		if ('\n' == *riter1) {
			auto riter2 = riter1;
			riter2++;
			retval.erase(riter1.base()--);
			while (retval.rend() != riter2) {
				/* look for and remove 'continued on the next line' backslash if present. */
				if ('\\' == (*riter2)) {
					riter1++;
					retval.erase(riter2.base()--);
					break;
				} else if (!std::isspace(*riter2)) {
					break;
				}

				riter2++;
			}
		}

		riter1++;
	}

	return retval;
}

/* No longer used. This function extracts the text of individual declarations when multiple
 * pointers are declared in the same declaration statement. */
std::vector<std::string> f_declared_object_strings(const std::string_view decl_stmt_str) {
	std::vector<std::string> retval;

	auto nice_decl_stmt_str = with_newlines_removed(decl_stmt_str);
	auto semicolon_position = std::string::npos;
	for (size_t pos = 3; pos < nice_decl_stmt_str.size(); pos += 1) {
		if (';' == nice_decl_stmt_str[pos]) {
			semicolon_position = pos;
		}
	}
	if (std::string::npos == semicolon_position) {
		assert(false);
		return retval;
	}

	std::vector<size_t> delimiter_positions;
	for (size_t pos = 3; ((pos < nice_decl_stmt_str.size()) && (pos < semicolon_position)); pos += 1) {
		if (',' == nice_decl_stmt_str[pos]) {
			delimiter_positions.push_back(pos);
		}
	}

	delimiter_positions.push_back(semicolon_position);
	auto first_delimiter_pos = delimiter_positions[0];

	{
		auto pos1 = first_delimiter_pos - 1;
		auto pos2 = pos1;
		bool nonspace_found = false;
		while ((2 <= pos1) && (!nonspace_found)) {
			if (!std::isspace(nice_decl_stmt_str[pos1])) {
				pos2 = pos1 + 1;
				nonspace_found = true;
			}

			pos1 -= 1;
		}
		if (!nonspace_found) {
			assert(false);
			return retval;
		}

		bool space_found = false;
		while ((1 <= pos1) && (!space_found)) {
			if (std::isspace(nice_decl_stmt_str[pos1])) {
				space_found = true;
			}

			pos1 -= 1;
		}
		if (!space_found) {
			assert(false);
			return retval;
		}

		pos1 += 2;
		std::string first_declaration_string = nice_decl_stmt_str.substr(pos1, pos2 - pos1);
		retval.push_back(first_declaration_string);
	}

	{
		size_t delimiter_index = 0;
		while (delimiter_positions.size() > (delimiter_index + 1)) {
			if (!(delimiter_positions[delimiter_index] + 1 < delimiter_positions[(delimiter_index + 1)])) {
				//assert(false);
			} else {
				std::string declaration_string = nice_decl_stmt_str.substr(delimiter_positions[delimiter_index] + 1, delimiter_positions[(delimiter_index + 1)] - (delimiter_positions[delimiter_index] + 1));
				retval.push_back(declaration_string);
			}

			delimiter_index += 1;
		}
	}

	return retval;
}

std::string tolowerstr(const std::string_view a) {
	std::string retval;
	for (const auto& ch : a) {
		retval += tolower(ch);
	}
	return retval;
}

bool string_begins_with(const std::string_view s1, const std::string_view prefix) {
	return (0 == s1.compare(0, prefix.length(), prefix));
}
bool string_ends_with(const std::string_view s1, const std::string_view suffix) {
	if (suffix.length() > s1.length()) {
		return false;
	}
	return (0 == s1.compare(s1.length() - suffix.length(), suffix.length(), suffix));
}


/* This function returns a list of individual declarations contained in the same declaration statement
 * as the given declaration. (eg.: "int a, b = 3, *c;" ) */
std::vector<const DeclaratorDecl*> IndividualDeclaratorDecls(const DeclaratorDecl* DD) {
	/* There's probably a more efficient way to do this, but this implementation seems to work. */
	std::vector<const DeclaratorDecl*> retval;

	if (!DD) {
		assert(false);
		return retval;
	}
	auto SR = DD->getSourceRange();
	if (!SR.isValid()) {
		return retval;
	}
	SourceLocation SL = SR.getBegin();

	auto decl_context = DD->getDeclContext();
	if ((!decl_context) || (!SL.isValid())) {
		assert(false);
		retval.push_back(DD);
	} else {
		for (auto decl_iter = decl_context->decls_begin(); decl_iter != decl_context->decls_end(); decl_iter++) {
			auto decl = (*decl_iter);
			auto l_DD = dyn_cast<const DeclaratorDecl>(decl);
			if (l_DD) {
				auto DDSR = l_DD->getSourceRange();
				if (DDSR.isValid()) {
					SourceLocation l_SL = DDSR.getBegin();
					if (l_SL == SL) {
						retval.push_back(l_DD);
					}
				}
			}
		}
	}
	if (0 == retval.size()) {
		//assert(false);
		retval.push_back(DD);
	}

	return retval;
}

std::vector<const DeclaratorDecl*> IndividualDeclaratorDecls(const DeclaratorDecl* DD, Rewriter &Rewrite) {
	if (!DD) {
		assert(false);
		return std::vector<const DeclaratorDecl*>{};
	}
	auto SR = nice_source_range(DD->getSourceRange(), Rewrite);
	std::string source_text;
	if (SR.isValid()) {
		source_text = Rewrite.getRewrittenText(SR);
	}
	SourceLocation SL = SR.getBegin();
	return IndividualDeclaratorDecls(DD);
}


/* Determine if a given type is defined using a 'typedef'ed type of pointer type. */
bool UsesPointerTypedef(clang::QualType qtype) {
	IF_DEBUG(std::string qtype_str = qtype.getAsString());
	if (qtype->isPointerType()) {
		auto TDT = clang::dyn_cast<clang::TypedefType>(qtype.getTypePtr());
		if (TDT) {
			return true;
		} else {
			return UsesPointerTypedef(qtype->getPointeeType());
		}
	} else if (qtype->isArrayType()) {
		if (llvm::isa<const clang::ArrayType>(qtype.getTypePtr())) {
			auto ATP = llvm::cast<const clang::ArrayType>(qtype.getTypePtr());
			return UsesPointerTypedef(ATP->getElementType());
		}
	}
	return false;
}
