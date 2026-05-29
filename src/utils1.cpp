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
std::pair<std::string, bool/*true indicates error*/> exec(const char* cmd) {
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
	auto& SM = Rewrite.getSourceMgr();
	SourceLocation SL = sr.getBegin();
	SourceLocation SLE = sr.getEnd();

	if (false && (SL.isMacroID() && SLE.isMacroID()) && (!filtered_out_by_location(SM, SL))) {
		if ((SM.isMacroArgExpansion(SL) || SM.isMacroBodyExpansion(SL))
			&& (SM.isMacroArgExpansion(SLE) || SM.isMacroBodyExpansion(SLE))) {

			auto SL2 = SM.getExpansionLoc(SL);
			auto SLE2 = SM.getExpansionLoc(SLE);
			return { SL2, SLE2 };
		}
	}

	SL = SM.getFileLoc(SL);
	SLE = SM.getFileLoc(SLE);

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
		they are part of the same macro. */
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

bool is_macro_instantiation(const clang::SourceRange& sr, clang::Rewriter &Rewrite)
{
	bool retval = false;
	SourceLocation SL = sr.getBegin();
	SourceLocation SLE = sr.getEnd();

	if (SL.isMacroID() && SLE.isMacroID() && (!filtered_out_by_location(Rewrite.getSourceMgr(), SL))) {
		/* If the start and end locations are macro (instantiation) locations, then we'll presume that
		they are part of the same macro. */
		IF_DEBUG(std::string debug_source_location_str = SL.printToString(Rewrite.getSourceMgr());)
		IF_DEBUG(std::string text1 = Rewrite.getRewrittenText({SL, SLE});)
		auto SL5 = Rewrite.getSourceMgr().getSpellingLoc(SL);
		auto SLE5 = Rewrite.getSourceMgr().getSpellingLoc(SLE);
		clang::SourceRange SR5 = { SL5, SLE5 };

		if ((!(SLE5 < SL5)) && (SR5.isValid())) {
			auto FLSL5 = Rewrite.getSourceMgr().getFileLoc(SL5);
			if (!filtered_out_by_location(Rewrite.getSourceMgr(), FLSL5)) {
				IF_DEBUG(std::string text5 = Rewrite.getRewrittenText(SR5);)
				/* This may be an macro function argument or something, but we don't think it's an
				actual instantiation of a macro. */
				return false;
			}
		} else {
			int q = 5;
		}
		return true;
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

	return false;
}

bool first_is_contained_in_second(const clang::SourceRange& first, const clang::SourceRange& second) {
	bool retval = true;
	if ((first.getBegin() < second.getBegin()) || (second.getEnd() < first.getEnd())) {
		retval = false;
	}
	return retval;
}

bool first_is_a_proper_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second) {
	bool retval = true;
	if ((!first_is_contained_in_second(first, second)) || (second == first)) {
		retval = false;
	}
	return retval;
}

bool errors_suppressed_by_location(const SourceManager &SM, SourceLocation SL, std::optional<CModifiablePathInfo> maybe_specified_modifiable_path_info/* = {}*/) {
	auto res1 = evaluate_filtering_by_location(SM, SL, maybe_specified_modifiable_path_info);
	return res1.m_suppress_errors;
}
bool errors_suppressed_by_location(ASTContext const& Ctx, SourceLocation SL, std::optional<CModifiablePathInfo> maybe_specified_modifiable_path_info/* = {}*/) {
  const SourceManager &SM = Ctx.getSourceManager();
  return errors_suppressed_by_location(SM, SL, maybe_specified_modifiable_path_info);
}
bool errors_suppressed_by_location(const ast_matchers::MatchFinder::MatchResult &MR, SourceLocation SL, std::optional<CModifiablePathInfo> maybe_specified_modifiable_path_info/* = {}*/) {
  ASTContext *const ASTC = MR.Context;
  assert(MR.Context);
  const SourceManager &SM = ASTC->getSourceManager();
  return errors_suppressed_by_location(SM, SL, maybe_specified_modifiable_path_info);
}

// trim from start (in place)
void ltrim(std::string &s) {
	auto isnotspace = [](int ch) { return !std::isspace(ch); };
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
            isnotspace));
}

// trim from end (in place)
void rtrim(std::string &s) {
	auto isnotspace = [](int ch) { return !std::isspace(ch); };
    s.erase(std::find_if(s.rbegin(), s.rend(),
            isnotspace).base(), s.end());
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

std::string blanked_out_str(const std::string_view a) {
	std::string retval = std::string(a);
	for (auto& ch_ref : retval) {
		if (!isspace(ch_ref)) {
			ch_ref = ' ';
		}
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
	IF_DEBUG(std::string typeClassName = qtype->getTypeClassName();)
	auto TDT = clang::dyn_cast<clang::TypedefType>(qtype.getTypePtr());
	if (TDT) {
		return true;
	} else {
		if (qtype->isPointerType()) {
			return UsesPointerTypedef(qtype->getPointeeType());
		} else if (qtype->isArrayType()) {
			if (llvm::isa<const clang::ArrayType>(qtype.getTypePtr())) {
				auto ATP = llvm::cast<const clang::ArrayType>(qtype.getTypePtr());
				return UsesPointerTypedef(ATP->getElementType());
			} else {
				int q = 3;
			}
		} else if (qtype->isFunctionType()) {
			if (llvm::isa<const clang::FunctionType>(qtype.getTypePtr())) {
				auto FT = llvm::cast<const clang::FunctionType>(qtype.getTypePtr());
				return UsesPointerTypedef(FT->getReturnType());
			} else {
				int q = 3;
			}
		}
	}
	return false;
}

ETReeWalkingStatus apply_to_stmt_and_each_descendant(const clang::Stmt& stmt, CNodeHandler& node_handler, int depth/* = 0*/) {
	auto retval = ETReeWalkingStatus::Default;
	const clang::Stmt* ST = &stmt;
	auto stmt_class = ST->getStmtClass();
	auto stmt_class_name = ST->getStmtClassName();
	auto res1 = node_handler.handle_node(stmt);
	if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
		retval = res1;
	} else if (ETReeWalkingStatus::DoNotProcessChildren != res1) {
		for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
			if (nullptr != (*child_iter)) {
				auto res2 = apply_to_stmt_and_each_descendant(*(*child_iter), node_handler, depth+1);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res2) {
					retval = ETReeWalkingStatus::SkipFurtherProcessing;
					//break;
					return retval;
				}
			} else {
				int q = 5;
			}
		}

		auto res2 = ETReeWalkingStatus::Default;
		do {
			auto DS = dyn_cast<const clang::DeclStmt>(ST);
			if (DS) {
				for (auto D : DS->decls()) {
					if (D) {
						res2 = apply_to_decl_and_each_descendant(*D, node_handler, 1 + depth);
					}
				}
				break;
			}

		} while (false);

		if (ETReeWalkingStatus::SkipFurtherProcessing == res2) {
			retval = ETReeWalkingStatus::SkipFurtherProcessing;
			//break;
			return retval;
		}
	}
	return retval;
}

ETReeWalkingStatus apply_to_decl_and_each_descendant(const clang::Decl& decl, CNodeHandler& node_handler, int depth/* = 0*/) {
	auto retval = ETReeWalkingStatus::Default;
	auto res1 = node_handler.handle_node(decl);
	if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
		retval = res1;
	} else if (ETReeWalkingStatus::DoNotProcessChildren != res1) {
		const clang::Decl* D = &decl;
		auto VD = dyn_cast<const clang::VarDecl>(D);
		auto FD = dyn_cast<const clang::FieldDecl>(D);
		auto FND = dyn_cast<const clang::FunctionDecl>(D);
		if (VD) {
			if (VD->hasInit() && VD->getInit()) {
				auto res1 = apply_to_stmt_and_each_descendant(*(VD->getInit()), node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
				}
			}
		} else if (FD) {
			if (FD->hasInClassInitializer() && FD->getInClassInitializer()) {
				auto res1 = apply_to_stmt_and_each_descendant(*(FD->getInClassInitializer()), node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
				}
			}
		} else if (FND) {
			for (auto const& PVD : FND->parameters()) {
				auto res1 = apply_to_decl_and_each_descendant(*PVD, node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
					break;
				}
			}
			if ((ETReeWalkingStatus::SkipFurtherProcessing != retval) && (FND->hasBody() && FND->getBody())) {
				auto res1 = apply_to_stmt_and_each_descendant(*(FND->getBody()), node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
				}
			}
		} else {
			auto DC2 = dyn_cast<const clang::DeclContext>(D);
			if (DC2) {
				auto res1 = apply_to_declcontext_and_each_descendant(*DC2, node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
				}
			}
			if ((ETReeWalkingStatus::SkipFurtherProcessing != retval) && (D->hasBody() && D->getBody())) {
				auto res1 = apply_to_stmt_and_each_descendant(*(D->getBody()), node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
					retval = res1;
				}
			}
			int q = 7;
		}
	}
	return retval;
}

ETReeWalkingStatus apply_to_declcontext_and_each_descendant(const clang::DeclContext& declcontext, CNodeHandler& node_handler, int depth/* = 0*/) {
	auto retval = ETReeWalkingStatus::Default;
	auto res1 = node_handler.handle_node(declcontext);
	if (ETReeWalkingStatus::SkipFurtherProcessing == res1) {
		retval = res1;
	} else if (ETReeWalkingStatus::DoNotProcessChildren != res1) {
		for (auto D : declcontext.decls()) {
			if (D) {
				auto res2 = apply_to_decl_and_each_descendant(*D, node_handler, 1 + depth);
				if (ETReeWalkingStatus::SkipFurtherProcessing == res2) {
					retval = ETReeWalkingStatus::SkipFurtherProcessing;
					break;
				}
			}
		}
	}
	return retval;
}

auto stmt_sibling_ordinal_info_if_available(clang::Stmt const& stmt, clang::ASTContext& context) -> std::optional<COrdinalInfo> {
	auto node_ST = &stmt;
	const auto& parents = context.getParents(*node_ST);
	if ( parents.empty() ) {
		return {};
	}
	const auto num_parents = parents.size();
	std::optional<size_t> maybe_child_ordinal;
	size_t count = 0;
	const auto* parent_ST = parents[0].template get<clang::Stmt>();
	if (parent_ST) {
		for (auto child : parent_ST->children()) {
			if (child == node_ST) {
				maybe_child_ordinal = count;
			}
			count += 1;
		}
	} else {
		const auto* parent_D = parents[0].template get<clang::Decl>();
		if (parent_D) {
			/* Are there any `clang::Decl`s with more than one child `clang::Stmt`? */
			maybe_child_ordinal = 0;
			count = 1;
		} else {
			int q = 3;
		}
	}
	if (maybe_child_ordinal.has_value()) {
		auto const& child_ordinal = maybe_child_ordinal.value();
		return COrdinalInfo{ child_ordinal, count };
	}
	return {};
}
auto decl_sibling_ordinal_info_if_available(clang::Decl const& decl, clang::ASTContext& context) -> std::optional<COrdinalInfo> {
	auto node_D = &decl;
	std::optional<size_t> maybe_child_ordinal;
	size_t count = 0;

	const auto& parents = context.getParents(*node_D);
	if ( !parents.empty() ) {
		const auto num_parents = parents.size();
		const auto* parent_ST = parents[0].template get<clang::Stmt>();
		if (parent_ST) {
			auto parent_DST = clang::dyn_cast<const clang::DeclStmt>(parent_ST);
			if (parent_DST) {
				for (const auto D : parent_DST->decls()) {
					if (D == node_D) {
						maybe_child_ordinal = count;
					}
					count += 1;
				}
			} else {
				int q = 3;
			}
		}
		if (!maybe_child_ordinal.has_value()) {
			const auto* parent_D = parents[0].template get<clang::Decl>();
			if (parent_D) {
				auto parent_FND = clang::dyn_cast<const clang::FunctionDecl>(parent_D);
				if (parent_FND) {
					for (const auto D : parent_FND->parameters()) {
						if (D == node_D) {
							maybe_child_ordinal = count;
						}
						count += 1;
					}
					if (parent_FND->hasBody() && parent_FND->getBody()) {
						const auto body_ST = parent_FND->getBody();
						for (auto child : body_ST->children()) {
							/* The children of body_ST are `clang::Stmt`s, so they cannot match the given `clang::Decl`, but we still want 
							to include them in the total (number of children). */
							count += 1;
						}
					}
				} else {
					auto parent_DC = clang::dyn_cast<const clang::DeclContext>(parent_D);
					if (parent_DC) {
						for (const auto D : parent_DC->decls()) {
							if (D == node_D) {
								maybe_child_ordinal = count;
							}
							count += 1;
						}
					} else {
						int q = 3;
					}
				}
			}
		}
	}
	if (!maybe_child_ordinal.has_value()) {
		count = 0;
		const auto* parent_DC = node_D->getDeclContext();
		if (parent_DC) {
			auto parent_FND = clang::dyn_cast<const clang::FunctionDecl>(parent_DC);
			if (parent_FND) {
				for (const auto D : parent_FND->parameters()) {
					if (D == node_D) {
						maybe_child_ordinal = count;
					}
					count += 1;
				}
				if (parent_FND->hasBody() && parent_FND->getBody()) {
					const auto body_ST = parent_FND->getBody();
					for (auto child : body_ST->children()) {
						/* The children of body_ST are `clang::Stmt`s, so they cannot match the given `clang::Decl`, but we still want 
						to include them in the total (number of children). */
						count += 1;
					}
				}
			} else {
				for (const auto D : parent_DC->decls()) {
					if (D == node_D) {
						maybe_child_ordinal = count;
					}
					count += 1;
				}
				if (0 == count) {
					const auto* parent_func_DC = node_D->getParentFunctionOrMethod();
					if (parent_func_DC && (parent_DC != parent_func_DC)) {
						for (const auto D : parent_func_DC->decls()) {
							if (D == node_D) {
								maybe_child_ordinal = count;
							}
							count += 1;
						}
					}
				}
			}
		} else {
			int q = 5;
		}
	}
	if (maybe_child_ordinal.has_value()) {
		auto const& child_ordinal = maybe_child_ordinal.value();
		return COrdinalInfo{ child_ordinal, count };
	}
	return {};
}
auto declcontext_sibling_ordinal_info_if_available(clang::DeclContext const& declcontext, clang::ASTContext& context) -> std::optional<COrdinalInfo> {
	const auto node_DC = &declcontext;
	std::optional<COrdinalInfo> retval;

	const auto D = clang::dyn_cast<const clang::Decl>(node_DC);
	if (D) {
		retval = decl_sibling_ordinal_info_if_available(*D, context);
	}
	if (!retval.has_value()) {
		const auto parent_DC = node_DC->getParent();
		if (parent_DC && (parent_DC != node_DC)) {
			std::optional<size_t> maybe_child_ordinal;
			size_t count = 0;
			for (const auto child_D : parent_DC->decls()) {
				const auto DC2 = clang::dyn_cast<const clang::DeclContext>(child_D);
				if (DC2 && (DC2 == node_DC)) {
					maybe_child_ordinal = count;
				}
				count += 1;
			}
			if (maybe_child_ordinal.has_value()) {
				auto const& child_ordinal = maybe_child_ordinal.value();
				return COrdinalInfo{ child_ordinal, count };
			}
		} else {
			int q = 5;
		}
	}
	return retval;
}

auto stmt_ast_location_if_available(clang::Stmt const& stmt, clang::ASTContext& context) -> std::optional<CASTLocation> {
	auto node_ST = &stmt;
	std::optional<CASTLocation> retval;

	CASTLocation ancestor_ast_location;
	const auto& parents = context.getParents(*node_ST);
	if ( !(parents.empty()) ) {
		const auto num_parents = parents.size();
		std::optional<CASTLocation> maybe_parent_ast_location;

		const auto* parent_ST = parents[0].template get<clang::Stmt>();
		if (parent_ST) {
			maybe_parent_ast_location = stmt_ast_location_if_available(*parent_ST, context);
		} else {
			const auto* parent_D = parents[0].template get<clang::Decl>();
			if (parent_D) {
				maybe_parent_ast_location = decl_ast_location_if_available(*parent_D, context);
			}
		}
		if (!(maybe_parent_ast_location.has_value())) {
			return retval;
		}
		ancestor_ast_location = maybe_parent_ast_location.value();
	}

	auto maybe_sibling_ordinal_info = stmt_sibling_ordinal_info_if_available(*node_ST, context);
	if (maybe_sibling_ordinal_info.has_value()) {
		auto const& sibling_ordinal_info = maybe_sibling_ordinal_info.value();
		const std::string total_elements_str = std::to_string(sibling_ordinal_info.total_elements);
		std::string ordinal_str = std::to_string(sibling_ordinal_info.ordinal);
		while (total_elements_str.length() > ordinal_str.length()) {
			ordinal_str = "0" + ordinal_str;
		}
		retval = ancestor_ast_location + "_" + ordinal_str;
	}
	return retval;
}
auto decl_ast_location_if_available(clang::Decl const& decl, clang::ASTContext& context) -> std::optional<CASTLocation> {
	auto node_D = &decl;
	std::optional<CASTLocation> retval;

	CASTLocation ancestor_ast_location;
	std::optional<CASTLocation> maybe_parent_ast_location;
	const auto& parents = context.getParents(*node_D);
	if ( !(parents.empty()) ) {
		const auto num_parents = parents.size();

		const auto* parent_ST = parents[0].template get<clang::Stmt>();
		if (parent_ST) {
			maybe_parent_ast_location = stmt_ast_location_if_available(*parent_ST, context);
		} else {
			const auto* parent_D = parents[0].template get<clang::Decl>();
			if (parent_D) {
				maybe_parent_ast_location = decl_ast_location_if_available(*parent_D, context);
			}
		}
	}
	if (!maybe_parent_ast_location.has_value()) {
		const auto* parent_DC = node_D->getDeclContext();
		if (parent_DC) {
			maybe_parent_ast_location = declcontext_ast_location_if_available(*parent_DC, context);
		}
	}
	if (maybe_parent_ast_location.has_value()) {
		ancestor_ast_location = maybe_parent_ast_location.value();
	}

	auto maybe_sibling_ordinal_info = decl_sibling_ordinal_info_if_available(*node_D, context);
	if (maybe_sibling_ordinal_info.has_value()) {
		auto const& sibling_ordinal_info = maybe_sibling_ordinal_info.value();
		const std::string total_elements_str = std::to_string(sibling_ordinal_info.total_elements);
		std::string ordinal_str = std::to_string(sibling_ordinal_info.ordinal);
		while (total_elements_str.length() > ordinal_str.length()) {
			ordinal_str = "0" + ordinal_str;
		}
		retval = ancestor_ast_location + "_" + ordinal_str;
	}
	return retval;
}
auto declcontext_ast_location_if_available(clang::DeclContext const& declcontext, clang::ASTContext& context) -> std::optional<CASTLocation> {
	auto node_DC = &declcontext;
	std::optional<CASTLocation> retval;

	CASTLocation ancestor_ast_location;
	std::optional<CASTLocation> maybe_parent_ast_location;
	const auto D = clang::dyn_cast<const clang::Decl>(node_DC);
	if (D) {
		auto maybe_ast_location1 = decl_ast_location_if_available(*D, context);
		if (maybe_ast_location1.has_value()) {
			return maybe_ast_location1.value();
		}
	}
	if (!maybe_parent_ast_location.has_value()) {
		const auto parent_DC = node_DC->getParent();
		if (parent_DC && (parent_DC != node_DC)) {
			maybe_parent_ast_location = declcontext_ast_location_if_available(*parent_DC, context);
			if (!(maybe_parent_ast_location.has_value())) {
				return retval;
			}
		} else {
			int q = 5;
		}
	}
	if (maybe_parent_ast_location.has_value()) {
		ancestor_ast_location = maybe_parent_ast_location.value();
	}

	auto maybe_sibling_ordinal_info = declcontext_sibling_ordinal_info_if_available(*node_DC, context);
	if (maybe_sibling_ordinal_info.has_value()) {
		auto const& sibling_ordinal_info = maybe_sibling_ordinal_info.value();
		const std::string total_elements_str = std::to_string(sibling_ordinal_info.total_elements);
		std::string ordinal_str = std::to_string(sibling_ordinal_info.ordinal);
		while (total_elements_str.length() > ordinal_str.length()) {
			ordinal_str = "0" + ordinal_str;
		}
		retval = ancestor_ast_location + "_" + ordinal_str;
	}
	return retval;
}
