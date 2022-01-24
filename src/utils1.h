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

#ifndef MU_UTIL_LLVM_MAJOR
#ifdef LLVM_VERSION_MAJOR
#define MU_UTIL_LLVM_MAJOR LLVM_VERSION_MAJOR
#else /*LLVM_VERSION_MAJOR*/
#ifdef __clang_major__
#define MU_UTIL_LLVM_MAJOR __clang_major__
#else /*__clang_major__*/
#define MU_UTIL_LLVM_MAJOR 10
#endif /*__clang_major__*/
#endif /*LLVM_VERSION_MAJOR*/
#endif /*MU_UTIL_LLVM_MAJOR*/

#define PP_CONCAT(a, b) a##b
#define DECLARE_CACHED_CONST_STRING(name, init_value) \
							thread_local std::string PP_CONCAT(s_, name); \
							if (PP_CONCAT(s_, name).empty()) { \
								PP_CONCAT(s_, name) = init_value; \
							} \
							const std::string& name = PP_CONCAT(s_, name);
#ifndef NDEBUG
#define DEBUG_SET_SOURCE_LOCATION_STR(source_location_str1, SourceRange1, Rewrite1) \
				if (SourceRange1.isValid()) { source_location_str1 = SourceRange1.getBegin().printToString(Rewrite1.getSourceMgr()); }
#define DEBUG_SET_SOURCE_TEXT_STR(source_text1, SourceRange1, Rewrite1) \
				if ((SourceRange1).isValid() && (((SourceRange1).getBegin() < (SourceRange1).getEnd()) || ((SourceRange1).getBegin() == (SourceRange1).getEnd()))) { source_text1 = Rewrite1.getRewrittenText(SourceRange1); }
#define IF_DEBUG(x) x
#else /*!NDEBUG*/
#define DEBUG_SET_SOURCE_LOCATION_STR(source_location_str1, SourceRange1, Rewrite1) ;
#define DEBUG_SET_SOURCE_TEXT_STR(source_text1, SourceRange1, Rewrite1) ;
#define IF_DEBUG(x)
#endif /*!NDEBUG*/

#define DEBUG_SOURCE_LOCATION_STR(source_location_str1, SourceRange1, Rewrite1) std::string source_location_str1; DEBUG_SET_SOURCE_LOCATION_STR(source_location_str1, SourceRange1, Rewrite1);
#define DEBUG_SOURCE_TEXT_STR(source_text1, SourceRange1, Rewrite1) std::string source_text1; DEBUG_SET_SOURCE_TEXT_STR(source_text1, SourceRange1, Rewrite1);

#define RETURN_IF_FILTERED_OUT_BY_LOCATION1 \
				if ((!SR.isValid()) || filtered_out_by_location(MR, SR.getBegin())) { \
					return void(); \
				}

#define RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1 \
				if (!SR.isValid()) { \
					return; \
				}

#define RETURN_IF_IS_IN_SUPPRESS_CHECK_REGION1(x) \
                auto ISR = instantiation_source_range(x->getSourceRange(), Rewrite); \
                auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR); \
                if (suppress_check_flag) { \
                    return; \
                }


inline const std::string& mse_namespace_str() {
	/* In the future, this could be specified (at run-time, as a command line parameter). */
	static const std::string l_mse_namespace_str = "mse";
	return l_mse_namespace_str;
}

/* Execute a shell command. */
std::pair<std::string, bool> exec(const char* cmd);

clang::SourceRange nice_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite);
inline clang::SourceLocation nice_source_location(const clang::SourceLocation& sl, clang::Rewriter &Rewrite) {
	return nice_source_range({ sl, sl }, Rewrite).getBegin();
}
clang::SourceRange instantiation_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite);
bool is_macro_instantiation(const clang::SourceRange& sr, clang::Rewriter &Rewrite);

/* not necessarily a proper subset */
bool first_is_a_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second);
bool first_is_a_proper_subset_of_second(const clang::SourceRange& first, const clang::SourceRange& second);

bool filtered_out_by_location(const clang::SourceManager &SM, clang::SourceLocation SL);

bool filtered_out_by_location(const clang::ast_matchers::MatchFinder::MatchResult &MR, clang::SourceLocation SL);

std::string with_whitespace_removed(const std::string_view str);

std::string with_newlines_removed(const std::string_view str);

/* No longer used. This function extracts the text of individual declarations when multiple
 * pointers are declared in the same declaration statement. */
std::vector<std::string> f_declared_object_strings(const std::string_view decl_stmt_str);

std::string tolowerstr(const std::string_view a);

bool string_begins_with(const std::string_view s1, const std::string_view prefix);
bool string_ends_with(const std::string_view s1, const std::string_view suffix);


/* This function returns a list of individual declarations contained in the same declaration statement
 * as the given declaration. (eg.: "int a, b = 3, *c;" ) */
std::vector<const clang::DeclaratorDecl*> IndividualDeclaratorDecls(const clang::DeclaratorDecl* DD);
std::vector<const clang::DeclaratorDecl*> IndividualDeclaratorDecls(const clang::DeclaratorDecl* DD, clang::Rewriter &Rewrite);

/* Determine if a given type is defined using a 'typedef'ed type of pointer type. */
bool UsesPointerTypedef(clang::QualType qtype);


class COrderedSourceRange : public clang::SourceRange {
	public:
	typedef clang::SourceRange base_class;
	using base_class::base_class;
	COrderedSourceRange(const COrderedSourceRange&) = default;

	/* rewritability_t is used to indicate whether this object holds a source range value that remains
	"valid" after it is used to replace the corresponding source text (via
	'clang::Rewriter::ReplaceText()'). By "valid" we mean that the source range value properly corresponds
	to the replacement/updated text even if the new text is a different length than the text it replaced. */
	enum rewritability_t : bool { WriteOnce, Rewritable };
	COrderedSourceRange(const base_class& src, rewritability_t rewritability = Rewritable) : base_class(src), m_rewritability(rewritability) {}

	COrderedSourceRange& operator=(const COrderedSourceRange&) = default;
	COrderedSourceRange& operator=(const base_class& src) { m_rewritability = Rewritable; base_class::operator=(src); return (*this); }

	bool is_rewritable() const { return (Rewritable == m_rewritability); }
	rewritability_t m_rewritability = Rewritable;
};
inline bool operator==(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	return static_cast<const COrderedSourceRange::base_class &>(LHS) == static_cast<const COrderedSourceRange::base_class &>(RHS);
	//return LHS.getBegin() == RHS.getBegin();
}
inline bool operator!=(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	return !(LHS == RHS);
}
inline bool operator<(const COrderedSourceRange &LHS, const COrderedSourceRange &RHS) {
	if (LHS.getBegin() < RHS.getBegin()) {
		return true;
	} else if (LHS.getBegin() == RHS.getBegin()) {
		if (RHS.getEnd() < LHS.getEnd()) {
			/* This might be a little counter-intuitive, but we need any source range that contains another
			source range to "come before" (i.e. "be less than") the other range, even if they begin at the
			same location. */
			return true;
		}
	}
	return false;
}
/* write_once_source_range() is used to create a source range value that is marked as one that becomes
"stale"/"invalid" after it is used to replace the corresponding source text (via
'clang::Rewriter::ReplaceText()'). */
inline COrderedSourceRange write_once_source_range(const COrderedSourceRange& src) { return COrderedSourceRange(src, COrderedSourceRange::WriteOnce); }
/* rewritable_source_range() is used to create a source range value that is marked as one that remains
valid after it is used to replace the corresponding source text (via 'clang::Rewriter::ReplaceText()').
This is the default state of COrderedSourceRange. */
inline COrderedSourceRange rewritable_source_range(const COrderedSourceRange& src) { return COrderedSourceRange(src, COrderedSourceRange::Rewritable); }

class COrderedRegionSet : public std::set<COrderedSourceRange> {
	public:
	typedef std::set<COrderedSourceRange> base_class;
	using base_class::base_class;
	bool contains(const clang::SourceRange& SR) const {
		IF_DEBUG(auto or_set_size = (*this).size());
		for (auto it = (*this).cbegin(); (*this).cend() != it; it++) {
			if (first_is_a_subset_of_second(SR, *it)) {
				return true;
			}
		}
		return false;
	}
	bool properly_contains(const clang::SourceRange& SR) const {
		for (auto it = (*this).cbegin(); (*this).cend() != it; it++) {
			if (first_is_a_proper_subset_of_second(SR, *it)) {
				return true;
			}
		}
		return false;
	}
	auto insert(const base_class::value_type& x) {
		if (false) {
			auto SL = x.getBegin();
			auto SLE = x.getEnd();
			auto begin_as_int = *((unsigned *)(&SL));
			auto end_as_int = *((unsigned *)(&SLE));
			if ((7037 >= begin_as_int) && (7037 <= end_as_int)) {
				int q = 5;
			}
		}
		return base_class::insert(x);
	}
};


	template<typename TPtr>
	inline auto IgnoreParenImpCasts(TPtr ptr) -> decltype(ptr->IgnoreImplicit()->IgnoreParenImpCasts()) {
		if (!ptr) { return ptr; }
		return ptr->IgnoreImplicit()->IgnoreParenImpCasts();
	}
	template<typename TPtr>
	inline auto IgnoreParenImpNoopCasts(TPtr ptr, clang::ASTContext& Ctx) -> decltype(IgnoreParenImpCasts(ptr)->IgnoreParenNoopCasts(Ctx)) {
		if (!ptr) { return ptr; }
		return IgnoreParenImpCasts(ptr)->IgnoreParenNoopCasts(Ctx);
	}
	template<typename TPtr>
	inline const clang::Expr* IgnoreExprWithCleanups(const TPtr ptr) {
		if (!ptr) { return ptr; }
		const clang::ExprWithCleanups* EWC = clang::dyn_cast<const clang::ExprWithCleanups>(ptr);
		if (EWC) {
			return EWC->getSubExpr();
		}
		return ptr;
	}

	template <typename ContainingElementT, typename NodeT>
	inline auto Tget_immediately_containing_element_of_type(const NodeT* NodePtr, clang::ASTContext& context) {
		const ContainingElementT* retval = nullptr;
		if (!NodePtr) {
			return retval;
		}
		const auto& parents = context.getParents(*NodePtr);
		if ( parents.empty() ) {
			return retval;
		}
		const auto num_parents = parents.size();
		const ContainingElementT* ST = parents[0].template get<ContainingElementT>();
		if (ST) {
			retval = clang::dyn_cast<const ContainingElementT>(ST);
			assert(retval);
		}
		return retval;
	}

	/* This function just returns the given clang::Expr if it is not an "Implicit", "Paren(thesis)"
	or "NoopCast" expression. Otherwise it returns the first ancestor that satisfies that criteria. */
	inline auto NonParenNoopCastThisOrParent(const clang::Expr* ptr, clang::ASTContext& Ctx) -> const clang::Expr* {
		if (!ptr) { return ptr; }
		while (ptr && (IgnoreParenImpCasts(ptr)->IgnoreParenNoopCasts(Ctx) != ptr)) {
			ptr = Tget_immediately_containing_element_of_type<clang::Expr>(ptr, Ctx);
		}
		return ptr;
	}

	/* This function just returns the immediate parent node, ignoring "Implicit", "Paren(thesis)"
	or "NoopCast" expressions, if that parent is a clang::Stmt. Otherwise it returns nullptr. */
	inline auto NonParenNoopCastParentStmt(const clang::Expr* ptr, clang::ASTContext& Ctx) -> const clang::Stmt* {
		clang::Stmt const* retval = nullptr;
		if (!ptr) { return retval; }
		auto parent_E = Tget_immediately_containing_element_of_type<clang::Expr>(ptr, Ctx);
		while (parent_E && (IgnoreParenImpCasts(parent_E)->IgnoreParenNoopCasts(Ctx) != parent_E)) {
			ptr = parent_E;
			parent_E = Tget_immediately_containing_element_of_type<clang::Expr>(parent_E, Ctx);
		}
		retval = Tget_immediately_containing_element_of_type<clang::Stmt>(ptr, Ctx);
		return retval;
	}

	template <typename ContainingElementT, typename NodeT>
	inline auto Tget_containing_element_of_type(const NodeT* NodePtr, clang::ASTContext& context) {
		const ContainingElementT* retval = nullptr;
		if (!NodePtr) {
			return retval;
		}
		const auto& parents = context.getParents(*NodePtr);
		if ( parents.empty() ) {
			return retval;
		}
		IF_DEBUG(const auto num_parents = parents.size();)
		const ContainingElementT* ST = parents[0].template get<ContainingElementT>();
		if (ST) {
			retval = clang::dyn_cast<const ContainingElementT>(ST);
			assert(retval);
		} else {
			return Tget_containing_element_of_type<ContainingElementT>(&(parents[0]), context);
		}
		return retval;
	}
	template <typename NodeT>
	inline auto get_containing_scope(const NodeT* NodePtr, clang::ASTContext& context) {
		return Tget_containing_element_of_type<clang::CompoundStmt>(NodePtr, context);
	}


	typedef std::string lifetime_id_t;
	struct CAbstractLifetime {
		lifetime_id_t m_id;
		clang::FunctionDecl const * m_function_context = nullptr;
		bool operator==(const CAbstractLifetime& rhs) const {
			return ((rhs.m_id == m_id) && (rhs.m_function_context == m_function_context));
		}
		bool operator<(const CAbstractLifetime& rhs) const {
			if (m_function_context < rhs.m_function_context) {
				return true;
			} else if ((m_function_context == rhs.m_function_context) && (m_id < rhs.m_id)) {
				return true;
			}
			return false;
		}
	};
	struct CScopeLifetimeInfo1 {
		std::optional<const clang::CompoundStmt*> m_maybe_containing_scope;
		std::optional<COrderedSourceRange> m_maybe_source_range;
		std::optional<CAbstractLifetime> m_maybe_abstract_lifetime;
		enum class ECategory { None, Automatic, ThisExpression, Immortal, Literal, AbstractLifetime };
		ECategory m_category = ECategory::None;
	};
	inline bool operator==(const CScopeLifetimeInfo1 &LHS, const CScopeLifetimeInfo1 &RHS) {
			return ((LHS.m_maybe_containing_scope == RHS.m_maybe_containing_scope)
			&& (LHS.m_maybe_source_range == RHS.m_maybe_source_range)
			&& (LHS.m_category == RHS.m_category)
			);
	}
	inline bool operator!=(const CScopeLifetimeInfo1 &LHS, const CScopeLifetimeInfo1 &RHS) {
		return !(LHS == RHS);
	}

	inline bool first_is_contained_in_scope_of_second(const clang::SourceRange& SR1, const clang::SourceRange& SR2, clang::ASTContext& context) {
		bool retval = true;
		if (SR1.isValid() && SR2.isValid()) {
			if (SR2.getEnd() < SR1.getBegin()) {
				/* The second item occurs before the first item. */
				retval = true;
			} else if ( ((SR1.getBegin() < SR2.getBegin()) || (SR1.getBegin() == SR2.getBegin()))
				&& ((SR2.getEnd() < SR1.getEnd()) || (SR1.getEnd() == SR2.getEnd())) ) {
				/* The second item and first item are the same item, or the second item is a member
				of, or base of, the first item. Perhaps unintuitively, the scope of an object is
				"contained inside" the scope of any of its members or base classes as they are
				constructed before and destructed after the object itself. Right? */
				retval = true;
			} else {
				retval = false;
			}
		} else {
			retval = false;
		}
		return retval;
	}

	inline bool first_is_contained_in_scope_of_second(clang::CompoundStmt const * scope1, clang::SourceRange SR1, clang::CompoundStmt const * const scope2, clang::SourceRange SR2, clang::ASTContext& context) {
		bool retval = true;
		if (scope2 == scope1) {
			if (SR1.isValid() && SR2.isValid()) {
				if (SR2.getEnd() < SR1.getBegin()) {
					/* The second item occurs before the first item. */
					retval = true;
				} else if ( ((SR1.getBegin() < SR2.getBegin()) || (SR1.getBegin() == SR2.getBegin()))
					&& ((SR2.getEnd() < SR1.getEnd()) || (SR1.getEnd() == SR2.getEnd())) ) {
					/* The second item and first item are the same item, or the second item is a member
					of, or base of, the first item. Perhaps unintuitively, the scope of an object is
					"contained inside" the scope of any of its members or base classes as they are
					constructed before and destructed after the object itself. Right? */
					retval = true;
				} else {
					retval = false;
				}
			} else {
				retval = false;
			}
		}
		while (scope2 != scope1) {
			scope1 = get_containing_scope(scope1, context);
			if (!scope1) {
				retval = (scope2 == scope1);
				break;
			}
		}
		return retval;
	}

	template <typename NodeT, typename Node2T>
	inline bool first_is_contained_in_scope_of_second(const NodeT* ST1, const Node2T* ST2, clang::ASTContext& context) {
		return first_is_contained_in_scope_of_second(get_containing_scope(ST1, context), ST1->getSourceRange()
			, get_containing_scope(ST2, context), ST2->getSourceRange(), context);
	}

	inline bool has_ancestor_base_class(const clang::QualType qtype, const std::string& qualified_base_class_name);
	inline bool has_ancestor_base_class(const clang::Type& type, const std::string& qualified_base_class_name) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			if (qualified_base_class_name == qname) {
				return true;
			}

			if (CXXRD->hasDefinition()) {
				for (const auto& base : CXXRD->bases()) {
					const auto base_qtype = base.getType();
					IF_DEBUG(const auto base_qtype_str = base_qtype.getAsString();)

					if (has_ancestor_base_class(base.getType(), qualified_base_class_name)) {
						return true;
					}
				}
			} else {
				int q = 5;
			}
		}

		return retval;
	}
	inline bool has_ancestor_base_class(const clang::QualType qtype, const std::string& qualified_base_class_name) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = has_ancestor_base_class(*TP, qualified_base_class_name);
		}
		return retval;
	}

	inline bool has_tag_method(const clang::CXXRecordDecl& record_decl_cref, const std::string& target_name) {
		bool retval = false;
		IF_DEBUG(auto qname = record_decl_cref.getQualifiedNameAsString();)
		static const std::string s_async_shareable_tag_str = "async_shareable_tag";
		static const std::string s_async_shareable_and_passable_tag_str = "async_shareable_and_passable_tag";
		for (const auto& method : record_decl_cref.methods()) {
			const auto method_name = method->getNameAsString();
			if (target_name == method_name) {
				return true;
			}
		}
		return retval;
	}

	inline bool statement_makes_reference_to_decl(const clang::ValueDecl& VLD_cref, const clang::Stmt& ST1_cref);
	inline bool statement_makes_reference_to_decl(const clang::VarDecl& VD_cref, const clang::Stmt& ST1_cref) {
		auto VLD = clang::dyn_cast<const clang::ValueDecl>(&VD_cref);
		assert(VLD);
		return statement_makes_reference_to_decl(*VLD, ST1_cref);
	}

	inline bool statement_makes_reference_to_decl(const clang::ValueDecl& VLD_cref, const clang::Stmt& ST1_cref) {
		bool retval = false;

		//auto ST = ST1_cref.IgnoreImplicit();
		auto ST = &ST1_cref;

		auto DRE1 = clang::dyn_cast<const clang::DeclRefExpr>(ST);
		if (DRE1) {
			const auto D1 = DRE1->getDecl();
			if (D1 == (&VLD_cref)) {
				return true;
			}
		}
		for (const auto& child : ST->children()) {
			const auto res1 = statement_makes_reference_to_decl(VLD_cref, *child);
			if (res1) {
				return true;
			}
		}

		return retval;
	}

	template <typename ContainedElementT>
	inline std::vector<const ContainedElementT*> Tget_contained_elements_of_type(const clang::Stmt& ST1_cref) {
		std::vector<const ContainedElementT*> retval;

		//auto ST = ST1_cref.IgnoreImplicit();
		auto ST = &ST1_cref;

		const ContainedElementT* contained_element_of_given_type = clang::dyn_cast<const ContainedElementT>(ST);

		if (contained_element_of_given_type) {
			retval.push_back(contained_element_of_given_type);
		}

		for (const auto& child : ST->children()) {
			auto res1 = Tget_contained_elements_of_type<ContainedElementT>(*child);
			retval.insert(retval.end(), res1.begin(), res1.end());
		}

		return retval;
	}
	inline std::vector<const clang::CXXThisExpr*> get_contained_elements_of_type_CXXThisExpr(const clang::Stmt& ST1_cref) {
		return Tget_contained_elements_of_type<clang::CXXThisExpr>(ST1_cref);
	}
	inline const clang::MemberExpr* get_immediately_containing_MemberExpr_from_CXXThisExpr_if_any(const clang::CXXThisExpr& CXXTE_cref, clang::ASTContext& context) {
		return Tget_immediately_containing_element_of_type<clang::MemberExpr>(IgnoreParenImpNoopCasts(&CXXTE_cref, context), context);
	}
	inline const clang::FieldDecl* get_FieldDecl_from_MemberExpr_if_any(const clang::MemberExpr& ME_cref) {
		auto decl = ME_cref.getMemberDecl();
		return clang::dyn_cast<const clang::FieldDecl>(decl);
	}
	inline bool is_nullptr_literal(const clang::Expr* EX, clang::ASTContext& Ctx) {
		bool retval = false;
		if (!EX) {
			return retval;
		}
		const auto* EXii = IgnoreParenImpNoopCasts(EX, Ctx);
		auto *CXXNPLE = clang::dyn_cast<const clang::CXXNullPtrLiteralExpr>(EXii);
		auto *GNE = clang::dyn_cast<const clang::GNUNullExpr>(EXii);
		if (CXXNPLE || GNE) {
			retval = true;
		} else {
			auto *IL = clang::dyn_cast<const clang::IntegerLiteral>(EXii);
			if (IL) {
				const auto apint_val = IL->getValue();
				const auto u64_limited_val = apint_val.getLimitedValue();
				const auto limited_val = int(u64_limited_val);
				if (0 == limited_val) {
					retval = true;
				} else {
					/* This should result in a compile error,
					so we don't issue a redundant error here. */
				}
			}
		}
		return retval;
	}

	inline std::optional<clang::QualType> get_first_template_parameter_if_any(const clang::QualType qtype) {
		std::optional<clang::QualType> retval;
		const auto CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				const auto& template_args = CTSD->getTemplateInstantiationArgs();
				const auto num_args = template_args.size();
				for (int i = 0; i < int(num_args); i += 1) {
					const auto template_arg = template_args[i];
					const auto ta_qtype = template_arg.getAsType();
					IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
					return ta_qtype;
				}
				/*unexpected*/
				int q = 5;
			}
		}
		return retval;
	}

	inline clang::QualType remove_fparam_wrappers(const clang::QualType& qtype) {
		const auto CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			//DECLARE_CACHED_CONST_STRING(treturnablefparam_str, mse_namespace_str() + "::rsv::TReturnableFParam");
			//DECLARE_CACHED_CONST_STRING(tfparam_str, mse_namespace_str() + "::rsv::TFParam");
			//DECLARE_CACHED_CONST_STRING(txsifcfparam_str, mse_namespace_str() + "::rsv::TXScopeItemFixedConstPointerFParam");
			//DECLARE_CACHED_CONST_STRING(txsiffparam_str, mse_namespace_str() + "::rsv::TXScopeItemFixedPointerFParam");

			DECLARE_CACHED_CONST_STRING(prefix_str, mse_namespace_str() + "::rsv::");
			static const std::string suffix_str = "FParam";
			if (!(string_begins_with(qname, prefix_str) && string_ends_with(qname, suffix_str))) {
				return qtype;
			}

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				const auto& template_args = CTSD->getTemplateInstantiationArgs();
				const auto num_args = template_args.size();
				for (int i = 0; i < int(num_args); i += 1) {
					const auto template_arg = template_args[i];
					if (clang::TemplateArgument::Type == template_arg.getKind()) {
						const auto ta_qtype = template_arg.getAsType();
						IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
						return remove_fparam_wrappers(ta_qtype);
					}
					/*unexpected*/
					int q = 5;
				}
				/*unexpected*/
				int q = 5;
			}
		}
		return qtype;
	}

	inline clang::QualType remove_mse_transparent_wrappers(const clang::QualType& qtype) {
		clang::QualType retval = remove_fparam_wrappers(qtype);
		const auto CXXRD = retval.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();

			DECLARE_CACHED_CONST_STRING(txscopeobj_str, mse_namespace_str() + "::TXScopeObj");
			DECLARE_CACHED_CONST_STRING(tregobj_str, mse_namespace_str() + "::TRegisteredObj");
			DECLARE_CACHED_CONST_STRING(tndregobj_str, mse_namespace_str() + "::TNDRegisteredObj");
			DECLARE_CACHED_CONST_STRING(tgnoradobj_str, mse_namespace_str() + "::us::impl::TGNoradObj");
			DECLARE_CACHED_CONST_STRING(tnoradobj_str, mse_namespace_str() + "::TNoradObj");
			DECLARE_CACHED_CONST_STRING(tndnoradobj_str, mse_namespace_str() + "::TNDNoradObj");
			DECLARE_CACHED_CONST_STRING(tasyncshareableobj_str, mse_namespace_str() + "::rsv::TAsyncShareableObj");
			DECLARE_CACHED_CONST_STRING(tasyncpassableobj_str, mse_namespace_str() + "::rsv::TAsyncPassableObj");
			DECLARE_CACHED_CONST_STRING(tasyncshareableandpassableobj_str, mse_namespace_str() + "::rsv::TAsyncShareableAndPassableObj");
			DECLARE_CACHED_CONST_STRING(tthreadlocalobj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");
			DECLARE_CACHED_CONST_STRING(tstaticimmutableobj_str, mse_namespace_str() + "::rsv::TStaticImmutableObj");
			DECLARE_CACHED_CONST_STRING(tstaticatomicobj_str, mse_namespace_str() + "::rsv::TStaticAtomicObj");

			if (!((qname == txscopeobj_str) || (qname == tregobj_str) || (qname == tndregobj_str)
				|| (qname == tgnoradobj_str) || (qname == tnoradobj_str) || (qname == tndnoradobj_str)
				|| (qname == tasyncshareableobj_str) || (qname == tasyncpassableobj_str) || (qname == tasyncshareableandpassableobj_str)
				|| (qname == tthreadlocalobj_str) || (qname == tstaticimmutableobj_str) || (qname == tstaticatomicobj_str)
				)) {
				return retval;
			}

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				const auto& template_args = CTSD->getTemplateInstantiationArgs();
				const auto num_args = template_args.size();
				for (int i = 0; i < int(num_args); i += 1) {
					const auto template_arg = template_args[i];
					if (clang::TemplateArgument::Type == template_arg.getKind()) {
						const auto ta_qtype = template_arg.getAsType();
						IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
						return remove_mse_transparent_wrappers(ta_qtype);
					}
					/*unexpected*/
					int q = 5;
				}
				/*unexpected*/
				int q = 5;
			}
		}
		return retval;
	}

	inline auto remove_cast_from_TPointerForLegacy_to_raw_pointer(const clang::Expr* EX1, clang::ASTContext& Ctx) {
		const clang::Expr* retval = EX1;
		if (!EX1) {
			return retval;
		}
		auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);
		auto CXXMCE = clang::dyn_cast<const clang::CXXMemberCallExpr>(EX);
		if (CXXMCE) {
			auto IOAEX = IgnoreParenImpNoopCasts(CXXMCE->getImplicitObjectArgument(), Ctx);
			assert(IOAEX);
			const auto IOA_qtype = IOAEX->getType();
			IF_DEBUG(const auto IOA_qtype_str = IOA_qtype.getAsString();)
			//DEBUG_SOURCE_TEXT_STR(IOAEX_source_text, nice_source_range(IOAEX->getSourceRange(), Rewrite), Rewrite);

			const auto CXXRD = IOA_qtype->getAsCXXRecordDecl();
			if (CXXRD) {
				auto qname = CXXRD->getQualifiedNameAsString();

				DECLARE_CACHED_CONST_STRING(tpfl_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
				if ((tpfl_str == qname) && (0 == CXXMCE->getNumArgs())) {
					auto method_name = CXXMCE->getDirectCallee()->getNameAsString();
					if (string_begins_with(method_name, "operator ") && string_ends_with(method_name, "*")) {
						retval = IOAEX;
						const auto ME = clang::dyn_cast<const clang::MemberExpr>(IOAEX);
						if (ME) {
							const auto EX2 = IgnoreParenImpNoopCasts(ME->getBase(), Ctx);
							if (EX2) {
								retval = EX2;
							}
						}
					}
				}
			}
		}
		return retval;
	}

	/* If qtype refers to a typedef, then we'll return a qtype that refers to the definition
	in the typedef. */
	inline auto definition_qtype(clang::QualType qtype) {
		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		auto is_const = qtype.isConstQualified();
		while (llvm::isa<const clang::TypedefType>(qtype)) {
			auto TDT = llvm::cast<const clang::TypedefType>(qtype);
			if (TDT) {
				auto TDND = TDT->getDecl();
				if (TDND) {
					qtype = TDND->getUnderlyingType();
					IF_DEBUG(std::string qtype_str2 = qtype.getAsString();)
					int q = 5;
				} else { assert(false); }
			} else { assert(false); }
		}
		if (is_const) {
			qtype.addConst();
		}
		IF_DEBUG(std::string qtype_str3 = qtype.getAsString();)
		return qtype;
	}

	inline bool is_char_or_const_char(const clang::QualType& qtype) {
		bool retval = false;
		static const std::string char_str = "char";
		static const std::string const_char_str = "const char";
		std::string qtype_str = qtype.getAsString();
		if ((char_str == qtype_str) || (const_char_str == qtype_str)) {
			retval = true;
		}
		return retval;
	}
	inline bool is_char_star_or_const_char_star(const clang::QualType& qtype) {
		bool retval = false;
		if (qtype->isPointerType()) {
			auto direct_type = qtype->getPointeeType();
			retval = is_char_or_const_char(direct_type);
		}
		return retval;
	}

	inline bool is_FILE_or_const_FILE(const clang::QualType& qtype) {
		bool retval = false;
		static const std::string FILE_str = "FILE";
		static const std::string const_FILE_str = "const FILE";
		std::string qtype_str = qtype.getAsString();
		if ((FILE_str == qtype_str) || (const_FILE_str == qtype_str)) {
			retval = true;
		}
		return retval;
	}
	inline bool is_FILE_star_or_const_FILE_star(const clang::QualType& qtype) {
		bool retval = false;
		if (qtype->isPointerType()) {
			auto direct_type = qtype->getPointeeType();
			retval = is_FILE_or_const_FILE(direct_type);
		}
		return retval;
	}

#if MU_UTIL_LLVM_MAJOR <= 12
#define SCPP_IMPL_DYNTYPEDNODE clang::ast_type_traits::DynTypedNode
#elif MU_UTIL_LLVM_MAJOR > 12
#define SCPP_IMPL_DYNTYPEDNODE clang::DynTypedNode
#endif /*MU_UTIL_LLVM_MAJOR*/

	class CRegionAndElementSet : public COrderedRegionSet {
	public:
		typedef COrderedRegionSet base_class;
		using base_class::base_class;

		bool contains(const clang::SourceRange& SR) const {
			return base_class::contains(SR);
		}
		template <typename NodeT>
		bool contains(NodeT* NodePtr, clang::ASTContext& context) const {
			if (!NodePtr) {
				return false;
			}
			NodeT const * NodeConstPtr = NodePtr;
			auto maybe_dyn_typed_node = std::optional(SCPP_IMPL_DYNTYPEDNODE::create(*NodeConstPtr));
			while (maybe_dyn_typed_node.has_value()) {
				const auto& dyn_typed_node_cref = maybe_dyn_typed_node.value();
				{
					auto it = m_dyn_type_nodes.find(dyn_typed_node_cref);
					if (m_dyn_type_nodes.end() != it) {
						return true;
					}
				}

				const auto& parents = context.getParents(dyn_typed_node_cref);
				if (parents.empty()) {
					return false;
				} else {
					IF_DEBUG(const auto num_parents = parents.size();)
					maybe_dyn_typed_node = parents[0];
				}
			}
			return false;
		}
		auto insert(const base_class::value_type& x) {
			return base_class::insert(x);
		}
		template <typename NodeT>
		auto insert(NodeT* NodePtr) {
			if (!NodePtr) {
				return std::pair{ m_dyn_type_nodes.end(), false };
			}
			NodeT const * NodeConstPtr = NodePtr;
			auto dyn_typed_node = SCPP_IMPL_DYNTYPEDNODE::create(*NodeConstPtr);
			auto retval = m_dyn_type_nodes.insert(dyn_typed_node);

			auto element_id = static_cast<const void*>(NodePtr);
			m_element_ids.insert(element_id);

			return retval;
		}

		std::set<SCPP_IMPL_DYNTYPEDNODE> m_dyn_type_nodes;
		std::set<const void*> m_element_ids;
	};
	class CSuppressCheckRegionSet : public CRegionAndElementSet {
	public:
		typedef CRegionAndElementSet base_class;
		using base_class::base_class;

		bool contains(const clang::SourceRange& SR) const {
			return base_class::contains(SR);
		}
		/* This function determines whether the given ast node is "contained" in the set of nodes to which
		a "suppress check" directive applies. That is, whether it, or any of its ancestors, are immediately
		preceded by a "suppress check" directive. */
		template <typename NodeT>
		bool contains(NodeT* NodePtr, clang::Rewriter &Rewrite, clang::ASTContext& context) const {
			if (!NodePtr) {
				return false;
			}
			IF_DEBUG(auto or_set_size = (*this).size());
			IF_DEBUG(auto dtn_set_size = (*this).m_dyn_type_nodes.size());
			NodeT const * NodeConstPtr = NodePtr;
			auto maybe_dyn_typed_node = std::optional(SCPP_IMPL_DYNTYPEDNODE::create(*NodeConstPtr));
			while (maybe_dyn_typed_node.has_value()) {
				const auto& dyn_typed_node_cref = maybe_dyn_typed_node.value();
				if (false) {
					/* We're not using the m_dyn_type_nodes at the moment because they don't seem to
					remain consistent across ast passes. */
					auto it = m_dyn_type_nodes.find(dyn_typed_node_cref);
					if (m_dyn_type_nodes.end() != it) {
						return true;
					}
				}

				const clang::Stmt* node_ST = dyn_typed_node_cref.template get<clang::Stmt>();
				const clang::Decl* node_D = dyn_typed_node_cref.template get<clang::Decl>();
				if (node_ST || node_D) {
					auto node_rawSR = node_ST ? node_ST->getSourceRange() : node_D->getSourceRange();
					auto ISR = instantiation_source_range(node_rawSR, Rewrite);

					/* The "contains(const clang::SourceRange&) method checks whether the given range is
					"contained" by the set of ranges (or "regions") under a "suppress check" directive.

					If it were the case that the source range of an element were always a (not necessarily
					proper) superset the ranges of its child elements, then checking the (ranges of)
					ancestor elements (as we do in this function) would be redundant. While this is
					usually the case, it may not always be the case when macros are involved. */
					if ((*this).contains(ISR)) {
						return true;
					}
				}

				const auto& parents = context.getParents(dyn_typed_node_cref);
				if (parents.empty()) {
					return false;
				} else {
					IF_DEBUG(const auto num_parents = parents.size();)

					if (false) {
						/* This code checks whether the node in question is (immediately) preceded by an element
						that indicates that checks (and code conversion) should be suppressed for the element in
						question (and any of its descendants).
						Currently we don't use this code, as we already checked the node against a set of stored
						source ranges of "check suppressed" elements. That check is much more efficient, but may
						or may not be completely reliable. */
						const clang::CompoundStmt* parent_CST = parents[0].template get<clang::CompoundStmt>();
						if (node_ST) {
							if (parent_CST && (parent_CST->child_begin() != parent_CST->child_end())) {
								decltype(*parent_CST->child_begin()) previous_child = nullptr;
								for (auto child_iter = parent_CST->child_begin(); child_iter != parent_CST->child_end(); child_iter++) {
									if (nullptr != (*child_iter)) {
										if (node_ST == (*child_iter)) {
											break;
										}
										previous_child = (*child_iter);
									} else { assert(false); }
								}
								if (previous_child) {
									if (llvm::isa<clang::CallExpr>(previous_child)) {
										auto CE = llvm::cast<clang::CallExpr>(previous_child);
										if (CE) {
											auto function_decl = CE->getDirectCallee();
											auto num_args = CE->getNumArgs();
											//assert(0 == num_args);
											if (function_decl) {
												IF_DEBUG(std::string function_name = function_decl->getNameAsString();)
												std::string qualified_function_name = function_decl->getQualifiedNameAsString();
												DECLARE_CACHED_CONST_STRING(suppress_check_directive_str, mse_namespace_str() + "::rsv::suppress_check_directive");
												if (suppress_check_directive_str == qualified_function_name) {
													return true;
												}
											}
										} else { assert(false); }
									}
								}
							}
						} else if (node_D) {
							auto decl_context = node_D->getDeclContext();
							if (decl_context && (decl_context->decls_begin() != decl_context->decls_end())) {
								const clang::Decl* previous_child = nullptr;
								for (auto child_iter = decl_context->decls_begin(); child_iter != decl_context->decls_end(); child_iter++) {
									if (nullptr != (*child_iter)) {
										if (node_D == (*child_iter)) {
											break;
										}
										previous_child = (*child_iter);
									} else {
										assert(false);
									}
								}
								if (previous_child && llvm::isa<clang::DeclaratorDecl>(previous_child)) {
									auto previous_child_DD = llvm::cast<clang::DeclaratorDecl>(previous_child);
									auto previous_child_DD_qtype = previous_child_DD->getType();
									IF_DEBUG(auto previous_child_DD_qtype_str = previous_child_DD_qtype.getAsString();)
									IF_DEBUG(std::string previous_child_DD_qtype_type_class_name = previous_child_DD_qtype->getTypeClassName();)
									auto previous_child_DD_definition_qtype = definition_qtype(previous_child_DD->getType());
									IF_DEBUG(auto previous_child_DD_definition_qtype_str = previous_child_DD_definition_qtype.getAsString();)
									IF_DEBUG(std::string previous_child_DD_definition_qtype_type_class_name = previous_child_DD_definition_qtype->getTypeClassName();)

									std::string method_name;
									if (llvm::isa<clang::FunctionDecl>(previous_child_DD)) {
										auto FND = llvm::cast<clang::FunctionDecl>(previous_child_DD);
										if (FND) {
											method_name = FND->getNameAsString();
										} else { assert(false); }
									} else if (llvm::isa<clang::CXXMethodDecl>(previous_child_DD)) {
										auto FND = llvm::cast<clang::CXXMethodDecl>(previous_child_DD);
										if (FND) {
											method_name = FND->getNameAsString();
										} else { assert(false); }
									}
									static const std::string suppress_checks_prefix = "mse_suppress_check_directive";
									if (suppress_checks_prefix == method_name.substr(0, suppress_checks_prefix.length())) {
										return true;
									}
									int q = 5;
								}
							}
						}
					}

					maybe_dyn_typed_node = parents[0];
				}
			}
			return false;
		}
	};


	class CCommonTUState1 {
	public:
		/* This container holds the locations of regions of code for which checking is
		(indicated to be) suppressed. */
		CSuppressCheckRegionSet m_suppress_check_region_set;

		/* Preprocessor symbols of interested. */
		bool m_MSE_SCOPEPOINTER_DISABLED_defined = false;
		bool m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined = false;
		bool m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined = false;
		bool raw_pointer_scope_restrictions_are_disabled() const {
			return (m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined
			|| m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined);
		}
		bool m_MSE_CHAR_STAR_EXEMPTED_defined = false;
		bool char_star_restrictions_are_disabled() const {
			return m_MSE_CHAR_STAR_EXEMPTED_defined;
		}
		bool raw_pointer_scope_restrictions_are_disabled_for_raw_pointers_with_this_target_type(const clang::QualType& qtype) const {
			bool retval = false;
			if (raw_pointer_scope_restrictions_are_disabled()) {
				retval = true;
			} else if (is_char_or_const_char(qtype)) {
				if (m_MSE_CHAR_STAR_EXEMPTED_defined) {
					retval = true;
				}
			} else if (is_FILE_or_const_FILE(qtype)) {
				/* We won't complain about 'FILE *'s for now. */
				retval = true;
			}
			return retval;
		}
		bool raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(const clang::QualType& qtype) const {
			bool retval = false;
			if (qtype.getTypePtr()->isPointerType()) {
				const auto pointee_qtype = qtype->getPointeeType();
				return raw_pointer_scope_restrictions_are_disabled_for_raw_pointers_with_this_target_type(pointee_qtype);
			} else {
				auto CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
				if (CXXRD) {
					{
						auto qname = CXXRD->getQualifiedNameAsString();

						DECLARE_CACHED_CONST_STRING(tpfl_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
						if (tpfl_str == qname) {
							const auto pointee_qtype = get_first_template_parameter_if_any(qtype).value();
							if (raw_pointer_scope_restrictions_are_disabled_for_raw_pointers_with_this_target_type(pointee_qtype)) {
								return true;
							}
						}
					}
				}
			}

			return retval;
		}
	};


	inline bool is_xscope_type(const clang::QualType qtype, const CCommonTUState1& tu_state_cref);
	inline bool is_xscope_type(const clang::Type& type, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			IF_DEBUG(auto qname = CXXRD->getQualifiedNameAsString();)
			DECLARE_CACHED_CONST_STRING(xscope_tag_str, mse_namespace_str() + "::us::impl::XScopeTagBase");
			if (has_ancestor_base_class(type, xscope_tag_str)) {
				return true;
			} else {
				static const std::string std_unique_ptr_str = "std::unique_ptr";
				auto name = CXXRD->getQualifiedNameAsString();
				if (std_unique_ptr_str == name) {
					return true;
				}
			}
		}
		if (!(tu_state_cref.m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined)) {
			if (type.isPointerType()) {
				if (!type.isFunctionPointerType()) {
					return true;
				}
			}
		}
		if (type.isReferenceType()) {
			return true;
		}

		return retval;
	}
	inline bool is_xscope_type(const clang::QualType qtype, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_xscope_type(*TP, tu_state_cref);
		}
		return retval;
	}

	inline bool contains_non_owning_scope_reference(const clang::QualType qtype, const CCommonTUState1& tu_state_cref);
	inline bool contains_non_owning_scope_reference(const clang::Type& type, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		const auto qtype = clang::QualType(&type, 0/*I'm just assuming zero specifies no qualifiers*/);
		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		auto CXXRD = type.getAsCXXRecordDecl();
		if (CXXRD) {
			DECLARE_CACHED_CONST_STRING(ContainsNonOwningScopeReference_tag_str, mse_namespace_str() + "::us::impl::ContainsNonOwningScopeReferenceTagBase");
			if (has_ancestor_base_class(*(CXXRD->getTypeForDecl()), ContainsNonOwningScopeReference_tag_str)) {
				return true;
			} else {
				auto qname = CXXRD->getQualifiedNameAsString();

				DECLARE_CACHED_CONST_STRING(tpfl_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
				if ((tpfl_str == qname) && (!tu_state_cref.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype))) {
					return true;
				}
			}
		} else if ((!tu_state_cref.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype))
			&& ((type.isPointerType()) || (type.isReferenceType()))) {
			return true;
		}
		return retval;
	}
	inline bool contains_non_owning_scope_reference(const clang::QualType qtype, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = contains_non_owning_scope_reference(*TP, tu_state_cref);
		}
		return retval;
	}

	inline bool referenceable_by_scope_pointer(const clang::QualType qtype, const CCommonTUState1& tu_state_cref);
	inline bool referenceable_by_scope_pointer(const clang::Type& type, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		auto CXXRD = type.getAsCXXRecordDecl();
		if (CXXRD) {
			DECLARE_CACHED_CONST_STRING(ReferenceableByScopePointer_tag_str, mse_namespace_str() + "::us::impl::ReferenceableByScopePointerTagBase");
			if (has_ancestor_base_class(*(CXXRD->getTypeForDecl()), ReferenceableByScopePointer_tag_str)) {
				return true;
			}
		}
		return retval;
	}
	inline bool referenceable_by_scope_pointer(const clang::QualType qtype, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = referenceable_by_scope_pointer(*TP, tu_state_cref);
		}
		return retval;
	}

	inline bool is_async_shareable(const clang::QualType qtype);
	inline bool is_async_shareable(const clang::Type& type) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			IF_DEBUG(auto qname = CXXRD->getQualifiedNameAsString();)
			static const std::string s_async_shareable_tag_str = "async_shareable_tag";
			static const std::string s_async_shareable_and_passable_tag_str = "async_shareable_and_passable_tag";
			if (has_tag_method(*CXXRD, s_async_shareable_tag_str) || has_tag_method(*CXXRD, s_async_shareable_and_passable_tag_str)) {
				return true;
			} else {
				DECLARE_CACHED_CONST_STRING(async_not_shareable_tag_str, mse_namespace_str() + "::us::impl::AsyncNotShareableTagBase");
				DECLARE_CACHED_CONST_STRING(async_not_shareable_and_not_passable_tag_str, mse_namespace_str() + "::us::impl::AsyncNotShareableAndNotPassableTagBase");
				if (has_ancestor_base_class(type, async_not_shareable_tag_str)
					|| has_ancestor_base_class(type, async_not_shareable_and_not_passable_tag_str)) {
					return false;
				} else {
					for (const auto& FD : CXXRD->fields()) {
						assert(FD);
						const auto FD_qtype = FD->getType();
						IF_DEBUG(auto FD_qtype_str = FD_qtype.getAsString();)

						if ((!is_async_shareable(FD->getType()))
							|| (FD->isMutable())) {
							return false;
						}
					}
					return true;
				}
			}
		} else if (TP->isArithmeticType()) {
			return true;
		} else if (TP->isFunctionPointerType()) {
			return true;
		} else if (TP->isEnumeralType()) {
			return true;
		} else if (TP->isMemberPointerType()) {
			return true;
		} else {
			/* todo: support pointers to non-capture lamdas and any other types we're forgetting */
		}

		return retval;
	}
	inline bool is_async_shareable(const clang::QualType qtype) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_async_shareable(*TP);
		}
		return retval;
	}

	inline bool is_async_passable(const clang::QualType qtype);
	inline bool is_async_passable(const clang::Type& type) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			IF_DEBUG(auto qname = CXXRD->getQualifiedNameAsString();)
			static const std::string s_async_passable_tag_str = "async_passable_tag";
			static const std::string s_async_passable_and_passable_tag_str = "async_passable_and_passable_tag";
			if (has_tag_method(*CXXRD, s_async_passable_tag_str) || has_tag_method(*CXXRD, s_async_passable_and_passable_tag_str)) {
				return true;
			} else {
				DECLARE_CACHED_CONST_STRING(async_not_passable_tag_str, mse_namespace_str() + "::us::impl::AsyncNotPassableTagBase");
				DECLARE_CACHED_CONST_STRING(async_not_shareable_and_not_passable_tag_str, mse_namespace_str() + "::us::impl::AsyncNotShareableAndNotPassableTagBase");
				if (has_ancestor_base_class(type, async_not_passable_tag_str)
					|| has_ancestor_base_class(type, async_not_shareable_and_not_passable_tag_str)) {
					return false;
				} else {
					for (const auto& FD : CXXRD->fields()) {
						assert(FD);
						const auto FD_qtype = FD->getType();
						IF_DEBUG(auto FD_qtype_str = FD_qtype.getAsString();)

						if (!is_async_passable(FD->getType())) {
							return false;
						}
					}
					return true;
				}
			}
		} else if (TP->isArithmeticType()) {
			return true;
		} else if (TP->isFunctionPointerType()) {
			return true;
		} else {
			/* todo: support pointers to non-capture lamdas and any other types we're forgetting */
		}

		return retval;
	}
	inline bool is_async_passable(const clang::QualType qtype) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_async_passable(*TP);
		}
		return retval;
	}


	struct CUnsupportedElementInfo {
		std::string m_name_of_unsupported;
		std::string m_slow_mode_replacement;
		std::string m_fast_mode_replacement;
		std::string m_recommended_alternative;
	};
	inline const std::vector<CUnsupportedElementInfo>& unsupported_element_infos() {
		/* In the future, this set could be augmented (at run-time, with data read from a file). */
		static const auto l_unsupported_element_infos = std::vector<CUnsupportedElementInfo> {
			{"std::thread", "mse::mstd::thread", "mse::xscope_thread", "mse::mstd::thread or mse::xscope_thread"}
			, {"std::async", "mse::mstd::async", "mse::xscope_asyc", "mse::mstd::async or mse::xscope_asyc"}
			, {"std::basic_string_view", "mse::mstd::basic_string_view", "mse::TXScopeCSSSXSTEStringSection", "a 'string section' from the SaferCPlusPlus library"}
			, {"std::span", "mse::TAnyRandomAccessSection", "mse::TXScopeCSSSXSTERandomAccessSection", "a 'random access section' from the SaferCPlusPlus library"}
			, {"std::array", "mse::mstd::array", "mse::nii_array", "a corresponding substitute from the SaferCPlusPlus library"}
			, {"std::vector", "mse::mstd::vector", "mse::stnii_vector", "a corresponding substitute from the SaferCPlusPlus library"}
			, {"std::basic_string", "mse::mstd::basic_string", "mse::stnii_basic_string", "a corresponding substitute from the SaferCPlusPlus library"}
			, {"std::__cxx11::basic_string", "mse::mstd::basic_string", "mse::stnii_basic_string", "a corresponding substitute from the SaferCPlusPlus library"}
			, {"std::string", "mse::mstd::string", "mse::stnii_string", "a corresponding substitute from the SaferCPlusPlus library"}
			, {"std::shared_ptr", "mse::TRefCountingPointer", "mse::TRefCountingPointer", "a reference counting pointer or an 'access requester' from the SaferCPlusPlus library"}
			, {"std::unique_ptr", "mse::TRefCountingPointer", "mse::TXScopeOwnerPointer", "mse::TXScopeOwnerPointer<> or a reference counting pointer from the SaferCPlusPlus library"}
			, {"std::function", "mse::mstd::function", "mse::xscope_function", "mse::mstd::function or mse::xscope_function"}
			, {"std::tuple", "mse::mstd::tuple", "mse::xscope_tuple", "mse::mstd::tuple or mse::xscope_tuple"}
			};
		return l_unsupported_element_infos;
	}
	namespace impl {
		inline auto unsupported_element_info_vector_to_unordered_map(const std::vector<CUnsupportedElementInfo>& vec) {
			std::unordered_map<std::string, CUnsupportedElementInfo> retval;
			for (const auto& item : vec) {
				retval.insert(typename std::unordered_map<std::string, CUnsupportedElementInfo>::value_type(item.m_name_of_unsupported, item));
			}
			return retval;
		}
		inline const std::unordered_map<std::string, CUnsupportedElementInfo>& unsupported_element_infos_uo_map() {
			static const auto l_unsupported_element_infos_uo_map = unsupported_element_info_vector_to_unordered_map(unsupported_element_infos());
			return l_unsupported_element_infos_uo_map;
		}
	}
	inline CUnsupportedElementInfo const * unsupported_element_info_ptr(const std::string& name) {
		CUnsupportedElementInfo const * retval = nullptr;
		auto iter = impl::unsupported_element_infos_uo_map().find(name);
		if (impl::unsupported_element_infos_uo_map().end() != iter) {
			retval = &((*iter).second);
		}
		return retval;
	}


	inline std::vector<clang::QualType> types_from_template_arg(const clang::TemplateArgument& template_arg) {
		std::vector<clang::QualType> retval;

		const auto kind = template_arg.getKind();
		if (clang::TemplateArgument::Type == template_arg.getKind()) {
			const auto ta_qtype = template_arg.getAsType();
			IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
			retval.push_back(ta_qtype);
		} else if (clang::TemplateArgument::Pack == template_arg.getKind()) {
			const auto pack_size = template_arg.pack_size();
			for (const auto& pack_element : template_arg.pack_elements()) {
				auto res = types_from_template_arg(pack_element);
				retval.insert(retval.end(), res.begin(), res.end());
			}
		} else {
			int q = 5;
		}

		return retval;
	}

	inline std::vector<clang::QualType> shallow_component_types_if_any(const clang::QualType& qtype) {
		std::vector<clang::QualType> retval;
		IF_DEBUG(const auto qtype_str = qtype.getAsString();)
		//const auto TP = &type;
		const auto TP = remove_fparam_wrappers(qtype);
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			IF_DEBUG(auto qname = CXXRD->getQualifiedNameAsString();)

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				const auto& template_args = CTSD->getTemplateInstantiationArgs();
				const auto num_args = template_args.size();
				for (int i = 0; i < int(num_args); i += 1) {
					const auto template_arg = template_args[i];
					auto res = types_from_template_arg(template_arg);
					retval.insert(retval.end(), res.begin(), res.end());
				}
			}
		} else if (qtype.getTypePtr()->isReferenceType()) {
			auto pointee_qtype = qtype.getTypePtr()->getPointeeType();
			retval.push_back(pointee_qtype);
		} else if (qtype.getTypePtr()->isPointerType()) {
			auto pointee_qtype = qtype.getTypePtr()->getPointeeType();
			retval.push_back(pointee_qtype);
		}
		return retval;
	}

	template<typename TLambda, typename ...Args>
	inline void apply_to_component_types_if_any(const clang::QualType& qtype, const TLambda& lambda, Args&&...args) {
		const auto targs = shallow_component_types_if_any(qtype);
		for (const auto& targ : targs) {
			apply_to_component_types_if_any(targ, lambda, args...);
			lambda(targ, args...);
		}
	}

	inline std::vector<clang::TypeLoc> shallow_component_types_if_any(clang::TypeLoc typeLoc) {
		std::vector<clang::TypeLoc> retval;
		auto qtype = typeLoc.getType();
		IF_DEBUG(const auto qtype_str = qtype.getAsString();)

		auto Specialization = typeLoc.getAs<clang::TemplateSpecializationTypeLoc>();
		while (!Specialization) {
			auto etl = typeLoc.getAs<clang::ElaboratedTypeLoc>();
			if (etl) {
				typeLoc = etl.getNamedTypeLoc();
				Specialization = typeLoc.getAs<clang::TemplateSpecializationTypeLoc>();
			} else {
				break;
			}
		}

		if (Specialization) {
			for (unsigned i = 0; i < Specialization.getNumArgs(); i += 1) {
				auto ArgumentLoc = Specialization.getArgLoc(i);
				const auto kind = ArgumentLoc.getArgument().getKind();
				if (kind == clang::TemplateArgument::Type) {
					retval.push_back(ArgumentLoc.getTypeSourceInfo()->getTypeLoc());
				} else if (kind == clang::TemplateArgument::Integral) {
					int q = 5;
				} else if (kind == clang::TemplateArgument::Expression) {
					int q = 5;
				} else {
					int q = 5;
				}
			}
		} else {
			auto Reference = typeLoc.getAs<clang::ReferenceTypeLoc>();
			if (Reference) {
				auto RefPointeeLoc = Reference.getPointeeLoc();
				retval.push_back(RefPointeeLoc);
			} else {
				auto Pointer = typeLoc.getAs<clang::PointerTypeLoc>();
				if (Pointer) {
					auto PointeeLoc = Pointer.getPointeeLoc();
					retval.push_back(PointeeLoc);
				}
			}
		}

		return retval;
	}

	template<typename TLambda, typename ...Args>
	inline void apply_to_component_types_if_any(const clang::TypeLoc& typeLoc, const TLambda& lambda, Args&&...args) {
		const auto targs = shallow_component_types_if_any(typeLoc);
		for (const auto& targ : targs) {
			apply_to_component_types_if_any(targ, lambda, args...);
			lambda(targ, targ.getSourceRange(), args...);
		}
	}
	template<typename TLambda, typename ...Args>
	inline void apply_to_component_types_if_any(const clang::DeclaratorDecl& ddecl, const TLambda& lambda, Args&&...args) {
		apply_to_component_types_if_any(ddecl.getTypeSourceInfo()->getTypeLoc(), lambda, args...);
	}

	template <typename DescendantT, typename NodeT>
	inline auto Tget_descendant_of_type(const NodeT* NodePtr, clang::ASTContext& context) {
		const DescendantT* retval = nullptr;
		if (!NodePtr) {
			return retval;
		}
		const auto children = NodePtr->children();
		for (const auto& child : children) {
			retval = clang::dyn_cast<const DescendantT>(child);
			if (retval) {
				return retval;
			} else {
				retval = Tget_descendant_of_type<DescendantT>(child, context);
				if (retval) {
					return retval;
				}
			}
		}
		return retval;
	}


#endif //__UTILS1_H
