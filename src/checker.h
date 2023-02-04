// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef __CHECKER_H
#define __CHECKER_H

#include "utils1.h"

/*Standard headers*/
#include <string>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <algorithm>
#include <variant>
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


namespace checker {
    using namespace llvm;
    using namespace clang;
    using namespace clang::ast_matchers;
    using namespace clang::driver;
    using namespace clang::tooling;

	static std::string s_target_debug_location_init_val() {
		std::string retval = "test1_1proj.cpp:539:";
		static const std::string src_pathname = "target_debug_location.txt";
		std::ifstream src;
		src.open(src_pathname, std::ios::binary);
		if (((src.rdstate() & std::ifstream::failbit ) != 0)) {
		} else {
			src >> retval;
			src.close();
		}
		return retval;
	}

	static const std::string g_target_debug_source_location_str1 = s_target_debug_location_init_val();

    bool CheckSystemHeader = false;
    bool MainFileOnly = false;
    bool CTUAnalysis = false;
    bool EnableNamespaceImport = false;
    bool SuppressPrompts = false;

    struct Options {
        bool CheckSystemHeader = false;
        bool MainFileOnly = false;
        bool CTUAnalysis = false;
        bool EnableNamespaceImport = false;
        bool SuppressPrompts = false;
    };


	class CErrorRecord {
		public:
		CErrorRecord() : m_id(next_available_id()) {}
		CErrorRecord(SourceManager& SM, clang::SourceLocation SL, std::string error_description_str = "", std::string tag_name = "")
			: m_SL(SL), m_source_location_str(s_source_location_str(SM, SL)), m_error_description_str(error_description_str)
			, m_tag_name(tag_name), m_id(next_available_id()) {}

		std::string as_a_string1() const {
			std::string retval = m_source_location_str + ": " + m_severity_str + ": " + m_error_description_str;
			if ("" != m_tag_name) {
				retval += " (" + m_tag_name + ") ";
			}
			return retval;
		}
		bool operator<(const CErrorRecord &RHS) const {
			//return (m_id < RHS.m_id);
			if (m_source_location_str != RHS.m_source_location_str) {
				return m_source_location_str < RHS.m_source_location_str;
			} else {
				return m_error_description_str < RHS.m_error_description_str;
			}
		}
		static std::string s_source_location_str(SourceManager& SM, clang::SourceLocation SL) {
			return SL.printToString(SM);
		}

		clang::SourceLocation m_SL;
		std::string m_source_location_str;
		std::string m_error_description_str;
		std::string m_tag_name;
		std::string m_severity_str = "error";
		int m_id = 0;
	private:
		int next_available_id() const { return m_next_available_id/*++*/; }
		static int m_next_available_id;
	};
	int CErrorRecord::m_next_available_id = 0;
	class CErrorRecords : public std::set<CErrorRecord> {};

	clang::FunctionDecl const * enclosing_function_if_any(clang::Expr const * E, clang::ASTContext& context) {
		clang::FunctionDecl const * retval = nullptr;
		if (E) {
			retval = Tget_containing_element_of_type<clang::FunctionDecl>(E, context);
		}
		return retval;
	}
	clang::FunctionDecl const * enclosing_function_if_any(clang::VarDecl const * VD) {
		clang::FunctionDecl const * retval = nullptr;
		if (VD) {
			auto DC = VD->getDeclContext();
			retval = DC ? dyn_cast<const clang::FunctionDecl>(DC) : nullptr;
			if (retval) {
				IF_DEBUG(std::string function_name = retval->getNameAsString();)
				int q = 5;
			} else {
				int q = 5;
			}
		}
		return retval;
	}
	clang::FunctionDecl const * function_from_param(clang::ParmVarDecl const * PVD) {
		return enclosing_function_if_any(PVD);
	}

	struct CPairwiseLifetimeConstraint {
	public:
		CPairwiseLifetimeConstraint(const CAbstractLifetime& first, const CAbstractLifetime& second) : m_first(first), m_second(second) {}
		virtual std::string species_str() const { return ""; }
		enum class EYesNoDontKnow { Yes, No, DontKnow };
		virtual EYesNoDontKnow second_can_be_assigned_to_first(const std::optional<CAbstractLifetime> maybe_first = {}, const std::optional<CAbstractLifetime> maybe_second = {}) const {
			EYesNoDontKnow retval = EYesNoDontKnow::DontKnow;
			return retval;
		}
		bool operator==(const CPairwiseLifetimeConstraint& rhs) const {
			return ((rhs.m_first == m_first) && (rhs.m_second == m_second));
		}
		CAbstractLifetime m_first;
		CAbstractLifetime m_second;
	};
	struct CEncompasses : public CPairwiseLifetimeConstraint {
		typedef CPairwiseLifetimeConstraint base_class;
		CEncompasses(const CAbstractLifetime& first, const CAbstractLifetime& second) : base_class{ first, second } {}
		virtual std::string species_str() const { return "encompasses"; }
		virtual EYesNoDontKnow second_can_be_assigned_to_first(const std::optional<CAbstractLifetime> maybe_first = {}, const std::optional<CAbstractLifetime> maybe_second = {}) const override {
			EYesNoDontKnow retval = EYesNoDontKnow::DontKnow;
			auto first = maybe_first.value_or(m_first);
			auto second = maybe_second.value_or(m_second);
			if ((first == m_first) && (second == m_second)) {
				retval = EYesNoDontKnow::No;
			} else if ((first == m_second) && (second == m_first)) {
				retval = EYesNoDontKnow::Yes;
			}
			return retval;
		}
	};

	typedef size_t param_ordinal_t;
#define IMPLICIT_THIS_PARAM_ORDINAL 0
 
	class CFunctionLifetimeAnnotations {
	public:
		std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> m_param_lifetime_map;
		std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > m_lifetime_constraint_shptrs;
		CAbstractLifetimeSet m_return_value_lifetimes;
		bool m_parse_errors_noted = false;
	};

	class CTypeLifetimeAnnotations {
	public:
		CAbstractLifetimeSet m_lifetime_set;
		std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > m_lifetime_constraint_shptrs;
		bool m_parse_errors_noted = false;
	};

	class CVariableLifetimeAnnotations {
	public:
		CAbstractLifetimeSet m_lifetime_set;
		std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > m_lifetime_constraint_shptrs;
		bool m_parse_errors_noted = false;
	};

	class CVariableLifetimeValues {
	public:
		CScopeLifetimeInfo1 m_scope_lifetime_info;
		bool m_errors_noted = false;
	};

	class CExpressionLifetimeValues {
	public:
		CScopeLifetimeInfo1 m_scope_lifetime_info;
		bool m_errors_noted = false;
	};

	class CTUState : public CCommonTUState1 {
	public:
		/* Set of detected errors and warnings. */
		CErrorRecords m_error_records;

		std::unordered_map<clang::FunctionDecl const *, CFunctionLifetimeAnnotations> m_function_lifetime_annotations_map;

		std::unordered_map<const clang::Type *, CTypeLifetimeAnnotations> m_type_lifetime_annotations_map;

		std::unordered_multimap<CAbstractLifetime, std::shared_ptr<CPairwiseLifetimeConstraint> > m_lhs_to_lifetime_constraint_shptr_mmap;
		std::unordered_multimap<CAbstractLifetime, std::shared_ptr<CPairwiseLifetimeConstraint> > m_rhs_to_lifetime_constraint_shptr_mmap;
		std::unordered_map<CAbstractLifetime, CAbstractLifetimeSet> m_lifetime_alias_map;

		std::unordered_map<clang::VarDecl const *, CVariableLifetimeAnnotations> m_vardecl_lifetime_annotations_map;
		std::unordered_map<clang::VarDecl const *, CVariableLifetimeValues> m_vardecl_lifetime_values_map;
		std::unordered_map<clang::Expr const *, CExpressionLifetimeValues> m_expr_lifetime_values_map;

		std::unordered_map<clang::FieldDecl const *, CAbstractLifetimeSet> m_fielddecl_to_abstract_lifetime_map;

		typedef CPairwiseLifetimeConstraint::EYesNoDontKnow EYesNoDontKnow;

		EYesNoDontKnow second_can_be_assigned_to_first(const CAbstractLifetime& first, const CAbstractLifetime& second) const {
			EYesNoDontKnow retval = EYesNoDontKnow::DontKnow;
			if (first == second) {
				return EYesNoDontKnow::Yes;
			}
			{
				auto range = m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(second);
				for (auto it = range.first; range.second != it; ++it) {
					auto res1 = it->second->second_can_be_assigned_to_first(first, second);
					if (EYesNoDontKnow::No == res1) {
						retval = res1;
						break;
					} else if (EYesNoDontKnow::Yes == res1) {
						retval = res1;
					}
				}
			}
			if (!(EYesNoDontKnow::No == retval)) {
				auto range = m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(first);
				auto begin_iter = range.first;
				auto end_iter = range.second;
				for (auto it = begin_iter; end_iter != it; ++it) {
					auto res1 = it->second->second_can_be_assigned_to_first(first, second);
					if (EYesNoDontKnow::No == res1) {
						retval = res1;
						break;
					} else if (EYesNoDontKnow::Yes == res1) {
						retval = res1;
					}
				}
			}
			return retval;
		}
		std::optional<CAbstractLifetimeSet> corresponding_abstract_lifetime_set_if_any(clang::VarDecl const * VD) const {
			auto iter = m_vardecl_lifetime_annotations_map.find(VD);
			if (m_vardecl_lifetime_annotations_map.end() != iter) {
				return iter->second.m_lifetime_set;
			}
			return std::optional<CAbstractLifetimeSet>{};
		}
		std::optional<CAbstractLifetime> corresponding_abstract_lifetime_if_any(clang::CXXThisExpr const * CXXTE, clang::ASTContext& context) const {
			const auto FND = Tget_containing_element_of_type<clang::FunctionDecl>(CXXTE, context);
			if (FND) {
				auto flta_iter = m_function_lifetime_annotations_map.find(FND);
				if (m_function_lifetime_annotations_map.end() != flta_iter) {
					auto pl_iter = flta_iter->second.m_param_lifetime_map.find(IMPLICIT_THIS_PARAM_ORDINAL);
					if (flta_iter->second.m_param_lifetime_map.end() != pl_iter) {
						auto& alts1 = pl_iter->second;
						if (1 == alts1.m_primary_lifetimes.size()) {
							return alts1.m_primary_lifetimes.front();
						} else {
							assert(false);
							return {};
						}
					}
				}
			}
			return std::optional<CAbstractLifetime>{};
		}
		std::optional<CAbstractLifetimeSet> corresponding_abstract_lifetime_set_if_any(clang::FieldDecl const * FD) const {
			if (FD) {
				auto iter1 = m_fielddecl_to_abstract_lifetime_map.find(FD);
				if (m_fielddecl_to_abstract_lifetime_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CAbstractLifetimeSet>{};
		}

		std::optional<CVariableLifetimeValues> corresponding_lifetime_values_if_any(clang::VarDecl const * VD) const {
			if (VD) {
				auto iter1 = m_vardecl_lifetime_values_map.find(VD);
				if (m_vardecl_lifetime_values_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CVariableLifetimeValues>{};
		}
		std::optional<CExpressionLifetimeValues> corresponding_lifetime_values_if_any(clang::Expr const * E) const {
			if (E) {
				auto iter1 = m_expr_lifetime_values_map.find(E);
				if (m_expr_lifetime_values_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CExpressionLifetimeValues>{};
		}
	};

	class MCSSSSuppressCheckDirectiveCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSuppressCheckDirectiveCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcssssuppresscheckcall");

			if ((CE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				//assert(0 == num_args);
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(suppress_check_directive_str, mse_namespace_str() + "::rsv::suppress_check_directive");
					if (suppress_check_directive_str == qualified_function_name) {
						bool parent_obtained = false;
						const Stmt* ST = CE;
						while (true) {
							//get parents
							const auto& parents = MR.Context->getParents(*ST);
							if ( parents.empty() ) {
								llvm::errs() << "Can not find parent\n";
								break;
							}
							//llvm::errs() << "find parent size=" << parents.size() << "\n";
							ST = parents[0].get<Stmt>();
							if (!ST) {
								break;
							}
							//ST->dump();
							if (isa<CompoundStmt>(ST)) {
								parent_obtained = true;
								break;
							}
						}
						if (!parent_obtained) {
							int q = 3;
						} else {
							const clang::Stmt* next_child = nullptr;
							for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
								if (nullptr != (*child_iter)) {
									if (CE == (*child_iter)) {
										++child_iter;
										if (child_iter != ST->child_end()) {
											next_child = (*child_iter);
										}
										break;
									}
								} else {
									assert(false);
								}
							}
							if (next_child && llvm::isa<clang::Stmt>(next_child)) {
								auto next_child_ST = llvm::cast<clang::Stmt>(next_child);
								IF_DEBUG(auto next_child_ST_class_name = next_child_ST->getStmtClassName();)

								auto l_ISR = instantiation_source_range(next_child_ST->getSourceRange(), Rewrite);
								DEBUG_SOURCE_LOCATION_STR(debug_source_location_str2, l_ISR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(debug_source_text2, l_ISR, Rewrite);
								SourceLocation l_ISL = l_ISR.getBegin();
								SourceLocation l_ISLE = l_ISR.getEnd();

#ifndef NDEBUG
								if (std::string::npos != debug_source_location_str2.find(g_target_debug_source_location_str1)) {
									int q = 5;
								}
#endif /*!NDEBUG*/
								m_state1.m_suppress_check_region_set.emplace(l_ISR);
								m_state1.m_suppress_check_region_set.insert(next_child_ST);

								int q = 5;
							}
						}
					}

				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSSuppressCheckDirectiveDeclField : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSuppressCheckDirectiveDeclField (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::CXXMethodDecl* CXXMD = MR.Nodes.getNodeAs<clang::CXXMethodDecl>("mcssssuppresscheckmemberdecl");

			if ((CXXMD != nullptr))
			{
				auto SR = nice_source_range(CXXMD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto method_name = CXXMD->getNameAsString();
				static const std::string suppress_checks_prefix = "mse_suppress_check_directive";
				if (suppress_checks_prefix == method_name.substr(0, suppress_checks_prefix.length())) {
					auto decl_context = CXXMD->getDeclContext();
					if (decl_context && (decl_context->decls_begin() != decl_context->decls_end())) {
						const clang::Decl* next_child = nullptr;
						for (auto child_iter = decl_context->decls_begin(); child_iter != decl_context->decls_end(); child_iter++) {
							if (nullptr != (*child_iter)) {
								if (CXXMD == (*child_iter)) {
									++child_iter;
									if (child_iter != decl_context->decls_end()) {
										next_child = (*child_iter);
									}
									break;
								}
							} else {
								assert(false);
							}
						}
						if (next_child && llvm::isa<clang::DeclaratorDecl>(next_child)) {
							auto next_child_DD = llvm::cast<clang::DeclaratorDecl>(next_child);
							auto next_child_DD_qtype = next_child_DD->getType();
							IF_DEBUG(auto next_child_DD_qtype_str = next_child_DD_qtype.getAsString();)
							IF_DEBUG(std::string next_child_DD_qtype_type_class_name = next_child_DD_qtype->getTypeClassName();)
							auto next_child_DD_definition_qtype = definition_qtype(next_child_DD->getType());
							IF_DEBUG(auto next_child_DD_definition_qtype_str = next_child_DD_definition_qtype.getAsString();)
							IF_DEBUG(std::string next_child_DD_definition_qtype_type_class_name = next_child_DD_definition_qtype->getTypeClassName();)

							auto l_ISR = instantiation_source_range(next_child_DD->getSourceRange(), Rewrite);
							DEBUG_SOURCE_LOCATION_STR(debug_source_location_str2, l_ISR, Rewrite);
							DEBUG_SOURCE_TEXT_STR(debug_source_text2, l_ISR, Rewrite);
							SourceLocation l_ISL = l_ISR.getBegin();
							SourceLocation l_ISLE = l_ISR.getEnd();

#ifndef NDEBUG
							if (std::string::npos != debug_source_location_str2.find(g_target_debug_source_location_str1)) {
								int q = 5;
							}
#endif /*!NDEBUG*/
							m_state1.m_suppress_check_region_set.emplace(l_ISR);
							m_state1.m_suppress_check_region_set.insert(next_child_DD);

							int q = 5;
						}
					}
				}

			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSSuppressCheckDirectiveDeclGlobal : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSuppressCheckDirectiveDeclGlobal (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::FunctionDecl* FND = MR.Nodes.getNodeAs<clang::FunctionDecl>("mcssssuppresscheckglobaldecl");

			if ((FND != nullptr))
			{
				auto SR = nice_source_range(FND->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto method_name = FND->getNameAsString();
				static const std::string suppress_checks_prefix = "mse_suppress_check_directive";
				if (suppress_checks_prefix == method_name.substr(0, suppress_checks_prefix.length())) {
					if (true) {
						auto decl_context = FND->getDeclContext();
						if (decl_context && (decl_context->decls_begin() != decl_context->decls_end())) {
							const clang::Decl* next_child = nullptr;
							for (auto child_iter = decl_context->decls_begin(); child_iter != decl_context->decls_end(); child_iter++) {
								if (nullptr != (*child_iter)) {
									if (FND == (*child_iter)) {
										++child_iter;
										if (child_iter != decl_context->decls_end()) {
											next_child = (*child_iter);
										}
										break;
									}
								} else {
									assert(false);
								}
							}
							if (next_child && llvm::isa<clang::DeclaratorDecl>(next_child)) {
								auto next_child_DD = llvm::cast<clang::DeclaratorDecl>(next_child);
								auto next_child_DD_qtype = next_child_DD->getType();
								IF_DEBUG(auto next_child_DD_qtype_str = next_child_DD_qtype.getAsString();)
								IF_DEBUG(std::string next_child_DD_qtype_type_class_name = next_child_DD_qtype->getTypeClassName();)
								auto next_child_DD_definition_qtype = definition_qtype(next_child_DD->getType());
								IF_DEBUG(auto next_child_DD_definition_qtype_str = next_child_DD_definition_qtype.getAsString();)
								IF_DEBUG(std::string next_child_DD_definition_qtype_type_class_name = next_child_DD_definition_qtype->getTypeClassName();)

								auto l_ISR = instantiation_source_range(next_child_DD->getSourceRange(), Rewrite);
								DEBUG_SOURCE_LOCATION_STR(debug_source_location_str2, l_ISR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(debug_source_text2, l_ISR, Rewrite);
								SourceLocation l_ISL = l_ISR.getBegin();
								SourceLocation l_ISLE = l_ISR.getEnd();

#ifndef NDEBUG
								if (std::string::npos != debug_source_location_str2.find(g_target_debug_source_location_str1)) {
									int q = 5;
								}
#endif /*!NDEBUG*/
								m_state1.m_suppress_check_region_set.emplace(l_ISR);
								m_state1.m_suppress_check_region_set.insert(next_child_DD);

								int q = 5;
							}
						}
					} else {
						auto decl_context = FND->getDeclContext();
						if (!decl_context) {
							assert(false);
						} else {
							auto FNDISR = instantiation_source_range(FND->getSourceRange(), Rewrite);
							auto FNDISL = FNDISR.getBegin();
							auto FNDISLE = FNDISR.getEnd();

							for (auto decl_iter = decl_context->decls_begin(); decl_iter != decl_context->decls_end(); decl_iter++) {
								if (nullptr != (*decl_iter)) {
									auto l_ISR = instantiation_source_range((*decl_iter)->getSourceRange(), Rewrite);
									DEBUG_SOURCE_LOCATION_STR(debug_source_location_str2, l_ISR, Rewrite);
									DEBUG_SOURCE_TEXT_STR(debug_source_text2, l_ISR, Rewrite);
									SourceLocation l_ISL = l_ISR.getBegin();
									SourceLocation l_ISLE = l_ISR.getEnd();

									if (filtered_out_by_location(MR, l_ISL)) {
										continue;
									}

									std::string l_source_text;
									if (l_ISL.isValid() && l_ISLE.isValid()) {
										l_source_text = Rewrite.getRewrittenText(SourceRange(l_ISL, l_ISLE));
									} else {
										continue;
									}
									if ("" != l_source_text) {
										int q = 5;
									}

									if (FNDISL == l_ISL) {
										int q = 5;
									}
									if ((FNDISLE < l_ISL)
										|| ((FNDISLE == l_ISL) && (FNDISLE < l_ISLE))) {

	#ifndef NDEBUG
										if (std::string::npos != debug_source_location_str2.find(g_target_debug_source_location_str1)) {
											int q = 5;
										}
	#endif /*!NDEBUG*/
										m_state1.m_suppress_check_region_set.emplace(l_ISR);
										m_state1.m_suppress_check_region_set.insert(*decl_iter);
										break;
									}
								} else {
									assert(false);
								}
							}
						}
					}
				}

			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	bool is_raw_pointer_or_equivalent(const clang::QualType& qtype) {
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(qtype, false);

		bool is_pointer_or_equivalent = true;
		if (!qtype->isPointerType()) {
			const auto RD = qtype->getAsRecordDecl();
			if (!RD) {
				is_pointer_or_equivalent = false;
			} else {
				/* `mse::us::impl::TPointerForLegacy<>` is sometimes used as (a functionally
				equivalent) substitute for native pointers that can act as a base class. */
				const auto CXXCE_rw_qtype_str = RD->getQualifiedNameAsString();
				DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
				if (TPointerForLegacy_str != CXXCE_rw_qtype_str) {
					is_pointer_or_equivalent = false;
				}
			}
		}
		return is_pointer_or_equivalent;
	}

	inline std::optional<clang::QualType> pointee_type_if_any(clang::QualType const& qtype) {
		std::optional<clang::QualType> retval;
		auto peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(peeled_qtype, retval);
		if (is_raw_pointer_or_equivalent(peeled_qtype)) {
			if (peeled_qtype->isPointerType()) {
				retval = peeled_qtype->getPointeeType();
			} else {
				auto RD = peeled_qtype->getAsRecordDecl();
				if (RD) {
					const auto RD_qtype_str = RD->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
					if (TPointerForLegacy_str == RD_qtype_str) {
						auto maybe_qtype2 = get_first_template_parameter_if_any(peeled_qtype);
						if (maybe_qtype2.has_value()) {
							retval = maybe_qtype2.value();
						}
					}
				}
			}
		} else if (peeled_qtype->isReferenceType()) {
			retval = peeled_qtype->getPointeeType();
		}
		return retval;
	}

	template<typename TCallExpr>
	auto arg_from_param_ordinal(TCallExpr const * CE, checker::param_ordinal_t param_ordinal) {
		clang::Expr const * retval = nullptr;
		if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
			auto CXXMCE = dyn_cast<CXXMemberCallExpr>(CE);
			auto CXXOCE = dyn_cast<clang::CXXOperatorCallExpr>(CE);
			if (CXXMCE) {
				retval = CXXMCE->getImplicitObjectArgument();
			} else if (CXXOCE) {
				if (CE->getNumArgs() < 1) {
					/* This should never happen, right? */
					//continue;
				} else {
					/* For CXXOperatorCallExpr, they just make the "ImplicitObjectArgument" (if any) the first
					argument. I think. */
					retval = CE->getArg(0);
				}
				//retval = CXXOCE->getImplicitObjectArgument();
			} else {
				//assert(false);?
				//todo: report error?
			}
		} else {
			if (!(1 <= param_ordinal)) {
				assert(false);
				//continue;
			}
			if (CE->getNumArgs() < param_ordinal) {
				/* If this happens then either an error should be reported elsewhere
				or there should be a compile error. */
				//continue;
			} else {
				retval = CE->getArg(int(param_ordinal - 1));
			}
		}
		return retval;
	};

	void process_type_lifetime_annotations(const clang::TypeDecl& type_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);

	auto new_or_existing_lifetime_from_label_id(const lifetime_id_t& label_id, const std::variant<clang::FunctionDecl const *, clang::Decl const *>& context, const std::optional<CAbstractLifetimeSet>& maybe_containing_type_alts) {
		CAbstractLifetime lifetime = CAbstractLifetime{ label_id, context };
		if (maybe_containing_type_alts.has_value()) {
			{
				auto maybe_already_declared_lifetime = maybe_containing_type_alts.value().lifetime_from_label_id_if_present(label_id);
				if (maybe_already_declared_lifetime.has_value()) {
					lifetime = maybe_already_declared_lifetime.value();
				}
			}
		}
		return lifetime;
	}

	CAbstractLifetimeSet parse_lifetime_ids(std::string_view sv1, std::variant<clang::FunctionDecl const *, clang::Decl const *> context, clang::SourceRange attr_SR, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		CAbstractLifetimeSet retval;

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
		bool suppress_check_flag = false;
		if (MR_ptr && Rewrite_ptr) {
			auto& MR = *MR_ptr;
			auto& Rewrite = *Rewrite_ptr;

			auto raw_SR = attr_SR;
			auto SR = nice_source_range(raw_SR, Rewrite);
			if (SR.isValid()) {
				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
				debug_source_location_str2 = debug_source_location_str;
				debug_source_text2 = debug_source_text;
#endif /*!NDEBUG*/
			}
		}

		std::optional<CAbstractLifetimeSet> maybe_containing_type_alts;
		if (std::holds_alternative<clang::FunctionDecl const *>(context)) {
			auto FD = std::get<clang::FunctionDecl const *>(context);
			auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(FD);
			if (CXXMD) {
				IF_DEBUG(const std::string qtype_str = CXXMD->getThisType()->getPointeeType().getAsString();)
				auto Type_ptr = CXXMD->getThisType()->getPointeeType().getTypePtr();
				auto containing_RD = Type_ptr->getAsRecordDecl();
				if (containing_RD) {
					process_type_lifetime_annotations(*containing_RD, state1, MR_ptr);

					auto iter = state1.m_type_lifetime_annotations_map.find(Type_ptr);
					if (state1.m_type_lifetime_annotations_map.end() != iter) {
						if (!(iter->second.m_lifetime_set.is_empty())) {
							maybe_containing_type_alts = iter->second.m_lifetime_set;
						}
					}
				}
			}
		}

		std::string lifetime_label_ids_str( sv1 );
		auto lifetime_label_ids = with_whitespace_removed(lifetime_label_ids_str);

		static const auto NPos = decltype(lifetime_label_ids)::npos;

		size_t one_after_last_comma_index = 0;

		while (lifetime_label_ids.length() > one_after_last_comma_index) {
			auto lbracket_index = lifetime_label_ids.find('[', one_after_last_comma_index);
			auto next_comma_index = lifetime_label_ids.find(',', one_after_last_comma_index);
			if (NPos == next_comma_index) {
				next_comma_index = lifetime_label_ids.length();
			}

			CAbstractLifetime abstract_lifetime;
			abstract_lifetime.m_context = context;

			std::string primary_lifetime_label_id_str = lifetime_label_ids.substr(one_after_last_comma_index, int(next_comma_index) - int(one_after_last_comma_index));
			if ((NPos != lbracket_index) && (lbracket_index + 1 < next_comma_index)) {
				if (one_after_last_comma_index + 1 <= lbracket_index) {
					primary_lifetime_label_id_str = lifetime_label_ids.substr(one_after_last_comma_index, int(lbracket_index) - int(one_after_last_comma_index));
				} else {
					primary_lifetime_label_id_str = "";
				}

				auto rbracket_index = Parse::find_matching_right_bracket(lifetime_label_ids, lbracket_index + 1);
				if (lifetime_label_ids.length() <= rbracket_index) {
					rbracket_index = lifetime_label_ids.length();
					if (MR_ptr) {
						std::string error_desc = std::string("Matching close bracket not found.");
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}

				if (lifetime_label_ids.length() <= rbracket_index) {
					next_comma_index = lifetime_label_ids.length();
				} else {
					next_comma_index = lifetime_label_ids.find(',', rbracket_index);
					if (NPos == next_comma_index) {
						next_comma_index = lifetime_label_ids.length();
					}
				}

				if (1 < next_comma_index - rbracket_index) {
					if (MR_ptr) {
						std::string error_desc = std::string("Unexpected character(s) after close bracket.");
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
				std::string sublifetime_label_ids = lifetime_label_ids.substr(lbracket_index + 1, int(rbracket_index) - int(lbracket_index + 1));
				auto sub_alts = parse_lifetime_ids(sublifetime_label_ids, context, attr_SR, state1, MR_ptr, Rewrite_ptr);
				*(abstract_lifetime.m_sublifetimes_vlptr) = sub_alts;
			}
			abstract_lifetime.m_id = primary_lifetime_label_id_str;
			if (maybe_containing_type_alts.has_value()) {
				auto maybe_existing_alt = maybe_containing_type_alts.value().lifetime_from_label_id_if_present(primary_lifetime_label_id_str);
				if (maybe_existing_alt.has_value()) {
					abstract_lifetime = maybe_existing_alt.value();
				}
			}

			retval.m_primary_lifetimes.push_back(abstract_lifetime);

			one_after_last_comma_index = next_comma_index + 1;
		}

		return retval;
	}

	inline auto type_lifetime_annotations_if_available(const clang::Type * TypePtr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> std::optional<CTypeLifetimeAnnotations *>;


	auto populate_lifetime_alias_map(const clang::Type * TypePtr2, const CAbstractLifetimeSet& alts, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};
		if (TypePtr2) {
			if (TypePtr2->isUndeducedType()) {
				int q = 5;
			}
		}

		auto template_params = get_template_parameters(TypePtr2);
		auto num_lt_aliases = alts.m_primary_lifetimes.size();
		size_t count = 0;
		for (size_t i = 0; i < num_lt_aliases; i += 1) {
			auto& alias = alts.m_primary_lifetimes.at(i);
			if ("" != alias.m_id) {
				CAbstractLifetimeSet unaliased_lifetimes;
				if (template_params.size() > i) {
					auto maybe_tlta_ptr = type_lifetime_annotations_if_available(template_params.at(i).getTypePtr(), state1, MR_ptr, Rewrite_ptr);
					if (maybe_tlta_ptr.has_value()) {
						auto unaliased_prefix1 = "__lifetime_set_" + alias.m_id;
						auto& tlta = *(maybe_tlta_ptr.value());

						for (auto& lifetime : tlta.m_lifetime_set.m_primary_lifetimes) {
							/* Ok, we now have a set of (the template parameter) type's declared lifetimes. Now we want to
							create corresponding (distinct) lifetimes in our template type. */
							CAbstractLifetime unaliased_lifetime;
							unaliased_lifetime.m_context = alias.m_context;
							count += 1;
							unaliased_lifetime.m_id = unaliased_prefix1 + "_" + std::to_string(count) + "_" + lifetime.m_id;

							/* And then we'll add this lifetime to the set of lifetimes the lifetime set alias represents. */
							unaliased_lifetimes.m_primary_lifetimes.push_back(unaliased_lifetime);
						}
					}
				} else {
					/* In the case of a template definition (as opposed to a template instantiation),
					get_template_parameters() won't return any parameters. In this case we'll just map the
					specified aliases to the empty set. */
					int q = 5;
				}
				state1.m_lifetime_alias_map.insert_or_assign(alias, unaliased_lifetimes);

				retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), unaliased_lifetimes.m_primary_lifetimes.begin(), unaliased_lifetimes.m_primary_lifetimes.end());
			}

			if (!(alias.m_sublifetimes_vlptr->is_empty())) {
				auto res1 = populate_lifetime_alias_map(template_params.at(i).getTypePtr(), *(alias.m_sublifetimes_vlptr), state1, MR_ptr, Rewrite_ptr);

				retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), res1.m_primary_lifetimes.begin(), res1.m_primary_lifetimes.end());
			}
		}
		return retval;
	};

	auto with_any_lifetime_aliases_dealiased(const CAbstractLifetimeSet& alts1, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};

		for (auto& lifetime : alts1.m_primary_lifetimes) {
			auto found_it = state1.m_lifetime_alias_map.find(lifetime);
			if (state1.m_lifetime_alias_map.end() != found_it) {
				/* This lifetime is actually an alias (for a set of lifetimes). */
				retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), found_it->second.m_primary_lifetimes.begin(), found_it->second.m_primary_lifetimes.end());
			} else {
				retval.m_primary_lifetimes.push_back(lifetime);
			}
		}

		return retval;
	}

	void process_type_lifetime_annotations(const clang::TypeDecl& type_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		auto iter = state1.m_type_lifetime_annotations_map.find(type_decl.getTypeForDecl());
		if (state1.m_type_lifetime_annotations_map.end() != iter) {
			if (iter->second.m_parse_errors_noted) {
				/* already processed */
				return;
			}
		}

		IF_DEBUG(auto type_str = type_decl.getNameAsString();)
#ifndef NDEBUG
		if ("CL" == type_str) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		CTypeLifetimeAnnotations tlta;
		tlta.m_parse_errors_noted = (nullptr != MR_ptr);

		if (type_decl.hasAttrs()) {
			DECLARE_CACHED_CONST_STRING(lifetime_notes_str, "lifetime_notes");
			DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
			DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
			DECLARE_CACHED_CONST_STRING(lifetime_set_aliases_from_template_parameters, "lifetime_set_aliases_from_template_parameters");
			DECLARE_CACHED_CONST_STRING(lifetime_set_alias_from_template_parameter, "lifetime_set_alias_from_template_parameter");

			DECLARE_CACHED_CONST_STRING(mse_lifetime_notes_str, mse_namespace_str() + "::lifetime_notes");
			DECLARE_CACHED_CONST_STRING(mse_lifetime_labels, mse_namespace_str() + "::lifetime_labels");
			DECLARE_CACHED_CONST_STRING(mse_lifetime_label, mse_namespace_str() + "::lifetime_label");

			auto& vec = type_decl.getAttrs();
			for (const auto& attr : vec) {
				auto attr_SR = attr->getRange();
				std::string raw_pretty_str;
				llvm::raw_string_ostream pretty_stream(raw_pretty_str);
				attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
				pretty_stream.flush();
				auto first_quote_index = raw_pretty_str.find('"');
				if (decltype(raw_pretty_str)::npos == first_quote_index) {
					continue;
				}
				auto first_mse_range = Parse::find_uncommented_token(mse_namespace_str(), raw_pretty_str, first_quote_index + 1);
				if (raw_pretty_str.length() <= first_mse_range.begin) {
					continue;
				}
				std::string pretty_str = raw_pretty_str.substr(first_mse_range.begin);
				auto index1 = pretty_str.find(mse_lifetime_notes_str);
				if (decltype(pretty_str)::npos != index1) {
					static const std::string lbrace = "{";
					static const std::string rbrace = "}";
					static const std::string langle_bracket = "<";
					static const std::string rangle_bracket = ">";
					static const std::string semicolon = ";";
					index1 = pretty_str.find(lbrace, index1 + mse_lifetime_notes_str.length());
					if (decltype(pretty_str)::npos == index1) {
						continue;
					}
					auto last_delimiter_index = index1;
					auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					while (decltype(pretty_str)::npos != next_delimiter_index) {
						std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));

						static const std::string encompasses_str = "encompasses";
						auto index2 = sv1.find(encompasses_str);
						if (decltype(sv1)::npos != index2) {

							std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
							std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

							if (true) {
								auto langle_bracket_index = sv1.find('<');
								if (std::string::npos != langle_bracket_index) {
									auto comma_index = sv1.find(',', langle_bracket_index+1);
									if ((std::string::npos != comma_index) && (langle_bracket_index + 1 < comma_index)) {
										auto rangle_bracket_index = sv1.find('>', comma_index+1);
										if ((std::string::npos != rangle_bracket_index) && (comma_index + 1 < rangle_bracket_index)) {
											std::string first_lifetime_label_id_str( sv1.substr(langle_bracket_index + 1, int(comma_index) - int(langle_bracket_index) - 1) );
											auto first_lifetime_label_id = with_whitespace_removed(first_lifetime_label_id_str);
											maybe_first_lifetime_label_id = lifetime_id_t(first_lifetime_label_id);
											if (1 <= first_lifetime_label_id.length()) {
											} else {
												if (MR_ptr) {
													std::string error_desc = std::string("No valid 'lifetime label id' specified.");
													auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
											}

											std::string second_lifetime_label_id_str( sv1.substr(comma_index + 1, int(rangle_bracket_index) - int(comma_index) - 1) );
											auto second_lifetime_label_id = with_whitespace_removed(second_lifetime_label_id_str);
											maybe_second_lifetime_label_id = lifetime_id_t(second_lifetime_label_id);
											if (1 <= second_lifetime_label_id.length()) {
											} else {
												if (MR_ptr) {
													std::string error_desc = std::string("No valid 'lifetime label id' specified.");
													auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
											}
										}
									}
								}
							}

							if (maybe_first_lifetime_label_id.has_value() && maybe_second_lifetime_label_id.has_value()) {
								CAbstractLifetime alt1;
								auto maybe_alt1 = tlta.m_lifetime_set.lifetime_from_label_id_if_present(maybe_first_lifetime_label_id.value());
								if (!maybe_alt1.has_value()) {
									int q = 3;
									alt1 = CAbstractLifetime{ maybe_first_lifetime_label_id.value(), &type_decl };
								} else {
									alt1 = maybe_alt1.value();
#ifndef NDEBUG
#if 0
									auto alt1b = CAbstractLifetime{ maybe_first_lifetime_label_id.value(), &type_decl };
									auto b1 = (alt1b == alt1);
									auto ctxhash1 = impl::general_hash_via_bit_representation(alt1.m_context);
									auto ctxhash2 = impl::general_hash_via_bit_representation(alt1b.m_context);
									auto ctxhash1b = impl::general_hash_via_bit_representation(alt1.m_context);
									auto ctxhash2b = impl::general_hash_via_bit_representation(alt1b.m_context);
									std::hash<decltype(alt1.m_context)> hasher1;
									auto ctxhash1c = hasher1(alt1.m_context.m_context);
									auto ctxhash2c = hasher1(alt1b.m_context.m_context);
									if (!b1) {
										int q = 5;
									}
									std::hash<decltype(alt1)> hasher2;
									auto althash1 = hasher2(alt1);
									auto althash2 = hasher2(alt1b);

									decltype(alt1.m_context) ctx3a((clang::FunctionDecl const *)(nullptr));
									decltype(alt1.m_context) ctx3b((clang::FunctionDecl const *)(nullptr));
									auto ctx3c_uqptr = std::make_unique<decltype(alt1.m_context)>((clang::FunctionDecl const *)(nullptr));
									auto ctx3ahash1 = impl::general_hash_via_bit_representation(ctx3a.m_context);
									auto ctx3bhash1 = impl::general_hash_via_bit_representation(ctx3b.m_context);
									auto ctx3chash1 = impl::general_hash_via_bit_representation((*ctx3c_uqptr).m_context);
									std::hash<decltype(ctx3a.m_context)> hasher3;
									auto ctx3ahash2 = hasher3(ctx3a.m_context);
									auto ctx3bhash2 = hasher3(ctx3b.m_context);
									auto ctx3chash2 = hasher3((*ctx3c_uqptr).m_context);

									int q = 5;
#endif /*0*/
#endif /*!NDEBUG*/
								}
								CAbstractLifetime alt2;
								auto maybe_alt2 = tlta.m_lifetime_set.lifetime_from_label_id_if_present(maybe_second_lifetime_label_id.value());
								if (!maybe_alt2.has_value()) {
									int q = 3;
									alt2 = CAbstractLifetime{ maybe_second_lifetime_label_id.value(), &type_decl };
								} else {
									alt2 = maybe_alt2.value();
								}

								tlta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
									CEncompasses{ alt1, alt2 }));
							} else {
								if (MR_ptr) {
									std::string error_desc = std::string("Parse error in 'encompassing' lifetime constraint specification.");
									auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}

							int q = 5;
						}

						last_delimiter_index = next_delimiter_index;
						next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					}
				} else {
					auto lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string langle_bracket = "<";
						static const std::string rangle_bracket = ">";

						auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= langle_bracket_range.begin) {
							continue;
						}
						auto langle_bracket_index = langle_bracket_range.begin;
						auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
						if (pretty_str.length() <= rangle_bracket_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						if (!(alts1.is_empty())) {
							tlta.m_lifetime_set = alts1;
						} else {
							if (MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					} else {
						auto lsftp_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_set_aliases_from_template_parameters }, pretty_str);
						if (pretty_str.length() <= lsftp_range.begin) {
							lsftp_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_set_alias_from_template_parameter }, pretty_str);
						}
						if (pretty_str.length() > lsftp_range.begin) {
							static const std::string langle_bracket = "<";
							static const std::string rangle_bracket = ">";

							auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lsftp_range.end);
							if (pretty_str.length() <= langle_bracket_range.begin) {
								continue;
							}
							auto langle_bracket_index = langle_bracket_range.begin;
							auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
							if (pretty_str.length() <= rangle_bracket_index) {
								int q = 3;
								continue;
							}
							std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

							CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

							if (alts1.is_empty()) {
								int q = 5;
							} else {
								auto check_for_illegal_label_reuse = [&state1, &attr_SR, &MR_ptr, &Rewrite_ptr](const CAbstractLifetime& alt) {
									{
										auto found_it = state1.m_lifetime_alias_map.find(alt);
										if (state1.m_lifetime_alias_map.end() != found_it) {
											if (MR_ptr) {
												std::string error_desc = std::string("'lifetime set' label '") + alt.m_id + "', specified in 'lifetime_set_aliases_from_template_parameters' annotation,";
												error_desc += " is already being used as a 'lifetime set' label. Please choose a different one.";
												auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
									}
								};
								for (auto& lifetime : alts1.m_primary_lifetimes) {
									apply_to_all_lifetimes_const(lifetime, check_for_illegal_label_reuse);
								}

								auto TypePtr = (MR_ptr && MR_ptr->Context) ? MR_ptr->Context->getTypeDeclType(&type_decl).getTypePtr()
									: type_decl.getTypeForDecl();

								auto lifetimes_from_specified_template_params = populate_lifetime_alias_map(TypePtr, alts1, state1, MR_ptr, Rewrite_ptr);
								tlta.m_lifetime_set.m_primary_lifetimes.insert(tlta.m_lifetime_set.m_primary_lifetimes.end(), lifetimes_from_specified_template_params.m_primary_lifetimes.begin(), lifetimes_from_specified_template_params.m_primary_lifetimes.end());

								if (false && alts1.is_empty()) {
									if (MR_ptr) {
										std::string error_desc = std::string("No valid 'lifetime set aliases' specified in 'lifetime_set_aliases_from_template_parameters' annotation.");
										error_desc += " (A valid use of the 'lifetime_set_aliases_from_template_parameters' annotation might look something like: ";
										error_desc += " 'mse::lifetime_set_aliases_from_template_parameters<51>' or 'mse::lifetime_set_aliases_from_template_parameters<51,52[521,522]>'.)";
										auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								} 
							}
						}
					}
				}
			}
		}

		state1.m_type_lifetime_annotations_map.insert_or_assign(type_decl.getTypeForDecl(), tlta);

		auto type_decl_Type_ptr = type_decl.getTypeForDecl();
		clang::CXXRecordDecl const * CXXRD = type_decl_Type_ptr ? type_decl_Type_ptr->getAsCXXRecordDecl() : (decltype(CXXRD))(nullptr);
		if (CXXRD) {
			for (auto CXXBS : CXXRD->bases()) {
			}
		}

		clang::RecordDecl const * RD = type_decl_Type_ptr ? type_decl_Type_ptr->getAsRecordDecl() : (decltype(RD))(nullptr);
		if (RD) {
			for (auto FD : RD->fields()) {
				if (FD->hasAttrs()) {
					DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
					DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
					DECLARE_CACHED_CONST_STRING(lifetime_set, "lifetime_set");

					DECLARE_CACHED_CONST_STRING(mse_lifetime_labels, mse_namespace_str() + "::lifetime_labels");
					DECLARE_CACHED_CONST_STRING(mse_lifetime_label, mse_namespace_str() + "::lifetime_label");
					DECLARE_CACHED_CONST_STRING(mse_lifetime_set, mse_namespace_str() + "::lifetime_set");

					CAbstractLifetimeSet abstract_lifetime_set;
					auto& vec = FD->getAttrs();
					for (const auto& attr : vec) {
						auto attr_SR = attr->getRange();
						std::string raw_pretty_str;
						llvm::raw_string_ostream pretty_stream(raw_pretty_str);
						attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
						pretty_stream.flush();
						auto first_quote_index = raw_pretty_str.find('"');
						if (decltype(raw_pretty_str)::npos == first_quote_index) {
							continue;
						}
						auto first_mse_range = Parse::find_uncommented_token(mse_namespace_str(), raw_pretty_str, first_quote_index + 1);
						if (raw_pretty_str.length() <= first_mse_range.begin) {
							continue;
						}
						std::string pretty_str = raw_pretty_str.substr(first_mse_range.begin);

						auto lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_labels }, pretty_str);
						if (pretty_str.length() <= lifetime_labels_range.begin) {
							lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_label }, pretty_str);
						}
						if (pretty_str.length() > lifetime_labels_range.begin) {
							static const std::string langle_bracket = "<";
							static const std::string rangle_bracket = ">";

							auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lifetime_labels_range.end);
							if (pretty_str.length() <= langle_bracket_range.begin) {
								continue;
							}
							auto langle_bracket_index = langle_bracket_range.begin;
							auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
							if (pretty_str.length() <= rangle_bracket_index) {
								int q = 3;
								continue;
							}
							std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

							CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

							if (!(alts1.is_empty())) {
								abstract_lifetime_set = alts1;
							} else {
								if (false && MR_ptr) {
									std::string error_desc = std::string("No valid 'lifetime label id' specified.");
									auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						} else {
							auto lifetime_set_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_set }, pretty_str);
							if (pretty_str.length() > lifetime_set_range.begin) {
								static const std::string langle_bracket = "<";
								static const std::string rangle_bracket = ">";

								auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lifetime_set_range.end);
								if (pretty_str.length() <= langle_bracket_range.begin) {
									continue;
								}
								auto langle_bracket_index = langle_bracket_range.begin;
								auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
								if (pretty_str.length() <= rangle_bracket_index) {
									int q = 3;
									continue;
								}
								std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

								CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
								CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

								if (false && (alts2.is_empty())) {
									if (MR_ptr) {
										std::string error_desc = std::string("No valid 'lifetime sets' specified.");
										auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}
						}

					}
					state1.m_fielddecl_to_abstract_lifetime_map.insert_or_assign(FD, abstract_lifetime_set);

					if (FD->hasInClassInitializer()) {
						if (MR_ptr) {
							std::string error_desc = std::string("Currently, member fields with lifetime annotation aren't permitted to have default values (with field of type '") + FD->getType().getAsString() + "').";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), FD->getSourceRange().getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			}
		}

		for (const auto& lifetime_constraint_shptr : tlta.m_lifetime_constraint_shptrs) {
			bool already_noted = false;
			auto range = state1.m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(lifetime_constraint_shptr->m_first);
			for (auto it = range.first; range.second != it; ++it) {
				if ((*(it->second)) == (*lifetime_constraint_shptr)) {
					already_noted = true;
					break;
				}
			}
			if (!already_noted) {
				typedef decltype(state1.m_lhs_to_lifetime_constraint_shptr_mmap)::value_type vt1;
				state1.m_lhs_to_lifetime_constraint_shptr_mmap.insert(vt1{ lifetime_constraint_shptr->m_first, lifetime_constraint_shptr });
				state1.m_rhs_to_lifetime_constraint_shptr_mmap.insert(vt1{ lifetime_constraint_shptr->m_second, lifetime_constraint_shptr });
			}
		}
		int q = 5;
	}
	void process_type_lifetime_annotations(const clang::FieldDecl& field_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		auto record_decl1 = field_decl.getParent();
		if (record_decl1) {
			IF_DEBUG(std::string record_name = record_decl1->getNameAsString();)
			process_type_lifetime_annotations(*record_decl1, state1, MR_ptr, Rewrite_ptr);
		}
	}

	void process_function_lifetime_annotations(const clang::FunctionDecl& func_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		auto iter = state1.m_function_lifetime_annotations_map.find(&func_decl);
		if (state1.m_function_lifetime_annotations_map.end() != iter) {
			if (iter->second.m_parse_errors_noted) {
				/* already processed */
				return;
			}
		}
		std::optional<CAbstractLifetimeSet> maybe_containing_type_alts;
		auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(&func_decl);
		if (CXXMD) {
			auto This_qtype = CXXMD->getThisType();
			MSE_RETURN_IF_TYPE_IS_NULL(This_qtype);
			auto This_pointee_qtype = This_qtype->getPointeeType();
			MSE_RETURN_IF_TYPE_IS_NULL(This_pointee_qtype);
			IF_DEBUG(const std::string qtype_str = This_pointee_qtype.getAsString();)
			auto Type_ptr = This_pointee_qtype.getTypePtr();
			auto containing_RD = Type_ptr->getAsRecordDecl();
			if (containing_RD) {
				process_type_lifetime_annotations(*containing_RD, state1, MR_ptr, Rewrite_ptr);

				auto iter = state1.m_type_lifetime_annotations_map.find(Type_ptr);
				if (state1.m_type_lifetime_annotations_map.end() != iter) {
					if (!(iter->second.m_lifetime_set.is_empty())) {
						maybe_containing_type_alts = iter->second.m_lifetime_set;
					}
				}
			}
		}

		CFunctionLifetimeAnnotations flta;
		flta.m_parse_errors_noted = (nullptr != MR_ptr);

		std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> param_lifetime_map;
		DECLARE_CACHED_CONST_STRING(clang_lifetimebound_str, "clang::lifetimebound");

		if (func_decl.hasAttrs()) {
			DECLARE_CACHED_CONST_STRING(mse_lifetime_notes_str, mse_namespace_str() + "::lifetime_notes");
			clang::AttrVec vec = func_decl.getAttrs();
			for (const auto& attr : vec) {
				auto attr_SR = attr->getRange();
				std::string raw_pretty_str;
				llvm::raw_string_ostream pretty_stream(raw_pretty_str);
				attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
				pretty_stream.flush();
				auto first_quote_index = raw_pretty_str.find('"');
				if (decltype(raw_pretty_str)::npos == first_quote_index) {
					continue;
				}
				auto first_mse_range = Parse::find_uncommented_token(mse_namespace_str(), raw_pretty_str, first_quote_index + 1);
				if (raw_pretty_str.length() <= first_mse_range.begin) {
					continue;
				}
				std::string pretty_str = raw_pretty_str.substr(first_mse_range.begin);

				auto index1 = pretty_str.find(mse_lifetime_notes_str);
				if (decltype(pretty_str)::npos != index1) {
					static const std::string lbrace = "{";
					static const std::string rbrace = "}";
					static const std::string langle_bracket = "<";
					static const std::string rangle_bracket = ">";
					static const std::string semicolon = ";";
					index1 = pretty_str.find(lbrace, index1 + mse_lifetime_notes_str.length());
					if (decltype(pretty_str)::npos == index1) {
						continue;
					}
					auto last_delimiter_index = index1;
					auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					while (decltype(pretty_str)::npos != next_delimiter_index) {
						std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));

						static const std::string return_value_str = "return_value";
						auto index2 = sv1.find(return_value_str);
						if (decltype(sv1)::npos != index2) {
							CAbstractLifetimeSet return_value_lifetimes;

							if (true) {
								const auto langle_bracket_index = sv1.find('<');
								if (std::string::npos != langle_bracket_index) {
									const auto rangle_bracket_index = sv1.find('>', langle_bracket_index+1);
									if ((std::string::npos != rangle_bracket_index) && (langle_bracket_index + 1 < rangle_bracket_index)) {
										auto sv2 = sv1.substr(langle_bracket_index + 1, int(rangle_bracket_index) - (int(langle_bracket_index) + 1));
										return_value_lifetimes = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
									}
								}
							}
							if (!(return_value_lifetimes.is_empty())) {
								flta.m_return_value_lifetimes = return_value_lifetimes;
							}

							int q = 5;
						}

						static const std::string this_str = "this";
						index2 = sv1.find(this_str);
						if (decltype(sv1)::npos != index2) {
							CAbstractLifetimeSet this_lifetimes;

							if (true) {
								const auto langle_bracket_index = sv1.find('<');
								if (std::string::npos != langle_bracket_index) {
									const auto rangle_bracket_index = sv1.find('>', langle_bracket_index+1);
									if ((std::string::npos != rangle_bracket_index) && (langle_bracket_index + 1 < rangle_bracket_index)) {
										auto sv2 = sv1.substr(langle_bracket_index + 1, int(rangle_bracket_index) - (int(langle_bracket_index) + 1));
										this_lifetimes = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
									}
								}
							}
							if (!(this_lifetimes.is_empty())) {
								param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, this_lifetimes);
							}

							int q = 5;
						}

						static const std::string encompasses_str = "encompasses";
						index2 = sv1.find(encompasses_str);
						if (decltype(sv1)::npos != index2) {
							CAbstractLifetimeSet first_lifetimes;
							CAbstractLifetimeSet second_lifetimes;

							std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
							std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

							if (true) {
								auto langle_bracket_index = sv1.find('<');
								if (std::string::npos != langle_bracket_index) {
									auto comma_index = Parse::find_token_at_same_nesting_depth1(",", sv1, langle_bracket_index+1).begin;
									if ((sv1.length() > comma_index) && (langle_bracket_index + 1 < comma_index)) {
										auto rangle_bracket_index = sv1.find('>', comma_index+1);
										if ((std::string::npos != rangle_bracket_index) && (comma_index + 1 < rangle_bracket_index)) {
											{
												auto sv2 = sv1.substr(langle_bracket_index + 1, int(comma_index) - (int(langle_bracket_index) + 1));
												first_lifetimes = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
												if (MR_ptr) {
													if (first_lifetimes.is_empty()) {
														std::string error_desc = std::string("First lifetime argument not found in 'encompasses' constraint.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													} else if (1 < first_lifetimes.m_primary_lifetimes.size()) {
														std::string error_desc = std::string("More than one lifetime given in the first lifetime argument of 'encompasses' constraint.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
											}
											{
												auto sv2 = sv1.substr(comma_index + 1, int(rangle_bracket_index) - (int(comma_index) + 1));
												second_lifetimes = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
												if (MR_ptr) {
													if (second_lifetimes.is_empty()) {
														std::string error_desc = std::string("Second lifetime argument not found in 'encompasses' constraint.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													} else if (1 < second_lifetimes.m_primary_lifetimes.size()) {
														std::string error_desc = std::string("More than one lifetime given in the second lifetime argument of 'encompasses' constraint.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
											}
										}
									}
								}
							}

							if ((1 == first_lifetimes.m_primary_lifetimes.size()) && (1 == second_lifetimes.m_primary_lifetimes.size())) {
								auto first_lifetime = first_lifetimes.m_primary_lifetimes.front();
								auto second_lifetime = second_lifetimes.m_primary_lifetimes.front();
								flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
									CEncompasses{ first_lifetime, second_lifetime }));
							} else {
								if (MR_ptr) {
									std::string error_desc = std::string("Parse error in 'encompasses' lifetime constraint specification.");
									auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}

							int q = 5;
						}

						last_delimiter_index = next_delimiter_index;
						next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					}
				} else {
					auto index2 = pretty_str.find(clang_lifetimebound_str);
					if (false && (decltype(pretty_str)::npos != index2)) {
						/* A [[clang::lifetimebound]] attribute on the function will be interpreted as an
						MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<1>; this<1> }") attribute. */
						flta.m_return_value_lifetimes = CAbstractLifetime{ lifetime_id_t("1"), &func_decl };
						param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, CAbstractLifetime{ lifetime_id_t("1"), &func_decl });
					} else {
						continue;
					}
				}
			}
		}

		param_ordinal_t param_ordinal = 1;
		for (auto param_iter = func_decl.param_begin(); func_decl.param_end() != param_iter; ++param_iter, param_ordinal += 1) {
			auto param = (*param_iter);
			IF_DEBUG(const std::string param_qtype_str = param->getType().getAsString();)

			if (param->hasAttrs()) {
				DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
				DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
				DECLARE_CACHED_CONST_STRING(lifetime_set, "lifetime_set");

				DECLARE_CACHED_CONST_STRING(mse_lifetime_labels, mse_namespace_str() + "::lifetime_labels");
				DECLARE_CACHED_CONST_STRING(mse_lifetime_label, mse_namespace_str() + "::lifetime_label");
				DECLARE_CACHED_CONST_STRING(mse_lifetime_set, mse_namespace_str() + "::lifetime_set");

				clang::AttrVec vec = param->getAttrs();
				for (const auto& attr : vec) {
					auto attr_SR = attr->getRange();
					std::string raw_pretty_str;
					llvm::raw_string_ostream pretty_stream(raw_pretty_str);
					attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
					pretty_stream.flush();
					auto first_quote_index = raw_pretty_str.find('"');
					if (decltype(raw_pretty_str)::npos == first_quote_index) {
						continue;
					}
					auto first_mse_range = Parse::find_uncommented_token(mse_namespace_str(), raw_pretty_str, first_quote_index + 1);
					if (raw_pretty_str.length() <= first_mse_range.begin) {
						continue;
					}
					std::string pretty_str = raw_pretty_str.substr(first_mse_range.begin);

					auto lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string langle_bracket = "<";
						static const std::string rangle_bracket = ">";

						auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= langle_bracket_range.begin) {
							continue;
						}
						auto langle_bracket_index = langle_bracket_range.begin;
						auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
						if (pretty_str.length() <= rangle_bracket_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if (!(alts1.is_empty())) {
							size_t num_declared_primary_lifetimes = 0;

							auto found_iter2 = state1.m_type_lifetime_annotations_map.find(param->getType().getTypePtr());
							if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
								auto& tlta = found_iter2->second;
								num_declared_primary_lifetimes = tlta.m_lifetime_set.m_primary_lifetimes.size();
							}

							auto found_iter = param_lifetime_map.find(param_ordinal);
							if (param_lifetime_map.end() != found_iter) {

								/* Under what circumstances would a paramter already have associated lifetimes? */

								auto& found_alts1 = found_iter->second;
								found_alts1 = alts1;
								if (num_declared_primary_lifetimes < found_alts1.m_primary_lifetimes.size()) {
									/* The number of given primary lifetimes may be larger then the number of declared
									primary lifetimes for this type. */
									int q = 3;
								}
							} else {
								param_lifetime_map.insert_or_assign(param_ordinal, alts1);
							}
						} else {
							if (false && MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					} else {
						auto lifetime_set_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_set }, pretty_str);
						if (pretty_str.length() > lifetime_set_range.begin) {
							static const std::string langle_bracket = "<";
							static const std::string rangle_bracket = ">";

							auto langle_bracket_range = Parse::find_token_at_same_nesting_depth1(langle_bracket, pretty_str, lifetime_set_range.end);
							if (pretty_str.length() <= langle_bracket_range.begin) {
								continue;
							}
							auto langle_bracket_index = langle_bracket_range.begin;
							auto rangle_bracket_index = Parse::find_matching_right_angle_bracket(pretty_str, langle_bracket_range.end);
							if (pretty_str.length() <= rangle_bracket_index) {
								int q = 3;
								continue;
							}
							std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

							CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

							if (!(alts1.is_empty())) {
								size_t num_declared_primary_lifetimes = 0;

								auto found_iter2 = state1.m_type_lifetime_annotations_map.find(param->getType().getTypePtr());
								if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
									auto& tlta = found_iter2->second;
									num_declared_primary_lifetimes = tlta.m_lifetime_set.m_primary_lifetimes.size();
								}

								auto found_iter = param_lifetime_map.find(param_ordinal);
								if (param_lifetime_map.end() != found_iter) {

									/* Under what circumstances would a paramter already have associated lifetimes? */

									auto& found_alts1 = found_iter->second;
									found_alts1 = alts1;
									if (num_declared_primary_lifetimes < found_alts1.m_primary_lifetimes.size()) {
										/* The number of given primary lifetimes may be larger then the number of declared
										primary lifetimes for this type. */
										int q = 3;
									}
								} else {
									param_lifetime_map.insert_or_assign(param_ordinal, alts1);
								}
							} else {
								if (false && MR_ptr) {
									std::string error_desc = std::string("No valid 'lifetime label id' specified.");
									auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						} else {
							auto index2 = pretty_str.find(clang_lifetimebound_str);
							if (false && (decltype(pretty_str)::npos != index2)) {
								/* A [[clang::lifetimebound]] attribute on the parameter will be interpreted as an
								MSE_ATTR_PARAM_STR("mse::lifetime_label<1>") attribute on the parameter and an
								MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<1> }") attribute on the function. */
								flta.m_return_value_lifetimes = CAbstractLifetime{ lifetime_id_t("1"), &func_decl };
								param_lifetime_map.insert_or_assign(param_ordinal, CAbstractLifetime{ lifetime_id_t("1"), &func_decl });
							} else {
								continue;
							}
						}
					}
				}
			}

			if (!(param->getType().isNull())) {
				/* This section is to support the old lifetime annotation format that embeds the lifetime constraints
				in (the type of) an extra "unused" parameter. */
				auto CXXRD = param->getType()->getAsCXXRecordDecl();
				if (CXXRD) {
					auto name = CXXRD->getQualifiedNameAsString();
					const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
					if (tmplt_CXXRD) {
						name = tmplt_CXXRD->getQualifiedNameAsString();
					}
					DECLARE_CACHED_CONST_STRING(mse_rsv_ltn_lifetime_notes_str, mse_namespace_str() + "::rsv::ltn::lifetime_notes");
					if (mse_rsv_ltn_lifetime_notes_str == name) {
						auto SR = Rewrite_ptr ? nice_source_range(param->getSourceRange(), *Rewrite_ptr)
							: param->getSourceRange();

						auto process_lifetime_note = [&flta, &param, &func_decl, &MR_ptr, &Rewrite_ptr, &maybe_containing_type_alts](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
							DECLARE_CACHED_CONST_STRING(mse_rsv_ltn_parameter_lifetime_labels_str, mse_namespace_str() + "::rsv::ltn::parameter_lifetime_labels");
							DECLARE_CACHED_CONST_STRING(mse_rsv_ltn_pll_str, mse_namespace_str() + "::rsv::ltn::pll");
							DECLARE_CACHED_CONST_STRING(mse_rsv_ltn_return_value_lifetime_str, mse_namespace_str() + "::rsv::ltn::return_value_lifetime");
							DECLARE_CACHED_CONST_STRING(mse_rsv_ltn_encompasses_lifetime_str, mse_namespace_str() + "::rsv::ltn::encompasses");

							auto qtype = typeLoc.getType();
							MSE_RETURN_IF_TYPE_IS_NULL(qtype);
							std::string element_name;
							const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
							if (l_CXXRD) {
								element_name = l_CXXRD->getQualifiedNameAsString();
							} else {
								element_name = qtype.getAsString();
							}

							if (mse_rsv_ltn_parameter_lifetime_labels_str == element_name) {
								auto SR = Rewrite_ptr ? nice_source_range(param->getSourceRange(), *Rewrite_ptr)
									: param->getSourceRange();

								std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> param_lifetime_map;

								auto process_parameter_lifetime = [&mse_rsv_ltn_pll_str, &param_lifetime_map, &func_decl, &MR_ptr](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
									auto qtype = typeLoc.getType();
									MSE_RETURN_IF_TYPE_IS_NULL(qtype);
									std::string element_name;
									const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
									if (l_CXXRD) {
										element_name = l_CXXRD->getQualifiedNameAsString();
									} else {
										element_name = qtype.getAsString();
									}
									if (mse_rsv_ltn_pll_str == element_name) {
										std::optional<param_ordinal_t> maybe_param_ordinal;
										std::optional<lifetime_id_t> maybe_lifetime_label_id;

										if (true) {
											/* Here we're parsing the non-type (size_t) template parameters of 'mse::rsv::pll<>'
											from the string representation of the type (of the instantiated template). This is
											kind of a hacky way to do it, but when we try to obtain the non-type template
											arguments programmatically, we seem to get a (not useful?) clang::TemplateArgument::Expression
											instead of a desired (and more specific and more useful) clang::TemplateArgument::Integral. */
											std::string qtype_str = qtype.getAsString();
											auto langle_bracket_index = qtype_str.find('<');
											if (std::string::npos != langle_bracket_index) {
												auto comma_index = qtype_str.find(',', langle_bracket_index+1);
												if ((std::string::npos != comma_index) && (langle_bracket_index + 1 < comma_index)) {
													auto rangle_bracket_index = qtype_str.find('>', comma_index+1);
													if ((std::string::npos != rangle_bracket_index) && (comma_index + 1 < rangle_bracket_index)) {
														std::string param_ordinal_str = qtype_str.substr(langle_bracket_index + 1, int(comma_index) - int(langle_bracket_index) - 1);
														auto param_ordinal = atoi(param_ordinal_str.c_str());
														if (int(func_decl.getNumParams()) < param_ordinal) {
															if (MR_ptr) {
																std::string error_desc = std::string("The specified parameter ordinal ('") + std::to_string(param_ordinal)
																	+ "') is greater than the number of parameters (" + std::to_string(func_decl.getNumParams()) + ").";
																auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
																if (res.second) {
																	std::cout << (*(res.first)).as_a_string1() << " \n\n";
																}
															}
														} else if ((1 <= param_ordinal) || (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal)) {
															maybe_param_ordinal = param_ordinal_t(param_ordinal);
														} else {
															if (MR_ptr) {
																std::string error_desc = std::string("'") + param_ordinal_str + "' is not a valid parameter ordinal value.";
																auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
																if (res.second) {
																	std::cout << (*(res.first)).as_a_string1() << " \n\n";
																}
															}
														}

														std::optional<CAbstractLifetime> maybe_return_value_lifetime;
														std::string lifetime_label_id_str = qtype_str.substr(comma_index + 1, int(rangle_bracket_index) - int(comma_index) - 1);
														auto lifetime_label_id = with_whitespace_removed(lifetime_label_id_str);
														if (1 <= lifetime_label_id.length()) {
															maybe_lifetime_label_id = lifetime_id_t(lifetime_label_id);
														}
													}
												}
											}
										} else {
											size_t component_index = 0;
											auto process_parameter_lifetime_component = [&maybe_param_ordinal, &maybe_lifetime_label_id, &component_index, &MR_ptr](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
												auto qtype = typeLoc.getType();
												MSE_RETURN_IF_TYPE_IS_NULL(qtype);
												std::string element_name;
												const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
												if (l_CXXRD) {
													element_name = l_CXXRD->getQualifiedNameAsString();
												} else {
													element_name = qtype.getAsString();
												}

												++component_index;

												int q = 5;
											};

											/* We expected the "component types", i.e. non-type (size_t) template arguments,
											in this case to be 'clang::TemplateArgument::Integral's. But instead they seem
											to be reported as the (less specific) 'clang::TemplateArgument::Expression's, 
											which aren't obviously useful (enough) for us. */
											apply_to_component_types_if_any(typeLoc, process_parameter_lifetime_component, state1);
										}

										bool is_valid = true;
										if ((!maybe_param_ordinal.has_value()) || ((1 > maybe_param_ordinal.value()) && (IMPLICIT_THIS_PARAM_ORDINAL != maybe_param_ordinal.value()))) {
											is_valid = false;
											if (MR_ptr) {
												std::string error_desc = std::string("The first template argument of the 'mse::rsv::pll<parameter_ordinal_t param_ordinal, lifetime_label_t lifetime_label_id>' ")
													+ "template must be an integer greater than zero or MSE_IMPLICIT_THIS_PARAM_ORDINAL.";
												auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
										if ((!maybe_lifetime_label_id.has_value()) || (1 > maybe_lifetime_label_id.value().length())) {
											is_valid = false;
											if (MR_ptr) {
												std::string error_desc = std::string("The second template argument of the 'mse::rsv::pll<size_t param_ordinal, std::string lifetime_label_id>' ")
													+ "template must be a valid non-empty string.";
												auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
										if (is_valid) {
											param_lifetime_map.insert_or_assign(maybe_param_ordinal.value(), CAbstractLifetime{ maybe_lifetime_label_id.value(), &func_decl });
										}
									} else {
										/* We seem to have encounterred a template argument that is not an mse::rsv::pll<>. We
										should probably complain. */
										int q = 5;
									}
									int q = 5;
								};
								auto tsi_ptr = param->getTypeSourceInfo();
								if (tsi_ptr) {
									//process_parameter_lifetime(tsi_ptr->getTypeLoc(), SR, state1);
									apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), process_parameter_lifetime, state1);
								}

								flta.m_param_lifetime_map = param_lifetime_map;

							} else if (mse_rsv_ltn_return_value_lifetime_str == element_name) {
								auto SR = Rewrite_ptr ? nice_source_range(param->getSourceRange(), *Rewrite_ptr)
									: param->getSourceRange();

								std::optional<CAbstractLifetime> maybe_return_value_lifetime;

								if (true) {
									std::string qtype_str = qtype.getAsString();
									const auto langle_bracket_index = qtype_str.find('<');
									if (std::string::npos != langle_bracket_index) {
										const auto rangle_bracket_index = qtype_str.find('>', langle_bracket_index+1);
										if ((std::string::npos != rangle_bracket_index) && (langle_bracket_index + 1 < rangle_bracket_index)) {
											auto last_delimiter_index = langle_bracket_index;
											auto next_delimiter_index = qtype_str.find(',', last_delimiter_index+1);
											if (std::string::npos == next_delimiter_index) {
												next_delimiter_index = rangle_bracket_index;
											}
											while (true) {
												if (!(last_delimiter_index + 1 < next_delimiter_index)) {
													if (MR_ptr) {
														std::string error_desc = std::string("Parse error in return value lifetime specification.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
													break;
												}
												std::string lifetime_label_id_str = qtype_str.substr(last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index) - 1);
												auto lifetime_label_id = with_whitespace_removed(lifetime_label_id_str);

												if (1 <= lifetime_label_id.length()) {
													maybe_return_value_lifetime = new_or_existing_lifetime_from_label_id(lifetime_id_t(lifetime_label_id), &func_decl, maybe_containing_type_alts);
												} else {
													if (MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}

												if (rangle_bracket_index == next_delimiter_index) {
													break;
												}
												last_delimiter_index = next_delimiter_index;
												next_delimiter_index = qtype_str.find(',', last_delimiter_index+1);
												if (std::string::npos == next_delimiter_index) {
													next_delimiter_index = rangle_bracket_index;
												}
											}
										}
									}
								}
								if (maybe_return_value_lifetime.has_value()) {
									flta.m_return_value_lifetimes = maybe_return_value_lifetime.value();
								}

								int q = 5;

							} else if (mse_rsv_ltn_encompasses_lifetime_str == element_name) {
								auto SR = Rewrite_ptr ? nice_source_range(param->getSourceRange(), *Rewrite_ptr)
									: param->getSourceRange();

								std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
								std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

								if (true) {
									/* Here we're parsing the non-type (size_t) template parameters of 'mse::rsv::encompasses<>'
									from the string representation of the type (of the instantiated template). This is
									kind of a hacky way to do it, but when we try to obtain the non-type template
									arguments programmatically, we seem to get a (not useful?) clang::TemplateArgument::Expression
									instead of a desired (and more specific and more useful) clang::TemplateArgument::Integral. */
									std::string qtype_str = qtype.getAsString();
									auto langle_bracket_index = qtype_str.find('<');
									if (std::string::npos != langle_bracket_index) {
										auto comma_index = qtype_str.find(',', langle_bracket_index+1);
										if ((std::string::npos != comma_index) && (langle_bracket_index + 1 < comma_index)) {
											auto rangle_bracket_index = qtype_str.find('>', comma_index+1);
											if ((std::string::npos != rangle_bracket_index) && (comma_index + 1 < rangle_bracket_index)) {
												std::string first_lifetime_label_id_str = qtype_str.substr(langle_bracket_index + 1, int(comma_index) - int(langle_bracket_index) - 1);
												auto first_lifetime_label_id = with_whitespace_removed(first_lifetime_label_id_str);
												maybe_first_lifetime_label_id = lifetime_id_t(first_lifetime_label_id);
												if (1 <= first_lifetime_label_id.length()) {
												} else {
													if (MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}

												std::string second_lifetime_label_id_str = qtype_str.substr(comma_index + 1, int(rangle_bracket_index) - int(comma_index) - 1);
												auto second_lifetime_label_id = with_whitespace_removed(second_lifetime_label_id_str);
												maybe_second_lifetime_label_id = lifetime_id_t(second_lifetime_label_id);
												if (1 <= second_lifetime_label_id.length()) {
												} else {
													if (MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
											}
										}
									}
								} else {
									size_t component_index = 0;
									auto process_parameter_lifetime_component = [&maybe_first_lifetime_label_id, &maybe_second_lifetime_label_id, &component_index, &MR_ptr](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
										auto qtype = typeLoc.getType();
										MSE_RETURN_IF_TYPE_IS_NULL(qtype);
										std::string element_name;
										const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
										if (l_CXXRD) {
											element_name = l_CXXRD->getQualifiedNameAsString();
										} else {
											element_name = qtype.getAsString();
										}

										++component_index;

										int q = 5;
									};

									auto tsi_ptr = param->getTypeSourceInfo();
									if (tsi_ptr) {
										/* We expected the "component types", i.e. non-type (size_t) template arguments,
										in this case to be 'clang::TemplateArgument::Integral's. But instead they seem
										to be reported as the (less specific) 'clang::TemplateArgument::Expression's, 
										which aren't obviously useful (enough) for us. */
										apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), process_parameter_lifetime_component, state1);
									}
								}

								if (maybe_first_lifetime_label_id.has_value() && maybe_second_lifetime_label_id.has_value()) {
									flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
										CEncompasses{ CAbstractLifetime{ maybe_first_lifetime_label_id.value(), &func_decl }
											, CAbstractLifetime{ maybe_second_lifetime_label_id.value(), &func_decl } }));
								} else {
									if (MR_ptr) {
										std::string error_desc = std::string("Parse error in 'encompassing' lifetime constraint specification.");
										auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}

								int q = 5;
							}
							int q = 5;
						};
						auto tsi_ptr = param->getTypeSourceInfo();
						if (tsi_ptr) {
							//process_parameter_lifetime(tsi_ptr->getTypeLoc(), SR, state1);
							apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), process_lifetime_note, state1);
						}
					}
				}
			}
		}
		if (1 <= param_lifetime_map.size()) {
			flta.m_param_lifetime_map = param_lifetime_map;
		}

		if (CXXMD) {
			IF_DEBUG(const std::string qtype_str = CXXMD->getThisType()->getPointeeType().getAsString();)
			auto Type_ptr = CXXMD->getThisType()->getPointeeType().getTypePtr();
			auto containing_RD = Type_ptr->getAsRecordDecl();
			if (containing_RD) {
				auto pl_iter = flta.m_param_lifetime_map.find(IMPLICIT_THIS_PARAM_ORDINAL);
				if (flta.m_param_lifetime_map.end() != pl_iter) {
					if (1 == pl_iter->second.m_primary_lifetimes.size()) {
						/* This is a member function whose implicit `this` parameter has an "abstract" lifetime. */
						auto& lifetime_of_this = pl_iter->second.m_primary_lifetimes.front();

						for (auto FD : containing_RD->fields()) {
							auto iter1 = state1.m_fielddecl_to_abstract_lifetime_map.find(FD);
							if (state1.m_fielddecl_to_abstract_lifetime_map.end() != iter1) {
								for (auto& field_primary_lifetime : iter1->second.m_primary_lifetimes) {
									/* Here we (automatically) add the (implicit) constraint that the "abstract" "primary" lifetime
									of the member field must outlive the "abstract" lifetime of the the implicit `this`. */

									flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
										CEncompasses{ field_primary_lifetime, lifetime_of_this }));
								}
							}
						}
					}
				}
			}
		}
		if (maybe_containing_type_alts.has_value()) {
			auto& containing_type_alts = maybe_containing_type_alts.value();
		}


		state1.m_function_lifetime_annotations_map.insert_or_assign(&func_decl, flta);

		for (const auto& param_lifetime : flta.m_param_lifetime_map) {
			auto param_iter = func_decl.param_begin();
			auto param_ordinal = param_lifetime.first;
			if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
				int q = 5;
			} else if (func_decl.param_size() < param_ordinal) {
				assert(false);
				continue;
			} else if (1 <= param_ordinal) {
				param_iter += (param_ordinal - 1);
				auto PVD = *param_iter;
				CVariableLifetimeAnnotations vlta;
				vlta.m_lifetime_set = param_lifetime.second;
				vlta.m_parse_errors_noted = flta.m_parse_errors_noted;
				state1.m_vardecl_lifetime_annotations_map.insert_or_assign(PVD, vlta);

				if (PVD->hasInit()) {
					if (MR_ptr) {
						std::string error_desc = std::string("Currently, parameters with lifetime annotation aren't permitted to have default values (with parameter of type '") + PVD->getType().getAsString() + "').";
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), PVD->getSourceRange().getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
			}
		}

		for (const auto& lifetime_constraint_shptr : flta.m_lifetime_constraint_shptrs) {
			bool already_noted = false;
			auto range = state1.m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(lifetime_constraint_shptr->m_first);
			for (auto it = range.first; range.second != it; ++it) {
				if ((*(it->second)) == (*lifetime_constraint_shptr)) {
					already_noted = true;
					break;
				}
			}
			if (!already_noted) {
				typedef decltype(state1.m_lhs_to_lifetime_constraint_shptr_mmap)::value_type vt1;
				state1.m_lhs_to_lifetime_constraint_shptr_mmap.insert(vt1{ lifetime_constraint_shptr->m_first, lifetime_constraint_shptr });
				state1.m_rhs_to_lifetime_constraint_shptr_mmap.insert(vt1{ lifetime_constraint_shptr->m_second, lifetime_constraint_shptr });
			}
		}
		int q = 5;
	}
	void process_function_lifetime_annotations(const clang::ParmVarDecl& parm_var_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		const clang::FunctionDecl* function_decl1 = function_from_param(&parm_var_decl);
		if (function_decl1) {
			IF_DEBUG(std::string function_name = function_decl1->getNameAsString();)
			process_function_lifetime_annotations(*function_decl1, state1, MR_ptr, Rewrite_ptr);
		}
	}
	void process_function_lifetime_annotations(const clang::VarDecl& var_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		const clang::FunctionDecl* function_decl1 = enclosing_function_if_any(&var_decl);
		if (function_decl1) {
			IF_DEBUG(std::string function_name = function_decl1->getNameAsString();)
			process_function_lifetime_annotations(*function_decl1, state1, MR_ptr, Rewrite_ptr);
		}
	}

	void process_variable_lifetime_annotations(const clang::VarDecl& var_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		auto iter = state1.m_vardecl_lifetime_annotations_map.find(&var_decl);
		if (state1.m_vardecl_lifetime_annotations_map.end() != iter) {
			if (iter->second.m_parse_errors_noted) {
				/* already processed */
				return;
			}
		}

		auto FND = enclosing_function_if_any(&var_decl);
		if (!FND) {
			return;
		}
		const clang::FunctionDecl& func_decl = *FND;

		process_function_lifetime_annotations(func_decl, state1, MR_ptr, Rewrite_ptr);

		{
			auto param = &var_decl;
			IF_DEBUG(const std::string param_qtype_str = param->getType().getAsString();)

			if (param->hasAttrs()) {
				DECLARE_CACHED_CONST_STRING(mse_lifetime_label, mse_namespace_str() + "::lifetime_label");
				DECLARE_CACHED_CONST_STRING(mse_lifetime_labels, mse_namespace_str() + "::lifetime_labels");
				clang::AttrVec vec = param->getAttrs();
				for (const auto& attr : vec) {
					auto attr_SR = attr->getRange();
					std::string pretty_str;
					llvm::raw_string_ostream pretty_stream(pretty_str);
					attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
					pretty_stream.flush();
					auto index1 = pretty_str.find(mse_lifetime_label);
					if (decltype(pretty_str)::npos != index1) {
						static const std::string langle_bracket = "<";
						static const std::string rangle_bracket = ">";

						index1 = pretty_str.find(langle_bracket, index1 + mse_lifetime_label.length());
						if (decltype(pretty_str)::npos == index1) {
							continue;
						}
						auto langle_bracket_index = index1;
						auto rangle_bracket_index = pretty_str.find(rangle_bracket, langle_bracket_index + 1);
						std::string_view sv1(pretty_str.data() + langle_bracket_index + 1, int(rangle_bracket_index) - int(langle_bracket_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						if (true /*!(alts1.is_empty())*/) {
							int num_declared_primary_lifetimes = 0;

							if (!(param->getType().isNull())) {
								auto found_iter2 = state1.m_type_lifetime_annotations_map.find(param->getType().getTypePtr());
								if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
									auto& tlta = found_iter2->second;
									num_declared_primary_lifetimes = tlta.m_lifetime_set.m_primary_lifetimes.size();
								}
							}

							CVariableLifetimeAnnotations vlta;
							vlta.m_lifetime_set = alts1;
							vlta.m_parse_errors_noted = (nullptr != MR_ptr);
							state1.m_vardecl_lifetime_annotations_map.insert_or_assign(&var_decl, vlta);
						} else {
							if (MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), attr_SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					} else {
						continue;
					}
				}
			}
		}
		int q = 5;
	}



	class MCSSSRecordDecl2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSRecordDecl2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const RecordDecl* RD)
		{
			if ((RD != nullptr))
			{
				auto SR = nice_source_range(RD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(RD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(RDISR);
				if (suppress_check_flag) {
					return;
				}

#ifndef NDEBUG
				auto qualified_name = RD->getQualifiedNameAsString();
				DECLARE_CACHED_CONST_STRING(mse_namespace_str1, mse_namespace_str() + "::");
				if (string_begins_with(qualified_name, mse_namespace_str1)) {
					int q = 5;
					//return;
				}
#endif /*!NDEBUG*/

				IF_DEBUG(const auto record_name = RD->getName();)
				if (!(MR.Context)) {
					int q = 3;
					return;
				}
				auto qtype = MR.Context->getTypeDeclType(RD);
				auto Type_ptr = qtype.getTypePtr();
				//auto Type_ptr = RD->getTypeForDecl();
				if (Type_ptr) {
					auto CXXRD = Type_ptr->getAsCXXRecordDecl();
					if (RD->isThisDeclarationADefinition()) {
						bool is_lambda = false;

						bool has_xscope_tag_base = false;
						bool has_ContainsNonOwningScopeReference_tag_base = false;
						bool has_ReferenceableByScopePointer_tag_base = false;

						if (CXXRD) {
							if (CXXRD->isLambda()) {
								is_lambda = true;

								auto& context = *MR.Context;
								const auto* LE = Tget_immediately_containing_element_of_type<clang::LambdaExpr>(CXXRD, *MR.Context);
								if (LE) {
									auto* MTE = Tget_immediately_containing_element_of_type<clang::MaterializeTemporaryExpr>(LE, *MR.Context);
									if (!MTE) {
										const clang::ImplicitCastExpr* ICE2 = Tget_immediately_containing_element_of_type<clang::ImplicitCastExpr>(LE, *MR.Context);
										const clang::ImplicitCastExpr* ICE1 = ICE2;
										do {
											ICE1 = ICE2;
											ICE2 = Tget_immediately_containing_element_of_type<clang::ImplicitCastExpr>(ICE1, *MR.Context);
										} while (ICE2);
										MTE = Tget_immediately_containing_element_of_type<clang::MaterializeTemporaryExpr>(ICE1, *MR.Context);
									}
									if (MTE) {
										const auto* CE = Tget_immediately_containing_element_of_type<clang::CallExpr>(
											MTE->IgnoreImpCasts(), *MR.Context);
										if (CE) {
											const auto qname = CE->getDirectCallee()->getQualifiedNameAsString();
											DECLARE_CACHED_CONST_STRING(mse_rsv_make_xscope_reference_or_pointer_capture_lambda_str, mse_namespace_str() + "::rsv::make_xscope_reference_or_pointer_capture_lambda");
											DECLARE_CACHED_CONST_STRING(mse_rsv_make_xscope_non_reference_or_pointer_capture_lambda_str, mse_namespace_str() + "::rsv::make_xscope_non_reference_or_pointer_capture_lambda");
											DECLARE_CACHED_CONST_STRING(mse_rsv_make_xscope_capture_lambda_str, mse_namespace_str() + "::rsv::make_xscope_capture_lambda");
											if ((mse_rsv_make_xscope_reference_or_pointer_capture_lambda_str == qname)
												|| (mse_rsv_make_xscope_non_reference_or_pointer_capture_lambda_str == qname)
												|| (mse_rsv_make_xscope_capture_lambda_str == qname)) {
												/* This CXXRecordDecl is a lambda expression being supplied as an argument
												to an 'mse::rsv::make_xscope_*_capture_lambda()' function. Being a lambda, it
												cannot inherit from 'mse::us::impl::XScopeTagBase' (or anything else for that
												matter), but here we'll treat it as if it satisfies that (potential)
												requirement as it should be safe (and is kind of necessary) here. */
												has_xscope_tag_base = true;

												/* The safety of the following is premised on the assumption that captured lambda
												variables(/fields) are not addressable (by scope pointer) from outside the lambda. */
												has_ReferenceableByScopePointer_tag_base = true;

												if (mse_rsv_make_xscope_reference_or_pointer_capture_lambda_str == qname) {
													has_ContainsNonOwningScopeReference_tag_base = true;
												}
											}
										}
									}
								} else {
									int q = 5; /* unexpected*/
								}
							} else {
								std::vector<const FieldDecl*> unverified_pointer_fields;
								for (const auto FD : RD->fields()) {
									const auto field_qtype = FD->getType();
									IF_DEBUG(auto field_qtype_str = field_qtype.getAsString();)
									if (!(field_qtype.isNull())) {
										const auto ICIEX = FD->getInClassInitializer();
										if (is_raw_pointer_or_equivalent(field_qtype)
											&& (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype))
											&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
											) {
											if (!ICIEX) {
												unverified_pointer_fields.push_back(FD);
											} else if (is_nullptr_literal(ICIEX, *(MR.Context))) {
												auto ICISR = nice_source_range(ICIEX->getSourceRange(), Rewrite);
												if (!ICISR.isValid()) {
													ICISR = SR;
												}
												const std::string error_desc = std::string("Null initialization of ")
													+ "native pointer fields (such as '" + FD->getNameAsString()
													+ "') is not supported.";
												auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, ICISR.getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
										if (ICIEX) {

											const auto EX = ICIEX;
											if (EX) {
												auto CXXThisExpr_range = get_contained_elements_of_type_CXXThisExpr(*EX);
												for (const auto CXXTE : CXXThisExpr_range) {
													assert(CXXTE);
													const auto ME = get_immediately_containing_MemberExpr_from_CXXThisExpr_if_any(*CXXTE, *(MR.Context));
													if (ME) {
														const auto FD2 = get_FieldDecl_from_MemberExpr_if_any(*ME);
														if (FD2) {
															bool res = first_is_contained_in_scope_of_second(FD, FD2, *(MR.Context));
															if ((!res) || (FD == FD2)) {
																auto MESR = nice_source_range(ME->getSourceRange(), Rewrite);
																if (!MESR.isValid()) {
																	MESR = SR;
																}

																const std::string error_desc = std::string("The FD '") + FD2->getNameAsString()
																	+ "' may be being referenced before it has been constructed.";
																auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, MESR.getBegin(), error_desc));
																if (res.second) {
																	std::cout << (*(res.first)).as_a_string1() << " \n\n";
																}
															}
														} else {
															const auto CXXMD = dyn_cast<const CXXMethodDecl>(ME->getMemberDecl());
															if (CXXMD) {
																/* error: Unable to verify that the member function used here can't access part of
																the object that hasn't been constructed yet. */
																const std::string error_desc = std::string("Calling non-static member functions ")
																+ "(such as '" + CXXMD->getQualifiedNameAsString() + "') of an object is not supported in "
																+ "constructor initializers or direct FD initializers of the object. Consider "
																+ "using a static member or free function instead. ";
																auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
																if (res.second) {
																	std::cout << (*(res.first)).as_a_string1() << " \n\n";
																}
															} else {
																/* Since this MemberExpr was obtained from a CXXThisExpr, if it doesn't refer to
																a FD, then presumably it refers to a (non-static) member function.
																So arriving here would be unexpected. */
																int q = 5;
															}
														}
													} else {
														const std::string error_desc = std::string("Unable to verify that the 'this' pointer ")
														+ "used here can't be used to access part of the object that hasn't been constructed yet.";
														auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
												if (field_qtype->isPointerType()
													&& (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype))
													&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
													) {
													if (is_nullptr_literal(EX, *(MR.Context))) {
														auto CISR = nice_source_range(EX->getSourceRange(), Rewrite);
														if (!CISR.isValid()) {
															CISR = SR;
														}
														const std::string error_desc = std::string("Null initialization of ")
															+ "native pointer FD '" + FD->getNameAsString()
															+ "' is not supported.";
														auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CISR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
											}
										}
									}
								}
								if (1 <= unverified_pointer_fields.size()) {
									if (CXXRD->ctors().end() == CXXRD->ctors().begin()) {
										/* There don't seem to be any constructors. */
										auto field_SR = nice_source_range(unverified_pointer_fields.front()->getSourceRange(), Rewrite);
										if (!field_SR.isValid()) {
											field_SR = SR;
										}
										const std::string error_desc = std::string("Missing constructor initializer (or ")
										+ "direct initializer) required for '" + unverified_pointer_fields.front()->getNameAsString()
										+ "' (raw) pointer field.";
										auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, field_SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
									for (const auto& constructor : CXXRD->ctors()) {
										if (constructor->isCopyOrMoveConstructor()) {
											if (constructor->isDefaulted()) {
												continue;
											}
										}
										auto l_unverified_pointer_fields = unverified_pointer_fields;
										int num_pointer_constructor_initializers = 0;
										for (const auto& constructor_initializer : constructor->inits()) {
											if (constructor_initializer->isMemberInitializer()) {
												const auto FD = constructor_initializer->getMember();
												if (FD) {
													for (auto iter = l_unverified_pointer_fields.begin(); l_unverified_pointer_fields.end() != iter; iter++) {
														if (FD == *iter) {
															l_unverified_pointer_fields.erase(iter);

															const auto CIEX = constructor_initializer->getInit();
															if (!CIEX) {
																/* unexpected*/
																int q = 3;
															} else {
																if (false && is_nullptr_literal(CIEX, *(MR.Context))) {
																	/* This case is handled in the MCSSSConstructionInitializer handler. */
																}
															}

															break;
														}
													}
												}
											} else if (constructor_initializer->isBaseInitializer()) {
												/* todo */
												int q = 5;
											} else {
												int q = 5;
											}
										}
										if (1 <= l_unverified_pointer_fields.size()) {
											auto constructor_SR = nice_source_range(constructor->getSourceRange(), Rewrite);
											if (!constructor_SR.isValid()) {
												constructor_SR = SR;
											}
											const std::string error_desc = std::string("Missing constructor initializer (or ")
											+ "direct initializer) required for '" + l_unverified_pointer_fields.front()->getNameAsString()
											+ "' (raw) pointer field.";
											auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, constructor_SR.getBegin(), error_desc));
											if (res.second) {
												std::cout << (*(res.first)).as_a_string1() << " \n\n";
											}
										}
									}
								}
							}
							if (is_xscope_type(*(CXXRD->getTypeForDecl()), state1)) {
								has_xscope_tag_base = true;
							}
							if (contains_non_owning_scope_reference(*(CXXRD->getTypeForDecl()), state1)) {
								has_ContainsNonOwningScopeReference_tag_base = true;
							}
							if (referenceable_by_scope_pointer(*(CXXRD->getTypeForDecl()), state1)) {
								has_ReferenceableByScopePointer_tag_base = true;
							}
						}

						for (const auto& field : RD->fields()) {
							const auto field_qtype = field->getType();
							auto field_qtype_str = field_qtype.getAsString();
							if (!(field_qtype.isNull())) {
								std::string error_desc;
								if (field_qtype.getTypePtr()->isPointerType()) {
									if (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype)
										&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(field->getSourceRange(), Rewrite)))
										) {
										if (has_xscope_tag_base) {
											/*
											error_desc = std::string("Native pointers are not (yet) supported as fields of xscope ")
												+ "structs or classes.";
											*/
										} else {
											if (is_lambda) {
												error_desc = std::string("Native pointers (such as those of type '") + field_qtype.getAsString()
													+ "') are not supported as captures of (non-xscope) lambdas. ";
											} else {
												error_desc = std::string("Native pointers (such as those of type '") + field_qtype.getAsString()
													+"') are not supported as fields of (non-xscope) structs or classes.";
											}
										}
									}
								} else if (field_qtype.getTypePtr()->isReferenceType()) {
									if (has_xscope_tag_base) {
										/*
										error_desc = std::string("Native references are not (yet) supported as fields of xscope ")
											+ "structs or classes.";
										*/
									} else {
										if (is_lambda) {
											error_desc = std::string("Native references (such as those of type '") + field_qtype.getAsString()
												+ "') are not supported as captures of (non-xscope) lambdas. ";
										} else {
											error_desc = std::string("Native references (such as those of type '") + field_qtype.getAsString()
												+"') are not supported as fields of (non-xscope) structs or classes.";
										}
									}
								}

								if ((!has_xscope_tag_base) && is_xscope_type(field_qtype, state1)) {
									if (is_lambda) {
										error_desc = std::string("Lambdas that capture variables of xscope type (such as '")
											+ field_qtype_str + "') must be scope lambdas (usually created via an "
											+  "'mse::rsv::make_xscope_*_lambda()' wrapper function).";
									} else {
										error_desc = std::string("Structs or classes containing fields of xscope type (such as '")
											+ field_qtype_str + "') must inherit from mse::rsv::XScopeTagBase.";
									}
								}
								if ((!has_ContainsNonOwningScopeReference_tag_base)
									&& contains_non_owning_scope_reference(field_qtype, state1)) {
									if (is_lambda) {
										error_desc = std::string("Lambdas that capture items (such as those of type '")
											+ field_qtype_str + "') that are, or contain, non-owning scope references must be "
											+ "scope 'reference or pointer capture' lambdas (created via the "
											+ "'mse::rsv::make_xscope_reference_or_pointer_capture_lambda()' "
											+ "wrapper function).";
									} else {
										error_desc = std::string("Structs or classes containing fields (such as those of type '")
											+ field_qtype_str + "') that are, or contain, non-owning scope references must inherit from "
											+ "mse::rsv::ContainsNonOwningScopeReferenceTagBase.";
									}
								}
								if ((!has_ReferenceableByScopePointer_tag_base)
									&& referenceable_by_scope_pointer(field_qtype, state1)) {
									if (is_lambda) {
										/* The assumption is that we don't have to worry about scope pointers targeting
										lambda capture variables(/fields) from outside the lambda, because they're not 
										directly accessible from outside? */
									} else {
										error_desc = std::string("Structs or classes containing fields (such as '") + field_qtype_str
											+ "') that yield scope pointers (from their overloaded 'operator&'), or contain an element "
											+ "that does, must inherit from mse::rsv::ReferenceableByScopePointerTagBase.";
									}
								}
								if ("" != error_desc) {
									auto FDISR = instantiation_source_range(field->getSourceRange(), Rewrite);
									auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, FDISR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						}
					}
				} else {
					int q = 3;
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR) {
			const RecordDecl* RD = MR.Nodes.getNodeAs<clang::RecordDecl>("mcsssrecorddecl");

			s_handler1(MR, Rewrite, m_state1, RD);
		}

		virtual void onEndOfTranslationUnit()
		{
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSAsAnFParam : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAsAnFParam (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssasanfparam1");

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (suppress_check_flag) {
					return;
				}
				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(as_an_fparam_str, mse_namespace_str() + "::rsv::as_an_fparam");
					DECLARE_CACHED_CONST_STRING(as_a_returnable_fparam_str, mse_namespace_str() + "::rsv::as_a_returnable_fparam");
					if ((as_an_fparam_str == qualified_function_name) || (as_a_returnable_fparam_str == qualified_function_name)) {
						if (1 == num_args) {
							auto EX1 = IgnoreParenImpNoopCasts(CE->getArg(0), *(MR.Context));
							bool satisfies_checks = false;
							auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX1);
							if (DRE1) {
								auto D1 = DRE1->getDecl();
								auto PVD = dyn_cast<const clang::ParmVarDecl>(D1);
								if (PVD) {
									satisfies_checks = true;
								}
							}
							if (!satisfies_checks) {
								const std::string error_desc = std::string("mse::rsv::as_an_fparam() and ")
									+ "mse::rsv::as_a_returnable_fparam() may only be used with function parameters.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						} else {
							/* Wrong number of arguments should result in a compile error,
							so we don't issue a redundant error here. */
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	const clang::Expr* containing_object_ref_expr_from_member_expr(const clang::MemberExpr* ME) {
		const clang::Expr* retval = nullptr;
		if (!ME) {
			//assert(false);
			return retval;
		}
		if (ME) {
			auto iter = ME->child_begin();
			if (ME->child_end() != iter) {
				auto child1 = *iter;
				auto child1_EX = dyn_cast<const clang::Expr>(child1);
				iter++;
				if (ME->child_end() == iter) {
					retval = child1_EX;
				} else {
					/* unexpected */
					int q = 5;
				}
			}
		}
		return retval;
	}

	/* CStaticLifetimeOwner is meant to specify an entity whose (scope) lifetime will be used to
	infer information (often a lower or upper bound) about the (possibly dynamic) lifetime of an
	associated entity (possibly itself). */
	typedef std::variant<const clang::VarDecl*, const clang::CXXThisExpr*, const clang::Expr*
		, const clang::StringLiteral*, CScopeLifetimeInfo1> CStaticLifetimeOwner;

	struct CStaticLifetimeOwnerInfo1Set;

	struct CStaticLifetimeOwnerInfo1 : CStaticLifetimeOwner {
		using CStaticLifetimeOwner::CStaticLifetimeOwner;
		std::vector<CPossessionLifetimeInfo1> m_possession_lifetime_info_chain;
		std::optional<value_ptr1_t<CStaticLifetimeOwnerInfo1Set> > m_maybe_sublifetime_owners_vlptr;
	};
	struct CStaticLifetimeOwnerInfo1Set {
		CStaticLifetimeOwnerInfo1Set() {}
		CStaticLifetimeOwnerInfo1Set(const CTUState& state1, const clang::Type * Type_ptr) {
			auto found_iter = state1.m_type_lifetime_annotations_map.find(Type_ptr);
			if (state1.m_type_lifetime_annotations_map.end() != found_iter) {
				auto& tlta = found_iter->second;
				m_maybe_primary_lifetime_owner_infos.resize(tlta.m_lifetime_set.m_primary_lifetimes.size());
			}
		}
		CStaticLifetimeOwnerInfo1Set(const CAbstractLifetimeSet& alts) {
			set_structure_to_match(alts);
		}
		void set_structure_to_match(const CAbstractLifetimeSet& alts) {
			m_maybe_primary_lifetime_owner_infos.resize(alts.m_primary_lifetimes.size());
			auto alts_iter1 = alts.m_primary_lifetimes.begin();
			auto sltos_iter1 = m_maybe_primary_lifetime_owner_infos.begin();
			for (; (alts.m_primary_lifetimes.end() != alts_iter1) && (m_maybe_primary_lifetime_owner_infos.end() != sltos_iter1)
				; ++alts_iter1, ++sltos_iter1) {

				CStaticLifetimeOwnerInfo1Set sltoi1s;
				sltoi1s.set_structure_to_match(*(alts_iter1->m_sublifetimes_vlptr));

				CStaticLifetimeOwnerInfo1 sltoi1;
				sltoi1.m_maybe_sublifetime_owners_vlptr = { sltoi1s };

				*sltos_iter1 = sltoi1;
			}
		}
		//CStaticLifetimeOwnerInfo1Set(const CStaticLifetimeOwnerInfo1& lifetime_owner_info) : m_maybe_primary_lifetime_owner_infos({ lifetime_owner_info }) {}
		CStaticLifetimeOwnerInfo1Set(const CStaticLifetimeOwnerInfo1Set&) = default;
		//CStaticLifetimeOwnerInfo1Set(CStaticLifetimeOwnerInfo1Set&&) = default;
		const CStaticLifetimeOwnerInfo1& first_lifetime_owner_info() const {
			return m_maybe_primary_lifetime_owner_infos.at(0).value();
		}
		CStaticLifetimeOwnerInfo1& first_lifetime_owner_info() {
			return m_maybe_primary_lifetime_owner_infos.at(0).value();
		}
		//operator const CStaticLifetimeOwnerInfo1& () const { return first_lifetime_owner_info(); }
		//operator CStaticLifetimeOwnerInfo1& () { return first_lifetime_owner_info(); }
		bool operator==(const CStaticLifetimeOwnerInfo1Set& rhs) const {
			return (rhs.m_maybe_primary_lifetime_owner_infos == m_maybe_primary_lifetime_owner_infos);
		}
		bool operator!=(const CStaticLifetimeOwnerInfo1Set& rhs) const {
			return !((*this) == rhs);
		}
		std::vector<std::optional<CStaticLifetimeOwnerInfo1> > m_maybe_primary_lifetime_owner_infos;
	};

	inline auto type_lifetime_annotations_if_available(const clang::Type * TypePtr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/)
		-> std::optional<CTypeLifetimeAnnotations *> {
		std::optional<CTypeLifetimeAnnotations *> retval;
		if (TypePtr) {
			auto TD = TypePtr->getAsRecordDecl();
			if (TD) {
				process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
			}
			auto tlta_iter1 = state1.m_type_lifetime_annotations_map.find(TypePtr);
			if (state1.m_type_lifetime_annotations_map.end() != tlta_iter1) {
				retval = &(tlta_iter1->second);
			}
		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::VarDecl& var_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations *> {
		std::optional<CTypeLifetimeAnnotations *> retval;
		auto qtype = var_decl.getType();
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(qtype, retval);
		if (qtype->isPointerType() || qtype->isReferenceType()) {
			process_variable_lifetime_annotations(var_decl, state1, MR_ptr, Rewrite_ptr);
			auto vlta_iter1 = state1.m_vardecl_lifetime_annotations_map.find(&var_decl);
			if (state1.m_vardecl_lifetime_annotations_map.end() != vlta_iter1) {
				/* Even though the raw pointer types don't have expliicit lifetime annotations in their definition (indeed
				they are built in types so their definition is not available at all), we will consider the type to have an
				implied lifetime annotation when it is the type of a member or variable declaration that has a lifetime
				annotation. */
				static auto s_implied_pointer_tlta = CTypeLifetimeAnnotations{ CAbstractLifetime{ "__implied raw pointer/reference lifetime__", (clang::Decl const *)(nullptr) } };
				retval = &s_implied_pointer_tlta;
			}
		} else {
			retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::FieldDecl& field_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations *> {
		std::optional<CTypeLifetimeAnnotations *> retval;
		auto qtype = field_decl.getType();
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(qtype, retval);
		if (qtype->isPointerType() || qtype->isReferenceType()) {
			process_type_lifetime_annotations(field_decl, state1, MR_ptr, Rewrite_ptr);
			auto fltv_iter1 = state1.m_fielddecl_to_abstract_lifetime_map.find(&field_decl);
			if (state1.m_fielddecl_to_abstract_lifetime_map.end() != fltv_iter1) {
				/* Even though the raw pointer types don't have expliicit lifetime annotations in their definition (indeed
				they are built in types so their definition is not available at all), we will consider the type to have an
				implied lifetime annotation when it is the type of a member or variable declaration that has a lifetime
				annotation. */
				static auto s_implied_pointer_tlta = CTypeLifetimeAnnotations{ CAbstractLifetime{ "__implied raw pointer/reference lifetime__", (clang::Decl const *)(nullptr) } };
				retval = &s_implied_pointer_tlta;
			}
		} else {
			retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::Expr& expr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations *> {
		std::optional<CTypeLifetimeAnnotations *> retval;
		auto E_ii = &expr;
		if (MR_ptr) {
			E_ii = IgnoreParenImpNoopCasts(&expr, *(MR_ptr->Context));
		}
		auto qtype = E_ii->getType();
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(qtype, retval);
		auto DRE = dyn_cast<const clang::DeclRefExpr>(E_ii);
		auto ME = dyn_cast<const clang::MemberExpr>(E_ii);
		if (DRE) {
			auto value_decl = DRE->getDecl();
			if (value_decl) {
				auto VD = dyn_cast<const clang::VarDecl>(value_decl);
				if (VD) {
					retval = type_lifetime_annotations_if_available(*VD, state1, MR_ptr, Rewrite_ptr);
				}
			}
		} else if (ME) {
			const auto VLD = ME->getMemberDecl();
			auto FD = dyn_cast<const clang::FieldDecl>(VLD);
			auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
			if (FD/* && !(ME->isBoundMemberFunction(Ctx))*/) {
				retval = type_lifetime_annotations_if_available(*FD, state1, MR_ptr, Rewrite_ptr);
			} else if (VD) {
				retval = type_lifetime_annotations_if_available(*VD, state1, MR_ptr, Rewrite_ptr);
			} else {
				int q = 5;
			}
		} else {
			if (qtype->isPointerType() || qtype->isReferenceType()) {
				auto CE = dyn_cast<const clang::CallExpr>(E_ii);
				if (CE) {
					const clang::FunctionDecl* function_decl = CE->getDirectCallee();
					if (function_decl) {
						process_function_lifetime_annotations(*function_decl, state1, MR_ptr, Rewrite_ptr);
						auto flta_iter = state1.m_function_lifetime_annotations_map.find(function_decl);
						if (state1.m_function_lifetime_annotations_map.end() != flta_iter) {
							auto flta = flta_iter->second;
							if (!(flta.m_return_value_lifetimes.is_empty())) {
								/* Even though the raw pointer types don't have expliicit lifetime annotations in their definition (indeed
								they are built in types so their definition is not available at all), we will consider the type to have an
								implied lifetime annotation when it is the return type of a function that has a return value lifetime
								annotation. */
								static auto s_implied_pointer_tlta = CTypeLifetimeAnnotations{ CAbstractLifetime{ "__implied raw pointer/reference lifetime__", (clang::Decl const *)(nullptr) } };
								retval = &s_implied_pointer_tlta;
								return retval;
							}
						}
					}
				}
			} else {
				auto CXXILE = dyn_cast<const clang::CXXStdInitializerListExpr>(E_ii);
				auto ILE = dyn_cast<const clang::InitListExpr>(E_ii);
				if (ILE) {
					auto sf_ILE = ILE;
					sf_ILE = ILE->getSemanticForm();
					if (!sf_ILE) {
						sf_ILE = ILE;
					}
					auto sf_ILE_qtype = sf_ILE->getType();
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL(sf_ILE_qtype, retval);
					if (sf_ILE_qtype->isArrayType()) {
						/* We're going to have homogeneous initializer lists implicitly inherit any lifetime
						annotations of their element type. */
						auto elem_qtype = sf_ILE_qtype->getPointeeType();
						retval = type_lifetime_annotations_if_available(elem_qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
						return retval;
					}
				} else if (CXXILE) {
					auto sub_E_ii = CXXILE->getSubExpr();
					if (MR_ptr) {
						sub_E_ii = IgnoreParenImpNoopCasts(CXXILE->getSubExpr(), *(MR_ptr->Context));
					}
					if (sub_E_ii && (sub_E_ii != CXXILE)) {
						/* We're expecting sub_E_ii to be a clang::InitListExpr (pointer). */
						retval = type_lifetime_annotations_if_available(*sub_E_ii, state1, MR_ptr, Rewrite_ptr);
						return retval;
					}
				}
			}
			retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}

	inline void slti_set_default_lower_bound_lifetimes_where_needed(CScopeLifetimeInfo1& slti, clang::QualType const& qtype) {
		MSE_RETURN_IF_TYPE_IS_NULL(qtype);
		auto peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		if (is_raw_pointer_or_equivalent(peeled_qtype)) {
			if ((*(slti.m_sublifetimes_vlptr)).is_empty()) {
				/* If a pointer doesn't already have a sublifetime, we'll use the primary lifetime of the
				pointer itself as a lower bound for its sublifetime. */
				auto shallow_slti = slti;
				shallow_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
				(*(slti.m_sublifetimes_vlptr)).m_primary_lifetime_infos = { CScopeLifetimeInfo1(shallow_slti) };
			}

			auto maybe_pointee_type = pointee_type_if_any(peeled_qtype);
			if (maybe_pointee_type.has_value()) {
				auto pointee_qtype = maybe_pointee_type.value();
				slti_set_default_lower_bound_lifetimes_where_needed((*(slti.m_sublifetimes_vlptr)).m_primary_lifetime_infos.front(), pointee_qtype);
			}
		}
	}

	CScopeLifetimeInfo1 scope_lifetime_info_from_lifetime_owner(const CStaticLifetimeOwnerInfo1& sloiv, ASTContext& Ctx, const CTUState& state1) {
		const CStaticLifetimeOwner& slov = sloiv;
		CScopeLifetimeInfo1 lifetime_info_result;

		auto visitor1 = overloaded {
			[](auto slov) {
				assert(false);
				},

			[&](const clang::StringLiteral* slov) {/* the "static lifetime owner" is a string literal declaration */
				lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::Literal;
				lifetime_info_result.m_maybe_corresponding_cpp_element = slov;
				int q = 5;
				},

			[&](const clang::VarDecl* slov) { /* the "static lifetime owner" is a variable declaration */
				auto maybe_ltvs = state1.corresponding_lifetime_values_if_any(slov);
				if (maybe_ltvs.has_value()) {
					lifetime_info_result = maybe_ltvs.value().m_scope_lifetime_info;
				} else {
					const auto slo_storage_duration = slov->getStorageDuration();
					const auto slo_is_immortal = ((clang::StorageDuration::SD_Static == slo_storage_duration) || (clang::StorageDuration::SD_Thread == slo_storage_duration)) ? true : false;
					lifetime_info_result.m_category = slo_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
					lifetime_info_result.m_maybe_containing_scope = get_containing_scope(slov, Ctx);
					lifetime_info_result.m_maybe_source_range = slov->getSourceRange();
					lifetime_info_result.m_maybe_corresponding_cpp_element = slov;

					auto maybe_alts = state1.corresponding_abstract_lifetime_set_if_any(slov);
					if (maybe_alts.has_value()) {
						/* If this variable declaration has corresponding abstract lifetime annotations then we'll set
						the (sub)lifetime values to those abstract lifetimes. */
						auto& alts = maybe_alts.value();
						*(lifetime_info_result.m_sublifetimes_vlptr) = { CScopeLifetimeInfo1Set { alts } };
					}
					slti_set_default_lower_bound_lifetimes_where_needed(lifetime_info_result, slov->getType());
				}
				},

			[&](const clang::CXXThisExpr* slov) { /* the "static lifetime owner" is a 'this' pointer expression' */
				/* Ok, first we're going to construct a lifetime value for the target object of the "this"
				pointer/expression. */
				CScopeLifetimeInfo1 pointer_target_lifetime_info;
				pointer_target_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::ThisExpression;
				pointer_target_lifetime_info.m_maybe_abstract_lifetime = state1.corresponding_abstract_lifetime_if_any(slov, Ctx);

				auto FND = enclosing_function_if_any(slov, Ctx);
				if (FND) {
					pointer_target_lifetime_info.m_maybe_containing_scope = get_containing_scope(FND, Ctx);
					auto FNDSL = FND->getLocation();
					if (FNDSL.isValid()) {
						/* A `this` pointer can be thought of as an implicit parameter of the associated (member) function
						or operator. So we're just going to report the source range of its declaration as being at the
						location of the function/operator name in the declaration, while assuming that's close enough to
						where an equivalent explicitly declared `this` parameter would be. */
						pointer_target_lifetime_info.m_maybe_source_range = clang::SourceRange{ FNDSL, FNDSL };
					} else {
						int q = 3;
					}
				} else {
					int q = 3;
				}
				pointer_target_lifetime_info.m_maybe_corresponding_cpp_element = slov;
				slti_set_default_lower_bound_lifetimes_where_needed(pointer_target_lifetime_info, slov->getType());

				/* Ok, now we set the values for the lifetime of the "this" expression itself. */
				lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::ThisExpression;
				lifetime_info_result.m_maybe_containing_scope = pointer_target_lifetime_info.m_maybe_containing_scope;
				lifetime_info_result.m_maybe_source_range = pointer_target_lifetime_info.m_maybe_source_range;
				lifetime_info_result.m_maybe_corresponding_cpp_element = slov;
				/* Here we set the sublifetime value of the "this" pointer to the lifetime value of the
				pointer target lifetime value we constructed earlier. */
				*(lifetime_info_result.m_sublifetimes_vlptr) = { pointer_target_lifetime_info };
				},

			[&](const clang::Expr* slov) { /* the "static lifetime owner" is an expression */
				auto maybe_ltvs = state1.corresponding_lifetime_values_if_any(slov);
				if (maybe_ltvs.has_value()) {
					lifetime_info_result = maybe_ltvs.value().m_scope_lifetime_info;
				} else {
					/* There's not much we can infer about the (scope) lifetime of an object if all we know
					is that it's the result of some expression. */
					lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
					lifetime_info_result.m_maybe_corresponding_cpp_element = slov;
					auto FND = enclosing_function_if_any(slov, Ctx);
					if (FND) {
						lifetime_info_result.m_maybe_containing_scope = get_containing_scope(FND, Ctx);
					} else {
						int q = 3;
					}
					lifetime_info_result.m_maybe_source_range = slov->getSourceRange();
				}
				},

			[&](const CScopeLifetimeInfo1& slov) {
				/* Instead of the precise "static lifetime owner", we've been given a struct that
				contains information about it's scope lifetime. */
				lifetime_info_result = slov;
				},
		};

		std::visit(visitor1, slov);
		lifetime_info_result.m_possession_lifetime_info_chain = sloiv.m_possession_lifetime_info_chain;
		return lifetime_info_result;
	}
	CStaticLifetimeOwnerInfo1 lifetime_owner_shallow(const clang::Expr* E, ASTContext& Ctx, const CTUState& state1) {
		CStaticLifetimeOwnerInfo1 retval = E;
		auto DRE = dyn_cast<const clang::DeclRefExpr>(E);
		if (DRE) {
			auto value_decl = DRE->getDecl();
			if (value_decl) {
				auto VD = dyn_cast<const clang::VarDecl>(value_decl);
				if (VD) {
					retval = VD; return retval;
				}
			}
		}
		auto CXXTE = dyn_cast<const clang::CXXThisExpr>(E);
		if (CXXTE) {
			retval = CXXTE; return retval;
		}
		auto SL = dyn_cast<const clang::StringLiteral>(E);
		if (SL) {
			retval = SL; return retval;
		}
		return retval;
	}
	CStaticLifetimeOwnerInfo1 lifetime_owner_shallow_ii(const clang::Expr* E, ASTContext& Ctx, const CTUState& state1) {
		return lifetime_owner_shallow(IgnoreParenImpNoopCasts(E, Ctx), Ctx, state1);
	}

	inline bool first_is_known_to_be_contained_in_scope_of_second_shallow(const CScopeLifetimeInfo1& sli1, const CScopeLifetimeInfo1& sli2, clang::ASTContext& context, const CTUState& tu_state_cref) {
		bool retval = true;
		if (CScopeLifetimeInfo1::ECategory::Literal == sli1.m_category) {
			return false;
		} else if (CScopeLifetimeInfo1::ECategory::Literal == sli2.m_category) {
			return true;
		} else if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli2.m_category) {
			if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli1.m_category) {
				if (sli1 == sli2) {
					return true;
				} else {
					/* A temporary expression could contain another (sub)expression, but at the moment, that
					determination is never used, so, for now, we won't bother evaluating it. */
					return false;
				}
			} else {
				return false;
			}
		} else if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli1.m_category) {
			/* The lifetime of a temporary expression may or may not be contained in the scope of another 
			given (non-temporary) scope lifetime, but at the moment, that determination is never used, so,
			for now, we won't bother evaluating it. */
			return true;
		} else if (sli1.m_maybe_abstract_lifetime.has_value() || sli2.m_maybe_abstract_lifetime.has_value()) {
			if (sli1.m_maybe_abstract_lifetime.has_value() && sli2.m_maybe_abstract_lifetime.has_value()) {
				bool b1a = (CScopeLifetimeInfo1::ECategory::AbstractLifetime == sli1.m_category);
				bool b2a = (CScopeLifetimeInfo1::ECategory::AbstractLifetime == sli2.m_category);
				bool b1b = (CScopeLifetimeInfo1::ECategory::ThisExpression == sli1.m_category);
				bool b2b = (CScopeLifetimeInfo1::ECategory::ThisExpression == sli2.m_category);
				if (!((b1a || b1b) && (b2a || b2b))) {
					int q = 3;
				}

				{
					auto pl_res1 = tu_state_cref.second_can_be_assigned_to_first(sli1.m_maybe_abstract_lifetime.value(), sli2.m_maybe_abstract_lifetime.value());
					if (CTUState::EYesNoDontKnow::Yes == pl_res1) {
					} else {
						return false;
					}

					auto& sli1_primary_sublifetimes = sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos;
					auto& sli2_primary_sublifetimes = sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos;

					if (sli1_primary_sublifetimes.size() != sli2_primary_sublifetimes.size()) {
						return false;
					} else {
						auto sl_iter1 = sli1_primary_sublifetimes.begin();
						auto sl_iter2 = sli2_primary_sublifetimes.begin();
						for ( ; sli1_primary_sublifetimes.end() != sl_iter1; ++sl_iter1, ++sl_iter2) {
							retval = first_is_known_to_be_contained_in_scope_of_second_shallow(*sl_iter1, *sl_iter2, context, tu_state_cref);
							if (false == retval) {
								return false;
							}
						}
					}
				}
				return retval;
			} else if (sli2.m_maybe_abstract_lifetime.has_value()) {
				if (CScopeLifetimeInfo1::ECategory::Automatic == sli1.m_category) {
					/* We're assuming that sli2 corresponds to a pointer/reference parameter with lifetime
					annotation. And that sli1 does not correspond to a reference parameter or reassignable
					(i.e. non-const) pointer. */
					if (sli1.m_maybe_containing_scope.has_value() && sli1.m_maybe_source_range.has_value()
						 && sli2.m_maybe_containing_scope.has_value() && sli2.m_maybe_source_range.has_value()) {
						retval = first_is_contained_in_scope_of_second(sli1.m_maybe_containing_scope.value(), sli1.m_maybe_source_range.value()
						 , sli2.m_maybe_containing_scope.value(), sli2.m_maybe_source_range.value(), context);
						if (!retval) {
							/* does this ever happen? */
							int q = 3;
						}
					} else {
						retval = false;
					}
					return retval;
				}
			}
			return false;
		} else {
			if (CScopeLifetimeInfo1::ECategory::Immortal == sli1.m_category) {
				if (CScopeLifetimeInfo1::ECategory::Immortal == sli2.m_category) {
					if (sli1.m_maybe_source_range.has_value() && sli2.m_maybe_source_range.has_value()) {
						return first_is_contained_in_scope_of_second(sli1.m_maybe_source_range.value(), sli2.m_maybe_source_range.value(), context);
					} else {
						/* There are (rare) cases when we know that the object in question must be of
						thread_local or static duration, but nothing else. */
						return false;
					}
				} else {
					return false;
				}
			} else if (CScopeLifetimeInfo1::ECategory::Immortal == sli2.m_category) {
				return true;
			} else if (CScopeLifetimeInfo1::ECategory::ThisExpression == sli1.m_category) {
				if (CScopeLifetimeInfo1::ECategory::ThisExpression == sli2.m_category) {
					/* Both items are "this" expressions. */

					/* A `this` pointer can be thought of as an implicit parameter of the associated (member) function
					or operator. In this case the given source range fields have been populated with (an approximation
					of) the location of where an equivalent explicitly declared `this` parameter would be. So we'll
					construct two CScopeLifetimeInfo1s as if their "scope lifetime owner" is such a hypothetical
					parameter variable and apply this function to them. */
					CScopeLifetimeInfo1 equivalent_for_our_purposes_sli1 = sli1;
					equivalent_for_our_purposes_sli1.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
					CScopeLifetimeInfo1 equivalent_for_our_purposes_sli2 = sli2;
					equivalent_for_our_purposes_sli2.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
					retval = first_is_known_to_be_contained_in_scope_of_second_shallow(equivalent_for_our_purposes_sli1, equivalent_for_our_purposes_sli2, context, tu_state_cref);
				} else {
					return false;
				}
			} else if (CScopeLifetimeInfo1::ECategory::ThisExpression == sli2.m_category) {
				return true;
			} else if (sli1.m_maybe_containing_scope.has_value()) {
				if (sli2.m_maybe_containing_scope.has_value()) {
					auto scope1 = sli1.m_maybe_containing_scope.value();
					const auto scope2 = sli2.m_maybe_containing_scope.value();
					if (scope2 == scope1) {
						if (sli1.m_maybe_source_range.has_value() && sli2.m_maybe_source_range.has_value()) {
							const auto SR1 = sli1.m_maybe_source_range.value();
							const auto SR2 = sli2.m_maybe_source_range.value();
							if (SR1.isValid() && SR2.isValid()) {
								if (SR2.getEnd() < SR1.getBegin()) {
									/* The second item occurs before the first item. */
									retval = true;
								} else if ( (SR1.getBegin() == SR2.getBegin()) && (SR1.getEnd() == SR2.getEnd()) ) {
									if (true) {
										/* We used to distinguish between the lifetime of, say, an object and a member field of that
										object, or between a container object and an element in that container. But since the
										introduced lifetime annotations are not (yet) specific enough to make the distinction, we
										will suspend making the distiction here (for now). */
										retval = true;
									} else {
										/* The second item and first item seem to have the same "scope lifetime owner". */
										auto possession_lifetime_info_chain1 = sli1.m_possession_lifetime_info_chain;
										std::reverse(possession_lifetime_info_chain1.begin(), possession_lifetime_info_chain1.end());
										auto possession_lifetime_info_chain2 = sli2.m_possession_lifetime_info_chain;
										std::reverse(possession_lifetime_info_chain2.begin(), possession_lifetime_info_chain2.end());

										auto max_possession_chain_size = std::max(possession_lifetime_info_chain1.size(), possession_lifetime_info_chain2.size());
										retval = true;
										for (size_t i = 0; i < max_possession_chain_size; i += 1) {
											if (i >= possession_lifetime_info_chain1.size()) {
												/* The second item seems to be a "possession" of the first and, in general, possessions outlive
												their owners (if only barely). */
												retval = true;
												break;
											} else if (i >= possession_lifetime_info_chain2.size()) {
												/* The second item seems to be an "owner" of the first and, in general, owners do not outlive
												their possessions (if only barely). */
												retval = false;
												break;
											} else {
												retval = false;
												auto& info1 = possession_lifetime_info_chain1.at(i);
												auto& info2 = possession_lifetime_info_chain2.at(i);
												if ((CPossessionLifetimeInfo1::is_element_in_a_multi_element_container_t::Yes == info1.m_is_element_in_a_multi_element_container)
													|| (CPossessionLifetimeInfo1::is_element_in_a_multi_element_container_t::Yes == info2.m_is_element_in_a_multi_element_container)) {
													if (info1.m_is_element_in_a_multi_element_container != info2.m_is_element_in_a_multi_element_container) {
														/* This never happens. Because if the (common) immediate owner of both elements is a
														multi-element container, then (generally) the only type of possession it would have would be a
														contained element (as opposed to a field member or a dereference target). Right? */
														int q = 3;
													}
													/* At the moment, for practical purposes, different elements in "multi-element" containers are
													considered here to have the "same" lifetime. This might theoretically be an issue if, for
													example, one element, with a mischievous destructor, points/refers to another element in the
													same container. When/if that case is supported, it will need to be addressed elsewhere. */
													retval = true;
													break;
												} else if (info2.m_maybe_field_source_range.has_value()) {
													/* info2 refers to a member field. */
													if (info1.m_maybe_field_source_range.has_value()) {
														/* The result for two member fields should be the same as if the member fields were instead
														variables declared in the same relative positions. So we'll construct two CScopeLifetimeInfo1s
														corresponding to those two hypothetical variables and apply this function to them. */
														CScopeLifetimeInfo1 equivalent_for_our_purposes_sli1{ sli1.m_maybe_containing_scope, info1.m_maybe_field_source_range.value() };
														CScopeLifetimeInfo1 equivalent_for_our_purposes_sli2{ sli2.m_maybe_containing_scope, info2.m_maybe_field_source_range.value() };
														retval = first_is_known_to_be_contained_in_scope_of_second_shallow(equivalent_for_our_purposes_sli1, equivalent_for_our_purposes_sli2, context, tu_state_cref);
														if (false == retval) {
															break;
														}
													} else {
														/* The second item is a member field and the first item isn't (so it's presumably a dereference
														target). */
														retval = false;
														break;
													}
												} else if (info1.m_maybe_field_source_range.has_value()) {
													assert(!(info2.m_maybe_field_source_range.has_value()));
													/* The first item is a member field and the second item isn't (so it's presumably a dereference
													target). */
													retval = true;
												} else {
													retval = false;
													break;
												}
											}
										}
									}
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
						} else {
							/* I don't know if there are any situations where we would know the containing scope
							of each item, but not the their source range. */
							assert(false);
							return false;
						}
					}
					while (scope2 != scope1) {
						scope1 = get_containing_scope(scope1, context);
						if (!scope1) {
							retval = (scope2 == scope1);
							break;
						}
					}
				} else {
					/* The second item doesn't seem to have any scope lifetime information. It may,
					for example, correspond to an expression for which the scope lifetime cannot be
					determined. */
					return false;
				}
			} else if (sli2.m_maybe_containing_scope.has_value()) {
				retval = true;
			} else {
				/* Neither item seems to have any scope lifetime information. This could, for example,
				be a case where both items correspond to expressions where the scope lifetime of the
				resulting object can't be determined. */
				retval = false;
			}
		}
		return retval;
	}

	inline bool slti_second_can_be_assigned_to_first(const CScopeLifetimeInfo1& sli1, const CScopeLifetimeInfo1& sli2, clang::ASTContext& context, const CTUState& tu_state_cref) {
		bool retval = true;

		/* Generally, the "primary" (scope) lifetimes of the argument objects don't matter for
		assignment. It's the "sublifetimes" (i.e. the lifetimes of any objects being referenced),
		if any, that matter. */

		auto sub_lifetime_infos1 = (sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos);
		auto sub_lifetime_infos2_ptr = &(sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos);
		std::vector<CScopeLifetimeInfo1> sub_lifetime_infos2_fallback;
		if (sub_lifetime_infos1.size() > sub_lifetime_infos2_ptr->size()) {
			/* Some or all of the sublifetime values of the second object are unavailable. As a substitute
			we will just use the value of its primary lifetime as a lower bound value for each of its
			sublifetimes. */
			sub_lifetime_infos2_fallback = std::vector<CScopeLifetimeInfo1>(sub_lifetime_infos1.size(), sli2);
			sub_lifetime_infos2_ptr = &sub_lifetime_infos2_fallback;
		} else if (sub_lifetime_infos1.size() < sub_lifetime_infos2_ptr->size()) {
			/* Some or all of the sublifetime values of the first object are unavailable. Unfortunately
			there's no way to determine an upper bound for these sublifetimes. */
			return false;
		}
		auto& sub_lifetime_infos2 = *sub_lifetime_infos2_ptr;

		auto sli1_sub_iter1 = sub_lifetime_infos1.begin();
		auto sli2_sub_iter1 = sub_lifetime_infos2.begin();
		for (; sub_lifetime_infos1.end() != sli1_sub_iter1; ++sli1_sub_iter1, ++sli2_sub_iter1) {
			retval &= first_is_known_to_be_contained_in_scope_of_second_shallow(*sli1_sub_iter1, *sli2_sub_iter1, context, tu_state_cref);
			if (!retval) {
				return retval;
			}
			retval &= slti_second_can_be_assigned_to_first(*sli1_sub_iter1, *sli2_sub_iter1, context, tu_state_cref);
			if (!retval) {
				return retval;
			}
		}

		return retval;
	}

	/* Given a pair of "lifetime owner" elements, it returns the one with the "shorter" lifetime. */
	std::optional<CStaticLifetimeOwnerInfo1> lower_bound_lifetime_owner(const std::optional<CStaticLifetimeOwnerInfo1>& lifetime_owner1,const std::optional<CStaticLifetimeOwnerInfo1>& lifetime_owner2, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;

		auto lhs_slo = lifetime_owner1;
		auto rhs_slo = lifetime_owner2;

		if (!(lhs_slo.has_value())) {
			/* An absent lifetime owner is deemed the one to have the "shortest" lifetime. */
			return lifetime_owner1;
		} else if (!(rhs_slo.has_value())) {
			return lifetime_owner2;
		} else {
			auto lhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(lhs_slo.value(), Ctx, tu_state_cref);
			auto rhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(rhs_slo.value(), Ctx, tu_state_cref);

			bool lhs_lifetime_is_shorter_than_rhs = first_is_known_to_be_contained_in_scope_of_second_shallow(lhs_lifetime_info, rhs_lifetime_info, Ctx, tu_state_cref);
			retval = lhs_lifetime_is_shorter_than_rhs ? lifetime_owner1 : lifetime_owner2;
		}

		return retval;
	}

	/* Given a set of "lifetime owner" elements, it returns the one with the "shortest" lifetime. */
	std::optional<CStaticLifetimeOwnerInfo1> lower_bound_lifetime_owner(const std::vector<std::optional<CStaticLifetimeOwnerInfo1> >& lifetime_owners, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;
		if (1 <= lifetime_owners.size()) {
			retval = lifetime_owners.front();
			for (const auto& lifetime_owner : lifetime_owners) {
				retval = lower_bound_lifetime_owner(retval, lifetime_owner, Ctx, tu_state_cref);
			}
		}
		return retval;
	}

	/* Given a pair of "lifetime owner" elements, it returns the one with the "longer" lifetime. */
	std::optional<CStaticLifetimeOwnerInfo1> upper_bound_lifetime_owner(const std::optional<CStaticLifetimeOwnerInfo1>& lifetime_owner1, const std::optional<CStaticLifetimeOwnerInfo1>& lifetime_owner2, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;
		const auto lblo = lower_bound_lifetime_owner(lifetime_owner1, lifetime_owner2, Ctx, tu_state_cref);
		/* Just return the opposite choice of the lower_bound_lifetime_owner() function. */
		retval = (lblo == lifetime_owner1) ? lifetime_owner2 : lifetime_owner1;
		return retval;
	}

	/* Given a set of "lifetime owner" elements, it returns the one with the "longest" lifetime. */
	std::optional<CStaticLifetimeOwnerInfo1> upper_bound_lifetime_owner(const std::vector<std::optional<CStaticLifetimeOwnerInfo1> >& lifetime_owners, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;
		if (1 <= lifetime_owners.size()) {
			retval = lifetime_owners.front();
			for (const auto& lifetime_owner : lifetime_owners) {
				retval = upper_bound_lifetime_owner(retval, lifetime_owner, Ctx, tu_state_cref);
			}
		}
		return retval;
	}

	/* Given a pair of "scope lifetime info" elements, it returns a "scope lifetime info" element
	representing the lower bound of the two. The returned element may or may not be equivalent to
	one of the two given elements. */
	CScopeLifetimeInfo1 lower_bound_lifetime_sli(const CScopeLifetimeInfo1& lhs_lifetime_info, const CScopeLifetimeInfo1& rhs_lifetime_info, ASTContext& Ctx, const CTUState& tu_state_cref) {
		CScopeLifetimeInfo1 lbl;
		{
			auto shallow_lhs_lifetime_info = lhs_lifetime_info;
			shallow_lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
			auto shallow_rhs_lifetime_info = rhs_lifetime_info;
			shallow_rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();

			/* If one of he lifetimes has fewer sublifetimes than the other we will just fill in the
			"missing" sublifetimes with the primary lifetime (which serves as a lower bound to any
			sublifetime). */
			while (lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.size() > rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) {
				rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(shallow_rhs_lifetime_info);
			}
			while (rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.size() > lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) {
				lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(shallow_lhs_lifetime_info);
			}

			/* First we determine the "shallow" lower bound (without sublifetimes). */
			if (slti_second_can_be_assigned_to_first(shallow_lhs_lifetime_info, shallow_rhs_lifetime_info, Ctx, tu_state_cref)) {
				lbl = shallow_lhs_lifetime_info;
			} else {
				lbl = shallow_rhs_lifetime_info;
			}

			/* Next we determine the lower bound of each pair of corresponding sublifetimes. */
			auto lhs_sublt_iter1 = lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.begin();
			auto rhs_sublt_iter1 = rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.begin();
			for (; lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.end() != lhs_sublt_iter1; lhs_sublt_iter1++, rhs_sublt_iter1++) {
				lbl.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(lower_bound_lifetime_sli(*lhs_sublt_iter1, *rhs_sublt_iter1, Ctx, tu_state_cref));
			}
		}
		return lbl;
	}

	/* Given a set of "scope lifetime info" elements, it returns a lower bound for all of them. The
	returned element may or may not be equivalent to one of the given elements. */
	CScopeLifetimeInfo1 lower_bound_lifetime_sli(const std::vector<CScopeLifetimeInfo1>& lifetime_infos, ASTContext& Ctx, const CTUState& tu_state_cref) {
		CScopeLifetimeInfo1 retval;
		if (1 <= lifetime_infos.size()) {
			retval = lifetime_infos.front();
			for (const auto& lifetime_info : lifetime_infos) {
				retval = lower_bound_lifetime_sli(retval, lifetime_info, Ctx, tu_state_cref);
			}
		}
		return retval;
	}


	struct CMaybeVariableLifetimeValuesWithHints : public std::optional<CVariableLifetimeValues> {
		typedef std::optional<CVariableLifetimeValues> base_class;
		using base_class::base_class;
		CMaybeVariableLifetimeValuesWithHints(const CMaybeVariableLifetimeValuesWithHints& src) = default;
		CMaybeVariableLifetimeValuesWithHints(CMaybeVariableLifetimeValuesWithHints&& src) = default;
		CMaybeVariableLifetimeValuesWithHints(const base_class& src) : base_class(src) {}
		CMaybeVariableLifetimeValuesWithHints(base_class&& src) : base_class(std::forward<decltype(src)>(src)) {}
		CMaybeVariableLifetimeValuesWithHints& operator=(const CMaybeVariableLifetimeValuesWithHints& src) = default;
		CMaybeVariableLifetimeValuesWithHints& operator=(CMaybeVariableLifetimeValuesWithHints&& src) = default;
		std::string hints_str() const {
			std::string retval;
			for (const auto& str : m_hints) {
				retval += str + " ";
			}
			if (!retval.empty()) {
				retval = retval.substr(0, retval.size() - 1);
			}
			return retval;
		}

		std::vector<std::string> m_hints;
		bool m_failure_due_to_dependent_type_flag = false;
	};
#define MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_lifetime_value, retval) \
	if (maybe_lifetime_value.m_failure_due_to_dependent_type_flag) { \
		/* Cannot properly evaluate because this is a template definition. Proper evaluation should \
		occur in any instantiation of the template. */ \
		return retval; \
	}
#define MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_lifetime_value) MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_lifetime_value, )

	struct CMaybeExpressionLifetimeValuesWithHints : public std::optional<CExpressionLifetimeValues> {
		typedef std::optional<CExpressionLifetimeValues> base_class;
		using base_class::base_class;
		CMaybeExpressionLifetimeValuesWithHints(const CMaybeExpressionLifetimeValuesWithHints& src) = default;
		CMaybeExpressionLifetimeValuesWithHints(CMaybeExpressionLifetimeValuesWithHints&& src) = default;
		CMaybeExpressionLifetimeValuesWithHints(const base_class& src) : base_class(src) {}
		CMaybeExpressionLifetimeValuesWithHints(base_class&& src) : base_class(std::forward<decltype(src)>(src)) {}
		CMaybeExpressionLifetimeValuesWithHints& operator=(const CMaybeExpressionLifetimeValuesWithHints& src) = default;
		CMaybeExpressionLifetimeValuesWithHints& operator=(CMaybeExpressionLifetimeValuesWithHints&& src) = default;
		std::string hints_str() const {
			std::string retval;
			for (const auto& str : m_hints) {
				retval += str + " ";
			}
			if (!retval.empty()) {
				retval = retval.substr(0, retval.size() - 1);
			}
			return retval;
		}

		std::vector<std::string> m_hints;
		bool m_failure_due_to_dependent_type_flag = false;
	};

	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::Expr* E);
	template<typename TCallOrConstuctorExpr>
	inline std::string function_call_handler2(CTUState& state1, const clang::FunctionDecl* function_decl
		, const TCallOrConstuctorExpr* CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);

	struct CMaybeStaticLifetimeOwnerWithHints : public std::optional<CStaticLifetimeOwnerInfo1> {
		typedef std::optional<CStaticLifetimeOwnerInfo1> base_class;
		using base_class::base_class;
		CMaybeStaticLifetimeOwnerWithHints(const CMaybeStaticLifetimeOwnerWithHints& src) = default;
		CMaybeStaticLifetimeOwnerWithHints(CMaybeStaticLifetimeOwnerWithHints&& src) = default;
		CMaybeStaticLifetimeOwnerWithHints(const base_class& src) : base_class(src) {}
		CMaybeStaticLifetimeOwnerWithHints(base_class&& src) : base_class(std::forward<decltype(src)>(src)) {}
		CMaybeStaticLifetimeOwnerWithHints& operator=(const CMaybeStaticLifetimeOwnerWithHints& src) = default;
		CMaybeStaticLifetimeOwnerWithHints& operator=(CMaybeStaticLifetimeOwnerWithHints&& src) = default;
		std::string hints_str() const {
			std::string retval;
			for (const auto& str : m_hints) {
				retval += str + " ";
			}
			if (!retval.empty()) {
				retval = retval.substr(0, retval.size() - 1);
			}
			return retval;
		}

		std::vector<std::string> m_hints;
	};

	std::optional<CStaticLifetimeOwnerInfo1> lower_bound_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	CMaybeStaticLifetimeOwnerWithHints lower_bound_lifetime_owner_of_pointer_target_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);

	/* This function is meant to return the part of a given expression that directly refers to the declared
	object (i.e. the `DeclRefExpr`) of interest, if it is present in the expression. The object of interest
	is the one from which we can infer the lifetime (or a lower bound of the lifetime) of the intended
	reference target (indicated by the given expression). If the intended reference target is itself a
	declared object (as opposed to, for example, a member of another object, or an element in a container),
	then it itself would be the object of interest. The object of interest could also be a (declared)
	(scope) reference/pointer to the target object, as target objects must outlive any corresponding scope
	references, so the lifetime of a (scope) reference is a lower bound for the lifetime of the
	corresponding target object. */
	CMaybeStaticLifetimeOwnerWithHints lower_bound_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		CMaybeStaticLifetimeOwnerWithHints retval;
		if (!EX1) {
			return retval;
		}
		bool satisfies_checks = false;

		auto MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(IgnoreExprWithCleanups(EX1));

		const auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);
		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		auto CXXTE = dyn_cast<const clang::CXXThisExpr>(EX);
		auto SL = dyn_cast<const clang::StringLiteral>(EX);
		if (MTE) {
			auto lifetime_extending_decl = MTE->getExtendingDecl();
			if (lifetime_extending_decl) {
				/* The expression has "scope lifetime" by virtue of being a "lifetime-extended" temporary. */
				auto le_VD = dyn_cast<const clang::VarDecl>(lifetime_extending_decl);
				if (le_VD) {
					retval = le_VD;
				}
			} else {
				retval = MTE;
			}
			return retval;
		} else if (DRE1) {
			const auto DRE1_qtype = DRE1->getType();
			IF_DEBUG(const auto DRE1_qtype_str = DRE1_qtype.getAsString();)
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL(DRE1_qtype, retval);

			auto D1 = DRE1->getDecl();
			const auto D1_qtype = D1->getType();
			IF_DEBUG(const auto D1_qtype_str = D1_qtype.getAsString();)
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL(D1_qtype, retval);

			auto VD = dyn_cast<const clang::VarDecl>(D1);
			if (VD) {
				const auto VD_qtype = VD->getType();
				IF_DEBUG(const auto VD_qtype_str = VD_qtype.getAsString();)
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);

				const auto storage_duration = VD->getStorageDuration();
				if ((clang::StorageDuration::SD_Automatic == storage_duration)
					|| (clang::StorageDuration::SD_Thread == storage_duration)
					) {
					if (VD_qtype->isReferenceType()) {
						satisfies_checks = false;
						retval = VD;
						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							process_function_lifetime_annotations(*PVD, tu_state_ref);
							auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(PVD);
							if (maybe_abstract_lifetime_set.has_value()) {
								auto& alts1 = maybe_abstract_lifetime_set.value();
								if (1 <= alts1.m_primary_lifetimes.size()) {
									auto& alt = alts1.m_primary_lifetimes.front();
									CScopeLifetimeInfo1 scope_lifetime_info1(alt);

									scope_lifetime_info1.m_maybe_containing_scope = get_containing_scope(PVD, Ctx);
									scope_lifetime_info1.m_maybe_source_range = PVD->getSourceRange();

									retval = scope_lifetime_info1;
								} else {
									/* unexpected? */
									int q = 3;
								}

								if (PVD->hasInit()) {
									/* todo: error: we currently do not support default initializers on parameters with lifetime
									annotations. */
									int q = 3;
								}
							}
							return retval;
						} else {
							if (VD->hasInit()) {
								retval = lower_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								return retval;
							}
						}
					} else {
						retval = VD;

						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							process_function_lifetime_annotations(*PVD, tu_state_ref);
							auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(PVD);
							if (maybe_abstract_lifetime_set.has_value()) {
								CScopeLifetimeInfo1 scope_lifetime_info1;
								*(scope_lifetime_info1.m_sublifetimes_vlptr) = maybe_abstract_lifetime_set.value();
								if (clang::StorageDuration::SD_Thread == storage_duration) {
									scope_lifetime_info1.m_category = CScopeLifetimeInfo1::ECategory::Immortal;
								} else {
									assert(clang::StorageDuration::SD_Automatic == storage_duration);
									scope_lifetime_info1.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
								}

								scope_lifetime_info1.m_maybe_containing_scope = get_containing_scope(PVD, Ctx);
								scope_lifetime_info1.m_maybe_source_range = PVD->getSourceRange();

								retval = scope_lifetime_info1;

								if (PVD->hasInit()) {
									/* todo: error: we currently do not support default initializers on parameters with lifetime
									annotations. */
									int q = 3;
								}
							}
							return retval;
						}

						if (is_raw_pointer_or_equivalent(VD_qtype)) {
							if (VD->hasInit()) {
								auto maybe_res1 = lower_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (maybe_res1.has_value()) {
									if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
										retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
										return retval;
									}
								}
								auto maybe_res2 = lower_bound_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
								if (maybe_res2.has_value()) {
									CStaticLifetimeOwnerInfo1Set sloi1_set;
									sloi1_set.m_maybe_primary_lifetime_owner_infos.push_back(maybe_res2.value());
									retval.value().m_maybe_sublifetime_owners_vlptr = { sloi1_set };
								}
								return retval;
							}
						}

						auto RD = VD_qtype.getTypePtr()->getAsRecordDecl();
						if (RD) {
							auto found_iter = tu_state_ref.m_type_lifetime_annotations_map.find(RD->getTypeForDecl());
							if (tu_state_ref.m_type_lifetime_annotations_map.end() != found_iter) {
								auto& tlta = found_iter->second;
								if (VD->hasInit()) {
									auto maybe_res1 = lower_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
									if (maybe_res1.has_value()) {
										if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
											retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
											return retval;
										}
									}
									auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(VD->getInit());
									auto CXXTOE = dyn_cast<const clang::CXXTemporaryObjectExpr>(VD->getInit());
									if (CXXCE) {
										auto CD = CXXCE->getConstructor();
										if (CD) {
											if (CD->isCopyOrMoveConstructor()) {
												if (1 == CXXCE->getNumArgs()) {
													auto arg_EX = CXXCE->getArg(0);
													auto maybe_res1 = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
													if (maybe_res1.has_value()) {
														if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
															retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
															return retval;
														}
													}
												} else { assert(false); }
											} else {
												process_function_lifetime_annotations(*CD, tu_state_ref);
												auto found_iter2 = tu_state_ref.m_function_lifetime_annotations_map.find(CD);
												if (tu_state_ref.m_function_lifetime_annotations_map.end() != found_iter2) {
													auto& flta = found_iter2->second;
													auto sublifetime_owners = CStaticLifetimeOwnerInfo1Set(tlta.m_lifetime_set);

													auto& CE = CXXCE;
													auto lifetime_owner_info_iter1 = sublifetime_owners.m_maybe_primary_lifetime_owner_infos.begin();
													for (const auto& type_lifetime : tlta.m_lifetime_set.m_primary_lifetimes) {
														for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
															auto& param_lifetime_set = param_lifetime1.second;
															if (1 == param_lifetime_set.m_primary_lifetimes.size()) {
																if (param_lifetime_set.first_lifetime() == type_lifetime) {
																	clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
																	if (!arg1_EX) {
																		continue;
																	} else {
																		auto maybe_res1 = lower_bound_lifetime_owner_if_available(arg1_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
																		if (maybe_res1.has_value()) {
																			auto param_index = int(param_lifetime1.first) - 1;
																			if ((0 <= param_index) && (int(CD->getNumParams()) > param_index)) {
																				auto PVD = CD->getParamDecl(param_index);
																				if (PVD) {
																					const auto PVD_qtype = PVD->getType();
																					MSE_RETURN_VALUE_IF_TYPE_IS_NULL(PVD_qtype, retval);
																					if (PVD_qtype->isReferenceType()) {
																						if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
																							auto& sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr.value();
																							if (1 == (*sublifetime_owners_vlptr).m_maybe_primary_lifetime_owner_infos.size()) {
																								auto maybe_primary_lifetime_owner_info = (*sublifetime_owners_vlptr).m_maybe_primary_lifetime_owner_infos.front();
																								if (maybe_primary_lifetime_owner_info.has_value()) {
																									(*lifetime_owner_info_iter1) = maybe_primary_lifetime_owner_info.value();
																									//sublifetime_owners = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
																								}
																							}
																						}
																					} else {
																						(*lifetime_owner_info_iter1) = maybe_res1.value();
																					}
																				}
																			}
																		}

																	}
																}
															}
														}
													}

												}
											}
										}
									}
									/*
									auto maybe_res2 = lower_bound_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
									if (maybe_res2.has_value()) {
										retval.value().m_maybe_sublifetime_owners_vlptr = { maybe_res2.value() };
									}
									*/
									return retval;
								}
							}
						}
					}
					return retval;
				} else if ((clang::StorageDuration::SD_Static == storage_duration)) {
					const auto VDSR = VD->getSourceRange();
					if (filtered_out_by_location(Ctx.getSourceManager(), VDSR.getBegin())) {
						/* This is a static variable that looks like it's declared in a standard
						or system header. This might include things like 'std::cout'. We'll just
						assume that they've been implemented to be safely directly accessible from
						different threads. */
						satisfies_checks = true;
						retval = VD;
						return retval;
					} else {
						const auto VD_qtype = VD->getType();
						IF_DEBUG(const auto VD_qtype_str = VD_qtype.getAsString();)
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);
						if (is_raw_pointer_or_equivalent(VD_qtype)) {
							const auto pointee_VD_qtype = pointee_type_if_any(VD_qtype).value();
							IF_DEBUG(const auto pointee_VD_qtype_str = pointee_VD_qtype.getAsString();)
							if (pointee_VD_qtype.isConstQualified() && is_async_shareable(pointee_VD_qtype)) {
								/* This case includes "C"-string literals. */
								satisfies_checks = true;
								retval = VD;
								return retval;
							}
						} else if (VD_qtype.isConstQualified() && is_async_shareable(VD_qtype)) {
							satisfies_checks = true;
							retval = VD;
							return retval;
						}
						if (!satisfies_checks) {
							std::string hint_str1 = std::string("'") + VD->getNameAsString()
								+ "' (of type '" + VD_qtype.getAsString()
								+ "') has 'static' storage duration and so may be accessible from different "
								+ "threads. In this case, data race safety could not be verified. If accessibility "
								+ "from different threads is not required, consider declaring the object "
								+ "'thread_local'.";
							retval.m_hints.push_back(hint_str1);
						}
					}
				}
			}
		}
		if (CXXTE) {
			retval = CXXTE;
			auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_if_any(CXXTE, Ctx);
			if (maybe_abstract_lifetime_set) {
				CScopeLifetimeInfo1 sli1;
				sli1.m_maybe_abstract_lifetime = maybe_abstract_lifetime_set.value();
				sli1.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
				const auto CS = Tget_containing_element_of_type<clang::CompoundStmt>(CXXTE, Ctx);
				if (CS) {
					sli1.m_maybe_containing_scope = CS;
				}
				sli1.m_maybe_source_range = CXXTE->getSourceRange();
				retval = CStaticLifetimeOwnerInfo1{ sli1 };
			}
		} else if (SL) {
			retval = SL;
		} else {
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME) {
				const auto VLD = ME->getMemberDecl();
				auto FD = dyn_cast<const clang::FieldDecl>(VLD);
				auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
				if (FD && !(ME->isBoundMemberFunction(Ctx))) {
					auto parent_RD = FD->getParent();
					auto found_iter = tu_state_ref.m_fielddecl_to_abstract_lifetime_map.find(FD);
					if (tu_state_ref.m_fielddecl_to_abstract_lifetime_map.end() != found_iter) {
						const auto FD_qtype = FD->getType();
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL(FD_qtype, retval);
						if (FD_qtype->isReferenceType()) {
							if (1 <= (*found_iter).second.m_primary_lifetimes.size()) {
								CScopeLifetimeInfo1 sli1;
								auto abstract_lifetime = (*found_iter).second.first_lifetime();

								/*
								sli1.m_maybe_abstract_lifetime_set = (*found_iter).second;
								sli1.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
								const auto CS = Tget_containing_element_of_type<clang::CompoundStmt>(ME, Ctx);
								if (CS) {
									sli1.m_maybe_containing_scope = CS;
								}
								sli1.m_maybe_source_range = ME->getSourceRange();
								retval = CStaticLifetimeOwnerInfo1{ sli1 };
								return retval;
								*/
							}
						} else {
							int q = 5;
						}
					}

					auto containing_ref_EX = containing_object_ref_expr_from_member_expr(ME);
					retval = lower_bound_lifetime_owner_if_available(containing_ref_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
					if (retval.has_value()) {
						/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
						retval.value().m_possession_lifetime_info_chain.push_back({ FD->getSourceRange() });
					}
					return retval;
				} else if (VD) {
					retval = VD; /* static member */
					return retval;
				} else {
					int q = 5;
				}
			}
			{
				auto UO = dyn_cast<const clang::UnaryOperator>(EX);
				if (UO) {
					const auto opcode = UO->getOpcode();
					IF_DEBUG(const auto opcode_str = UO->getOpcodeStr(opcode);)
					if (clang::UnaryOperator::Opcode::UO_Deref == opcode) {
						const auto UOSE = UO->getSubExpr();
						if (UOSE) {
							const auto UOSE_qtype = UOSE->getType();
							IF_DEBUG(const auto UOSE_qtype_str = UOSE_qtype.getAsString();)
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL(UOSE_qtype, retval);

							if (is_raw_pointer_or_equivalent(UOSE_qtype)) {
								/* The declrefexpression is a direct dereference of a native pointer. */
								auto maybe_slo1 = lower_bound_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (maybe_slo1.has_value()) {
									auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);
									auto& sublifetimes = sloi1.m_sublifetimes_vlptr->m_primary_lifetime_infos;
									if (sublifetimes.size() == 1) {
										retval = sublifetimes.front();
									} else {
										retval = maybe_slo1;
										/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
										retval.value().m_possession_lifetime_info_chain.push_back({});
									}
								}
							}
						}
					} else if (clang::UnaryOperator::Opcode::UO_AddrOf == opcode) {
						const auto UOSE = UO->getSubExpr();
						if (UOSE) {
							const auto UOSE_qtype = UOSE->getType();
							IF_DEBUG(const auto UOSE_qtype_str = UOSE_qtype.getAsString();)
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL(UOSE_qtype, retval);

							auto maybe_slo1 = lower_bound_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
							if (maybe_slo1.has_value()) {
								auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);

								auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
								expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
								auto sublifetimes2 = CScopeLifetimeInfo1Set{};
								sublifetimes2.m_primary_lifetime_infos.push_back(sloi1);
								*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = sublifetimes2;
								retval = expr_scope_lifetime_info;
							}
						}
					}

				} else {
					auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(EX);
					auto CO = dyn_cast<const clang::ConditionalOperator>(EX);
					if (CXXCE) {
						const auto qtype = CXXCE->getType();
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL(qtype, retval);
						const auto CXXCE_rw_type_ptr = remove_mse_transparent_wrappers(qtype).getTypePtr();
						if (is_raw_pointer_or_equivalent(qtype)) {
							const auto numArgs = CXXCE->getNumArgs();
							if (1 == CXXCE->getNumArgs()) {
								const auto arg_EX = CXXCE->getArg(0);
								assert(arg_EX);

								retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (retval.has_value()) {
									/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
									retval.value().m_possession_lifetime_info_chain.push_back({});
								}
							}
						}
					} else if (CO) {
						auto res1 = lower_bound_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
						auto res2 = lower_bound_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
						return lower_bound_lifetime_owner(res1, res2, Ctx, tu_state_ref);
					} else {
						const clang::Expr* potential_owner_EX = nullptr;
						auto CE = dyn_cast<const clang::CallExpr>(EX);
						if (CE) {
							auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(EX);
							auto CXXMCE = dyn_cast<const clang::CXXMemberCallExpr>(EX);

							bool rv_lifetime_annotation_is_present = false;
							auto FD = CE->getDirectCallee();
							if (FD) {
								auto function_qname = FD->getQualifiedNameAsString();

								static const std::string std_addressof_str = "std::addressof";
								if ((std_addressof_str == function_qname) && (1 == CE->getNumArgs())) {
									retval = lower_bound_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
									return retval;
								}
								if (true) {
									const clang::FunctionDecl* function_decl = CE->getDirectCallee();
									if (function_decl) {
										function_call_handler2(tu_state_ref, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr);
										auto eltv_iter = tu_state_ref.m_expr_lifetime_values_map.find(CE);
										if (tu_state_ref.m_expr_lifetime_values_map.end() != eltv_iter) {
											auto const& esli = eltv_iter->second.m_scope_lifetime_info;
											retval = eltv_iter->second.m_scope_lifetime_info;

											auto sli_of_hypothetical_variable = [](CTUState& state1, clang::CallExpr const * CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
												CScopeLifetimeInfo1 retval;
												CScopeLifetimeInfo1 scope_lifetime_info1;
												//*(scope_lifetime_info1.m_sublifetimes_vlptr) = maybe_abstract_lifetime_set.value();
												scope_lifetime_info1.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
												scope_lifetime_info1.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
												scope_lifetime_info1.m_maybe_source_range = CE->getSourceRange();
												retval = scope_lifetime_info1;

												auto maybe_tlta_ptr = type_lifetime_annotations_if_available(*CE, state1, MR_ptr, Rewrite_ptr);
												if (maybe_tlta_ptr.has_value()) {
													auto& tlta = *(maybe_tlta_ptr.value());
													auto& alts = tlta.m_lifetime_set;
													auto slis = CScopeLifetimeInfo1Set { alts };
													auto replace_lifetime = [& scope_lifetime_info1](CScopeLifetimeInfo1& sli) {
														sli = scope_lifetime_info1;
													};
													slis.apply_to_all_lifetimes(replace_lifetime);
													*(retval.m_sublifetimes_vlptr) = slis;
												}
												auto CE_qtype = remove_mse_transparent_wrappers(CE->getType());
												IF_DEBUG(auto CE_qtype_str = CE_qtype.getAsString();)
												MSE_RETURN_VALUE_IF_TYPE_IS_NULL(CE_qtype, retval);
												slti_set_default_lower_bound_lifetimes_where_needed(retval, CE_qtype);

												return retval;
											};

											auto sli_hv = sli_of_hypothetical_variable(tu_state_ref, CE, Ctx, MR_ptr, Rewrite_ptr);
											if (slti_second_can_be_assigned_to_first(sli_hv, esli, Ctx, tu_state_ref)) {
												return retval;
											} else {
												/* It seems that we got kind of a "disappointing" lifetime lower bound. So we'll give the
												"older" code a chance to come up with a better one, as it may identify certain "special cases"
												that the "newer" code doesn't. */
											}
										} else {
											retval = CE;
										}
									} else {
										retval = CE;
									}

								//} else {

									process_function_lifetime_annotations(*FD, tu_state_ref);
									auto flta_iter = tu_state_ref.m_function_lifetime_annotations_map.find(FD);
									if (tu_state_ref.m_function_lifetime_annotations_map.end() != flta_iter) {
										auto flta = flta_iter->second;
										if (1 == flta.m_return_value_lifetimes.m_primary_lifetimes.size()) {
											const auto rv_lifetime = flta.m_return_value_lifetimes.m_primary_lifetimes.front();
											/* A list of function call arguments that correspond to the parameters that are
											specified (via annotations) to have the same lifetime as the return value. */
											std::vector<clang::Expr const *> potential_rv_source_args;

											IF_DEBUG(auto num_args = CE->getNumArgs();)
											auto arg1_iter = CE->arg_begin();
											const auto arg1_end = CE->arg_end();
											for (const auto& param1 : FD->parameters()) {
												if (arg1_end == arg1_iter) {
													break;
												}
												auto maybe_abstract_lifetime_set1 = tu_state_ref.corresponding_abstract_lifetime_set_if_any(param1);
												if (maybe_abstract_lifetime_set1.has_value()) {
													const auto abstract_lifetime1 = maybe_abstract_lifetime_set1.value();
													if (abstract_lifetime1 == rv_lifetime) {
														potential_rv_source_args.push_back(*arg1_iter);
													}
												}
												++arg1_iter;
											}
											if (CXXMCE) {
												auto this_lifetime_iter = flta.m_param_lifetime_map.find(IMPLICIT_THIS_PARAM_ORDINAL);
												if (flta.m_param_lifetime_map.end() != this_lifetime_iter) {
													const auto abstract_lifetime1 = this_lifetime_iter->second;
													if (abstract_lifetime1 == rv_lifetime) {
														auto this_arg_EX = CXXMCE->getImplicitObjectArgument();
														potential_rv_source_args.push_back(this_arg_EX);
													}
												}
											}

											if (1 <= potential_rv_source_args.size()) {
												CMaybeStaticLifetimeOwnerWithHints maybe_lblo = lower_bound_lifetime_owner_if_available(potential_rv_source_args.front(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
												for (auto arg_EX : potential_rv_source_args) {
													auto maybe_lblo2 = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
													/* We assign to maybe_lblo the "lesser" of itself and maybe_lblo2 */
													maybe_lblo = lower_bound_lifetime_owner(maybe_lblo, maybe_lblo2, Ctx, tu_state_ref);
												}
												if (maybe_lblo.has_value()) {
													return maybe_lblo.value();
												}
											}

										}
									}
								}
							}

							if (CXXOCE) {
								static const std::string operator_star_str = "operator*";
								static const std::string operator_arrow_str = "operator->";
								static const std::string operator_subscript_str = "operator[]";
								auto operator_fdecl = CXXOCE->getDirectCallee();
								std::string operator_name;
								if (operator_fdecl) {
									operator_name = operator_fdecl->getNameAsString();
								} else {
									int q = 3;
								}

								if (((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
									auto arg_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
									if (arg_EX) {
										const auto arg_EX_qtype = arg_EX->getType();
										IF_DEBUG(const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();)
										MSE_RETURN_VALUE_IF_TYPE_IS_NULL(arg_EX_qtype, retval);

										const auto CXXRD = remove_mse_transparent_wrappers(arg_EX_qtype).getTypePtr()->getAsCXXRecordDecl();
										if (CXXRD) {
											DECLARE_CACHED_CONST_STRING(xscope_f_ptr_str, mse_namespace_str() + "::TXScopeFixedPointer");
											DECLARE_CACHED_CONST_STRING(xscope_f_const_ptr_str, mse_namespace_str() + "::TXScopeFixedConstPointer");
											DECLARE_CACHED_CONST_STRING(xscope_obj_f_ptr_str, mse_namespace_str() + "::TXScopeObjFixedPointer");
											DECLARE_CACHED_CONST_STRING(xscope_obj_f_const_ptr_str, mse_namespace_str() + "::TXScopeObjFixedConstPointer");
											DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");

											DECLARE_CACHED_CONST_STRING(rsv_xscope_f_ptr_str, mse_namespace_str() + "::rsv::TXScopeFixedPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_f_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeFixedConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_f_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjFixedPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_f_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjFixedConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_nn_ptr_str, mse_namespace_str() + "::rsv::TXScopeNotNullPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_nn_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeNotNullConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_nn_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjNotNullPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_nn_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjNotNullConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_ptr_str, mse_namespace_str() + "::rsv::TXScopePointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_obj_const_ptr_str, mse_namespace_str() + "::rsv::TXScopeObjConstPointer");
											DECLARE_CACHED_CONST_STRING(rsv_xscope_owner_ptr_str, mse_namespace_str() + "::rsv::TXScopeOwnerPointer");

											DECLARE_CACHED_CONST_STRING(reg_proxy_ptr_str, mse_namespace_str() + "::TRegisteredProxyPointer");
											DECLARE_CACHED_CONST_STRING(reg_proxy_const_ptr_str, mse_namespace_str() + "::TRegisteredProxyConstPointer");
											DECLARE_CACHED_CONST_STRING(norad_proxy_ptr_str, mse_namespace_str() + "::TNoradProxyPointer");
											DECLARE_CACHED_CONST_STRING(norad_proxy_const_ptr_str, mse_namespace_str() + "::TNoradProxyConstPointer");
											static const std::string unique_ptr_str = "std::unique_ptr";
											auto qname = CXXRD->getQualifiedNameAsString();
											if ((xscope_f_ptr_str == qname) || (xscope_f_const_ptr_str == qname)
												|| (xscope_obj_f_ptr_str == qname) || (xscope_obj_f_const_ptr_str == qname)

												|| (rsv_xscope_f_ptr_str == qname) || (rsv_xscope_f_const_ptr_str == qname)
												|| (rsv_xscope_obj_f_ptr_str == qname) || (rsv_xscope_obj_f_const_ptr_str == qname)
												|| (rsv_xscope_nn_ptr_str == qname) || (rsv_xscope_nn_const_ptr_str == qname)
												|| (rsv_xscope_obj_nn_ptr_str == qname) || (rsv_xscope_obj_nn_const_ptr_str == qname)
												|| (rsv_xscope_ptr_str == qname) || (rsv_xscope_const_ptr_str == qname)
												|| (rsv_xscope_obj_ptr_str == qname) || (rsv_xscope_obj_const_ptr_str == qname)

												|| (reg_proxy_ptr_str == qname) || (reg_proxy_const_ptr_str == qname)
												|| (norad_proxy_ptr_str == qname) || (norad_proxy_const_ptr_str == qname)
												/*|| ((xscope_owner_ptr_str == qname) && ())*/) {
												satisfies_checks = true;
												retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
												if (!retval.has_value()) {
													retval = dyn_cast<const clang::Expr>(arg_EX);
												}
												if (retval.has_value()) {
													/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
													retval.value().m_possession_lifetime_info_chain.push_back({});
												}
												return retval;
											} else if (unique_ptr_str == qname) {
												if (arg_EX_qtype.isConstQualified()) {
													/* We're treating `const std::unique_ptr<>`s as similar to mse::TXScopeOwnerPointer<>s. */
													if (arg_EX->isLValue()) {
														satisfies_checks = true;
														retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
														if (!retval.has_value()) {
															retval = dyn_cast<const clang::Expr>(arg_EX);
														}
														if (retval.has_value()) {
															/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
															retval.value().m_possession_lifetime_info_chain.push_back({});
														}
														return retval;
													}
												}
											}
										} else if (arg_EX_qtype->isReferenceType()) {
											int q = 5;
										}
									}
								}

								if ((((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs()))
									|| (((operator_subscript_str == operator_name)) && (2 == CXXOCE->getNumArgs()))
									) {
									potential_owner_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
								}
							} else if (CXXMCE) {
								static const std::string method_value_str = "value";
								static const std::string method_at_str = "at";
								static const std::string method_front_str = "front";
								static const std::string method_back_str = "back";
								auto method_decl = CXXMCE->getDirectCallee();
								std::string method_name;
								if (method_decl) {
									method_name = method_decl->getNameAsString();
								} else {
									int q = 3;
								}

								if ((((method_value_str == method_name)) && (0 == CXXMCE->getNumArgs()))
									|| (((method_at_str == method_name)) && (1 == CXXMCE->getNumArgs()))
									|| (((method_front_str == method_name)) && (0 == CXXMCE->getNumArgs()))
									|| (((method_back_str == method_name)) && (0 == CXXMCE->getNumArgs()))
									) {
									potential_owner_EX = IgnoreParenImpNoopCasts(CXXMCE->getImplicitObjectArgument(), Ctx);
								} else {
									const auto CXXMCE_qtype = CXXMCE->getType();
									MSE_RETURN_VALUE_IF_TYPE_IS_NULL(CXXMCE_qtype, retval);
									if (contains_non_owning_scope_reference(CXXMCE_qtype, tu_state_ref)) {
										auto maybe_lb_lifetime_owner = lower_bound_lifetime_owner_of_returned_reference_object_if_available(CXXMCE, Ctx, tu_state_ref);
										if (maybe_lb_lifetime_owner.has_value()) {
											return maybe_lb_lifetime_owner;
										}
									}
								}
							} else if (CE) {
								auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();

								static const std::string std_move_str = "std::move";
								if ((std_move_str == function_qname) && (1 == CE->getNumArgs())) {
									return lower_bound_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								}

								static const std::string function_get_str = "std::get";
								if (((function_get_str == function_qname)) && (1 == CE->getNumArgs())) {
									potential_owner_EX = IgnoreParenImpNoopCasts(CE->getArg(0), Ctx);
								} else {
									const auto CE_qtype = CE->getType();
									MSE_RETURN_VALUE_IF_TYPE_IS_NULL(CE_qtype, retval);
									if (contains_non_owning_scope_reference(CE_qtype, tu_state_ref)) {
										auto maybe_lb_lifetime_owner = lower_bound_lifetime_owner_of_returned_reference_object_if_available(CE, Ctx, tu_state_ref);
										if (maybe_lb_lifetime_owner.has_value()) {
											return maybe_lb_lifetime_owner;
										}
									}
								}
							}
						}
						if (potential_owner_EX) {
							auto potential_owner_EX_ii = IgnoreParenImpNoopCasts(potential_owner_EX, Ctx);
							if (potential_owner_EX_ii) {
								const auto potential_owner_EX_ii_qtype = potential_owner_EX_ii->getType();
								IF_DEBUG(const auto potential_owner_EX_ii_qtype_str = potential_owner_EX_ii_qtype.getAsString();)
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL(potential_owner_EX_ii_qtype, retval);

								const auto CXXRD = remove_mse_transparent_wrappers(potential_owner_EX_ii_qtype).getTypePtr()->getAsCXXRecordDecl();
								if (CXXRD) {
									/* static structure containers */
									DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");
									DECLARE_CACHED_CONST_STRING(xscope_tuple_str, mse_namespace_str() + "::xscope_tuple");

									static const std::string std_unique_ptr_str = "std::unique_ptr";
									static const std::string std_tuple_str = "std::tuple";
									static const std::string std_pair_str = "std::pair";
									static const std::string std_array_str = "std::array";

									DECLARE_CACHED_CONST_STRING(mstd_tuple_str, mse_namespace_str() + "::mstd::tuple");
									DECLARE_CACHED_CONST_STRING(nii_array_str, mse_namespace_str() + "::nii_array");
									DECLARE_CACHED_CONST_STRING(mstd_array_str, mse_namespace_str() + "::mstd::array");
									DECLARE_CACHED_CONST_STRING(xscope_nii_array_str, mse_namespace_str() + "::xscope_nii_array");
									DECLARE_CACHED_CONST_STRING(fixed_nii_vector_str, mse_namespace_str() + "::fixed_nii_vector");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_vector_str, mse_namespace_str() + "::xscope_fixed_nii_vector");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_vector_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_vector");
									DECLARE_CACHED_CONST_STRING(fixed_nii_basic_string_str, mse_namespace_str() + "::fixed_nii_basic_string");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_basic_string_str, mse_namespace_str() + "::xscope_fixed_nii_basic_string");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_basic_string_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_basic_string");
									DECLARE_CACHED_CONST_STRING(fixed_optional_str, mse_namespace_str() + "::fixed_optional");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_optional_str, mse_namespace_str() + "::xscope_fixed_optional");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_optional_str, mse_namespace_str() + "::xscope_borrowing_fixed_optional");
									DECLARE_CACHED_CONST_STRING(fixed_any_str, mse_namespace_str() + "::fixed_any");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_any_str, mse_namespace_str() + "::xscope_fixed_any");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_any_str, mse_namespace_str() + "::xscope_borrowing_fixed_any");

									auto qname = CXXRD->getQualifiedNameAsString();
									if ((xscope_owner_ptr_str == qname)
										|| (fixed_optional_str == qname) || (xscope_fixed_optional_str == qname) || (xscope_borrowing_fixed_optional_str == qname)
										|| (fixed_any_str == qname) || (xscope_fixed_any_str == qname) || (xscope_borrowing_fixed_any_str == qname)
										) {
										retval = lower_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
										if (retval.has_value()) {
											/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
											retval.value().m_possession_lifetime_info_chain.push_back({});
										}
									} else if ((xscope_tuple_str == qname)
										|| (std_tuple_str == qname) || (std_pair_str == qname) || (std_array_str == qname)

										|| (mstd_tuple_str == qname) || (nii_array_str == qname) || (mstd_array_str == qname) || (xscope_nii_array_str == qname)
										|| (fixed_nii_vector_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
										|| (fixed_nii_basic_string_str == qname) || (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)
										) {
										retval = lower_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
										if (retval.has_value()) {
											/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
											retval.value().m_possession_lifetime_info_chain.push_back({ {}, CPossessionLifetimeInfo1::is_element_in_a_multi_element_container_t::Yes });
										}
									}
								} else if (potential_owner_EX_ii_qtype->isReferenceType()) {
									int q = 5;
								}
							}
						}
					}
				}
			}
		}

		return retval;
	}

	std::optional<CStaticLifetimeOwnerInfo1> lower_bound_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;
		if (!EX1) {
			return retval;
		}
		const auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);
		bool satisfies_checks = false;

		auto CXXMCE = dyn_cast<const clang::CXXMemberCallExpr>(EX);
		auto CE = dyn_cast<const clang::CallExpr>(EX);
		if (CXXMCE) {
			IF_DEBUG(auto function_qname = CXXMCE->getDirectCallee()->getQualifiedNameAsString();)

			/* So the idea here is that since a returned scope pointer/reference object cannot target a
			local variable inside the member function (including parameter local variables), then it must
			refer to a (thread_local or static object or an) object targeted by a scope pointer/reference
			object passed as an argument to the function, or an object targeted by a member (or the object
			itself). Therefore we conclude that the target(s) of the returned scope pointer/reference
			object live at least as long as the shortest-lived of the scope pointer/reference objects
			passed as an argument to the function or the object itself. */

			std::vector<std::optional<CStaticLifetimeOwnerInfo1> > lifetime_owners;
			for (size_t i = 0; i < CXXMCE->getNumArgs(); i+=1) {
				const auto arg_EX_ii = IgnoreParenImpNoopCasts(CXXMCE->getArg(i), Ctx);
				const auto arg_EX_ii_qtype = arg_EX_ii->getType();
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL(arg_EX_ii_qtype, retval);
				if (contains_non_owning_scope_reference(arg_EX_ii_qtype, tu_state_ref)) {
					lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CXXMCE->getArg(i), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
				}
			}
			lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CXXMCE->getImplicitObjectArgument(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
			retval = lower_bound_lifetime_owner(lifetime_owners, Ctx, tu_state_ref);
		} else if (CE) {
			IF_DEBUG(auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();)

			/* So the idea here is that since a returned scope pointer/reference object cannot target a
			local variable inside the (free) function (including parameter local variables), then it must
			refer to a (thread_local or static object or an) object targeted by a scope pointer/reference
			object passed as an argument to the function. Therefore we conclude that the target(s) of the
			returned scope pointer/reference object live at least as long as the shortest-lived scope
			pointer/reference object passed as an argument to the function. */

			std::vector<std::optional<CStaticLifetimeOwnerInfo1> > lifetime_owners;
			for (size_t i = 0; i < CE->getNumArgs(); i+=1) {
				const auto arg_EX_ii = IgnoreParenImpNoopCasts(CE->getArg(i), Ctx);
				const auto arg_EX_ii_qtype = arg_EX_ii->getType();
				if (contains_non_owning_scope_reference(arg_EX_ii_qtype, tu_state_ref)) {
					lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CE->getArg(i), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
				}
			}
			if (0 == lifetime_owners.size()) {
				/* If no scope pointer/reference objects are passed to the function, then the returned
				scope pointer/reference object must be targeting thread_local or static (duration)
				object(s). (Right?) */
				CScopeLifetimeInfo1 scope_lifetime_info_obj;
				scope_lifetime_info_obj.m_category = CScopeLifetimeInfo1::ECategory::Immortal;

				retval = scope_lifetime_info_obj;
			} else {
				retval = lower_bound_lifetime_owner(lifetime_owners, Ctx, tu_state_ref);
			}
		}

		return retval;
	}

	/* Similar to `lower_bound_lifetime_owner_if_available()`, this function is meant to return the part of a given
	expression that directly refers to the declared object (i.e. the `DeclRefExpr`) of interest, if such an
	object is present. Unlike lower_bound_lifetime_owner_if_available(), the given expression is presumed to
	indicate the (scope) reference/pointer object to be retargeted, rather than the target object. So the
	object of interest in this case is the one from which we can infer the lifetime (or an upper bound of the
	lifetime) of the reference object to be retargeted. If the indicated reference object is itself a declared
	object (as opposed to, for example, a member of another object, or an element in a container), then it
	itself would be the object of interest. */
	CMaybeStaticLifetimeOwnerWithHints upper_bound_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		CMaybeStaticLifetimeOwnerWithHints retval;
		if (!EX1) {
			return retval;
		}
		const auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);

		if (!(retval.has_value())) {
			auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(tu_state_ref, EX, Ctx, MR_ptr, Rewrite_ptr);
			if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
				/* Cannot properly evaluate because this is a template definition. Proper evaluation should
				occur in any instantiation of the template. */
			} else if (maybe_expr_lifetime_value.has_value()) {
				CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
				if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == expr_slti.m_category) {
					/* If the lifetime of a pointer's target is abstract, it can be used as the ("upper bound")
					lifetime of the lhs of a (pointer) assignment. Right? */
					retval = expr_slti;
					return retval;
				}
			}
		}

		bool satisfies_checks = false;
		auto UO = dyn_cast<const clang::UnaryOperator>(EX);
		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		auto CXXTE = dyn_cast<const clang::CXXThisExpr>(EX);
		if (UO) {
			const auto opcode = UO->getOpcode();
			IF_DEBUG(const auto opcode_str = UO->getOpcodeStr(opcode);)
			if (clang::UnaryOperator::Opcode::UO_AddrOf == opcode) {
				const auto UOSE = UO->getSubExpr();
				if (UOSE) {
					const auto UOSE = UO->getSubExpr();
					if (UOSE) {
						const auto UOSE_qtype = UOSE->getType();
						IF_DEBUG(const auto UOSE_qtype_str = UOSE_qtype.getAsString();)

						auto maybe_slo1 = upper_bound_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref);
						if (maybe_slo1.has_value()) {
							auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);

							auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
							expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
							auto sublifetimes2 = CScopeLifetimeInfo1Set{};
							sublifetimes2.m_primary_lifetime_infos.push_back(sloi1);
							*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = sublifetimes2;
							retval = expr_scope_lifetime_info;
						}
					}
				}
			} else if (clang::UnaryOperator::Opcode::UO_Deref == opcode) {
				const auto UOSE = UO->getSubExpr();
				if (UOSE) {
					const auto UOSE_qtype = UOSE->getType();
					IF_DEBUG(const auto UOSE_qtype_str = UOSE_qtype.getAsString();)

					if (is_raw_pointer_or_equivalent(UOSE->getType())) {
						/* The declrefexpression is a direct dereference of a native pointer. */
						auto UOSE_ii = IgnoreParenImpNoopCasts(UOSE, Ctx);
						auto DRE2 = dyn_cast<const clang::DeclRefExpr>(UOSE_ii);
						if (DRE2) {
							auto VLD = DRE2->getDecl();
							auto VD = dyn_cast<const clang::VarDecl>(VLD);
							if (VD) {
								auto maybe_decl_lifetime_value = evaluate_declaration_lower_bound_lifetimes(tu_state_ref, VD, Ctx, MR_ptr, Rewrite_ptr);
								if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
									/* Cannot properly evaluate because this is a template definition. Proper evaluation should
									occur in any instantiation of the template. */
									//retval.m_failure_due_to_dependent_type_flag = true;
									//return retval;
								} else if (maybe_decl_lifetime_value.has_value()) {
									CScopeLifetimeInfo1& decl_slti = maybe_decl_lifetime_value.value().m_scope_lifetime_info;
									if (1 == decl_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) {
										auto subsli = decl_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.front();
										if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == subsli.m_category) {
											/* If the lifetime of a pointer's target is abstract, it can be used as the ("upper bound")
											lifetime of the lhs of a (pointer) assignment. Right? */
											retval = subsli;
											return retval;
										} else if (VD->getType().isConstQualified()) {
											/* If the pointer itself was actually declared const (as opposed to being referred to by const
											reference), then its target never changes, so the upper bound lifetime of its target will just
											the value of target's lifetime evaluated at any point. */
											retval = subsli;
											return retval;
										}
									}
								} else if (VD->getType().isConstQualified()) {
									/* If the pointer itself was actually declared const (as opposed to being referred to by const
									reference), then its target never changes, so the upper bound lifetime of its target will just
									the lifetime of the target it was initialized with. */
									if (VD->hasInit()) {
										auto init_E = VD->getInit();
										if (init_E) {
											retval = upper_bound_lifetime_owner_if_available(init_E, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
											if (retval.has_value()) {
												return retval;
											}
										}
									}
								}
							}
						}



						auto maybe_slo1 = lower_bound_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
						if (maybe_slo1.has_value()) {
							auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);
							auto& sublifetimes = sloi1.m_sublifetimes_vlptr->m_primary_lifetime_infos;
							if (sublifetimes.size() == 1) {
								retval = sublifetimes.front();
							} else {
								retval = maybe_slo1;
								/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
								retval.value().m_possession_lifetime_info_chain.push_back({});
							}
						}
					}
				}
			}
		}
		if (DRE1) {
			const auto DRE1_qtype = DRE1->getType();
			IF_DEBUG(const auto DRE1_qtype_str = DRE1_qtype.getAsString();)

			auto D1 = DRE1->getDecl();
			const auto D1_qtype = D1->getType();
			IF_DEBUG(const auto D1_qtype_str = D1_qtype.getAsString();)

			auto VD = dyn_cast<const clang::VarDecl>(D1);
			if (VD) {
				const auto VD_qtype = VD->getType();
				IF_DEBUG(const auto VD_qtype_str = VD_qtype.getAsString();)

				const auto storage_duration = VD->getStorageDuration();
				if ((clang::StorageDuration::SD_Automatic == storage_duration)
					|| (clang::StorageDuration::SD_Thread == storage_duration)
					) {
					auto VD_qtype = VD->getType();
					IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);
					if (VD_qtype->isReferenceType()) {
						satisfies_checks = false;
						retval = {};
						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							process_function_lifetime_annotations(*PVD, tu_state_ref);
							auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(PVD);
							if (maybe_abstract_lifetime_set.has_value()) {
								auto& alts1 = maybe_abstract_lifetime_set.value();
								if (1 <= alts1.m_primary_lifetimes.size()) {
									auto& alt = alts1.m_primary_lifetimes.front();
									CScopeLifetimeInfo1 scope_lifetime_info1(alt);

									scope_lifetime_info1.m_maybe_containing_scope = get_containing_scope(PVD, Ctx);
									scope_lifetime_info1.m_maybe_source_range = PVD->getSourceRange();

									retval = scope_lifetime_info1;
								} else {
									/* unexpected? */
									int q = 3;
								}
	
								if (PVD->hasInit()) {
									/* todo: error: we currently do not support default initializers on parameters with lifetime
									annotations. */
									int q = 3;
								}
							}
							return retval;
						} else {
							if (VD->hasInit()) {
								retval = upper_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref);
								return retval;
							}
						}
					} else {
						retval = VD;

						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							process_function_lifetime_annotations(*PVD, tu_state_ref);
							auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(PVD);
							if (maybe_abstract_lifetime_set.has_value()) {
								CScopeLifetimeInfo1 scope_lifetime_info1;
								*(scope_lifetime_info1.m_sublifetimes_vlptr) = maybe_abstract_lifetime_set.value();
								if (clang::StorageDuration::SD_Thread == storage_duration) {
									scope_lifetime_info1.m_category = CScopeLifetimeInfo1::ECategory::Immortal;
								} else {
									assert(clang::StorageDuration::SD_Automatic == storage_duration);
									scope_lifetime_info1.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
								}

								scope_lifetime_info1.m_maybe_containing_scope = get_containing_scope(PVD, Ctx);
								scope_lifetime_info1.m_maybe_source_range = PVD->getSourceRange();

								retval = scope_lifetime_info1;

								if (PVD->hasInit()) {
									/* todo: error: we currently do not support default initializers on parameters with lifetime
									annotations. */
									int q = 3;
								}
							}
							return retval;
						}

						auto VD_qtype = VD->getType();
						IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);
						if (VD_qtype->isPointerType()) {
							if (VD->hasInit()) {
								auto maybe_res1 = lower_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (maybe_res1.has_value()) {
									if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
										retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
										return retval;
									}
								}
								auto maybe_res2 = lower_bound_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
								if (maybe_res2.has_value()) {
									CStaticLifetimeOwnerInfo1Set sloi1_set;
									sloi1_set.m_maybe_primary_lifetime_owner_infos.push_back(maybe_res2.value());
									retval.value().m_maybe_sublifetime_owners_vlptr = { sloi1_set };
								}
								return retval;
							}
						}

						auto RD = VD->getType().getTypePtr()->getAsRecordDecl();
						if (RD) {
							auto found_iter = tu_state_ref.m_type_lifetime_annotations_map.find(RD->getTypeForDecl());
							if (tu_state_ref.m_type_lifetime_annotations_map.end() != found_iter) {
								auto& tlta = found_iter->second;
								if (VD->hasInit()) {
									auto maybe_res1 = lower_bound_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
									if (maybe_res1.has_value()) {
										if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
											retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
											return retval;
										}
									}
									auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(VD->getInit());
									auto CXXTOE = dyn_cast<const clang::CXXTemporaryObjectExpr>(VD->getInit());
									if (CXXCE) {
										auto CD = CXXCE->getConstructor();
										if (CD) {
											if (CD->isCopyOrMoveConstructor()) {
												if (1 == CXXCE->getNumArgs()) {
													auto arg_EX = CXXCE->getArg(0);
													auto maybe_res1 = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
													if (maybe_res1.has_value()) {
														if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
															retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
															return retval;
														}
													}
												} else { assert(false); }
											} else {
												process_function_lifetime_annotations(*CD, tu_state_ref);
												auto found_iter2 = tu_state_ref.m_function_lifetime_annotations_map.find(CD);
												if (tu_state_ref.m_function_lifetime_annotations_map.end() != found_iter2) {
													auto& flta = found_iter2->second;
													auto sublifetime_owners = CStaticLifetimeOwnerInfo1Set(tlta.m_lifetime_set);

													auto& CE = CXXCE;
													auto lifetime_owner_info_iter1 = sublifetime_owners.m_maybe_primary_lifetime_owner_infos.begin();
													for (const auto& type_lifetime : tlta.m_lifetime_set.m_primary_lifetimes) {
														for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
															auto& param_lifetime_set = param_lifetime1.second;
															if (1 == param_lifetime_set.m_primary_lifetimes.size()) {
																if (param_lifetime_set.first_lifetime() == type_lifetime) {
																	clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
																	if (!arg1_EX) {
																		continue;
																	} else {
																		auto maybe_res1 = lower_bound_lifetime_owner_if_available(arg1_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
																		if (maybe_res1.has_value()) {
																			auto param_index = int(param_lifetime1.first) - 1;
																			if ((0 <= param_index) && (int(CD->getNumParams()) > param_index)) {
																				auto PVD = CD->getParamDecl(param_index);
																				if (PVD) {
																					auto PVD_qtype = PVD->getType();
																					IF_DEBUG(auto PVD_qtype_str = PVD_qtype.getAsString();)
																					MSE_RETURN_VALUE_IF_TYPE_IS_NULL(PVD_qtype, retval);
																					if (PVD_qtype->isReferenceType()) {
																						if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
																							auto& sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr.value();
																							if (1 == (*sublifetime_owners_vlptr).m_maybe_primary_lifetime_owner_infos.size()) {
																								auto maybe_primary_lifetime_owner_info = (*sublifetime_owners_vlptr).m_maybe_primary_lifetime_owner_infos.front();
																								if (maybe_primary_lifetime_owner_info.has_value()) {
																									(*lifetime_owner_info_iter1) = maybe_primary_lifetime_owner_info.value();
																									//sublifetime_owners = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
																								}
																							}
																						}
																					} else {
																						(*lifetime_owner_info_iter1) = maybe_res1.value();
																					}
																				}
																			}
																		}

																	}
																}
															}
														}
													}

												}
											}
										}
									}
									/*
									auto maybe_res2 = lower_bound_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
									if (maybe_res2.has_value()) {
										retval.value().m_maybe_sublifetime_owners_vlptr = { maybe_res2.value() };
									}
									*/
									return retval;
								}
							}
						}
					}
					return retval;
				} else if ((clang::StorageDuration::SD_Static == storage_duration)) {
					const auto VDSR = VD->getSourceRange();
					if (filtered_out_by_location(Ctx.getSourceManager(), VDSR.getBegin())) {
						/* This is a static variable that looks like it's declared in a standard
						or system header. This might include things like 'std::cout'. We'll just
						assume that they've been implemented to be safely directly accessible from
						different threads. */
						satisfies_checks = true;
						retval = VD;
						return retval;
					} else {
						const auto VD_qtype = VD->getType();
						IF_DEBUG(const auto VD_qtype_str = VD_qtype.getAsString();)
						if (is_raw_pointer_or_equivalent(VD_qtype)) {
							const auto pointee_VD_qtype = pointee_type_if_any(VD_qtype).value();
							IF_DEBUG(const auto pointee_VD_qtype_str = pointee_VD_qtype.getAsString();)
							if (pointee_VD_qtype.isConstQualified() && is_async_shareable(pointee_VD_qtype)) {
								/* This case includes "C"-string literals. */
								satisfies_checks = true;
								retval = VD;
								return retval;
							}
						} else if (VD_qtype.isConstQualified() && is_async_shareable(VD_qtype)) {
							satisfies_checks = true;
							retval = VD;
							return retval;
						}
						if (!satisfies_checks) {
							std::string hint_str1 = std::string("'") + VD->getNameAsString()
								+ "' (of type '" + VD_qtype.getAsString()
								+ "') has 'static' storage duration and so may be accessible from different "
								+ "threads. In this case, data race safety could not be verified. If accessibility "
								+ "from different threads is not required, consider declaring the object "
								+ "'thread_local'.";
							retval.m_hints.push_back(hint_str1);
						}
					}
				}
			}
		} else if (CXXTE) {
			retval = CXXTE;
			auto maybe_abstract_lifetime = tu_state_ref.corresponding_abstract_lifetime_if_any(CXXTE, Ctx);
			if (maybe_abstract_lifetime) {
				CScopeLifetimeInfo1 sli1;
				sli1.m_maybe_abstract_lifetime = maybe_abstract_lifetime.value();
				sli1.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;

				auto FND = enclosing_function_if_any(CXXTE, Ctx);
				if (FND) {
					auto FNST = FND->getBody();
					if (FNST) {
						sli1.m_maybe_containing_scope = get_containing_scope(FNST, Ctx);
						sli1.m_maybe_source_range = FNST->getSourceRange();
					} else {
						sli1.m_maybe_containing_scope = get_containing_scope(CXXTE, Ctx);
						sli1.m_maybe_source_range = CXXTE->getSourceRange();
					}
				}
				retval = CStaticLifetimeOwnerInfo1{ sli1 };
			}
		} else {
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME) {
				const auto VLD = ME->getMemberDecl();
				auto VLD_qtype = VLD->getType();
				IF_DEBUG(auto VLD_qtype_str = VLD_qtype.getAsString();)
				auto FD = dyn_cast<const clang::FieldDecl>(VLD);
				auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
				if (FD && !(ME->isBoundMemberFunction(Ctx))) {
					auto containing_ref_EX = containing_object_ref_expr_from_member_expr(ME);
					retval = upper_bound_lifetime_owner_if_available(containing_ref_EX, Ctx, tu_state_ref);
					if (retval.has_value()) {
						auto& retval_slo_ref = retval.value();

						/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
						retval_slo_ref.m_possession_lifetime_info_chain.push_back({ FD->getSourceRange() });

						process_type_lifetime_annotations(*FD, tu_state_ref);
						auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(FD);
						if (maybe_abstract_lifetime_set.has_value()) {
							auto& abstract_lifetime_set1 = maybe_abstract_lifetime_set.value();
							auto lhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(retval_slo_ref, Ctx, tu_state_ref);
							if (VLD_qtype->isReferenceType()) {
								/* unsupported */
								int q = 3;
							} else {
								if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == lhs_lifetime_info.m_category) {
									assert(lhs_lifetime_info.m_maybe_abstract_lifetime.has_value());
									{
										auto& sublifetimes = *(lhs_lifetime_info.m_sublifetimes_vlptr);
										if (0 == sublifetimes.m_primary_lifetime_infos.size()) {
											sublifetimes = abstract_lifetime_set1;
										} else {
											/* unexpected */
											int q = 3;
										}
									}
								} else {
									/* If the object's lifetime is not abstract, then the associated abstract lifetime should refer to
									the object's "type lifetime(s)". */
									*(lhs_lifetime_info.m_sublifetimes_vlptr) = abstract_lifetime_set1;
								}
								retval = lhs_lifetime_info;
							}

						}
					}


				} else if (VD) {
					retval = VD; /* static member */
				} else {
					int q = 5;
				}
			} else {
				{
					auto CO = dyn_cast<const clang::ConditionalOperator>(EX);
					if (CO) {
						auto res1 = upper_bound_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_ref);
						auto res2 = upper_bound_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_ref);
						return upper_bound_lifetime_owner(res1, res2, Ctx, tu_state_ref);
					} else {
						const clang::Expr* potential_owner_EX = nullptr;
						auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(EX);
						auto CXXMCE = dyn_cast<const clang::CXXMemberCallExpr>(EX);
						auto CE = dyn_cast<const clang::CallExpr>(EX);
						if (CXXOCE) {
							static const std::string operator_star_str = "operator*";
							static const std::string operator_arrow_str = "operator->";
							static const std::string operator_subscript_str = "operator[]";
							auto operator_fdecl = CXXOCE->getDirectCallee();
							std::string operator_name;
							if (operator_fdecl) {
								operator_name = operator_fdecl->getNameAsString();
							} else {
								int q = 3;
							}

							if ((((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs()))
								|| (((operator_subscript_str == operator_name)) && (2 == CXXOCE->getNumArgs()))
								) {
								potential_owner_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
							}
						} else if (CXXMCE) {
							static const std::string method_value_str = "value";
							static const std::string method_at_str = "at";
							static const std::string method_front_str = "front";
							static const std::string method_back_str = "back";
							auto method_decl = CXXMCE->getDirectCallee();
							std::string method_name;
							if (method_decl) {
								method_name = method_decl->getNameAsString();
							} else {
								int q = 3;
							}

							if ((((method_value_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								|| (((method_at_str == method_name)) && (1 == CXXMCE->getNumArgs()))
								|| (((method_front_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								|| (((method_back_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								) {
								potential_owner_EX = IgnoreParenImpNoopCasts(CXXMCE->getImplicitObjectArgument(), Ctx);
							}
						} else if (CE) {
							auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();
							static const std::string function_get_str = "std::get";
							if (((function_get_str == function_qname)) && (1 == CE->getNumArgs())) {
								potential_owner_EX = IgnoreParenImpNoopCasts(CE->getArg(0), Ctx);
							} else {
								static const std::string std_addressof_str = "std::addressof";
								if ((std_addressof_str == function_qname) && (1 == CE->getNumArgs())) {
									retval = upper_bound_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref);
									return retval;
								}
							}
						}
						if (potential_owner_EX) {
							auto potential_owner_EX_ii = IgnoreParenImpNoopCasts(potential_owner_EX, Ctx);
							if (potential_owner_EX_ii) {
								const auto potential_owner_EX_ii_qtype = potential_owner_EX_ii->getType();
								IF_DEBUG(const auto potential_owner_EX_ii_qtype_str = potential_owner_EX_ii_qtype.getAsString();)
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL(potential_owner_EX_ii_qtype, retval);

								const auto CXXRD = remove_mse_transparent_wrappers(potential_owner_EX_ii->getType()).getTypePtr()->getAsCXXRecordDecl();
								if (CXXRD) {
									/* owning containers (that might contain pointer/reference elements) */

									DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");
									DECLARE_CACHED_CONST_STRING(xscope_optional_str, mse_namespace_str() + "::xscope_optional");
									DECLARE_CACHED_CONST_STRING(xscope_tuple_str, mse_namespace_str() + "::xscope_tuple");
									DECLARE_CACHED_CONST_STRING(xscope_nii_array_str, mse_namespace_str() + "::xscope_nii_array");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_vector_str, mse_namespace_str() + "::xscope_fixed_nii_vector");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_vector_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_vector");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_basic_string_str, mse_namespace_str() + "::xscope_fixed_nii_basic_string");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_basic_string_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_basic_string");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_optional_str, mse_namespace_str() + "::xscope_fixed_optional");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_optional_str, mse_namespace_str() + "::xscope_borrowing_fixed_optional");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_any_str, mse_namespace_str() + "::xscope_fixed_any");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_any_str, mse_namespace_str() + "::xscope_borrowing_fixed_any");

									static const std::string std_unique_ptr_str = "std::unique_ptr";
									static const std::string std_shared_ptr_str = "std::shared_ptr";
									static const std::string std_optional_str = "std::optional";
									static const std::string std_tuple_str = "std::tuple";
									static const std::string std_pair_str = "std::pair";
									static const std::string std_array_str = "std::array";
									static const std::string std_vector_str = "std::vector";
									static const std::string std_list_str = "std::list";
									static const std::string std_map_str = "std::map";
									static const std::string std_set_str = "std::set";
									static const std::string std_multimap_str = "std::multimap";
									static const std::string std_multiset_str = "std::multiset";
									static const std::string std_unordered_map_str = "std::unordered_map";
									static const std::string std_unordered_set_str = "std::unordered_set";
									static const std::string std_unordered_multimap_str = "std::unordered_multimap";
									static const std::string std_unordered_multiset_str = "std::unordered_multiset";

									/* Conceptually, the following containers (and others not listed here) should also be
									considered, but since they don't support/allow xscope elements, their elements would
									never factor into the determination of a scope reference's lifetime (upper bound). 
									*/
									/*
									DECLARE_CACHED_CONST_STRING(mstd_optional_str, mse_namespace_str() + "::mstd::optional");
									DECLARE_CACHED_CONST_STRING(mstd_tuple_str, mse_namespace_str() + "::mstd::tuple");
									DECLARE_CACHED_CONST_STRING(nii_array_str, mse_namespace_str() + "::nii_array");
									DECLARE_CACHED_CONST_STRING(mstd_array_str, mse_namespace_str() + "::mstd::array");
									DECLARE_CACHED_CONST_STRING(nii_vector_str, mse_namespace_str() + "::nii_vector");
									DECLARE_CACHED_CONST_STRING(stnii_vector_str, mse_namespace_str() + "::stnii_vector");
									DECLARE_CACHED_CONST_STRING(mtnii_vector_str, mse_namespace_str() + "::mtnii_vector");
									DECLARE_CACHED_CONST_STRING(mstd_vector_str, mse_namespace_str() + "::mstd::vector");
									*/

									auto qname = CXXRD->getQualifiedNameAsString();
									if ((xscope_owner_ptr_str == qname) || (xscope_optional_str == qname)

										|| (std_unique_ptr_str == qname) || (std_shared_ptr_str == qname) || (std_optional_str == qname)
										|| (xscope_fixed_optional_str == qname) || (xscope_borrowing_fixed_optional_str == qname)
										|| (xscope_fixed_any_str == qname) || (xscope_borrowing_fixed_any_str == qname)

										/*
										|| (mstd_optional_str == qname)
										*/
										) {
										retval = upper_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref);
										if (retval.has_value()) {
											/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
											retval.value().m_possession_lifetime_info_chain.push_back({});
										}
									} else if ((xscope_tuple_str == qname)

										|| (std_tuple_str == qname) || (std_pair_str == qname)
										|| (std_array_str == qname) || (std_vector_str == qname) || (std_list_str == qname)
										|| (std_map_str == qname) || (std_set_str == qname) || (std_multimap_str == qname) || (std_multiset_str == qname)
										|| (std_unordered_map_str == qname) || (std_unordered_set_str == qname) || (std_unordered_multimap_str == qname) || (std_unordered_multiset_str == qname)
										|| (xscope_nii_array_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
										|| (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)

										/*
										|| (mstd_tuple_str == qname)
										|| (nii_array_str == qname) || (mstd_array_str == qname)
										|| (nii_vector_str == qname) || (stnii_vector_str == qname) || (mtnii_vector_str == qname)
										|| (mstd_vector_str == qname)
										*/
										) {
										retval = upper_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref);
										if (retval.has_value()) {
											/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
											retval.value().m_possession_lifetime_info_chain.push_back({ {}, CPossessionLifetimeInfo1::is_element_in_a_multi_element_container_t::Yes });
										}
									}
								} else if (potential_owner_EX_ii_qtype->isReferenceType()) {
									int q = 5;
								}
							}
						}
					}
				}
			}
		}

		return retval;
	}

	struct CBoolWithHints {
		operator bool() const { return m_value; }
		CBoolWithHints(const CBoolWithHints& src) = default;
		CBoolWithHints(CBoolWithHints&& src) = default;
		CBoolWithHints(const bool& src) : m_value(src) {}
		CBoolWithHints& operator=(const CBoolWithHints& src) = default;
		CBoolWithHints& operator=(CBoolWithHints&& src) = default;
		std::string hints_str() const {
			std::string retval;
			for (const auto& str : m_hints) {
				retval += str + " ";
			}
			if (!retval.empty()) {
				retval = retval.substr(0, retval.size() - 1);
			}
			return retval;
		}

		std::vector<std::string> m_hints;
		bool m_value = false;
	};

	CBoolWithHints can_be_safely_targeted_with_an_xscope_reference(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		auto res1 = lower_bound_lifetime_owner_if_available(EX1, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
		CBoolWithHints retval = res1.has_value();
		if (true && res1.has_value()) {
			auto& lblo = res1.value();
			auto sli1_ptr = std::get_if<CScopeLifetimeInfo1>(&static_cast<CStaticLifetimeOwner const &>(lblo));
			if (sli1_ptr) {
				auto sli1 = *sli1_ptr;
				retval = false;
				if ((CScopeLifetimeInfo1::ECategory::Automatic == sli1.m_category)
				|| (CScopeLifetimeInfo1::ECategory::ThisExpression == sli1.m_category)
				|| (CScopeLifetimeInfo1::ECategory::Immortal == sli1.m_category)
				|| (CScopeLifetimeInfo1::ECategory::Literal == sli1.m_category)
				|| (CScopeLifetimeInfo1::ECategory::AbstractLifetime == sli1.m_category)
				) {
					retval = true;
				}
			}
		}
		retval.m_hints = std::move(res1.m_hints);
		return retval;
	}

	/* Given an expression that evaluates to a raw pointer, this function attempts to isolate the part
	of the expression that refers to the pointer's target object, if present. It's often or usually not
	present though. */
	const clang::Expr* raw_pointer_target_expression_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		const clang::Expr* retval = nullptr;
		if (!EX1) {
			return retval;
		}
		const auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);

		auto UO = dyn_cast<const clang::UnaryOperator>(EX);
		auto CE = dyn_cast<const clang::CallExpr>(EX);
		if (UO) {
			const auto opcode = UO->getOpcode();
			IF_DEBUG(const auto opcode_str = UO->getOpcodeStr(opcode);)
			if (clang::UnaryOperator::Opcode::UO_AddrOf == opcode) {
				retval = UO->getSubExpr();
			}
		} else if (CE) {
			auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();

			static const std::string std_addressof_str = "std::addressof";
			if ((std_addressof_str == function_qname) && (1 == CE->getNumArgs())) {
				retval = CE->getArg(0);
			}
		}

		return retval;
	}

	CMaybeStaticLifetimeOwnerWithHints lower_bound_lifetime_owner_of_pointer_target_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		auto res1 = lower_bound_lifetime_owner_if_available(raw_pointer_target_expression_if_available(EX1, Ctx, tu_state_ref), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
		if (res1.has_value()) {
			return res1;
		} else {
			/* The lifetime owner of the pointer target directly is not available. But the lifetime of
			the pointer itself serves as a lower bound for the lifetime of its target. */
			auto retval = lower_bound_lifetime_owner_if_available(EX1, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
			if (retval.has_value()) {
				if (std::holds_alternative<CScopeLifetimeInfo1>(retval.value())) {
					auto& sli1 = std::get<CScopeLifetimeInfo1>(retval.value());
					{
						CScopeLifetimeInfo1 sli2;
						sli2.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;

						if (1 == sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) {
							/* The pointer target's lifetime should be the first pointer's first "sublifetime". */
							sli2 = sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.front();
						} else {
							if (0 != sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) {
								/* unexpected */
								int q =3;
							}
							/* If the lifetime of the target object is not available then we'll just return the
							(abstract) lifetime of the pointer, which should serve as a lower bound for the lifetime of its
							target object. */
							sli2 = sli1;
						}
						retval = sli2;
					}
				}
			}
			return retval;
		}
	}

	CBoolWithHints pointer_target_can_be_safely_targeted_with_an_xscope_reference(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref) {
		auto res1 = lower_bound_lifetime_owner_of_pointer_target_if_available(EX1, Ctx, tu_state_ref);
		CBoolWithHints retval = res1.has_value();
		retval.m_hints = std::move(res1.m_hints);
		return retval;
	}

	class MCSSSMakeXScopePointerTo : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMakeXScopePointerTo (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmakexscopepointerto1");

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(make_xscope_pointer_to_str, mse_namespace_str() + "::rsv::make_xscope_pointer_to");
					DECLARE_CACHED_CONST_STRING(make_xscope_const_pointer_to_str, mse_namespace_str() + "::rsv::make_xscope_const_pointer_to");
					if ((make_xscope_pointer_to_str == qualified_function_name) || (make_xscope_const_pointer_to_str == qualified_function_name)) {
						if (1 == num_args) {
							auto EX1 = IgnoreParenImpNoopCasts(CE->getArg(0), *(MR.Context));
							auto res1 = can_be_safely_targeted_with_an_xscope_reference(EX1, *(MR.Context), m_state1);
							bool satisfies_checks = res1;
							if (!satisfies_checks) {
								std::string error_desc = std::string("Unable to verify that the use of mse::rsv::make_xscope_pointer_to() or ")
									+ "mse::rsv::make_xscope_const_pointer_to() (with argument type '" + CE->getArg(0)->getType().getAsString()
									+ "') is safe here.";
								const auto hints_str = res1.hints_str();
								if (!hints_str.empty()) {
									error_desc += " (" + hints_str + ")";
								} else {
									error_desc += " (Possibly due to being unable to verify that the target object outlives the scope pointer/reference.)";
								}
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSNativeReferenceVar : public MatchFinder::MatchCallback
	{
	public:
		MCSSSNativeReferenceVar (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::VarDecl* VD = MR.Nodes.getNodeAs<clang::VarDecl>("mcsssnativereferencevar1");

			if ((VD != nullptr))
			{
				auto SR = nice_source_range(VD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(VD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (suppress_check_flag) {
					return;
				}

				auto qtype = VD->getType();
				IF_DEBUG(std::string qtype_str = VD->getType().getAsString();)
				if (qtype->isReferenceType()) {
					if (clang::StorageDuration::SD_Automatic != VD->getStorageDuration()) {
						const std::string error_desc = std::string("Native references (such as those of type '")
							+ qtype.getAsString()
							+ "') that are not local variables (or function parameters) are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
					const auto EX = VD->getInit();
					if (EX) {
						auto res1 = can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), m_state1);
						bool satisfies_checks = res1;
						if (!satisfies_checks) {
							const auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
							const auto pointee_qtype = qtype->getPointeeType();
							if (PVD && (pointee_qtype.isConstQualified())) {
								/* We're dealing with a const reference parameter in this case. */
								auto MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(IgnoreExprWithCleanups(EX));
								if (MTE) {
									/* The initializer is a temporary. Since the initializer is a default parameter
									value in this case, it should be the case that it can be safely targeted with an
									xscope reference. */
									satisfies_checks = true;
								}
							}
						}
						if (!satisfies_checks) {
							std::string error_desc = std::string("Unable to verify that the ")
								+ "native reference (of type '" + qtype.getAsString() + "') is safe here.";
							const auto hints_str = res1.hints_str();
							if (!hints_str.empty()) {
								error_desc += " (" + hints_str + ")";
							} else {
								error_desc += " (Possibly due to being unable to verify that the target object outlives the (scope) reference.)";
							}
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSArgToNativeReferenceParam : public MatchFinder::MatchCallback
	{
	public:
		MCSSSArgToNativeReferenceParam (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssargtonativereferenceparam1");
			const CXXMemberCallExpr* CXXMCE = MR.Nodes.getNodeAs<clang::CXXMemberCallExpr>("mcsssargtonativereferenceparam1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcsssargtonativereferenceparam1");

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				const auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(mse_namespace_str, mse_namespace_str() + "::");
					static const std::string std_namespace_str = "std::";
					if (string_begins_with(qualified_function_name, mse_namespace_str)
						|| string_begins_with(qualified_function_name, std_namespace_str)) {
						return;
					}

					int arg_index = 0;
					auto num_params = function_decl->getNumParams();
					if (num_params < num_args) {
						/* todo: investigate and handle this case */
						if (num_params + 1 == num_args) {
							if (string_begins_with(function_name, "operator")) {
								int q = 5;
							}
						}
						if (0 == arg_index) {
							return;
						}
					}
					auto param_iter = function_decl->param_begin();
					for (; arg_index < int(num_args); arg_index++, param_iter++) {
						if (function_decl->param_end() == param_iter) {
							break;
						}
						const auto qtype = (*param_iter)->getType();
						IF_DEBUG(const std::string qtype_str = (*param_iter)->getType().getAsString();)
						if (qtype->isReferenceType()) {
							auto EX = CE->getArg(arg_index);
							bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), m_state1);
							if (!satisfies_checks) {
								auto const * const MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(EX);
								if (MTE) {
									auto MTE_qtype = MTE->getType();
									IF_DEBUG(auto MTE_qtype_str = MTE_qtype.getAsString();)
									MSE_RETURN_IF_TYPE_IS_NULL(MTE_qtype);
									if (!(MTE_qtype->isReferenceType())) {
										/* This argument is a temporary (non-reference) (that should outlive
										the function parameter). */
										satisfies_checks = true;
									}
								}
							}
							if (!satisfies_checks) {
								const std::string param_name = (*param_iter)->getNameAsString();
								std::string function_species_str = (CXXOCE) ? "operator" :
									((CXXMCE) ? "member function" : "function");

								auto SR = nice_source_range(EX->getSourceRange(), Rewrite);
								SourceLocation SL = SR.getBegin();

								const std::string error_desc = std::string("Unable to verify that the ")
									+ "argument passed to the parameter '" + param_name + "' of native reference type ('"
									+ qtype.getAsString() + "') of the " + function_species_str + " '" + qualified_function_name
									+ "' is safe here. (This is often addressed "
									+ "by obtaining a scope pointer to the intended argument and passing "
									+ "an expression consisting of a dereference of that pointer.)";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSReturnStmt : public MatchFinder::MatchCallback
	{
	public:
		MCSSSReturnStmt (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::ReturnStmt* ST = MR.Nodes.getNodeAs<clang::ReturnStmt>("mcsssreturnstmt");

			if ((ST != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(ST->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ST, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(STISR);
				if (suppress_check_flag) {
					return;
				}

				if (ST->getRetValue()) {
					if (is_xscope_type(ST->getRetValue()->getType(), (*this).m_state1)
						&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(ST->getRetValue()->getType()))
						) {

						bool rv_lifetime_annotation_is_present = false;
						auto E = dyn_cast<const clang::Expr>(ST->getRetValue());
						if (E) {
							auto FD = Tget_containing_element_of_type<clang::FunctionDecl>(E, *(MR.Context));
							if (FD) {
								process_function_lifetime_annotations(*FD, m_state1, &MR, &Rewrite);
								auto flta_iter = m_state1.m_function_lifetime_annotations_map.find(FD);
								if (m_state1.m_function_lifetime_annotations_map.end() != flta_iter) {
									if (!(flta_iter->second.m_return_value_lifetimes.is_empty())) {
										rv_lifetime_annotation_is_present = true;
										bool satisfies_checks = false;
										auto rv_abstract_lifetimes = flta_iter->second.m_return_value_lifetimes;
										auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, m_state1, E);
										MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value);
										if (maybe_expr_lifetime_value.has_value()) {
											auto &lbsli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
											CScopeLifetimeInfo1 rvsli;
											auto E_qtype = E->getType();
											IF_DEBUG(auto E_qtype_str = E_qtype.getAsString();)
											MSE_RETURN_IF_TYPE_IS_NULL(E_qtype);
											if (E_qtype->isReferenceType()) {
												/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
												same as the object it refers to (without an added level of indirection). */
												if (1 == rv_abstract_lifetimes.m_primary_lifetimes.size()) {
													rvsli.m_maybe_abstract_lifetime = rv_abstract_lifetimes.m_primary_lifetimes.front();
													rvsli.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
												} else {
													/* Unexpected. Native references with abstract lifetimes should have exactly one primary abstract
													lifetime. */
													int q = 3;
												}
											} else {
												rvsli.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
												rvsli.m_maybe_containing_scope = get_containing_scope(ST, *(MR.Context));
												rvsli.m_maybe_source_range = ST->getSourceRange();
												/* None of the above really matters. Really only the sublifetimes matter for determining whether
												the expression can be assigned to a (hypothetical) abstract return value in this case. */
												*(rvsli.m_sublifetimes_vlptr) = rv_abstract_lifetimes;
											}
											satisfies_checks = slti_second_can_be_assigned_to_first(rvsli, lbsli, *(MR.Context), m_state1);
										}
										if (!satisfies_checks) {
											const std::string error_desc = std::string("Unable to verify that the return value conforms to the lifetime specified.");
											auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
											if (res.second) {
												std::cout << (*(res.first)).as_a_string1() << " \n\n";
											}
										}
										return;
									}
								}
							} else {
								int q = 3;
							}
						}
						if (rv_lifetime_annotation_is_present) {
							return;
						}

						bool xscope_return_value_wrapper_present = false;
						const clang::Stmt* stii = IgnoreParenImpNoopCasts(ST->getRetValue(), *(MR.Context));
						while (!dyn_cast<const CallExpr>(stii)) {
							//stii->dump();
							auto* CXXCE = dyn_cast<const CXXConstructExpr>(stii);
							if (CXXCE && (1 <= CXXCE->getNumArgs()) && (CXXCE->getArg(0))) {
								stii = IgnoreParenImpNoopCasts(CXXCE->getArg(0), *(MR.Context));
								continue;
							}
							break;
						}
						auto* CE = dyn_cast<const CallExpr>(stii);
						if (CE) {
							auto function_decl = CE->getDirectCallee();
							auto num_args = CE->getNumArgs();
							if (function_decl) {
								std::string qualified_function_name = function_decl->getQualifiedNameAsString();
								DECLARE_CACHED_CONST_STRING(return_value_str, mse_namespace_str() + "::return_value");
								if (return_value_str == qualified_function_name) {
									xscope_return_value_wrapper_present = true;
								}
							}
						}
						if (!xscope_return_value_wrapper_present) {
							std::string error_desc = std::string("Return values of xscope type (such as '")
							+ ST->getRetValue()->getType().getAsString() + "') need to be wrapped in the mse::return_value() function wrapper"
							+ ", or have their lifetime specified with the 'return_value<>' function lifetime annotation.";

							auto FND = enclosing_function_if_any(ST->getRetValue(), *(MR.Context));
							if (FND) {
								if (FND->isDefaulted()) {
									error_desc += " (This return statement is contained in the (possibly implicit/default member) function '" + FND->getNameAsString() + "' .)";
								}
							}

							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSPointerArithmetic : public MatchFinder::MatchCallback
	{
	public:
		MCSSSPointerArithmetic (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const Expr* EX = MR.Nodes.getNodeAs<clang::Expr>("mcssspointerarithmetic1");

			if ((EX != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(EX->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EX, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (suppress_check_flag) {
					return;
				}

				IF_DEBUG(const auto EX_qtype_str = EX->getType().getAsString();)
				const auto EX_rw_type_ptr = remove_mse_transparent_wrappers(EX->getType()).getTypePtr();
				assert(EX_rw_type_ptr);
				if (!EX_rw_type_ptr->isPointerType()) {
					const auto RD = EX_rw_type_ptr->getAsRecordDecl();
					if (!RD) {
						return;
					} else {
						/* `mse::us::impl::TPointerForLegacy<>` is sometimes used as (a functionally
						equivalent) substitute for native pointers that can act as a base class. */
						const auto EX_rw_qtype_str = RD->getQualifiedNameAsString();
						DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
						if (TPointerForLegacy_str != EX_rw_qtype_str) {
							return;
						}
					}
				} else {
					const auto qtype = clang::QualType(EX_rw_type_ptr, 0/*I'm just assuming zero specifies no qualifiers*/);
					if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)) {
						return;
					}
				}

				{
					const std::string error_desc = std::string("Pointer arithmetic (including ")
						+ "native array subscripts) is not supported. (The expression in question here is of type '"
						+ EX->getType().getAsString() + "'.)";
					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSAddressOf : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAddressOf (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Expr* EX = MR.Nodes.getNodeAs<clang::Expr>("mcsssaddressof1");

			if ((EX != nullptr))
			{
				auto SR = nice_source_range(EX->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EX, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (suppress_check_flag) {
					return;
				}

				if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(EX->getType())) {
					return;
				}

				const clang::Expr* resulting_pointer_EX = MR.Nodes.getNodeAs<clang::Expr>("mcsssaddressof2");
				if (resulting_pointer_EX) {
					const auto qtype = resulting_pointer_EX->getType();
					IF_DEBUG(const std::string qtype_str = resulting_pointer_EX->getType().getAsString();)
					if ((resulting_pointer_EX->getType().getTypePtr()->isMemberPointerType())
						|| (resulting_pointer_EX->getType().getTypePtr()->isFunctionPointerType())
						|| (*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)
						) {
						return;
					}
				}
				{
					auto res1 = can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), m_state1);
					bool satisfies_checks = res1;
					if (!satisfies_checks) {
						std::string error_desc = std::string("Unable to verify that the return value ")
							+ "of the '&' operator or std::addressof() (with argument type '"
							+ EX->getType().getAsString() + "') is safe here.";
						const auto hints_str = res1.hints_str();
						if (!hints_str.empty()) {
							error_desc += " (" + hints_str + ")";
						} else {
							error_desc += " (Possibly due to being unable to verify that the target object outlives the (scope) pointer.)";
						}
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSNativePointerVar : public MatchFinder::MatchCallback
	{
	public:
		MCSSSNativePointerVar (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::VarDecl* VD = MR.Nodes.getNodeAs<clang::VarDecl>("mcsssnativepointervar1");

			if ((VD != nullptr))
			{
				auto SR = nice_source_range(VD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(VD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (suppress_check_flag) {
					return;
				}

				auto qtype = VD->getType();
				IF_DEBUG(std::string qtype_str = VD->getType().getAsString();)
				if (qtype->isPointerType()) {

					if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)) {
						return;
					}

					/*
					if ((clang::StorageDuration::SD_Automatic != VD->getStorageDuration())
						&& (clang::StorageDuration::SD_Thread != VD->getStorageDuration())) {
						const std::string error_desc = std::string("Native pointers that are ")
							+ "not (automatic or thread) local variables (or function parameters) are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
					*/
					const auto* EX = VD->getInit();
					if (EX) {
						bool null_initialization = is_nullptr_literal(EX, *(MR.Context));

						if (null_initialization) {
							const std::string error_desc = std::string("Null initialization of ")
								+ "native pointers (such as those of type '" + qtype.getAsString()
								+ "') is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					} else {
						auto *PVD = dyn_cast<const ParmVarDecl>(VD);
						if (!PVD) {
							if (!VD->isExternallyDeclarable()) {
								const std::string error_desc = std::string("Uninitialized ")
									+ "native pointer variables are not supported.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							} else {
								/* todo: emit error that (uninitialized) 'extern' pointer variables
								aren't supported?  */;
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSCast : public MatchFinder::MatchCallback
	{
	public:
		MCSSSCast (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Expr* EX = MR.Nodes.getNodeAs<clang::Expr>("mcssscast1");

			if ((EX != nullptr))
			{
				auto SR = nice_source_range(EX->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EX, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (suppress_check_flag) {
					return;
				}

				std::string cast_type_str;
				const auto* EXii = IgnoreParenImpCasts(EX);
				auto const * const CSTE = dyn_cast<const clang::CastExpr>(EXii);
				if (CSTE) {
					const auto cast_kind = CSTE->getCastKind();
					if (clang::CK_IntegralToPointer == CSTE->getCastKind()) {
						cast_type_str = "Integral-to-pointer";
					} else if (clang::CK_NoOp == CSTE->getCastKind()) {
						/* If the cast is a no-op, I guess we won't complain. */
						//cast_type_str = "";
					} else {
						auto const * const CSCE = dyn_cast<const CStyleCastExpr>(EXii);
						if (CSCE) {
							cast_type_str = "'C-style'";
						} else {
							auto const * const CXXRCE = dyn_cast<const CXXReinterpretCastExpr>(EXii);
							if (CXXRCE) {
								cast_type_str = "Reinterpret";
							} else {
								auto const * const CXXCCE = dyn_cast<const CXXConstCastExpr>(EXii);
								if (CXXCCE) {
									cast_type_str = "Const";
								} else {
									auto const * const CXXFCE = dyn_cast<const CXXFunctionalCastExpr>(EXii);
									if (CXXFCE) {
										const auto qtype = CXXFCE->getType();
										IF_DEBUG(const std::string qtype_str = CXXFCE->getType().getAsString();)

										const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
										if (CXXRD) {
											auto name = CXXRD->getQualifiedNameAsString();
											const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
											if (tmplt_CXXRD) {
												name = tmplt_CXXRD->getQualifiedNameAsString();
											}
											DECLARE_CACHED_CONST_STRING(mse_rsv_tfparam_str1, mse_namespace_str() + "::rsv::TFParam");
											if (mse_rsv_tfparam_str1 == name) {
												if (1 == CXXRD->getNumBases()) {
													cast_type_str = "Explicit mse::rsv::TFParam<> functional";
												} else {
													/* This branch shouldn't happen. Unless the library's been changed somehow. */
												}
											}
										}
									}
								}
							}
						}
					}
				}
				if ("" != cast_type_str) {
					assert(CSTE);
					const std::string error_desc = cast_type_str
						+ " casts are not supported (in expression of type '"
						+ CSTE->getType().getAsString() + "').";
					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSMemberFunctionCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMemberFunctionCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const CXXMemberCallExpr* CXXMCE = nullptr, const CXXOperatorCallExpr* CXXOCE = nullptr) {

			if ((CXXMCE != nullptr) || (CXXOCE != nullptr))
			{
				auto raw_SR = CXXMCE ? CXXMCE->getSourceRange() : CXXOCE->getSourceRange();
				auto SR = nice_source_range(raw_SR, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = CXXMCE ? state1.m_suppress_check_region_set.contains(CXXMCE, Rewrite, *(MR.Context))
					: state1.m_suppress_check_region_set.contains(CXXOCE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CXXMCEISR);
				if (suppress_check_flag) {
					return;
				}

				std::string method_name;
				const clang::CXXMethodDecl* method_decl = nullptr;
				if (CXXMCE) {
					const auto method_decl1 = CXXMCE->getDirectCallee();
					if (method_decl1) {
						method_name = method_decl1->getNameAsString();
					}
					method_decl = dyn_cast<const clang::CXXMethodDecl>(method_decl1);
				} else {
					const auto method_decl1 = CXXOCE->getDirectCallee();
					if (method_decl1) {
						method_name = method_decl1->getNameAsString();
					}
					method_decl = dyn_cast<const clang::CXXMethodDecl>(method_decl1);
					if (!method_decl) {
						/* This may be a free operator, rather than a member operator. */
						return;
					}
				}
				if (method_decl) {
					const std::string qmethod_name = method_decl->getQualifiedNameAsString();

					auto method_declSR = nice_source_range(method_decl->getSourceRange(), Rewrite);
					SourceLocation method_declSL = method_declSR.getBegin();

					DECLARE_CACHED_CONST_STRING(mse_ns_prefix, mse_namespace_str() + std::string("::"));
					static const std::string std_ns_prefix = "std::";
					if (string_begins_with(qmethod_name, mse_ns_prefix)
						|| string_begins_with(qmethod_name, std_ns_prefix)
						|| filtered_out_by_location(MR, method_declSL)) {
						/* The idea is to permit member function calls only if it's clear that the implicit 'this'
						pointer parameter will remain valid for the duration of the member function call. Generally
						we'll require that the implicit 'this' pointer argument be a scope pointer/reference, or
						equivalent.
						Here we waive this requirement if the member function is part of the SaferCPlusPlus library
						or the standard library. The SaferCPlusPlus containers have run-time safety mechanisms that
						ensure that the 'this' pointer target is not desroyed. In the future we may have an
						option/mode where the (run-time) safety mechanisms are disabled and the SaferCPlusPlus
						containers are not exempt.
						We don't apply this requirement to standard library elements here as standard library
						containers are themselves considered unsafe by default. */
						return;
					}

					auto template_kind = method_decl->getTemplatedKind();
					if (clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate == template_kind) {
						return;
					}
					if (method_decl->isDefaulted()) {
						return;
					}
					if (method_decl->isStatic()) {
						return;
					}
					if (method_decl->isImplicit()) {
						return;
					}
					const auto CXXDD = dyn_cast<const clang::CXXDestructorDecl>(method_decl);
					if (CXXDD) {
						const std::string error_desc =  std::string("Explicitly calling destructors (such as '") + qmethod_name
							+ "') is not supported.";
						auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				} else {
					static const std::string tilda_str = "~";
					if (string_begins_with(method_name, tilda_str)) {
						const std::string error_desc =  std::string("'") + method_name
							+ "' looks like a destructor. Explicitly calling destructors is not supported.";
						auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}

				const clang::Expr* EX = nullptr;
				const clang::Expr* EX_iic = nullptr;
				std::string function_qname;
				std::string function_species_str;
				if (CXXMCE) {
					EX = IgnoreParenImpNoopCasts(CXXMCE->getImplicitObjectArgument(), *(MR.Context));
					EX_iic = CXXMCE->getImplicitObjectArgument()->IgnoreImpCasts();
					function_qname = CXXMCE->getDirectCallee()->getQualifiedNameAsString();
					function_species_str = "function";
				} else {
					function_qname = CXXOCE->getDirectCallee()->getQualifiedNameAsString();
					function_species_str = "operator";
					if (1 <= CXXOCE->getNumArgs()) {
						EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), *(MR.Context));
						EX_iic = CXXOCE->getArg(0)->IgnoreImpCasts();
					}
				}
				if (EX) {
					const auto qtype = EX->getType();
					IF_DEBUG(const auto qtype_str = qtype.getAsString();)
					DEBUG_SOURCE_TEXT_STR(EX_source_text, nice_source_range(EX->getSourceRange(), Rewrite), Rewrite);

					bool satisfies_checks = qtype->isPointerType()
						? pointer_target_can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), state1)
						: can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), state1);
					if ((!satisfies_checks) && EX_iic) {
						const auto MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(EX_iic);
						//const auto CXXTOE = dyn_cast<const clang::CXXTemporaryObjectExpr>(EX_iic);
						//const auto CXXBTE = dyn_cast<const clang::CXXBindTemporaryExpr>(EX_iic);
						if (MTE) {
							/* Calling a member function of a temporary should be fine. The temporary object
							will outlive the function call. */
							satisfies_checks = true;
						}
					}
					if (!satisfies_checks) {
						const std::string error_desc = std::string("Unable to verify that the 'this' pointer ")
							+ "will remain valid for the duration of the member " + function_species_str + " call ('"
							+ function_qname + "'). (This is often addressed "
							+ "by obtaining a scope pointer to the object then calling the member " + function_species_str
							+ " through the scope pointer.)";
						auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CXXMemberCallExpr* CXXMCE = MR.Nodes.getNodeAs<clang::CXXMemberCallExpr>("mcsssmemberfunctioncall1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcssscxxoperatorcall1");

			s_handler1(MR, Rewrite, m_state1, CXXMCE, CXXOCE);
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	inline bool is_known_to_be_const_declared_variable(clang::VarDecl const * VD) {
		bool retval = false;
		if (!VD) {
			return false;
		}
		retval = VD->getType().isConstQualified();
		return retval;
	}
	inline bool is_known_to_be_const_declared_variable(clang::Expr const * E) {
		bool retval = false;
		if (!E) {
			return false;
		}
		const auto E_ii = IgnoreParenImpCasts(E)/*IgnoreParenImpNoopCasts(E, Ctx)*/;
		auto DRE = dyn_cast<const clang::DeclRefExpr>(E_ii);
		if (DRE) {
			auto value_decl = DRE->getDecl();
			if (value_decl) {
				auto VD = dyn_cast<const clang::VarDecl>(value_decl);
				if (VD) {
					retval = is_known_to_be_const_declared_variable(VD);
				}
			}
		}

		return retval;
	}

	inline bool is_recognized_dynamic_container(clang::QualType const& qtype) {
		auto const peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		bool retval = false;

		thread_local std::vector<std::string> known_dynamic_container_names;
		thread_local std::unordered_set<std::string_view> known_dynamic_container_name_svs;
		thread_local std::unordered_set<std::string_view> known_dynamic_container_truncated_name_svs;
		thread_local size_t length_of_shortest_container_name = 0;
		if (0 == known_dynamic_container_name_svs.size()) {
			known_dynamic_container_names.push_back("std::unique_ptr");
			known_dynamic_container_names.push_back("std::shared_ptr");
			known_dynamic_container_names.push_back("std::optional");
			known_dynamic_container_names.push_back("std::vector");
			known_dynamic_container_names.push_back("std::basic_string");
			known_dynamic_container_names.push_back("std::string");
			known_dynamic_container_names.push_back("std::basic_string_view");
			known_dynamic_container_names.push_back("std::string_view");
			known_dynamic_container_names.push_back("std::span");
			known_dynamic_container_names.push_back("std::list");
			known_dynamic_container_names.push_back("std::map");
			known_dynamic_container_names.push_back("std::set");
			known_dynamic_container_names.push_back("std::multimap");
			known_dynamic_container_names.push_back("std::multiset");
			known_dynamic_container_names.push_back("std::unordered_map");
			known_dynamic_container_names.push_back("std::unordered_set");
			known_dynamic_container_names.push_back("std::unordered_multimap");
			known_dynamic_container_names.push_back("std::unordered_multiset");
			known_dynamic_container_names.push_back("std::dequeue");

			known_dynamic_container_names.push_back("__gnu_cxx::__normal_iterator");
			//__gnu_cxx::__normal_iterator<int *, std::vector<int>>
			//std::vector<int>::iterator
			//known_dynamic_container_names.push_back("const char *");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::us::impl::ns_optional::optional_base2");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::xscope_optional");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::optional");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::mstd::optional");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::optional");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TOptionalElementFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TOptionalElementFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::us::impl::gnii_vector");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::nii_vector");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::stnii_vector");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::mtnii_vector");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::mstd::vector");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::impl::ns_gnii_vector::Tgnii_vector_ss_iterator_type");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::impl::ns_gnii_vector::Tgnii_vector_ss_const_iterator_type");

			//mse::mstd::vector<int>::iterator

			//mse::us::impl::gnii_vector
			//mse::impl::ns_gnii_vector::Tgnii_vector_xscope_cslsstrong_iterator_type
			//mse::impl::ns_gnii_vector::Tgnii_vector_ss_iterator_type
			//mse::us::impl::ns_ra_iter::TRAIteratorBase<mse::TNDRegisteredNotNullPointer<mse::us::impl::gnii_vector<int>>>::reference

			//mse::TOptionalElementFixedPointer
			//mse::us::impl::ns_optional::optional_base2

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingNotNullPointer");
			//known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingNotNullConstPointer");
			//known_dynamic_container_names.push_back(mse_namespace_str() + "::TRefCountingFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDRegisteredFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNDCRegisteredFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradNotNullPointer");
			//known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradNotNullConstPointer");
			//known_dynamic_container_names.push_back(mse_namespace_str() + "::TNoradFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TAnyFixedConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopeAnyFixedConstPointer");

			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TPolyFixedConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyNotNullPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyFixedPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyNotNullConstPointer");
			known_dynamic_container_names.push_back(mse_namespace_str() + "::TXScopePolyFixedConstPointer");

			if (known_dynamic_container_names.size() > 0) {
				length_of_shortest_container_name = known_dynamic_container_names.front().length();
			}

			for (auto& name : known_dynamic_container_names) {
				known_dynamic_container_name_svs.insert(name);
				if (name.length() < length_of_shortest_container_name) {
					length_of_shortest_container_name = name.length();
				}
			}
			for (auto name_sv : known_dynamic_container_name_svs) {
				known_dynamic_container_truncated_name_svs.insert(name_sv.substr(0, length_of_shortest_container_name));
			}
		}

		const auto CXXRD = peeled_qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			std::string_view qname_sv{ qname };

			if (string_begins_with(qname_sv, mse_namespace_str()) || (string_begins_with(qname_sv, "std"))
				|| (string_begins_with(qname_sv, "__gnu_cxx"))) {

				auto truncated_qname_sv = qname_sv.substr(0, length_of_shortest_container_name);
				auto found_it = known_dynamic_container_truncated_name_svs.find(truncated_qname_sv);
				if (known_dynamic_container_truncated_name_svs.end() != found_it) {
					for (auto const & name : known_dynamic_container_names) {
						if (string_begins_with(qname_sv, name)) {
							return true;
						}
					}
				}
				if (string_begins_with(qname_sv, "::us::impl::ns_ra_iter::TRAIteratorBase") || string_begins_with(qname_sv, "::us::impl::ns_ra_iter::TRAConstIteratorBase")) {
					auto maybe_qtype2 = get_first_template_parameter_if_any(peeled_qtype);
					if (maybe_qtype2.has_value()) {
						auto maybe_qtype3 = get_first_template_parameter_if_any(remove_mse_transparent_wrappers(maybe_qtype2.value()));
						if (maybe_qtype3.has_value()) {
							retval |= is_recognized_dynamic_container(remove_mse_transparent_wrappers(maybe_qtype3.value()));
						} else {
							return true;
						}
					} else {
						return true;
					}
				}
			}
		}

		return retval;
	}

	template<typename TCallOrConstuctorExpr>
	inline std::string function_call_handler2(CTUState& state1, const clang::FunctionDecl* function_decl
		, const TCallOrConstuctorExpr* CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		if (CE != nullptr)
		{
			IF_DEBUG(std::string debug_source_location_str2;)
			IF_DEBUG(std::string debug_source_text2;)
			if (MR_ptr && Rewrite_ptr) {
				auto& MR = *MR_ptr;
				auto& Rewrite = *Rewrite_ptr;

				auto raw_SR = CE->getSourceRange();
				auto SR = nice_source_range(raw_SR, Rewrite);

				//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
				if (SR.isValid()) {

					DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

					//RETURN_IF_FILTERED_OUT_BY_LOCATION1;
					if ((!SR.isValid()) || filtered_out_by_location(MR, SR.getBegin())) {
						return {};
					}

					DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

	#ifndef NDEBUG
					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
					debug_source_location_str2 = debug_source_location_str;
					debug_source_text2 = debug_source_text;
	#endif /*!NDEBUG*/
				}
			}
			{
				auto eltv_iter = state1.m_expr_lifetime_values_map.find(CE);
				bool needs_processing = (state1.m_expr_lifetime_values_map.end() == eltv_iter);
				/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
				needs_processing = (needs_processing || ((!(eltv_iter->second.m_errors_noted)) && bool(MR_ptr)));
				if (!needs_processing) {
					/* Already processed (and any errors noted). */
					return {};
				}
			}

			auto raw_SR = CE->getSourceRange();
			auto SR = Rewrite_ptr ? nice_source_range(raw_SR, *Rewrite_ptr) : raw_SR;

			if (function_decl) {
				IF_DEBUG(const std::string function_name = function_decl->getNameAsString();)
				const std::string qfunction_name = function_decl->getQualifiedNameAsString();

				auto function_declSR = Rewrite_ptr ? nice_source_range(function_decl->getSourceRange(), *Rewrite_ptr)
					: function_decl->getSourceRange();
				SourceLocation function_declSL = function_declSR.getBegin();

				auto CXXMCE = dyn_cast<clang::CXXMemberCallExpr>(CE);
				auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);

				auto CXXOCE = dyn_cast<clang::CXXOperatorCallExpr>(CE);
				auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);

				/* Here we're going to try to evaluate and store the lifetimes of (the resulting value of)
				this (call) expression. We're presuming that the direct lifetime of any expression is a
				"temporary lifetime". But the expression may have other associated lifetimes defined by
				lifetime annotations on the type of the expression. */

				std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> CE_type_lifetime_value_map;
				CScopeLifetimeInfo1Set expr_scope_sublifetimes;

				IF_DEBUG(const auto FD_qtype_str = function_decl->getType().getAsString();)
				IF_DEBUG(const auto retval_qtype_str = function_decl->getReturnType().getAsString();)
				IF_DEBUG(const auto CE_qtype_str = CE->getType().getAsString();)
				const auto CE_TypePtr1 = CE->getType().getTypePtr();

				if (CXXCE) {
					auto maybe_tla_ptr = type_lifetime_annotations_if_available(CE_TypePtr1, state1, MR_ptr, Rewrite_ptr);

					if (!(maybe_tla_ptr.has_value())) {
						if (CE_TypePtr1) {
							auto TD = CE_TypePtr1->getAsTagDecl();
							if (TD) {
								process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
							}
						}
						maybe_tla_ptr = type_lifetime_annotations_if_available(CE_TypePtr1, state1, MR_ptr, Rewrite_ptr);
					}

					if (maybe_tla_ptr.has_value()) {
						auto& tla_ref = *(maybe_tla_ptr.value());
						/* We initialize the expression "sublifetimes" with the abstract lifetimes of this (constructor)
						expression's type. Where we can, we will subsequently replace each of these abstract lifetimes
						with the corresponding concrete lifetime inferred from the (constructor) expression. */
						expr_scope_sublifetimes = CScopeLifetimeInfo1Set(tla_ref.m_lifetime_set);

						for (auto& abstract_lifetime : tla_ref.m_lifetime_set.m_primary_lifetimes) {
							CE_type_lifetime_value_map.insert_or_assign( abstract_lifetime, abstract_lifetime );
						}
					}
				}

				if (CXXCE && CXXCE->getConstructor() && CXXCE->getConstructor()->isCopyOrMoveConstructor()) {
					if (1 == CE->getNumArgs()) {
						auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
						expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;

						auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, CE->getArg(0), Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							return {};
						}
						if (maybe_expr_lifetime_value.has_value()) {
							auto& expr_lifetime_value_ref = maybe_expr_lifetime_value.value();
							auto CE_qtype = CE->getType();
							IF_DEBUG(auto CE_qtype_str = CE_qtype.getAsString();)
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL(CE_qtype, {});
							if (CE_qtype->isReferenceType()) {
								/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
								same as the object it refers to (without an added level of indirection). */
								expr_scope_lifetime_info = expr_lifetime_value_ref.m_scope_lifetime_info;
							} else {
								CScopeLifetimeInfo1& arg_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
								//expr_scope_sublifetimes = *(arg_slti.m_sublifetimes_vlptr);
								/* Here we set the evaluated expression sublifetimes. */
								*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = *(arg_slti.m_sublifetimes_vlptr);
							}
							/* Here we put the evaluated expression lifetimes in "persistent" storage. */
							state1.m_expr_lifetime_values_map.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
							return {};
						}
					} else {
						int q = 3;
					}
				} else {
					process_function_lifetime_annotations(*function_decl, state1, MR_ptr, Rewrite_ptr);
					auto flta_iter = state1.m_function_lifetime_annotations_map.find(function_decl);
					if (state1.m_function_lifetime_annotations_map.end() != flta_iter) {
						auto flta = flta_iter->second;

						if (!(flta.m_return_value_lifetimes.is_empty())) {
							/* The function seems to have explicitly annotated return value lifetimes (which correspond to
							the sublifetime values of the return value). We initialize the expression "sublifetimes" with
							the abstract lifetimes of the function's return value lifetime annotation. Where we can, we
							will subsequently replace each of these abstract lifetimes with the corresponding concrete
							lifetime inferred from the (call) expression. */
							expr_scope_sublifetimes = CScopeLifetimeInfo1Set(flta.m_return_value_lifetimes);

							CE_type_lifetime_value_map.clear();
							for (auto& abstract_lifetime : flta.m_return_value_lifetimes.m_primary_lifetimes) {
								CE_type_lifetime_value_map.insert_or_assign( abstract_lifetime, abstract_lifetime );
							}
						}

						std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> initialized_lifetime_value_map = CE_type_lifetime_value_map;
						std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> present_lifetime_value_map = CE_type_lifetime_value_map;

						std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> IOA_type_lifetime_value_map;

						if (CXXMCE) {
							/* This is a member function call. */
							auto IOA_E = CXXMCE->getImplicitObjectArgument();
							auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr);
							if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
								/* Cannot properly evaluate because this is a template definition. Proper evaluation should
								occur in any instantiation of the template. */
								return {};
							}
							if (maybe_expr_lifetime_value.has_value()) {
								auto &this_sli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

								/* The actual lifetime values for this object are available. */
								auto& sublifetime_values_ref = this_sli.m_sublifetimes_vlptr->m_primary_lifetime_infos;

								IF_DEBUG(const auto IOA_qtype_str = IOA_E->getType().getAsString();)
								const auto IOA_TypePtr1 = IOA_E->getType().getTypePtr();
								auto IOA_tlta_iter = state1.m_type_lifetime_annotations_map.find(IOA_TypePtr1);
								if (state1.m_type_lifetime_annotations_map.end() == IOA_tlta_iter) {
									if (IOA_TypePtr1) {
										auto TD = IOA_TypePtr1->getAsTagDecl();
										if (TD) {
											process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
										}
									}
									IOA_tlta_iter = state1.m_type_lifetime_annotations_map.find(IOA_TypePtr1);
								}
								if (state1.m_type_lifetime_annotations_map.end() != IOA_tlta_iter) {
									auto& IOA_type_lifetime_set_cref = IOA_tlta_iter->second.m_lifetime_set;
									const auto IOA_type_abstract_lifetime_end_iter = IOA_type_lifetime_set_cref.m_primary_lifetimes.cend();
									auto IOA_type_abstract_lifetime_iter1 = IOA_type_lifetime_set_cref.m_primary_lifetimes.cbegin();
									const auto IOA_lifetime_values_end_iter = sublifetime_values_ref.end();
									auto IOA_lifetime_values_iter1 = sublifetime_values_ref.begin();
									/* Here we are iterating over each of the object type's (annotated) abstract lifetimes. */
									for (; (IOA_type_abstract_lifetime_end_iter != IOA_type_abstract_lifetime_iter1) && (IOA_lifetime_values_end_iter != IOA_lifetime_values_iter1)
										; IOA_type_abstract_lifetime_iter1++, IOA_lifetime_values_iter1++) {

										/* Here we're adding a mapping between each abstract lifetime (of the object's type) and the actual lifetime
										value (assigned at object initialization) associated with it. */
										IOA_type_lifetime_value_map.insert_or_assign( *IOA_type_abstract_lifetime_iter1, *IOA_lifetime_values_iter1 );
									}
								} else {
									int q = 3;
								}
							}
						}
						initialized_lifetime_value_map.merge(IOA_type_lifetime_value_map);
						present_lifetime_value_map.merge(IOA_type_lifetime_value_map);

						if (CXXCE) {
							/* Here we iterate over each constructor parameter with an associated abstract lifetime (annotation). */
							for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
								const auto abstract_lifetime1 = param_lifetime1.second.first_lifetime();
								CScopeLifetimeInfo1 corresponding_present_scope_lifetime = abstract_lifetime1;
								auto found_it = initialized_lifetime_value_map.find(abstract_lifetime1);
								if (initialized_lifetime_value_map.end() != found_it) {
									corresponding_present_scope_lifetime = found_it->second;
								}

								CScopeLifetimeInfo1 corresponding_initialized_scope_lifetime = corresponding_present_scope_lifetime;
								clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
								if (!arg1_EX) {
								} else {
									/* Now we try to evaluate the "concrete" lifetime of the corresponding argument in the contructor expression. */

									auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, arg1_EX, Ctx, MR_ptr, Rewrite_ptr);
									if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
										/* Cannot properly evaluate because this is a template definition. Proper evaluation should
										occur in any instantiation of the template. */
										return {};
									}
									if (maybe_expr_lifetime_value.has_value()) {
										CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
										auto sloi1 = expr_slti;
										if (!slti_second_can_be_assigned_to_first(corresponding_present_scope_lifetime, sloi1, Ctx, state1)) {
											/* The current stored value of the lifetime (is abstract or) doesn't seem to be shorter than the lifetime
											of the argument we just evaluated, so we will replace the stored one with the one we just evaluated. (It's
											possible that the already stored one could be shorter in the case, for example, where multiple constructor
											parameters are annotated with the same lifetime.) */
											corresponding_present_scope_lifetime = sloi1;
										}
									}
								}
								initialized_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
								present_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
							}
							for (auto& initialized_lifetime_value_mapping : initialized_lifetime_value_map) {
								auto& abstract_lifetime1 = initialized_lifetime_value_mapping.first;
								auto found_it = CE_type_lifetime_value_map.find(abstract_lifetime1);
								if (CE_type_lifetime_value_map.end() != found_it) {
									found_it->second = initialized_lifetime_value_mapping.second;
								}
							}
						} else {
							/* Here we iterate over each function parameter with an associated abstract lifetime (annotation). */
							for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
								const auto abstract_lifetime1 = param_lifetime1.second.first_lifetime();
								CScopeLifetimeInfo1 corresponding_initialized_scope_lifetime = abstract_lifetime1;
								auto found_it = initialized_lifetime_value_map.find(abstract_lifetime1);
								if (initialized_lifetime_value_map.end() != found_it) {
									corresponding_initialized_scope_lifetime = found_it->second;
								}

								CScopeLifetimeInfo1 corresponding_present_scope_lifetime = corresponding_initialized_scope_lifetime;
								clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
								if (!arg1_EX) {
									int q = 5;
								} else {
									CScopeLifetimeInfo1 sloi1;

									auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, arg1_EX, Ctx, MR_ptr, Rewrite_ptr);
									if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
										/* Cannot properly evaluate because this is a template definition. Proper evaluation should
										occur in any instantiation of the template. */
										return {};
									}
									if (maybe_expr_lifetime_value.has_value()) {
										CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
										sloi1 = expr_slti;
									} else {
										/* We generally don't expect to get here, but if for some reason a lower bound for the argument
										lifetime isn't available, we'll just use the shortest viable lifetime. */
										sloi1.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
										sloi1.m_maybe_source_range = arg1_EX->getSourceRange();
									}

									auto found_it2 = IOA_type_lifetime_value_map.find(abstract_lifetime1);
									if (IOA_type_lifetime_value_map.end() != found_it2) {
										/* The annotated lifetime of the parameter is an annotated lifetime of the object type. */
										if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == corresponding_initialized_scope_lifetime.m_category) {
											corresponding_initialized_scope_lifetime = sloi1;
											int q = 3;
										}
										if (!slti_second_can_be_assigned_to_first(corresponding_initialized_scope_lifetime, sloi1, Ctx, state1)) {
											std::string implicit_or_explicit_str;

											auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
											if (CXXMD) {
												implicit_or_explicit_str = "(implicit or explicit) member ";
											}
											std::string function_or_constructor_str = "function ";
											if (std::is_same_v<clang::CXXConstructExpr, TCallOrConstuctorExpr>) {
												function_or_constructor_str = "constructor ";
												implicit_or_explicit_str = "";
											}
											if (MR_ptr) {
												std::string error_desc = std::string("Unable to verify that in the '") + function_decl->getQualifiedNameAsString()
													+ "' member function call expression, the argument corresponding to a parameter with lifetime label id '"
													+ abstract_lifetime1.m_id + "' has a lifetime that is strictly greater than the (minimum required) lifetime set when the object was initialized.";
												auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
										corresponding_present_scope_lifetime = sloi1;
									} else {
										/* The annotated lifetime of the parameter is not an annotated lifetime of the object type, if any. */
										bool replace_corresponding_present_scope_lifetime = (CScopeLifetimeInfo1::ECategory::AbstractLifetime == corresponding_present_scope_lifetime.m_category);
										if (!replace_corresponding_present_scope_lifetime) {
											replace_corresponding_present_scope_lifetime |= (!slti_second_can_be_assigned_to_first(corresponding_present_scope_lifetime, sloi1, Ctx, state1));
										}
										if (replace_corresponding_present_scope_lifetime) {
											/* The current stored value of the lifetime (is abstract or) doesn't seem to be shorter than the lifetime
											of the argument we just evaluated, so we will replace the stored one with the one we just evaluated. (It's
											possible that the already stored one could be shorter in the case, for example, where multiple constructor
											parameters are annotated with the same lifetime.) */
											corresponding_present_scope_lifetime = sloi1;
											corresponding_initialized_scope_lifetime = sloi1;
										}
									}
								}
								initialized_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_initialized_scope_lifetime );
								present_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
							}
						}

						for (const auto& present_lifetime_value_mapping1 : present_lifetime_value_map) {
							const auto& abstract_lifetime1 = present_lifetime_value_mapping1.first;
							const auto& lifetime_value1 = present_lifetime_value_mapping1.second;

							std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > lt1_constraint_shptrs;
							auto range1 = state1.m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(abstract_lifetime1);
							for (auto iter1 = range1.first; range1.second != iter1; ++iter1) {
								lt1_constraint_shptrs.push_back(iter1->second);
							}

							for (const auto& initialized_lifetime_value_mapping2 : initialized_lifetime_value_map) {
								const auto& abstract_lifetime2 = initialized_lifetime_value_mapping2.first;
								const auto& lifetime_value2 = initialized_lifetime_value_mapping2.second;

								for (const auto& constraint_shptr : lt1_constraint_shptrs) {
									if (constraint_shptr->m_second == abstract_lifetime2) {
										decltype(initialized_lifetime_value_mapping2.second) const * lhs_lifetime_value_ptr = nullptr;
										decltype(initialized_lifetime_value_mapping2.second) const * rhs_lifetime_value_ptr = nullptr;
										if (CPairwiseLifetimeConstraint::EYesNoDontKnow::Yes == constraint_shptr->second_can_be_assigned_to_first(abstract_lifetime1, abstract_lifetime2)) {
											lhs_lifetime_value_ptr = &lifetime_value1;
											rhs_lifetime_value_ptr = &lifetime_value2;
										} else if (CPairwiseLifetimeConstraint::EYesNoDontKnow::Yes == constraint_shptr->second_can_be_assigned_to_first(abstract_lifetime2, abstract_lifetime1)) {
											lhs_lifetime_value_ptr = &lifetime_value2;
											rhs_lifetime_value_ptr = &lifetime_value1;
										}
										if (lhs_lifetime_value_ptr && rhs_lifetime_value_ptr) {

											bool satisfies_checks = false;

											satisfies_checks = slti_second_can_be_assigned_to_first(*lhs_lifetime_value_ptr, *rhs_lifetime_value_ptr, Ctx, state1);

											if (!satisfies_checks) {
												std::string implicit_or_explicit_str;
												auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
												if (CXXMD) {
													implicit_or_explicit_str = "(implicit or explicit) member ";
												}
												std::string function_or_constructor_str = "function ";
												if (std::is_same_v<clang::CXXConstructExpr, TCallOrConstuctorExpr>) {
													function_or_constructor_str = "constructor ";
												}
												if (MR_ptr) {
													std::string error_desc = std::string("Unable to verify that in the '") + function_decl->getQualifiedNameAsString()
														+ "' " + implicit_or_explicit_str + function_or_constructor_str + "call expression, the specified '" + constraint_shptr->species_str()
														+ "' lifetime constraint (applied to lifetime label ids '"
														+ abstract_lifetime1.m_id + "' and '" + abstract_lifetime2.m_id + "') is satisfied.";
													auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
											}

											int q = 5;
										}
									}
								}
							}
						}

						expr_scope_sublifetimes.m_primary_lifetime_infos.clear();
						for (auto& CE_type_lifetime_value_mapping : CE_type_lifetime_value_map) {
							auto found_it = initialized_lifetime_value_map.find(CE_type_lifetime_value_mapping.first);
							if (initialized_lifetime_value_map.end() != found_it) {
								CE_type_lifetime_value_mapping.second = found_it->second;
							}
							expr_scope_sublifetimes.m_primary_lifetime_infos.push_back(CE_type_lifetime_value_mapping.second);
						}

						bool could_be_a_dynamic_container_accessor = false;
						{
							clang::Expr const * IOA_E = nullptr;
							if (CXXMCE) {
								IOA_E = CXXMCE->getImplicitObjectArgument();
							} else if (CXXOCE) {
								if (1 <= CXXOCE->getNumArgs()) {
									IOA_E = CXXOCE->getArg(0);
								} else {
									int q = 3;
								}
							}
							if (IOA_E) {
								if (!is_known_to_be_const_declared_variable(IOA_E)) {
									could_be_a_dynamic_container_accessor |= is_recognized_dynamic_container(IOA_E->getType());
								}
							}
						}

						if (!could_be_a_dynamic_container_accessor) {
							/* Any object referenced by a return value must outlive the call expression itself, or
							similarly, a hypothetical local variable declared just before the call expression. */
							CScopeLifetimeInfo1 hard_lower_bound_shallow_lifetime;
							hard_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
							hard_lower_bound_shallow_lifetime.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
							hard_lower_bound_shallow_lifetime.m_maybe_source_range = CE->getSourceRange();

							if (is_raw_pointer_or_equivalent(function_decl->getReturnType())
								|| function_decl->getReturnType()->isReferenceType()) {
								if (1 > expr_scope_sublifetimes.m_primary_lifetime_infos.size()) {
									/* If we have not determined a lifetime for this pointer return value, then we will just use
									the minimum (allowable) value (for now). */
									expr_scope_sublifetimes.m_primary_lifetime_infos.push_back(hard_lower_bound_shallow_lifetime);
								}
							}

							std::vector<CScopeLifetimeInfo1> arg_lifetimes;
							for (size_t i = 0; i < CE->getNumArgs(); i += 1) {
								auto arg_E = CE->getArg(i);
								auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, arg_E, Ctx, MR_ptr, Rewrite_ptr);
								if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
									/* Cannot properly evaluate because this is a template definition. Proper evaluation should
									occur in any instantiation of the template. */
									return {};
								}
								if (maybe_expr_lifetime_value.has_value()) {
									auto &arg_sli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									arg_lifetimes.push_back(arg_sli);
								}
							}
							if (CXXMCE) {
								auto IOA_E = CXXMCE->getImplicitObjectArgument();
								auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr);
								if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
									/* Cannot properly evaluate because this is a template definition. Proper evaluation should
									occur in any instantiation of the template. */
									return {};
								}
								if (maybe_expr_lifetime_value.has_value()) {
									auto &IOA_sli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									arg_lifetimes.push_back(IOA_sli);
								}
							}

							CScopeLifetimeInfo1 args_lower_bound_shallow_lifetime;
							args_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::Immortal;
							args_lower_bound_shallow_lifetime.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
							args_lower_bound_shallow_lifetime.m_maybe_source_range = CE->getSourceRange();

							for (auto& arg_lifetime : arg_lifetimes) {
								auto sli_shallow = arg_lifetime;
								sli_shallow.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
								if (first_is_known_to_be_contained_in_scope_of_second_shallow(hard_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
									if (!first_is_known_to_be_contained_in_scope_of_second_shallow(args_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
										if (!first_is_known_to_be_contained_in_scope_of_second_shallow(sli_shallow, args_lower_bound_shallow_lifetime, Ctx, state1)) {
											/* We don't seem to be able to unambiguously determine which argument lifetime is the
											shortest. So we'll just use the hard_lower_bound_shallow_lifetime. */
											args_lower_bound_shallow_lifetime = hard_lower_bound_shallow_lifetime;
											break;
										}
										/* The shallow part of the given lifetime is not known to outlive the current value of
										args_lower_bound_shallow_lifetime, so we will consider it to be the new
										args_lower_bound_shallow_lifetime value. */
										args_lower_bound_shallow_lifetime = sli_shallow;

									}
								}
							}
							/* Ok, we've determined a (but not necessarily "the") lower bound lifetime of all the (implicit
							or explicit) arguments (that live at least as long as the call expression).
							Presumably, any object referenced by a return value must have live at least that long. If any
							of the return value's (sub)lifetimes have not (yet) been determined to live that long, then
							we'll just set their value to the lower bound. */

							auto make_lifetime_at_least_args_lower_bound_shallow_lifetime = [&args_lower_bound_shallow_lifetime, &hard_lower_bound_shallow_lifetime, &Ctx, &state1](CScopeLifetimeInfo1& sli_ref) {
								auto sli_shallow = sli_ref;
								sli_shallow.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
								if (!first_is_known_to_be_contained_in_scope_of_second_shallow(args_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
									if (!first_is_known_to_be_contained_in_scope_of_second_shallow(sli_shallow, args_lower_bound_shallow_lifetime, Ctx, state1)) {
										/* We don't seem to be able to unambiguously determine which lifetime is shorter. So we'll just
										ensure that the lifetime is at least as long as the hard_lower_bound_shallow_lifetime. */
										if (!first_is_known_to_be_contained_in_scope_of_second_shallow(hard_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
											sli_shallow = hard_lower_bound_shallow_lifetime;
										}
									} else {
										/* The shallow part of the given lifetime does not outlive args_lower_bound_shallow_lifetime,
										so we will make the value of the shallow part of the given lifetime be
										args_lower_bound_shallow_lifetime. */
										sli_shallow = args_lower_bound_shallow_lifetime;
									}
									*(sli_shallow.m_sublifetimes_vlptr) = *(sli_ref.m_sublifetimes_vlptr);
									sli_ref = sli_shallow;
								}
							};

							expr_scope_sublifetimes.apply_to_all_lifetimes(make_lifetime_at_least_args_lower_bound_shallow_lifetime);
						} else {
							int q = 5;
						}

						/* set the expression lifetime values */
						auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
						expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
						expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = CE;

						/* Here we set the previously evaluated expression sublifetimes. */
						*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = expr_scope_sublifetimes;

						if (function_decl->getReturnType()->isReferenceType()) {
							/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
							same as the object it refers to (without an added level of indirection). */
							/* So we will attempt to remove one level of indirection from the expression lifetime. */
							auto& sublifetimes = expr_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;
							if (1 == sublifetimes.size()) {
								expr_scope_lifetime_info = sublifetimes.at(0);
							} else {
								if (0 != sublifetimes.size()) {
									/* unexpected */
									int q = 3;
								} else {
									/* The lifetime of the target object is not availiable. */
								}
							}
						}

						/* Here we put the evaluated expression lifetimes in "persistent" storage. */
						state1.m_expr_lifetime_values_map.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
					}
				}
			}
		}
		int q = 5;
		return {};
	}

	class MCSSSFunctionCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFunctionCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		template<typename TCallOrConstuctorExpr>
		static void s_handler2(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::FunctionDecl* function_decl, const TCallOrConstuctorExpr* CE) {

			if (CE != nullptr)
			{
				auto raw_SR = CE->getSourceRange();
				auto SR = nice_source_range(raw_SR, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				if (suppress_check_flag) {
					return;
				}

				function_call_handler2(state1, function_decl, CE, *(MR.Context), &MR, &Rewrite);
			}
		}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const CallExpr* CE) {

			if (CE != nullptr)
			{
				const clang::FunctionDecl* function_decl = CE->getDirectCallee();
				if (function_decl) {
					auto CXXMCE = dyn_cast<clang::CXXMemberCallExpr>(CE);
					if (CXXMCE) {
						auto CXXMD = CXXMCE->getMethodDecl();
						if (CXXMD) {
							s_handler2(MR, Rewrite, state1, function_decl, CE);
						} else {
							int q = 3;
							s_handler2(MR, Rewrite, state1, function_decl, CE);
						}
					} else {
						s_handler2(MR, Rewrite, state1, function_decl, CE);
					}
				}
			}
		}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::CXXConstructExpr* CXXCE) {

			if (CXXCE != nullptr)
			{
				const clang::FunctionDecl* function_decl = CXXCE->getConstructor();
				if (function_decl) {
					auto CXXCD = CXXCE->getConstructor();
					if (CXXCD) {
						s_handler2(MR, Rewrite, state1, function_decl, CXXCE);
					} else {
						int q = 3;
						s_handler2(MR, Rewrite, state1, function_decl, CXXCE);
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfunctioncall1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcssscxxoperatorcall1");
			const CXXConstructExpr* CXXCE = MR.Nodes.getNodeAs<clang::CXXConstructExpr>("mcsssfunctioncall1");
			if (CXXCE) {
				s_handler1(MR, Rewrite, m_state1, CXXCE);
			} else {
				if ((!CE) && CXXOCE) {
					CE = CXXOCE;
				}
				s_handler1(MR, Rewrite, m_state1, CE);
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSConstructorCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSConstructorCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler2(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::CXXConstructorDecl* constructor_decl, const clang::CXXConstructExpr* CE) {

			if (CE != nullptr)
			{
				auto raw_SR = CE->getSourceRange();
				auto SR = nice_source_range(raw_SR, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				if (suppress_check_flag) {
					return;
				}
				if (false && constructor_decl) {
					auto& func_decl = *constructor_decl;
					auto function_decl = constructor_decl;
					typedef clang::CXXConstructExpr TCallOrConstuctorExpr;

					const std::string function_name = constructor_decl->getNameAsString();
					const std::string qfunction_name = constructor_decl->getQualifiedNameAsString();

					auto constructor_declSR = nice_source_range(constructor_decl->getSourceRange(), Rewrite);
					SourceLocation constructor_declSL = constructor_declSR.getBegin();

					auto CXXMCE = dyn_cast<clang::CXXMemberCallExpr>(CE);
					auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);

					/* Here we're going to try to evaluate and store the lifetimes of this (constructor
					call) expression. We're presuming that the direct lifetime of any expression is a "temporary
					lifetime". But the expression may have other associated lifetimes defined by lifetime
					annotations on the type of the expression and the lifetime annotations on the type's
					constructor(s). */

					{
						auto eltv_iter = state1.m_expr_lifetime_values_map.find(CE);
						bool needs_processing = (state1.m_expr_lifetime_values_map.end() == eltv_iter);
						/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
						needs_processing = (needs_processing || (!(eltv_iter->second.m_errors_noted)));
						if (!needs_processing) {
							/* Already processed (and any errors noted). */
							return;
						}
					}
					IF_DEBUG(const auto CE_qtype_str = CE->getType().getAsString();)
					const auto CE_TypePtr1 = CE->getType().getTypePtr();
					auto tlta_iter = state1.m_type_lifetime_annotations_map.find(CE_TypePtr1);
					if (state1.m_type_lifetime_annotations_map.end() == tlta_iter) {
						if (CE_TypePtr1) {
							auto TD = CE_TypePtr1->getAsTagDecl();
							if (TD) {
								process_type_lifetime_annotations(*TD, state1, &MR, &Rewrite);
							}
						}
						tlta_iter = state1.m_type_lifetime_annotations_map.find(CE_TypePtr1);
						if (state1.m_type_lifetime_annotations_map.end() == tlta_iter) {
							return;
						}
					}
					/* The lifetime annotations of the (constructor call) expression's type. */
					auto const& type_lifetime_set_cref = tlta_iter->second.m_lifetime_set;
					/* We initialize the expression "sublifetimes" with the abstract lifetimes of the expression's
					type. Where we can, we will subsequently replace each of these abstract lifetimes with the
					corresponding concrete lifetime inferred from the (constructor call) expression. */
					auto expr_scope_sublifetimes = CScopeLifetimeInfo1Set(type_lifetime_set_cref);

					if (constructor_decl->isCopyOrMoveConstructor()) {
						if (1 == CE->getNumArgs()) {
							auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, state1, CE->getArg(0));
							if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
								/* Cannot properly evaluate because this is a template definition. Proper evaluation should
								occur in any instantiation of the template. */
								return;
							}
							if (maybe_expr_lifetime_value.has_value()) {
								CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
								expr_scope_sublifetimes = *(expr_slti.m_sublifetimes_vlptr);
							}

							{
								auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
								expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
								/* Here we try to evaluate the direct lifetime of the expression. */
								auto maybe_expr_owner = lower_bound_lifetime_owner_if_available(CE, *(MR.Context), state1, &MR, &Rewrite);
								if (maybe_expr_owner.has_value()) {
									auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_expr_owner.value(), *(MR.Context), state1);
									expr_scope_lifetime_info = sloi1;
								}
								{
									/* Here we set the evaluated expression sublifetimes. */
									*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = expr_scope_sublifetimes;
								}
								/* Here we put the evaluated expression lifetimes in "persistent" storage. */
								state1.m_expr_lifetime_values_map.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, true } );
							}
						} else {
							int q = 3;
						}
					} else {
						process_function_lifetime_annotations(*constructor_decl, state1, &MR, &Rewrite);
						auto flta_iter = state1.m_function_lifetime_annotations_map.find(constructor_decl);
						if (state1.m_function_lifetime_annotations_map.end() != flta_iter) {
							auto flta = flta_iter->second;

							std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> type_lifetime_value_map;

							if (CXXMCE || CXXCE) {
								if (CXXMCE) {
									/* This is a member function call. */
									auto found_it = state1.m_expr_lifetime_values_map.find(CXXMCE->getImplicitObjectArgument());
									if (state1.m_expr_lifetime_values_map.end() != found_it) {
										/* The actual lifetime values for this object are available. */
										auto& sublifetime_values_ref = found_it->second.m_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;

										const auto type_abstract_lifetime_end_iter = type_lifetime_set_cref.m_primary_lifetimes.cend();
										auto type_abstract_lifetime_iter1 = type_lifetime_set_cref.m_primary_lifetimes.cbegin();
										const auto expr_lifetime_values_end_iter = sublifetime_values_ref.end();
										auto expr_lifetime_values_iter1 = sublifetime_values_ref.begin();
										/* Here we are iterating over each of the object type's (annotated) abstract lifetimes. */
										for (; (type_abstract_lifetime_end_iter != type_abstract_lifetime_iter1) && (expr_lifetime_values_end_iter != expr_lifetime_values_iter1)
											; type_abstract_lifetime_iter1++, expr_lifetime_values_iter1++) {

											/* Here we're adding a mapping between each abstract lifetime (of the object's type) and the actual lifetime
											value (assigned at object initialization) associated with it. */
											type_lifetime_value_map.insert_or_assign( *type_abstract_lifetime_iter1, *expr_lifetime_values_iter1 );
										}
									}
								}
								if (0 == type_lifetime_value_map.size()) {
									/* Corresponding lifetime values for the object type's annotated (abstract) lifetimes don't seem to be
									available. So we'll just use the abstract lifetime itself. */
									for (auto& abstract_lifetime : type_lifetime_set_cref.m_primary_lifetimes) {
										type_lifetime_value_map.insert_or_assign( abstract_lifetime, abstract_lifetime );
									}
								}
							}
							std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> initialized_lifetime_value_map = type_lifetime_value_map;
							std::unordered_map<CAbstractLifetime, CScopeLifetimeInfo1> present_lifetime_value_map = type_lifetime_value_map;

							if (CXXCE) {
								/* Here we iterate over each constructor parameter with an associated abstract lifetime (annotation). */
								for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
									const auto abstract_lifetime1 = param_lifetime1.second.first_lifetime();
									CScopeLifetimeInfo1 corresponding_present_scope_lifetime = abstract_lifetime1;
									auto found_it = initialized_lifetime_value_map.find(abstract_lifetime1);
									if (initialized_lifetime_value_map.end() != found_it) {
										corresponding_present_scope_lifetime = found_it->second;
									}

									CScopeLifetimeInfo1 corresponding_initialized_scope_lifetime = corresponding_present_scope_lifetime;
									clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
									if (!arg1_EX) {
									} else {
										/* Now we try to evaluate the "concrete" lifetime of the corresponding argument in the contructor expression. */
										auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, state1, arg1_EX);
										if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
											/* Cannot properly evaluate because this is a template definition. Proper evaluation should
											occur in any instantiation of the template. */
											return;
										}
										auto maybe_arg_owner = lower_bound_lifetime_owner_if_available(arg1_EX, *(MR.Context), state1, &MR, &Rewrite);
										if (maybe_arg_owner.has_value()) {
											auto& arg_owner = maybe_arg_owner.value();
											auto sloi1 = scope_lifetime_info_from_lifetime_owner(arg_owner, *(MR.Context), state1);
											if (!slti_second_can_be_assigned_to_first(corresponding_present_scope_lifetime, sloi1, *(MR.Context), state1)) {
												/* The current stored value of the lifetime (is abstract or) doesn't seem to be shorter than the lifetime
												of the argument we just evaluated, so we will replace the stored one with the one we just evaluated. (It's
												possible that the already stored one could be shorter in the case, for example, where multiple constructor
												parameters are annotated with the same lifetime.) */
												corresponding_present_scope_lifetime = sloi1;
											}
										}
									}
									type_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
									initialized_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
									present_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
								}
							} else {
								/* Here we iterate over each function parameter with an associated abstract lifetime (annotation). */
								for (const auto& param_lifetime1 : flta.m_param_lifetime_map) {
									const auto abstract_lifetime1 = param_lifetime1.second.first_lifetime();
									CScopeLifetimeInfo1 corresponding_present_scope_lifetime = abstract_lifetime1;
									auto found_it = initialized_lifetime_value_map.find(abstract_lifetime1);
									if (initialized_lifetime_value_map.end() != found_it) {
										corresponding_present_scope_lifetime = found_it->second;
									}

									CScopeLifetimeInfo1 corresponding_initialized_scope_lifetime = corresponding_present_scope_lifetime;
									clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime1.first);
									if (!arg1_EX) {
									} else {
										/* Now we try to evaluate the "concrete" lifetime of the corresponding argument in the contructor expression. */
										auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, state1, arg1_EX);
										if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
											/* Cannot properly evaluate because this is a template definition. Proper evaluation should
											occur in any instantiation of the template. */
											return;
										}
										auto maybe_arg_owner = lower_bound_lifetime_owner_if_available(arg1_EX, *(MR.Context), state1, &MR, &Rewrite);
										if (maybe_arg_owner.has_value()) {
											auto& arg_owner = maybe_arg_owner.value();
											auto sloi1 = scope_lifetime_info_from_lifetime_owner(arg_owner, *(MR.Context), state1);

											auto found_it2 = type_lifetime_value_map.find(abstract_lifetime1);
											if (type_lifetime_value_map.end() != found_it2) {
												/* The annotated lifetime of the parameter is an annotated lifetime of the object type. */
												if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == corresponding_present_scope_lifetime.m_category) {
													corresponding_initialized_scope_lifetime = sloi1;
													int q = 3;
												}
												if (!slti_second_can_be_assigned_to_first(corresponding_initialized_scope_lifetime, sloi1, *(MR.Context), state1)) {
													std::string implicit_or_explicit_str;

													auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(&func_decl);
													if (CXXMD) {
														implicit_or_explicit_str = "(implicit or explicit) member ";
													}
													std::string function_or_constructor_str = "function ";
													if (std::is_same_v<clang::CXXConstructExpr, TCallOrConstuctorExpr>) {
														function_or_constructor_str = "constructor ";
														implicit_or_explicit_str = "";
													}
													std::string error_desc = std::string("Unable to verify that in the '") + func_decl.getQualifiedNameAsString()
														+ "' member function call expression, the argument corresponding to a parameter with lifetime label id '"
														+ abstract_lifetime1.m_id + "' has a lifetime that is strictly greater than the (minimum required) lifetime set when the object was initialized.";
													auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
												corresponding_present_scope_lifetime = sloi1;
											} else {
												/* The annotated lifetime of the parameter is not an annotated lifetime of the object type, if any. */
												if (!slti_second_can_be_assigned_to_first(corresponding_present_scope_lifetime, sloi1, *(MR.Context), state1)) {
													/* The current stored value of the lifetime (is abstract or) doesn't seem to be shorter than the lifetime
													of the argument we just evaluated, so we will replace the stored one with the one we just evaluated. (It's
													possible that the already stored one could be shorter in the case, for example, where multiple constructor
													parameters are annotated with the same lifetime.) */
													corresponding_present_scope_lifetime = sloi1;
													corresponding_initialized_scope_lifetime = sloi1;
												}
											}
										}
									}
									initialized_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_initialized_scope_lifetime );
									present_lifetime_value_map.insert_or_assign( abstract_lifetime1, corresponding_present_scope_lifetime );
								}
							}

							for (const auto& present_lifetime_value_mapping1 : present_lifetime_value_map) {
								const auto& abstract_lifetime1 = present_lifetime_value_mapping1.first;
								const auto& lifetime_value1 = present_lifetime_value_mapping1.second;

								std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > lt1_constraint_shptrs;
								auto range1 = state1.m_lhs_to_lifetime_constraint_shptr_mmap.equal_range(abstract_lifetime1);
								for (auto iter1 = range1.first; range1.second != iter1; ++iter1) {
									lt1_constraint_shptrs.push_back(iter1->second);
								}

								for (const auto& initialized_lifetime_value_mapping2 : initialized_lifetime_value_map) {
									const auto& abstract_lifetime2 = initialized_lifetime_value_mapping2.first;
									const auto& lifetime_value2 = initialized_lifetime_value_mapping2.second;

									for (const auto& constraint_shptr : lt1_constraint_shptrs) {
										if (constraint_shptr->m_second == abstract_lifetime2) {
											decltype(initialized_lifetime_value_mapping2.second) const * lhs_lifetime_value_ptr = nullptr;
											decltype(initialized_lifetime_value_mapping2.second) const * rhs_lifetime_value_ptr = nullptr;
											if (CPairwiseLifetimeConstraint::EYesNoDontKnow::Yes == constraint_shptr->second_can_be_assigned_to_first(abstract_lifetime1, abstract_lifetime2)) {
												lhs_lifetime_value_ptr = &lifetime_value1;
												rhs_lifetime_value_ptr = &lifetime_value2;
											} else if (CPairwiseLifetimeConstraint::EYesNoDontKnow::Yes == constraint_shptr->second_can_be_assigned_to_first(abstract_lifetime2, abstract_lifetime1)) {
												lhs_lifetime_value_ptr = &lifetime_value2;
												rhs_lifetime_value_ptr = &lifetime_value1;
											}
											if (lhs_lifetime_value_ptr && rhs_lifetime_value_ptr) {

												bool satisfies_checks = false;

												satisfies_checks = slti_second_can_be_assigned_to_first(*lhs_lifetime_value_ptr, *rhs_lifetime_value_ptr, *(MR.Context), state1);

												if (!satisfies_checks) {
													std::string implicit_or_explicit_str;
													auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(&func_decl);
													if (CXXMD) {
														implicit_or_explicit_str = "(implicit or explicit) member ";
													}
													std::string function_or_constructor_str = "function ";
													if (std::is_same_v<clang::CXXConstructExpr, TCallOrConstuctorExpr>) {
														function_or_constructor_str = "constructor ";
													}
													std::string error_desc = std::string("Unable to verify that in the '") + func_decl.getQualifiedNameAsString()
														+ "' " + implicit_or_explicit_str + function_or_constructor_str + "call expression, the specified '" + constraint_shptr->species_str()
														+ "' lifetime constraint (applied to lifetime label ids '"
														+ abstract_lifetime1.m_id + "' and '" + abstract_lifetime2.m_id + "') is satisfied.";
													auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}

												int q = 5;
											}
										}
									}
								}
							}

							for (auto& expr_scope_sublifetime_ref : expr_scope_sublifetimes.m_primary_lifetime_infos) {
								if (expr_scope_sublifetime_ref.m_maybe_abstract_lifetime.has_value()) {
									auto found_it = type_lifetime_value_map.find(expr_scope_sublifetime_ref.m_maybe_abstract_lifetime.value());
									if (type_lifetime_value_map.end() != found_it) {
										expr_scope_sublifetime_ref = found_it->second;
									}
								} else {
									int q = 3;
								}
							}

							/* set the expression lifetime values */
							auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
							expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
							/* Here we try to evaluate the direct lifetime of the (constructor) expression. */
							auto maybe_expr_owner = lower_bound_lifetime_owner_if_available(CE, *(MR.Context), state1, &MR, &Rewrite);
							if (maybe_expr_owner.has_value()) {
								auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_expr_owner.value(), *(MR.Context), state1);
								expr_scope_lifetime_info = sloi1;
							}
							/* Here we set the previously evaluated expression sublifetimes. */
							*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = expr_scope_sublifetimes;
							/* Here we put the evaluated expression lifetimes in "persistent" storage. */
							state1.m_expr_lifetime_values_map.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, true } );
						}
					}
				}
			}
		}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::CXXConstructExpr* CXXCE) {

			if (CXXCE != nullptr)
			{
				auto constructor_decl = CXXCE->getConstructor();
				if (constructor_decl) {
					s_handler2(MR, Rewrite, state1, constructor_decl, CXXCE);
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CXXConstructExpr* CXXCE = MR.Nodes.getNodeAs<clang::CXXConstructExpr>("mcsssconstructorcall1");
			if (CXXCE) {
				MCSSSFunctionCall::s_handler1(MR, Rewrite, m_state1, CXXCE);
				//s_handler1(MR, Rewrite, m_state1, CXXCE);
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		CMaybeExpressionLifetimeValuesWithHints retval;
		if (!E) {
			return retval;
		}

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
		if (MR_ptr && Rewrite_ptr) {
			auto& MR = *MR_ptr;
			auto& Rewrite = *Rewrite_ptr;

			auto raw_SR = E->getSourceRange();
			auto SR = nice_source_range(raw_SR, Rewrite);
			if (SR.isValid()) {
				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
				debug_source_location_str2 = debug_source_location_str;
				debug_source_text2 = debug_source_text;
#endif /*!NDEBUG*/
			}
		}
		{
			auto eltv_iter = state1.m_expr_lifetime_values_map.find(E);
			bool needs_processing = (state1.m_expr_lifetime_values_map.end() == eltv_iter);
			/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
			needs_processing = (needs_processing || ((!(eltv_iter->second.m_errors_noted)) && bool(MR_ptr)));
			if (!needs_processing) {
				/* Already processed (and any errors noted). */
				return eltv_iter->second;
			}
		}

		//const auto E_ii = IgnoreParenImpNoopCasts(E, Ctx);
		const auto E_ip = IgnoreParenNoopCasts(E, Ctx);

		auto qtype = E_ip->getType();
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		if (qtype.isNull()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			retval.m_failure_due_to_dependent_type_flag = true;
			return retval;
		}

		auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(E_ip);
		if (CXXCE) {
			auto elv_iter1 = state1.m_expr_lifetime_values_map.find(CXXCE);
			if (state1.m_expr_lifetime_values_map.end() == elv_iter1) {
				const clang::FunctionDecl* function_decl = CXXCE->getConstructor();
				if (function_decl) {
					function_call_handler2(state1, function_decl, CXXCE, Ctx, MR_ptr, Rewrite_ptr);
				}
			}

			elv_iter1 = state1.m_expr_lifetime_values_map.find(CXXCE);
			if (state1.m_expr_lifetime_values_map.end() != elv_iter1) {
				retval = elv_iter1->second;
				return retval;
			}
		} else {
			std::optional<CScopeLifetimeInfo1Set> maybe_sublifetimes;

			auto DSDRE = dyn_cast<const clang::DependentScopeDeclRefExpr>(E_ip);
			auto DRE = dyn_cast<const clang::DeclRefExpr>(E_ip);
			if (DRE) {
				auto value_decl = DRE->getDecl();
				if (value_decl) {
					auto VD = dyn_cast<const clang::VarDecl>(value_decl);
					if (VD) {
						auto maybe_decl_lifetime_value = evaluate_declaration_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							retval.m_failure_due_to_dependent_type_flag = true;
							return retval;
						}
						if (maybe_decl_lifetime_value.has_value()) {
							CScopeLifetimeInfo1& decl_slti = maybe_decl_lifetime_value.value().m_scope_lifetime_info;

							auto res1 = state1.m_expr_lifetime_values_map.insert_or_assign( E_ip, CExpressionLifetimeValues{ decl_slti, bool(MR_ptr) } );
							return (res1.first)->second;
						}
					}
				}
			} else if (DSDRE) {
				retval.m_failure_due_to_dependent_type_flag = true;
				return retval;
			} else {
				auto CXXILE = dyn_cast<const clang::CXXStdInitializerListExpr>(E_ip);
				auto ILE = dyn_cast<const clang::InitListExpr>(E_ip);
				auto CXXDSME = dyn_cast<const clang::CXXDependentScopeMemberExpr>(E_ip);
				auto ME = dyn_cast<const clang::MemberExpr>(E_ip);
				if (ME) {
					const auto VLD = ME->getMemberDecl();
					auto VLD_qtype = VLD->getType();
					IF_DEBUG(auto VLD_qtype_str = VLD_qtype.getAsString();)
					auto FD = dyn_cast<const clang::FieldDecl>(VLD);
					auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
					if (FD && !(ME->isBoundMemberFunction(Ctx))) {
						auto containing_ref_EX = containing_object_ref_expr_from_member_expr(ME);
						auto containing_qtype = containing_ref_EX->getType();
						IF_DEBUG(const auto containing_ref_qtype_str = containing_qtype.getAsString();)
						if (ME->isArrow()) {
							if (is_raw_pointer_or_equivalent(containing_qtype)) {
								containing_qtype = pointee_type_if_any(containing_qtype).value();
							} else {
								int q = 3;
							}
						}
						if (containing_qtype->isReferenceType()) {
							containing_qtype = containing_qtype->getPointeeType();
						}
						IF_DEBUG(const auto containing_qtype_str = containing_qtype.getAsString();)

						auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, containing_ref_EX, Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							retval.m_failure_due_to_dependent_type_flag = true;
							return retval;
						}
						if (maybe_expr_lifetime_value.has_value()) {
							CScopeLifetimeInfo1 owner_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

							if (is_raw_pointer_or_equivalent(containing_ref_EX->getType())) {
								/* In this case, containing_ref_EX does not evaluate to the parent/containing object, it
								evaluates to a pointer to the parent/containing object. So in this case there's a level of
								indirection in front of the (parent/containing) object we're interested in. So ideally, we
								will remove this extra level of indirection by replacing the lifetime value with its (sole)
								sublifetime value, if available. If the sublifetime value (i.e. the lifetime value of the
								pointer target) is not available, then the lifetime value of the pointer itself will serve
								as a lower bound for the lifetime (lower bound) value we're interested in. */
								auto& containing_ref_lv_sublifetimes = owner_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos;
								if (1 == containing_ref_lv_sublifetimes.size()) {
									auto adj_owner_slti = containing_ref_lv_sublifetimes.front();
									owner_slti = adj_owner_slti;
								}
							}
							auto const& owner_type_lifetime_values = owner_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos;

							/* expr_stli will (hopefully) be the returned lifetime value of the member field. We will start
							by initializing it with the lifetime value of its parent/containing object. */
							CScopeLifetimeInfo1 expr_slti = owner_slti;

							CPossessionLifetimeInfo1 pli;
							pli.m_maybe_field_source_range = FD->getSourceRange();
							/* Here we're adding info about the field's relationship to the parent from which its lifetime
							value is based. */
							expr_slti.m_possession_lifetime_info_chain.push_back(pli);

							/* While the member field's primary lifetime is based on its parent/containing object, its
							sublifetimes are, in general, not. We still need to evaluate them. */
							expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();

							auto maybe_tlta = type_lifetime_annotations_if_available(containing_qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
							if (maybe_tlta.has_value()) {
								/* The parent/containing type seems to have (zero or more) annotated lifetimes. */

								auto CXXTE = dyn_cast<const clang::CXXThisExpr>(containing_ref_EX);
								if (CXXTE) {
									/* Ok, the parent/owner of this member expression is a "this" expression. This means that the
									value of the parent/containing object's (sub)lifetimes remain abstract/"unevaluated". And, at
									the moment, the (sub)lifetimes of "this" expressions are not included in their lifetime values,
									so we have to obtain them here manually. But this just involves setting the parent/containing
									object's (sub)lifetime values to the set of the object type's (annotated) abstract lifetimes. */

									*(owner_slti.m_sublifetimes_vlptr) = CScopeLifetimeInfo1Set{ maybe_tlta.value()->m_lifetime_set };
								}

								auto& owner_type_abstract_lifetimes = maybe_tlta.value()->m_lifetime_set.m_primary_lifetimes;

								IF_DEBUG(auto sz1 = owner_type_abstract_lifetimes.size();)
								IF_DEBUG(auto sz2 = owner_type_lifetime_values.size();)

								auto falts_iter1 = state1.m_fielddecl_to_abstract_lifetime_map.find(FD);
								if (state1.m_fielddecl_to_abstract_lifetime_map.end() != falts_iter1) {
									/* The member field's type also seems to have (zero or more) annotated lifetimes. Presumably, its
									abstract lifetimes are a subset of its parent/containing object's abstract lifetimes. */

									/* For each of the member field's abstract lifetimes, we'll look for the corresponding abstract
									lifetime of the parent/containing object, and its corresponding evaluated (non-abstract) value,
									if available, and assign that value to the member field accordingly. */
									for (auto const& field_abstract_lifetime1 : falts_iter1->second.m_primary_lifetimes) {
										auto owner_tlv_iter1 = owner_type_lifetime_values.begin();
										auto owner_talt_iter1 = owner_type_abstract_lifetimes.begin();
										for (; (owner_type_abstract_lifetimes.end() != owner_talt_iter1) && (owner_type_lifetime_values.end() != owner_tlv_iter1)
											; ++owner_talt_iter1, ++owner_tlv_iter1) {

											if (field_abstract_lifetime1 == (*owner_talt_iter1)) {
												expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(*owner_tlv_iter1);
												break;
											}
										}
									}
								}
								int q = 5;
							}
							slti_set_default_lower_bound_lifetimes_where_needed(expr_slti, FD->getType());

							/* Here we put the evaluated expression lifetimes in "persistent" storage. */
							state1.m_expr_lifetime_values_map.insert_or_assign( E, CExpressionLifetimeValues{ expr_slti, bool(MR_ptr) } );
							if (E_ip != E) {
								auto res1 = state1.m_expr_lifetime_values_map.insert_or_assign( E_ip, CExpressionLifetimeValues{ expr_slti, bool(MR_ptr) } );
								retval = (res1.first)->second;
							}
							retval = CExpressionLifetimeValues{ expr_slti, bool(MR_ptr) };
							return retval;
						}
					} else if (VD) {
						auto maybe_decl_lifetime_value = evaluate_declaration_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							retval.m_failure_due_to_dependent_type_flag = true;
							return retval;
						}
						if (maybe_decl_lifetime_value.has_value()) {
							CScopeLifetimeInfo1& decl_slti = maybe_decl_lifetime_value.value().m_scope_lifetime_info;

							maybe_sublifetimes = *(decl_slti.m_sublifetimes_vlptr);
						}
					} else {
						int q = 5;
					}
				} else if (CXXDSME) {
					retval.m_failure_due_to_dependent_type_flag = true;
					return retval;
				} else if (ILE) {
					auto sf_ILE = ILE;
					sf_ILE = ILE->getSemanticForm();
					if (!sf_ILE) {
						sf_ILE = ILE;
					}
					if ((1 <= sf_ILE->getNumInits()) && (sf_ILE->getInit(0))) {
						auto first_init_E = sf_ILE->getInit(0);
						bool all_the_same_type = true;
						if (true) {
							auto sf_ILE_qtype = sf_ILE->getType();
							IF_DEBUG(auto sf_ILE_qtype_str = sf_ILE_qtype.getAsString();)
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL(sf_ILE_qtype, retval);
							all_the_same_type = sf_ILE_qtype->isArrayType();
						} else {
							const auto first_qtype = first_init_E->getType();
							for (size_t i = 1; sf_ILE->getNumInits() > i; i += 1) {
								auto l_qtype = sf_ILE->getInit(i)->getType();
								if (first_qtype != l_qtype) {
									all_the_same_type = false;
									break;
								}
							}
						}
						if (all_the_same_type) {
							std::vector<CScopeLifetimeInfo1> element_slis;
							for (size_t i = 0; i < sf_ILE->getNumInits(); i += 1) {
								auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, sf_ILE->getInit(i), Ctx, MR_ptr, Rewrite_ptr);
								if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
									/* Cannot properly evaluate because this is a template definition. Proper evaluation should
									occur in any instantiation of the template. */
									retval.m_failure_due_to_dependent_type_flag = true;
									return retval;
								}
								if (maybe_expr_lifetime_value.has_value()) {
									CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									element_slis.push_back(expr_slti);
								} else {
									element_slis.push_back(CScopeLifetimeInfo1{});
								}
							}
							/* Here we deem the lower bound lifetime of a (homogeneous) initializer list to be just the
							lower bound lifetime of the set of all of its elements. */
							retval = CExpressionLifetimeValues{ lower_bound_lifetime_sli(element_slis, Ctx, state1), bool(MR_ptr) };
							return retval;
						}
					}
				} else if (CXXILE) {
					const auto sub_E_ip = IgnoreParenNoopCasts(CXXILE->getSubExpr(), Ctx);
					if (sub_E_ip && (sub_E_ip != CXXILE)) {
						/* We're expecting sub_E_ip to be a clang::InitListExpr (pointer). */
						return evaluate_expression_lower_bound_lifetimes(state1, sub_E_ip, Ctx, MR_ptr, Rewrite_ptr);
					}
				} else if (is_raw_pointer_or_equivalent(qtype)) {
					auto target_EX = raw_pointer_target_expression_if_available(E_ip, Ctx, state1);
					auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, target_EX, Ctx, MR_ptr, Rewrite_ptr);
					if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						retval.m_failure_due_to_dependent_type_flag = true;
						return retval;
					}
					if (maybe_expr_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
						maybe_sublifetimes = { expr_slti };
					}
				}
			}

			auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
			expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
			/* Here we try to evaluate the direct lifetime of the expression. */
			auto maybe_expr_owner = lower_bound_lifetime_owner_if_available(E_ip, Ctx, state1, MR_ptr, Rewrite_ptr);
			if (maybe_expr_owner.has_value()) {
				auto& expr_owner = maybe_expr_owner.value();
				if (std::holds_alternative<const VarDecl*>(expr_owner)) {
					auto VD = std::get<const VarDecl*>(expr_owner);
					auto maybe_decl_lifetime_value = evaluate_declaration_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
					if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						retval.m_failure_due_to_dependent_type_flag = true;
						return retval;
					}
				}

				auto sloi1 = scope_lifetime_info_from_lifetime_owner(maybe_expr_owner.value(), Ctx, state1);
				expr_scope_lifetime_info = sloi1;
			}
			if (maybe_sublifetimes.has_value()) {
				/* Here we set the evaluated expression sublifetimes. */
				*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = maybe_sublifetimes.value();
			}
			if (qtype->isReferenceType()) {
				/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
				same as the object it refers to (without an added level of indirection). */
				/* So we will attempt to remove one level of indirection from the expression lifetime. */
				auto& sublifetimes = expr_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;
				if (1 == sublifetimes.size()) {
					expr_scope_lifetime_info = sublifetimes.at(0);
				} else {
					/* unexpected */
					int q = 3;
					expr_scope_lifetime_info = CScopeLifetimeInfo1{};
				}
			}
			/* Here we put the evaluated expression lifetimes in "persistent" storage. */
			state1.m_expr_lifetime_values_map.insert_or_assign( E, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
			if (E_ip != E) {
				auto res1 = state1.m_expr_lifetime_values_map.insert_or_assign( E_ip, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
				retval = (res1.first)->second;
			}
			retval = CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) };
		}
		return retval;
	}
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::Expr* E) {
		return evaluate_expression_lower_bound_lifetimes(state1, E, *(MR.Context), &MR, &Rewrite);
	}

	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		CMaybeVariableLifetimeValuesWithHints retval;
		if (!DD) {
			return retval;
		}

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
		bool suppress_check_flag = false;
		if (MR_ptr && Rewrite_ptr) {
			auto& MR = *MR_ptr;
			auto& Rewrite = *Rewrite_ptr;

			auto raw_SR = DD->getSourceRange();
			auto SR = nice_source_range(raw_SR, Rewrite);
			if (SR.isValid()) {
				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
				debug_source_location_str2 = debug_source_location_str;
				debug_source_text2 = debug_source_text;
#endif /*!NDEBUG*/
			}

			//RETURN_IF_FILTERED_OUT_BY_LOCATION1;
			if ((!SR.isValid()) || filtered_out_by_location(MR, SR.getBegin())) {
				suppress_check_flag |= true;
			}

			suppress_check_flag |= state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
		}

		auto SR = Rewrite_ptr ? nice_source_range(DD->getSourceRange(), *Rewrite_ptr) : DD->getSourceRange();

		const auto qtype = DD->getType();
		const std::string qtype_str = DD->getType().getAsString();
		auto DD_qtype = DD->getType();
		IF_DEBUG(auto DD_qtype_str = DD_qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL(DD_qtype, retval);
		const auto TST = DD_qtype->getAs<clang::TemplateSpecializationType>();

		auto VD = dyn_cast<const clang::VarDecl>(DD);
		if (VD) {
			auto vltv_iter1 = state1.m_vardecl_lifetime_values_map.find(VD);
			bool needs_processing = (state1.m_vardecl_lifetime_values_map.end() == vltv_iter1);
			/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
			needs_processing = (needs_processing || ((!(vltv_iter1->second.m_errors_noted)) && bool(MR_ptr)));
			if (!needs_processing) {
				/* Already processed (and any errors noted). */
				return vltv_iter1->second;
			}

			const auto var_qualified_name = VD->getQualifiedNameAsString();

			process_function_lifetime_annotations(*VD, state1, MR_ptr, Rewrite_ptr);

			auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
			if (PVD) {
				auto FD = function_from_param(PVD);
				if (FD) {
					process_function_lifetime_annotations(*FD, state1, MR_ptr, Rewrite_ptr);
					auto flta_iter1 = state1.m_function_lifetime_annotations_map.find(FD);
					if (state1.m_function_lifetime_annotations_map.end() != flta_iter1) {
						auto& param_lifetime_map_ref = flta_iter1->second.m_param_lifetime_map;

						/* Here we iterate over the function's parameters to determine the parameter ordinal
						of our parameter declaration. (Not necessarily the most efficient set up.) */
						param_ordinal_t param_ordinal = 1;
						for (auto param_iter = FD->param_begin(); FD->param_end() != param_iter; ++param_iter, param_ordinal += 1) {
							auto param = (*param_iter);
							auto qtype = param->getType();
							IF_DEBUG(const std::string qtype_str = qtype.getAsString();)
							if (param == PVD) {
								auto plm_iter = param_lifetime_map_ref.find(param_ordinal);
								if (param_lifetime_map_ref.end() != plm_iter) {
									/* Turns out that our parameter seems to have a lifetime annotation. */
									CScopeLifetimeInfo1 sli2;
									if (qtype->isReferenceType()) {
										if (plm_iter->second.m_primary_lifetimes.size() == 1) {
											/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
											same as the object it refers to (without an added level of indirection). */
											auto& alt = plm_iter->second.m_primary_lifetimes.front();
											sli2.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
											sli2.m_maybe_abstract_lifetime = alt;
											sli2.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
											sli2.m_maybe_source_range = VD->getSourceRange();

											*(sli2.m_sublifetimes_vlptr) = *(alt.m_sublifetimes_vlptr);
										} else {
											/* invalid? A reference parameter with lifetime annotation should have exactly one primary
											lifetime label. */
											int q = 3;

											const auto sl_storage_duration = VD->getStorageDuration();
											const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
											sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
											sli2.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
											sli2.m_maybe_source_range = VD->getSourceRange();
										}
									} else {
										const auto sl_storage_duration = VD->getStorageDuration();
										const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
										sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
										sli2.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
										sli2.m_maybe_source_range = VD->getSourceRange();

										/* Since, unlike regular variables, the initialization value of parameter variables (i.e. the
										corresponding argument) is not available in the declaration. So we use the annotated abstract
										lifetimes themselves as the lifetime (initialization) values. */
										*(sli2.m_sublifetimes_vlptr) = plm_iter->second;
									}
									auto res1 = state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, CVariableLifetimeValues{ sli2, bool(MR_ptr) });
									retval = res1.first->second;
									//return retval;
								} else {
									int q = 5;
								}
							}
						}
					}
				} else {
					int q = 5;
				}
			} else {
				process_variable_lifetime_annotations(*VD, state1, MR_ptr, Rewrite_ptr);

				auto maybe_tlta = type_lifetime_annotations_if_available(*VD, state1, MR_ptr, Rewrite_ptr);

				if (maybe_tlta.has_value()) {
					{
						if (VD->hasInit()) {
							auto init_E = VD->getInit();
							assert(init_E);

							auto ILE = dyn_cast<const clang::InitListExpr>(init_E);
							auto VD_qtype = VD->getType();
							IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);
							if (ILE && VD_qtype->isAggregateType()) {
								if (!(VD_qtype->isPointerType() || VD_qtype->isReferenceType())) {
									if (MR_ptr && (!suppress_check_flag)) {
										const std::string error_desc = std::string("Aggregate initialization (of '") + var_qualified_name
											+ "') is not (currently) supported for types (like '" + qtype_str + "') that have (explicit or "
											+ "implicit) lifetime annotations.";
										auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}

							auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, init_E, Ctx, MR_ptr, Rewrite_ptr);
							if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
								/* Cannot properly evaluate because this is a template definition. Proper evaluation should
								occur in any instantiation of the template. */
								retval.m_failure_due_to_dependent_type_flag = true;
								//return retval;
							}
							if (maybe_expr_lifetime_value.has_value()) {
								if (qtype->isReferenceType()) {
									/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
									same as the object it refers to (without an added level of indirection). */
									retval = CVariableLifetimeValues{ maybe_expr_lifetime_value.value().m_scope_lifetime_info, bool(MR_ptr) };
								} else {
									CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

									CScopeLifetimeInfo1 sli2;

									const auto sl_storage_duration = VD->getStorageDuration();
									const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
									sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
									sli2.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
									sli2.m_maybe_source_range = VD->getSourceRange();
									*(sli2.m_sublifetimes_vlptr) = *(expr_slti.m_sublifetimes_vlptr);

									auto res1 = state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, CVariableLifetimeValues{ sli2, bool(MR_ptr) });
									retval = res1.first->second;
									//return retval;
								}
							} else {
								int q = 5;
							}
						} else {
							if (MR_ptr && (!suppress_check_flag)) {
								const std::string error_desc = std::string("(Non-parameter) variable '")
									+ var_qualified_name + "' of type '" + qtype_str + "' has as an associated lifetime label which "
									+ "requires that the decalaration have an initialization value (that doesn't seem to be present).";
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				} else if (qtype->isPointerType()) {
					auto sli1 = scope_lifetime_info_from_lifetime_owner(VD, Ctx, state1);
					auto res1 = state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, CVariableLifetimeValues{ sli1, bool(MR_ptr) });
					retval = res1.first->second;
					//return retval;
				}
			}

			const auto storage_duration = VD->getStorageDuration();
			const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();

			if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
				bool satisfies_checks = false;
				if (CXXRD) {
					auto type_name1 = CXXRD->getQualifiedNameAsString();
					const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
					if (tmplt_CXXRD) {
						type_name1 = tmplt_CXXRD->getQualifiedNameAsString();
					}

					DECLARE_CACHED_CONST_STRING(mse_rsv_static_immutable_obj_str1, mse_namespace_str() + "::rsv::TStaticImmutableObj");
					static const std::string std_atomic_str = std::string("std::atomic");
					DECLARE_CACHED_CONST_STRING(mse_AsyncSharedV2ReadWriteAccessRequester_str, mse_namespace_str() + "::TAsyncSharedV2ReadWriteAccessRequester");
					DECLARE_CACHED_CONST_STRING(mse_AsyncSharedV2ReadOnlyAccessRequester_str, mse_namespace_str() + "::TAsyncSharedV2ReadOnlyAccessRequester");
					DECLARE_CACHED_CONST_STRING(mse_TAsyncSharedV2ImmutableFixedPointer_str, mse_namespace_str() + "::TAsyncSharedV2ImmutableFixedPointer");
					DECLARE_CACHED_CONST_STRING(mse_TAsyncSharedV2AtomicFixedPointer_str, mse_namespace_str() + "::TAsyncSharedV2AtomicFixedPointer");
					DECLARE_CACHED_CONST_STRING(mse_rsv_ThreadLocalObj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");

					if ((type_name1 == mse_rsv_static_immutable_obj_str1)
						|| (type_name1 == std_atomic_str)
						|| (type_name1 == mse_AsyncSharedV2ReadWriteAccessRequester_str)
						|| (type_name1 == mse_AsyncSharedV2ReadOnlyAccessRequester_str)
						|| (type_name1 == mse_TAsyncSharedV2ImmutableFixedPointer_str)
						|| (type_name1 == mse_TAsyncSharedV2AtomicFixedPointer_str)
						|| ((type_name1 == mse_rsv_ThreadLocalObj_str) && (clang::StorageDuration::SD_Thread == storage_duration))
						) {
						satisfies_checks = true;
					}
				}

				if (!satisfies_checks) {
					if (clang::StorageDuration::SD_Static == storage_duration) {
						DECLARE_CACHED_CONST_STRING(const_char_star_str, "const char *");
						if ((qtype.isConstQualified()) && (is_async_shareable(qtype))) {
							satisfies_checks = true;
						} else if (qtype.getAsString() == const_char_star_str) {
							/* This isn't technically safe, but presumably this is likely
							to be a string literal, which should be fine, so for now we'll
							let it go. */
							satisfies_checks = true;
						} else {
							if (MR_ptr && (!suppress_check_flag)) {
								const std::string error_desc = std::string("Unable to verify the safety of variable '")
									+ var_qualified_name + "' of type '" + qtype_str + "' with 'static storage duration'. "
									+ "'static storage duration' is supported for eligible types wrapped in the "
									+ "'mse::rsv::TStaticImmutableObj<>' transparent template wrapper. Other supported wrappers include: "
									+ "mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
									+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>. "
									+ "Note that objects with 'static storage duration' may be simultaneously accessible from different threads "
									+ "and so have more stringent safety requirements than objects with 'thread_local storage duration'.";
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					} else {
						assert(clang::StorageDuration::SD_Thread == storage_duration);
						if (true || is_async_shareable(qtype)) {
							satisfies_checks = true;
						} else {
							if (MR_ptr && (!suppress_check_flag)) {
								const std::string error_desc = std::string("Unable to verify the safety of variable '")
									+ var_qualified_name + "' of type '" + qtype_str + "' with 'thread local storage duration'. "
									+ "'thread local storage duration' is supported for eligible types wrapped in the "
									+ "'mse::rsv::TThreadLocalObj<>' transparent template wrapper. Other supported wrappers include: "
									+ "mse::rsv::TStaticImmutableObj<>, mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
									+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				}
			}
			if (CXXRD) {
				auto type_name1 = CXXRD->getQualifiedNameAsString();
				const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
				if (tmplt_CXXRD) {
					type_name1 = tmplt_CXXRD->getQualifiedNameAsString();
				}

				DECLARE_CACHED_CONST_STRING(mse_rsv_static_immutable_obj_str1, mse_namespace_str() + "::rsv::TStaticImmutableObj");
				DECLARE_CACHED_CONST_STRING(mse_rsv_ThreadLocalObj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");

				if (type_name1 == mse_rsv_static_immutable_obj_str1) {
					if (clang::StorageDuration::SD_Static != storage_duration) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
								+ mse_rsv_static_immutable_obj_str1 + "' must be declared to have 'static' storage duration.";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				} else if (type_name1 == mse_rsv_ThreadLocalObj_str) {
					if (clang::StorageDuration::SD_Thread != storage_duration) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
								+ mse_rsv_ThreadLocalObj_str + "' must be declared to have 'thread_local' storage duration.";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			}

			if (qtype.getTypePtr()->isScalarType()) {
				const auto init_EX = VD->getInit();
				if (!init_EX) {
					auto PVD = dyn_cast<const ParmVarDecl>(VD);
					if (!PVD) {
						if (!VD->isExternallyDeclarable()) {
							if (MR_ptr && (!suppress_check_flag)) {
								const std::string error_desc = std::string("Uninitialized ")
									+ "scalar variable '" + VD->getNameAsString() + "' (of type '"
									+ qtype.getAsString() + "') ";
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						} else {
							/* todo: emit error that (uninitialized) 'extern' variables
							aren't supported?  */;
						}
					}
				}
			}

			const auto init_EX = VD->getInit();
			if (init_EX) {
				auto res = statement_makes_reference_to_decl(*VD, *init_EX);
				if (res) {
					if (MR_ptr && (!suppress_check_flag)) {
						const std::string error_desc = std::string("Reference to variable '")
							+ VD->getNameAsString() + "' before the completion of its "
							+ "construction/initialization is not supported.";
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
			} else if (false && VD->isExternallyDeclarable()) {
				if (MR_ptr && (!suppress_check_flag)) {
					const std::string error_desc = std::string("\"External\"/inline ")
						+ "variable declarations (such as the declaration of "
						+ VD->getNameAsString() + "' of type '" + qtype.getAsString()
						+ "') are not currently supported.";
					auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}

			if (!(retval.has_value())) {
				/* set the expression lifetime values */
				auto vardecl_scope_lifetime_info = CScopeLifetimeInfo1{};
				vardecl_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
				vardecl_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
				vardecl_scope_lifetime_info.m_maybe_source_range = VD->getSourceRange();
				vardecl_scope_lifetime_info.m_maybe_corresponding_cpp_element = VD;

				auto reference_removed_qtype = VD->getType();
				auto VD_qtype = VD->getType();
				IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL(VD_qtype, retval);
				if (VD_qtype->isReferenceType()) {
					/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
					same as the object it refers to (without an added level of indirection). */
					/* So we will attempt to remove one level of indirection from the expression lifetime. */
					auto& sublifetimes = vardecl_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;
					if (1 == sublifetimes.size()) {
						vardecl_scope_lifetime_info = sublifetimes.at(0);
						reference_removed_qtype = VD_qtype->getPointeeType();
					} else {
						if (0 != sublifetimes.size()) {
							/* unexpected */
							int q = 3;
						} else {
							/* The lifetime of the target object is not availiable. */
						}
					}
				}
				slti_set_default_lower_bound_lifetimes_where_needed(vardecl_scope_lifetime_info, reference_removed_qtype);

				auto vltvs = CVariableLifetimeValues{ vardecl_scope_lifetime_info, bool(MR_ptr) };
				/* Here we put the evaluated expression lifetimes in "persistent" storage. */
				state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, vltvs );
				retval = vltvs;
				//return retval;
			}
		} else {
			auto FD = dyn_cast<const clang::FieldDecl>(DD);
			if (FD) {
				if (false && (qtype.getTypePtr()->isPointerType() || qtype.getTypePtr()->isReferenceType())) {
					/* These are handled in MCSSSRecordDecl2. */
				} else if (qtype.getTypePtr()->isScalarType()) {
					const auto* init_EX = FD->getInClassInitializer();
					if (!init_EX) {
						const auto grandparent_DC = FD->getParent()->getParentFunctionOrMethod();
						bool is_lambda_capture_field = false;

						const auto& parents = Ctx.getParents(*(FD->getParent()));
						if ( !(parents.empty()) ) {
							const auto LE = parents[0].get<LambdaExpr>();
							if (LE) {
								is_lambda_capture_field = true;
							}
						}
						if (!is_lambda_capture_field) {
							if (qtype.getTypePtr()->isPointerType()) {
							} else {
								if (MR_ptr && (!suppress_check_flag)) {
									const std::string error_desc = std::string("(Non-pointer) scalar fields (such those of type '")
										+ qtype.getAsString() + "') require direct initializers.";
									auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						}
					}
				}

				process_type_lifetime_annotations(*FD, state1, MR_ptr, Rewrite_ptr);
			}
		}

		const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto name = CXXRD->getQualifiedNameAsString();
			const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
			if (tmplt_CXXRD) {
				name = tmplt_CXXRD->getQualifiedNameAsString();
			}
			DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncShareableObj_str1, mse_namespace_str() + "::rsv::TAsyncShareableObj");
			DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncPassableObj_str1, mse_namespace_str() + "::rsv::TAsyncPassableObj");
			DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncShareableAndPassableObj_str1, mse_namespace_str() + "::rsv::TAsyncShareableAndPassableObj");
			DECLARE_CACHED_CONST_STRING(mse_rsv_TFParam_str, mse_namespace_str() + "::rsv::TFParam");
			static const std::string std_unique_ptr_str = "std::unique_ptr";
			if (mse_rsv_TAsyncShareableObj_str1 == name) {
				if (1 == CXXRD->getNumBases()) {
					const auto& base = *(CXXRD->bases_begin());
					const auto base_qtype = base.getType();
					const auto base_qtype_str = base_qtype.getAsString();
					if (!is_async_shareable(base_qtype)) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableObj<> template, '"
								+ base_qtype_str + "', is eligible to be safely shared (among threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				} else {
					/* This branch shouldn't happen. Unless the library's been changed somehow. */
				}
			} else if (mse_rsv_TAsyncPassableObj_str1 == name) {
				if (1 == CXXRD->getNumBases()) {
					const auto& base = *(CXXRD->bases_begin());
					const auto base_qtype = base.getType();
					const auto base_qtype_str = base_qtype.getAsString();
					if (!is_async_passable(base_qtype)) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncPassableObj<> template, '"
								+ base_qtype_str + "', is eligible to be safely passed (between threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				} else {
					/* This branch shouldn't happen. Unless the library's been changed somehow. */
				}
			} else if (mse_rsv_TAsyncShareableAndPassableObj_str1 == name) {
				if (1 == CXXRD->getNumBases()) {
					const auto& base = *(CXXRD->bases_begin());
					const auto base_qtype = base.getType();
					const auto base_qtype_str = base_qtype.getAsString();
					if ((!is_async_shareable(base_qtype)) || (!is_async_passable(base_qtype))) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableAndPassableObj<> template, '"
								+ base_qtype_str + "', is eligible to be safely shared and passed (among threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				} else {
					/* This branch shouldn't happen. Unless the library's been changed somehow. */
				}
			} else if (mse_rsv_TFParam_str == name) {
				bool satisfies_checks = false;
				auto VD = dyn_cast<const clang::VarDecl>(DD);
				if (VD) {
					auto FND = dyn_cast<const clang::FunctionDecl>(VD->getParentFunctionOrMethod());
					if (FND) {
						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							satisfies_checks = true;
						} else {
							auto CE = dyn_cast<const clang::CallExpr>(IgnoreParenNoopCasts(VD->getInit(), Ctx));
							if (CE) {
								auto function_decl = CE->getDirectCallee();
								auto num_args = CE->getNumArgs();
								if (function_decl) {
									std::string qualified_function_name = function_decl->getQualifiedNameAsString();
									DECLARE_CACHED_CONST_STRING(as_an_fparam_str, mse_namespace_str() + "::rsv::as_an_fparam");
									if ((as_an_fparam_str == qualified_function_name)) {
										if (1 == num_args) {
											satisfies_checks = true;
										}
									}
								}
							}
						}
					}
				}
				if (!satisfies_checks) {
					if (MR_ptr && (!suppress_check_flag)) {
						const std::string error_desc = std::string("Unsupported use of ")
							+ "mse::rsv::TFParam<> (in type '" + name + "'). ";
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				}
			} else if (qtype.getTypePtr()->isUnionType()) {
				if (MR_ptr && (!suppress_check_flag)) {
					const std::string error_desc = std::string("Native unions (such as '" + qtype.getAsString() + "') are not ")
						+ "supported. ";
					auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			} else if (true && (std_unique_ptr_str == name)) {
				if (!qtype.isConstQualified()) {
					if (MR_ptr && (!suppress_check_flag)) {
						const std::string error_desc = std::string("std::unique_ptr<>s that are not const qualified are not supported. ")
							+ "Consider using a reference counting pointer from the SaferCPlusPlus library. ";
						auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				} else {
					const auto init_EX = VD->getInit();
					bool null_initialization = true;
					if (init_EX) {
						null_initialization = is_nullptr_literal(init_EX, Ctx);
						if (!null_initialization) {
							const auto init_EX_ii = IgnoreParenNoopCasts(init_EX, Ctx);
							const auto CXXCE = dyn_cast<const CXXConstructExpr>(init_EX_ii);
							if (CXXCE) {
								if (1 == CXXCE->getNumArgs()) {
									null_initialization = is_nullptr_literal(CXXCE->getArg(0), Ctx);
								} else if (0 == CXXCE->getNumArgs()) {
									null_initialization = true;
								}
							}
						}
					}
					if (null_initialization) {
						if (MR_ptr && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Null/default initialization of ")
								+ "std::unique_ptr<>s (such as those of type '" + qtype.getAsString()
								+ "') is not supported.";
							auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			} else {
				auto check_for_and_handle_unsupported_element = [MR_ptr, suppress_check_flag, &SR](const clang::QualType& qtype, CTUState& state1) {
					std::string element_name;
					const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
					if (l_CXXRD) {
						element_name = l_CXXRD->getQualifiedNameAsString();
					} else {
						element_name = qtype.getAsString();
					}

					{
						auto uei_ptr = unsupported_element_info_ptr(element_name);
						if (uei_ptr) {
							const auto& unsupported_element_info = *uei_ptr;
							if (MR_ptr && (!suppress_check_flag)) {
								std::string error_desc = std::string("'") + element_name + std::string("' is not ")
									+ "supported (in this declaration of type '" + qtype.getAsString() + "'). ";
								if ("" != unsupported_element_info.m_recommended_alternative) {
									error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
								}
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				};
				//check_for_and_handle_unsupported_element(qtype, state1);
				//apply_to_component_types_if_any(qtype, check_for_and_handle_unsupported_element, state1);

				auto check_for_and_handle_unsupported_element2 = [MR_ptr, suppress_check_flag](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
					auto qtype = typeLoc.getType();
					std::string element_name;
					const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
					if (l_CXXRD) {
						element_name = l_CXXRD->getQualifiedNameAsString();
					} else {
						element_name = qtype.getAsString();
					}

					{
						auto uei_ptr = unsupported_element_info_ptr(element_name);
						if (uei_ptr) {
							const auto& unsupported_element_info = *uei_ptr;
							if (MR_ptr && (!suppress_check_flag)) {
								std::string error_desc = std::string("'") + element_name + std::string("' is not ")
									+ "supported (in type '" + qtype.getAsString() + "' used in this declaration). ";
								if ("" != unsupported_element_info.m_recommended_alternative) {
									error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
								}
								auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), typeLoc.getSourceRange().getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				};
				auto tsi_ptr = DD->getTypeSourceInfo();
				if (tsi_ptr) {
					check_for_and_handle_unsupported_element2(tsi_ptr->getTypeLoc(), SR, state1);
					apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), check_for_and_handle_unsupported_element2, state1);
				}
			}
		} else {
			std::string unsupported_type_str;
			if (qtype.getTypePtr()->isArrayType()) {
				unsupported_type_str = "Native array";
			} else if (qtype.getTypePtr()->isUnionType()) {
				unsupported_type_str = "Native union";
			}
			if ("" != unsupported_type_str) {
				if (MR_ptr && (!suppress_check_flag)) {
					const std::string error_desc = unsupported_type_str + std::string("s are not ")
						+ "supported (in this declaration of type '" + qtype.getAsString() + "'). ";
					auto res = state1.m_error_records.emplace(CErrorRecord(*(MR_ptr->SourceManager), SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}
		}
		return retval;
	}
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD) {
		return evaluate_declaration_lower_bound_lifetimes(state1, DD, *(MR.Context), &MR, &Rewrite);
	}

	class MCSSSConstructionInitializer : public MatchFinder::MatchCallback
	{
	public:
		MCSSSConstructionInitializer (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CXXCtorInitializer* CXXCI = MR.Nodes.getNodeAs<clang::CXXCtorInitializer>("mcsssconstructioninitializer1");

			if ((CXXCI != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = nice_source_range(CXXCI->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CXXCI, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CXXCIISR);
				if (suppress_check_flag) {
					return;
				}
				if (!(CXXCI->isWritten())) {
					/* We think this construction initializer is implicit and generated by the compiler. */
					return;
				}

				const auto FD = CXXCI->getMember();
				if (!FD) {
					/* This is presumably a base class initializer (rather than a member field initializer). */
					return;
				}
				IF_DEBUG(const auto name = FD->getNameAsString();)
				process_type_lifetime_annotations(*FD, m_state1, &MR, &Rewrite);

				const auto init_E = CXXCI->getInit();
				if (init_E) {
					auto CXXCD = Tget_containing_element_of_type<clang::CXXConstructorDecl>(init_E, *(MR.Context));
					if (CXXCD) {
						process_function_lifetime_annotations(*CXXCD, m_state1, &MR, &Rewrite);
					}

					auto CXXThisExpr_range = get_contained_elements_of_type_CXXThisExpr(*init_E);
					for (const auto CXXTE : CXXThisExpr_range) {
						assert(CXXTE);
						const auto ME = get_immediately_containing_MemberExpr_from_CXXThisExpr_if_any(*CXXTE, *(MR.Context));
						if (ME) {
							const auto FD2 = get_FieldDecl_from_MemberExpr_if_any(*ME);
							if (FD2) {
								bool res = first_is_contained_in_scope_of_second(FD, FD2, *(MR.Context));
								if ((!res) || (FD == FD2)) {
									auto MESR = nice_source_range(ME->getSourceRange(), Rewrite);
									if (!MESR.isValid()) {
										MESR = SR;
									}

									const std::string error_desc = std::string("The field '") + FD2->getNameAsString()
										+ "' may be being referenced before it has been constructed.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, MESR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else {
								const auto CXXMD = dyn_cast<const CXXMethodDecl>(ME->getMemberDecl());
								if (CXXMD) {
									/* error: Unable to verify that the member function used here can't access part of
									the object that hasn't been constructed yet. */
									const std::string error_desc = std::string("Calling non-static member functions ")
									+ "(such as '" + CXXMD->getQualifiedNameAsString() + "') of an object is not supported in "
									+ "constructor initializers or direct field initializers of the object. Consider "
									+ "using a static member or free function instead. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								} else {
									/* Since this MemberExpr was obtained from a CXXThisExpr, if it doesn't refer to
									a field, then presumably it refers to a (non-static) member function.
									So arriving here would be unexpected. */
									int q = 5;
								}
							}
						} else {
							const std::string error_desc = std::string("Unable to verify that the 'this' pointer ")
							+ "used here can't be used to access part of the object that hasn't been constructed yet.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
					if (is_raw_pointer_or_equivalent(FD->getType())
						&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(FD->getType()))
						&& (!m_state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
						) {
						if (is_nullptr_literal(init_E, *(MR.Context))) {
							auto CISR = nice_source_range(init_E->getSourceRange(), Rewrite);
							if (!CISR.isValid()) {
								CISR = SR;
							}
							const std::string error_desc = std::string("Null initialization of ")
								+ "native pointer field '" + FD->getNameAsString()
								+ "' is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CISR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}

						auto maybe_alts = m_state1.corresponding_abstract_lifetime_set_if_any(FD);
						if (maybe_alts.has_value()) {
							auto& alts = maybe_alts.value();
							if ((1 <= alts.m_primary_lifetimes.size())) {
								/* The member is a pointer with a lifetime annotation. Here we're going to check that the
								initialization value has a compatible (sub)lifetime value.
								The thinking is that this check only needs to be done for pointers because other supported
								member reference types will have explicitly defined constructors and the initalization value
								will be checked when their constructor is called. */

								/* Since the member pointer's sublifetime is abstract and known, the primary lifetime of the
								pointer is not relevant in determining if the validity of the assignment/initialization. So
								we just use a "stand in" primamry lifetime. */
								CScopeLifetimeInfo1 lhs_lifetime_value;
								lhs_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
								lhs_lifetime_value.m_maybe_containing_scope = get_containing_scope(CXXCI, *(MR.Context));
								lhs_lifetime_value.m_maybe_source_range = CXXCI->getSourceRange();
								//lhs_lifetime_value.m_maybe_corresponding_cpp_element = CXXCI;
								//slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, FD->getType());
								lhs_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos = { alts.m_primary_lifetimes.front() };

								CScopeLifetimeInfo1 rhs_lifetime_value;
								bool rhs_lifetime_values_evaluated = false;
								std::string rhs_hints;
								auto adj_RHSEX = remove_cast_from_TPointerForLegacy_to_raw_pointer(init_E, *(MR.Context));
								auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, m_state1, adj_RHSEX);
								MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value);
								if (maybe_expr_lifetime_value.has_value()) {
									CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									rhs_lifetime_value = expr_slti;
									rhs_lifetime_values_evaluated = true;
								} else {
									auto maybe_rhs_slo = lower_bound_lifetime_owner_of_pointer_target_if_available(
										adj_RHSEX, *(MR.Context), m_state1);
									if (!(maybe_rhs_slo.has_value())) {
										/* We were unable to determine a lifetime owner for this expression. We haven't
										contemplated all circumstances for which this might happen, if any, but for now
										we're just going to set the expression itself as the owner. */
										maybe_rhs_slo = adj_RHSEX;
										rhs_hints = maybe_rhs_slo.hints_str();
									} else {
										rhs_lifetime_values_evaluated = true; // ?
									}
									rhs_lifetime_value = scope_lifetime_info_from_lifetime_owner(maybe_rhs_slo.value(), *(MR.Context), m_state1);
								}

								bool satisfies_checks = slti_second_can_be_assigned_to_first(lhs_lifetime_value, rhs_lifetime_value, *(MR.Context), m_state1);

								if (!satisfies_checks) {
									std::string error_desc = std::string("Unable to verify that this pointer assignment (of type '")
										+ FD->getType().getAsString() + "') is safe.";
									std::string hints;
									if ((!rhs_lifetime_values_evaluated) && (!rhs_hints.empty())) {
										hints += " (" + rhs_hints + ")";
									}
									if (hints.empty()) {
										hints += " (Possibly due to being unable to verify that the new target object outlives the (scope) pointer.)";
									}
									error_desc += hints;

									error_desc += " (This pointer assignment occurs in the constructor initializer for member field '" + FD->getNameAsString() + "' .)";

									auto res = m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}

							}
						}
					}
				} else {
					auto FD_qtype = FD->getType();
					IF_DEBUG(auto FD_qtype_str = FD_qtype.getAsString();)
					MSE_RETURN_IF_TYPE_IS_NULL(FD_qtype);
					if (FD_qtype->isPointerType()) {
						{
							const std::string error_desc = std::string("Default initialization of ")
								+ "native pointer field '" + FD->getNameAsString()
								+ "' is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSPointerAssignment : public MatchFinder::MatchCallback
	{
	public:
		MCSSSPointerAssignment (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::Expr* LHSEX, const clang::Expr* RHSEX) {

			if ((LHSEX != nullptr) && (RHSEX != nullptr))
			{
				SourceRange SR = nice_source_range(LHSEX->getSourceRange(), Rewrite);

				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(LHSEX, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				/* In our case, it's significantly harder to verify the safety of a pointer assignment than say,
				a value initialization of a pointer. With initialization, we only need to verify that the target
				object has scope lifetime, as that alone is sufficient to conclude that the target object
				outlives the pointer. Not so with pointer assignment. There we need to obtain an "upper bound"
				for the (scope) lifetime of the (lhs) pointer being modified and a lower bound for the (scope)
				lifetime of the new target object. */

				assert(LHSEX && RHSEX);
				IF_DEBUG(const auto LHSEX_qtype_str = LHSEX->getType().getAsString();)
				const auto LHSEX_rw_type_ptr = remove_mse_transparent_wrappers(LHSEX->getType()).getTypePtr();
				assert(LHSEX_rw_type_ptr);
				if (!LHSEX_rw_type_ptr->isPointerType()) {
					const auto RD = LHSEX_rw_type_ptr->getAsRecordDecl();
					if (!RD) {
						return;
					} else {
						/* `mse::us::impl::TPointerForLegacy<>` is sometimes used as (a functionally
						equivalent) substitute for native pointers that can act as a base class. */
						const auto LHSEX_rw_qtype_str = RD->getQualifiedNameAsString();
						DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
						if (TPointerForLegacy_str != LHSEX_rw_qtype_str) {
							return;
						}
					}
				} else {
					const auto qtype = clang::QualType(LHSEX_rw_type_ptr, 0/*I'm just assuming zero specifies no qualifiers*/);
					if (state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)) {
						return;
					}
				}

				bool satisfies_checks = false;

				auto FND = enclosing_function_if_any(LHSEX, *(MR.Context));
				if (FND) {
					process_function_lifetime_annotations(*FND, state1, &MR, &Rewrite);
				}

				/* Obtaining the declaration (location) of the pointer to be modified (or its owner) (and
				therefore its scope lifetime) can be challenging. We are not always going to be able to
				do so. */

				CScopeLifetimeInfo1 rhs_lifetime_value;
				bool rhs_lifetime_values_evaluated = false;
				std::string rhs_hints;
				auto adj_RHSEX = remove_cast_from_TPointerForLegacy_to_raw_pointer(RHSEX, *(MR.Context));
				auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, state1, adj_RHSEX);
				if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
					return;
				}
				if (maybe_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
					rhs_lifetime_value = expr_slti;
					rhs_lifetime_values_evaluated = true;
				} else {
					auto maybe_rhs_slo = lower_bound_lifetime_owner_of_pointer_target_if_available(
						adj_RHSEX, *(MR.Context), state1);
					if (!(maybe_rhs_slo.has_value())) {
						/* We were unable to determine a lifetime owner for this expression. We haven't
						contemplated all circumstances for which this might happen, if any, but for now
						we're just going to set the expression itself as the owner. */
						maybe_rhs_slo = adj_RHSEX;
						rhs_hints = maybe_rhs_slo.hints_str();
					} else {
						rhs_lifetime_values_evaluated = true; // ?
					}
					rhs_lifetime_value = scope_lifetime_info_from_lifetime_owner(maybe_rhs_slo.value(), *(MR.Context), state1);
				}

				CScopeLifetimeInfo1 lhs_lifetime_value;
				lhs_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
				lhs_lifetime_value.m_maybe_containing_scope = get_containing_scope(LHSEX, *(MR.Context));
				lhs_lifetime_value.m_maybe_source_range = LHSEX->getSourceRange();
				lhs_lifetime_value.m_maybe_corresponding_cpp_element = LHSEX;
				slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, LHSEX->getType());
				bool lhs_lifetime_values_evaluated = false;

				{
					auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, state1, LHSEX);
					if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						return;
					} else if (maybe_expr_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
						slti_set_default_lower_bound_lifetimes_where_needed(expr_slti, LHSEX->getType());
						if ((1 == expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.size())
							&& (1 == lhs_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.size())) {
							const auto& target_slti_cref = expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.front();
							if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == target_slti_cref.m_category) {
								/* If the lifetime of a pointer's target is abstract, it can be used as the ("upper bound")
								lifetime of the lhs of a (pointer) assignment. Right? */
								auto& lhs_target_slti_ref = lhs_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.front();
								lhs_target_slti_ref = target_slti_cref;
								lhs_lifetime_values_evaluated = true;
							}
						} else {
							/* unexpected */
							int i = 3;
						}
					}
				}

				std::string lhs_hints;

				if (!lhs_lifetime_values_evaluated) {
					auto maybe_lhs_slo = upper_bound_lifetime_owner_if_available(LHSEX, *(MR.Context), state1);

					if (maybe_lhs_slo.has_value()) {
						auto& lhs_slo = maybe_lhs_slo.value();

						lhs_lifetime_value = scope_lifetime_info_from_lifetime_owner(lhs_slo, *(MR.Context), state1);
						slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, LHSEX->getType());
						lhs_lifetime_values_evaluated = true;
					} else {
						lhs_hints = maybe_lhs_slo.hints_str();
					}
				}

				satisfies_checks = slti_second_can_be_assigned_to_first(lhs_lifetime_value, rhs_lifetime_value, *(MR.Context), state1);

				if (!satisfies_checks) {
					std::string error_desc = std::string("Unable to verify that this pointer assignment (of type '")
						+ LHSEX->getType().getAsString() + "') is safe.";
					std::string hints;
					if ((!lhs_lifetime_values_evaluated) && (!lhs_hints.empty())) {
						hints += " (" + lhs_hints + ")";
					}
					if ((!rhs_lifetime_values_evaluated) && (!rhs_hints.empty())) {
						hints += " (" + rhs_hints + ")";
					}
					if (hints.empty()) {
						hints += " (Possibly due to being unable to verify that the new target object outlives the (scope) pointer.)";
					}
					error_desc += hints;

					auto FND = enclosing_function_if_any(LHSEX, *(MR.Context));
					if (FND) {
						if (FND->isDefaulted()) {
							error_desc += " (This pointer assignment is contained in the (possibly implicit/default member) function '" + FND->getNameAsString() + "' .)";
						}
					}

					auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR) {
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcssspointerassignment1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcssspointerassignment1");

			if ((BO != nullptr) || (CXXOCE != nullptr))
			{
				if ((BO == nullptr) && (CXXOCE != nullptr)) {
					/* This handler may be called by either of two matchers. One matcher matches against 
					`BinaryOperator` elements that are assignment operators. The other matcher matches against
					`CXXOperatorCallExpr` elements, but does not filter to ensure they are "assignment"
					`CXXOperatorCallExpr` elements. So we do the filtering here. */
					const auto l_operator = CXXOCE->getDirectCallee();
					if (l_operator) {
						//const auto operator_name = l_operator->getName();
						const auto operator_name = l_operator->getNameAsString();
						static const std::string assignment_operator_str = "operator=";
						if (assignment_operator_str != operator_name) {
							return;
						}
					} else {
						return;
					}
				}

				SourceRange SR = (BO != nullptr) ? nice_source_range(BO->getSourceRange(), Rewrite)
					: nice_source_range(CXXOCE->getSourceRange(), Rewrite);

				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = (BO != nullptr) ? m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context))
					: m_state1.m_suppress_check_region_set.contains(CXXOCE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				/* In our case, it's significantly harder to verify the safety of a pointer assignment than say,
				a value initialization of a pointer. With initialization, we only need to verify that the target
				object has scope lifetime, as that alone is sufficient to conclude that the target object
				outlives the pointer. Not so with pointer assignment. There we need to obtain an "upper bound"
				for the (scope) lifetime of the (lhs) pointer being modified and a lower bound for the (scope)
				lifetime of the new target object. */

				const clang::Expr* LHSEX = nullptr;
				const clang::Expr* RHSEX = nullptr;
				if (BO != nullptr) {
					LHSEX = IgnoreParenImpNoopCasts(BO->getLHS(), *(MR.Context));
					RHSEX = IgnoreParenImpNoopCasts(BO->getRHS(), *(MR.Context));
				} else {
					const auto numArgs = CXXOCE->getNumArgs();
					if (2 == CXXOCE->getNumArgs()) {
						LHSEX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), *(MR.Context));
						RHSEX = IgnoreParenImpNoopCasts(CXXOCE->getArg(1), *(MR.Context));
					} else {
						return;
					}
				}

				assert(LHSEX && RHSEX);
				s_handler1(MR, Rewrite, m_state1, LHSEX, RHSEX);
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSAssignment : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAssignment (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcsssassignment1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcsssassignment1");

			if ((BO != nullptr) || (CXXOCE != nullptr))
			{
				if ((BO == nullptr) && (CXXOCE != nullptr)) {
					/* This handler may be called by either of two matchers. One matcher matches against 
					`BinaryOperator` elements that are assignment operators. The other matcher matches against
					`CXXOperatorCallExpr` elements, but does not filter to ensure they are "assignment"
					`CXXOperatorCallExpr` elements. So we do the filtering here. */
					const auto l_operator = CXXOCE->getDirectCallee();
					if (l_operator) {
						//const auto operator_name = l_operator->getName();
						const auto operator_name = l_operator->getNameAsString();
						static const std::string assignment_operator_str = "operator=";
						if (assignment_operator_str != operator_name) {
							return;
						}
					} else {
						return;
					}
				}

				SourceRange SR = (BO != nullptr) ? nice_source_range(BO->getSourceRange(), Rewrite)
					: nice_source_range(CXXOCE->getSourceRange(), Rewrite);

				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = (BO != nullptr) ? m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context))
					: m_state1.m_suppress_check_region_set.contains(CXXOCE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				/* In our case, it's significantly harder to verify the safety of a pointer assignment than say,
				a value initialization of a pointer. With initialization, we only need to verify that the target
				object has scope lifetime, as that alone is sufficient to conclude that the target object
				outlives the pointer. Not so with pointer assignment. There we need to obtain an "upper bound"
				for the (scope) lifetime of the (lhs) pointer being modified and a lower bound for the (scope)
				lifetime of the new target object. */

				const clang::Expr* LHSEX = nullptr;
				const clang::Expr* RHSEX = nullptr;
				if (BO != nullptr) {
					LHSEX = IgnoreParenImpNoopCasts(BO->getLHS(), *(MR.Context));
					RHSEX = IgnoreParenImpNoopCasts(BO->getRHS(), *(MR.Context));
				} else {
					const auto numArgs = CXXOCE->getNumArgs();
					if (2 == CXXOCE->getNumArgs()) {
						LHSEX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), *(MR.Context));
						RHSEX = IgnoreParenImpNoopCasts(CXXOCE->getArg(1), *(MR.Context));
					} else {
						return;
					}
				}

				assert(LHSEX && RHSEX);
				IF_DEBUG(const auto LHSEX_qtype_str = LHSEX->getType().getAsString();)
				const auto LHSEX_rw_type_ptr = remove_mse_transparent_wrappers(LHSEX->getType()).getTypePtr();
				assert(LHSEX_rw_type_ptr);
				if (!LHSEX_rw_type_ptr->isPointerType()) {
					const auto RD = LHSEX_rw_type_ptr->getAsRecordDecl();
					if (!RD) {
						/* This is just some non-pointer scalar whose assignment is of no concern to us. */
						return;
					} else {
						/* `mse::us::impl::TPointerForLegacy<>` is sometimes used as (a functionally
						equivalent) substitute for native pointers that can act as a base class. */
						const auto LHSEX_rw_qtype_str = RD->getQualifiedNameAsString();
						DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
						if (TPointerForLegacy_str != LHSEX_rw_qtype_str) {
							/* We are only concerned with raw pointers and TPointerForLegacy<>s, which the SaferCPlusPlus
							library uses as a functional equivalent of a raw pointer (that can be used as a base class).
							(Btw, we would also be concerned with raw references if they supported assignment (rebinding),
							but they don't.) */
							return;
						}
					}
				} else {
					const auto qtype = clang::QualType(LHSEX_rw_type_ptr, 0/*I'm just assuming zero specifies no qualifiers*/);
					if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)) {
						return;
					}
				}

				bool satisfies_checks = false;
				bool lhs_lifetime_values_evaluated = false;
				bool rhs_lifetime_values_evaluated = false;

				auto maybe_tlta = type_lifetime_annotations_if_available(*LHSEX, m_state1, &MR, &Rewrite);
				if (!(maybe_tlta.has_value())) {
					return;
				}

				auto maybe_lhs_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, m_state1, LHSEX);
				if (maybe_lhs_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
					return;
				}
				if (maybe_lhs_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& lhs_expr_slti = maybe_lhs_expr_lifetime_value.value().m_scope_lifetime_info;
					lhs_lifetime_values_evaluated = true;
					auto maybe_rhs_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, m_state1, RHSEX);
					if (maybe_rhs_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						return;
					}
					if (maybe_rhs_expr_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& rhs_expr_slti = maybe_rhs_expr_lifetime_value.value().m_scope_lifetime_info;
						rhs_lifetime_values_evaluated = true;
						satisfies_checks = slti_second_can_be_assigned_to_first(lhs_expr_slti, rhs_expr_slti, *(MR.Context), m_state1);
					}
				}

				if (!satisfies_checks) {
					std::string error_desc = std::string("Unable to verify that this assignment (of type '")
						+ LHSEX->getType().getAsString() + "') satisfies the (annotated) lifetime constraints of the type.";
					if (!lhs_lifetime_values_evaluated) {
						error_desc += " (Unable to evaluate the lifetimes of the left hand side expression.)";
					} else if (!rhs_lifetime_values_evaluated) {
						error_desc += " (Unable to evaluate the lifetimes of the right hand side expression.)";
					}

					auto FND = enclosing_function_if_any(LHSEX, *(MR.Context));
					if (FND) {
						if (FND->isDefaulted()) {
							error_desc += " (This assignment is contained in the (possibly implicit/default member) function '" + FND->getNameAsString() + "' .)";
						}
					}

					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n\n";
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	clang::CXXMethodDecl const * call_operator_if_any(const clang::CXXRecordDecl* CXXRD) {
		clang::CXXMethodDecl const * retval = nullptr;
		if (CXXRD) {
			if (CXXRD->isLambda()) {
				retval = CXXRD->getLambdaCallOperator();
			} else {
				static const std::string call_operator_str = "operator()";
				for (const auto& method : CXXRD->methods()) {
					if (call_operator_str == method->getNameAsString()) {
						/* The object appears to be a functor. */
						retval = method;
						break;
					}
				}

				if (!retval) {
					/* No call operator. Maybe it inherits one from a base class. */
					for (const auto& base : CXXRD->bases()) {
						retval = call_operator_if_any(base.getType()->getAsCXXRecordDecl());
						if (retval) {
							break;
						}
					}
				}
			}
		}
		return retval;
	}

	class MCSSSExprUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSExprUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Expr* EX = MR.Nodes.getNodeAs<clang::Expr>("mcsssexprutil1");

			if (EX != nullptr)
			{
				auto SR = nice_source_range(EX->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EX, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (suppress_check_flag) {
					return;
				}

				/* For some reason some of our expression matchers seem to be unreliable.
				So we (redundantly) implement (some of) them in this general expression matcher
				which seems to be more reliable. */

				{
					auto const * const EX_ii = IgnoreParenImpNoopCasts(EX, *(MR.Context));
					auto const * const CE = dyn_cast<const CallExpr>(EX_ii);
					if (CE) {
						MCSSSFunctionCall::s_handler1(MR, Rewrite, m_state1, CE);

						auto function_decl = CE->getDirectCallee();
						auto num_args = CE->getNumArgs();
						if (function_decl) {
							const std::string qualified_function_name = function_decl->getQualifiedNameAsString();
							const auto FDSR = function_decl->getSourceRange();
							bool is_potentially_from_standard_header = FDSR.getBegin().isInvalid()
								|| MR.SourceManager->isInSystemHeader(FDSR.getBegin());
							if (!is_potentially_from_standard_header) {
								/*
								bool filename_is_invalid = false;
								std::string full_path_name = MR.SourceManager->getBufferName(FDSR.getBegin(), &filename_is_invalid);
								static const std::string built_in_str = "<built-in>";
								is_potentially_from_standard_header |= (built_in_str == full_path_name);
								*/
								/* filtered_out_by_location() returns true for SaferCPlusPlus headers as well
								as standard and system headers.  */
								is_potentially_from_standard_header |= filtered_out_by_location(*(MR.SourceManager), FDSR.getBegin());
							}
							if (is_potentially_from_standard_header) {
								static const std::string std_move_str = "std::move";
								if ((std_move_str == qualified_function_name) && (1 == num_args)) {
									const auto arg_EX = CE->getArg(0);
									assert(arg_EX);
									const auto arg_qtype = arg_EX->getType();
									IF_DEBUG(const auto arg_qtype_str = arg_EX->getType().getAsString();)
									if (referenceable_by_scope_pointer(arg_qtype, (*this).m_state1)) {
										auto l_source_text = Rewrite.getRewrittenText(SR);
										if (true || string_begins_with(l_source_text, std_move_str)) {
											/* todo: check for aliases */
											const std::string error_desc = std::string("Cannot (yet) verify the safety of this explicit use of std::move() with ")
												+ "an argument type ('" + arg_EX->getType().getAsString() + "') that yields scope pointers (unconditionally via the "
												+ "'operator &' of some component). "
												+ "(In particular, explicit use of std::move() with 'mse::TXScopeOwnerPointer<>' "
												+ "or any object that might contain an 'mse::TXScopeOwnerPointer<>' is not supported.) ";
											auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
											if (res.second) {
												std::cout << (*(res.first)).as_a_string1() << " \n\n";
											}
										}
									} else if (false) {
										/* This branch is now redundant with the other branch, but may be 
										resurrected at some point. */
										const auto* CXXRD = arg_qtype.getTypePtr()->getAsCXXRecordDecl();
										if (CXXRD) {
											auto name = CXXRD->getQualifiedNameAsString();
											const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
											if (tmplt_CXXRD) {
												name = tmplt_CXXRD->getQualifiedNameAsString();
											}

											DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");
											if (name == xscope_owner_ptr_str) {
												const std::string error_desc = std::string("Explicit use of std::move() on ")
													+ xscope_owner_ptr_str + "<> " + " is not supported.";
												auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}
									}
								} else {
									static const std::string malloc_str = "malloc";
									static const std::string realloc_str = "realloc";
									static const std::string free_str = "free";
									static const std::string calloc_str = "calloc";
									static const std::string alloca_str = "alloca";
									std::string unsupported_function_str;
									if (malloc_str == qualified_function_name) {
										unsupported_function_str = malloc_str;
									} else if (realloc_str == qualified_function_name) {
										unsupported_function_str = realloc_str;
									} else if (free_str == qualified_function_name) {
										unsupported_function_str = free_str;
									} else if (calloc_str == qualified_function_name) {
										unsupported_function_str = calloc_str;
									} else if (alloca_str == qualified_function_name) {
										unsupported_function_str = alloca_str;
									}
									if ("" != unsupported_function_str) {
										const std::string error_desc = std::string("The '") + unsupported_function_str
											+ "' function is not supported.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									} else {

										for (const auto& arg_EX : CE->arguments()) {
											assert(arg_EX);
											const auto arg_EX_ii = IgnoreParenImpNoopCasts(arg_EX, *(MR.Context));
											assert(arg_EX_ii);
											const auto qtype = arg_EX_ii->getType();

											auto const * const CXXRD = qtype->getAsCXXRecordDecl();
											clang::FunctionDecl const * l_FD = call_operator_if_any(CXXRD);
											if (l_FD) {
												/* We're passing a lambda expression or function object as a parameter to
												some kind of "opaque" standard library (or system, or SaferCPlusPlus) function.
												In regular code, the function object would be checked when/where it's actually
												called, but since we don't check standard library (or system) code, we have to
												check for potential dangers here where it's being passed as a parameter. */
												for (const auto& param : l_FD->parameters()) {
													auto param_qtype = param->getType();
													IF_DEBUG(auto param_qtype_str = param_qtype.getAsString();)
													MSE_RETURN_IF_TYPE_IS_NULL(param_qtype);
													if (param_qtype->isReferenceType()) {
														auto arg_SR = arg_EX_ii->getSourceRange();
														if (arg_SR.isInvalid()) {
															arg_SR = SR;
														}
														const std::string error_desc = std::string("Unable to verify the safety ")
															+ "of the native reference parameter, '" + param->getNameAsString()
															+ "', of the function object being passed (to an opaque "
															+ "function/method/operator) here.";
														auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, arg_SR.getBegin(), error_desc));
														if (res.second) {
															std::cout << (*(res.first)).as_a_string1() << " \n\n";
														}
													}
												}
											} else if (qtype->isFunctionPointerType()) {
												const auto function_qtype = qtype->getPointeeType();
												const auto FPT = function_qtype->getAs<const clang::FunctionProtoType>();
												if (FPT) {
													for (const auto& param_type : FPT->param_types()) {
														if (param_type->isReferenceType()) {
															auto arg_SR = arg_EX_ii->getSourceRange();
															if (arg_SR.isInvalid()) {
																arg_SR = SR;
															}
															const std::string error_desc = std::string("Unable to verify the safety ")
																+ "of a native reference parameter of type '" + param_type.getAsString()
																+ "', of the function being passed (to an opaque "
																+ "function/method/operator) here.";
															auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, arg_SR.getBegin(), error_desc));
															if (res.second) {
																std::cout << (*(res.first)).as_a_string1() << " \n\n";
															}
														}
													}
												}
											}
										}

										std::string function_name = function_decl->getNameAsString();
										static const std::string str_prefix_str = "str";
										static const std::string mem_prefix_str = "mem";
										if ((!(*this).m_state1.char_star_restrictions_are_disabled())
											&& string_begins_with(function_name, str_prefix_str)) {
											for (const auto& param : function_decl->parameters()) {
												const auto uqtype_str = param->getType().getUnqualifiedType().getAsString();
												static const std::string const_char_star_str = "const char *";
												static const std::string char_star_str = "char *";
												if ((const_char_star_str == uqtype_str) || (char_star_str == uqtype_str)) {
													const std::string error_desc = std::string("'") + qualified_function_name
														+ "' heuristically looks like a C standard library string function. "
														+ "Those are not supported.";
													auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
											}
										} else if (string_begins_with(function_name, mem_prefix_str)) {
											for (const auto& param : function_decl->parameters()) {
												const auto uqtype_str = param->getType().getUnqualifiedType().getAsString();
												static const std::string const_void_star_str = "const void *";
												static const std::string void_star_str = "void *";
												if ((const_void_star_str == uqtype_str) || (void_star_str == uqtype_str)) {
													const std::string error_desc = std::string("'") + qualified_function_name
														+ "' heuristically looks like a C standard library memory function. "
														+ "Those are not supported.";
													auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
													if (res.second) {
														std::cout << (*(res.first)).as_a_string1() << " \n\n";
													}
												}
											}
										}

									}
								}
							}

						}
					} else {
						auto const * const CXXNE = dyn_cast<const clang::CXXNewExpr>(EX_ii);
						auto const * const CXXDE = dyn_cast<const clang::CXXDeleteExpr>(EX_ii);
						auto const * const CXXSVIE = dyn_cast<const clang::CXXScalarValueInitExpr>(EX_ii);
						std::string unsupported_expression_str;
						if (CXXNE) {
							unsupported_expression_str = "'operator new' (returning type '"
								+ CXXNE->getType().getAsString() + "')";
						} else if (CXXDE) {
							const auto arg_EX = CXXDE->getArgument();
							unsupported_expression_str = "'operator delete'";
							if (arg_EX) {
								unsupported_expression_str += " (with argument type '"
									+ arg_EX->getType().getAsString() + "')";
							}
						} else if (CXXSVIE) {
							IF_DEBUG(CXXSVIE->getType().getAsString();)
							unsupported_expression_str = "Default construction of scalar types (such as '" + CXXSVIE->getType().getAsString() + "')";
						}
						if ("" != unsupported_expression_str) {
							const std::string error_desc = unsupported_expression_str
								+ " is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}

					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSDeclUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSDeclUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Decl* D = MR.Nodes.getNodeAs<clang::Decl>("mcsssdeclutil1");

			if ((D != nullptr))
			{
				auto SR = nice_source_range(D->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(D, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DISR);
				if (suppress_check_flag) {
					return;
				}

				auto TD = dyn_cast<const clang::TypeDecl>(D);
				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();
					auto DD_qtype = DD->getType();
					IF_DEBUG(auto DD_qtype_str = DD_qtype.getAsString();)
					MSE_RETURN_IF_TYPE_IS_NULL(DD_qtype);
					const auto TST = DD_qtype->getAs<clang::TemplateSpecializationType>();

					auto maybe_decl_lifetime_value = evaluate_declaration_lower_bound_lifetimes(MR, Rewrite, m_state1, DD);
					if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						return;
					}
					if (maybe_decl_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& decl_slti = maybe_decl_lifetime_value.value().m_scope_lifetime_info;
					}
#if 0
					auto VD = dyn_cast<const clang::VarDecl>(D);
					if (VD) {
						const auto var_qualified_name = VD->getQualifiedNameAsString();

						auto PVD = dyn_cast<const clang::ParmVarDecl>(D);
						if (PVD) {
							auto FD = function_from_param(PVD);
							if (FD) {
								auto flta_iter1 = m_state1.m_function_lifetime_annotations_map.find(FD);
								if (m_state1.m_function_lifetime_annotations_map.end() != flta_iter1) {
									auto& param_lifetime_map_ref = flta_iter1->second.m_param_lifetime_map;

									/* Here we iterate over the function's parameters to determine the parameter ordinal
									of our parameter declaration. (Not necessarily the most efficient set up.) */
									param_ordinal_t param_ordinal = 1;
									for (auto param_iter = FD->param_begin(); FD->param_end() != param_iter; ++param_iter, param_ordinal += 1) {
										auto param = (*param_iter);
										auto qtype = param->getType();
										IF_DEBUG(const std::string qtype_str = qtype.getAsString();)
										if (param == PVD) {
											auto plm_iter = param_lifetime_map_ref.find(param_ordinal);
											if (param_lifetime_map_ref.end() != plm_iter) {
												/* Turns out that our parameter seems to have a lifetime annotation. */
												CScopeLifetimeInfo1 sli2;
												if (qtype->isReferenceType()) {
													if (plm_iter->second.m_primary_lifetimes.size() == 1) {
														/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
														same as the object it refers to (without an added level of indirection). */
														auto& alt = plm_iter->second.m_primary_lifetimes.front();
														sli2.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
														sli2.m_maybe_abstract_lifetime = alt;
														sli2.m_maybe_containing_scope = get_containing_scope(VD, *(MR.Context));
														sli2.m_maybe_source_range = VD->getSourceRange();

														*(sli2.m_sublifetimes_vlptr) = *(alt.m_sublifetimes_vlptr);
													} else {
														/* invalid? A reference parameter with lifetime annotation should have exactly one primary
														lifetime label. */
														int q = 3;

														const auto sl_storage_duration = VD->getStorageDuration();
														const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
														sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
														sli2.m_maybe_containing_scope = get_containing_scope(VD, *(MR.Context));
														sli2.m_maybe_source_range = VD->getSourceRange();
													}
												} else {
													const auto sl_storage_duration = VD->getStorageDuration();
													const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
													sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
													sli2.m_maybe_containing_scope = get_containing_scope(VD, *(MR.Context));
													sli2.m_maybe_source_range = VD->getSourceRange();

													/* Since, unlike regular variables, the initialization value of parameter variables (i.e. the
													corresponding argument) is not available in the declaration. So we use the annotated abstract
													lifetimes themselves as the lifetime (initialization) values. */
													*(sli2.m_sublifetimes_vlptr) = plm_iter->second;
												}
												m_state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, CVariableLifetimeValues{ sli2, true });
											} else {
												int q = 5;
											}
										}
									}
								}
							} else {
								int q = 5;
							}
						} else {
							process_variable_lifetime_annotations(*VD, m_state1, &MR, &Rewrite);

							auto maybe_tlta = type_lifetime_annotations_if_available(*VD, m_state1, &MR, &Rewrite);

							if (maybe_tlta.has_value()) {
								auto vltv_iter1 = m_state1.m_vardecl_lifetime_values_map.find(VD);
								if (m_state1.m_vardecl_lifetime_values_map.end() == vltv_iter1) {
									if (VD->hasInit()) {
										auto init_E = VD->getInit();
										assert(init_E);

										auto ILE = dyn_cast<const clang::InitListExpr>(init_E);
										auto VD_qtype = VD->getType();
										IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
										MSE_RETURN_IF_TYPE_IS_NULL(VD_qtype);
										if (ILE && VD_qtype->isAggregateType()) {
											if (!(VD_qtype->isPointerType() || VD_qtype->isReferenceType())) {
												const std::string error_desc = std::string("Aggregate initialization (of '") + var_qualified_name
													+ "') is not (currently) supported for types (like '" + qtype_str + "') that have (explicit or "
													+ "implicit) lifetime annotations.";
												auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
												if (res.second) {
													std::cout << (*(res.first)).as_a_string1() << " \n\n";
												}
											}
										}

										auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(MR, Rewrite, m_state1, init_E);
										if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
											/* Cannot properly evaluate because this is a template definition. Proper evaluation should
											occur in any instantiation of the template. */
											return;
										}
										if (maybe_expr_lifetime_value.has_value()) {
											CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

											CScopeLifetimeInfo1 sli2;

											const auto sl_storage_duration = VD->getStorageDuration();
											const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
											sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
											sli2.m_maybe_containing_scope = get_containing_scope(VD, *(MR.Context));
											sli2.m_maybe_source_range = VD->getSourceRange();
											*(sli2.m_sublifetimes_vlptr) = *(expr_slti.m_sublifetimes_vlptr);

											m_state1.m_vardecl_lifetime_values_map.insert_or_assign(VD, CVariableLifetimeValues{ sli2, true });
										} else {
											int q = 5;
										}
									} else {
										const std::string error_desc = std::string("(Non-parameter) variable '")
											+ var_qualified_name + "' of type '" + qtype_str + "' has as an associated lifetime label which "
											+ "requires that the decalaration have an initialization value (that doesn't seem to be present).";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}
						}

						const auto storage_duration = VD->getStorageDuration();
						const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();

						if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
							bool satisfies_checks = false;
							if (CXXRD) {
								auto type_name1 = CXXRD->getQualifiedNameAsString();
								const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
								if (tmplt_CXXRD) {
									type_name1 = tmplt_CXXRD->getQualifiedNameAsString();
								}

								DECLARE_CACHED_CONST_STRING(mse_rsv_static_immutable_obj_str1, mse_namespace_str() + "::rsv::TStaticImmutableObj");
								static const std::string std_atomic_str = std::string("std::atomic");
								DECLARE_CACHED_CONST_STRING(mse_AsyncSharedV2ReadWriteAccessRequester_str, mse_namespace_str() + "::TAsyncSharedV2ReadWriteAccessRequester");
								DECLARE_CACHED_CONST_STRING(mse_AsyncSharedV2ReadOnlyAccessRequester_str, mse_namespace_str() + "::TAsyncSharedV2ReadOnlyAccessRequester");
								DECLARE_CACHED_CONST_STRING(mse_TAsyncSharedV2ImmutableFixedPointer_str, mse_namespace_str() + "::TAsyncSharedV2ImmutableFixedPointer");
								DECLARE_CACHED_CONST_STRING(mse_TAsyncSharedV2AtomicFixedPointer_str, mse_namespace_str() + "::TAsyncSharedV2AtomicFixedPointer");
								DECLARE_CACHED_CONST_STRING(mse_rsv_ThreadLocalObj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");

								if ((type_name1 == mse_rsv_static_immutable_obj_str1)
									|| (type_name1 == std_atomic_str)
									|| (type_name1 == mse_AsyncSharedV2ReadWriteAccessRequester_str)
									|| (type_name1 == mse_AsyncSharedV2ReadOnlyAccessRequester_str)
									|| (type_name1 == mse_TAsyncSharedV2ImmutableFixedPointer_str)
									|| (type_name1 == mse_TAsyncSharedV2AtomicFixedPointer_str)
									|| ((type_name1 == mse_rsv_ThreadLocalObj_str) && (clang::StorageDuration::SD_Thread == storage_duration))
									) {
									satisfies_checks = true;
								}
							}

							if (!satisfies_checks) {
								if (clang::StorageDuration::SD_Static == storage_duration) {
									DECLARE_CACHED_CONST_STRING(const_char_star_str, "const char *");
									if ((qtype.isConstQualified()) && (is_async_shareable(qtype))) {
										satisfies_checks = true;
									} else if (qtype.getAsString() == const_char_star_str) {
										/* This isn't technically safe, but presumably this is likely
										to be a string literal, which should be fine, so for now we'll
										let it go. */
										satisfies_checks = true;
									} else {
										const std::string error_desc = std::string("Unable to verify the safety of variable '")
											+ var_qualified_name + "' of type '" + qtype_str + "' with 'static storage duration'. "
											+ "'static storage duration' is supported for eligible types wrapped in the "
											+ "'mse::rsv::TStaticImmutableObj<>' transparent template wrapper. Other supported wrappers include: "
											+ "mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
											+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>. "
											+ "Note that objects with 'static storage duration' may be simultaneously accessible from different threads "
											+ "and so have more stringent safety requirements than objects with 'thread_local storage duration'.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								} else {
									assert(clang::StorageDuration::SD_Thread == storage_duration);
									if (true || is_async_shareable(qtype)) {
										satisfies_checks = true;
									} else {
										const std::string error_desc = std::string("Unable to verify the safety of variable '")
											+ var_qualified_name + "' of type '" + qtype_str + "' with 'thread local storage duration'. "
											+ "'thread local storage duration' is supported for eligible types wrapped in the "
											+ "'mse::rsv::TThreadLocalObj<>' transparent template wrapper. Other supported wrappers include: "
											+ "mse::rsv::TStaticImmutableObj<>, mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
											+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}
						}
						if (CXXRD) {
							auto type_name1 = CXXRD->getQualifiedNameAsString();
							const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
							if (tmplt_CXXRD) {
								type_name1 = tmplt_CXXRD->getQualifiedNameAsString();
							}

							DECLARE_CACHED_CONST_STRING(mse_rsv_static_immutable_obj_str1, mse_namespace_str() + "::rsv::TStaticImmutableObj");
							DECLARE_CACHED_CONST_STRING(mse_rsv_ThreadLocalObj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");

							if (type_name1 == mse_rsv_static_immutable_obj_str1) {
								if (clang::StorageDuration::SD_Static != storage_duration) {
									const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
										+ mse_rsv_static_immutable_obj_str1 + "' must be declared to have 'static' storage duration.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else if (type_name1 == mse_rsv_ThreadLocalObj_str) {
								if (clang::StorageDuration::SD_Thread != storage_duration) {
									const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
										+ mse_rsv_ThreadLocalObj_str + "' must be declared to have 'thread_local' storage duration.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						}

						if (qtype.getTypePtr()->isScalarType()) {
							const auto init_EX = VD->getInit();
							if (!init_EX) {
								auto PVD = dyn_cast<const ParmVarDecl>(VD);
								if (!PVD) {
									if (!VD->isExternallyDeclarable()) {
										const std::string error_desc = std::string("Uninitialized ")
											+ "scalar variable '" + VD->getNameAsString() + "' (of type '"
											+ qtype.getAsString() + "') ";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									} else {
										/* todo: emit error that (uninitialized) 'extern' variables
										aren't supported?  */;
									}
								}
							}
						}

						const auto init_EX = VD->getInit();
						if (init_EX) {
							auto res = statement_makes_reference_to_decl(*VD, *init_EX);
							if (res) {
								const std::string error_desc = std::string("Reference to variable '")
									+ VD->getNameAsString() + "' before the completion of its "
									+ "construction/initialization is not supported.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						} else if (false && VD->isExternallyDeclarable()) {
							const std::string error_desc = std::string("\"External\"/inline ")
								+ "variable declarations (such as the declaration of "
								+ VD->getNameAsString() + "' of type '" + qtype.getAsString()
								+ "') are not currently supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}

						process_function_lifetime_annotations(*VD, m_state1, &MR, &Rewrite);
					} else {
						auto FD = dyn_cast<const clang::FieldDecl>(D);
						if (FD) {
							if (false && (qtype.getTypePtr()->isPointerType() || qtype.getTypePtr()->isReferenceType())) {
								/* These are handled in MCSSSRecordDecl2. */
							} else if (qtype.getTypePtr()->isScalarType()) {
								const auto* init_EX = FD->getInClassInitializer();
								if (!init_EX) {
									const auto grandparent_DC = FD->getParent()->getParentFunctionOrMethod();
									bool is_lambda_capture_field = false;

									const auto& parents = MR.Context->getParents(*(FD->getParent()));
									if ( !(parents.empty()) ) {
										const auto LE = parents[0].get<LambdaExpr>();
										if (LE) {
											is_lambda_capture_field = true;
										}
									}
									if (!is_lambda_capture_field) {
										if (qtype.getTypePtr()->isPointerType()) {
										} else {
											const std::string error_desc = std::string("(Non-pointer) scalar fields (such those of type '")
												+ qtype.getAsString() + "') require direct initializers.";
											auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
											if (res.second) {
												std::cout << (*(res.first)).as_a_string1() << " \n\n";
											}
										}
									}
								}
							}

							process_type_lifetime_annotations(*FD, m_state1, &MR, &Rewrite);
						}
					}

					const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
					if (CXXRD) {
						auto name = CXXRD->getQualifiedNameAsString();
						const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
						if (tmplt_CXXRD) {
							name = tmplt_CXXRD->getQualifiedNameAsString();
						}
						DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncShareableObj_str1, mse_namespace_str() + "::rsv::TAsyncShareableObj");
						DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncPassableObj_str1, mse_namespace_str() + "::rsv::TAsyncPassableObj");
						DECLARE_CACHED_CONST_STRING(mse_rsv_TAsyncShareableAndPassableObj_str1, mse_namespace_str() + "::rsv::TAsyncShareableAndPassableObj");
						DECLARE_CACHED_CONST_STRING(mse_rsv_TFParam_str, mse_namespace_str() + "::rsv::TFParam");
						static const std::string std_unique_ptr_str = "std::unique_ptr";
						if (mse_rsv_TAsyncShareableObj_str1 == name) {
							if (1 == CXXRD->getNumBases()) {
								const auto& base = *(CXXRD->bases_begin());
								const auto base_qtype = base.getType();
								const auto base_qtype_str = base_qtype.getAsString();
								if (!is_async_shareable(base_qtype)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableObj<> template, '"
										+ base_qtype_str + "', is eligible to be safely shared (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else {
								/* This branch shouldn't happen. Unless the library's been changed somehow. */
							}
						} else if (mse_rsv_TAsyncPassableObj_str1 == name) {
							if (1 == CXXRD->getNumBases()) {
								const auto& base = *(CXXRD->bases_begin());
								const auto base_qtype = base.getType();
								const auto base_qtype_str = base_qtype.getAsString();
								if (!is_async_passable(base_qtype)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of the mse::rsv::TAsyncPassableObj<> template, '"
										+ base_qtype_str + "', is eligible to be safely passed (between threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else {
								/* This branch shouldn't happen. Unless the library's been changed somehow. */
							}
						} else if (mse_rsv_TAsyncShareableAndPassableObj_str1 == name) {
							if (1 == CXXRD->getNumBases()) {
								const auto& base = *(CXXRD->bases_begin());
								const auto base_qtype = base.getType();
								const auto base_qtype_str = base_qtype.getAsString();
								if ((!is_async_shareable(base_qtype)) || (!is_async_passable(base_qtype))) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableAndPassableObj<> template, '"
										+ base_qtype_str + "', is eligible to be safely shared and passed (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else {
								/* This branch shouldn't happen. Unless the library's been changed somehow. */
							}
						} else if (mse_rsv_TFParam_str == name) {
							bool satisfies_checks = false;
							auto VD = dyn_cast<const clang::VarDecl>(DD);
							if (VD) {
								auto FND = dyn_cast<const clang::FunctionDecl>(VD->getParentFunctionOrMethod());
								if (FND) {
									auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
									if (PVD) {
										satisfies_checks = true;
									} else {
										auto CE = dyn_cast<const clang::CallExpr>(IgnoreParenImpNoopCasts(VD->getInit(), *(MR.Context)));
										if (CE) {
											auto function_decl = CE->getDirectCallee();
											auto num_args = CE->getNumArgs();
											if (function_decl) {
												std::string qualified_function_name = function_decl->getQualifiedNameAsString();
												DECLARE_CACHED_CONST_STRING(as_an_fparam_str, mse_namespace_str() + "::rsv::as_an_fparam");
												if ((as_an_fparam_str == qualified_function_name)) {
													if (1 == num_args) {
														satisfies_checks = true;
													}
												}
											}
										}
									}
								}
							}
							if (!satisfies_checks) {
								const std::string error_desc = std::string("Unsupported use of ")
									+ "mse::rsv::TFParam<> (in type '" + name + "'). ";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						} else if (qtype.getTypePtr()->isUnionType()) {
							const std::string error_desc = std::string("Native unions (such as '" + qtype.getAsString() + "') are not ")
								+ "supported. ";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						} else if (true && (std_unique_ptr_str == name)) {
							if (!qtype.isConstQualified()) {
								const std::string error_desc = std::string("std::unique_ptr<>s that are not const qualified are not supported. ")
									+ "Consider using a reference counting pointer from the SaferCPlusPlus library. ";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							} else {
								const auto init_EX = VD->getInit();
								bool null_initialization = true;
								if (init_EX) {
									null_initialization = is_nullptr_literal(init_EX, *(MR.Context));
									if (!null_initialization) {
										const auto init_EX_ii = IgnoreParenImpNoopCasts(init_EX, *(MR.Context));
										const auto CXXCE = dyn_cast<const CXXConstructExpr>(init_EX_ii);
										if (CXXCE) {
											if (1 == CXXCE->getNumArgs()) {
												null_initialization = is_nullptr_literal(CXXCE->getArg(0), *(MR.Context));
											} else if (0 == CXXCE->getNumArgs()) {
												null_initialization = true;
											}
										}
									}
								}
								if (null_initialization) {
									const std::string error_desc = std::string("Null/default initialization of ")
										+ "std::unique_ptr<>s (such as those of type '" + qtype.getAsString()
										+ "') is not supported.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							}
						} else {
							auto check_for_and_handle_unsupported_element = [&MR, &SR](const clang::QualType& qtype, CTUState& state1) {
								std::string element_name;
								const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
								if (l_CXXRD) {
									element_name = l_CXXRD->getQualifiedNameAsString();
								} else {
									element_name = qtype.getAsString();
								}

								{
									auto uei_ptr = unsupported_element_info_ptr(element_name);
									if (uei_ptr) {
										const auto& unsupported_element_info = *uei_ptr;
										std::string error_desc = std::string("'") + element_name + std::string("' is not ")
											+ "supported (in this declaration of type '" + qtype.getAsString() + "'). ";
										if ("" != unsupported_element_info.m_recommended_alternative) {
											error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
										}
										auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							};
							//check_for_and_handle_unsupported_element(qtype, (*this).m_state1);
							//apply_to_component_types_if_any(qtype, check_for_and_handle_unsupported_element, (*this).m_state1);

							auto check_for_and_handle_unsupported_element2 = [&MR](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
								auto qtype = typeLoc.getType();
								std::string element_name;
								const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
								if (l_CXXRD) {
									element_name = l_CXXRD->getQualifiedNameAsString();
								} else {
									element_name = qtype.getAsString();
								}

								{
									auto uei_ptr = unsupported_element_info_ptr(element_name);
									if (uei_ptr) {
										const auto& unsupported_element_info = *uei_ptr;
										std::string error_desc = std::string("'") + element_name + std::string("' is not ")
											+ "supported (in type '" + qtype.getAsString() + "' used in this declaration). ";
										if ("" != unsupported_element_info.m_recommended_alternative) {
											error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
										}
										auto res = state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, typeLoc.getSourceRange().getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							};
							auto tsi_ptr = DD->getTypeSourceInfo();
							if (tsi_ptr) {
								check_for_and_handle_unsupported_element2(tsi_ptr->getTypeLoc(), SR, (*this).m_state1);
								apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), check_for_and_handle_unsupported_element2, (*this).m_state1);
							}
						}
					} else {
						std::string unsupported_type_str;
						if (qtype.getTypePtr()->isArrayType()) {
							unsupported_type_str = "Native array";
						} else if (qtype.getTypePtr()->isUnionType()) {
							unsupported_type_str = "Native union";
						}
						if ("" != unsupported_type_str) {
							const std::string error_desc = unsupported_type_str + std::string("s are not ")
								+ "supported (in this declaration of type '" + qtype.getAsString() + "'). ";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}
#endif /*0*/
				} else if (TD) {
					process_type_lifetime_annotations(*TD, m_state1, &MR, &Rewrite);
				} else {
					auto NAD = dyn_cast<const NamespaceAliasDecl>(D);
					if (NAD) {
						const auto ND = NAD->getNamespace();
						if (ND) {
							const auto source_namespace_str = ND->getQualifiedNameAsString();

							DECLARE_CACHED_CONST_STRING(mse_namespace_str1, mse_namespace_str());
							DECLARE_CACHED_CONST_STRING(mse_namespace_str2, mse_namespace_str() + std::string("::"));
							if ((source_namespace_str == mse_namespace_str1)
								|| string_begins_with(source_namespace_str, mse_namespace_str2)) {

								/* This check might be a bit of a hack. The idea is that we want to
								prevent the subversion of checks for use of elements in the mse::us
								namespace by using an alias to the namespace.
								*/

								const std::string error_desc = std::string("This namespace alias (of namespace '")
									+ source_namespace_str + "') could be used to subvert some of the checks. "
									+ "So its use requires a 'check suppression' directive.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSDeclRefExprUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSDeclRefExprUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssdeclrefexprutil1");

			if ((DRE != nullptr))
			{
				auto SR = nice_source_range(DRE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DRE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DREISR);
				if (suppress_check_flag) {
					return;
				}

				{
					auto D = DRE->getDecl();

#ifndef NDEBUG
					if (D->getType() != DRE->getType()) {
						auto D_qtype = D->getType();
						IF_DEBUG(auto D_qtype_str = D_qtype.getAsString();)
						MSE_RETURN_IF_TYPE_IS_NULL(D_qtype);
						auto DRE_qtype = DRE->getType();
						IF_DEBUG(auto DRE_qtype_str = DRE_qtype.getAsString();)
						MSE_RETURN_IF_TYPE_IS_NULL(DRE_qtype);
						if (D_qtype->isReferenceType() == DRE_qtype->isReferenceType()) {
							/* just a break-point site for debugging */
							int q = 5;
						}
					}
#endif /*!NDEBUG*/

					auto DD = dyn_cast<const DeclaratorDecl>(D);
					if (DD) {
						auto qtype = DD->getType();
						IF_DEBUG(std::string qtype_str = DD->getType().getAsString();)
						const auto qualified_name = DD->getQualifiedNameAsString();
						DECLARE_CACHED_CONST_STRING(mse_us_namespace_str1, mse_namespace_str() + "::us::");
						if (string_begins_with(qualified_name, mse_us_namespace_str1)) {

							DECLARE_CACHED_CONST_STRING(mse_us_namespace_str2, std::string("::") + mse_namespace_str() + "::us::");
							auto l_source_text = Rewrite.getRewrittenText(SR);
							if (string_begins_with(l_source_text, mse_us_namespace_str1)
								|| string_begins_with(l_source_text, mse_us_namespace_str2)) {

								/* We can't just flag all instantiations of elements in the 'mse::us' namespace because
								they are used by some of the safe library elements. We just want to flag cases where they
								are explicitly instantiated by the programmer. For now we'll just check that it's
								explicitly expressed in the source text. This wouldn't catch aliases of elements, so the
								declaration/definition of the offending aliases (including "using namespace") will need
								to be screened for and flagged as well. */


								const std::string error_desc = std::string("Elements in the 'mse::us' namespace (such as '"
									+ qualified_name + "') are potentially unsafe. ")
									+ "Their use requires a 'check suppression' directive.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}

				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};


	struct CDiag {
		CDiag() {}
		CDiag(IntrusiveRefCntPtr<DiagnosticIDs>& DiagIDs_ircptr_param, IntrusiveRefCntPtr<DiagnosticsEngine>& DiagEngine_ircptr_param)
		: DiagIDs_ircptr(DiagIDs_ircptr_param), DiagEngine_ircptr(DiagEngine_ircptr_param) {}
		IntrusiveRefCntPtr<DiagnosticIDs> DiagIDs_ircptr;
		IntrusiveRefCntPtr<DiagnosticsEngine> DiagEngine_ircptr;
	};
	struct CMultiTUState {
		std::vector<std::unique_ptr<clang::ASTUnit>> ast_units;
		std::vector<CDiag> diags;
	};

	void import_decl(ASTImporter& Importer, clang::Decl& decl_ref, clang::Rewriter& localRewriter, CompilerInstance &CI) {
		auto D = &decl_ref;
#if MU_LLVM_MAJOR <= 8
		auto *ToDecl = Importer.Import(D);
#elif MU_LLVM_MAJOR > 8
		const clang::Decl *ToDecl = nullptr;
#endif /*MU_LLVM_MAJOR*/
		if (ToDecl) {
			auto TDSR = nice_source_range(ToDecl->getSourceRange(), localRewriter);
			if (TDSR.isValid()) {
				auto TDSL = TDSR.getBegin();
				auto ToDecl_source_location_str = TDSL.printToString(CI.getASTContext().getSourceManager());

				if (std::string::npos != ToDecl_source_location_str.find("lodepng.cpp")) {
					int q = 5;
				} else if (std::string::npos != ToDecl_source_location_str.find("lodepng_util.cpp")) {
					int q = 5;
				} else {
					int q = 5;
				}
				int q = 5;
			} else {
				int q = 5;
			}
		} else {
			int q = 5;
		}
	}

	void import_other_TUs(CMultiTUState* multi_tu_state_ptr, CompilerInstance &CI, int current_tu_num = 0) {
		if (multi_tu_state_ptr) {
			errs() << "EXECUTE ACTION\n";
			//CompilerInstance &CI = getCompilerInstance();

			IntrusiveRefCntPtr<DiagnosticIDs> DiagIDs(CI.getDiagnostics().getDiagnosticIDs());
			IntrusiveRefCntPtr<DiagnosticsEngine> DiagEngine(new DiagnosticsEngine(DiagIDs, &CI.getDiagnosticOpts(),
					new IgnoringDiagConsumer(),/*ShouldOwnClient=*/true));
			CDiag diag(DiagIDs, DiagEngine);
			multi_tu_state_ptr->diags.push_back(diag);
			//CI.setDiagnostics(DiagEngine.get());

			//CI.getPreprocessor().setDiagnostics(*DiagEngine);

			//CI.getDiagnostics().setClient(new IgnoringDiagConsumer(), true/*take ownership*/);

			//CI.getDiagnostics().getClient()->BeginSourceFile(CI.getASTContext().getLangOpts());
			//CI.getDiagnostics().SetArgToStringFn(&FormatASTNodeDiagnosticArgument, &CI.getASTContext());

			//llvm::raw_fd_ostream *output = CI.createOutputFile("test.ast",true,false,"","",true);
			//auto output = CI.createOutputFile("test.ast",true,false,"","",true);
			//*output << "Test\n";
			//TheRewriter.setSourceMgr(CI.getASTContext().getSourceManager(), CI.getASTContext().getLangOpts());

			errs() << multi_tu_state_ptr->ast_units.size() << "\n";
			for (unsigned I = 0, N = multi_tu_state_ptr->ast_units.size(); I != N; ++I) {
				if (current_tu_num - 1 == int(I)) {
					continue;
				}
				errs() << "LOOP\n";
				//IntrusiveRefCntPtr<DiagnosticsEngine> DiagEngine(new DiagnosticsEngine(DiagIDs, &CI.getDiagnosticOpts(),
				//		new ForwardingDiagnosticConsumer(*CI.getDiagnostics().getClient()),/*ShouldOwnClient=*/true));
				IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
				TextDiagnosticPrinter *DiagClient = new TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);
				IntrusiveRefCntPtr<DiagnosticIDs> DiagID(new DiagnosticIDs());
				IntrusiveRefCntPtr<DiagnosticsEngine> DiagEngine(new DiagnosticsEngine(DiagID, &*DiagOpts, DiagClient));
				//multi_tu_state_ptr->ast_units.at(I)->getDiagnostics().setClient(DiagClient, true/*take ownership*/);

				//CI.getDiagnostics().Reset();
				//CI.getDiagnostics().setSourceManager(&(multi_tu_state_ptr->ast_units.at(I)->getASTContext().getSourceManager()));

				if (!multi_tu_state_ptr->ast_units.at(I))
					continue;

				if (true) {
					ASTImporter Importer(CI.getASTContext(),
							CI.getFileManager(),
								multi_tu_state_ptr->ast_units.at(I)->getASTContext(),
								multi_tu_state_ptr->ast_units.at(I)->getFileManager(),
								/* MinimalImport=*/false);

					clang::Rewriter localRewriter;
					localRewriter.setSourceMgr(multi_tu_state_ptr->ast_units.at(I)->getASTContext().getSourceManager(), multi_tu_state_ptr->ast_units.at(I)->getASTContext().getLangOpts());

					TranslationUnitDecl *TU = multi_tu_state_ptr->ast_units.at(I)->getASTContext().getTranslationUnitDecl();
					for (auto *D : TU->decls()) {
						assert(D);
						//D->dump();

						auto SR = nice_source_range(D->getSourceRange(), localRewriter);
						if (!SR.isValid()) {
							continue;
						}
						if (filtered_out_by_location(localRewriter.getSourceMgr(), SR.getBegin())) {
							continue;
						}

						auto SL = SR.getBegin();
						std::string debug_source_location_str = SL.printToString(localRewriter.getSourceMgr());

						if (std::string::npos != debug_source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != debug_source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						} else {
							int q = 5;
						}

						auto *ND = dyn_cast<const NamedDecl>(D);
						if (!ND) {
							continue;
						}
						IF_DEBUG(std::string name = ND->getNameAsString();)

						// Don't re-import __va_list_tag, __builtin_va_list.
						//if (auto const * const ND = dyn_cast<NamedDecl>(D))
							if (IdentifierInfo *II = ND->getIdentifier())
								if (II->isStr("__va_list_tag") || II->isStr("__builtin_va_list") || II->isStr("main"))
									continue;

						if (nullptr == Importer.GetAlreadyImportedOrNull(D)) {
							auto FD = D->getAsFunction();
							if (FD) {
								IF_DEBUG(std::string function_name = FD->getNameAsString();)
							} else if (llvm::isa<clang::NamespaceDecl>(D)) {
								auto NSD = llvm::cast<clang::NamespaceDecl>(D);
								assert(NSD);
								auto NNS = clang::NestedNameSpecifier::Create(multi_tu_state_ptr->ast_units.at(I)->getASTContext(), nullptr, NSD);
								if (false && NNS) {
#if MU_LLVM_MAJOR <= 8
									auto *NNSToDecl = Importer.Import(NNS);
#elif MU_LLVM_MAJOR > 8
									clang::NestedNameSpecifier *NNSToDecl = nullptr;
#endif /*MU_LLVM_MAJOR*/
									if (NNSToDecl) {
										int q = 5;
									} else {
										int q = 7;
									}
								} else {
									int q = 7;
								}

								if (EnableNamespaceImport) {
									for (auto *D : NSD->decls()) {
										D->dump();
										import_decl(Importer, *D, localRewriter, CI);
									}
								}
								continue;
							} else {
								int q = 5;
							}

							import_decl(Importer, *D, localRewriter, CI);
						} else {
							int q = 5;
						}
					}
				}

			}
			//CI.createDefaultOutputFile()->flush();
			//CI.createDefaultOutputFile()->close();
			//CI.getDiagnostics().getClient()->EndSourceFile();
		}
	}

	class Misc1 : public MatchFinder::MatchCallback
	{
	public:
		Misc1 (Rewriter &Rewrite, CTUState& state1, CompilerInstance &CI_ref, int current_tu_num = 0) :
			Rewrite(Rewrite), m_state1(state1), CI(CI_ref), m_current_tu_num(current_tu_num) {
			s_current_tu_num += 1;
			if (0 == m_current_tu_num) {
				m_current_tu_num = s_current_tu_num;
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			if (!m_other_TUs_imported) {
				if (CTUAnalysis) {
					import_other_TUs(&s_multi_tu_state, CI, m_current_tu_num);
				}
				m_other_TUs_imported = true;
			}
		}

		static CMultiTUState& s_multi_tu_state_ref() { return s_multi_tu_state; }

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
		CompilerInstance &CI;
		int m_current_tu_num = 0;

		bool m_other_TUs_imported = false;
		static CMultiTUState s_multi_tu_state;
		static int s_current_tu_num;
	};
	CMultiTUState Misc1::s_multi_tu_state;
	int Misc1::s_current_tu_num = 0;

	/**********************************************************************************************************************/
	class MyASTConsumer : public ASTConsumer {

	public:
		MyASTConsumer(Rewriter &R, CompilerInstance &CI, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param), HandlerMisc1(R, tu_state(), CI),
			HandlerForSSSSuppressCheckDirectiveCall(R, tu_state()), HandlerForSSSSuppressCheckDirectiveDeclField(R, tu_state()),
			HandlerForSSSSuppressCheckDirectiveDeclGlobal(R, tu_state()), 
			HandlerForSSSExprUtil(R, tu_state()), HandlerForSSSDeclUtil(R, tu_state()), HandlerForSSSDeclRefExprUtil(R, tu_state()),
			HandlerForSSSReturnStmt(R, tu_state()), HandlerForSSSRecordDecl2(R, tu_state()), HandlerForSSSAsAnFParam(R, tu_state()),
			HandlerForSSSMakeXScopePointerTo(R, tu_state()), HandlerForSSSNativeReferenceVar(R, tu_state()),
			HandlerForSSSArgToNativeReferenceParam(R, tu_state()), HandlerForSSSPointerArithmetic(R, tu_state()), HandlerForSSSAddressOf(R, tu_state()),
			HandlerForSSSNativePointerVar(R, tu_state()), HandlerForSSSCast(R, tu_state()), HandlerForSSSMemberFunctionCall(R, tu_state()), 
			HandlerForSSSFunctionCall(R, tu_state()), HandlerForSSSConstructionInitializer(R, tu_state()), HandlerForSSSPointerAssignment(R, tu_state())
		{
			Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerMisc1);
			Matcher.addMatcher(callExpr(argumentCountIs(0)).bind("mcssssuppresscheckcall"), &HandlerForSSSSuppressCheckDirectiveCall);
			Matcher.addMatcher(cxxMethodDecl(decl().bind("mcssssuppresscheckmemberdecl")), &HandlerForSSSSuppressCheckDirectiveDeclField);
			Matcher.addMatcher(functionDecl(decl().bind("mcssssuppresscheckglobaldecl")), &HandlerForSSSSuppressCheckDirectiveDeclGlobal);

			Matcher.addMatcher(expr().bind("mcsssexprutil1"), &HandlerForSSSExprUtil);
			Matcher.addMatcher(callExpr().bind("mcsssexprutil1"), &HandlerForSSSExprUtil);
			Matcher.addMatcher(cxxNewExpr().bind("mcsssexprutil1"), &HandlerForSSSExprUtil);
			Matcher.addMatcher(cxxDeleteExpr().bind("mcsssexprutil1"), &HandlerForSSSExprUtil);

			Matcher.addMatcher(decl().bind("mcsssdeclutil1"), &HandlerForSSSDeclUtil);
			Matcher.addMatcher(declRefExpr().bind("mcsssdeclrefexprutil1"), &HandlerForSSSDeclRefExprUtil);
			Matcher.addMatcher(returnStmt().bind("mcsssreturnstmt"), &HandlerForSSSReturnStmt);
			Matcher.addMatcher(clang::ast_matchers::recordDecl().bind("mcsssrecorddecl"), &HandlerForSSSRecordDecl2);
			Matcher.addMatcher(callExpr(/*argumentCountIs(1)*/).bind("mcsssasanfparam1"), &HandlerForSSSAsAnFParam);
			Matcher.addMatcher(callExpr(/*argumentCountIs(1)*/).bind("mcsssmakexscopepointerto1"), &HandlerForSSSMakeXScopePointerTo);
			Matcher.addMatcher(varDecl().bind("mcsssnativereferencevar1"), &HandlerForSSSNativeReferenceVar);
			Matcher.addMatcher(callExpr().bind("mcsssargtonativereferenceparam1"), &HandlerForSSSArgToNativeReferenceParam);
			Matcher.addMatcher(cxxMemberCallExpr().bind("mcsssargtonativereferenceparam1"), &HandlerForSSSArgToNativeReferenceParam);
			Matcher.addMatcher(cxxOperatorCallExpr().bind("mcsssargtonativereferenceparam1"), &HandlerForSSSArgToNativeReferenceParam);
			Matcher.addMatcher(expr(allOf(
				ignoringImplicit(ignoringParenImpCasts(expr(anyOf(
					unaryOperator(hasOperatorName("++")), unaryOperator(hasOperatorName("--")),
					binaryOperator(hasOperatorName("+=")), binaryOperator(hasOperatorName("-=")),
					binaryOperator(hasOperatorName("+")), binaryOperator(hasOperatorName("+=")),
					binaryOperator(hasOperatorName("-")), binaryOperator(hasOperatorName("-=")),
					binaryOperator(hasOperatorName("<=")), binaryOperator(hasOperatorName("<")),
					binaryOperator(hasOperatorName(">=")), binaryOperator(hasOperatorName(">")),
					arraySubscriptExpr()/*, clang::ast_matchers::castExpr(hasParent(arraySubscriptExpr()))*/
					)).bind("mcssspointerarithmetic1"))),
				/* We'd like to select for pointer types exclusively here, but it seems to miss some cases
				where the (dependent?) type is an alias for a pointer type. And "pointer equivalent" types
				that we use. So we'll delegate the filtering for pointer types to the handler itself. */
				//hasType(pointerType())
				anything()
				)).bind("mcssspointerarithmetic3"), &HandlerForSSSPointerArithmetic);
			Matcher.addMatcher(callExpr(allOf(
				callee(functionDecl(hasName("std::addressof"))),
				argumentCountIs(1),
				hasArgument(0, ignoringImplicit(ignoringParenImpCasts(expr().bind("mcsssaddressof1"))))
				)).bind("mcsssaddressof2"), &HandlerForSSSAddressOf);
			Matcher.addMatcher(expr(hasParent(
				unaryOperator(hasOperatorName("&")).bind("mcsssaddressof2")
				)).bind("mcsssaddressof1"), &HandlerForSSSAddressOf);
			Matcher.addMatcher(varDecl().bind("mcsssnativepointervar1"), &HandlerForSSSNativePointerVar);
			Matcher.addMatcher(cxxReinterpretCastExpr().bind("mcssscast1"), &HandlerForSSSCast);
			Matcher.addMatcher(cxxFunctionalCastExpr().bind("mcssscast1"), &HandlerForSSSCast);
			Matcher.addMatcher(cxxConstCastExpr().bind("mcssscast1"), &HandlerForSSSCast);
			Matcher.addMatcher(cxxMemberCallExpr().bind("mcsssmemberfunctioncall1"), &HandlerForSSSMemberFunctionCall);
			Matcher.addMatcher(cxxOperatorCallExpr().bind("mcssscxxoperatorcall1"), &HandlerForSSSMemberFunctionCall);
			Matcher.addMatcher(cxxMemberCallExpr().bind("mcsssfunctioncall1"), &HandlerForSSSFunctionCall);
			Matcher.addMatcher(cxxOperatorCallExpr().bind("mcssscxxoperatorcall1"), &HandlerForSSSFunctionCall);
			Matcher.addMatcher(cxxConstructExpr().bind("mcsssfunctioncall1"), &HandlerForSSSFunctionCall);
			Matcher.addMatcher(cxxCtorInitializer().bind("mcsssconstructioninitializer1"), &HandlerForSSSConstructionInitializer);
			Matcher.addMatcher(expr(allOf(
				ignoringImplicit(ignoringParenImpCasts(expr(
						binaryOperator(hasOperatorName("="))
					).bind("mcssspointerassignment1"))),
				/* We'd like to select for pointer types exclusively here, but it seems to miss some cases
				where the (dependent?) type is an alias for a pointer type. And "pointer equivalent" types
				that we use. So we'll delegate the filtering for pointer types to the handler itself. */
				//hasType(pointerType())
				anything()
				)).bind("mcssspointerassignment3"), &HandlerForSSSPointerAssignment);
			Matcher.addMatcher(expr(
				ignoringImplicit(ignoringParenImpCasts(cxxOperatorCallExpr().bind("mcssspointerassignment1")))
				).bind("mcssspointerassignment3"), &HandlerForSSSPointerAssignment);
		}

		~MyASTConsumer() {
			int q = 5;
		}

		void HandleTranslationUnit(ASTContext &Context) override 
		{
			Matcher.matchAST(Context);
		}

	private:

		CTUState *m_tu_state_ptr = nullptr;
		CTUState& tu_state() { return *m_tu_state_ptr;}

		Misc1 HandlerMisc1;
		MCSSSSuppressCheckDirectiveCall HandlerForSSSSuppressCheckDirectiveCall;
		MCSSSSuppressCheckDirectiveDeclField HandlerForSSSSuppressCheckDirectiveDeclField;
		MCSSSSuppressCheckDirectiveDeclGlobal HandlerForSSSSuppressCheckDirectiveDeclGlobal;
		MCSSSExprUtil HandlerForSSSExprUtil;
		MCSSSDeclUtil HandlerForSSSDeclUtil;
		MCSSSDeclRefExprUtil HandlerForSSSDeclRefExprUtil;
		MCSSSReturnStmt HandlerForSSSReturnStmt;
		MCSSSRecordDecl2 HandlerForSSSRecordDecl2;
		MCSSSAsAnFParam HandlerForSSSAsAnFParam;
		MCSSSMakeXScopePointerTo HandlerForSSSMakeXScopePointerTo;
		MCSSSNativeReferenceVar HandlerForSSSNativeReferenceVar;
		MCSSSArgToNativeReferenceParam HandlerForSSSArgToNativeReferenceParam;
		MCSSSPointerArithmetic HandlerForSSSPointerArithmetic;
		MCSSSAddressOf HandlerForSSSAddressOf;
		MCSSSNativePointerVar HandlerForSSSNativePointerVar;
		MCSSSCast HandlerForSSSCast;
		MCSSSMemberFunctionCall HandlerForSSSMemberFunctionCall;
		MCSSSFunctionCall HandlerForSSSFunctionCall;
		MCSSSConstructionInitializer HandlerForSSSConstructionInitializer;
		MCSSSPointerAssignment HandlerForSSSPointerAssignment;

		MatchFinder Matcher;
	};

	/**********************************************************************************************************************/

	struct CFirstIncludeInfo {
		CFirstIncludeInfo(const SourceLocation &beginning_of_file_loc) : m_beginning_of_file_loc(beginning_of_file_loc),
				m_beginning_of_file_loc_is_valid(true) {}

		bool m_legacyhelpers_include_directive_found = false;

		bool m_first_include_directive_loc_is_valid = false;
		SourceLocation m_first_include_directive_loc;

		bool m_first_macro_directive_ptr_is_valid = false;
		const MacroDirective* m_first_macro_directive_ptr = nullptr;

		SourceLocation m_beginning_of_file_loc;
		bool m_beginning_of_file_loc_is_valid = false;
	};

	class MyPPCallbacks : public PPCallbacks
	{
	public:
		MyPPCallbacks(Rewriter& Rewriter_ref, CompilerInstance &CI_ref, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param), m_Rewriter_ref(Rewriter_ref), CI(CI_ref) {
			int q = 5;
		}
		~MyPPCallbacks() {
			int q = 5;
		}

		void InclusionDirective(
		SourceLocation hash_loc,
		const Token &include_token,
		StringRef file_name,
		bool is_angled,
		CharSourceRange filename_range,
	#if MU_LLVM_MAJOR < 15
		const FileEntry *file,
	#elif MU_LLVM_MAJOR < 16
		Optional<FileEntryRef> File,
	#else /*MU_LLVM_MAJOR*/
		OptionalFileEntryRef File,
	#endif /*MU_LLVM_MAJOR*/
		StringRef search_path,
		StringRef relative_path,
		const clang::Module *imported
	#if MU_LLVM_MAJOR <= 6
	#elif MU_LLVM_MAJOR >= 8
		, SrcMgr::CharacteristicKind file_type
	#endif /*MU_LLVM_MAJOR*/
		) override {

			if (current_fii_shptr()) {
				if (!(current_fii_shptr()->m_first_include_directive_loc_is_valid)) {
					current_fii_shptr()->m_first_include_directive_loc = hash_loc;
					current_fii_shptr()->m_first_include_directive_loc_is_valid = true;
				}

				auto file_name_str = std::string(file_name);
				if ("mselegacyhelpers.h" == file_name_str) {
					current_fii_shptr()->m_legacyhelpers_include_directive_found = true;
				}
				int q = 5;
			}
		}

		void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) override {
			{
				auto MNTSL = MacroNameTok.getLocation();
				auto MNTSLE = MacroNameTok.getEndLoc();
				std::string macro_name = m_Rewriter_ref.getRewrittenText(SourceRange(MNTSL, MNTSLE));
				if ("MSE_SCOPEPOINTER_DISABLED" == macro_name) {
					(*this).tu_state().m_MSE_SCOPEPOINTER_DISABLED_defined = true;
				} else if ("MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED" == macro_name) {
					(*this).tu_state().m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined = true;
				} else if ("MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS" == macro_name) {
					(*this).tu_state().m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined = true;
				} else if ("MSE_CHAR_STAR_EXEMPTED" == macro_name) {
					(*this).tu_state().m_MSE_CHAR_STAR_EXEMPTED_defined = true;
				}
			}

			if (current_fii_shptr()) {
				if (!(current_fii_shptr()->m_first_macro_directive_ptr_is_valid)) {
					if (MD) {
						current_fii_shptr()->m_first_macro_directive_ptr = MD;
						current_fii_shptr()->m_first_macro_directive_ptr_is_valid = true;
					} else { assert(false); }
				}
			}
		}

		void FileChanged(SourceLocation Loc, FileChangeReason Reason,
								SrcMgr::CharacteristicKind FileType,
								FileID PrevFID = FileID()) override {

			bool filename_is_invalid = false;
			auto full_path_name = std::string(m_Rewriter_ref.getSourceMgr().getBufferName(Loc, &filename_is_invalid));

			std::string full_path_name2 = Loc.printToString(m_Rewriter_ref.getSourceMgr());
			auto last_colon_pos = full_path_name2.find_first_of(':');
			if (last_colon_pos + 1 < full_path_name2.size()) {
				full_path_name2 = full_path_name2.substr(0, last_colon_pos);
			} else {
				int q = 7;
			}
			if ("" == full_path_name) {
				full_path_name = full_path_name2;
			}

			if (string_begins_with(full_path_name, "/home")) {
				int q = 5;
			}
			if (std::string::npos != full_path_name.find("iostream")) {
				int q = 5;
			}

			if (PPCallbacks::FileChangeReason::EnterFile == Reason) {
				auto fii_iter = m_first_include_info_map.find(Loc);
					if (m_first_include_info_map.end() == fii_iter) {
						std::unordered_map<SourceLocation, std::shared_ptr<CFirstIncludeInfo>>::value_type item(Loc, std::make_shared<CFirstIncludeInfo>(Loc));
						fii_iter = (m_first_include_info_map.insert(item)).first;
					}
					m_current_fii_shptr_stack.push_back((*fii_iter).second);
			} else if (PPCallbacks::FileChangeReason::ExitFile == Reason) {
				if (1 <= m_current_fii_shptr_stack.size()) {
					m_current_fii_shptr_stack.pop_back();
				} else {
					assert(false);
				}
			}
		}

		std::shared_ptr<CFirstIncludeInfo> current_fii_shptr() {
			std::shared_ptr<CFirstIncludeInfo> retval = nullptr;
			if (1 <= m_current_fii_shptr_stack.size()) {
				retval = m_current_fii_shptr_stack.back();
			}
			return retval;
		}

		CTUState *m_tu_state_ptr = nullptr;
		CTUState& tu_state() { return *m_tu_state_ptr;}

		std::unordered_map<SourceLocation, std::shared_ptr<CFirstIncludeInfo>> m_first_include_info_map;
		std::vector<std::shared_ptr<CFirstIncludeInfo>> m_current_fii_shptr_stack;
		Rewriter& m_Rewriter_ref;
		CompilerInstance &CI;
	};

	/* The number of parameters ASTFrontendAction::BeginSourceFileAction() has depends
	* on the version of the llvm library being used. Using ASTFrontendActionCompatibilityWrapper1
	* in place of ASTFrontendAction insulates you from this difference based on the
	* library version being used. */
	class ASTFrontendActionCompatibilityWrapper1 : public ASTFrontendAction {
	public:
		virtual bool BeginSourceFileAction(CompilerInstance &ci) = 0;
		virtual bool BeginSourceFileAction(CompilerInstance &ci, StringRef) {
			return BeginSourceFileAction(ci);
		}
	};

	struct CFileConversionRecord {
	public:
		std::string m_path;
		std::string m_original_filename;
		std::string m_target_filename;
		std::vector<size_t> m_converted_version_tu_numbers;
	};

	/* global variable to store (some of) the information obtained during the "checker" pass */
	thread_local std::vector<CTUState> g_final_tu_states;

	class MyFrontendAction : public ASTFrontendActionCompatibilityWrapper1
	{
	public:
		MyFrontendAction() {
			int q = 5;
		}
		~MyFrontendAction() {
			std::cout << "\n" << (*this).m_tu_state.m_error_records.size() << " verification failures. \n";
			std::cout.flush();
			//llvm::errs() << "\n~MyFrontendAction() " << '\n';

			/* copy the final states to global storage so the information can be used by subsequent passes */
			g_final_tu_states.push_back((*this).m_tu_state);
		}

		bool BeginSourceFileAction(CompilerInstance &ci) override {
			std::unique_ptr<MyPPCallbacks> my_pp_callbacks_ptr(new MyPPCallbacks(TheRewriter, ci, (*this).m_tu_state));
			auto my_pp_callbacks_rawptr = my_pp_callbacks_ptr.get();
			m_callbacks_stack.push_back(my_pp_callbacks_rawptr);

			clang::Preprocessor &pp = ci.getPreprocessor();
			pp.addPPCallbacks(std::move(my_pp_callbacks_ptr));

			return true;
		}

		void EndSourceFileAction() override {
			//TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(llvm::outs());

			{
				clang::CompilerInstance &ci = getCompilerInstance();
				clang::Preprocessor &pp = ci.getPreprocessor();
				//MyPPCallbacks *my_pp_callbacks_ptr = static_cast<MyPPCallbacks *>(pp.getPPCallbacks());
				auto pp_callbacks_ptr = pp.getPPCallbacks();

				assert(pp_callbacks_ptr);
				if (!pp_callbacks_ptr) {
					int q = 7;
				}
				if (m_callbacks_stack.cend() == std::find(m_callbacks_stack.cbegin(), m_callbacks_stack.cend(), pp_callbacks_ptr)) {
					pp_callbacks_ptr = nullptr;
				}

				if (1 <= m_callbacks_stack.size()) {
					if (m_callbacks_stack.back() != pp_callbacks_ptr) {
						int q = 7;
						if (!pp_callbacks_ptr) {
							int q = 7;
							pp_callbacks_ptr = m_callbacks_stack.back();
							m_callbacks_stack.pop_back();
						} 
					} else {
						m_callbacks_stack.pop_back();
					}
				}
				if (pp_callbacks_ptr) {
					MyPPCallbacks *my_pp_callbacks_ptr = static_cast<MyPPCallbacks *>(pp_callbacks_ptr);

					for (auto& item_ref : my_pp_callbacks_ptr->m_first_include_info_map) {
						auto& fii_ref = *(item_ref.second);
						assert(fii_ref.m_beginning_of_file_loc_is_valid);

						bool filename_is_invalid = false;
						auto full_path_name = std::string(TheRewriter.getSourceMgr().getBufferName(fii_ref.m_beginning_of_file_loc, &filename_is_invalid));

						if (filtered_out_by_location(TheRewriter.getSourceMgr(), fii_ref.m_beginning_of_file_loc)) {
							continue;
						}

						if (!(fii_ref.m_legacyhelpers_include_directive_found)) {
							if (fii_ref.m_first_include_directive_loc_is_valid) {
								TheRewriter.InsertTextBefore(fii_ref.m_first_include_directive_loc,
										"\n#include \"mselegacyhelpers.h\"\n");
							} else if (fii_ref.m_first_macro_directive_ptr_is_valid) {
								TheRewriter.InsertTextAfterToken(fii_ref.m_first_macro_directive_ptr->getLocation(),
										"\n#include \"mselegacyhelpers.h\"\n");
							} else if (fii_ref.m_beginning_of_file_loc_is_valid) {
								TheRewriter.InsertTextBefore(fii_ref.m_beginning_of_file_loc,
										"\n#include \"mselegacyhelpers.h\"\n");
							}
						}
					}
					my_pp_callbacks_ptr->m_tu_state_ptr = nullptr;
				} else {
					//assert(false);
					int q = 7;
				}
			}
		}

		std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef file) override {
			TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
			return std::make_unique<MyASTConsumer>(TheRewriter, CI, (*this).m_tu_state);
		}

	private:
		CTUState m_tu_state;
		Rewriter TheRewriter;
		std::vector<PPCallbacks*> m_callbacks_stack;
	};

	auto buildASTs_and_run(ClangTool& Tool, Options options = Options()) {
        CheckSystemHeader = options.CheckSystemHeader;
        MainFileOnly = options.MainFileOnly;
        CTUAnalysis = options.CTUAnalysis;
        EnableNamespaceImport = options.EnableNamespaceImport;
        SuppressPrompts = options.SuppressPrompts;

		int Status = Tool.buildASTs(Misc1::s_multi_tu_state_ref().ast_units);
		int ASTStatus = 0;
		if (Status == 1) {
			// Building ASTs failed.
			std::cout << "\nBuild failed. \n";
			return 1;
		} else if (Status == 2) {
			ASTStatus |= 1;
			llvm::errs() << "Failed to build AST for some of the files, "
							<< "results may be incomplete."
							<< "\n";
		} else {
			assert(Status == 0 && "Unexpected status returned");
		}

		auto retval = Tool.run(newFrontendActionFactory<MyFrontendAction>().get());

		if (0 != retval) {
			std::cout << "\nThere were errors in the build. Results may be unreliable.\n";
		}

		return retval;
	}
}

#endif //__CHECKER_H
