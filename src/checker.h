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
#include <locale>

#include <cstdio>
#include <memory>
#include <array>

#include <fstream>

extern std::string g_mse_namespace_str;

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

    bool CheckSystemHeader = false;
    bool MainFileOnly = false;
    bool ConvertToSCPP = true;
    bool CTUAnalysis = false;
    bool EnableNamespaceImport = false;
    bool SuppressPrompts = false;
    bool DoNotReplaceOriginalSource = false;
    std::string MergeCommand = "";

    struct Options {
        bool CheckSystemHeader = false;
        bool MainFileOnly = false;
        bool ConvertToSCPP = true;
        bool CTUAnalysis = false;
        bool EnableNamespaceImport = false;
        bool SuppressPrompts = false;
        bool DoNotReplaceOriginalSource = false;
        std::string MergeCommand = "";
    };



	class COrderedSourceRange : public SourceRange {
		public:
		typedef SourceRange base_class;
		using base_class::base_class;
		COrderedSourceRange(const SourceRange& src) : base_class(src.getBegin(), src.getEnd()) {}
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
		bool contains(const SourceRange& SR) const {
			for (auto it = (*this).cbegin(); (*this).cend() != it; it++) {
				if (first_is_a_subset_of_second(SR, *it)) {
					return true;
				}
			}
			return false;
		}
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

	class CTUState {
	public:
		/* This container holds the locations of regions of code for which checking is
		(indicated to be) suppressed. */
		CSuppressCheckRegionSet m_suppress_check_region_set;

		/* Set of detecteed errors and warnings. */
		CErrorRecords m_error_records;

		/* Preprocessor symbols of interested. */
		bool m_MSE_SCOPEPOINTER_DISABLED_defined = false;
		bool m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined = false;
		bool m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined = false;
		bool raw_pointer_scope_restrictions_are_disabled() const {
			return (m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined
			|| m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined);
		}
	};

	class MCSSSSupressCheckDirectiveCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSupressCheckDirectiveCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcssssuppresscheckmemberdeclmcssssuppresscheckcall");

			if ((CE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CESL)) {
					return void();
				}

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				//assert(0 == num_args);
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string suppress_check_directive_str = g_mse_namespace_str + "::rsv::suppress_check_directive";
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
							auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
							auto CEISL = CEISR.getBegin();
							auto CEISLE = CEISR.getEnd();
							for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
								if (nullptr != (*child_iter)) {
									auto l_STISR = instantiation_source_range((*child_iter)->getSourceRange(), Rewrite);
									SourceLocation l_STISL = l_STISR.getBegin();
									SourceLocation l_STISLE = l_STISR.getEnd();
									if (CEISL == l_STISL) {
										int q = 5;
									}
									if ((CEISLE < l_STISL)
										|| ((CEISLE == l_STISL) && (CEISLE < l_STISLE))) {
										m_state1.m_suppress_check_region_set.emplace(l_STISR);
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

	class MCSSSSupressCheckDirectiveDeclField : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSupressCheckDirectiveDeclField (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::CXXMethodDecl* CXXMD = MR.Nodes.getNodeAs<clang::CXXMethodDecl>("mcssssuppresscheckmemberdecl");

			if ((CXXMD != nullptr))
			{
				auto CXXMDSR = nice_source_range(CXXMD->getSourceRange(), Rewrite);
				SourceLocation CXXMDSL = CXXMDSR.getBegin();
				SourceLocation CXXMDSLE = CXXMDSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCXXMDSL = ASTC->getFullLoc(CXXMDSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CXXMDSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CXXMDSL)) {
					return void();
				}

				std::string source_text;
				if (CXXMDSL.isValid() && CXXMDSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CXXMDSL, CXXMDSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				auto method_name = CXXMD->getNameAsString();
				static const std::string suppress_checks_prefix = "mse_suppress_check_directive";
				if (suppress_checks_prefix == method_name.substr(0, suppress_checks_prefix.length())) {
					auto decl_context = CXXMD->getDeclContext();
					if (!decl_context) {
						assert(false);
					} else {
						auto CXXMDISR = instantiation_source_range(CXXMD->getSourceRange(), Rewrite);
						auto CXXMDISL = CXXMDISR.getBegin();
						auto CXXMDISLE = CXXMDISR.getEnd();

						for (auto decl_iter = decl_context->decls_begin(); decl_iter != decl_context->decls_end(); decl_iter++) {
							if (nullptr != (*decl_iter)) {
								auto l_DISR = instantiation_source_range((*decl_iter)->getSourceRange(), Rewrite);
								SourceLocation l_DISL = l_DISR.getBegin();
								SourceLocation l_DISLE = l_DISR.getEnd();

								if (filtered_out_by_location(MR, l_DISL)) {
									continue;
								}

								std::string l_source_text;
								if (l_DISL.isValid() && l_DISLE.isValid()) {
									l_source_text = Rewrite.getRewrittenText(SourceRange(l_DISL, l_DISLE));
								} else {
									continue;
								}
								if ("" != l_source_text) {
									int q = 5;
								}

								if (CXXMDISL == l_DISL) {
									int q = 5;
								}
								if ((CXXMDISLE < l_DISL)
									|| ((CXXMDISLE == l_DISL) && (CXXMDISLE < l_DISLE))) {
									m_state1.m_suppress_check_region_set.emplace(l_DISR);
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

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};
	template <typename NodeT>
	auto get_containing_scope(const NodeT* NodePtr, clang::ASTContext& context) {
		const CompoundStmt* retval = nullptr;
		if (!NodePtr) {
			return retval;
		}
		const auto& parents = context.getParents(*NodePtr);
		if ( parents.empty() ) {
			return retval;
		}
		const auto num_parents = parents.size();
		const CompoundStmt* ST = parents[0].template get<CompoundStmt>();
		if (ST) {
			retval = dyn_cast<const CompoundStmt>(ST);
			assert(retval);
		} else {
			return get_containing_scope(&(parents[0]), context);
		}
		return retval;
	}
	bool first_is_contained_in_scope_of_second(const Stmt* ST1, const Stmt* ST2, clang::ASTContext& context) {
		bool retval = true;
		auto scope1 = get_containing_scope(ST1, context);
		const auto scope2 = get_containing_scope(ST2, context);
		bool parent_obtained = false;
		while (scope2 != scope1) {
			scope1 = get_containing_scope(scope1, context);
			if (!scope1) {
				retval = (scope2 == scope1);
				break;
			}
		}
		return retval;
	}
	bool first_is_contained_in_scope_of_second(const VarDecl& VD1_cref, const VarDecl& VD2_cref, clang::ASTContext& context) {
		bool retval = true;
		auto ST1 = VD1_cref.getAnyInitializer();
		{
			const auto storage_duration = VD1_cref.getStorageDuration();
			if (clang::StorageDuration::SD_Automatic == storage_duration) {
			} else if (clang::StorageDuration::SD_Thread == storage_duration) {
				/* (In our usage) a null ST1 means that the pointer to be modified has
				thread_local lifetime (i.e. no containing parent scope), and so can essentially
				only point to objects that also have thread local lifetime. */
				ST1 = nullptr;
			}
		}

		auto ST2 = VD2_cref.getAnyInitializer();
		{
			const auto storage_duration = VD2_cref.getStorageDuration();
			if (clang::StorageDuration::SD_Automatic == storage_duration) {
			} else if (clang::StorageDuration::SD_Thread == storage_duration) {
				ST2 = nullptr;
			}
		}

		return first_is_contained_in_scope_of_second(ST1, ST2, context);
	}
	bool first_is_contained_in_scope_of_second(const DeclContext* scope1, const DeclContext* const scope2) {
		if (scope2 == scope1) {
			return true;
		}
		while (scope1) {
			scope1 = scope1->getParent();
			if (scope2 == scope1) {
				return true;
			}
		}
		return false;
	}

	bool has_ancestor_base_class(const clang::QualType qtype, const std::string& qualified_base_class_name);
	bool has_ancestor_base_class(const clang::Type& type, const std::string& qualified_base_class_name) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			if (qualified_base_class_name == qname) {
				return true;
			}
			for (const auto& base : CXXRD->bases()) {
				const auto base_qtype = base.getType();
				const auto base_qtype_str = base_qtype.getAsString();

				if (has_ancestor_base_class(base.getType(), qualified_base_class_name)) {
					return true;
				}
			}
		}

		return retval;
	}
	bool has_ancestor_base_class(const clang::QualType qtype, const std::string& qualified_base_class_name) {
		bool retval = false;

		std::string qtype_str = qtype.getAsString();
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = has_ancestor_base_class(*TP, qualified_base_class_name);
		}
		return retval;
	}

	bool is_xscope_type(const clang::QualType qtype, const CTUState& tu_state_cref);
	bool is_xscope_type(const clang::Type& type, const CTUState& tu_state_cref) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			const std::string xscope_tag_str = g_mse_namespace_str + "::us::impl::XScopeTagBase";
			if (has_ancestor_base_class(type, xscope_tag_str)) {
				return true;
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
	bool is_xscope_type(const clang::QualType qtype, const CTUState& tu_state_cref) {
		bool retval = false;

		std::string qtype_str = qtype.getAsString();
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_xscope_type(*TP, tu_state_cref);
		}
		return retval;
	}

	bool has_tag_method(const clang::CXXRecordDecl& record_decl_cref, const std::string& target_name) {
		bool retval = false;
		auto qname = record_decl_cref.getQualifiedNameAsString();
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

	bool is_async_shareable(const clang::QualType qtype, const CTUState& tu_state_cref);
	bool is_async_shareable(const clang::Type& type, const CTUState& tu_state_cref) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			static const std::string s_async_shareable_tag_str = "async_shareable_tag";
			static const std::string s_async_shareable_and_passable_tag_str = "async_shareable_and_passable_tag";
			if (has_tag_method(*CXXRD, s_async_shareable_tag_str) || has_tag_method(*CXXRD, s_async_shareable_and_passable_tag_str)) {
				return true;
			} else {
				const std::string async_not_shareable_tag_str = g_mse_namespace_str + "::us::impl::AsyncNotShareableTagBase";
				const std::string async_not_shareable_and_not_passable_tag_str = g_mse_namespace_str + "::us::impl::AsyncNotShareableAndNotPassableTagBase";
				if (has_ancestor_base_class(type, async_not_shareable_tag_str)
					|| has_ancestor_base_class(type, async_not_shareable_and_not_passable_tag_str)) {
					return false;
				} else {
					for (const auto& FD : CXXRD->fields()) {
						assert(FD);
						const auto FD_qtype = FD->getType();
						auto FD_qtype_str = FD_qtype.getAsString();

						if ((!is_async_shareable(FD->getType(), tu_state_cref))
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
		} else {
			/* todo: support pointers to non-capture lamdas and any other types we're forgetting */
		}

		return retval;
	}
	bool is_async_shareable(const clang::QualType qtype, const CTUState& tu_state_cref) {
		bool retval = false;

		std::string qtype_str = qtype.getAsString();
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_async_shareable(*TP, tu_state_cref);
		}
		return retval;
	}

	bool is_async_passable(const clang::QualType qtype, const CTUState& tu_state_cref);
	bool is_async_passable(const clang::Type& type, const CTUState& tu_state_cref) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			static const std::string s_async_passable_tag_str = "async_passable_tag";
			static const std::string s_async_passable_and_passable_tag_str = "async_passable_and_passable_tag";
			if (has_tag_method(*CXXRD, s_async_passable_tag_str) || has_tag_method(*CXXRD, s_async_passable_and_passable_tag_str)) {
				return true;
			} else {
				const std::string async_not_passable_tag_str = g_mse_namespace_str + "::us::impl::AsyncNotPassableTagBase";
				const std::string async_not_shareable_and_not_passable_tag_str = g_mse_namespace_str + "::us::impl::AsyncNotShareableAndNotPassableTagBase";
				if (has_ancestor_base_class(type, async_not_passable_tag_str)
					|| has_ancestor_base_class(type, async_not_shareable_and_not_passable_tag_str)) {
					return false;
				} else {
					for (const auto& FD : CXXRD->fields()) {
						assert(FD);
						const auto FD_qtype = FD->getType();
						auto FD_qtype_str = FD_qtype.getAsString();

						if (!is_async_passable(FD->getType(), tu_state_cref)) {
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
	bool is_async_passable(const clang::QualType qtype, const CTUState& tu_state_cref) {
		bool retval = false;

		std::string qtype_str = qtype.getAsString();
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_async_passable(*TP, tu_state_cref);
		}
		return retval;
	}


	class MCSSSStmtUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSStmtUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Stmt* ST = MR.Nodes.getNodeAs<clang::Stmt>("mcsssstmtutil1");

			if (ST != nullptr)
			{
				auto STSR = nice_source_range(ST->getSourceRange(), Rewrite);
				SourceLocation STSL = STSR.getBegin();
				SourceLocation STSLE = STSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSTSL = ASTC->getFullLoc(STSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = STSL.printToString(*MR.SourceManager);

				if (std::string::npos != source_location_str.find("1proj")) {
					int q = 5;
				}

				if (filtered_out_by_location(MR, STSL)) {
					return void();
				}

				auto STISR = instantiation_source_range(ST->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(STISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (STSL.isValid() && STSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(STSL, STSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				auto const * const EX = dyn_cast<const Expr>(ST);
				if (EX) {
					auto const * const EX_ii = EX->IgnoreImplicit()->IgnoreParenImpCasts();
					auto const * const CE = dyn_cast<const CallExpr>(EX_ii);
					if (CE) {
						auto function_decl = CE->getDirectCallee();
						auto num_args = CE->getNumArgs();
						if (function_decl) {
							const std::string qualified_function_name = function_decl->getQualifiedNameAsString();
							static const std::string std_move_str = "std::move";
							if (std_move_str == qualified_function_name) {
								if (1 == num_args) {
									if (true || string_begins_with(source_text, std_move_str)) {
										/* todo: check for aliases */
										const std::string error_desc = std::string("Explicit use of std::move() ")
											+ "is not (yet) supported.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, STSL, error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n";
										}
									}
								}
							} else {
								static const std::string malloc_str = "malloc";
								static const std::string realloc_str = "realloc";
								static const std::string free_str = "free";
								static const std::string alloca_str = "alloca";
								std::string unsupported_function_str;
								if (malloc_str == qualified_function_name) {
									unsupported_function_str = malloc_str;
								} else if (realloc_str == qualified_function_name) {
									unsupported_function_str = realloc_str;
								} else if (free_str == qualified_function_name) {
									unsupported_function_str = free_str;
								} else if (alloca_str == qualified_function_name) {
									unsupported_function_str = alloca_str;
								}
								if ("" != unsupported_function_str) {
									const std::string error_desc = std::string("The '") + unsupported_function_str
										+ "' function is not supported.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, STSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
									}
								}
							}
						}
					} else {
						auto const * const CXXNE = dyn_cast<const clang::CXXNewExpr>(EX_ii);
						auto const * const CXXDE = dyn_cast<const clang::CXXDeleteExpr>(EX_ii);
						std::string unsupported_expression_str;
						if (CXXNE) {
							unsupported_expression_str = "operator new";
						} else if (CXXDE) {
							unsupported_expression_str = "operator delete";
						}
						if ("" != unsupported_expression_str) {
							const std::string error_desc = std::string("'") + unsupported_expression_str
								+ "' is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, STSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto DSR = nice_source_range(D->getSourceRange(), Rewrite);
				SourceLocation DSL = DSR.getBegin();
				SourceLocation DSLE = DSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FDSL = ASTC->getFullLoc(DSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = DSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, DSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find(":128:")) {
					int q = 5;
				}

				auto DISR = instantiation_source_range(D->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(DISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (DSL.isValid() && DSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(DSL, DSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();

					if (is_xscope_type(qtype, (*this).m_state1)) {
						int q = 5;
					}

					auto VD = dyn_cast<const clang::VarDecl>(D);
					if (VD) {
						const auto storage_duration = VD->getStorageDuration();
						const auto qualified_name = VD->getQualifiedNameAsString();

						if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
							bool satisfies_checks = false;
							const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
							if (CXXRD) {
								auto name = CXXRD->getQualifiedNameAsString();
								const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
								if (tmplt_CXXRD) {
									name = tmplt_CXXRD->getQualifiedNameAsString();
								}

								const std::string mse_rsv_static_immutable_obj_str1 = g_mse_namespace_str + "::rsv::TStaticImmutableObj";
								const std::string std_atomic_str = std::string("std::atomic");
								const std::string mse_AsyncSharedV2ReadWriteAccessRequester_str = g_mse_namespace_str + "::TAsyncSharedV2ReadWriteAccessRequester";
								const std::string mse_AsyncSharedV2ReadOnlyAccessRequester_str = g_mse_namespace_str + "::TAsyncSharedV2ReadOnlyAccessRequester";
								const std::string mse_TAsyncSharedV2ImmutableFixedPointer_str = g_mse_namespace_str + "::TAsyncSharedV2ImmutableFixedPointer";
								const std::string mse_TAsyncSharedV2AtomicFixedPointer_str = g_mse_namespace_str + "::TAsyncSharedV2AtomicFixedPointer";
								const std::string mse_rsv_ThreadLocalObj_str = g_mse_namespace_str + "::rsv::TThreadLocalObj";

								if ((name == mse_rsv_static_immutable_obj_str1)
									|| (name == std_atomic_str)
									|| (name == mse_AsyncSharedV2ReadWriteAccessRequester_str)
									|| (name == mse_AsyncSharedV2ReadOnlyAccessRequester_str)
									|| (name == mse_TAsyncSharedV2ImmutableFixedPointer_str)
									|| (name == mse_TAsyncSharedV2AtomicFixedPointer_str)
									|| ((name == mse_rsv_ThreadLocalObj_str) && (clang::StorageDuration::SD_Thread == storage_duration))
									) {
									satisfies_checks = true;
								}
							}

							if (!satisfies_checks) {
								std::string error_desc;
								if (clang::StorageDuration::SD_Static == storage_duration) {
									error_desc = std::string("'static storage duration' is not ")
										+ "supported for this type. Eligible types wrapped in the 'mse::rsv::TStaticImmutableObj<>' "
										+ "transparent template wrapper would be supported. Other supported wrappers include: "
										+ "mse::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
										+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
								} else {
									assert(clang::StorageDuration::SD_Thread == storage_duration);
									error_desc = std::string("'thread local storage duration' is not ")
										+ "supported for this type. Eligible types wrapped in the 'mse::rsv::TThreadLocalObj<>' "
										+ "transparent template wrapper would be supported. Other supported wrappers include: "
										+ "mse::rsv::TStaticImmutableObj<>, mse::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
										+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
								}
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
								}
							}
						}

						if (qtype.getTypePtr()->isScalarType()) {
							const auto* init_EX = VD->getInit();
							if (!init_EX) {
								auto *PVD = dyn_cast<const ParmVarDecl>(VD);
								if (!PVD) {
									const std::string error_desc = std::string("uninitialized ")
										+ "scalar variable ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
									}
								}
							}
						}
					} else {
						auto FD = dyn_cast<const clang::FieldDecl>(D);
						if (FD) {
							if (qtype.getTypePtr()->isPointerType() || qtype.getTypePtr()->isReferenceType()) {
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
										const std::string error_desc = std::string("Scalar member fields ")
											+ "require direct initializers.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n";
										}
									}
								}
							}
						}
					}

					const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
					if (CXXRD) {
						auto name = CXXRD->getQualifiedNameAsString();
						const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
						if (tmplt_CXXRD) {
							name = tmplt_CXXRD->getQualifiedNameAsString();
						}
						const std::string mse_rsv_TAsyncShareableObj_str1 = g_mse_namespace_str + "::rsv::TAsyncShareableObj";
						const std::string mse_rsv_TAsyncPassableObj_str1 = g_mse_namespace_str + "::rsv::TAsyncPassableObj";
						const std::string mse_rsv_TAsyncShareableAndPassableObj_str1 = g_mse_namespace_str + "::rsv::TAsyncShareableAndPassableObj";
						const std::string mse_rsv_TFParam_str = g_mse_namespace_str + "::rsv::TFParam";
						if (mse_rsv_TAsyncShareableObj_str1 == name) {
							if (1 == CXXRD->getNumBases()) {
								const auto& base = *(CXXRD->bases_begin());
								const auto base_qtype = base.getType();
								const auto base_qtype_str = base_qtype.getAsString();
								if (!is_async_shareable(base_qtype, (*this).m_state1)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncShareableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely shared (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
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
								if (!is_async_passable(base_qtype, (*this).m_state1)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncPassableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely passed (between threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
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
								if ((!is_async_shareable(base_qtype, (*this).m_state1)) || (!is_async_passable(base_qtype, (*this).m_state1))) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncShareableAndPassableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely shared and passed (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
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
										auto CE = dyn_cast<const clang::CallExpr>(VD->getInit()->IgnoreImplicit()->IgnoreParenImpCasts());
										if (CE) {
											auto function_decl = CE->getDirectCallee();
											auto num_args = CE->getNumArgs();
											if (function_decl) {
												std::string function_name = function_decl->getNameAsString();
												std::string qualified_function_name = function_decl->getQualifiedNameAsString();
												const std::string as_an_fparam_str = g_mse_namespace_str + "::rsv::as_an_fparam";
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
									+ "mse::rsv::TFParam<>. ";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
								}
							}
						} else if (qtype.getTypePtr()->isUnionType()) {
							const std::string error_desc = std::string("Native unions are not ")
								+ "supported. ";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
							}
						} else {
							struct CErrDef {
								std::string m_name_of_unsupported;
								std::string m_recommended_alternative;
							};
							auto err_defs = std::vector<CErrDef>{
								{"std::thread", "mse::thread or mse::xscope_thread"}
								, {"std::async", "mse::async or mse::xscope_asyc"}
								, {"std::basic_string_view", "a 'string section' from the SaferCPlusPlus library"}
								, {"std::span", "a 'random access section' from the SaferCPlusPlus library"}
								, {"std::array", "a corresponding substitute from the SaferCPlusPlus library"}
								, {"std::vector", "a corresponding substitute from the SaferCPlusPlus library"}
								, {"std::basic_string", "a corresponding substitute from the SaferCPlusPlus library"}
								, {"std::__cxx11::basic_string", "a corresponding substitute from the SaferCPlusPlus library"}
								, {"std::shared_ptr", "a reference counting pointer or an 'access requester' from the SaferCPlusPlus library"}
								, {"std::unique_ptr", "mse::TXScopeOwnerPointer<> or a reference counting pointer from the SaferCPlusPlus library"}
								};
							for (const auto& err_def : err_defs) {
								if (name == err_def.m_name_of_unsupported) {
									std::string error_desc = std::string("'") + name + std::string("' is not ")
										+ "supported. ";
									if ("" != err_def.m_recommended_alternative) {
										error_desc += "Consider using " + err_def.m_recommended_alternative + " instead.";
									}
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
									}
									break;
								}
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
								+ "supported. ";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
							}
						}
					}
				} else {
					auto NAD = dyn_cast<const NamespaceAliasDecl>(D);
					if (NAD) {
						const auto ND = NAD->getNamespace();
						if (ND) {
							const auto source_namespace_str = ND->getQualifiedNameAsString();

							const std::string mse_namespace_str1 = g_mse_namespace_str;
							const std::string mse_namespace_str2 = g_mse_namespace_str + std::string("::");
							if ((source_namespace_str == mse_namespace_str1)
								|| string_begins_with(source_namespace_str, mse_namespace_str2)) {

								/* This check might be a bit of a hack. The idea is that we want to
								prevent the subversion of checks for use of elements in the mse::us
								namespace by using an alias to the namespace.
								*/

								const std::string error_desc = std::string("This namespace alias could ")
									+ "be used to subvert some of the checks. "
									+ "So its use requires a 'check suppression' directive.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DSL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto DRESR = nice_source_range(DRE->getSourceRange(), Rewrite);
				SourceLocation DRESL = DRESR.getBegin();
				SourceLocation DRESLE = DRESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FDRESL = ASTC->getFullLoc(DRESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = DRESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, DRESL)) {
					return void();
				}

				auto DREISR = instantiation_source_range(DRE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(DREISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (DRESL.isValid() && DRESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(DRESL, DRESLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}
				{
					auto D = DRE->getDecl();

					if (D->getType() != DRE->getType()) {
						auto D_qtype_str = D->getType().getAsString();
						auto DRE_qtype_str = DRE->getType().getAsString();
						if (D->getType()->isReferenceType() == DRE->getType()->isReferenceType()) {
							/* Does this ever happen? */
							assert(false);
						}
					}

					auto DD = dyn_cast<const DeclaratorDecl>(D);
					if (DD) {
						auto qtype = DD->getType();
						std::string qtype_str = DD->getType().getAsString();
						const auto qualified_name = DD->getQualifiedNameAsString();
						const std::string mse_us_namespace_str1 = g_mse_namespace_str + "::us::";
						if (string_begins_with(qualified_name, mse_us_namespace_str1)) {

							const std::string mse_us_namespace_str2 = std::string("::") + g_mse_namespace_str + "::us::";
							if (string_begins_with(source_text, mse_us_namespace_str1)
								|| string_begins_with(source_text, mse_us_namespace_str2)) {

								/* We can't just flag all instantiations of elements in the 'mse::us' namespace because
								they are used by some of the safe library elements. We just want to flag cases where they
								are explicitly instantiated by the programmer. For now we'll just check that it's
								explicitly expressed in the source text. This wouldn't catch aliases of elements, so the
								declaration/definition of the offending aliases (including "using namespace") will need
								to be screened for and flagged as well. */


								const std::string error_desc = std::string("Elements in the 'mse::us' namespace (like '"
									+ qualified_name + "') are potentially unsafe. ")
									+ "Their use requires a 'check suppression' directive.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, DRESL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto STSR = nice_source_range(ST->getSourceRange(), Rewrite);
				SourceLocation STSL = STSR.getBegin();
				SourceLocation STSLE = STSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSTSL = ASTC->getFullLoc(STSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = STSL.printToString(*MR.SourceManager);

				if (std::string::npos != source_location_str.find("1proj")) {
					int q = 5;
				}

				if (filtered_out_by_location(MR, STSL)) {
					return void();
				}

				auto STISR = instantiation_source_range(ST->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(STISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (STSL.isValid() && STSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(STSL, STSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}
				if (ST->getRetValue()) {
					if (is_xscope_type(ST->getRetValue()->getType(), (*this).m_state1)) {
						bool xscope_return_value_wrapper_present = false;
						const clang::Stmt* stii = ST->getRetValue()->IgnoreImplicit()->IgnoreParenImpCasts();
						while (!dyn_cast<const CallExpr>(stii)) {
							//stii->dump();
							auto* CXXCE = dyn_cast<const CXXConstructExpr>(stii);
							if (CXXCE && (1 <= CXXCE->getNumArgs()) && (CXXCE->getArg(0))) {
								stii = CXXCE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
								continue;
							}
							break;
						}
						auto* CE = dyn_cast<const CallExpr>(stii);
						if (CE) {
							auto function_decl = CE->getDirectCallee();
							auto num_args = CE->getNumArgs();
							if (function_decl) {
								std::string function_name = function_decl->getNameAsString();
								std::string qualified_function_name = function_decl->getQualifiedNameAsString();
								const std::string return_value_str = g_mse_namespace_str + "::return_value";
								if (return_value_str == qualified_function_name) {
									xscope_return_value_wrapper_present = true;
								}
							}
						}
						if (!xscope_return_value_wrapper_present) {
							const std::string error_desc = std::string("Return values of xscope type ")
							+ "need to be wrapped in the mse::return_value() function wrapper.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, STSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
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

	class MCSSSRecordDecl2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSRecordDecl2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const RecordDecl* RD = MR.Nodes.getNodeAs<clang::RecordDecl>("mcsssrecorddecl");

			if ((RD != nullptr))
			{
				auto SR = nice_source_range(RD->getSourceRange(), Rewrite);
				auto decl_source_range = SR;
				SourceLocation SL = SR.getBegin();
				SourceLocation SLE = SR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSL = ASTC->getFullLoc(SL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = SL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, SL)) {
					return void();
				}

				auto RDISR = instantiation_source_range(RD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(RDISR);
				if (supress_check_flag) {
					return;
				}

				if (std::string::npos != source_location_str.find("871")) {
					int q = 5;
				}

				std::string source_text;
				if (SR.isValid()) {
					source_text = Rewrite.getRewrittenText(SR);
				} else {
					return;
				}

				std::string name = RD->getNameAsString();

				auto qualified_name = RD->getQualifiedNameAsString();
				const std::string mse_namespace_str1 = g_mse_namespace_str + "::";
				if (string_begins_with(qualified_name, mse_namespace_str1)) {
					int q = 5;
					//return;
				}

				const auto record_name = RD->getName();
				RD->getTypeForDecl();
				auto CXXRD = RD->getTypeForDecl()->getAsCXXRecordDecl();
				if (RD->isThisDeclarationADefinition()) {
					const std::string xscope_tag_str = g_mse_namespace_str + "::us::impl::XScopeTagBase";
					const std::string ContainsNonOwningScopeReference_tag_str = g_mse_namespace_str + "::us::impl::ContainsNonOwningScopeReferenceTagBase";
					const std::string ReferenceableByScopePointer_tag_str = g_mse_namespace_str + "::us::impl::ReferenceableByScopePointerTagBase";

					bool has_xscope_tag_base = false;
					bool has_ContainsNonOwningScopeReference_tag_base = false;
					bool has_ReferenceableByScopePointer_tag_base = false;

					if (CXXRD) {
						if (is_xscope_type(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_xscope_tag_base = true;
						}
						if (has_ancestor_base_class(*(CXXRD->getTypeForDecl()), ContainsNonOwningScopeReference_tag_str)) {
							has_ContainsNonOwningScopeReference_tag_base = true;
						}
						if (has_ancestor_base_class(*(CXXRD->getTypeForDecl()), ReferenceableByScopePointer_tag_str)) {
							has_ReferenceableByScopePointer_tag_base = true;
						}
					}

					for (const auto& field : RD->fields()) {
						const auto field_qtype = field->getType();
						auto field_qtype_str = field_qtype.getAsString();

						std::string error_desc;
						if (field_qtype.getTypePtr()->isPointerType()) {
							if (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled()) {
								if (has_xscope_tag_base) {
									error_desc = std::string("Native pointers are not (yet) supported as fields of xscope ")
										+ "structs or classes.";
								} else {
									error_desc = std::string("Native pointers are not supported as fields of (non-xscope) ")
										+ "structs or classes.";
								}
							}
						} else if (field_qtype.getTypePtr()->isReferenceType()) {
							if (has_xscope_tag_base) {
								error_desc = std::string("Native references are not (yet) supported as fields of xscope ")
									+ "structs or classes.";
							} else {
								error_desc = std::string("Native references are not supported as fields of (non-xscope) ")
									+ "structs or classes.";
							}
						}

						if ((!has_xscope_tag_base) && is_xscope_type(field_qtype, (*this).m_state1)) {
							error_desc = std::string("Structs or classes containing fields of xscope type (like '")
								+ field_qtype_str + "') must inherit from mse::rsv::XScopeTagBase.";
						}
						if ((!has_ContainsNonOwningScopeReference_tag_base) && (
								has_ancestor_base_class(field_qtype, ContainsNonOwningScopeReference_tag_str)
								|| (field_qtype->isPointerType()) || (field_qtype->isReferenceType())
							)) {
							error_desc = std::string("Structs or classes containing fields that are, or contain, ")
								+ "non-owning scope references (like '" + field_qtype_str + "') must inherit from "
								+ "mse::rsv::ContainsNonOwningScopeReferenceTagBase.";
						}
						if ((!has_ReferenceableByScopePointer_tag_base)
							&& has_ancestor_base_class(field_qtype, ReferenceableByScopePointer_tag_str)) {
							error_desc = std::string("Structs or classes containing fields (like '") + field_qtype_str
								+ "') that yield scope pointers (from their overloaded 'operator&'), or contain an element "
								+ "that does, must inherit from mse::rsv::ReferenceableByScopePointerTagBase.";
						}
						if ("" != error_desc) {
							auto FDISR = instantiation_source_range(field->getSourceRange(), Rewrite);
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, FDISR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
							}
						}
					}
				}
			}
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
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CESL)) {
					return void();
				}

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string as_an_fparam_str = g_mse_namespace_str + "::rsv::as_an_fparam";
					const std::string as_a_returnable_fparam_str = g_mse_namespace_str + "::rsv::as_a_returnable_fparam";
					if ((as_an_fparam_str == qualified_function_name) || (as_a_returnable_fparam_str == qualified_function_name)) {
						if (1 == num_args) {
							auto EX1 = CE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
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
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CESL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
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

	const clang::Expr* containing_object_expr_from_member_expr(const clang::MemberExpr* ME) {
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

	const clang::DeclRefExpr* declrefexpr_of_member_expr_if_any(const clang::MemberExpr* ME) {
		const clang::DeclRefExpr* retval = nullptr;

		auto EX = containing_object_expr_from_member_expr(ME);
		if (EX) {
			auto EX_ii = EX->IgnoreImplicit()->IgnoreParenCasts();
			auto DRE = dyn_cast<const clang::DeclRefExpr>(EX_ii);
			if (DRE) {
				return DRE;
			} else {
				auto ME2 = dyn_cast<const clang::MemberExpr>(EX);
				if (ME2) {
					/* nested member */
					return declrefexpr_of_member_expr_if_any(ME2);
				}
			}
		}
		return retval;
	}

	const clang::DeclRefExpr* declrefexpr_of_expr_if_any(const clang::Expr* EX1, ASTContext& Ctx) {
		if (!EX1) {
			return nullptr;
		}
		const auto EX = EX1->IgnoreImplicit()->IgnoreParenImpCasts();
		bool satisfies_checks = false;
		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		if (!DRE1) {
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME) {
				if (!(ME->isBoundMemberFunction(Ctx))) {
					DRE1 = declrefexpr_of_member_expr_if_any(ME);
				} else {
					int q = 5;
				}
			}
		}

		return DRE1;
	}

	const clang::Type* remove_fparam_wrappers(const clang::QualType& qtype);

	auto remove_fparam_wrappers(const clang::Type& type) {
		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			//const std::string treturnablefparam_str = g_mse_namespace_str + "::rsv::TReturnableFParam";
			//const std::string tfparam_str = g_mse_namespace_str + "::rsv::TFParam";
			//const std::string txsifcfparam_str = g_mse_namespace_str + "::rsv::TXScopeItemFixedConstPointerFParam";
			//const std::string txsiffparam_str = g_mse_namespace_str + "::rsv::TXScopeItemFixedPointerFParam";

			const std::string prefix_str = g_mse_namespace_str + "::rsv::";
			const std::string suffix_str = "FParam";
			if (!(string_begins_with(qname, prefix_str) || string_ends_with(qname, suffix_str))) {
				return TP;
			}
			for (const auto& base : CXXRD->bases()) {
				const auto base_qtype = base.getType();
				const auto base_qtype_str = base_qtype.getAsString();
				/* The first base class should be the one we're interested in. */
				return remove_fparam_wrappers(base_qtype);
			}
			/*unexpected*/
			int q = 5;
		}
		return TP;
	}
	const clang::Type* remove_fparam_wrappers(const clang::QualType& qtype) {
		return remove_fparam_wrappers(*(qtype.getTypePtr()));
	}

	bool can_be_safely_targeted_with_an_xscope_reference(const clang::Expr* EX1, ASTContext& Ctx) {
		if (!EX1) {
			//assert(false);
			return false;
		}
		const auto EX = EX1->IgnoreImplicit()->IgnoreParenImpCasts();
		const auto EX_qtype = EX->getType();
		const auto EX_qtype_str = EX_qtype.getAsString();

		bool satisfies_checks = false;
		auto DRE1 = declrefexpr_of_expr_if_any(EX, Ctx);
		if (DRE1) {
			const auto DRE1_qtype = DRE1->getType();
			const auto DRE1_qtype_str = DRE1_qtype.getAsString();

			auto D1 = DRE1->getDecl();
			const auto D1_qtype = D1->getType();
			const auto D1_qtype_str = D1_qtype.getAsString();

			auto VD = dyn_cast<const clang::VarDecl>(D1);
			if (VD) {
				const auto VD_qtype = VD->getType();
				const auto VD_qtype_str = VD_qtype.getAsString();

				const auto storage_duration = VD->getStorageDuration();
				if ((clang::StorageDuration::SD_Automatic == storage_duration)
					|| (clang::StorageDuration::SD_Thread == storage_duration)
					) {
					if (VD->getType()->isReferenceType()) {
						/* We're assuming that imposed restrictions imply that objects targeted
						by native references qualify as scope objects (at least for the duration
						of the reference). */
						int q = 5;
					}
					satisfies_checks = true;
				}
			}
		} else {
			auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(EX);
			if (CXXOCE) {
				static const std::string operator_star_str = "operator*";
				static const std::string operator_arrow_str = "operator->";
				auto operator_name = CXXOCE->getDirectCallee()->getNameAsString();
				if (((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
					auto arg_EX = CXXOCE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
					if (arg_EX) {
						const auto arg_EX_qtype = arg_EX->getType();
						const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();

						const auto CXXRD = remove_fparam_wrappers(*(arg_EX->getType()))->getAsCXXRecordDecl();
						if (CXXRD) {
							const std::string xscope_item_f_ptr_str = g_mse_namespace_str + "::TXScopeItemFixedPointer";
							const std::string xscope_item_f_const_ptr_str = g_mse_namespace_str + "::TXScopeItemFixedConstPointer";
							const std::string xscope_f_ptr_str = g_mse_namespace_str + "::TXScopeFixedPointer";
							const std::string xscope_f_const_ptr_str = g_mse_namespace_str + "::TXScopeFixedConstPointer";
							const std::string xscope_owner_ptr_str = g_mse_namespace_str + "::TXScopeOwnerPointer";
							auto qname = CXXRD->getQualifiedNameAsString();
							if ((xscope_item_f_ptr_str == qname) || (xscope_item_f_const_ptr_str == qname)
								|| (xscope_f_ptr_str == qname) || (xscope_f_const_ptr_str == qname)
								/*|| ((xscope_owner_ptr_str == qname) && ())*/) {
								satisfies_checks = true;
							}
						} else if (arg_EX->getType()->isReferenceType()) {
							int q = 5;
						}
					}
				}
				int q = 5;
			} else {
				auto UO = dyn_cast<const clang::UnaryOperator>(EX);
				if (UO) {
					const auto opcode = UO->getOpcode();
					const auto opcode_str = UO->getOpcodeStr(opcode);
					if (clang::UnaryOperator::Opcode::UO_Deref == opcode) {
						const auto UOSE = UO->getSubExpr();
						if (UOSE) {
							const auto UOSE_qtype = UOSE->getType();
							const auto UOSE_qtype_str = UOSE_qtype.getAsString();

							if (UOSE->getType()->isPointerType()) {
								/* The declrefexpression is a direct dereference of a native pointer. */
								satisfies_checks = true;
							}
						}
					}
				} else {
					auto ME = dyn_cast<const clang::MemberExpr>(EX);
					if (ME) {
						for (const auto& child : ME->children()) {
							auto CXXTE = dyn_cast<const clang::CXXThisExpr>(child->IgnoreImplicit());
							if (CXXTE) {
								/* The expression is a (possibly implicit) dereference of a 'this' pointer.
								And this tool treats 'this' pointers, like all native pointers, as scope
								pointers. */
								satisfies_checks = true;
							} else {
								break;
							}
						}
					}
				}
			}
		}
		return satisfies_checks;
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
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CESL)) {
					return void();
				}

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string make_xscope_pointer_to_str = g_mse_namespace_str + "::rsv::make_xscope_pointer_to";
					const std::string make_xscope_const_pointer_to_str = g_mse_namespace_str + "::rsv::make_xscope_const_pointer_to";
					if ((make_xscope_pointer_to_str == qualified_function_name) || (make_xscope_const_pointer_to_str == qualified_function_name)) {
						if (1 == num_args) {
							auto EX1 = CE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
							bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX1, *ASTC);
							if (!satisfies_checks) {
								const std::string error_desc = std::string("Cannot verify that mse::rsv::make_xscope_pointer_to() or ")
									+ "mse::rsv::make_xscope_const_pointer_to() is safe here.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CESL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto VDSR = nice_source_range(VD->getSourceRange(), Rewrite);
				SourceLocation VDSL = VDSR.getBegin();
				SourceLocation VDSLE = VDSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FVDSL = ASTC->getFullLoc(VDSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = VDSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, VDSL)) {
					return void();
				}

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (VDSL.isValid() && VDSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(VDSL, VDSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}
				if (std::string::npos != source_text.find("ref1")) {
					int q = 5;
				}

				auto qtype = VD->getType();
				std::string qtype_str = VD->getType().getAsString();
				if (qtype->isReferenceType()) {
					if (clang::StorageDuration::SD_Automatic != VD->getStorageDuration()) {
						const std::string error_desc = std::string("Native references that are ")
							+ "not local variables (or function parameters) are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
						}
					}
					auto* EX = VD->getInit();
					if (EX) {
						bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX, *ASTC);
						if (!satisfies_checks) {
							const std::string error_desc = std::string("Cannot verify that the ")
								+ "native reference (" + qtype_str + ") is safe here.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
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

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CESL)) {
					return void();
				}

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				const auto num_args = CE->getNumArgs();
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string mse_namespace_str = g_mse_namespace_str + "::";
					const std::string std_namespace_str = "std::";
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
					for (; arg_index < num_args; arg_index++, param_iter++) {
						if (function_decl->param_end() == param_iter) {
							break;
						}
						const auto qtype = (*param_iter)->getType();
						const std::string qtype_str = (*param_iter)->getType().getAsString();
						if (qtype->isReferenceType()) {
							auto EX = CE->getArg(arg_index);
							bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX, *ASTC);
							if (!satisfies_checks) {
								auto const * const MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(EX);
								if (MTE && (!(MTE->getType()->isReferenceType()))) {
									/* This argument is a temporary (non-reference) (that should outlive
									the function parameter). */
									satisfies_checks = true;
								}
							}
							if (!satisfies_checks) {
								auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
								SourceLocation EXSL = EXSR.getBegin();

								const std::string error_desc = std::string("Cannot verify that the ")
									+ "argument passed to the parameter of native reference type ("
									+ qtype_str + ") is safe here.";
								auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, EXSL, error_desc));
								if (res.second) {
									std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
				SourceLocation EXSL = EXSR.getBegin();
				SourceLocation EXSLE = EXSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FEXSL = ASTC->getFullLoc(EXSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = EXSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, EXSL)) {
					return void();
				}

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (EXSL.isValid() && EXSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(EXSL, EXSLE));
				} else {
					return;
				}

				{
					const std::string error_desc = std::string("Pointer arithmetic (including ")
						+ "native array subscripts) is not supported.";
					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, EXSL, error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
				SourceLocation EXSL = EXSR.getBegin();
				SourceLocation EXSLE = EXSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FEXSL = ASTC->getFullLoc(EXSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = EXSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, EXSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find(":147:")) {
					int q = 5;
				}

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
					return;
				}
				if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled()) {
					return;
				}

				std::string source_text;
				if (EXSL.isValid() && EXSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(EXSL, EXSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				const clang::Expr* resulting_pointer_EX = MR.Nodes.getNodeAs<clang::Expr>("mcsssaddressof2");
				if (resulting_pointer_EX) {
					const auto qtype = resulting_pointer_EX->getType();
					const std::string qtype_str = resulting_pointer_EX->getType().getAsString();
					if ((resulting_pointer_EX->getType().getTypePtr()->isMemberPointerType())
						|| (resulting_pointer_EX->getType().getTypePtr()->isFunctionPointerType())) {
						return;
					}
				}
				{
					bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX, *ASTC);
					if (!satisfies_checks) {
						const std::string error_desc = std::string("Cannot verify that the return value ")
							+ "of the '&' operator or std::addressof() is safe here.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, EXSL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto VDSR = nice_source_range(VD->getSourceRange(), Rewrite);
				SourceLocation VDSL = VDSR.getBegin();
				SourceLocation VDSLE = VDSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FVDSL = ASTC->getFullLoc(VDSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = VDSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, VDSL)) {
					return void();
				}

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
					return;
				}
				if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled()) {
					return;
				}

				std::string source_text;
				if (VDSL.isValid() && VDSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(VDSL, VDSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				auto qtype = VD->getType();
				std::string qtype_str = VD->getType().getAsString();
				if (qtype->isPointerType()) {
					if (clang::StorageDuration::SD_Automatic != VD->getStorageDuration()) {
						const std::string error_desc = std::string("Native pointers that are ")
							+ "not (automatic) local variables (or function parameters) are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
						}
					}
					if (false && (!(qtype.isConstQualified()))) {
						const std::string error_desc = std::string("Retargetable (aka non-const) native pointers ")
							+ "are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
						}
					}
					const auto* EX = VD->getInit();
					if (EX) {
						const auto* EXii = EX->IgnoreImplicit()->IgnoreParenImpCasts();
						auto *CXXNPLE = dyn_cast<const CXXNullPtrLiteralExpr>(EXii);
						auto *GNE = dyn_cast<const GNUNullExpr>(EXii);
						bool null_initialization = false;
						if (CXXNPLE || GNE) {
							null_initialization = true;
						} else {
							auto *IL = dyn_cast<const IntegerLiteral>(EXii);
							if (IL) {
								const auto apint_val = IL->getValue();
								const auto u64_limited_val = apint_val.getLimitedValue();
								const auto limited_val = int(u64_limited_val);
								if (0 == limited_val) {
									null_initialization = true;
								} else {
									/* This should result in a compile error,
									so we don't issue a redundant error here. */
								}
							}
						}

						if (null_initialization) {
							const std::string error_desc = std::string("Null initialization of ")
								+ "native pointers is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
							}
						}
					} else {
						auto *PVD = dyn_cast<const ParmVarDecl>(VD);
						if (!PVD) {
							const std::string error_desc = std::string("Uninitialized ")
								+ "native pointer variables are not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, VDSL, error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n";
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
				auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
				SourceLocation EXSL = EXSR.getBegin();
				SourceLocation EXSLE = EXSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FEXSL = ASTC->getFullLoc(EXSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = EXSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, EXSL)) {
					return void();
				}

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
					return;
				}
				if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled()) {
					return;
				}

				std::string source_text;
				if (EXSL.isValid() && EXSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(EXSL, EXSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}

				std::string cast_type_str;
				const auto* EXii = EX->IgnoreImplicit()->IgnoreParenImpCasts();
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
										const std::string qtype_str = CXXFCE->getType().getAsString();

										const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
										if (CXXRD) {
											auto name = CXXRD->getQualifiedNameAsString();
											const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
											if (tmplt_CXXRD) {
												name = tmplt_CXXRD->getQualifiedNameAsString();
											}
											const std::string mse_rsv_tfparam_str1 = g_mse_namespace_str + "::rsv::TFParam";
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
					const std::string error_desc = cast_type_str
						+ " casts are not supported.";
					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, EXSL, error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n";
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CXXMemberCallExpr* CXXMCE = MR.Nodes.getNodeAs<clang::CXXMemberCallExpr>("mcsssmemberfunctioncall1");

			if ((CXXMCE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto CXXMCESR = nice_source_range(CXXMCE->getSourceRange(), Rewrite);
				SourceLocation CXXMCESL = CXXMCESR.getBegin();
				SourceLocation CXXMCESLE = CXXMCESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCXXMCESL = ASTC->getFullLoc(CXXMCESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CXXMCESL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, CXXMCESL)) {
					return void();
				}

				auto CXXMCEISR = instantiation_source_range(CXXMCE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CXXMCEISR);
				if (supress_check_flag) {
					return;
				}
				std::string instantiation_source_text;
				if (CXXMCEISR.isValid()) {
					instantiation_source_text = Rewrite.getRewrittenText(SourceRange(CXXMCESL, CXXMCESLE));
				}

				std::string source_text;
				if (CXXMCESL.isValid() && CXXMCESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CXXMCESL, CXXMCESLE));
				} else {
					return;
				}

				std::string qmethod_name;
				std::string method_name;
				auto method_decl = CXXMCE->getMethodDecl();
				if (method_decl) {
					qmethod_name = method_decl->getQualifiedNameAsString();
					method_name = method_decl->getNameAsString();

					auto method_declSR = nice_source_range(method_decl->getSourceRange(), Rewrite);
					SourceLocation method_declSL = method_declSR.getBegin();

					const std::string mse_ns_prefix = g_mse_namespace_str + std::string("::");
					static const std::string std_ns_prefix = "std::";
					if (string_begins_with(qmethod_name, mse_ns_prefix)
						|| string_begins_with(qmethod_name, std_ns_prefix)
						|| filtered_out_by_location(MR, method_declSL)) {
						/* The idea is to permit member function calls only if it's clear that the implicit 'this'
						pointer parameter will remain valid for the duration of the member function call. Generally
						we'll require that the implicit 'this' pointer argument be a scope pointer/reference, or
						equivalent.
						Here we waive this requirement if the member function is part of the SaferCPlusPlus library
						or the standard library. The dynamic SaferCPlusPlus containers have run-time safety
						mechanisms that ensure that the 'this' pointer target is not desroyed (i.e. its destructor 
						executed) during the call. In the future we may have an option/mode where the (run-time)
						safety mechanisms are disabled and the SaferCPlusPlus containers are not exempt.
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
				} else {
					int q = 5;
				}
				const auto EX = CXXMCE->getImplicitObjectArgument()->IgnoreImplicit()->IgnoreParenImpCasts();
				const auto num_args = CXXMCE->getNumArgs();
				if (EX) {
					const auto qtype = EX->getType();
					const auto qtype_str = qtype.getAsString();

					auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
					std::string EX_source_text;
					if (EXSR.isValid()) {
						EX_source_text = Rewrite.getRewrittenText(EXSR);
					}

					bool satisfies_checks = can_be_safely_targeted_with_an_xscope_reference(EX, *ASTC);
					if (!satisfies_checks) {
						const auto EX_iic = CXXMCE->getImplicitObjectArgument()->IgnoreImpCasts();

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
						const std::string error_desc = std::string("Cannot verify that the 'this' pointer ")
							+ "will remain valid for the duration of the member function call.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CXXMCESL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcssspointerassignment1");

			if ((BO != nullptr)/* && (DRE != nullptr)*/)
			{
				auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
				SourceLocation BOSL = BOSR.getBegin();
				SourceLocation BOSLE = BOSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FBOSL = ASTC->getFullLoc(BOSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = BOSL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, BOSL)) {
					return void();
				}

				auto BOISR = instantiation_source_range(BO->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(BOISR);
				if (supress_check_flag) {
					return;
				}

				std::string source_text;
				if (BOSL.isValid() && BOSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(BOSL, BOSLE));
				} else {
					return;
				}

				/* In our case, it's significantly harder to verify the safety of a pointer assignment than say,
				a value initialization of a pointer. With initialization, we only need to verify that the target
				object has scope lifetime, as that alone is sufficient to conclude that the target object
				outlives the pointer. Not so with pointer assignment. There we need to obtain an "upper bound"
				for the (scope) lifetime of the (lhs) pointer being modified and a lower bound for the (scope)
				lifetime of the new target object. */

				const auto LHSEX = BO->getLHS()->IgnoreImplicit()->IgnoreParenImpCasts();
				const auto RHSEX = BO->getRHS()->IgnoreImplicit()->IgnoreParenImpCasts();
				bool satisfies_checks = false;

				/* Currently we are only able to obtain the declaration (location) of the pointer to be
				modified (and therefore its scope lifetime) if the left hand side is a declref or member
				expression of the pointer. */
				auto LHSDRE1 = declrefexpr_of_expr_if_any(LHSEX, *(MR.Context));
				const VarDecl * LHSVD = nullptr;
				if (LHSDRE1) {
					auto D1 = LHSDRE1->getDecl();
					auto VD = dyn_cast<const clang::VarDecl>(D1);
					if (VD) {
						const auto storage_duration = VD->getStorageDuration();
						if (clang::StorageDuration::SD_Automatic == storage_duration) {
							LHSVD = VD;
						} else if (clang::StorageDuration::SD_Thread == storage_duration) {
							LHSVD = VD;
						}
					}
				}

				auto RHSDRE1 = declrefexpr_of_expr_if_any(RHSEX, *(MR.Context));
				const VarDecl * RHSVD = nullptr;
				if (RHSDRE1) {
					auto D1 = RHSDRE1->getDecl();
					auto VD = dyn_cast<const clang::VarDecl>(D1);
					if (VD) {
						/* The rhs is a pointer variable (or member of a variable). */
						const auto storage_duration = VD->getStorageDuration();
						if (clang::StorageDuration::SD_Automatic == storage_duration) {
							RHSVD = VD;
						} else if (clang::StorageDuration::SD_Thread == storage_duration) {
							RHSVD = VD;
						} else if (clang::StorageDuration::SD_Static == storage_duration) {
							const auto qtype = VD->getType();
							const auto qtype_str = qtype.getAsString();
							if (qtype.getTypePtr()->isPointerType()) {
								const auto pointee_qtype = qtype.getTypePtr()->getPointeeType();
								const auto pointee_qtype_str = pointee_qtype.getAsString();
								if (pointee_qtype.isConstQualified() && pointee_qtype.getTypePtr()->isArithmeticType()) {
									/* This case includes "C"-string literals. */
									RHSVD = VD;
								}
							}
						}
					}
				}
				if (LHSVD) {
					if (RHSVD) {
						satisfies_checks |= first_is_contained_in_scope_of_second(*LHSVD, *RHSVD, *(MR.Context));
					} else {
						/* The rhs was not a pointer variable (or member of a variable). */
						const clang::Expr* subexpr = nullptr;
						auto UO = dyn_cast<const clang::UnaryOperator>(RHSEX);
						if (UO) {
							const auto opcode = UO->getOpcode();
							const auto opcode_str = UO->getOpcodeStr(opcode);
							if (clang::UnaryOperator::Opcode::UO_AddrOf == opcode) {
								/* The rhs expression is essentially of the form "&(subexpr)" */
								subexpr = UO->getSubExpr()->IgnoreImplicit()->IgnoreParenImpCasts();
							}
						} else {
							auto CE = dyn_cast<const clang::CallExpr>(RHSEX);
							if (CE) {
								auto function_decl = CE->getDirectCallee();
								auto num_args = CE->getNumArgs();
								if (function_decl) {
									const std::string qualified_function_name = function_decl->getQualifiedNameAsString();
									static const std::string std_addressof_str = "std::addressof";
									if (std_addressof_str == qualified_function_name) {
										if (1 == num_args) {
											/* The rhs expression is essentially of the form "std::addressof(subexpr)" */
											subexpr = CE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
										}
									}
								}
							}
						}
						if (subexpr) {
							auto subexpr_DRE = declrefexpr_of_expr_if_any(subexpr, *(MR.Context));
							if (subexpr_DRE) {
								auto D1 = subexpr_DRE->getDecl();
								auto VD = dyn_cast<const clang::VarDecl>(D1);
								if (VD) {
									RHSVD = VD;
									satisfies_checks |= first_is_contained_in_scope_of_second(*LHSVD, *RHSVD, *(MR.Context));
								}
							} else {
								/* The target object from which the new pointer value is being derived is not expressed
								 directly as a variable or member of a variable. Perhaps it's being expressed as a
								 dereference of another (scope) pointer of some kind. Let's check: */
								const clang::Expr* subsubexpr = nullptr;
								auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(subexpr);
								if (CXXOCE) {
									static const std::string operator_star_str = "operator*";
									static const std::string operator_arrow_str = "operator->";
									auto operator_name = CXXOCE->getDirectCallee()->getNameAsString();
									if (((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
										auto arg_EX = CXXOCE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
										if (arg_EX) {
											const auto CXXRD = arg_EX->getType()->getAsCXXRecordDecl();
											if (CXXRD) {
												const std::string xscope_item_f_ptr_str = g_mse_namespace_str + "::TXScopeItemFixedPointer";
												const std::string xscope_item_f_const_ptr_str = g_mse_namespace_str + "::TXScopeItemFixedConstPointer";
												const std::string xscope_f_ptr_str = g_mse_namespace_str + "::TXScopeFixedPointer";
												const std::string xscope_f_const_ptr_str = g_mse_namespace_str + "::TXScopeFixedConstPointer";
												const std::string xscope_owner_ptr_str = g_mse_namespace_str + "::TXScopeOwnerPointer";
												auto qname = CXXRD->getQualifiedNameAsString();
												if ((xscope_item_f_ptr_str == qname) || (xscope_item_f_const_ptr_str == qname)
													|| (xscope_f_ptr_str == qname) || (xscope_f_const_ptr_str == qname)
													/*|| ((xscope_owner_ptr_str == qname) && ())*/) {
													subsubexpr = arg_EX;
												}
											} else if (arg_EX->getType()->isReferenceType()) {
												int q = 5;
											}
										}
									}
									int q = 5;
								} else {
									auto UO = dyn_cast<const clang::UnaryOperator>(subexpr);
									if (UO) {
										const auto opcode = UO->getOpcode();
										const auto opcode_str = UO->getOpcodeStr(opcode);
										if (clang::UnaryOperator::Opcode::UO_Deref == opcode) {
											const auto UOSE = UO->getSubExpr();
											if (UOSE) {
												if (UOSE->getType()->isPointerType()) {
													/* subexpr is a direct dereference of a native pointer. */
													subsubexpr = UOSE;
												}
											}
										}
									}
								}
								auto subsubexpr_DRE = declrefexpr_of_expr_if_any(subsubexpr, *(MR.Context));
								if (subsubexpr_DRE) {
									/* The rhs expression seems to be the address of a dereference of a scope pointer
									of some type. The (scope) lifetime of the scope pointer must be contained within
									the lifetime of the target object, so we can use it as a "lower bound" for the
									lifetime of the target object. */

									auto D1 = subsubexpr_DRE->getDecl();
									auto VD = dyn_cast<const clang::VarDecl>(D1);
									if (VD) {
										RHSVD = VD;
										satisfies_checks |= first_is_contained_in_scope_of_second(*LHSVD, *RHSVD, *(MR.Context));
									}
								}
							}
						} else {
							auto STRL = dyn_cast<const clang::StringLiteral>(RHSEX);
							if (STRL) {
								satisfies_checks = true;
							}
						}
					}
				} else {
					/* We were not able to obtain a declaration from the lhs, so we're not going to be able the verify anything. */
				}
				if (!satisfies_checks) {
					const std::string error_desc = std::string("Cannot verify that this pointer assignment ")
						+ "is safe.";
					auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, BOSL, error_desc));
					if (res.second) {
						std::cout << (*(res.first)).as_a_string1() << " \n";
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
		auto *ToDecl = Importer.Import(D);
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
						D->dump();

						auto SR = nice_source_range(D->getSourceRange(), localRewriter);
						if (!SR.isValid()) {
							continue;
						}
						if (filtered_out_by_location(localRewriter.getSourceMgr(), SR.getBegin())) {
							continue;
						}

						auto SL = SR.getBegin();
						std::string source_location_str = SL.printToString(localRewriter.getSourceMgr());

						if (std::string::npos != source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						} else {
							int q = 5;
						}

						auto *ND = dyn_cast<const NamedDecl>(D);
						if (!ND) {
							continue;
						}
						std::string name = ND->getNameAsString();

						// Don't re-import __va_list_tag, __builtin_va_list.
						//if (auto const * const ND = dyn_cast<NamedDecl>(D))
							if (IdentifierInfo *II = ND->getIdentifier())
								if (II->isStr("__va_list_tag") || II->isStr("__builtin_va_list") || II->isStr("main"))
									continue;

						if (nullptr == Importer.GetAlreadyImportedOrNull(D)) {
							auto FD = D->getAsFunction();
							if (FD) {
								std::string function_name = FD->getNameAsString();
							} else if (llvm::isa<clang::NamespaceDecl>(D)) {
								auto NSD = llvm::cast<clang::NamespaceDecl>(D);
								assert(NSD);
								auto NNS = clang::NestedNameSpecifier::Create(multi_tu_state_ptr->ast_units.at(I)->getASTContext(), nullptr, NSD);
								if (false && NNS) {
									auto *NNSToDecl = Importer.Import(NNS);
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
			HandlerForSSSSupressCheckDirectiveCall(R, tu_state()), HandlerForSSSSupressCheckDirectiveDeclField(R, tu_state()),
			HandlerForSSSStmtUtil(R, tu_state()), HandlerForSSSDeclUtil(R, tu_state()), HandlerForSSSDeclRefExprUtil(R, tu_state()),
			HandlerForSSSReturnStmt(R, tu_state()), HandlerForSSSRecordDecl2(R, tu_state()), HandlerForSSSAsAnFParam(R, tu_state()),
			HandlerForSSSMakeXScopePointerTo(R, tu_state()), HandlerForSSSNativeReferenceVar(R, tu_state()),
			HandlerForSSSArgToNativeReferenceParam(R, tu_state()), HandlerForSSSPointerArithmetic(R, tu_state()), HandlerForSSSAddressOf(R, tu_state()),
			HandlerForSSSNativePointerVar(R, tu_state()), HandlerForSSSCast(R, tu_state()), HandlerForSSSMemberFunctionCall(R, tu_state()),
			HandlerForSSSPointerAssignment(R, tu_state())
		{
			Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerMisc1);
			Matcher.addMatcher(callExpr(argumentCountIs(0)).bind("mcssssuppresscheckmemberdeclmcssssuppresscheckcall"), &HandlerForSSSSupressCheckDirectiveCall);
			Matcher.addMatcher(cxxMethodDecl(decl().bind("mcssssuppresscheckmemberdecl")), &HandlerForSSSSupressCheckDirectiveDeclField);
			Matcher.addMatcher(stmt().bind("mcsssstmtutil1"), &HandlerForSSSStmtUtil);
			Matcher.addMatcher(decl().bind("mcsssdeclutil1"), &HandlerForSSSDeclUtil);
			Matcher.addMatcher(declRefExpr().bind("mcsssdeclrefexprutil1"), &HandlerForSSSDeclRefExprUtil);
			Matcher.addMatcher(returnStmt().bind("mcsssreturnstmt"), &HandlerForSSSReturnStmt);
			Matcher.addMatcher(clang::ast_matchers::recordDecl().bind("mcsssrecorddecl"), &HandlerForSSSRecordDecl2);
			Matcher.addMatcher(callExpr(/*argumentCountIs(1)*/).bind("mcsssasanfparam1"), &HandlerForSSSAsAnFParam);
			Matcher.addMatcher(callExpr(/*argumentCountIs(1)*/).bind("mcsssmakexscopepointerto1"), &HandlerForSSSMakeXScopePointerTo);
			Matcher.addMatcher(varDecl().bind("mcsssnativereferencevar1"), &HandlerForSSSNativeReferenceVar);
			Matcher.addMatcher(callExpr().bind("mcsssargtonativereferenceparam1"), &HandlerForSSSArgToNativeReferenceParam);
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
				hasType(pointerType())
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
			Matcher.addMatcher(expr(allOf(
				ignoringImplicit(ignoringParenImpCasts(expr(
						binaryOperator(hasOperatorName("="))
					).bind("mcssspointerassignment1"))),
				hasType(pointerType())
				)).bind("mcssspointerassignment3"), &HandlerForSSSPointerAssignment);
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
		MCSSSSupressCheckDirectiveCall HandlerForSSSSupressCheckDirectiveCall;
		MCSSSSupressCheckDirectiveDeclField HandlerForSSSSupressCheckDirectiveDeclField;
		MCSSSStmtUtil HandlerForSSSStmtUtil;
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
		const FileEntry *file,
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

				std::string file_name_str = file_name;
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
			std::string full_path_name = m_Rewriter_ref.getSourceMgr().getBufferName(Loc, &filename_is_invalid);

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
						std::map<SourceLocation, std::shared_ptr<CFirstIncludeInfo>>::value_type item(Loc, std::make_shared<CFirstIncludeInfo>(Loc));
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

		std::map<SourceLocation, std::shared_ptr<CFirstIncludeInfo>> m_first_include_info_map;
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

	class MyFrontendAction : public ASTFrontendActionCompatibilityWrapper1
	{
	public:
		MyFrontendAction() {
			int q = 5;
		}
		~MyFrontendAction() {
			std::cout << "\n" << (*this).m_tu_state.m_error_records.size() << " errors found. \n";
			//llvm::errs() << "\n~MyFrontendAction() " << '\n';
			if (false && ConvertToSCPP) {
				auto res = overwriteChangedFiles();
				int q = 5;
			}
		}

		bool BeginSourceFileAction(CompilerInstance &ci) override {
			s_source_file_action_num += 1;
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
						std::string full_path_name = TheRewriter.getSourceMgr().getBufferName(fii_ref.m_beginning_of_file_loc, &filename_is_invalid);

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
			return llvm::make_unique<MyASTConsumer>(TheRewriter, CI, (*this).m_tu_state);
		}

		bool overwriteChangedFiles() {
			std::set<std::pair<std::string, std::string> > filename_info_set;
			{
				for (auto  I = TheRewriter.buffer_begin(), E = TheRewriter.buffer_end(); I != E; ++I) {
					const FileEntry *Entry = TheRewriter.getSourceMgr().getFileEntryForID(I->first);

					std::string pathname = Entry->tryGetRealPathName();
					auto found_pos = pathname.find_last_of("/\\");
					std::string path;
					std::string filename;
					if (std::string::npos != found_pos) {
						path = pathname.substr(0, found_pos);
						if (pathname.length() > found_pos) {
							filename = pathname.substr(found_pos + 1);
						}
					} else {
						filename = pathname;
					}
					std::pair<std::string, std::string> item(path, filename);
					filename_info_set.insert(item);

					int q = 5;
				}
				std::set<std::pair<std::string, std::string> > unbackupable_filename_info_set;
				for (const auto& filename_info_ref : filename_info_set) {
					std::string backup_pathname = filename_info_ref.first + "/" + filename_info_ref.second + ".bak.tmp";
					std::string tmp_pathname = filename_info_ref.first + "/" + filename_info_ref.second + ".tmp";
					std::string src_pathname = filename_info_ref.first + "/" + filename_info_ref.second;

					std::ifstream src;
					src.open(src_pathname, std::ios::binary);
					std::ofstream dst;
					dst.open(tmp_pathname, std::ios::binary);
					if (((src.rdstate() & std::ifstream::failbit ) != 0)
							|| ((dst.rdstate() & std::ofstream::failbit ) != 0)) {
						unbackupable_filename_info_set.insert(filename_info_ref);
					} else {
						dst << src.rdbuf();
						src.close();
						dst.close();
						std::remove(backup_pathname.c_str());
						std::rename(src_pathname.c_str(), backup_pathname.c_str());
						std::rename(tmp_pathname.c_str(), src_pathname.c_str());
					}
				}
				for (const auto& filename_info_ref : unbackupable_filename_info_set) {
					filename_info_set.erase(filename_info_ref);
				}
			}

			bool retval = TheRewriter.overwriteChangedFiles();

			{
				for (const auto& filename_info_ref : filename_info_set) {
					{
						std::string converted_version_filename = filename_info_ref.second;

						static const std::string dot_c_str = ".c";
						bool ends_with_dot_c = string_ends_with(converted_version_filename, dot_c_str);
						if (ends_with_dot_c) {
							converted_version_filename = converted_version_filename.substr(0, converted_version_filename.size() - 2);
							converted_version_filename += ".cpp";
						}

						{
							auto found_it = s_file_conversion_record_map.find(filename_info_ref.second);
							if (s_file_conversion_record_map.end() == found_it) {
								CFileConversionRecord file_conversion_record;
							file_conversion_record.m_path = filename_info_ref.first;
							file_conversion_record.m_original_filename = filename_info_ref.second;
								file_conversion_record.m_target_filename = converted_version_filename;
								std::map<std::string, CFileConversionRecord>::value_type item(filename_info_ref.second, file_conversion_record);
								found_it = s_file_conversion_record_map.insert(item).first;
								assert(s_file_conversion_record_map.end() != found_it);
							}
							CFileConversionRecord& file_conversion_record_ref = (*found_it).second;
							file_conversion_record_ref.m_converted_version_tu_numbers.push_back(s_source_file_action_num);
						}

						converted_version_filename += ".converted_" + std::to_string(s_source_file_action_num);

						std::string converted_version_pathname = filename_info_ref.first + "/" + converted_version_filename;
						std::string src_pathname = filename_info_ref.first + "/" + filename_info_ref.second;
						std::remove(converted_version_pathname.c_str());
						std::rename(src_pathname.c_str(), converted_version_pathname.c_str());
					}
					{
						std::string backup_filename = filename_info_ref.second + ".bak.tmp";
						std::string backup_pathname = filename_info_ref.first + "/" + backup_filename;
						std::string dst_pathname = filename_info_ref.first + "/" + filename_info_ref.second;
						//std::remove(dst_pathname.c_str());
						std::rename(backup_pathname.c_str(), dst_pathname.c_str());
					}
				}
			}

			return retval;
		}

		static std::map<std::string, CFileConversionRecord> s_file_conversion_record_map;

	private:
		CTUState m_tu_state;
		Rewriter TheRewriter;
		std::vector<PPCallbacks*> m_callbacks_stack;
		static int s_source_file_action_num;
	};
	int MyFrontendAction::s_source_file_action_num = 0;
	std::map<std::string, CFileConversionRecord> MyFrontendAction::s_file_conversion_record_map;

	auto buildASTs_and_run(ClangTool& Tool, Options options = Options()) {

        CheckSystemHeader = options.CheckSystemHeader;
        MainFileOnly = options.MainFileOnly;
        ConvertToSCPP = options.ConvertToSCPP;
        CTUAnalysis = options.CTUAnalysis;
        EnableNamespaceImport = options.EnableNamespaceImport;
        SuppressPrompts = options.SuppressPrompts;
        DoNotReplaceOriginalSource = options.DoNotReplaceOriginalSource;
        MergeCommand = options.MergeCommand;

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
