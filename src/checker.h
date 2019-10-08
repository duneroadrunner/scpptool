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
	};

	class MCSSSSupressCheckDirectiveCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSupressCheckDirectiveCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcssssuppresscheckmemberdeclmcssssuppresscheckcall");
			//const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");
			//const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfree3");

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

	bool is_xscope_type(const clang::QualType qtype, const CTUState& tu_state_cref);

	bool is_xscope_type(const clang::Type& type, const CTUState& tu_state_cref) {
		bool retval = false;

		const auto TP = &type;
		const auto CXXRD = TP->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			const std::string xscope_tag_str = g_mse_namespace_str + "::us::impl::XScopeTagBase";
			if (xscope_tag_str == qname) {
				return true;
			}
			for (const auto& base : CXXRD->bases()) {
				const auto base_qtype = base.getType();
				auto base_qtype_str = base_qtype.getAsString();

				if (is_xscope_type(base.getType(), tu_state_cref)) {
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
	bool is_xscope_type(const clang::QualType qtype, const CTUState& tu_state_cref) {
		bool retval = false;

		std::string qtype_str = qtype.getAsString();
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_xscope_type(*TP, tu_state_cref);
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
			//const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcssssuppresscheckmemberdeclmcssssuppresscheckcall");
			//const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");
			//const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfree3");

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
				{
					auto VD = dyn_cast<const clang::VarDecl>(D);
					if (VD) {
						const auto storage_duration = VD->getStorageDuration();
						auto qtype = VD->getType();
						std::string qtype_str = VD->getType().getAsString();
						const auto qualified_name = VD->getQualifiedNameAsString();

						if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
							static const std::string class_space_str = "class ";
							const std::string mse_rsv_static_immutable_obj_str1 = class_space_str + g_mse_namespace_str + "::rsv::TStaticImmutableObj<";
							const std::string mse_scope_atomic_obj_str = class_space_str + g_mse_namespace_str + "::TXScopeAtomicObj<";
							const std::string mse_AsyncSharedV2ReadWriteAccessRequester_str = class_space_str + g_mse_namespace_str + "::TAsyncSharedV2ReadWriteAccessRequester<";
							const std::string mse_AsyncSharedV2ReadOnlyAccessRequester_str = class_space_str + g_mse_namespace_str + "::TAsyncSharedV2ReadOnlyAccessRequester<";
							const std::string mse_TAsyncSharedV2ImmutableFixedPointer_str = class_space_str + g_mse_namespace_str + "::TAsyncSharedV2ImmutableFixedPointer<";
							const std::string mse_TAsyncSharedV2AtomicFixedPointer_str = class_space_str + g_mse_namespace_str + "::TAsyncSharedV2AtomicFixedPointer<";
							if (clang::StorageDuration::SD_Static == storage_duration) {
								bool satisfies_checks = false;
								if ((0 == qtype_str.compare(0, mse_rsv_static_immutable_obj_str1.size(), mse_rsv_static_immutable_obj_str1))
									|| (0 == qtype_str.compare(0, mse_scope_atomic_obj_str.size(), mse_scope_atomic_obj_str))
									|| (0 == qtype_str.compare(0, mse_AsyncSharedV2ReadWriteAccessRequester_str.size(), mse_AsyncSharedV2ReadWriteAccessRequester_str))
									|| (0 == qtype_str.compare(0, mse_AsyncSharedV2ReadOnlyAccessRequester_str.size(), mse_AsyncSharedV2ReadOnlyAccessRequester_str))
									|| (0 == qtype_str.compare(0, mse_TAsyncSharedV2ImmutableFixedPointer_str.size(), mse_TAsyncSharedV2ImmutableFixedPointer_str))
									|| (0 == qtype_str.compare(0, mse_TAsyncSharedV2AtomicFixedPointer_str.size(), mse_TAsyncSharedV2AtomicFixedPointer_str))
									) {
									satisfies_checks = true;
								}
								if (!satisfies_checks) {
									const std::string error_desc = std::string("'static storage duration' is not ")
										+ "supported for this type. Eligible types wrapped in the 'mse::rsv::TStaticImmutableObj<>' "
										+ "transparent template wrapper would be supported. Other supported wrappers include: "
										+ "mse::TXScopeAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
										+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
									auto res = (*this).m_state1.m_error_records.emplace(
										CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
									}
								}
							} else if (clang::StorageDuration::SD_Thread == storage_duration) {
								const std::string mse_rsv_thread_local_obj_str1 = class_space_str + g_mse_namespace_str + "::rsv::TThreadLocalObj<";
								bool satisfies_checks = false;
								if ((0 == qtype_str.compare(0, mse_rsv_static_immutable_obj_str1.size(), mse_rsv_static_immutable_obj_str1))
									|| (0 == qtype_str.compare(0, mse_scope_atomic_obj_str.size(), mse_scope_atomic_obj_str))
									|| (0 == qtype_str.compare(0, mse_AsyncSharedV2ReadWriteAccessRequester_str.size(), mse_AsyncSharedV2ReadWriteAccessRequester_str))
									|| (0 == qtype_str.compare(0, mse_AsyncSharedV2ReadOnlyAccessRequester_str.size(), mse_AsyncSharedV2ReadOnlyAccessRequester_str))
									|| (0 == qtype_str.compare(0, mse_TAsyncSharedV2ImmutableFixedPointer_str.size(), mse_TAsyncSharedV2ImmutableFixedPointer_str))
									|| (0 == qtype_str.compare(0, mse_TAsyncSharedV2AtomicFixedPointer_str.size(), mse_TAsyncSharedV2AtomicFixedPointer_str))
									|| (0 == qtype_str.compare(0, mse_rsv_thread_local_obj_str1.size(), mse_rsv_thread_local_obj_str1))
									) {
									satisfies_checks = true;
								}
								if (!satisfies_checks) {
									const std::string error_desc = std::string("'thread local storage duration' is not ")
										+ "supported for this type. Eligible types wrapped in the 'mse::rsv::TThreadLocalObj<>' "
										+ "transparent template wrapper would be supported. Other supported wrappers include: "
										+ "mse::rsv::TStaticImmutableObj<>, mse::TXScopeAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
										+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
									auto res = (*this).m_state1.m_error_records.emplace(
										CErrorRecord(*MR.SourceManager, DSL, error_desc));
									if (res.second) {
										std::cout << (*(res.first)).as_a_string1() << " \n";
									}
								}
							}
						}
					}

					auto DD = dyn_cast<const DeclaratorDecl>(D);
					if (DD) {
						auto qtype = DD->getType();
						std::string qtype_str = DD->getType().getAsString();
						if (is_xscope_type(qtype, (*this).m_state1)) {
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
					auto DD = dyn_cast<const DeclaratorDecl>(D);
					if (DD) {
						auto qtype = DD->getType();
						std::string qtype_str = DD->getType().getAsString();
						const auto qualified_name = DD->getQualifiedNameAsString();
						const std::string mse_us_namespace_str1 = g_mse_namespace_str + "::us::";
						const std::string mse_us_namespace_str2 = std::string("::") + g_mse_namespace_str + "::us::";
						if ((0 == qualified_name.compare(0, mse_us_namespace_str1.size(), mse_us_namespace_str1))
							|| (0 == qualified_name.compare(0, mse_us_namespace_str2.size(), mse_us_namespace_str2))) {

							if ((0 == source_text.compare(0, mse_us_namespace_str1.size(), mse_us_namespace_str1))
								|| (0 == source_text.compare(0, mse_us_namespace_str2.size(), mse_us_namespace_str2))) {

								/* We can't just flag all instantiations of elements in the 'mse::us' namespace because
								they are used by some of the safe library elements. We just want to flag cases where they
								are explicitly instantiated by the programmer. For now we'll just check that it's
								explicitly expressed in the source text. This wouldn't catch aliases of elements, so the
								declaration/definition of the offending aliases (including "using namespace") will need
								to be screened for and flagged as well. */

								const std::string error_desc = std::string("Elements in the 'mse::us' namespace (like '"
									+ qualified_name + "') are potentially unsafe. ")
									+ "Their use requires a 'check suppression' directive.";
								auto res = (*this).m_state1.m_error_records.emplace(
									CErrorRecord(*MR.SourceManager, DRESL, error_desc));
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
							auto res = (*this).m_state1.m_error_records.emplace(
								CErrorRecord(*MR.SourceManager, STSL, error_desc));
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
				const std::string mse_namespace_str2 = std::string("::") + g_mse_namespace_str + "::";
				if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
						|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
					int q = 5;
					//return;
				}

				if (RD->isThisDeclarationADefinition()) {
					auto CXXRD = RD->getTypeForDecl()->getAsCXXRecordDecl();
					if (CXXRD && is_xscope_type(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
						const std::string error_desc = std::string("Using xscope types as base classes ")
						+ "is not supported (currently by this tool).";
						auto res = (*this).m_state1.m_error_records.emplace(
							CErrorRecord(*MR.SourceManager, SL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
						}
					}
					for (const auto& field : RD->fields()) {
						const auto field_qtype = field->getType();
						auto field_qtype_str = field_qtype.getAsString();

						if (is_xscope_type(field->getType(), (*this).m_state1)) {
							auto FDISR = instantiation_source_range(field->getSourceRange(), Rewrite);
							const std::string error_desc = std::string("Using xscope types (like '"
							+ field_qtype_str + "') as struct/class ")
							+ "fields is not supported (currently by this tool). (Note that mse::xscope_tuple<> "
							+ "does support elements of xscope type.)";
							auto res = (*this).m_state1.m_error_records.emplace(
								CErrorRecord(*MR.SourceManager, FDISR.getBegin(), error_desc));
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

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				//assert(1 == num_args);
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
								auto res = (*this).m_state1.m_error_records.emplace(
									CErrorRecord(*MR.SourceManager, CESL, error_desc));
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

	class MCSSSTFParam : public MatchFinder::MatchCallback
	{
	public:
		MCSSSTFParam (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::VarDecl* VD = MR.Nodes.getNodeAs<clang::VarDecl>("mcssstfparam1");

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

				std::string source_text;
				if (VDSL.isValid() && VDSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(VDSL, VDSLE));
				} else {
					return;
				}
				if ("" != source_text) {
					int q = 5;
				}
				if (std::string::npos != source_text.find("fparam3")) {
					int q = 5;
				}

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
					return;
				}

				auto qtype = VD->getType();
				std::string qtype_str = VD->getType().getAsString();
				const std::string TFParam_str = g_mse_namespace_str + "::rsv::TFParam<";
				if (std::string::npos != qtype_str.find(TFParam_str)) {
					bool satisfies_checks = false;
					auto FD = dyn_cast<const clang::FunctionDecl>(VD->getParentFunctionOrMethod());
					if (FD) {
						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							satisfies_checks = true;
						} else {
							auto CE = dyn_cast<const clang::CallExpr>(VD->getInit()->IgnoreImplicit()->IgnoreParenImpCasts());
							if (CE) {
								auto function_decl = CE->getDirectCallee();
								auto num_args = CE->getNumArgs();
								//assert(1 == num_args);
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
					if (!satisfies_checks) {
						const std::string error_desc = std::string("Unsupported use of ")
							+ "mse::rsv::TFParam<>. ";
						auto res = (*this).m_state1.m_error_records.emplace(
							CErrorRecord(*MR.SourceManager, VDSL, error_desc));
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

	bool can_safely_target_with_an_xscope_reference(const clang::Expr* EX1) {
		if (!EX1) {
			assert(false);
			return false;
		}
		const auto EX = EX1->IgnoreImplicit()->IgnoreParenImpCasts();
		bool satisfies_checks = false;
		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		if (DRE1) {
			auto D1 = DRE1->getDecl();
			auto VD = dyn_cast<const clang::VarDecl>(D1);
			if (VD) {
				const auto storage_duration = VD->getStorageDuration();
				if ((clang::StorageDuration::SD_Automatic == storage_duration)
					|| (clang::StorageDuration::SD_Static == storage_duration)
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
				const std::string operator_star_str = "operator*";
				auto operator_name = CXXOCE->getDirectCallee()->getNameAsString();
				if ((operator_star_str == operator_name) && (1 == CXXOCE->getNumArgs())) {
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
								satisfies_checks = true;
							}
						} else if (arg_EX->getType()->isReferenceType()) {
							int q = 5;
						}
					}
				}
				int q = 5;
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

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				//assert(1 == num_args);
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string make_xscope_pointer_to_str = g_mse_namespace_str + "::rsv::make_xscope_pointer_to";
					const std::string make_xscope_const_pointer_to_str = g_mse_namespace_str + "::rsv::make_xscope_const_pointer_to";
					if ((make_xscope_pointer_to_str == qualified_function_name) || (make_xscope_const_pointer_to_str == qualified_function_name)) {
						if (1 == num_args) {
							auto EX1 = CE->getArg(0)->IgnoreImplicit()->IgnoreParenImpCasts();
							bool satisfies_checks = can_safely_target_with_an_xscope_reference(EX1);
							if (!satisfies_checks) {
								const std::string error_desc = std::string("Cannot verify that mse::rsv::make_xscope_pointer_to() or ")
									+ "mse::rsv::make_xscope_const_pointer_to() is safe here.";
								auto res = (*this).m_state1.m_error_records.emplace(
									CErrorRecord(*MR.SourceManager, CESL, error_desc));
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

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
					return;
				}

				auto qtype = VD->getType();
				std::string qtype_str = VD->getType().getAsString();
				if (qtype->isReferenceType()) {
					if (clang::StorageDuration::SD_Automatic != VD->getStorageDuration()) {
						const std::string error_desc = std::string("Native references that are ")
							+ "not local variables (or function parameters) are not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(
							CErrorRecord(*MR.SourceManager, VDSL, error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n";
						}
					}
					auto* EX = VD->getInit();
					if (EX) {
						bool satisfies_checks = can_safely_target_with_an_xscope_reference(EX);
						if (!satisfies_checks) {
							const std::string error_desc = std::string("Cannot verify that the ")
								+ "native reference (" + qtype_str + ") is safe here.";
							auto res = (*this).m_state1.m_error_records.emplace(
								CErrorRecord(*MR.SourceManager, VDSL, error_desc));
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

				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				const auto num_args = CE->getNumArgs();
				//assert(1 == num_args);
				if (function_decl) {
					std::string function_name = function_decl->getNameAsString();
					std::string qualified_function_name = function_decl->getQualifiedNameAsString();
					const std::string mse_namespace_str1 = g_mse_namespace_str + "::";
					const std::string mse_namespace_str2 = std::string("::") + g_mse_namespace_str + "::";
					if ((0 == qualified_function_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
							|| (0 == qualified_function_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
						return;
					}
					auto param_iter = function_decl->param_begin();
					for (int i = 0; i < num_args; i++, param_iter++) {
						if (function_decl->param_end() == param_iter) {
							assert(false);
							break;
						}
						const auto qtype = (*param_iter)->getType();
						const std::string qtype_str = (*param_iter)->getType().getAsString();
						if (qtype->isReferenceType()) {
							auto EX = CE->getArg(i);
							bool satisfies_checks = can_safely_target_with_an_xscope_reference(EX);
							if (!satisfies_checks) {
								const auto *MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(EX);
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
								auto res = (*this).m_state1.m_error_records.emplace(
									CErrorRecord(*MR.SourceManager, EXSL, error_desc));
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

				std::string source_text;
				if (EXSL.isValid() && EXSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(EXSL, EXSLE));
				} else {
					return;
				}

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
					return;
				}

				{
					const std::string error_desc = std::string("Pointer arithmetic (including ")
						+ "native array subscripts) is not supported.";
					auto res = (*this).m_state1.m_error_records.emplace(
						CErrorRecord(*MR.SourceManager, EXSL, error_desc));
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
						//if (const auto *ND = dyn_cast<NamedDecl>(D))
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
			HandlerForSSSTFParam(R, tu_state()), HandlerForSSSMakeXScopePointerTo(R, tu_state()), HandlerForSSSNativeReferenceVar(R, tu_state()),
			HandlerForSSSArgToNativeReferenceParam(R, tu_state()), HandlerForSSSPointerArithmetic(R, tu_state())
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
			Matcher.addMatcher(varDecl().bind("mcssstfparam1"), &HandlerForSSSTFParam);
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
					)).bind("mcssspointerarithmetic1")))),
				hasType(pointerType())
				)).bind("mcssspointerarithmetic3"), &HandlerForSSSPointerArithmetic);
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
		MCSSSTFParam HandlerForSSSTFParam;
		MCSSSMakeXScopePointerTo HandlerForSSSMakeXScopePointerTo;
		MCSSSNativeReferenceVar HandlerForSSSNativeReferenceVar;
		MCSSSArgToNativeReferenceParam HandlerForSSSArgToNativeReferenceParam;
		MCSSSPointerArithmetic HandlerForSSSPointerArithmetic;

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
			int q = 5;
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
						bool ends_with_dot_c = ((converted_version_filename.size() >= dot_c_str.size())
								&& (0 == converted_version_filename.compare(converted_version_filename.size() - dot_c_str.size(), dot_c_str.size(), dot_c_str)));
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
