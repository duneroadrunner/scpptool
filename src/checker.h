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

	class CTUState : public CCommonTUState1 {
	public:
		/* Set of detecteed errors and warnings. */
		CErrorRecords m_error_records;
	};

	class MCSSSSupressCheckDirectiveCall : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSupressCheckDirectiveCall (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcssssuppresscheckcall");

			if ((CE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

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
							auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
							auto CEISL = CEISR.getBegin();
							auto CEISLE = CEISR.getEnd();
							for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
								if (nullptr != (*child_iter)) {
									auto l_STISR = instantiation_source_range((*child_iter)->getSourceRange(), Rewrite);
									DEBUG_SOURCE_LOCATION_STR(debug_source_location_str2, l_STISR, MR);
									DEBUG_SOURCE_TEXT_STR(debug_source_text2, l_STISR, Rewrite);
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
				auto SR = nice_source_range(CXXMD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

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

	class MCSSSSupressCheckDirectiveDeclGlobal : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSupressCheckDirectiveDeclGlobal (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::FunctionDecl* FD = MR.Nodes.getNodeAs<clang::FunctionDecl>("mcssssuppresscheckglobaldecl");

			if ((FD != nullptr))
			{
				auto SR = nice_source_range(FD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto method_name = FD->getNameAsString();
				static const std::string suppress_checks_prefix = "mse_suppress_check_directive";
				if (suppress_checks_prefix == method_name.substr(0, suppress_checks_prefix.length())) {
					auto decl_context = FD->getDeclContext();
					if (!decl_context) {
						assert(false);
					} else {
						auto FDISR = instantiation_source_range(FD->getSourceRange(), Rewrite);
						auto FDISL = FDISR.getBegin();
						auto FDISLE = FDISR.getEnd();

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

								if (FDISL == l_DISL) {
									int q = 5;
								}
								if ((FDISLE < l_DISL)
									|| ((FDISLE == l_DISL) && (FDISLE < l_DISLE))) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				if (std::string::npos != debug_source_location_str.find(":295:")) {
					int q = 5;
				}

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
					return;
				}

				{
					auto const * const EX_ii = IgnoreParenImpNoopCasts(EX, *(MR.Context));
					auto const * const CE = dyn_cast<const CallExpr>(EX_ii);
					if (CE) {
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
													if (param->getType()->isReferenceType()) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				if (std::string::npos != debug_source_location_str.find(":357:")) {
					int q = 5;
				}

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto DISR = instantiation_source_range(D->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(DISR);
				if (supress_check_flag) {
					return;
				}

				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();
					const auto TST = DD->getType()->getAs<clang::TemplateSpecializationType>();

					auto VD = dyn_cast<const clang::VarDecl>(D);
					if (VD) {
						const auto storage_duration = VD->getStorageDuration();
						const auto var_qualified_name = VD->getQualifiedNameAsString();
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto DREISR = instantiation_source_range(DRE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(DREISR);
				if (supress_check_flag) {
					return;
				}

				{
					auto D = DRE->getDecl();

#ifndef NDEBUG
					if (D->getType() != DRE->getType()) {
						auto D_qtype_str = D->getType().getAsString();
						auto DRE_qtype_str = DRE->getType().getAsString();
						if (D->getType()->isReferenceType() == DRE->getType()->isReferenceType()) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto STISR = instantiation_source_range(ST->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(STISR);
				if (supress_check_flag) {
					return;
				}

				if (ST->getRetValue()) {
					if (is_xscope_type(ST->getRetValue()->getType(), (*this).m_state1)
						&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(ST->getRetValue()->getType()))
						) {
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
							const std::string error_desc = std::string("Return values of xscope type (such as '")
							+ ST->getRetValue()->getType().getAsString() + "') need to be wrapped in the mse::return_value() function wrapper.";
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
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				if (std::string::npos != debug_source_location_str.find(":327:")) {
					int q = 5;
				}

				auto RDISR = instantiation_source_range(RD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(RDISR);
				if (supress_check_flag) {
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
				RD->getTypeForDecl();
				auto CXXRD = RD->getTypeForDecl()->getAsCXXRecordDecl();
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
							for (const auto& field : RD->fields()) {
								const auto field_qtype = field->getType();
								IF_DEBUG(auto field_qtype_str = field_qtype.getAsString();)
								if (field_qtype.getTypePtr()->isPointerType()
									&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype))
									&& (!m_state1.m_suppress_check_region_set.contains(instantiation_source_range(field->getSourceRange(), Rewrite)))
									) {
									const auto ICIEX = field->getInClassInitializer();
									if (!ICIEX) {
										unverified_pointer_fields.push_back(field);
									} else if (is_nullptr_literal(ICIEX, *(MR.Context))) {
										auto ICISR = nice_source_range(ICIEX->getSourceRange(), Rewrite);
										if (!ICISR.isValid()) {
											ICISR = SR;
										}
										const std::string error_desc = std::string("Null initialization of ")
											+ "native pointer fields (such as '" + field->getNameAsString()
											+ "') is not supported.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, ICISR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}
							if (1 <= unverified_pointer_fields.size()) {
								for (const auto& constructor : CXXRD->ctors()) {
									if (constructor->isCopyOrMoveConstructor()) {
										if (constructor->isDefaulted()) {
											continue;
										}
									}
									auto l_unverified_pointer_fields = unverified_pointer_fields;
									int num_pointer_constructor_initializers = 0;
									for (const auto& constructor_initializer : constructor->inits()) {
										const auto FD = constructor_initializer->getMember();
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
									if (1 <= l_unverified_pointer_fields.size()) {
										auto constructor_SR = nice_source_range(constructor->getSourceRange(), Rewrite);
										if (!SR.isValid()) {
											constructor_SR = SR;
										}
										const std::string error_desc = std::string("Missing constructor initializer (or ")
										+ "direct initializer) required for '" + l_unverified_pointer_fields.front()->getNameAsString()
										+ "' (raw) pointer field.";
										auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, constructor_SR.getBegin(), error_desc));
										if (res.second) {
											std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}
								}
							}
						}
						if (is_xscope_type(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_xscope_tag_base = true;
						}
						if (contains_non_owning_scope_reference(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_ContainsNonOwningScopeReference_tag_base = true;
						}
						if (referenceable_by_scope_pointer(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_ReferenceableByScopePointer_tag_base = true;
						}
					}

					for (const auto& field : RD->fields()) {
						const auto field_qtype = field->getType();
						auto field_qtype_str = field_qtype.getAsString();

						std::string error_desc;
						if (field_qtype.getTypePtr()->isPointerType()) {
							if (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype)
								&& (!m_state1.m_suppress_check_region_set.contains(instantiation_source_range(field->getSourceRange(), Rewrite)))
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

						if ((!has_xscope_tag_base) && is_xscope_type(field_qtype, (*this).m_state1)) {
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
							&& contains_non_owning_scope_reference(field_qtype, (*this).m_state1)) {
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
							&& referenceable_by_scope_pointer(field_qtype, (*this).m_state1)) {
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
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, FDISR.getBegin(), error_desc));
							if (res.second) {
								std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
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

	/* CStaticLifetimeOwner is meant to specify an entity whose (scope) lifetime will be used to
	infer information (often a lower or upper bound) about the (possibly dynamic) lifetime of an
	associated entity (possibly itself). */
	typedef std::variant<const clang::VarDecl*, const clang::CXXThisExpr*, const clang::Expr*, const clang::StringLiteral*, CScopeLifetimeInfo1> CStaticLifetimeOwner;

	template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
	template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>; // not needed as of C++20

	CScopeLifetimeInfo1 scope_lifetime_info_from_lifetime_owner(const CStaticLifetimeOwner& slov, ASTContext& Ctx) {
		CScopeLifetimeInfo1 lifetime_info_result;

		auto visitor1 = overloaded {
			[](auto slov) {
				assert(false);
				},

			[&](const clang::StringLiteral* slov) {/* the "static lifetime owner" is a string literal declaration */
				lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::Literal;
				int q = 5;
				},

			[&](const clang::VarDecl* slov) { /* the "static lifetime owner" is a variable declaration */
				const auto slo_storage_duration = slov->getStorageDuration();
				const auto slo_is_immortal = ((clang::StorageDuration::SD_Static == slo_storage_duration) || (clang::StorageDuration::SD_Thread == slo_storage_duration)) ? true : false;
				lifetime_info_result.m_category = slo_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
				lifetime_info_result.m_maybe_containing_scope = get_containing_scope(slov, Ctx);
				lifetime_info_result.m_maybe_source_range = slov->getSourceRange();
				},

			[&](const clang::CXXThisExpr* slov) { /* the "static lifetime owner" is a 'this' pointer expression' */
				lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::ThisExpression;
				},

			[&](const clang::Expr* slov) { /* the "static lifetime owner" is an expression */
				/* There's not much we can infer about the (scope) lifetime of an object if all we know
				is that it's the result of some expression. */
				},

			[&](const CScopeLifetimeInfo1& slov) {
				/* Instead of the precise "static lifetime owner", we've been given a struct that
				contains information about it's scope lifetime. */
				lifetime_info_result = slov;
				},
		};

		std::visit(visitor1, slov);
		return lifetime_info_result;
	}

	/* Given a pair of "lifetime owner" elements, it returns the one with the "shorter" lifetime. */
	std::optional<CStaticLifetimeOwner> lower_bound_lifetime_owner(const std::optional<CStaticLifetimeOwner>& lifetime_owner1,const std::optional<CStaticLifetimeOwner>& lifetime_owner2, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwner> retval;

		auto lhs_slo = lifetime_owner1;
		auto rhs_slo = lifetime_owner2;

		if (!(lhs_slo.has_value())) {
			/* An absent lifetime owner is deemed the one deemed to have the "shortest" lifetime. */
			return lifetime_owner1;
		} else if (!(rhs_slo.has_value())) {
			return lifetime_owner2;
		} else {
			auto lhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(lhs_slo.value(), Ctx);
			auto rhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(rhs_slo.value(), Ctx);

			bool lhs_lifetime_is_shorter_than_rhs = first_is_known_to_be_contained_in_scope_of_second(lhs_lifetime_info, rhs_lifetime_info, Ctx);
			retval = lhs_lifetime_is_shorter_than_rhs ? lifetime_owner1 : lifetime_owner2;
		}

		return retval;
	}

	/* Given a set of "lifetime owner" elements, it returns the one with the "shortest" lifetime. */
	std::optional<CStaticLifetimeOwner> lower_bound_lifetime_owner(const std::vector<std::optional<CStaticLifetimeOwner> >& lifetime_owners, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwner> retval;
		if (1 <= lifetime_owners.size()) {
			retval = lifetime_owners.front();
			for (const auto& lifetime_owner : lifetime_owners) {
				retval = lower_bound_lifetime_owner(retval, lifetime_owner, Ctx, tu_state_cref);
			}
		}
		return retval;
	}

	/* Given a pair of "lifetime owner" elements, it returns the one with the "longer" lifetime. */
	std::optional<CStaticLifetimeOwner> upper_bound_lifetime_owner(const std::optional<CStaticLifetimeOwner>& lifetime_owner1, const std::optional<CStaticLifetimeOwner>& lifetime_owner2, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwner> retval;
		const auto lblo = lower_bound_lifetime_owner(lifetime_owner1, lifetime_owner2, Ctx, tu_state_cref);
		/* Just return the opposite choice of the lower_bound_lifetime_owner() function. */
		retval = (lblo == lifetime_owner1) ? lifetime_owner2 : lifetime_owner1;
		return retval;
	}

	/* Given a set of "lifetime owner" elements, it returns the one with the "longest" lifetime. */
	std::optional<CStaticLifetimeOwner> upper_bound_lifetime_owner(const std::vector<std::optional<CStaticLifetimeOwner> >& lifetime_owners, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwner> retval;
		if (1 <= lifetime_owners.size()) {
			retval = lifetime_owners.front();
			for (const auto& lifetime_owner : lifetime_owners) {
				retval = upper_bound_lifetime_owner(retval, lifetime_owner, Ctx, tu_state_cref);
			}
		}
		return retval;
	}


	struct CMaybeStaticLifetimeOwnerWithHints : public std::optional<CStaticLifetimeOwner> {
		typedef std::optional<CStaticLifetimeOwner> base_class;
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

	std::optional<CStaticLifetimeOwner> lower_bound_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref);

	/* This function is meant to return the part of a given expression that directly refers to the declared
	object (i.e. the `DeclRefExpr`) of interest, if it is present in the expression. The object of interest
	is the one from which we can infer the lifetime (or a lower bound of the lifetime) of the intended
	reference target (indicated by the given expression). If the intended reference target is itself a
	declared object (as opposed to, for example, a member of another object, or an element in a container),
	then it itself would be the object of interest. The object of interest could also be a (declared)
	(scope) reference/pointer to the target object, as target objects must outlive any corresponding scope
	references, so the lifetime of a (scope) reference is a lower bound for the lifetime of the
	corresponding target object. */
	CMaybeStaticLifetimeOwnerWithHints lower_bound_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
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
			}
			return retval;
		} else if (DRE1) {
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
					if (VD->getType()->isReferenceType()) {
						/* We're assuming that imposed restrictions imply that objects targeted
						by native references qualify as scope objects (at least for the duration
						of the reference). */
						int q = 5;
					}
					satisfies_checks = true;
					retval = VD;
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
						if (VD_qtype.getTypePtr()->isPointerType()) {
							const auto pointee_VD_qtype = VD_qtype.getTypePtr()->getPointeeType();
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
		} else if (SL) {
			retval = SL;
		} else {
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME) {
				const auto VLD = ME->getMemberDecl();
				auto FD = dyn_cast<const clang::FieldDecl>(VLD);
				auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
				if (FD && !(ME->isBoundMemberFunction(Ctx))) {
					auto containing_EX = containing_object_expr_from_member_expr(ME);
					retval = lower_bound_lifetime_owner_if_available(containing_EX, Ctx, tu_state_cref);
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

							if (UOSE->getType()->isPointerType()) {
								/* The declrefexpression is a direct dereference of a native pointer. */
								satisfies_checks = true;
								retval = dyn_cast<const clang::Expr>(UOSE);
							}
						}
					}
				} else {
					auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(EX);
					auto CO = dyn_cast<const clang::ConditionalOperator>(EX);
					if (CXXCE) {
						const auto qtype = CXXCE->getType();
						const auto CXXCE_rw_type_ptr = remove_mse_transparent_wrappers(CXXCE->getType()).getTypePtr();
						bool is_pointer_or_equivalent = true;
						if (!CXXCE_rw_type_ptr->isPointerType()) {
							const auto RD = CXXCE_rw_type_ptr->getAsRecordDecl();
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
						if (is_pointer_or_equivalent) {
							const auto numArgs = CXXCE->getNumArgs();
							if (1 == CXXCE->getNumArgs()) {
								const auto arg_EX = CXXCE->getArg(0);
								assert(arg_EX);

								retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_cref);
							}
						}
					} else if (CO) {
						auto res1 = lower_bound_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_cref);
						auto res2 = lower_bound_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_cref);
						return lower_bound_lifetime_owner(res1, res2, Ctx, tu_state_cref);
					} else {
						const clang::Expr* potential_owner_EX = nullptr;
						auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(EX);
						auto CXXMCE = dyn_cast<const clang::CXXMemberCallExpr>(EX);
						auto CE = dyn_cast<const clang::CallExpr>(EX);
						if (CXXOCE) {
							static const std::string operator_star_str = "operator*";
							static const std::string operator_arrow_str = "operator->";
							static const std::string operator_subscript_str = "operator[]";
							auto operator_name = CXXOCE->getDirectCallee()->getNameAsString();

							if (((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
								auto arg_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
								if (arg_EX) {
									const auto arg_EX_qtype = arg_EX->getType();
									IF_DEBUG(const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();)

									const auto CXXRD = remove_mse_transparent_wrappers(arg_EX->getType()).getTypePtr()->getAsCXXRecordDecl();
									if (CXXRD) {
										DECLARE_CACHED_CONST_STRING(xscope_f_ptr_str, mse_namespace_str() + "::TXScopeFixedPointer");
										DECLARE_CACHED_CONST_STRING(xscope_f_const_ptr_str, mse_namespace_str() + "::TXScopeFixedConstPointer");
										DECLARE_CACHED_CONST_STRING(xscope_obj_f_ptr_str, mse_namespace_str() + "::TXScopeObjFixedPointer");
										DECLARE_CACHED_CONST_STRING(xscope_obj_f_const_ptr_str, mse_namespace_str() + "::TXScopeObjFixedConstPointer");
										DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");
										DECLARE_CACHED_CONST_STRING(reg_proxy_ptr_str, mse_namespace_str() + "::TRegisteredProxyPointer");
										DECLARE_CACHED_CONST_STRING(reg_proxy_const_ptr_str, mse_namespace_str() + "::TRegisteredProxyConstPointer");
										DECLARE_CACHED_CONST_STRING(norad_proxy_ptr_str, mse_namespace_str() + "::TNoradProxyPointer");
										DECLARE_CACHED_CONST_STRING(norad_proxy_const_ptr_str, mse_namespace_str() + "::TNoradProxyConstPointer");
										static const std::string unique_ptr_str = "std::unique_ptr";
										auto qname = CXXRD->getQualifiedNameAsString();
										if ((xscope_f_ptr_str == qname) || (xscope_f_const_ptr_str == qname)
											|| (xscope_obj_f_ptr_str == qname) || (xscope_obj_f_const_ptr_str == qname)
											|| (reg_proxy_ptr_str == qname) || (reg_proxy_const_ptr_str == qname)
											|| (norad_proxy_ptr_str == qname) || (norad_proxy_const_ptr_str == qname)
											/*|| ((xscope_owner_ptr_str == qname) && ())*/) {
											satisfies_checks = true;
											retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_cref);
											if (!retval.has_value()) {
												retval = dyn_cast<const clang::Expr>(arg_EX);
											}
											return retval;
										} else if (unique_ptr_str == qname) {
											if (arg_EX_qtype.isConstQualified()) {
												/* We're treating `const std::unique_ptr<>`s as similar to mse::TXScopeOwnerPointer<>s. */
												if (arg_EX->isLValue()) {
													satisfies_checks = true;
													retval = lower_bound_lifetime_owner_if_available(arg_EX, Ctx, tu_state_cref);
													if (!retval.has_value()) {
														retval = dyn_cast<const clang::Expr>(arg_EX);
													}
													return retval;
												}
											}
										}
									} else if (arg_EX->getType()->isReferenceType()) {
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
							auto method_name = CXXMCE->getDirectCallee()->getNameAsString();
							if ((((method_value_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								|| (((method_at_str == method_name)) && (1 == CXXMCE->getNumArgs()))
								|| (((method_front_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								|| (((method_back_str == method_name)) && (0 == CXXMCE->getNumArgs()))
								) {
								potential_owner_EX = IgnoreParenImpNoopCasts(CXXMCE->getImplicitObjectArgument(), Ctx);
							} else {
								const auto CXXMCE_qtype = CXXMCE->getType();
								if (contains_non_owning_scope_reference(CXXMCE_qtype, tu_state_cref)) {
									auto maybe_lb_lifetime_owner = lower_bound_lifetime_owner_of_returned_reference_object_if_available(CXXMCE, Ctx, tu_state_cref);
									if (maybe_lb_lifetime_owner.has_value()) {
										return maybe_lb_lifetime_owner;
									}
								}
							}
						} else if (CE) {
							auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();

							static const std::string std_move_str = "std::move";
							if ((std_move_str == function_qname) && (1 == CE->getNumArgs())) {
								return lower_bound_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_cref);
							}

							static const std::string function_get_str = "std::get";
							if (((function_get_str == function_qname)) && (1 == CE->getNumArgs())) {
								potential_owner_EX = IgnoreParenImpNoopCasts(CE->getArg(0), Ctx);
							} else {
								const auto CE_qtype = CE->getType();
								if (contains_non_owning_scope_reference(CE_qtype, tu_state_cref)) {
									auto maybe_lb_lifetime_owner = lower_bound_lifetime_owner_of_returned_reference_object_if_available(CE, Ctx, tu_state_cref);
									if (maybe_lb_lifetime_owner.has_value()) {
										return maybe_lb_lifetime_owner;
									}
								}
							}
						}
						if (potential_owner_EX) {
							auto potential_owner_EX_ii = IgnoreParenImpNoopCasts(potential_owner_EX, Ctx);
							if (potential_owner_EX_ii) {
								const auto potential_owner_EX_ii_qtype = potential_owner_EX_ii->getType();
								IF_DEBUG(const auto potential_owner_EX_ii_qtype_str = potential_owner_EX_ii_qtype.getAsString();)

								const auto CXXRD = remove_mse_transparent_wrappers(potential_owner_EX_ii->getType()).getTypePtr()->getAsCXXRecordDecl();
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
									DECLARE_CACHED_CONST_STRING(fixed_nii_optional_str, mse_namespace_str() + "::fixed_nii_optional");
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_optional_str, mse_namespace_str() + "::xscope_fixed_nii_optional");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_optional_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_optional");

									auto qname = CXXRD->getQualifiedNameAsString();
									if ((xscope_owner_ptr_str == qname) || (xscope_tuple_str == qname)

										//|| (std_unique_ptr_str == qname)
										|| (std_tuple_str == qname) || (std_pair_str == qname) || (std_array_str == qname)

										|| (mstd_tuple_str == qname) || (nii_array_str == qname) || (mstd_array_str == qname) || (xscope_nii_array_str == qname)
										|| (fixed_nii_vector_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
										|| (fixed_nii_basic_string_str == qname) || (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)
										|| (fixed_nii_optional_str == qname) || (xscope_fixed_nii_optional_str == qname) || (xscope_borrowing_fixed_nii_optional_str == qname)
										) {
										retval = lower_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_cref);
									}
								} else if (potential_owner_EX_ii->getType()->isReferenceType()) {
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

	std::optional<CStaticLifetimeOwner> lower_bound_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		std::optional<CStaticLifetimeOwner> retval;
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

			std::vector<std::optional<CStaticLifetimeOwner> > lifetime_owners;
			for (size_t i = 0; i < CXXMCE->getNumArgs(); i+=1) {
				const auto arg_EX_ii = IgnoreParenImpNoopCasts(CXXMCE->getArg(i), Ctx);
				const auto arg_EX_ii_qtype = arg_EX_ii->getType();
				if (contains_non_owning_scope_reference(arg_EX_ii_qtype, tu_state_cref)) {
					lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CXXMCE->getArg(i), Ctx, tu_state_cref));
				}
			}
			lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CXXMCE->getImplicitObjectArgument(), Ctx, tu_state_cref));
			retval = lower_bound_lifetime_owner(lifetime_owners, Ctx, tu_state_cref);
		} else if (CE) {
			IF_DEBUG(auto function_qname = CE->getDirectCallee()->getQualifiedNameAsString();)

			/* So the idea here is that since a returned scope pointer/reference object cannot target a
			local variable inside the (free) function (including parameter local variables), then it must
			refer to a (thread_local or static object or an) object targeted by a scope pointer/reference
			object passed as an argument to the function. Therefore we conclude that the target(s) of the
			returned scope pointer/reference object live at least as long as the shortest-lived scope
			pointer/reference object passed as an argument to the function. */

			std::vector<std::optional<CStaticLifetimeOwner> > lifetime_owners;
			for (size_t i = 0; i < CE->getNumArgs(); i+=1) {
				const auto arg_EX_ii = IgnoreParenImpNoopCasts(CE->getArg(i), Ctx);
				const auto arg_EX_ii_qtype = arg_EX_ii->getType();
				if (contains_non_owning_scope_reference(arg_EX_ii_qtype, tu_state_cref)) {
					lifetime_owners.push_back(lower_bound_lifetime_owner_if_available(CE->getArg(i), Ctx, tu_state_cref));
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
				retval = lower_bound_lifetime_owner(lifetime_owners, Ctx, tu_state_cref);
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
	CMaybeStaticLifetimeOwnerWithHints upper_bound_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		CMaybeStaticLifetimeOwnerWithHints retval;
		if (!EX1) {
			return retval;
		}
		const auto EX = IgnoreParenImpNoopCasts(EX1, Ctx);
		bool satisfies_checks = false;
		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		auto CXXTE = dyn_cast<const clang::CXXThisExpr>(EX);
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
					if (VD->getType()->isReferenceType()) {
						/* We're assuming that imposed restrictions imply that objects referenceed
						by native references qualify as scope objects (at least for the duration
						of the reference). */
						int q = 5;
					}
					satisfies_checks = true;
					retval = VD;
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
						if (VD_qtype.getTypePtr()->isPointerType()) {
							const auto pointee_VD_qtype = VD_qtype.getTypePtr()->getPointeeType();
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
		} else {
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME) {
				const auto VLD = ME->getMemberDecl();
				auto FD = dyn_cast<const clang::FieldDecl>(VLD);
				auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
				if (FD && !(ME->isBoundMemberFunction(Ctx))) {
					auto containing_EX = containing_object_expr_from_member_expr(ME);
					retval = upper_bound_lifetime_owner_if_available(containing_EX, Ctx, tu_state_cref);
				} else if (VD) {
					retval = VD; /* static member */
				} else {
					int q = 5;
				}
			} else {
				{
					auto CO = dyn_cast<const clang::ConditionalOperator>(EX);
					if (CO) {
						auto res1 = upper_bound_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_cref);
						auto res2 = upper_bound_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_cref);
						return upper_bound_lifetime_owner(res1, res2, Ctx, tu_state_cref);
					} else {
						const clang::Expr* potential_owner_EX = nullptr;
						auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(EX);
						auto CXXMCE = dyn_cast<const clang::CXXMemberCallExpr>(EX);
						auto CE = dyn_cast<const clang::CallExpr>(EX);
						if (CXXOCE) {
							static const std::string operator_star_str = "operator*";
							static const std::string operator_arrow_str = "operator->";
							static const std::string operator_subscript_str = "operator[]";
							auto operator_name = CXXOCE->getDirectCallee()->getNameAsString();
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
							auto method_name = CXXMCE->getDirectCallee()->getNameAsString();
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
							}
						}
						if (potential_owner_EX) {
							auto potential_owner_EX_ii = IgnoreParenImpNoopCasts(potential_owner_EX, Ctx);
							if (potential_owner_EX_ii) {
								const auto potential_owner_EX_ii_qtype = potential_owner_EX_ii->getType();
								IF_DEBUG(const auto potential_owner_EX_ii_qtype_str = potential_owner_EX_ii_qtype.getAsString();)

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
									DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_optional_str, mse_namespace_str() + "::xscope_fixed_nii_optional");
									DECLARE_CACHED_CONST_STRING(xscope_borrowing_fixed_nii_optional_str, mse_namespace_str() + "::xscope_borrowing_fixed_nii_optional");

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
									if ((xscope_owner_ptr_str == qname) || (xscope_optional_str == qname) || (xscope_tuple_str == qname)

										|| (std_unique_ptr_str == qname) || (std_shared_ptr_str == qname) || (std_optional_str == qname)
										|| (std_tuple_str == qname) || (std_pair_str == qname)
										|| (std_array_str == qname) || (std_vector_str == qname) || (std_list_str == qname)
										|| (std_map_str == qname) || (std_set_str == qname) || (std_multimap_str == qname) || (std_multiset_str == qname)
										|| (std_unordered_map_str == qname) || (std_unordered_set_str == qname) || (std_unordered_multimap_str == qname) || (std_unordered_multiset_str == qname)
										|| (xscope_nii_array_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
										|| (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)
										|| (xscope_fixed_nii_optional_str == qname) || (xscope_borrowing_fixed_nii_optional_str == qname)

										/*
										|| (mstd_optional_str == qname) || (mstd_tuple_str == qname)
										|| (nii_array_str == qname) || (mstd_array_str == qname)
										|| (nii_vector_str == qname) || (stnii_vector_str == qname) || (mtnii_vector_str == qname)
										|| (mstd_vector_str == qname)
										*/
										) {
										retval = upper_bound_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_cref);
									}
								} else if (potential_owner_EX_ii->getType()->isReferenceType()) {
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

	CBoolWithHints can_be_safely_targeted_with_an_xscope_reference(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		auto res1 = lower_bound_lifetime_owner_if_available(EX1, Ctx, tu_state_cref);
		CBoolWithHints retval = res1.has_value();
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

	CMaybeStaticLifetimeOwnerWithHints lower_bound_lifetime_owner_of_pointer_target_if_available(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		auto res1 = lower_bound_lifetime_owner_if_available(raw_pointer_target_expression_if_available(EX1, Ctx, tu_state_cref), Ctx, tu_state_cref);
		if (res1.has_value()) {
			return res1;
		} else {
			/* The lifetime owner of the pointer target directly is not available. But the lifetime of
			the pointer itself serves as a lower bound for the lifetime of its target. */
			return lower_bound_lifetime_owner_if_available(EX1, Ctx, tu_state_cref);
		}
	}

	CBoolWithHints pointer_target_can_be_safely_targeted_with_an_xscope_reference(const clang::Expr* EX1, ASTContext& Ctx, const CTUState& tu_state_cref) {
		auto res1 = lower_bound_lifetime_owner_of_pointer_target_if_available(EX1, Ctx, tu_state_cref);
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				if (std::string::npos != debug_source_location_str.find(":366:")) {
					int q = 5;
				}

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				if (std::string::npos != debug_source_location_str.find(":423:")) {
					int q = 5;
				}

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto CEISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CEISR);
				if (supress_check_flag) {
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
								if (MTE && (!(MTE->getType()->isReferenceType()))) {
									/* This argument is a temporary (non-reference) (that should outlive
									the function parameter). */
									satisfies_checks = true;
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto VDISR = instantiation_source_range(VD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(VDISR);
				if (supress_check_flag) {
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto EXISR = instantiation_source_range(EX->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(EXISR);
				if (supress_check_flag) {
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CXXMemberCallExpr* CXXMCE = MR.Nodes.getNodeAs<clang::CXXMemberCallExpr>("mcsssmemberfunctioncall1");
			const CXXOperatorCallExpr* CXXOCE = MR.Nodes.getNodeAs<clang::CXXOperatorCallExpr>("mcssscxxoperatorcall1");

			if ((CXXMCE != nullptr) || (CXXOCE != nullptr))
			{
				auto raw_SR = CXXMCE ? CXXMCE->getSourceRange() : CXXOCE->getSourceRange();
				auto SR = nice_source_range(raw_SR, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				if (std::string::npos != debug_source_location_str.find(":423:")) {
					int q = 5;
				}

				auto CXXMCEISR = instantiation_source_range(raw_SR, Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CXXMCEISR);
				if (supress_check_flag) {
					return;
				}

				std::string method_name;
				const clang::CXXMethodDecl* method_decl = nullptr;
				if (CXXMCE) {
					method_name = CXXMCE->getDirectCallee()->getNameAsString();;
					method_decl = CXXMCE->getMethodDecl();
				} else {
					method_name = CXXOCE->getDirectCallee()->getNameAsString();;
					const auto method_decl1 = CXXOCE->getDirectCallee();
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
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
						if (res.second) {
							std::cout << (*(res.first)).as_a_string1() << " \n\n";
						}
					}
				} else {
					static const std::string tilda_str = "~";
					if (string_begins_with(method_name, tilda_str)) {
						const std::string error_desc =  std::string("'") + method_name
							+ "' looks like a destructor. Explicitly calling destructors is not supported.";
						auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
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
						? pointer_target_can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), m_state1)
						: can_be_safely_targeted_with_an_xscope_reference(EX, *(MR.Context), m_state1);
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto CXXCIISR = instantiation_source_range(CXXCI->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(CXXCIISR);
				if (supress_check_flag) {
					return;
				}

				const auto FD = CXXCI->getMember();
				if (!FD) {
					/* This is presumably a base class initializer (rather than a member field initializer). */
					return;
				}
				IF_DEBUG(const auto name = FD->getNameAsString();)

				const auto EX = CXXCI->getInit();
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
									const std::string error_desc = std::string("The field '") + FD2->getNameAsString()
										+ "' may be being referenced before it has been constructed.";
									auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
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
					if (FD->getType()->isPointerType()
						&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(FD->getType()))
						&& (!m_state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
						) {
						if (is_nullptr_literal(EX, *(MR.Context))) {
							auto CISR = nice_source_range(EX->getSourceRange(), Rewrite);
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
					}
				} else {
					if (FD->getType()->isPointerType()) {
						{
							auto CISR = nice_source_range(EX->getSourceRange(), Rewrite);
							if (!CISR.isValid()) {
								CISR = SR;
							}
							const std::string error_desc = std::string("Default initialization of ")
								+ "native pointer field '" + FD->getNameAsString()
								+ "' is not supported.";
							auto res = (*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, CISR.getBegin(), error_desc));
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
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

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				if (std::string::npos != debug_source_location_str.find(":397:")) {
					int q = 5;
				}

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = (BO != nullptr) ? instantiation_source_range(BO->getSourceRange(), Rewrite)
					: instantiation_source_range(CXXOCE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
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
					if ((*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype)) {
						return;
					}
				}

				bool satisfies_checks = false;

				/* Obtaining the declaration (location) of the pointer to be modified (or its owner) (and
				therefore its scope lifetime) can be challenging. We are not always going to be able to
				do so. */

				auto lhs_slo = upper_bound_lifetime_owner_if_available(LHSEX, *(MR.Context), m_state1);
				auto rhs_slo = lower_bound_lifetime_owner_of_pointer_target_if_available(
					remove_cast_from_TPointerForLegacy_to_raw_pointer(RHSEX, *(MR.Context)), *(MR.Context), m_state1);
				if (!(lhs_slo.has_value())) {
					satisfies_checks = false;
				} else if (!(rhs_slo.has_value())) {
					satisfies_checks = false;
				} else {
					auto lhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(lhs_slo.value(), *(MR.Context));
					auto rhs_lifetime_info = scope_lifetime_info_from_lifetime_owner(rhs_slo.value(), *(MR.Context));

					satisfies_checks = first_is_known_to_be_contained_in_scope_of_second(lhs_lifetime_info, rhs_lifetime_info, *(MR.Context));
				}

				if (!satisfies_checks) {
					std::string error_desc = std::string("Unable to verify that this pointer assignment (of type '")
						+ LHSEX->getType().getAsString() + "') is safe.";
					const auto hints_str = rhs_slo.hints_str();
					if (!hints_str.empty()) {
						error_desc += " (" + hints_str + ")";
					} else {
						const auto hints_str = lhs_slo.hints_str();
						if (!hints_str.empty()) {
							error_desc += " (" + hints_str + ")";
						} else {
							error_desc += " (Possibly due to being unable to verify that the new target object outlives the (scope) pointer.)";
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
			HandlerForSSSSupressCheckDirectiveCall(R, tu_state()), HandlerForSSSSupressCheckDirectiveDeclField(R, tu_state()),
			HandlerForSSSSupressCheckDirectiveDeclGlobal(R, tu_state()), 
			HandlerForSSSExprUtil(R, tu_state()), HandlerForSSSDeclUtil(R, tu_state()), HandlerForSSSDeclRefExprUtil(R, tu_state()),
			HandlerForSSSReturnStmt(R, tu_state()), HandlerForSSSRecordDecl2(R, tu_state()), HandlerForSSSAsAnFParam(R, tu_state()),
			HandlerForSSSMakeXScopePointerTo(R, tu_state()), HandlerForSSSNativeReferenceVar(R, tu_state()),
			HandlerForSSSArgToNativeReferenceParam(R, tu_state()), HandlerForSSSPointerArithmetic(R, tu_state()), HandlerForSSSAddressOf(R, tu_state()),
			HandlerForSSSNativePointerVar(R, tu_state()), HandlerForSSSCast(R, tu_state()), HandlerForSSSMemberFunctionCall(R, tu_state()),
			HandlerForSSSConstructionInitializer(R, tu_state()), HandlerForSSSPointerAssignment(R, tu_state())
		{
			Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerMisc1);
			Matcher.addMatcher(callExpr(argumentCountIs(0)).bind("mcssssuppresscheckcall"), &HandlerForSSSSupressCheckDirectiveCall);
			Matcher.addMatcher(cxxMethodDecl(decl().bind("mcssssuppresscheckmemberdecl")), &HandlerForSSSSupressCheckDirectiveDeclField);
			Matcher.addMatcher(functionDecl(decl().bind("mcssssuppresscheckglobaldecl")), &HandlerForSSSSupressCheckDirectiveDeclGlobal);

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
		MCSSSSupressCheckDirectiveCall HandlerForSSSSupressCheckDirectiveCall;
		MCSSSSupressCheckDirectiveDeclField HandlerForSSSSupressCheckDirectiveDeclField;
		MCSSSSupressCheckDirectiveDeclGlobal HandlerForSSSSupressCheckDirectiveDeclGlobal;
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

	/* global variable to store (some of) the information obtained during the "checker" pass */
	thread_local std::vector<CTUState> g_final_tu_states;

	class MyFrontendAction : public ASTFrontendActionCompatibilityWrapper1
	{
	public:
		MyFrontendAction() {
			int q = 5;
		}
		~MyFrontendAction() {
			std::cout << "\n" << (*this).m_tu_state.m_error_records.size() << " errors found. \n";
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
