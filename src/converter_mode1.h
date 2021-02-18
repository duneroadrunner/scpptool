// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef __CONVERTER_MODE1_H
#define __CONVERTER_MODE1_H

#include "utils1.h"
#include "checker.h"

/*Standard headers*/
#include <string>
#include <iostream>
#include <string>
#include <functional>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <list>
#include <algorithm>
#include <limits>
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


namespace convm1 {
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
    bool DoNotResolveMergeConflicts = false;
    std::string ConvertMode = "";
    bool ScopeTypeFunctionParameters = false;
    bool ScopeTypePointerFunctionParameters = false;

    struct Options {
        bool CheckSystemHeader = false;
        bool MainFileOnly = false;
        bool ConvertToSCPP = true;
        bool CTUAnalysis = false;
        bool EnableNamespaceImport = false;
        bool SuppressPrompts = false;
        bool DoNotReplaceOriginalSource = false;
        std::string MergeCommand = "";
        bool DoNotResolveMergeConflicts = false;
        std::string ConvertMode = "";
        bool ScopeTypeFunctionParameters = false;
        bool ScopeTypePointerFunctionParameters = false;
    };

	/* This class specifies a declaration and a level of "indirection"(/"dereference") relative to the declared
	* object. For example, given the declaration "int **var1[5];", (*var1) and (**var1) are 1 and 2 "levels of
	* indirection", respectively, relative to var1. */
	class CDDeclIndirection {
	public:
		static const size_t no_indirection = std::numeric_limits<size_t>::max();
		CDDeclIndirection(const clang::DeclaratorDecl& ddecl_cref, size_t indirection_level) :
			m_ddecl_cptr(&ddecl_cref), m_indirection_level(indirection_level) {}
		CDDeclIndirection(const CDDeclIndirection&) = default;
		bool operator <(const CDDeclIndirection &rhs) const {
			if (m_ddecl_cptr == rhs.m_ddecl_cptr) {
				return (m_indirection_level < rhs.m_indirection_level);
			} else {
				return (m_ddecl_cptr < rhs.m_ddecl_cptr);
			}
		}

		const clang::DeclaratorDecl* m_ddecl_cptr = nullptr;
		size_t m_indirection_level = 0;
	};

	/* We use the term "indirect type" to mean basically a type the "dereferences" to another
	specifiable type. At least initially, we primarily use it to refer to native pointers and arrays
	(that are candidates for being converted to safer counterparts). */
	/* CIndirectionState holds information used to determine what (safe) type, if any, the
	associated "indirect type" instance should be converted to. */
	class CIndirectionState {
	public:
		CIndirectionState(const std::optional<clang::TypeLoc>& maybe_typeLoc, const std::string& original, const std::string& current,
				bool current_is_function_type = false, const std::vector<std::string>& params = std::vector<std::string>())
			: m_original_species(original), m_current_species(current), m_current_is_function_type(current_is_function_type),
				m_params_original(params), m_params_current(params), m_maybe_typeLoc(maybe_typeLoc) {}
		CIndirectionState(const std::optional<clang::TypeLoc>& maybe_typeLoc, const std::string& original, const std::string& current,
				const std::string& array_size_expr)
			: m_original_species(original), m_current_species(current), m_array_size_expr(array_size_expr), m_maybe_typeLoc(maybe_typeLoc) {}
		CIndirectionState(const std::string& original, const std::string& current,
				bool current_is_function_type = false, const std::vector<std::string>& params = std::vector<std::string>())
			: m_original_species(original), m_current_species(current), m_current_is_function_type(current_is_function_type),
				m_params_original(params), m_params_current(params) {}
		CIndirectionState(const std::string& original, const std::string& current,
				const std::string& array_size_expr)
			: m_original_species(original), m_current_species(current), m_array_size_expr(array_size_expr) {}
		CIndirectionState(const CIndirectionState& src) = default;

		bool current_is_function_type() const { return m_current_is_function_type; }
		std::string current_params_string() const {
			std::string retval;
			if (1 <= m_params_current.size()) {
				static const std::string comma_space_str = ", ";
				for (auto& param_current : m_params_current) {
					retval += param_current + comma_space_str;
				}
				retval = retval.substr(0, retval.size() - comma_space_str.size());
				retval = "(" + retval + ")";
			}
			return retval;
		}
		void set_current_species(const std::string& new_current) {
			m_current_species = new_current;
		}
		const std::string& current_species() const {
			return m_current_species;
		}
		const std::string& original_species() const {
			return m_original_species;
		}
		void set_current_pointer_target_state(const std::string& new_current) {
			m_current_pointer_target_state = new_current;
		}
		void set_original_pointer_target_state(const std::string& pointer_target_state) {
			m_original_pointer_target_state = pointer_target_state;
			set_current_pointer_target_state(pointer_target_state);
		}
		const std::string& current_pointer_target_state() const {
			return m_current_pointer_target_state;
		}
		const std::string& original_pointer_target_state() const {
			return m_original_pointer_target_state;
		}

	private:
		std::string m_original_species;
		std::string m_current_species;

	public:
		std::string m_array_size_expr;
		bool m_array_size_expr_read_from_source_text = false;

		bool m_current_is_function_type = false;
		std::vector<std::string> m_params_original;
		std::vector<std::string> m_params_current;
		std::string m_params_current_str;
		
		std::optional<clang::TypeLoc> m_maybe_typeLoc;
		std::string m_prefix_str;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_prefix_SR_or_insert_before_point;
		std::string m_suffix_str;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_suffix_SR_or_insert_before_point;
		std::optional<clang::SourceRange> m_maybe_source_range;
		std::optional<clang::SourceRange> m_maybe_source_range_including_any_const_qualifier;
		std::optional<clang::SourceRange> m_maybe_prefix_source_range;
		std::optional<clang::SourceRange> m_maybe_suffix_source_range;

		std::string m_original_pointer_target_state;
		std::string m_current_pointer_target_state;
	};

	/* CTypeState1 holds information about a type instance (in the source text) and the type, if
	any, it may/will be converted to. */
	class CTypeState1 {
		public:
		void set_current_qtype(const clang::QualType& qtype) {
			m_maybe_current_qtype = qtype;
			m_current_qtype_is_current = true;
		}
		void set_original_qtype(const clang::QualType& qtype) {
			m_maybe_original_qtype = qtype;
			set_current_qtype(qtype);
		}
		void set_current_qtype_str(const std::string& qtype_str) {
			m_current_qtype_str = qtype_str;
			m_current_qtype_is_current = false;
		}
		std::string current_qtype_str() const {
			if (m_current_qtype_is_current) {
				if (!m_maybe_current_qtype.has_value()) {
					assert(false);
				} else {
					return m_maybe_current_qtype.value().getAsString();
				}
			}
			return m_current_qtype_str;
		}
		std::optional<clang::QualType> current_qtype_if_any() const { return m_maybe_current_qtype;}
		bool current_qtype_is_current() const { return m_current_qtype_is_current; }
		bool qtype_has_been_changed() const {
			if (!m_maybe_original_qtype.has_value()) {
				assert(false);
				return true;
			} else {
				if (m_current_qtype_is_current) {
					assert(m_maybe_current_qtype.has_value());
					return (m_maybe_current_qtype.value() != m_maybe_original_qtype.value());
				} else if (m_maybe_original_qtype.value().getAsString() != m_current_qtype_str)  {
					return true;
				}
			}
			return false;
		}
		void set_current_pointer_target_state(const std::string& pointer_target_state) {
			m_current_pointer_target_state = pointer_target_state;
		}
		void set_original_pointer_target_state(const std::string& pointer_target_state) {
			m_original_pointer_target_state = pointer_target_state;
			set_current_pointer_target_state(pointer_target_state);
		}
		const std::string& current_pointer_target_state() const {
			return m_current_pointer_target_state;
		}
		bool is_const() const {
			if (current_qtype_is_current()) {
				return current_qtype_if_any().value().isConstQualified();
			} else if (m_maybe_original_qtype.has_value() && (!qtype_has_been_changed())) {
				return m_maybe_original_qtype.value().isConstQualified();
			} else {
				/* hack alert */
				auto padded_current_qtype_str = " " + current_qtype_str() + " ";
				return (std::string::npos != padded_current_qtype_str.find(" const "));
			}
		}

		std::optional<clang::SourceRange> m_maybe_source_range;
		std::optional<clang::SourceRange> m_maybe_source_range_including_any_const_qualifier;
		std::optional<clang::TypeLoc> m_maybe_typeLoc;
		std::string m_current_params_str;

		std::string m_original_pointer_target_state;
		std::string m_current_pointer_target_state;

		private:
		std::optional<clang::QualType> m_maybe_original_qtype;
		bool m_current_qtype_is_current = false;
		std::optional<clang::QualType> m_maybe_current_qtype;
		std::string m_current_qtype_str;
	};

	/* CIndirectionStateStack holds information about an instance of a type that is composed of
	nested "indirect types" (like (native) pointers and/or arrays) and the ultimate direct type.
	So for example, the type 'int*[3]' would be an array (indirect type) of pointers (indirect
	type) to ints (direct type).
	Note that currently here we're only supporting indirect types whose dereference operator(s)
	return exactly one type, which is probably sufficient for C code but not C++ code. For example,
	tuples could also be considered indirect types with the corresponding 'std::get<>()' functions
	(potentially returning different types) as their dereference operator(s). In order to support
	multiple dereference types, this CIndirectionStateStack would probably need to be generalized
	to a CIndirectionStateTree. */
	class CIndirectionStateStack : public std::vector<CIndirectionState> {
	public:
		std::optional<const clang::DeclaratorDecl*> m_maybe_DD;
		CTypeState1 m_direct_type_state;
	};

	/* Given a type and an (empty) CIndirectionStateStack, this function will fill the stack with indications of
	* whether each level of indirection (if any) of the type is of the pointer or the array variety. Pointers
	* can, of course, function as arrays, but context is required to identify those situations. Such identification
	* is not done in this function. It is done elsewhere.  */
	clang::QualType populateQTypeIndirectionStack(CIndirectionStateStack& stack, const clang::QualType qtype, std::optional<clang::TypeLoc> maybe_typeLoc = {}, int depth = 0) {
		auto l_qtype = qtype;

		bool is_function_type = false;
		std::vector<std::string> param_strings;
		if(l_qtype->isFunctionType()) {
			auto type_class = l_qtype->getTypeClass();
			if (clang::Type::Decayed == type_class) {
				int q = 5;
			} else if (clang::Type::FunctionNoProto == type_class) {
				int q = 5;
			} else if (clang::Type::FunctionProto == type_class) {
				if (llvm::isa<const clang::FunctionProtoType>(l_qtype)) {
					auto FNQT = llvm::cast<const clang::FunctionProtoType>(l_qtype);
					if (FNQT) {
						auto num_params = FNQT->getNumParams();
						auto param_types = FNQT->param_types();
						for (auto& param_type : param_types) {
							param_strings.push_back(param_type.getAsString());
						}
					} else {
						assert(false);
					}
				} else {
					assert(false);
				}
				int q = 5;
			}

			if (clang::Type::Paren == type_class) {
				if (llvm::isa<const clang::ParenType>(l_qtype)) {
					auto PNQT = llvm::cast<const clang::ParenType>(l_qtype);
					if (PNQT) {
						std::optional<clang::TypeLoc> new_maybe_typeLoc;
						if (maybe_typeLoc.has_value()) {
							auto ParenLoc = (*maybe_typeLoc).getAs<clang::ParenTypeLoc>();
							if (ParenLoc) {
								new_maybe_typeLoc = ParenLoc.getInnerLoc();
							}
						}
						return populateQTypeIndirectionStack(stack, PNQT->getInnerType(), new_maybe_typeLoc, depth+1);
					} else {
						assert(false);
					}
				} else {
					int q = 7;
				}
			}
			if (llvm::isa<const clang::FunctionType>(l_qtype)) {
				auto FNQT = llvm::cast<const clang::FunctionType>(l_qtype);
				if (FNQT) {
					is_function_type = true;
					l_qtype = FNQT->getReturnType();

					if (maybe_typeLoc.has_value()) {
						auto FunLoc = (*maybe_typeLoc).getAs<clang::FunctionTypeLoc>();
						if (false && FunLoc) {
							maybe_typeLoc = FunLoc.getReturnLoc();
						}
					}
				} else {
					assert(false);
				}
			}
		}

		std::string l_qtype_str = l_qtype.getAsString();
		auto TP = l_qtype.getTypePtr();

		if (TP->isArrayType()) {
			auto type_class = l_qtype->getTypeClass();
			if (clang::Type::Decayed == type_class) {
				int q = 5;
			} else if (clang::Type::ConstantArray == type_class) {
				int q = 5;
			}

			std::string size_text;
			if (llvm::isa<const clang::VariableArrayType>(TP)) {
				auto VATP = llvm::cast<const clang::VariableArrayType>(TP);
				if (!VATP) {
					assert(false);
				} else {
					auto size_expr = VATP->getSizeExpr();
					//auto SR = nice_source_range(size_expr->getSourceRange(), Rewrite);
					//size_text = Rewrite.getRewrittenText(SR);
				}
			} else if (llvm::isa<const clang::ConstantArrayType>(TP)) {
				auto CATP = llvm::cast<const clang::ConstantArrayType>(TP);
				if (!CATP) {
					assert(false);
				} else {
					if (true || (!maybe_typeLoc.has_value())) {
						/* When there is no source text, we'll generate the array size expression text here. */
						auto array_size = CATP->getSize();
						size_text = array_size.toString(10, false);/*check this*/
					} else {
						/* When there is source text, the array size expression will be read from the source
						(elsewhere) when required. */
					}

					if (false) {
						/*
						auto DDSR = nice_source_range(DD->getSourceRange(), Rewrite);
						std::string array_size_expression_text;
						std::string source_text;
						if (DDSR.isValid()) {
							source_text = Rewrite.getRewrittenText(DDSR);

							auto left_bracket_pos = source_text.find('[');
							auto right_bracket_pos = source_text.find(']');
							if ((std::string::npos != left_bracket_pos) && (std::string::npos != right_bracket_pos)
									&& (left_bracket_pos + 1 < right_bracket_pos)) {
								auto array_size_expression_text = source_text.substr(left_bracket_pos + 1, right_bracket_pos - (left_bracket_pos + 1));
								int q = 3;
							} else {
								int q = 7;
							}
						} else {
							int q = 5;
						}
						*/
					}
				}
			}

			const clang::ArrayType* ATP = TP->getAsArrayTypeUnsafe();
			if (ATP) {
				clang::QualType QT = ATP->getElementType();
				IF_DEBUG(auto l_type_str = QT.getAsString();)

				std::optional<clang::TypeLoc> new_maybe_typeLoc;
				if (maybe_typeLoc.has_value()) {
					auto ArrayLoc = (*maybe_typeLoc).getAs<clang::ArrayTypeLoc>();
					if (ArrayLoc) {
						new_maybe_typeLoc = ArrayLoc.getElementLoc();
					}
				}

				stack.push_back(CIndirectionState(maybe_typeLoc, "native array", "native array", size_text));

				return populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, depth+1);
			} else {
				assert(false);
			}

		} else if (l_qtype->isPointerType()) {
			auto type_class = l_qtype->getTypeClass();
			if (clang::Type::Decayed == type_class) {
				int q = 5;
			} else if (clang::Type::Pointer == type_class) {
				int q = 5;
			}

			if (llvm::isa<const clang::PointerType>(l_qtype)) {
				auto PQT = llvm::cast<const clang::PointerType>(l_qtype);
				if (PQT) {
					int q = 5;
				} else {
					int q = 5;
				}
			} else {
				int q = 5;
			}

			clang::QualType QT = l_qtype->getPointeeType();
			IF_DEBUG(auto l_type_str = QT.getAsString();)

			std::optional<clang::TypeLoc> new_maybe_typeLoc;
			if (maybe_typeLoc.has_value()) {
				auto typeLoc = (*maybe_typeLoc);
				IF_DEBUG(auto typeLocClass = typeLoc.getTypeLocClass();)
				auto PointerLoc = typeLoc.getAs<clang::PointerTypeLoc>();
				while (!PointerLoc) {
					auto etl = typeLoc.getAs<clang::ElaboratedTypeLoc>();
					auto qtl = typeLoc.getAs<clang::QualifiedTypeLoc>();
					if (etl) {
						typeLoc = etl.getNamedTypeLoc();
						PointerLoc = typeLoc.getAs<clang::PointerTypeLoc>();
					} else if (qtl) {
						typeLoc = qtl.getUnqualifiedLoc();
						PointerLoc = typeLoc.getAs<clang::PointerTypeLoc>();
					} else {
						break;
					}
				}
				if (PointerLoc) {
					new_maybe_typeLoc = PointerLoc.getPointeeLoc();
				} else {
					auto ArrayLoc = typeLoc.getAs<clang::ArrayTypeLoc>();
					if (ArrayLoc) {
						new_maybe_typeLoc = ArrayLoc.getElementLoc();
					} else {
						int q = 5;
					}
				}
			}

			stack.push_back(CIndirectionState(maybe_typeLoc, "native pointer", "native pointer", is_function_type, param_strings));

			return populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, depth+1);
		} else if (l_qtype->isReferenceType()) {
			auto type_class = l_qtype->getTypeClass();

			if (llvm::isa<const clang::ReferenceType>(l_qtype)) {
				auto PQT = llvm::cast<const clang::ReferenceType>(l_qtype);
				if (PQT) {
					int q = 5;
				} else {
					int q = 5;
				}
			} else {
				int q = 5;
			}

			clang::QualType QT = l_qtype->getPointeeType();
			IF_DEBUG(auto l_type_str = QT.getAsString();)

			std::optional<clang::TypeLoc> new_maybe_typeLoc;
			if (maybe_typeLoc.has_value()) {
				auto typeLoc = (*maybe_typeLoc);
				auto ReferenceLoc = typeLoc.getAs<clang::ReferenceTypeLoc>();
				while (!ReferenceLoc) {
					auto etl = typeLoc.getAs<clang::ElaboratedTypeLoc>();
					if (etl) {
						typeLoc = etl.getNamedTypeLoc();
						ReferenceLoc = typeLoc.getAs<clang::ReferenceTypeLoc>();
					} else {
						break;
					}
				}
				if (ReferenceLoc) {
					new_maybe_typeLoc = ReferenceLoc.getPointeeLoc();
				}
			}

			stack.push_back(CIndirectionState(maybe_typeLoc, "native reference", "native reference", is_function_type, param_strings));

			return populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, depth+1);
		}

		if (maybe_typeLoc.has_value()) {
			stack.m_direct_type_state.m_maybe_typeLoc = maybe_typeLoc;
		}

		if (is_function_type) {
			return qtype;
		} else {
			return l_qtype;
		}
	}

	/* Given an expression (in the form of a clang::Stmt) and an (empty) (string) stack,
	* this function will fill the stack with indications of whether each level of indirection
	* (if any) (in the expression) is a pointer dereference or an array subscript. */
	const clang::Expr* populateStmtIndirectionStack(std::vector<std::string>& stack, const clang::Stmt& stmt, int depth = 0) {
		const clang::Expr* retval = nullptr;
		const clang::Stmt* ST = &stmt;
		auto stmt_class = ST->getStmtClass();
		auto stmt_class_name = ST->getStmtClassName();
		bool process_child_flag = false;
		if (clang::Stmt::StmtClass::ArraySubscriptExprClass == stmt_class) {
			stack.push_back("ArraySubscriptExpr");
			process_child_flag = true;
		} else if (clang::Stmt::StmtClass::UnaryOperatorClass == stmt_class) {
			auto UO = llvm::cast<const clang::UnaryOperator>(ST);
			if (UO) {
				if (clang::UnaryOperatorKind::UO_Deref == UO->getOpcode()) {
					stack.push_back("Deref");
					process_child_flag = true;
				} else {
					auto QT = UO->getType();
					const clang::Type* TP = QT.getTypePtr();
					if (TP && TP->isPointerType()) {
						if ((clang::UnaryOperatorKind::UO_PreInc == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PostInc == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PreDec == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PostDec == UO->getOpcode())) {
							/* Incrementing/decrementing a pointer type is pointer arithmetic and
							* implies the pointer is being used as an array iterator. */
							/* To do: modify the stack entry to reflect this. */
						}
					}
				}
			} else {
				assert(false);
			}
		} else if ((clang::Stmt::StmtClass::ImplicitCastExprClass == stmt_class)) {
			auto ICE = llvm::cast<const clang::ImplicitCastExpr>(ST);
			if (ICE) {
				auto cast_kind_name = ICE->getCastKindName();
				auto cast_kind = ICE->getCastKind();
				if ((clang::CK_FunctionToPointerDecay == cast_kind)) {
					process_child_flag = true;
				} else {
					if ((clang::CK_ArrayToPointerDecay == cast_kind) || (clang::CK_LValueToRValue == cast_kind)) {
						process_child_flag = true;
					} else {
						process_child_flag = true;
					}
				}
			} else { assert(false); }
		} else if ((clang::Stmt::StmtClass::CStyleCastExprClass == stmt_class)) {
			auto CSCE = llvm::cast<const clang::CStyleCastExpr>(ST);
			if (CSCE) {
				auto cast_kind_name = CSCE->getCastKindName();
				auto cast_kind = CSCE->getCastKind();
				auto qtype = CSCE->getType();
				if ((clang::CK_FunctionToPointerDecay == cast_kind)) {
					process_child_flag = true;
				} else {
					if ((clang::CK_ArrayToPointerDecay == cast_kind) || (clang::CK_LValueToRValue == cast_kind)) {
						process_child_flag = true;
					} else {
						process_child_flag = true;
					}
				}
			} else { assert(false); }
		} else if ((clang::Stmt::StmtClass::ParenExprClass == stmt_class)) {
			process_child_flag = true;
		} else if ((clang::Stmt::StmtClass::CallExprClass == stmt_class)) {
			process_child_flag = true;
		} else if(clang::Stmt::StmtClass::DeclRefExprClass == stmt_class) {
			auto DRE = llvm::cast<const clang::DeclRefExpr>(ST);
			if (DRE) {
				retval = DRE;
				process_child_flag = true;
			} else {
				assert(false);
			}
		} else if(clang::Stmt::StmtClass::MemberExprClass == stmt_class) {
			auto ME = llvm::cast<const clang::MemberExpr>(ST);
			if (ME) {
				retval = ME;
			} else {
				assert(false);
			}
		} else {
			if (0 == depth) {
				int q = 5;
			}
			int q = 5;
		}
		if (process_child_flag) {
			auto child_iter = ST->child_begin();
			if (child_iter != ST->child_end()) {
				if (nullptr != (*child_iter)) {
					const auto noted_stack_size = stack.size();
					auto res = populateStmtIndirectionStack(stack, *(*child_iter), depth+1);
					if ((nullptr == retval) || (stack.size() > noted_stack_size)) {
						retval = res;
					}
				} else {
					assert(false);
				}
			} else {
				int q = 5;
			}
		}
		return retval;
	}

	const clang::Stmt* find_stmt(const clang::Stmt::StmtClass& target_stmt_class, const clang::Stmt& stmt, int depth = 0) {
		const clang::Stmt* ST = &stmt;
		const auto stmt_class = ST->getStmtClass();
		const auto stmt_class_name = ST->getStmtClassName();
		if (stmt_class == target_stmt_class) {
			return ST;
		}
		bool process_children_flag = true;
		if (process_children_flag) {
			for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
				if (nullptr != (*child_iter)) {
					auto res1 = find_stmt(target_stmt_class, *(*child_iter), depth+1);
					if (res1) {
						return res1;
					}
				} else {
					assert(false);
				}
			}
		}
		return nullptr;
	}

	void walkTheAST1(const clang::Stmt& stmt, int depth = 0) {
		const clang::Stmt* ST = &stmt;
		auto stmt_class = ST->getStmtClass();
		auto stmt_class_name = ST->getStmtClassName();
		bool process_children_flag = true;
		if (clang::Stmt::StmtClass::ArraySubscriptExprClass == stmt_class) {
			//stack.push_back("ArraySubscriptExpr");
			process_children_flag = true;
		} else if (clang::Stmt::StmtClass::UnaryOperatorClass == stmt_class) {
			auto UO = llvm::cast<const clang::UnaryOperator>(ST);
			if (UO) {
				if (clang::UnaryOperatorKind::UO_Deref == UO->getOpcode()) {
					//stack.push_back("Deref");
					process_children_flag = true;
				} else {
					auto QT = UO->getType();
					const clang::Type* TP = QT.getTypePtr();
					if (TP && TP->isPointerType()) {
						if ((clang::UnaryOperatorKind::UO_PreInc == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PostInc == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PreDec == UO->getOpcode())
								|| (clang::UnaryOperatorKind::UO_PostDec == UO->getOpcode())) {
							/* Incrementing/decrementing a pointer type is pointer arithmetic and
							* implies the pointer is being used as an array iterator. */
							int q = 5;
						}
					}
				}
			} else {
				assert(false);
			}
		} else if ((clang::Stmt::StmtClass::ImplicitCastExprClass == stmt_class)) {
			auto ICE = llvm::cast<const clang::ImplicitCastExpr>(ST);
			if (ICE) {
				auto cast_kind_name = ICE->getCastKindName();
				auto cast_kind = ICE->getCastKind();
				if ((clang::CK_FunctionToPointerDecay == cast_kind)) {
					process_children_flag = false;
				} else {
					if ((clang::CK_ArrayToPointerDecay == cast_kind) || (clang::CK_LValueToRValue == cast_kind)) {
						process_children_flag = true;
					} else {
						process_children_flag = true;
					}
				}
			} else { assert(false); }
		} else if ((clang::Stmt::StmtClass::ParenExprClass == stmt_class)) {
			process_children_flag = true;
		} else if(clang::Stmt::StmtClass::DeclRefExprClass == stmt_class) {
			auto DRE = llvm::cast<const clang::DeclRefExpr>(ST);
			if (DRE) {
				//retval = DRE;
				process_children_flag = true;
			} else {
				assert(false);
			}
		} else if(clang::Stmt::StmtClass::MemberExprClass == stmt_class) {
			auto ME = llvm::cast<const clang::MemberExpr>(ST);
			if (ME) {
				//retval = ME;
			} else {
				assert(false);
			}
		} else {
			if (0 == depth) {
				int q = 5;
			}
			int q = 5;
		}
		if (process_children_flag) {
			for (auto child_iter = ST->child_begin(); child_iter != ST->child_end(); child_iter++) {
				if (nullptr != (*child_iter)) {
					walkTheAST1(*(*child_iter), depth+1);
				} else {
					assert(false);
				}
			}
		}
		return;
	}

	class CDDeclConversionState {
	public:
		CDDeclConversionState(const clang::DeclaratorDecl& ddecl) : m_ddecl_cptr(&ddecl) {
			if ((*this).m_ddecl_cptr) {
				std::string variable_name = m_ddecl_cptr->getNameAsString();
				if ("buffer" == variable_name) {
					std::string qtype_str = m_ddecl_cptr->getType().getAsString();
					if ("const unsigned char *" == qtype_str) {
						int q = 5;
					}
				}
			}
			QualType QT = ddecl.getType();
			IF_DEBUG(std::string qtype_str = m_ddecl_cptr->getType().getAsString();)
			assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
			if (QT->isFunctionType()) {
				m_is_a_function = true;
			}
			std::optional<clang::TypeLoc> maybe_typeLoc;
			auto tsi = ddecl.getTypeSourceInfo();
			if (tsi) {
				maybe_typeLoc = tsi->getTypeLoc();
				IF_DEBUG(std::string tl_qtype_str = tsi->getTypeLoc().getType().getAsString();)
				if (tsi->getTypeLoc().getType() != QT) {
					int q = 5;
				}
			}
			auto original_direct_qtype = populateQTypeIndirectionStack(m_indirection_state_stack, QT, maybe_typeLoc);
			set_original_direct_qtype(original_direct_qtype);
			//std::reverse(m_indirection_state_stack.begin(), m_indirection_state_stack.end());
			m_indirection_state_stack.m_maybe_DD = &ddecl;
		}
		std::optional<clang::SourceRange> new_overwrite_location_for_delimiter_and_type() const {
			std::optional<clang::SourceRange> retval;
			auto DD = m_ddecl_cptr;
			auto VD = dyn_cast<const clang::VarDecl>(DD);
			if (VD) {
				auto individual_declarator_decls = IndividualDeclaratorDecls(DD);
				if (1 > individual_declarator_decls.size()) {
					assert(false);
				} else if (individual_declarator_decls.front() != DD) {
					assert(2 <= individual_declarator_decls.size());
					int DD_index = 1;
					for (; DD_index < int(individual_declarator_decls.size()); DD_index += 1) {
						if (individual_declarator_decls[DD_index] == DD) {
							break;
						}
					}
					if (int(individual_declarator_decls.size()) <= DD_index) {
						assert(false);
					} else {
						auto l_SL1 = individual_declarator_decls[DD_index - 1]->getSourceRange().getEnd().getLocWithOffset(+1);
						auto l_SL2 = VD->getLocation().getLocWithOffset(-1);
						if ((l_SL1 < l_SL2) || (l_SL1 == l_SL2)) {
							retval = clang::SourceRange{l_SL1, l_SL2};
							//std::string interstitial_text = Rewrite.getRewrittenText({l_SL1, l_SL2});
							int q = 5;
						} else {
							assert(false);
						}
					}
				}
			}
			return retval;
		}
		void set_indirection_current(size_t indirection_level, const std::string& new_current) {
#ifndef NDEBUG
			if ((*this).m_ddecl_cptr) {
				std::string variable_name = m_ddecl_cptr->getNameAsString();
				if ("buffer" == variable_name) {
					std::string qtype_str = m_ddecl_cptr->getType().getAsString();
					if ("const unsigned char *" == qtype_str) {
						int q = 5;
					}
				}
			}
#endif /*!NDEBUG*/
			(*this).m_indirection_state_stack.at(indirection_level).set_current_species(new_current);
		}
		const std::string& indirection_current(size_t indirection_level) const {
			return (*this).m_indirection_state_stack.at(indirection_level).current_species();
		}
		bool is_an_indirect_type(size_t indirection_level = CDDeclIndirection::no_indirection) const {
			int i_indirection_level = (CDDeclIndirection::no_indirection == indirection_level) ? -1 : int(indirection_level);
			return (int(m_indirection_state_stack.size()) > (i_indirection_level + 1));
		}
		bool has_been_determined_to_be_an_array(size_t indirection_level) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection != indirection_level) && (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				const auto& current_state = m_indirection_state_stack.at(indirection_level).current_species();
				if (("inferred array" == current_state) || ("dynamic array" == current_state) || ("native array" == current_state)) {
					retval = true;
				}
			}
			return retval;
		}
		bool has_been_determined_to_be_a_dynamic_array(size_t indirection_level) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection != indirection_level) && (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				const auto& current_state = m_indirection_state_stack.at(indirection_level).current_species();
				if ("dynamic array" == current_state) {
					retval = true;
				}
			}
			return retval;
		}
		bool has_been_determined_to_be_a_pointer_target(size_t indirection_level = CDDeclIndirection::no_indirection) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection == indirection_level) || (m_indirection_state_stack.size() > indirection_level));
			static const std::string pointer_target_str = "pointer target";
			const auto& current_pointer_target_state_cref = (CDDeclIndirection::no_indirection == indirection_level)
				? direct_type_state_ref().current_pointer_target_state() : m_indirection_state_stack.at(indirection_level).current_pointer_target_state();
			if (pointer_target_str == current_pointer_target_state_cref) {
				retval = true;
			}
			return retval;
		}
		bool direct_qtype_has_been_changed() const {
			return direct_type_state_ref().qtype_has_been_changed();
		}
		bool initializer_has_been_changed() const {
			if (m_original_initialization_has_been_noted) {
				if (m_current_initialization_expr_str != m_original_initialization_expr_str) {
					return true;
				}
			} else if ("" != m_current_initialization_expr_str)  {
				return true;
			}
			return false;
		}
		CTypeState1& direct_type_state_ref() { return m_indirection_state_stack.m_direct_type_state; }
		const CTypeState1& direct_type_state_ref() const { return m_indirection_state_stack.m_direct_type_state; }

		void set_original_direct_qtype(const clang::QualType& qtype) {
			direct_type_state_ref().set_original_qtype(qtype);
		}
		void set_current_direct_qtype(const clang::QualType& qtype) {
			direct_type_state_ref().set_current_qtype(qtype);
		}
		std::string current_direct_qtype_str() const { return direct_type_state_ref().current_qtype_str(); }
		void set_current_direct_qtype_str(const std::string& new_qtype_str) { direct_type_state_ref().set_current_qtype_str(new_qtype_str); }

		const DeclaratorDecl* m_ddecl_cptr = nullptr;
		CIndirectionStateStack m_indirection_state_stack;

		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_initializer_SR_or_insert_before_point;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_thread_local_specifier_SR_or_insert_before_point;

		std::string m_current_initialization_expr_str;
		bool m_original_initialization_has_been_noted = false;
		std::string m_original_initialization_expr_str;
		bool m_original_source_text_has_been_noted = false;
		std::string m_original_source_text_str;

		bool m_is_a_function = false;
		std::vector<const clang::ParmVarDecl*> m_original_function_parameter_decl_cptrs;
		std::string m_function_return_type_original_source_text_str;

		std::optional<clang::StorageDuration> m_maybe_original_storage_duration;
		std::optional<clang::StorageDuration> m_maybe_current_storage_duration;
	};

	class CDDeclConversionStateMap : public std::unordered_map<const clang::DeclaratorDecl*, CDDeclConversionState> {
	public:
		std::pair<iterator, bool> insert(const clang::DeclaratorDecl& ddecl) {
			std::string variable_name = ddecl.getNameAsString();
			if ("bitlen" == variable_name) {
				int q = 5;
			}
			value_type item(&ddecl, CDDeclConversionState(ddecl));
			return std::unordered_map<const clang::DeclaratorDecl*, CDDeclConversionState>::insert(item);
		}
	};

	class CRecordDeclConversionState {
	public:
		CRecordDeclConversionState(const clang::RecordDecl& recdecl, Rewriter &Rewrite) : m_recdecl_ptr(&recdecl), Rewrite(Rewrite) {
			m_original_source_text_str = Rewrite.getRewrittenText(nice_source_range());
			m_current_text_str = m_original_source_text_str;
		}

		clang::SourceRange source_range() const {
			return m_recdecl_ptr->getSourceRange();
		}
		clang::SourceRange nice_source_range() const {
			return ::nice_source_range(source_range(), Rewrite);
		}

		const clang::RecordDecl *recdecl_ptr() const {
			return m_recdecl_ptr;
		}

		const clang::RecordDecl *m_recdecl_ptr;
		std::string m_original_source_text_str;
		std::string m_current_text_str;
		Rewriter &Rewrite;
	};

	class CRecordDeclConversionStateMap : public std::unordered_map<const clang::RecordDecl*, CRecordDeclConversionState> {
	public:
		std::pair<iterator, bool> insert(const clang::RecordDecl& recdecl, Rewriter &Rewrite) {
			value_type item(&recdecl, CRecordDeclConversionState(recdecl, Rewrite));
			return std::unordered_map<const clang::RecordDecl*, CRecordDeclConversionState>::insert(item);
		}
	};

	class CLocationToRecordDeclMap : public std::map<clang::SourceLocation, const clang::RecordDecl*> {
	public:
		std::pair<iterator, bool> insert(const clang::SourceLocation& SL, const clang::RecordDecl& recdecl) {
			value_type item(SL, &(recdecl));
			return std::map<clang::SourceLocation, const clang::RecordDecl*>::insert(item);
		}
		std::pair<iterator, bool> insert(const clang::RecordDecl& recdecl, Rewriter &Rewrite) {
			auto SR = nice_source_range(recdecl.getSourceRange(), Rewrite);
			if (!(SR.isValid())) {
				return std::pair<iterator, bool>((*this).end(), false);
			}
			auto SL = nice_source_range(recdecl.getSourceRange(), Rewrite).getBegin();
			return insert(SL, recdecl);
		}
	};

	bool is_an_indirect_type(const CDDeclIndirection& ddecl_indirection) {
		return CDDeclConversionState(*(ddecl_indirection.m_ddecl_cptr)).is_an_indirect_type(ddecl_indirection.m_indirection_level);
	}
	bool is_an_indirect_type(const clang::DeclaratorDecl& ddecl) {
		return CDDeclConversionState(ddecl).is_an_indirect_type();
	}

	bool is_an_indirect_type(const QualType& QT, size_t indirection_level = 0) {
		CIndirectionStateStack m_indirection_state_stack;
		auto direct_qtype = populateQTypeIndirectionStack(m_indirection_state_stack, QT);
		return (indirection_level < m_indirection_state_stack.size());
	}

	class CExprTextModifier {
	public:
		virtual ~CExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return input_text;
		}
		virtual std::string species_str() const {
			return "no op";
		}
	};

	class CWrapExprTextModifier : public CExprTextModifier {
	public:
		CWrapExprTextModifier(const std::string& prefix, const std::string& suffix) :
			m_prefix(prefix), m_suffix(suffix) {}
		virtual ~CWrapExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return m_prefix + input_text + m_suffix;
		}
		virtual std::string species_str() const {
			return "wrap";
		}
		std::string m_prefix;
		std::string m_suffix;
	};

	class CNullableAnyRandomAccessIterCastExprTextModifier : public CWrapExprTextModifier {
	public:
		CNullableAnyRandomAccessIterCastExprTextModifier(const clang::QualType& qtype) :
			CWrapExprTextModifier("mse::lh::TLHNullableAnyRandomAccessIterator<" + qtype.getAsString() + " >(", ")"),
			m_qtype(qtype) {}
		virtual ~CNullableAnyRandomAccessIterCastExprTextModifier() {}
		virtual std::string species_str() const {
			return "nullable any random access iter cast";
		}
		clang::QualType m_qtype;
	};

	class CStraightReplacementExprTextModifier : public CExprTextModifier {
	public:
		CStraightReplacementExprTextModifier(const std::string& replacement_text) :
			m_replacement_text(replacement_text) {}
		virtual ~CStraightReplacementExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return m_replacement_text;
		}
		virtual std::string species_str() const {
			return "straight replacement";
		}
		std::string m_replacement_text;
	};

	class CExprTextModifierStack : public std::vector<std::shared_ptr<CExprTextModifier>> {
	public:
	};

	class CExprConversionState {
	public:
		CExprConversionState(const clang::Expr& expr, Rewriter &Rewrite) : m_expr_cptr(&expr), Rewrite(Rewrite) {
			m_original_source_text_str = Rewrite.getRewrittenText(nice_source_range());
			m_current_text_str = m_original_source_text_str;
		}
		virtual ~CExprConversionState() {}
		virtual void update_current_text() {
			m_current_text_str = modified_copy(m_original_source_text_str);
		}

		clang::SourceRange source_range() const {
			return m_expr_cptr->getSourceRange();
		}
		clang::SourceRange nice_source_range() const {
			return ::nice_source_range(source_range(), Rewrite);
		}
		void set_parent_shptr(const std::shared_ptr<CExprConversionState>& parent_shptr) {
			m_parent_shptr = parent_shptr;
		}
		void set_childrens_parent_shptr(const std::shared_ptr<CExprConversionState>& parent_shptr) {
			for (auto& child_shptr : m_children_shptrs) {
				child_shptr->set_parent_shptr(parent_shptr);
			}
		}

		std::string modified_copy(const std::string& input_text) const {
			std::string retval = input_text;
			for (const auto& modifier_shptr_cref : m_expr_text_modifier_stack) {
				retval = (*modifier_shptr_cref).modified_copy(retval, m_expr_cptr);
			}
			return retval;
		}

		CExprTextModifierStack m_expr_text_modifier_stack;

		std::shared_ptr<CExprConversionState> m_parent_shptr;
		std::vector<std::shared_ptr<CExprConversionState>> m_children_shptrs;

		const Expr* m_expr_cptr = nullptr;
		std::string m_original_source_text_str;
		std::string m_current_text_str;
		Rewriter &Rewrite;
	};

	template<class X, class... Args>
	std::shared_ptr<X> make_expr_conversion_state_shared_ptr(Args&&... args) {
		std::shared_ptr<X> retval = std::make_shared<X>(std::forward<Args>(args)...);
		retval->set_childrens_parent_shptr(retval);
		return retval;
	}

	class CConditionalOperatorExprConversionState : public CExprConversionState {
	public:
		CConditionalOperatorExprConversionState(const clang::ConditionalOperator& co_cref, Rewriter &Rewrite) : CExprConversionState(co_cref, Rewrite) {
			auto cond_ptr = co_cref.getCond();
			auto lhs_ptr = co_cref.getLHS();
			auto rhs_ptr = co_cref.getRHS();
			if (cond_ptr && lhs_ptr && rhs_ptr) {
				m_cond_shptr = std::make_shared<CExprConversionState>(*cond_ptr, Rewrite);
				m_children_shptrs.push_back(m_cond_shptr);

				m_lhs_shptr = std::make_shared<CExprConversionState>(*lhs_ptr, Rewrite);
				m_children_shptrs.push_back(m_lhs_shptr);

				m_rhs_shptr = std::make_shared<CExprConversionState>(*rhs_ptr, Rewrite);
				m_children_shptrs.push_back(m_rhs_shptr);
			} else {
				assert(false);
				SCPPT_THROW("");
			}
		}
		const clang::ConditionalOperator& co_cref() const {
			return (*(static_cast<const clang::ConditionalOperator *>(m_expr_cptr)));
		}
		virtual void update_current_text() {
			if (true) {
				/* Do we need to update the kids first? */
				cond_shptr()->update_current_text();
				lhs_shptr()->update_current_text();
				rhs_shptr()->update_current_text();
			}
			std::string updated_text = cond_shptr()->m_current_text_str + " ? "
					+ lhs_shptr()->m_current_text_str + " : " + rhs_shptr()->m_current_text_str;
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}

		std::shared_ptr<CExprConversionState>& cond_shptr() {
			if (3 != m_children_shptrs.size()) {
				assert(false);
				return m_cond_shptr;
			} else {
				return m_children_shptrs[0];
			}
		}
		std::shared_ptr<CExprConversionState>& lhs_shptr() {
			if (3 != m_children_shptrs.size()) {
				assert(false);
				return m_lhs_shptr;
			} else {
				return m_children_shptrs[1];
			}
		}
		std::shared_ptr<CExprConversionState>& rhs_shptr() {
			if (3 != m_children_shptrs.size()) {
				assert(false);
				return m_rhs_shptr;
			} else {
				return m_children_shptrs[2];
			}
		}

	private:
		std::shared_ptr<CExprConversionState> m_cond_shptr;
		std::shared_ptr<CExprConversionState> m_lhs_shptr;
		std::shared_ptr<CExprConversionState> m_rhs_shptr;
	};

	class CAddressofExprConversionState : public CExprConversionState {
	public:
		CAddressofExprConversionState(const clang::UnaryOperator& addrofexpr_cref, Rewriter &Rewrite, const clang::Expr& expr_cref) : CExprConversionState(addrofexpr_cref, Rewrite), m_expr_cptr(&expr_cref) {
			assert(clang::UnaryOperatorKind::UO_AddrOf == addrofexpr_cref.getOpcode());
			m_expr_shptr = std::make_shared<CExprConversionState>(*m_expr_cptr, Rewrite);
			m_children_shptrs.push_back(m_expr_shptr);
		}
		const clang::UnaryOperator& addrofexpr_cref() const {
			return (*(static_cast<const clang::UnaryOperator *>(m_expr_cptr)));
		}
		virtual void update_current_text() {
			if (true) {
				/* Do we need to update the kids first? */
				expr_shptr()->update_current_text();
			}
			std::string updated_text = "&(" + expr_shptr()->m_current_text_str + ")";
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}

		std::shared_ptr<CExprConversionState>& expr_shptr() {
			if (1 != m_children_shptrs.size()) {
				assert(false);
				return m_expr_shptr;
			} else {
				return m_children_shptrs[0];
			}
		}

	private:
		std::shared_ptr<CExprConversionState> m_expr_shptr;
		const clang::Expr* m_expr_cptr = nullptr;
	};

	class CExprConversionStateMap : public std::unordered_map<const clang::Expr*, std::shared_ptr<CExprConversionState>> {
	public:
		typedef std::unordered_map<const clang::Expr*, std::shared_ptr<CExprConversionState>> base_class;
		iterator insert( const std::shared_ptr<CExprConversionState>& cr_shptr ) {
			iterator retval(end());
			if (!cr_shptr) { assert(false); } else {
				decltype((*cr_shptr).m_children_shptrs) mapped_children_shptrs;
				for (auto& child_shptr_ref : (*cr_shptr).m_children_shptrs) {
					//value_type val((*child_shptr_ref).m_expr_cptr, child_shptr_ref);
					//auto res2 = base_class::insert(val);
					auto iter1 = insert(child_shptr_ref);
					if (child_shptr_ref != (*iter1).second) {
						int q = 7;
					}
					mapped_children_shptrs.push_back((*iter1).second);
				}

				value_type val((*cr_shptr).m_expr_cptr, cr_shptr);
				auto res1 = base_class::insert(val);
				retval = res1.first;
				(*((*retval).second)).m_children_shptrs = mapped_children_shptrs;
			}
			return retval;
		}
	};


	/* CTUState holds any "per translation unit" state information we might need to store. */
	class CTUState;

	/* A "replacement action" object represents an "action" that replaces an element in the source
	text (with something else). There are different types (subclasses) of replacement actions
	depending various factors (like what is being replaced, and "why" it is being replaced). A
	"replacement action" object may exist without ever being used to actually execute the
	replacement action. Some replacement actions, when executed, may trigger the execution of
	other replacement actions. */
	class CReplacementAction {
	public:
		CReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR) : m_Rewrite(Rewrite), m_MR(MR) {}
		virtual ~CReplacementAction() {}
		virtual void do_replacement(CTUState& state1) const = 0;

		Rewriter& m_Rewrite;
		const MatchFinder::MatchResult m_MR;
	};

	class CExprTextReplacementAction : public CReplacementAction {
	public:
		CExprTextReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const Expr* EX,
			const std::string& replacement_code) : CReplacementAction(Rewrite, MR), m_EX(EX), m_replacement_code(replacement_code) {}
		virtual ~CExprTextReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const Expr* m_EX = nullptr;
		std::string m_replacement_code;
	};

	class CDDeclIndirectionReplacementAction : public CReplacementAction {
	public:
		CDDeclIndirectionReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR,
				const CDDeclIndirection& ddecl_indirection) : CReplacementAction(Rewrite, MR), m_ddecl_indirection(ddecl_indirection) {}
		virtual ~CDDeclIndirectionReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const = 0;
		virtual const clang::DeclaratorDecl* get_ddecl_cptr() const { return m_ddecl_indirection.m_ddecl_cptr; }
		virtual const CDDeclIndirection& ddecl_indirection_cref() const { return m_ddecl_indirection; }

		//Rewriter& m_Rewrite;
		//const MatchFinder::MatchResult m_MR;
		CDDeclIndirection m_ddecl_indirection;
	};

	class CExprTextDDIReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CExprTextDDIReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const Expr* EX, const std::string& replacement_code) :
					CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection), m_EX(EX), m_replacement_code(replacement_code) {}
		virtual ~CExprTextDDIReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const Expr* m_EX = nullptr;
		std::string m_replacement_code;
	};

	class CSameTypeReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		enum class apply_to_redeclarations_t : bool { no, yes };
		CSameTypeReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection1,
				const CDDeclIndirection& ddecl_indirection2, apply_to_redeclarations_t apply_to_redeclarations = apply_to_redeclarations_t::yes)
				: CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection1), m_ddecl_indirection2(ddecl_indirection2), m_apply_to_redeclarations(apply_to_redeclarations) {}
		CSameTypeReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const clang::DeclaratorDecl& ddecl_cref1,
				const clang::DeclaratorDecl& ddecl_cref2, apply_to_redeclarations_t apply_to_redeclarations = apply_to_redeclarations_t::yes)
				: CDDeclIndirectionReplacementAction(Rewrite, MR, CDDeclIndirection(ddecl_cref1, CDDeclIndirection::no_indirection)), m_ddecl_indirection2(CDDeclIndirection(ddecl_cref2, CDDeclIndirection::no_indirection)), m_apply_to_redeclarations(apply_to_redeclarations) {}
		virtual ~CSameTypeReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		CDDeclIndirection m_ddecl_indirection2;
		apply_to_redeclarations_t m_apply_to_redeclarations = apply_to_redeclarations_t::yes;
	};

	class CAddressofArraySubscriptExprReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CAddressofArraySubscriptExprReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::UnaryOperator& addrofexpr_cref, const clang::ArraySubscriptExpr& arraysubscriptexpr_cref) :
					CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection), m_addrofexpr_cptr(&addrofexpr_cref), m_arraysubscriptexpr_cptr(&arraysubscriptexpr_cref) {}
		virtual ~CAddressofArraySubscriptExprReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::UnaryOperator* m_addrofexpr_cptr = nullptr;
		const clang::ArraySubscriptExpr* m_arraysubscriptexpr_cptr = nullptr;
	};

	class CAddressofSubscriptOperatorCallExprReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CAddressofSubscriptOperatorCallExprReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::UnaryOperator& addrofexpr_cref, const clang::CXXOperatorCallExpr& arraysubscriptexpr_cref) :
					CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection), m_addrofexpr_cptr(&addrofexpr_cref), m_arraysubscriptexpr_cptr(&arraysubscriptexpr_cref) {}
		virtual ~CAddressofSubscriptOperatorCallExprReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::UnaryOperator* m_addrofexpr_cptr = nullptr;
		const clang::CXXOperatorCallExpr* m_arraysubscriptexpr_cptr = nullptr;
	};

	class CArray2ReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		using CDDeclIndirectionReplacementAction::CDDeclIndirectionReplacementAction;
		virtual ~CArray2ReplacementAction() {}
	};

	class CMemsetArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CMemsetArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CallExpr* CE, const std::string& ce_replacement_code) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_CE(CE), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_ce_replacement_code(ce_replacement_code) {}
		virtual ~CMemsetArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CallExpr* m_CE = nullptr;
		//const DeclRefExpr* m_DRE = nullptr;
		//const MemberExpr* m_ME = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_ce_replacement_code;
	};

	class CMemcpyArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CMemcpyArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CallExpr* CE, const std::string& ce_replacement_code) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_CE(CE), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_ce_replacement_code(ce_replacement_code) {}
		virtual ~CMemcpyArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CallExpr* m_CE = nullptr;
		//const DeclRefExpr* m_DRE = nullptr;
		//const MemberExpr* m_ME = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_ce_replacement_code;
	};

	class CAssignmentTargetConstrainsSourceArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CAssignmentTargetConstrainsSourceArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CAssignmentTargetConstrainsSourceArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	class CAssignmentSourceConstrainsTargetArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CAssignmentSourceConstrainsTargetArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CAssignmentSourceConstrainsTargetArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	class CSameTypeArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CSameTypeArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CSameTypeArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	class CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::UnaryOperator& addrofexpr_cref, const clang::ArraySubscriptExpr& arraysubscriptexpr_cref) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_addrofexpr_cptr(&addrofexpr_cref), m_arraysubscriptexpr_cptr(&arraysubscriptexpr_cref) {}
		virtual ~CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::UnaryOperator* m_addrofexpr_cptr = nullptr;
		const clang::ArraySubscriptExpr* m_arraysubscriptexpr_cptr = nullptr;
	};

	class CAssignmentTargetConstrainsAddressofSubscriptOperatorCallExprArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CAssignmentTargetConstrainsAddressofSubscriptOperatorCallExprArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::UnaryOperator& addrofexpr_cref, const clang::CXXOperatorCallExpr& arraysubscriptexpr_cref) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_addrofexpr_cptr(&addrofexpr_cref), m_arraysubscriptexpr_cptr(&arraysubscriptexpr_cref) {}
		virtual ~CAssignmentTargetConstrainsAddressofSubscriptOperatorCallExprArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::UnaryOperator* m_addrofexpr_cptr = nullptr;
		const clang::CXXOperatorCallExpr* m_arraysubscriptexpr_cptr = nullptr;
	};

	class CUpdateIndirectFunctionTypeParamsArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CUpdateIndirectFunctionTypeParamsArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const DeclaratorDecl& indirect_function_ddecl, const clang::CallExpr& call_expr) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_indirect_function_DD(&indirect_function_ddecl), m_CE(&call_expr) {}
		virtual ~CUpdateIndirectFunctionTypeParamsArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::DeclaratorDecl *m_indirect_function_DD = nullptr;
		const clang::CallExpr* m_CE = nullptr;
	};

	class CTargetConstrainsCStyleCastExprArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CTargetConstrainsCStyleCastExprArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::CStyleCastExpr& c_style_cast_expr_cref) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_c_style_cast_expr_cptr(&c_style_cast_expr_cref) {}
		virtual ~CTargetConstrainsCStyleCastExprArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::CStyleCastExpr* m_c_style_cast_expr_cptr = nullptr;
	};

	/*
	A CDDeclIndirection specifies an (indirect) component type (like a pointer or array) of the
	type of a clang::DeclaratorDecl (i.e. a declaration statement) (that may consist of nested
	indirect types (like a pointer to a pointer)). CDDeclIndirectionReplacementActionMap is a map from 
	CDDeclIndirection objects to "replacement action" objects.
	
	Conceptually, this map stores "if-then" statements like: "If this given (native) pointer
	component of the type of this given declaration statement is determined to be being used as an
	array iterator (and is converted accordingly), then this other (native) pointer component of
	the type of this other declaration statement must also be being used as an array iterator (and
	should also be converted accordingly)."

	Such an "if-then" statement might, for example, be the result of an observation that at some
	point in the given source code, the value of one pointer variable is compared to the value of
	another pointer variable, implying that if one is being used as an array iterator, likely the
	other is as well.
	
	This collection of "if-then" statements is used to help ensure that (for example) all the
	native pointers are converted to the appropriate counterpart safe types. */
	class CDDeclIndirectionReplacementActionMap : public std::multimap<CDDeclIndirection, std::shared_ptr<CDDeclIndirectionReplacementAction>> {
	public:
		typedef std::multimap<CDDeclIndirection, std::shared_ptr<CDDeclIndirectionReplacementAction>> base_class;
		iterator insert( const std::shared_ptr<CDDeclIndirectionReplacementAction>& cr_shptr ) {
			iterator retval(end());
			if (!cr_shptr) { assert(false); } else {
				value_type val((*cr_shptr).ddecl_indirection_cref(), cr_shptr);
				retval = base_class::insert(val);
			}
			return retval;
		}

		/* This function executes the action of all the "replacement action" objects associated with
		the given CDDeclIndirection, then removes them from the map. */
		void do_and_dispose_matching_replacements(CTUState& state1, const CDDeclIndirection& ddecl_indirection) {
			/* The base class map may be modified during loop iterations. Maybe. */
			auto iter = base_class::find(ddecl_indirection);
			while (base_class::end() != iter) {
				(*((*iter).second)).do_replacement(state1);
				base_class::erase(iter);

				iter = base_class::find(ddecl_indirection);
			}
		}
	};

	class CDynamicArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		using CArray2ReplacementAction::CArray2ReplacementAction;
		virtual ~CDynamicArray2ReplacementAction() {}
	};

	class CMallocArray2ReplacementAction : public CDynamicArray2ReplacementAction {
	public:
		CMallocArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const BinaryOperator* BO, const std::string& bo_replacement_code) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_BO(BO), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_bo_replacement_code(bo_replacement_code) {}
		virtual ~CMallocArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const BinaryOperator* m_BO = nullptr;
		//const CallExpr* m_CE = nullptr;
		//const DeclRefExpr* m_DRE = nullptr;
		//const MemberExpr* m_ME = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_bo_replacement_code;
	};

	class CInitializerArray2ReplacementAction : public CDynamicArray2ReplacementAction {
	public:
		CInitializerArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const DeclStmt* DS, const std::string& initializer_info_str) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_DS(DS), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_current_initialization_expr_str(initializer_info_str) {}
		virtual ~CInitializerArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const DeclStmt* m_DS = nullptr;
		//const CallExpr* m_CE = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_current_initialization_expr_str;
	};

	class CFreeDynamicArray2ReplacementAction : public CDynamicArray2ReplacementAction {
	public:
		CFreeDynamicArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CallExpr* CE, const std::string& ce_replacement_code) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_CE(CE), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_ce_replacement_code(ce_replacement_code) {}
		virtual ~CFreeDynamicArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CallExpr* m_CE = nullptr;
		//const DeclRefExpr* m_DRE = nullptr;
		//const MemberExpr* m_ME = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_ce_replacement_code;
	};

	class CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction : public CDynamicArray2ReplacementAction {
	public:
		CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	/* This class represents and "enforces" the constraint that the lhs and rhs
	* values of a conditional operator must be the same type. */
	class CConditionalOperatorReconciliation2ReplacementAction : public CDynamicArray2ReplacementAction {
	public:
		CConditionalOperatorReconciliation2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const clang::ConditionalOperator* CO, const DeclaratorDecl* lhs_DD, const DeclaratorDecl* rhs_DD, const DeclaratorDecl* var_DD = nullptr) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_CO(CO), m_lhs_DD(lhs_DD), m_rhs_DD(rhs_DD), m_var_DD(var_DD) {}
		virtual ~CConditionalOperatorReconciliation2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::ConditionalOperator* m_CO = nullptr;
		const DeclaratorDecl* m_lhs_DD = nullptr;
		const DeclaratorDecl* m_rhs_DD = nullptr;
		const DeclaratorDecl* m_var_DD = nullptr;
	};

	class CDynamicArray2ReplacementActionMap : public CDDeclIndirectionReplacementActionMap {
	public:
		iterator insert( const std::shared_ptr<CDynamicArray2ReplacementAction>& cr_shptr ) {
			return CDDeclIndirectionReplacementActionMap::insert(static_cast<std::shared_ptr<CDDeclIndirectionReplacementAction> >(cr_shptr));
		}
	};

	class CArray2ReplacementActionMap : public CDDeclIndirectionReplacementActionMap {
	public:
		/*
		iterator insert( const std::shared_ptr<CArray2ReplacementAction>& cr_shptr ) {
			return CDDeclIndirectionReplacementActionMap::insert(static_cast<std::shared_ptr<CDDeclIndirectionReplacementAction> >(cr_shptr));
		}
		*/
	};

	inline bool is_alpha_numerickish(char ch) {
		if (std::isalnum(ch)) {
			return true;
		}
		if ('_' == ch) {
			return true;
		}
		return false;
	}

	inline bool replace_whole_instances_of_given_string(std::string& text, const std::string_view old_string, const std::string_view new_string) {
		bool changed_flag = false;

		if (text.size() >= old_string.size()) {
			bool preceeding_char_is_alpha_numerickish = false;
			bool succeeding_char_is_alpha_numerickish = false;
			int i = int(text.length()) - int(old_string.length());
			while (0 <= i) {
				if (1 <= i) {
					preceeding_char_is_alpha_numerickish = is_alpha_numerickish(text[i - 1]);
				} else {
					preceeding_char_is_alpha_numerickish = false;
				}
				if (i + int(old_string.length()) < int(text.length())) {
					succeeding_char_is_alpha_numerickish = is_alpha_numerickish(i + int(old_string.length()));
				} else {
					succeeding_char_is_alpha_numerickish = false;
				}
				if (!(preceeding_char_is_alpha_numerickish || succeeding_char_is_alpha_numerickish)) {
					const std::string_view sv1(&(text[i]), old_string.length());
					if (old_string == sv1) {
						text.replace(i, old_string.length(), new_string);
						changed_flag = true;
					}
				}

				i -= 1;
			}
		}
		return changed_flag;
	}

	int source_range_length(const clang::SourceRange& sr)
	{
		/* Note the existence of clang::Rewriter::getRangeSize(). */
		if (!sr.isValid()) {
			return 0;
		}
		SourceLocation SL = sr.getBegin();
		SourceLocation SLE = sr.getEnd();
		int length = 1;
		if (SL == SLE) {
			return 1;
		} else if (SL < SLE) {
			while (SL < SLE) {
				length += 1;
				SL = SL.getLocWithOffset(+1);
			}
		} else {
			assert(SLE < SL);
			while (SLE < SL) {
				length -= 1;
				SL = SL.getLocWithOffset(-1);
			}
		}
		return length;
	}

	/* A CCodeModificationActions object stores an ordered map of source location ranges to function
	objects that modify the source text within the corresponding source range. This container is
	used to (re)order the code modification actions such that any action for a given source range is
	executed before any action for any source range that entirely contains (aka "is a superset of")
	the first source range.
	
	You want to try to ensure this because modifying the contents of a source range could (and often
	does) invalidate any subranges (i.e. contained source ranges). */
	class CCodeModificationActions : public std::map<COrderedSourceRange, std::list<std::function<void(void)>> > {
		public:
		typedef std::map<COrderedSourceRange, std::list<std::function<void(void)>> > base_class;
		using base_class::base_class;

		std::pair<base_class::iterator, bool> add_replacement_action(const COrderedSourceRange& OSR, const std::function<void(void)>& modifier) {
			auto iter1 = base_class::find(OSR);
			if (base_class::end() != iter1) {
				(*iter1).second.push_back(modifier);
				return std::pair<base_class::iterator, bool>(iter1, false);
			} else {
				decltype((*iter1).second) function_list1 { modifier };
				auto res2 = base_class::insert(base_class::value_type(OSR, function_list1));
				assert(base_class::end() != res2.first);
				return std::pair<base_class::iterator, bool>(res2.first, true);
			}
		}
		std::pair<base_class::iterator, bool> add_straight_text_overwrite_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, const std::string& new_text) {
			auto lambda = [this, &Rewrite, OSR, new_text]() {
					if (OSR.isValid()) {
						Rewrite.ReplaceText(OSR, new_text);
						this->m_already_modified_regions.insert(OSR);
					}
				};
			return add_replacement_action(OSR, lambda);
		}
		std::pair<base_class::iterator, bool> add_replacement_of_instances_of_given_string_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, const std::string& old_string, const std::string& new_string) {
			auto lambda = [this, &Rewrite, OSR, old_string, new_string]() {
					if (OSR.isValid()) {
						std::string l_source_text1 = Rewrite.getRewrittenText(OSR);

						if ("" != l_source_text1) {
							auto replacement_code = l_source_text1;
							bool changed_flag = false;

							auto index1 = replacement_code.find(old_string);
							while (std::string::npos != index1) {
								replacement_code.replace(index1, old_string.length(), new_string);
								changed_flag = true;
								index1 = replacement_code.find(old_string, index1 + new_string.length());
							}

							if (changed_flag) {
								//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, OSR, replacement_code);
								auto res2 = Rewrite.ReplaceText(OSR, replacement_code);
								this->m_already_modified_regions.insert(OSR);
							}
						}
					}
				};
			return add_replacement_action(OSR, lambda);
		}
		std::pair<base_class::iterator, bool> add_replacement_of_whole_instances_of_given_string_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, const std::string& old_string, const std::string& new_string) {
			auto lambda = [this, &Rewrite, OSR, old_string, new_string]() {
					if (OSR.isValid()) {
						std::string replacement_code = Rewrite.getRewrittenText(OSR);
						bool changed_flag = replace_whole_instances_of_given_string(replacement_code, old_string, new_string);

						if (changed_flag) {
							//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, OSR, replacement_code);
							auto res2 = Rewrite.ReplaceText(OSR, replacement_code);
							this->m_already_modified_regions.insert(OSR);
						}
					}
				};
			return add_replacement_action(OSR, lambda);
		}

		bool contains(const clang::SourceRange& SR) const {
			for (auto it = (*this).cbegin(); (*this).cend() != it; it++) {
				if (first_is_a_subset_of_second(SR, (*it).first)) {
					return true;
				}
			}
			return false;
		}

		COrderedRegionSet m_already_modified_regions;
	};

	class CTUState : public CCommonTUState1 {
	public:
		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be a dynamic array. */
		CArray2ReplacementActionMap m_dynamic_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be an array (dynamic or otherwise). */
		CArray2ReplacementActionMap m_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be a pointer target. */
		CDDeclIndirectionReplacementActionMap m_pointer_target_contingent_replacement_map;

		/* This container holds information about each item's original type and which
		* type it might be converted to.  */
		CDDeclConversionStateMap m_ddecl_conversion_state_map;

		/* This container maps "clang::SourceLocation"s to "clang::RecordDecl"s at that
		* location. */
		CLocationToRecordDeclMap m_recdecl_map;

		/* This container holds information about all the "clang::RecordDecl"s (i.e.
		* declarations and definitions of structs, classes, etc.). */
		CRecordDeclConversionStateMap m_recdecl_conversion_state_map;

		/* This container holds information about selected expressions' original text and
		* any modifications we might have made.  */
		CExprConversionStateMap m_expr_conversion_state_map;

		/* This container holds, in sorted order, locations of original source code to be modified
		and functions that will execute the modifications. */
		CCodeModificationActions m_pending_code_modification_actions;
	};

	static clang::SourceLocation get_previous_non_whitespace_SL(clang::SourceLocation SL, Rewriter &Rewrite) {
		int offset2 = 0;
		bool exit_flag = false;
		while (!exit_flag) {
			offset2 -= 1;
			clang::SourceRange SR2 { SL.getLocWithOffset(offset2), SL.getLocWithOffset(offset2) };
			auto text2 = Rewrite.getRewrittenText(SR2);
			for (const auto& ch : text2) {
				if (!std::isspace(ch)) {
					exit_flag = true;
					break;
				}
			}
		}
		return SL.getLocWithOffset(offset2);
	}

	static clang::SourceLocation get_next_non_whitespace_SL(clang::SourceLocation SL, Rewriter &Rewrite) {
		int offset2 = 0;
		bool exit_flag = false;
		while (!exit_flag) {
			offset2 += 1;
			clang::SourceRange SR2 { SL.getLocWithOffset(offset2), SL.getLocWithOffset(offset2) };
			auto text2 = Rewrite.getRewrittenText(SR2);
			for (const auto& ch : text2) {
				if (!std::isspace(ch)) {
					exit_flag = true;
					break;
				}
			}
		}
		return SL.getLocWithOffset(offset2);
	}

	clang::SourceRange extended_to_include_west_const_if_any(Rewriter& Rewrite, const clang::SourceRange& SR) {
		/* look for (west) const qualifier */
		/* This implementation is currently a quick hack that doesn't work correctly
		in all cases. */
		auto cq_SR = SR;
		static const std::string const_str = "const";
		auto SL3 = get_previous_non_whitespace_SL(cq_SR.getBegin(), Rewrite);
		clang::SourceRange SR3 { SL3.getLocWithOffset(1 - int(const_str.length())), cq_SR.getBegin().getLocWithOffset(-1) };
		auto text3 = Rewrite.getRewrittenText(SR3);
		if (text3.substr(0, const_str.length()) == const_str) {
			/* (west) const qualifier found */
			/* extend the source range(s) to include the const qualifier */
			cq_SR.setBegin(SR3.getBegin());
			IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(cq_SR);)
		}
		return cq_SR;
	}

	clang::SourceRange extended_to_include_east_const_if_any(Rewriter& Rewrite, const clang::SourceRange& SR) {
		/* look for (west) const qualifier */
		/* This implementation is currently a quick hack that doesn't work correctly
		in all cases. */
		auto cq_SR = SR;
		static const std::string const_str = "const";
		auto SL3 = get_next_non_whitespace_SL(cq_SR.getEnd(), Rewrite);
		clang::SourceRange SR3 { SL3, SL3.getLocWithOffset(int(const_str.length()) - 1) };
		auto text3 = Rewrite.getRewrittenText(SR3);
		if (const_str == text3) {
			/* east const qualifier found */
			/* extend the source range(s) to include the const qualifier */
			cq_SR.setEnd(SR3.getEnd());
			IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(cq_SR);)
		}
		return cq_SR;
	}

	/* Finds all the matching parentheses that enclose the given source range. */
	std::vector<clang::SourceRange> enclosing_parentheses(Rewriter& Rewrite, clang::SourceRange enclosed_SR, std::optional<clang::SourceRange> maybe_bounding_SR) {
		/* Current implementation is a naive hack that generally works for our purposes. */
		std::vector<clang::SourceRange> retval;
		bool exit_flag = false;
		int nested_depth = 0;
		auto SL = enclosed_SR.getBegin().getLocWithOffset(-1);
		std::string text1 = Rewrite.getRewrittenText({ SL, SL });
		while (("(" != text1) || (0 != nested_depth)) {
			if ("(" == text1) {
				nested_depth -= 1;
			} else if (")" == text1) {
				nested_depth += 1;
			}
			SL = SL.getLocWithOffset(-1);
			if ((!SL.isValid())
				|| (maybe_bounding_SR.has_value() && (SL < maybe_bounding_SR.value().getBegin()))) {
				exit_flag = true;
				//break;
				return retval;
			}
			text1 = Rewrite.getRewrittenText({ SL, SL });
		};

		auto SLE = enclosed_SR.getEnd().getLocWithOffset(+1);
		text1 = Rewrite.getRewrittenText({ SLE, SLE });
		while ((")" != text1) || (0 != nested_depth)) {
			if (")" == text1) {
				nested_depth -= 1;
			} else if ("(" == text1) {
				nested_depth += 1;
			}
			SLE = SLE.getLocWithOffset(+1);
			if ((!SLE.isValid())
				|| (maybe_bounding_SR.has_value() && (maybe_bounding_SR.value().getEnd() < SLE))) {
				exit_flag = true;
				//break;
				return retval;
			}
			text1 = Rewrite.getRewrittenText({ SLE, SLE });
		};

		clang::SourceRange paren_SR { SL, SLE };
		retval = enclosing_parentheses(Rewrite, paren_SR, maybe_bounding_SR);
		retval.push_back(paren_SR);
		return retval;
	}

	class CTypeIndirectionPrefixAndSuffixItem {
	public:
		std::string m_prefix_str;
		std::string m_suffix_str;
		std::string m_post_name_suffix_str;
		std::string m_action_species;
		bool m_direct_type_must_be_non_const = false;
		bool m_changed_from_original = false;
		bool m_just_a_native_array = false;
		std::string m_native_array_size_text;
	};

	/* If given a non-null state1_ptr argument, this function will modify the source text to reflect
	any currently indicated changes to the declaration. In any case, it will return an information
	object that can be used to construct a text string of the currently indicated replacement type. */
	static CTypeIndirectionPrefixAndSuffixItem type_indirection_prefix_and_suffix_modifier_and_code_generator(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, bool is_a_function_parameter, CTUState* state1_ptr = nullptr) {
		CTypeIndirectionPrefixAndSuffixItem retval;

#ifndef NDEBUG
		if (indirection_state_stack.m_maybe_DD.has_value()) {
			auto& SM = Rewrite.getSourceMgr();
			auto SR = indirection_state_stack.m_maybe_DD.value()->getSourceRange();
			IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

			if (std::string::npos != debug_source_location_str.find(":689:")) {
				int q = 5;
			}
		}
#endif /*!NDEBUG*/

		auto& direct_type_state_ref = indirection_state_stack.m_direct_type_state;
		bool direct_type_is_char_type = (("char" == direct_type_state_ref.current_qtype_str()) || ("const char" == direct_type_state_ref.current_qtype_str()));
		bool direct_type_is_FILE_type = (("FILE" == direct_type_state_ref.current_qtype_str()) || ("const FILE" == direct_type_state_ref.current_qtype_str()));
		bool direct_type_is_void_type = (("void" == direct_type_state_ref.current_qtype_str()) || ("const void" == direct_type_state_ref.current_qtype_str()));
		bool direct_type_is_function_type = false;
		if (direct_type_state_ref.current_qtype_if_any().has_value()) {
			direct_type_is_function_type = direct_type_state_ref.current_qtype_if_any().value().getTypePtr()->isFunctionType();
		} else {
			assert(false);
		}

		bool changed_from_original = false;
		std::string replacement_code;

		std::string cumulative_prefix_str;
		std::string cumulative_suffix_str;
		std::string cumulative_post_name_suffix_str;

		if (true) {
			//for (size_t i = 0; i < indirection_state_stack.size(); i += 1)
			for (size_t j = 0; j < indirection_state_stack.size(); j += 1)
			{
				size_t i = indirection_state_stack.size() - 1 - j;
				bool l_changed_from_original = (indirection_state_stack[i].current_species() != indirection_state_stack[i].original_species());

				bool is_char_star = false;
				bool is_FILE_star = false;
				bool is_void_star = false;
				bool is_function_pointer = false;
				bool is_last_indirection = (indirection_state_stack.size() == (i+1));
				if (is_last_indirection && (direct_type_is_char_type)) {
					is_char_star = true;
					/* For the moment, we leave "char *" types alone. This will change at some point. */
					l_changed_from_original = ("native pointer" != indirection_state_stack[i].original_species());
				} else if (is_last_indirection && (direct_type_is_FILE_type)) {
					is_FILE_star = true;
					/* For the moment, we leave "FILE *" types alone. This may change at some point. */
					l_changed_from_original = ("native pointer" != indirection_state_stack[i].original_species());
				} else if (is_last_indirection && (direct_type_is_void_type)) {
					is_void_star = true;
					/* For the moment, we leave "void *" types alone. This may change at some point. */
					l_changed_from_original = ("native pointer" != indirection_state_stack[i].original_species());
				} else if (is_last_indirection && direct_type_is_function_type) {
					is_function_pointer = true;
				} else if ((!is_last_indirection) && indirection_state_stack[i+1].current_is_function_type()) {
					is_function_pointer = true;
				}

				bool is_argv = false;
				if (indirection_state_stack.m_maybe_DD.has_value()) {
					const std::string name = indirection_state_stack.m_maybe_DD.value()->getNameAsString();
					if ("argv" == name) {
						const std::string qtype_str = indirection_state_stack.m_maybe_DD.value()->getType().getAsString();
						if ("char **" == qtype_str) {
							is_argv = true;
						}
					}
				}

				std::string prefix_str;
				std::string suffix_str;
				std::string post_name_suffix_str;

				if (indirection_state_stack[i].current_is_function_type()) {
					//suffix_str = indirection_state_stack[i].current_params_string() + suffix_str;
				}

				if ("inferred array" == indirection_state_stack[i].current_species()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else if(is_argv) {
						suffix_str = "* ";
						retval.m_action_species = "char** argv";
					} else {
						if (is_last_indirection) {
							//retval.m_direct_type_must_be_non_const = true;
						}
						if ("FasterAndStricter" == ConvertMode) {
							prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
							suffix_str = "> ";
						} else {
							if (is_a_function_parameter && ScopeTypeFunctionParameters) {
								if ("Dual" == ConvertMode) {
									prefix_str = "MSE_LH_PARAM_ONLY_ARRAY_ITERATOR_TYPE(";
									suffix_str = ") ";
								} else {
									prefix_str = "mse::lh::TXScopeLHNullableAnyRandomAccessIterator<";
									suffix_str = "> ";
								}
							} else {
								if ("Dual" == ConvertMode) {
									prefix_str = "MSE_LH_ARRAY_ITERATOR_TYPE(";
									suffix_str = ") ";
								} else {
									prefix_str = "mse::lh::TLHNullableAnyRandomAccessIterator<";
									suffix_str = "> ";
								}
							}
						}
						retval.m_action_species = "native pointer to MSE_LH_ARRAY_ITERATOR_TYPE";
					}
				} else if ("dynamic array" == indirection_state_stack[i].current_species()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else {
						if (is_last_indirection) {
							retval.m_direct_type_must_be_non_const = true;
						}
						if ("Dual" == ConvertMode) {
							prefix_str = "MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(";
							//prefix_str = "MSE_LH_ARRAY_ITERATOR_TYPE(";
							suffix_str = ") ";
						} else if ("FasterAndStricter" == ConvertMode) {
							//prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
							prefix_str = "mse::lh::TStrongVectorIterator<";
							suffix_str = "> ";
						} else {
							prefix_str = "mse::lh::TStrongVectorIterator<";
							//prefix_str = "mse::lh::TLHNullableAnyRandomAccessIterator<";
							suffix_str = "> ";
						}
						if (is_a_function_parameter) {
							retval.m_action_species = "native pointer parameter to DYNAMIC_ARRAY_ITERATOR_TYPE";
						} else {
							retval.m_action_species = "native pointer to DYNAMIC_ARRAY_ITERATOR_TYPE";
						}
					}
				} else if ("native array" == indirection_state_stack[i].current_species()) {
					std::string size_text = indirection_state_stack[i].m_array_size_expr;

					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char[] for now. At some point we'll replace it with an mse::string or whatever. */
						if (1 == indirection_state_stack.size()) {
							post_name_suffix_str = "[" + size_text + "]";
						} else {
							suffix_str = "[" + size_text + "]";
						}
					} else {
						l_changed_from_original = true;
						if (is_a_function_parameter) {
							if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
								suffix_str = "> ";
							} else {
								if (ScopeTypeFunctionParameters) {
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_PARAM_ONLY_ARRAY_ITERATOR_TYPE(";
										suffix_str = ") ";
									} else {
										prefix_str = "mse::lh::TXScopeLHNullableAnyRandomAccessIterator<";
										suffix_str = "> ";
									}
								} else {
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_ARRAY_ITERATOR_TYPE(";
										suffix_str = ") ";
									} else {
										prefix_str = "mse::lh::TLHNullableAnyRandomAccessIterator<";
										suffix_str = "> ";
									}
								}
							}
							retval.m_action_species = "native array parameter to MSE_LH_ARRAY_ITERATOR_TYPE";
						} else {
							if (is_last_indirection) {
								//retval.m_direct_type_must_be_non_const = true;
							}

							if ("Dual" == ConvertMode) {
								prefix_str = "MSE_LH_FIXED_ARRAY_TYPE_PREFIX(" + size_text + ") ";
								suffix_str = " MSE_LH_FIXED_ARRAY_TYPE_SUFFIX(" + size_text + ") ";
								post_name_suffix_str = " MSE_LH_FIXED_ARRAY_TYPE_POST_NAME_SUFFIX(" + size_text + ")";
							} else if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TXScopeObj<mse::nii_array<";
								suffix_str = ", " + size_text + "> > ";
							} else {
								prefix_str = "mse::lh::TNativeArrayReplacement<";
								suffix_str = ", " + size_text + "> ";
							}

							retval.m_action_species = "native array to MSE_LH_FIXED_ARRAY_TYPE";
							if (1 == indirection_state_stack.size()) {
								retval.m_just_a_native_array = true;
								retval.m_native_array_size_text = size_text;
							}
						}
					}
				} else if ("native pointer" == indirection_state_stack[i].current_species()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else if (is_void_star) {
						/* We'll just leave it as a void* for now. */
						suffix_str = "* ";
						retval.m_action_species = "void*";
					} else if (is_function_pointer) {
						l_changed_from_original = true;
						prefix_str = "mse::mstd::function<";
						suffix_str = "> ";
						retval.m_action_species = "function pointer to mse::mstd::function";
					} else {
						if (true/*for now*/) {
							l_changed_from_original = true;

							if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TXScopeAnyPointer<";
								suffix_str = "> ";
							} else {
								if (is_a_function_parameter && (ScopeTypePointerFunctionParameters || ScopeTypeFunctionParameters)) {
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_PARAM_ONLY_POINTER_TYPE(";
										suffix_str = ") ";
									} else {
										prefix_str = "mse::lh::TXScopeLHNullableAnyPointer<";
										suffix_str = "> ";
									}
								} else {
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_POINTER_TYPE(";
										suffix_str = ") ";
									} else {
										prefix_str = "mse::lh::TLHNullableAnyPointer<";
										suffix_str = "> ";
									}
								}
							}

							retval.m_action_species = "native pointer to TAnyPointer";
						} else {
							//prefix_str = "";
							suffix_str = "* ";
							retval.m_action_species = "native pointer";
						}
					}
				} else if ("malloc target" == indirection_state_stack[i].current_species()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else if (is_void_star) {
						/* We'll just leave it as a void* for now. */
						suffix_str = "* ";
						retval.m_action_species = "void*";
					} else {
						if (false) {
							/* We'll just leaving it as a native pointer for now. Ultimately, this won't be the case. */
							if ("native pointer" == indirection_state_stack[i].original_species()) {
								l_changed_from_original = false;
							}
							//prefix_str = "";
							suffix_str = "* ";
							retval.m_action_species = "malloc target";
						} else {
							if ("Dual" == ConvertMode) {
								//prefix_str = "MSE_LH_ALLOC_POINTER_TYPE(";
								prefix_str = "MSE_LH_POINTER_TYPE(";
								suffix_str = ") ";
							} else if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TRefCountingPointer<";
								suffix_str = "> ";
							} else {
								prefix_str = "mse::lh::TLHNullableAnyPointer<";
								suffix_str = "> ";
							}

							retval.m_action_species = "malloc target to TAnyPointer";
						}
					}
				} else if ("native reference" == indirection_state_stack[i].current_species()) {
					{
						l_changed_from_original = false;
						//prefix_str = "";
						suffix_str = "& ";
						retval.m_action_species = "native reference";
					}
				} else {
					int q = 5;
				}

				auto pointee_maybe_typeLoc = ((i + 1) < indirection_state_stack.size())
					? indirection_state_stack[i+1].m_maybe_typeLoc
					: indirection_state_stack.m_direct_type_state.m_maybe_typeLoc;

				if (indirection_state_stack[i].m_maybe_typeLoc.has_value() && pointee_maybe_typeLoc.has_value()) {
					auto& typeLoc = indirection_state_stack[i].m_maybe_typeLoc.value();
					IF_DEBUG(auto typeLocClass = typeLoc.getTypeLocClass();)

					auto pointee_typeLoc = pointee_maybe_typeLoc.value();
					IF_DEBUG(auto pointee_typeLocClass = pointee_typeLoc.getTypeLocClass();)

					//auto SR = typeLoc.getSourceRange();
					bool needs_processing = false;
					if (!(indirection_state_stack[i].m_maybe_source_range.has_value())) {
						needs_processing = true;
						indirection_state_stack[i].m_maybe_source_range = nice_source_range(typeLoc.getSourceRange(), Rewrite);
					}
					auto& SR = indirection_state_stack[i].m_maybe_source_range.value();

					auto& maybe_lexical_suffix_SR = indirection_state_stack[i].m_maybe_suffix_source_range;
					auto& maybe_prefix_SR = indirection_state_stack[i].m_maybe_prefix_source_range;

					if (needs_processing && SR.isValid()) {
						std::string old_text = Rewrite.getRewrittenText(SR);

						std::optional<clang::SourceLocation> maybe_name_SL;
						if (indirection_state_stack.m_maybe_DD.has_value()) {
							auto VD = dyn_cast<const clang::VarDecl>(indirection_state_stack.m_maybe_DD.value());
							auto FD = dyn_cast<const clang::FieldDecl>(indirection_state_stack.m_maybe_DD.value());
							if (VD || FD) {
								/* This declaration has a variable/field name. */
								auto name_SL = VD ? VD->getLocation() : FD->getLocation();
								auto name_SR1 = nice_source_range(clang::SourceRange{ name_SL, name_SL }, Rewrite);
								std::string name_text = Rewrite.getRewrittenText(name_SR1);
								int offset1 = name_text.length() - 1;
								if (0 > offset1) {
									offset1 = 0;
								}
								auto name_SR = clang::SourceRange{ name_SL, name_SL.getLocWithOffset(offset1) };
								name_text = Rewrite.getRewrittenText(name_SR);
								if (!(SR.getEnd() < name_SR.getBegin())) {
									/* The given source range of the type (currently) contains the variable/field name.
									This is not the case for most declarations (such as with 'int a;'), but is for native
									array declarations ('int a[3];') and function pointer declarations ('int (*a)(int)'). */
									auto enclosing_parentheses1 = enclosing_parentheses(Rewrite, name_SR, SR);
									if (1 <= enclosing_parentheses1.size()) {
										/* The variable/field name is enclosed by parentheses (such as with the function
										pointer declaration 'int (*a)(int)'). */
										IF_DEBUG(std::string enclosing_parentheses_text = Rewrite.getRewrittenText(enclosing_parentheses1.back());)
										auto new_SLE = enclosing_parentheses1.back().getBegin().getLocWithOffset(-1);
										if (!(new_SLE < SR.getBegin())) {
											/* Here we truncate the source range of the type to exclude the variable/field
											name and any enclosing parentheses. */
											SR.setEnd(new_SLE);
											old_text = Rewrite.getRewrittenText(SR);
										} else {
											int q = 3;
										}
										if (0 == i) {
											if (ConvertToSCPP) {
												/* This is the last "indirection" to be processed. Now we're going
												to "blank out"/erase any parentheses and contained items ((pointer)
												asterisks, bracketed array size expressions, const qualifiers, etc)
												that are enclosing the variable/field name. Their semantics should
												now be expressed in the new converted types. */
												auto SL2 = enclosing_parentheses1.back().getBegin();
												while (SL2 < name_SR.getBegin()) {
													/* "Blanking out"/erasing enclosing items to the left of the
													variable/field name. */
													std::string text1 = Rewrite.getRewrittenText(clang::SourceRange{ SL2, SL2 });
													for (auto& ch : text1) {
														ch = ' ';
													}
													Rewrite.ReplaceText(clang::SourceRange{ SL2, SL2 }, text1);
													SL2 = SL2.getLocWithOffset(+1);
												}
												SL2 = name_SR.getEnd().getLocWithOffset(+1);
												while (!(enclosing_parentheses1.back().getEnd() < SL2)) {
													/* "Blanking out"/erasing enclosing items to the right of the
													variable/field name. */
													std::string text1 = Rewrite.getRewrittenText(clang::SourceRange{ SL2, SL2 });
													for (auto& ch : text1) {
														ch = ' ';
													}
													Rewrite.ReplaceText(clang::SourceRange{ SL2, SL2 }, text1);
													SL2 = SL2.getLocWithOffset(+1);
												}
												IF_DEBUG(enclosing_parentheses_text = Rewrite.getRewrittenText(enclosing_parentheses1.back());)
											}
										}
									} else {
										auto new_SLE = name_SR.getBegin().getLocWithOffset(-1);
										if (!(new_SLE < SR.getBegin())) {
											/* Here we truncate the source range of the type to exclude the variable/field
											name. */
											SR.setEnd(new_SLE);
											old_text = Rewrite.getRewrittenText(SR);
										} else {
											int q = 3;
										}
									}
								}
							}
						}

						std::string old_pointee_text = "[unset]";
						std::string old_suffix_text = "[unset]";
						std::string old_prefix_text = "[unset]";

						bool pointee_needs_processing = false;
						auto& pointee_maybe_stored_source_range = ((i + 1) < indirection_state_stack.size())
							? indirection_state_stack[i+1].m_maybe_source_range
							: indirection_state_stack.m_direct_type_state.m_maybe_source_range;
						if (!(pointee_maybe_stored_source_range.has_value())) {
							pointee_needs_processing = true;
							pointee_maybe_stored_source_range = nice_source_range(pointee_typeLoc.getSourceRange(), Rewrite);
						}

						if (pointee_maybe_stored_source_range.value().isValid()) {
							auto& pointee_SR = pointee_maybe_stored_source_range.value();
							old_pointee_text = Rewrite.getRewrittenText(pointee_SR);

							if (pointee_needs_processing) {
								if (pointee_SR.getEnd().isMacroID()) {
									/* The pointee appears to be (or end with) a macro. */
									/* It could be a function macro, but at this point pointee_SR (and
									old_pointee_text) will only encompass the macro name, not the arguments
									(or parentheses), if any. We need to obtain the range that includes the
									arguments. */
									/* super hack: We'll just search for the last closing parenthesis (if any)
									and assume it is the closing parentheses of the macro arguments. */
									auto close_paranthesis_pos = old_text.find_last_of(')');
									if (std::string::npos != close_paranthesis_pos) {
										auto new_pointee_SLE = SR.getBegin().getLocWithOffset(close_paranthesis_pos);
										if (pointee_SR.getEnd() < new_pointee_SLE) {
											pointee_SR.setEnd(new_pointee_SLE);
											old_pointee_text = Rewrite.getRewrittenText(pointee_SR);
										}
									}
								}
								if (SR.getEnd() < pointee_SR.getEnd()) {
									pointee_SR.setEnd(SR.getEnd());
									old_pointee_text = Rewrite.getRewrittenText(pointee_SR);
								}
							}

							bool pointee_including_any_const_qualifier_needs_processing = false;
							auto& pointee_maybe_stored_source_range_including_any_const_qualifier = ((i + 1) < indirection_state_stack.size())
								? indirection_state_stack[i+1].m_maybe_source_range_including_any_const_qualifier
								: indirection_state_stack.m_direct_type_state.m_maybe_source_range_including_any_const_qualifier;
							if (!(pointee_maybe_stored_source_range_including_any_const_qualifier.has_value())) {
								pointee_including_any_const_qualifier_needs_processing = true;
								pointee_maybe_stored_source_range_including_any_const_qualifier = pointee_SR;
							}
							auto& cq_pointee_SR = pointee_maybe_stored_source_range_including_any_const_qualifier.value();

							if (pointee_including_any_const_qualifier_needs_processing) {
								cq_pointee_SR = extended_to_include_west_const_if_any(Rewrite, cq_pointee_SR);
								old_pointee_text = Rewrite.getRewrittenText(cq_pointee_SR);
								if (cq_pointee_SR.getBegin() < SR.getBegin()) {
									SR.setBegin(cq_pointee_SR.getBegin());
									old_text = Rewrite.getRewrittenText(SR);
								}
							}

							auto arrayTypeLoc = typeLoc.getAsAdjusted<clang::ArrayTypeLoc>();
							if (arrayTypeLoc) {
								bool SR_end_adjusted = false;
								if (indirection_state_stack.m_maybe_DD.has_value()) {
									auto VD = dyn_cast<const clang::VarDecl>(indirection_state_stack.m_maybe_DD.value());
									auto FD = dyn_cast<const clang::FieldDecl>(indirection_state_stack.m_maybe_DD.value());
									if (VD || FD) {
										/* In the case of native array variable declarations (like 'int array_name[5];')
										we're choosing to adjust the end of the type's source range to the end of the part
										before the start of the array variable name (rather than at the right bracket). */
										auto name_SL = VD ? VD->getLocation() : FD->getLocation();
										auto new_SLE = cq_pointee_SR.getEnd();
										if (name_SL < new_SLE) {
											/* We're trying to avoid this situation. If it does occur, we don't really
											handle it very well. */
											new_SLE = name_SL.getLocWithOffset(-2);
										}
										if (!(new_SLE < SR.getBegin())) {
											SR.setEnd(new_SLE);
											SR_end_adjusted = true;
										} else {
											int q = 3;
										}
									}
								}
								if (!SR_end_adjusted) {
									auto brackets_SR = nice_source_range(arrayTypeLoc.getBracketsRange(), Rewrite);
									if (brackets_SR.isValid()) {
										auto new_SLE = brackets_SR.getBegin().getLocWithOffset(-1);
										if (!(new_SLE < SR.getBegin())) {
											SR.setEnd(new_SLE);
										} else {
											int q = 3;
										}
									}
								}
							}

							int lexical_end_offset1 = int(old_text.length()) - 1;
							if (0 > lexical_end_offset1) {
								lexical_end_offset1 = 0;
							}
							clang::SourceRange lexical_SR { SR.getBegin(), SR.getBegin().getLocWithOffset(lexical_end_offset1) };
							std::string old_text2 = Rewrite.getRewrittenText(lexical_SR);

							int lexical_pointee_end_offset1 = int(old_pointee_text.length()) - 1;
							if (0 > lexical_pointee_end_offset1) {
								lexical_pointee_end_offset1 = 0;
							}
							clang::SourceRange lexical_cq_pointee_SR { cq_pointee_SR.getBegin(), cq_pointee_SR.getBegin().getLocWithOffset(lexical_pointee_end_offset1) };
							std::string old_pointee_text2 = Rewrite.getRewrittenText(lexical_cq_pointee_SR);
							clang::SourceRange lexical_suffix_SR { lexical_cq_pointee_SR.getEnd().getLocWithOffset(+1), lexical_SR.getEnd() };
							bool pointee_east_const_qualifier_found = false;
							if (lexical_suffix_SR.isValid() && (!(lexical_suffix_SR.getEnd() < lexical_suffix_SR.getBegin()))) {
								old_suffix_text = Rewrite.getRewrittenText(lexical_suffix_SR);

								if (pointee_including_any_const_qualifier_needs_processing
									&& pointee_typeLoc.getType().isConstQualified()) {

									const auto tmp_SR = extended_to_include_east_const_if_any(Rewrite, lexical_suffix_SR);
									if (!(tmp_SR == lexical_suffix_SR)) {
										pointee_east_const_qualifier_found = true;
										lexical_suffix_SR = tmp_SR;
									}
								}
							}
							if (lexical_suffix_SR.isValid() && (!(lexical_suffix_SR.getEnd() < lexical_suffix_SR.getBegin()))) {
								maybe_lexical_suffix_SR = lexical_suffix_SR;
							}

							clang::SourceRange prefix_SR { SR.getBegin(), cq_pointee_SR.getBegin().getLocWithOffset(-1) };
							if (prefix_SR.isValid() && (!(prefix_SR.getEnd() < prefix_SR.getBegin()))) {
								maybe_prefix_SR = prefix_SR;
							}
						}
						if (!is_last_indirection) {
							int q = 5;
						}

					}

					if (SR.isValid()) {

						if ("native array" == indirection_state_stack[i].current_species()) {
							/* Currently, in the transformation of native array declarations, we blank out
							the original bracketed size expression and add the size expression to the suffix
							of the new type (essentially moving the array size expression from the right
							side of the array variable name to the left side). This means that after the
							transformation is applied, the original clang::SourceRange of the bracketed size
							expression will no longer indicate the location of the size expression.
							Alternatively, we could keep the array size expression in place and instead move
							the array variable name (to the right side of the size expression). So far we
							haven't had cause to switch to such an alternative, but conceivably may need to
							do so in the future. */

							if (!indirection_state_stack[i].m_array_size_expr_read_from_source_text) {
								auto ArrayLoc = typeLoc.getAs<clang::ArrayTypeLoc>();
								if (ArrayLoc) {
									auto brackets_SR = ArrayLoc.getBracketsRange();
									auto size_expr_SR = clang::SourceRange{ brackets_SR.getBegin().getLocWithOffset(+1), brackets_SR.getEnd().getLocWithOffset(-1) };
									if (size_expr_SR.isValid() && (!(size_expr_SR.getEnd() < size_expr_SR.getBegin()))) {
										std::string size_expr_text = Rewrite.getRewrittenText(size_expr_SR);
										if ("" != size_expr_text) {
											indirection_state_stack[i].m_array_size_expr = size_expr_text;
											indirection_state_stack[i].m_array_size_expr_read_from_source_text = true;

											/* We've updated the array size expression text (from the literal number derived from
											the type to the expression read from the source text), so here we're updating the prefix
											and sufix strings accordingly. */

											std::string size_text = indirection_state_stack[i].m_array_size_expr;

											if (is_char_star) {
											} else if (is_FILE_star) {
											} else {
												if (is_a_function_parameter) {
												} else {
													if (is_last_indirection) {
														//retval.m_direct_type_must_be_non_const = true;
													}

													if ("Dual" == ConvertMode) {
														prefix_str = "MSE_LH_FIXED_ARRAY_TYPE_PREFIX(" + size_text + ") ";
														suffix_str = " MSE_LH_FIXED_ARRAY_TYPE_SUFFIX(" + size_text + ") ";
														post_name_suffix_str = " MSE_LH_FIXED_ARRAY_TYPE_POST_NAME_SUFFIX(" + size_text + ")";
													} else if ("FasterAndStricter" == ConvertMode) {
														prefix_str = "mse::TXScopeObj<mse::nii_array<";
														suffix_str = ", " + size_text + "> > ";
													} else {
														prefix_str = "mse::lh::TNativeArrayReplacement<";
														suffix_str = ", " + size_text + "> ";
													}

													retval.m_action_species = "native array to MSE_LH_FIXED_ARRAY_TYPE";
													if (1 == indirection_state_stack.size()) {
														retval.m_just_a_native_array = true;
														retval.m_native_array_size_text = size_text;
													}
												}
											}

										} else {
											int q = 3;
										}
									} else {
										int q = 3;
									}
								} else {
									int q = 3;
								}
							}
						} else if ("native pointer" == indirection_state_stack[i].current_species()) {
							if (is_function_pointer) {
								auto functionProtoTypeLoc = pointee_typeLoc.getAsAdjusted<clang::FunctionProtoTypeLoc>();
								if (true && functionProtoTypeLoc) {
									auto parens_SR = functionProtoTypeLoc.getParensRange();
									if (parens_SR.isValid()) {
										std::string parens_text = Rewrite.getRewrittenText(parens_SR);

										auto& pointee_params_current_str = ((i + 1) < indirection_state_stack.size())
											? indirection_state_stack[i+1].m_params_current_str
											: indirection_state_stack.m_direct_type_state.m_current_params_str;

										if ("" == pointee_params_current_str) {
											pointee_params_current_str = parens_text;
											//pointee_params_current_str = "[params placeholder]";

											if (ConvertToSCPP) {
												/* We've stored the function parameters as a string. Now we're going
												to "blank out"/erase the original source text of the parameters. */
												auto SL2 = parens_SR.getBegin();
												while (!(parens_SR.getEnd() < SL2)) {
													std::string text1 = Rewrite.getRewrittenText(clang::SourceRange{ SL2, SL2 });
													for (auto& ch : text1) {
														ch = ' ';
													}
													Rewrite.ReplaceText(clang::SourceRange{ SL2, SL2 }, text1);
													SL2 = SL2.getLocWithOffset(+1);
												}
												IF_DEBUG(std::string parens_text2 = Rewrite.getRewrittenText(parens_SR));
											}
										}
										//suffix_str = pointee_params_current_str + suffix_str;
									}
								}
							}
						}
					}

#define REGOBJ_TEST1_FLAG false

					if (!is_argv) {
						if ((REGOBJ_TEST1_FLAG || ("pointer target" == indirection_state_stack[i].current_pointer_target_state()))
							&& (!string_begins_with(prefix_str, "mse::TRegisteredObj<"))
							&& (!string_begins_with(prefix_str, "MSE_LH_ADDRESSABLE_TYPE("))
							&& (!string_begins_with(prefix_str, "const mse::TRegisteredObj<"))
							&& (!string_begins_with(prefix_str, "const MSE_LH_ADDRESSABLE_TYPE("))
							&& ("" == post_name_suffix_str)) {
							if ("native reference" != indirection_state_stack[i].current_species()) {
								if ("Dual" == ConvertMode) {
									prefix_str = "MSE_LH_ADDRESSABLE_TYPE(" + prefix_str;
									suffix_str = suffix_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
								} else {
									prefix_str = "mse::TRegisteredObj<" + prefix_str;
									suffix_str = suffix_str + " >";
								}
							} else {
								int q = 5;
							}
						}
					}

					if (SR.isValid()) {
						if (state1_ptr) {
							auto& state1 = *state1_ptr;
							suffix_str = indirection_state_stack[i].m_params_current_str + suffix_str;
							if ("" != suffix_str) {
								indirection_state_stack[i].m_suffix_str = suffix_str;
								if (maybe_lexical_suffix_SR.has_value()) {
									//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, maybe_lexical_suffix_SR.value(), suffix_str);
									indirection_state_stack[i].m_suffix_SR_or_insert_before_point = maybe_lexical_suffix_SR.value();
								} else {
									indirection_state_stack[i].m_suffix_SR_or_insert_before_point = SR.getEnd().getLocWithOffset(+1);
								}

								indirection_state_stack[i].m_prefix_str = prefix_str;
								if (maybe_prefix_SR.has_value()) {
									//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, maybe_prefix_SR.value(), prefix_str);
									indirection_state_stack[i].m_prefix_SR_or_insert_before_point = maybe_prefix_SR.value();
								} else {
									indirection_state_stack[i].m_prefix_SR_or_insert_before_point = SR.getBegin();
								}

								auto arrayTypeLoc = typeLoc.getAsAdjusted<clang::ArrayTypeLoc>();
								if (arrayTypeLoc) {
									auto brackets_SR = nice_source_range(arrayTypeLoc.getBracketsRange(), Rewrite);
									if (brackets_SR.isValid()) {
										state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, brackets_SR, post_name_suffix_str);
									}
								} else {
									/*
									auto funcTypeLoc = typeLoc.getAsAdjusted<clang::FunctionTypeLoc>();
									if (funcTypeLoc) {
										auto parens_SR = nice_source_range(funcTypeLoc.getParensRange(), Rewrite);
										if (parens_SR.isValid()) {
											state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, parens_SR, post_name_suffix_str);
										}
									}
									*/
								}
							}
						}
					}

				}

				cumulative_prefix_str = prefix_str + cumulative_prefix_str;
				cumulative_suffix_str = cumulative_suffix_str + suffix_str;
				cumulative_post_name_suffix_str = cumulative_post_name_suffix_str + post_name_suffix_str;

				changed_from_original |= l_changed_from_original;
			}
		}
		retval.m_prefix_str = cumulative_prefix_str;
		retval.m_suffix_str = cumulative_suffix_str;
		retval.m_post_name_suffix_str = cumulative_post_name_suffix_str;
		retval.m_changed_from_original = changed_from_original;

		return retval;
	}

	/* This function will modify the source text to reflect any currently indicated changes to the
	declaration. */
	static CTypeIndirectionPrefixAndSuffixItem type_indirection_prefix_and_suffix_modifier(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, CTUState* state1_ptr, bool is_a_function_parameter) {

			return type_indirection_prefix_and_suffix_modifier_and_code_generator(indirection_state_stack, Rewrite, is_a_function_parameter, state1_ptr);
	}

	/* This function will return an information object that can be used to construct a text string 
	f the currently indicated replacement type. */
	static CTypeIndirectionPrefixAndSuffixItem generate_type_indirection_prefix_and_suffix(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, bool is_a_function_parameter) {

			return type_indirection_prefix_and_suffix_modifier_and_code_generator(indirection_state_stack, Rewrite, is_a_function_parameter);
	}

	class CDeclarationReplacementCodeItem {
	public:
		std::string m_replacement_code;
		std::string m_replacement_type_str;
		std::string m_action_species;
		bool m_changed_from_original = false;
		bool m_individual_from_compound_declaration = false;
	};

	static CDeclarationReplacementCodeItem declaration_modifier_helper1(const DeclaratorDecl* DD,
			Rewriter &Rewrite, CTUState* state1_ptr, CDDeclConversionStateMap& ddecl_conversion_state_map, std::string options_str = "") {
		CDeclarationReplacementCodeItem retval;

		if (!DD) {
			return retval;
		}
		auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
		if (!(decl_source_range.isValid())) {
			return retval;
		}

		auto res1 = ddecl_conversion_state_map.insert(*DD);
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;

		QualType QT = DD->getType();

		const clang::FunctionDecl* FND = nullptr;
		bool type_is_function_type = false;
		assert(DD->isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if (QT->isFunctionType()) {
			FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				type_is_function_type = true;
				QT = FND->getReturnType();
			}
		}

		std::string variable_name = DD->getNameAsString();
		std::string identifier_name_str;
		auto pIdentifier = DD->getIdentifier();
		if (pIdentifier) {
			identifier_name_str = pIdentifier->getName();
		}
		if ("" == variable_name) {
			int q = 7;
		} else if ("lodepng_chunk_data_const" == variable_name) {
			int q = 5;
		}

		clang::StorageClass storage_class = clang::StorageClass::SC_None;
		bool is_extern = false;
		clang::StorageDuration storage_duration = ddcs_ref.m_maybe_current_storage_duration.value_or(clang::StorageDuration::SD_Automatic);
		bool is_static = false;
		bool is_a_function_parameter = false;
		bool is_member = false;
		bool is_vardecl = false;
		std::string initialization_expr_str = ddcs_ref.m_current_initialization_expr_str;
		bool is_function = DD->isFunctionOrFunctionTemplate();

		auto VD = dyn_cast<const clang::VarDecl>(DD);
		auto FD = dyn_cast<const clang::FieldDecl>(DD);
		if (VD) {
			is_vardecl = true;
			storage_class = VD->getStorageClass();
			is_extern = (clang::StorageClass::SC_Extern == storage_class);

			ddcs_ref.m_maybe_original_storage_duration = VD->getStorageDuration();
			if (!ddcs_ref.m_maybe_current_storage_duration.has_value()) { ddcs_ref.m_maybe_current_storage_duration = ddcs_ref.m_maybe_original_storage_duration; }
			storage_duration = ddcs_ref.m_maybe_current_storage_duration.value();

			is_static = (clang::StorageClass::SC_Static == storage_class);
			if ((clang::StorageDuration::SD_Static == storage_duration) && (!is_static)) {
				int q = 5;
			}
			is_a_function_parameter = (VD->isLocalVarDeclOrParm() && (!VD->isLocalVarDecl()));

			if (!ddcs_ref.m_original_initialization_has_been_noted) {
				if (VD->hasInit()) {
					auto pInitExpr = VD->getInit();
					if (pInitExpr) {
						auto init_expr_source_range = nice_source_range(pInitExpr->getSourceRange(), Rewrite);
						if (init_expr_source_range.isValid()) {
							initialization_expr_str = Rewrite.getRewrittenText(init_expr_source_range);
							if (variable_name == initialization_expr_str) {
								/* We encountered a weird bug where the initialization expression sometimes
								* was indicated as being present and the source range set to the variable name
								* when actually no initialization expression was present in the original source. */
								initialization_expr_str = "";
							} else {
								ddcs_ref.m_initializer_SR_or_insert_before_point = init_expr_source_range;
								ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
								ddcs_ref.m_current_initialization_expr_str = initialization_expr_str;
							}
						} else {
							int q = 3;
						}
					} else {
						int q = 3;
					}
				}
				ddcs_ref.m_original_initialization_has_been_noted = true;
			}
		} else if (FD) {
			{
				is_member = true;

				if (!ddcs_ref.m_original_initialization_has_been_noted) {
					if (FD->hasInClassInitializer()) {
						auto pInitExpr = FD->getInClassInitializer();
						if (pInitExpr) {
							auto init_expr_source_range = nice_source_range(pInitExpr->getSourceRange(), Rewrite);
							if (init_expr_source_range.isValid()) {
								initialization_expr_str = Rewrite.getRewrittenText(init_expr_source_range);
								if (variable_name == initialization_expr_str) {
									/* We encountered a weird bug where the initialization expression sometimes
									* was indicated as being present and the source range set to the variable name
									* when actually no initialization expression was present in the original source. */
									initialization_expr_str = "";
								} else {
									ddcs_ref.m_initializer_SR_or_insert_before_point = init_expr_source_range;
									ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
									ddcs_ref.m_current_initialization_expr_str = initialization_expr_str;
								}
							} else {
								int q = 3;
							}
						} else {
							int q = 3;
						}
					}
					ddcs_ref.m_original_initialization_has_been_noted = true;
				}
			}
		}
		ddcs_ref.m_original_initialization_has_been_noted = true;

		const clang::Type* TP = QT.getTypePtr();
		auto qtype_str = QT.getAsString();
		auto direct_qtype_str = ddcs_ref.current_direct_qtype_str();

		auto non_const_direct_qtype_str = direct_qtype_str;
		if (ddcs_ref.direct_type_state_ref().current_qtype_is_current()) {
			assert(ddcs_ref.direct_type_state_ref().current_qtype_if_any().has_value());
			auto current_direct_qtype = ddcs_ref.direct_type_state_ref().current_qtype_if_any().value();
			auto non_const_direct_qtype = current_direct_qtype;
			non_const_direct_qtype.removeLocalConst();
			non_const_direct_qtype_str = non_const_direct_qtype.getAsString();
		} else {
			/* hack alert */
			static const std::string const_space_str = "const ";
			if (string_begins_with(non_const_direct_qtype_str, const_space_str)) {
				non_const_direct_qtype_str = non_const_direct_qtype_str.substr(const_space_str.size());
			}
		}

		if ("_Bool" == direct_qtype_str) {
			direct_qtype_str = "bool";
		} else if ("const _Bool" == direct_qtype_str) {
			direct_qtype_str = "const bool";
		}
		if ("_Bool" == non_const_direct_qtype_str) {
			non_const_direct_qtype_str = "bool";
		}

		if (!(ddcs_ref.m_original_source_text_has_been_noted)) {
			ddcs_ref.m_original_source_text_str = Rewrite.getRewrittenText(decl_source_range);
			if (FND) {
				assert(type_is_function_type);
				ddcs_ref.m_is_a_function = true;
				for (size_t i = 0; i < FND->getNumParams(); i+=1) {
					ddcs_ref.m_original_function_parameter_decl_cptrs.push_back(FND->getParamDecl(i));
				}
				auto return_type_source_range = nice_source_range(FND->getReturnTypeSourceRange(), Rewrite);
				if (!(return_type_source_range.isValid())) {
					return retval;
				}
				ddcs_ref.m_function_return_type_original_source_text_str = Rewrite.getRewrittenText(return_type_source_range);
			}
			ddcs_ref.m_original_source_text_has_been_noted = true;
		}

		bool changed_from_original = false;
		std::string replacement_code;
		std::string replacement_type_str;
		std::string prefix_str;
		std::string suffix_str;
		std::string post_name_suffix_str;

		auto res4 = type_indirection_prefix_and_suffix_modifier(ddcs_ref.m_indirection_state_stack,
				Rewrite, state1_ptr, is_a_function_parameter);

		retval.m_action_species = res4.m_action_species;

		if (res4.m_direct_type_must_be_non_const) {
			direct_qtype_str = non_const_direct_qtype_str;
		}
		prefix_str = res4.m_prefix_str;
		suffix_str = res4.m_suffix_str;
		post_name_suffix_str = res4.m_post_name_suffix_str;

		if ((!FND) && ("" == post_name_suffix_str)) {
			bool is_char_type = false;
			bool is_FILE_type = false;
			bool is_void_type = false;
			if (("char" == direct_qtype_str) || ("const char" == direct_qtype_str)) {
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					is_char_type = true;
				}
			} else if (("FILE" == direct_qtype_str) || ("const FILE" == direct_qtype_str)) {
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					is_FILE_type = true;
				}
			} else if (("void" == direct_qtype_str) || ("const void" == direct_qtype_str)) {
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					is_void_type = true;
				}
			}
			if ((!is_char_type) && (!is_FILE_type) && (!is_void_type)) {
				if ((REGOBJ_TEST1_FLAG || ("pointer target" == ddcs_ref.direct_type_state_ref().current_pointer_target_state()))
					&& (!string_begins_with(ddcs_ref.current_direct_qtype_str(), "mse::TRegisteredObj<"))
					&& (!string_begins_with(ddcs_ref.current_direct_qtype_str(), "MSE_LH_ADDRESSABLE_TYPE("))
					&& (!string_begins_with(ddcs_ref.current_direct_qtype_str(), "const mse::TRegisteredObj<"))
					&& (!string_begins_with(ddcs_ref.current_direct_qtype_str(), "const MSE_LH_ADDRESSABLE_TYPE("))
					) {

					if ("Dual" == ConvertMode) {
						direct_qtype_str = "MSE_LH_ADDRESSABLE_TYPE(" + non_const_direct_qtype_str + ")";
						if (ddcs_ref.direct_type_state_ref().is_const()) {
							direct_qtype_str = "const " + direct_qtype_str;
						}
						ddcs_ref.set_current_direct_qtype_str(direct_qtype_str);
					} else if ("FasterAndStricter" == ConvertMode) {
					} else {
						direct_qtype_str = "mse::TRegisteredObj<" + non_const_direct_qtype_str + " >";
						if (ddcs_ref.direct_type_state_ref().is_const()) {
							direct_qtype_str = "const " + direct_qtype_str;
						}
						ddcs_ref.set_current_direct_qtype_str(direct_qtype_str);
					}
				} else {
					int q = 5;
				}
			}
		}

		bool discard_initializer_option_flag = (std::string::npos != options_str.find("[discard-initializer]"));
		std::string initializer_append_str;
		if ((!discard_initializer_option_flag) && ("" != initialization_expr_str)) {
			initializer_append_str = " = " + initialization_expr_str;
		}

		bool individual_from_compound_declaration = false;
		//if (("" != prefix_str) || ("" != suffix_str)/* || ("" != post_name_suffix_str)*/)
		if (res4.m_changed_from_original || ddcs_ref.direct_qtype_has_been_changed()) {
			changed_from_original = true;
		} else if (ddcs_ref.initializer_has_been_changed() || (discard_initializer_option_flag)) {
			changed_from_original = true;
		} else if (true && (2 <= IndividualDeclaratorDecls(DD, Rewrite).size())) {
			/* There is more than one declaration in the declaration statement. We split
			* them so that each has their own separate declaration statement. This counts
			* as a change from the original source code. */
			individual_from_compound_declaration = true;
		}

		if (FND) {
			assert(type_is_function_type);
			if (changed_from_original || individual_from_compound_declaration) {
				replacement_type_str += prefix_str + direct_qtype_str + suffix_str;
			} else {
				replacement_type_str = ddcs_ref.m_function_return_type_original_source_text_str;
			}
			replacement_code = replacement_type_str;
		} else {
			if (changed_from_original || individual_from_compound_declaration) {
				if (is_extern) {
					if ("" == ddcs_ref.m_original_initialization_expr_str) {
						replacement_code += "extern ";
					}
				} else if (is_static) {
					replacement_code += "static ";
				}
				if (res4.m_just_a_native_array) {
					replacement_code += "MSE_LH_FIXED_ARRAY_DECLARATION(" + direct_qtype_str;
					replacement_code += ", " + res4.m_native_array_size_text;
					replacement_code += ", " + variable_name + ")";
				} else {
					replacement_code += prefix_str + direct_qtype_str + suffix_str;
					replacement_code += " ";
					replacement_code += variable_name;
					replacement_code += post_name_suffix_str;
				}

				replacement_code += initializer_append_str;
			} else {
				replacement_code = ddcs_ref.m_original_source_text_str;
			}
			replacement_type_str = prefix_str + direct_qtype_str + suffix_str + post_name_suffix_str;

			if (0 == ddcs_ref.m_indirection_state_stack.size()) {
				/* This type is not a (currently) recognized "indirect" type (i.e. a pointer or
				array, etc.). */
				if (ddcs_ref.direct_qtype_has_been_changed()) {
					/* It seems the type component of a declaration can only be reliably (individually
					/surgically) replaced/modified once (at most), and so is handled at the end of
					processing (in the EndSourceFileAction() function). Here we're noting the source
					text location of the type to be replaced. */

					assert(ddcs_ref.m_ddecl_cptr);
					auto directTypeLoc = ddcs_ref.m_ddecl_cptr->getTypeSourceInfo()->getTypeLoc();
					auto directTypeSR = nice_source_range(directTypeLoc.getSourceRange(), Rewrite);
					IF_DEBUG(auto old_text1 = Rewrite.getRewrittenText(directTypeSR);)
					if (directTypeSR.getBegin().isMacroID() || ddcs_ref.m_ddecl_cptr->getSourceRange().getBegin().isMacroID()) {
						auto directTypeSRb = directTypeLoc.getSourceRange();
						IF_DEBUG(auto old_text1b = Rewrite.getRewrittenText(directTypeSR);)
						int q = 5;
					}
					auto cq_direct_type_SR = extended_to_include_west_const_if_any(Rewrite, directTypeSR);
					IF_DEBUG(auto old_text2 = Rewrite.getRewrittenText(cq_direct_type_SR);)
					cq_direct_type_SR = extended_to_include_east_const_if_any(Rewrite, cq_direct_type_SR);
					IF_DEBUG(auto old_text3 = Rewrite.getRewrittenText(cq_direct_type_SR);)

					auto& direct_type_state = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
					direct_type_state.m_maybe_source_range_including_any_const_qualifier = cq_direct_type_SR;
					direct_type_state.m_maybe_source_range = directTypeSR;
					direct_type_state.m_maybe_typeLoc = directTypeLoc;
				} else {
					int q = 7;
				}
			} else {
				/* In this case, where the declaration type is an indirect type, the direct type
				(component of the indirect type) can only be reliably replaced/modified once (at
				most), and so is handled at the end of processing (in the EndSourceFileAction()
				function). */
			}
		}

		retval.m_replacement_code = replacement_code;
		retval.m_replacement_type_str = replacement_type_str;
		retval.m_changed_from_original = changed_from_original;
		retval.m_individual_from_compound_declaration = individual_from_compound_declaration;
		return retval;
	}

	static CDeclarationReplacementCodeItem generate_declaration_replacement_code(const DeclaratorDecl* DD,
			Rewriter &Rewrite, CDDeclConversionStateMap& ddecl_conversion_state_map, std::string options_str = "") {
		return declaration_modifier_helper1(DD, Rewrite, nullptr, ddecl_conversion_state_map, options_str);
	}

	static void declaration_modifier(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, std::string options_str = "") {
		const DeclaratorDecl* DD = &ddecl;
		auto SR = nice_source_range(DD->getSourceRange(), Rewrite);

		QualType QT = DD->getType();
		const clang::Type* TP = QT.getTypePtr();
		auto qtype_str = QT.getAsString();

		auto& SM = Rewrite.getSourceMgr();

		IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

		if (filtered_out_by_location(SM, SR.getBegin())) {
			return void();
		}

#ifndef NDEBUG
		if (std::string::npos != debug_source_location_str.find(":708:")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		auto ISR = instantiation_source_range(DD->getSourceRange(), Rewrite);
		auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
		if (supress_check_flag) {
			return;
		}

		std::string variable_name = DD->getNameAsString();

		auto raw_SR = DD->getSourceRange();
		auto raw_macro_flag = raw_SR.getBegin().isMacroID() && raw_SR.getEnd().isMacroID();
		auto macro_flag = SR.getBegin().isMacroID() && SR.getEnd().isMacroID();
		if (raw_macro_flag || macro_flag) {
			IF_DEBUG(auto old_text1b = Rewrite.getRewrittenText(raw_SR);)
			if (raw_macro_flag != macro_flag) {
				int q = 5;
			}
			/* This declaration seems to be a macro, or part of a macro. For now we'll just leave
			it alone and not attempt any modifications. */
			return;
		}

		if (("" == variable_name) || (!TP)) {
			return;
		}

		bool changed_from_original = false;

		assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if ((TP->isFunctionType()) || (false)) {
			const clang::FunctionDecl* FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				auto name_str = FND->getNameAsString();
				auto return_type_source_range = nice_source_range(FND->getReturnTypeSourceRange(), Rewrite);
				if (!(return_type_source_range.isValid())) {
					return;
				}
				if (SR.getBegin() < return_type_source_range.getBegin()) {
					/* FunctionDecl::getReturnTypeSourceRange() seems to not include prefix qualifiers, like
					* "const". So we check if there is a "const" prefix present. */

					//std::string return_type_source_range_str = Rewrite.getRewrittenText(return_type_source_range);
					//std::string SR_str = Rewrite.getRewrittenText(SR);
					std::string test_str = Rewrite.getRewrittenText({ SR.getBegin(), return_type_source_range.getBegin().getLocWithOffset(-1) });

					static const std::string const_str = "const";
					if (string_begins_with(test_str, const_str)) {
						/* Extend the return_type_source_range to include the "const" prefix. */
						return_type_source_range.setBegin(SR.getBegin());
					}
				}

				//declaration_modifier_helper1(&ddecl, Rewrite, &state1, state1.m_ddecl_conversion_state_map, options_str);

				auto res = generate_declaration_replacement_code(&ddecl, Rewrite, state1.m_ddecl_conversion_state_map, options_str);
				changed_from_original |= res.m_changed_from_original;

				if (ConvertToSCPP && return_type_source_range.isValid() && (1 <= res.m_replacement_code.size())
						&& changed_from_original) {
					IF_DEBUG(std::string code_to_be_replaced = return_type_source_range.isValid() ? Rewrite.getRewrittenText(return_type_source_range) : std::string("[invalid]");)
					state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, return_type_source_range, res.m_replacement_code);
					//auto res2 = Rewrite.ReplaceText(return_type_source_range, res.m_replacement_code);
				} else {
					int q = 7;
				}
			}
		} else {
			std::string replacement_code;

			auto rd_map_iter = state1.m_recdecl_map.find(SR.getBegin());
			if (state1.m_recdecl_map.end() != rd_map_iter) {
				auto RD = (*rd_map_iter).second;

				auto res1 = state1.m_recdecl_conversion_state_map.insert(*RD, Rewrite);
				auto rdcs_map_iter = res1.first;
				auto& rdcs_ref = (*rdcs_map_iter).second;
				//bool update_declaration_flag = res1.second;

				std::string rd_name = rdcs_ref.recdecl_ptr()->getNameAsString();
				if ("" != rd_name) {
					if (rdcs_ref.recdecl_ptr()->isThisDeclarationADefinition()) {
						replacement_code += rdcs_ref.m_current_text_str + "; ";
					}
				} else {
					/* We are unable to handle this case at the moment. */
					return;
				}
				int q = 5;
			}

			/* There may be multiple declarations in the same declaration statement. Replacing
			* one of them requires replacing all of them together. */
			auto ddecls = IndividualDeclaratorDecls(DD, Rewrite);
			if ((1 <= ddecls.size())/* && (ddecls.back() == DD)*/) {
				if (2 <= ddecls.size()) {
					static const std::string semicolon_space_str = "; ";
					std::vector<std::string> action_species_list;
					for (const auto& ddecl_cref : ddecls) {
						auto res = generate_declaration_replacement_code(ddecl_cref, Rewrite, state1.m_ddecl_conversion_state_map, options_str);
						changed_from_original |= res.m_changed_from_original;

						action_species_list.push_back(res.m_action_species);
						replacement_code += res.m_replacement_code;
						replacement_code += semicolon_space_str;
					}
					if (replacement_code.size() >= 3) {
						replacement_code = replacement_code.substr(0, replacement_code.size() - semicolon_space_str.length());
					}

					/* (Only) the source range of the last individual declaration in the declaration statement
					* should encompass the whole statement. */
					auto last_ddecl = ddecls.back();
					auto last_decl_source_range = rewritable_source_range(nice_source_range(last_ddecl->getSourceRange(), Rewrite));

					IF_DEBUG(std::string last_decl_source_text = last_decl_source_range.isValid() ? Rewrite.getRewrittenText(last_decl_source_range) : std::string("[invalid]");)

					{
						auto res1 = state1.m_ddecl_conversion_state_map.insert(*DD);
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						/* In this case we're overwriting the whole declaration and including any
						initializers, so we want to remove any direction to append the initializer to
						the end of the declaration. */
						//ddcs_ref.m_maybe_embedded_initializer_insert_before_point = {};
						ddcs_ref.m_initializer_SR_or_insert_before_point = {};
					}

					if (ConvertToSCPP && last_decl_source_range.isValid() && (3 <= replacement_code.size())
							&& changed_from_original) {
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, last_decl_source_range, replacement_code);
						//auto res2 = Rewrite.ReplaceText(last_decl_source_range, replacement_code);
					} else {
						int q = 7;
					}
				} else {
					declaration_modifier_helper1(&ddecl, Rewrite, &state1, state1.m_ddecl_conversion_state_map, options_str);
				}
			} else {
				int q = 7;
			}
		}
	}

	static void update_declaration(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, std::string options_str = "") {
		const DeclaratorDecl* DD = &ddecl;
		auto SR = rewritable_source_range(nice_source_range(DD->getSourceRange(), Rewrite));

		QualType QT = DD->getType();
		const clang::Type* TP = QT.getTypePtr();
		IF_DEBUG(auto qtype_str = QT.getAsString();)

		assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if ((TP->isFunctionType()) || (false)) {
			const clang::FunctionDecl* FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				auto return_type_source_range = nice_source_range(FND->getReturnTypeSourceRange(), Rewrite);
				if (!(return_type_source_range.isValid())) {
					return;
				}
				SR = return_type_source_range;
			}
		}
		if (SR.isValid()) {
			auto lambda = [&ddecl, &Rewrite, &state1, options_str]() {
					declaration_modifier(ddecl, Rewrite, state1, options_str);
				};
			/* This modification needs to be queued so that it will be executed after any other
			modifications that might affect the relevant part of the source text. */
			state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
		}
	}

	void note_array_determination(Rewriter &Rewrite, CTUState& state1, const CDDeclIndirection& ddecl_indirection) {

		auto res1 = state1.m_ddecl_conversion_state_map.insert(*(ddecl_indirection.m_ddecl_cptr));
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;
		bool update_declaration_flag = res1.second;

		if (ddcs_ref.m_indirection_state_stack.size() >= ddecl_indirection.m_indirection_level) {
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(ddecl_indirection.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(ddecl_indirection.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(ddecl_indirection.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(ddecl_indirection.m_indirection_level, "dynamic array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
				state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
			} else {
				int q = 3;
			}
		} else {
			int q = 7;
		}

		if (update_declaration_flag) {
			update_declaration(*(ddecl_indirection.m_ddecl_cptr), Rewrite, state1);
		}
	}

	struct CLeadingAddressofOperatorInfo {
		bool leading_addressof_operator_detected = false;
		const clang::Expr* without_leading_addressof_operator_expr_cptr = nullptr;
		const clang::UnaryOperator* addressof_unary_operator_cptr = nullptr;
	};
	CLeadingAddressofOperatorInfo leading_addressof_operator_info_from_stmt(const clang::Stmt& stmt_cref, int depth = 0) {
		CLeadingAddressofOperatorInfo retval;
		const clang::Stmt* ST = &stmt_cref;
		auto stmt_class = ST->getStmtClass();
		auto stmt_class_name = ST->getStmtClassName();
		bool process_child_flag = false;
		if (clang::Stmt::StmtClass::UnaryOperatorClass == stmt_class) {
			auto UO = llvm::cast<const clang::UnaryOperator>(ST);
			if (UO) {
				if (clang::UnaryOperatorKind::UO_AddrOf == UO->getOpcode()) {
					retval.leading_addressof_operator_detected = true;
					retval.addressof_unary_operator_cptr = UO;

					auto child_iter = ST->child_begin();
					if ((child_iter != ST->child_end()) && (llvm::isa<const clang::Expr>(*child_iter))) {
						retval.without_leading_addressof_operator_expr_cptr =
								llvm::cast<const clang::Expr>(*child_iter);
					} else {
						assert(false);
					}
				}
			} else {
				assert(false);
			}
		} else if ((clang::Stmt::StmtClass::ImplicitCastExprClass == stmt_class)) {
			auto ICE = llvm::cast<const clang::ImplicitCastExpr>(ST);
			if (ICE) {
				auto cast_kind_name = ICE->getCastKindName();
				auto cast_kind = ICE->getCastKind();
				if ((clang::CK_FunctionToPointerDecay == cast_kind)) {
					process_child_flag = false;
				} else {
					if ((clang::CK_ArrayToPointerDecay == cast_kind) || (clang::CK_LValueToRValue == cast_kind)) {
						process_child_flag = true;
					} else {
						process_child_flag = true;
					}
				}
			} else { assert(false); }
		} else if ((clang::Stmt::StmtClass::CStyleCastExprClass == stmt_class)) {
			auto CSCE = llvm::cast<const clang::CStyleCastExpr>(ST);
			if (CSCE) {
				auto cast_kind_name = CSCE->getCastKindName();
				auto cast_kind = CSCE->getCastKind();
				auto qtype = CSCE->getType();
				if ((clang::CK_FunctionToPointerDecay == cast_kind)) {
					process_child_flag = false;
				} else {
					if ((clang::CK_ArrayToPointerDecay == cast_kind) || (clang::CK_LValueToRValue == cast_kind)) {
						process_child_flag = true;
					} else {
						process_child_flag = true;
					}
				}
			} else { assert(false); }
		} else if ((clang::Stmt::StmtClass::ParenExprClass == stmt_class)) {
			process_child_flag = true;
		} else if ((clang::Stmt::StmtClass::CallExprClass == stmt_class)) {
			process_child_flag = true;
		} else if(clang::Stmt::StmtClass::DeclRefExprClass == stmt_class) {
			auto DRE = llvm::cast<const clang::DeclRefExpr>(ST);
			if (DRE) {
				//retval = DRE;
				//process_child_flag = true;
			} else {
				assert(false);
			}
		} else if(clang::Stmt::StmtClass::MemberExprClass == stmt_class) {
			auto ME = llvm::cast<const clang::MemberExpr>(ST);
			if (ME) {
				//retval = ME;
			} else {
				assert(false);
			}
		} else {
			if (0 == depth) {
				int q = 5;
			}
			int q = 5;
		}
		if (process_child_flag) {
			auto child_iter = ST->child_begin();
			if (child_iter != ST->child_end()) {
				if (nullptr != (*child_iter)) {
					retval = leading_addressof_operator_info_from_stmt(*(*child_iter), depth+1);
				} else {
					assert(false);
				}
			} else {
				int q = 5;
			}
		}
		return retval;
	}

	struct CArrayInferenceInfo {
		bool is_an_indirect_type() const {
			if (nullptr != ddecl_conversion_state_ptr) {
				return (1 <= ddecl_conversion_state_ptr->m_indirection_state_stack.size());
			} else {
				return false;
			}
		}
		bool update_declaration_flag = false;
		bool has_just_been_determined_to_be_an_array_flag = false;
		size_t has_just_been_determined_to_be_an_array_indirection_level = 0;
		size_t indirection_level = 0;
		const DeclaratorDecl* ddecl_cptr = nullptr;
		CDDeclConversionState* ddecl_conversion_state_ptr = nullptr;
		const clang::Expr* declaration_expr_cptr = nullptr;
	};
	CArrayInferenceInfo infer_array_type_info_from_stmt_indirection_stack(CDDeclConversionState& ddcs_ref,
			const std::vector<std::string>& stmt_indirection_stack, CTUState& state1_ref) {
		CArrayInferenceInfo retval;
		auto DD = ddcs_ref.m_ddecl_cptr;
		if (!DD) { assert(false); return retval; }
		for (size_t i = 0; ((i < ddcs_ref.m_indirection_state_stack.size())
				&& (i < stmt_indirection_stack.size())); i += 1) {
			if (("" == stmt_indirection_stack[i])) {
				/* We're using the empty string as a generic state for the "terminal level of indirection"
				* when we don't want to bother specifying a specific state. */
				retval.indirection_level = i;
			} else if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(i).current_species()) {
				if (("ArraySubscriptExpr" == stmt_indirection_stack[i])
						|| ("pointer arithmetic" == stmt_indirection_stack[i])) {
					ddcs_ref.set_indirection_current(i, "inferred array");
					retval.update_declaration_flag = true;
					state1_ref.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				} else if (("malloc target" == stmt_indirection_stack[i])) {
					ddcs_ref.set_indirection_current(i, "malloc target");
					retval.indirection_level = i;
				} else if (("set to null" == stmt_indirection_stack[i]) ||
						("memset/cpy target" == stmt_indirection_stack[i])) {
					retval.indirection_level = i;
				}
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(i).current_species()) {
				if (("ArraySubscriptExpr" == stmt_indirection_stack[i])
						|| ("pointer arithmetic" == stmt_indirection_stack[i])) {
					ddcs_ref.set_indirection_current(i, "dynamic array");
					retval.update_declaration_flag = true;
					state1_ref.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
					state1_ref.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				}
			} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(i).current_species()) {
				if (("malloc target" == stmt_indirection_stack[i]) ||
						("set to null" == stmt_indirection_stack[i])) {
					ddcs_ref.set_indirection_current(i, "dynamic array");
					retval.update_declaration_flag = true;
					retval.has_just_been_determined_to_be_an_array_flag = true;
					retval.indirection_level = i;
					state1_ref.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				} else if (("memset/cpy target" == stmt_indirection_stack[i])) {
					retval.has_just_been_determined_to_be_an_array_flag = true;
					retval.indirection_level = i;
				}
			} else if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(i).current_species()) {
				if (("malloc target" == stmt_indirection_stack[i]) ||
						("set to null" == stmt_indirection_stack[i])) {
					retval.has_just_been_determined_to_be_an_array_flag = true;
					retval.indirection_level = i;
				}
			}
			/* We've changed it here so that retval.indirection_level will always end up being
			* (stmt_indirection_stack.size() - 1) when (1 <= stmt_indirection_stack.size()). This
			* renders the above "retval.indirection_level = i;" statements redundant, but we'll
			* leave them there for now in case we need them again. */
			retval.indirection_level = i;
		}
		return retval;
	}

	/* We are trying to determine, for each and every pointer, whether or not it is being used as an
	* array iterator. So this function takes an expression using a declared (pointer) variable
	* and notes when the (pointer) variable, or any dereference of the
	* (pointer) variable, is being used as an array iterator. So for example, given a declaration,
	* say, "int*** ptr1;" and an expression, say, "(*(ptr1[3]))[5]", it will note the two levels
	* of dereference/"indirection" that are being used as array iterators/pointers. Upon determining
	* that a pointer (or a dereference/indirection of the pointer) is being used as an array iterator,
	* the function will execute any queued up actions that were contingent on such a determination. */
	CArrayInferenceInfo infer_array_type_info_from_stmt(const clang::Stmt& stmt_cref, const std::string& stmt_array_info_str,
			CTUState& state1_ref, const DeclaratorDecl* DD = nullptr) {
		CArrayInferenceInfo retval;

		std::vector<std::string> stmt_indirection_stack;
		const clang::Expr* expr2 = populateStmtIndirectionStack(stmt_indirection_stack, stmt_cref);
		std::reverse(stmt_indirection_stack.begin(), stmt_indirection_stack.end());
		stmt_indirection_stack.push_back(stmt_array_info_str);
		if (expr2) {
			const DeclaratorDecl* l_DD = DD;
			std::string expr2_stmt_class_name;
			expr2_stmt_class_name = expr2->getStmtClassName();
			const DeclaratorDecl* expr2_DD = nullptr;
			if (clang::Stmt::StmtClass::DeclRefExprClass == expr2->getStmtClass()) {
				auto expr2_DRE = llvm::cast<const clang::DeclRefExpr>(expr2);
				if (expr2_DRE) {
					auto expr2_decl = expr2_DRE->getDecl();
					expr2_DD = dyn_cast<const DeclaratorDecl>(expr2_decl);
				} else { assert(false); }
			} else if (clang::Stmt::StmtClass::MemberExprClass == expr2->getStmtClass()) {
				auto expr2_ME = llvm::cast<const clang::MemberExpr>(expr2);
				if (expr2_ME) {
					auto expr2_FD = dyn_cast<const clang::FieldDecl>(expr2_ME->getMemberDecl());
					if (expr2_FD) {
						expr2_DD = expr2_FD;
					} else { assert(false); }
				} else { assert(false); }
			}
			if (expr2_DD) {
				auto expr2_QT = expr2_DD->getType();
				auto expr2_type_str = expr2_QT.getAsString();
				std::string expr2_variable_name = expr2_DD->getNameAsString();

				if (nullptr == l_DD) {
					l_DD = expr2_DD;
				}
				auto res1 = state1_ref.m_ddecl_conversion_state_map.insert(*l_DD);
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				bool update_declaration_flag = res1.second;

				auto QT = (*l_DD).getType();
				std::string variable_name = (*l_DD).getNameAsString();

				if ((expr2_QT == QT) && (expr2_variable_name == variable_name)) {
					retval = infer_array_type_info_from_stmt_indirection_stack(ddcs_ref, stmt_indirection_stack, state1_ref);
				}

				retval.update_declaration_flag |= update_declaration_flag;
				retval.ddecl_conversion_state_ptr = &ddcs_ref;
			}
			retval.ddecl_cptr = expr2_DD;
		}
		retval.declaration_expr_cptr = expr2;

		return retval;
	}


	void CExprTextReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const Expr* EX = m_EX;
		if (EX) {
			auto EXSR = write_once_source_range(nice_source_range(EX->getSourceRange(), (*this).m_Rewrite));
			if (EXSR.isValid()) {
				auto excs_iter = state1.m_expr_conversion_state_map.find(EX);
				if (state1.m_expr_conversion_state_map.end() == excs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*EX, m_Rewrite);
					excs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& excs_shptr_ref = (*excs_iter).second;

				if (ConvertToSCPP) {
					std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>((*this).m_replacement_code);
					(*excs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
					(*excs_shptr_ref).update_current_text();

					state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, EXSR, (*excs_shptr_ref).m_current_text_str);
					//(*this).m_Rewrite.ReplaceText(EXSR, (*excs_shptr_ref).m_current_text_str);
				}
			}
		}
	}

	/* Ensure that the given declarations are the same type (give or take a reference). */
	void homogenize_types(CTUState& state1, Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const clang::DeclaratorDecl& ddecl_cref1,
		const clang::DeclaratorDecl& ddecl_cref2) {

#ifndef NDEBUG
		auto SR = nice_source_range(ddecl_cref1.getSourceRange(), Rewrite);
		//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

		//RETURN_IF_FILTERED_OUT_BY_LOCATION1;

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(":612")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		/* homogenize the direct types (i.e. the types with any pointer/reference/array/etc indirections removed) */
		CSameTypeReplacementAction(Rewrite, MR, CDDeclIndirection(ddecl_cref2, CDDeclIndirection::no_indirection)
			, CDDeclIndirection(ddecl_cref1, CDDeclIndirection::no_indirection), CSameTypeReplacementAction::apply_to_redeclarations_t::no).do_replacement(state1);

		auto lhs_res1 = state1.m_ddecl_conversion_state_map.insert(ddecl_cref2);
		auto lhs_ddcs_map_iter = lhs_res1.first;
		auto& lhs_ddcs_ref = (*lhs_ddcs_map_iter).second;
		bool lhs_update_declaration_flag = lhs_res1.second;

		auto rhs_res1 = state1.m_ddecl_conversion_state_map.insert(ddecl_cref1);
		auto rhs_ddcs_map_iter = rhs_res1.first;
		auto& rhs_ddcs_ref = (*rhs_ddcs_map_iter).second;
		bool rhs_update_declaration_flag = rhs_res1.second;

		size_t lhs_indirection_level_adjustment = 0;
		if (1 <= lhs_ddcs_ref.m_indirection_state_stack.size()) {
			if ("native reference" == lhs_ddcs_ref.m_indirection_state_stack.front().current_species()) {
				lhs_indirection_level_adjustment = 1;
			}
		}
		size_t rhs_indirection_level_adjustment = 0;
		if (1 <= rhs_ddcs_ref.m_indirection_state_stack.size()) {
			if ("native reference" == rhs_ddcs_ref.m_indirection_state_stack.front().current_species()) {
				rhs_indirection_level_adjustment = 1;
			}
		}

		if ((lhs_ddcs_ref.m_indirection_state_stack.size() + rhs_indirection_level_adjustment) != (rhs_ddcs_ref.m_indirection_state_stack.size() + lhs_indirection_level_adjustment)) {
			return;
		} else {
			/* homogenize the types of all the indirections */
			size_t adjusted_num_indirection_levels = lhs_ddcs_ref.m_indirection_state_stack.size() - lhs_indirection_level_adjustment;
			for (size_t i = 0; i < adjusted_num_indirection_levels; i += 1) {
				CSameTypeReplacementAction(Rewrite, MR, CDDeclIndirection(ddecl_cref2, i + lhs_indirection_level_adjustment)
					, CDDeclIndirection(ddecl_cref1, i + rhs_indirection_level_adjustment), CSameTypeReplacementAction::apply_to_redeclarations_t::no).do_replacement(state1);

				CSameTypeArray2ReplacementAction(Rewrite, MR, CDDeclIndirection(*(lhs_ddcs_ref.m_ddecl_cptr), i + lhs_indirection_level_adjustment)
					, CDDeclIndirection(*(rhs_ddcs_ref.m_ddecl_cptr), i + rhs_indirection_level_adjustment)).do_replacement(state1);
			}
		}
	}

	/* Ensure that all the (re)declarations of the same variable are the same type. */
	void homogenize_redeclaration_types(const clang::DeclaratorDecl* ddecl_cptr, CTUState& state1, Rewriter &Rewrite, const MatchFinder::MatchResult &MR, int ttl = -1) {
		if (!ddecl_cptr) { return; }

#ifndef NDEBUG
		auto SR = nice_source_range(ddecl_cptr->getSourceRange(), Rewrite);
		//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

		//RETURN_IF_FILTERED_OUT_BY_LOCATION1;

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(":612")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto PVD = dyn_cast<const clang::ParmVarDecl>(ddecl_cptr);
		if (PVD) {
			auto DC = PVD->getDeclContext();
			const clang::FunctionDecl* function_decl1 = DC ? dyn_cast<const clang::FunctionDecl>(DC) : nullptr;
			if (function_decl1) {
				std::string function_name = function_decl1->getNameAsString();
				auto lc_function_name = tolowerstr(function_name);

				std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
				for (auto param_PVD : function_decl1->parameters()) {
					param_decls_of_first_function_decl.push_back(param_PVD);
				}

				auto function_decls_range = function_decl1->redecls();
				for (const auto& function_decl : function_decls_range) {
					auto fdecl_source_range = nice_source_range(function_decl->getSourceRange(), Rewrite);
					auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(*MR.SourceManager);

					auto param_index = PVD->getFunctionScopeIndex();
					auto PVD2 = function_decl->getParamDecl(param_index);
					if (PVD2) {
						if (PVD2 != PVD) {
							homogenize_types(state1, Rewrite, MR, *PVD2, *PVD);

							IF_DEBUG(std::string PVD2_qtype_str = PVD2->getType().getAsString();)
							if (PVD2->getType()->isReferenceType()) {
								if (!(PVD2->getType()->getPointeeType().isConstQualified())) {
									/* This parameter is of non-const reference type, so it's
									initialization value (if any) must be of the same type. */
									auto init_EX2 = PVD2->getInit();
									if (init_EX2) {
										auto DRE2 = dyn_cast<const clang::DeclRefExpr>(init_EX2->IgnoreParenImpCasts());
										if (DRE2) {
											auto init_VD2 = dyn_cast<const clang::VarDecl>(DRE2->getDecl());
											if (init_VD2) {
												homogenize_types(state1, Rewrite, MR, *init_VD2, *PVD2);

												/* A non-negative ttl parameter specifies a maximum permitted number of
												recursive calls (to ensure no infinite recursion). */
												if (0 != ttl) {
													auto new_ttl = (0 < ttl) ? (ttl - 1) : ttl;
													homogenize_redeclaration_types(init_VD2, state1, Rewrite, MR, new_ttl);
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

		auto VD = dyn_cast<const clang::VarDecl>(ddecl_cptr);
		if (VD) {
			for (auto redecl : VD->redecls()) {
				/* this part hasn't been tested yet */
				if (redecl != VD) {
					homogenize_types(state1, Rewrite, MR, *redecl, *VD);
				}
			}

			IF_DEBUG(std::string VD_qtype_str = VD->getType().getAsString();)
			if (VD->getType()->isReferenceType()) {
				if (!(VD->getType()->getPointeeType().isConstQualified())) {
					/* This variable is of non-const reference type, so it's
					initialization value (if any) must be of the same type. */
					auto init_EX2 = VD->getInit();
					if (init_EX2) {
						auto DRE2 = dyn_cast<const clang::DeclRefExpr>(init_EX2->IgnoreParenImpCasts());
						if (DRE2) {
							auto init_VD2 = dyn_cast<const clang::VarDecl>(DRE2->getDecl());
							if (init_VD2) {
								homogenize_types(state1, Rewrite, MR, *init_VD2, *VD);

								if (0 != ttl) {
									auto new_ttl = (0 < ttl) ? (ttl - 1) : ttl;
									homogenize_redeclaration_types(init_VD2, state1, Rewrite, MR, new_ttl);
								}
							}
						}
					}
				}
			}
		}
	}

	void CSameTypeReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		auto lhs_res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection2.m_ddecl_cptr));
		auto lhs_ddcs_map_iter = lhs_res1.first;
		auto& lhs_ddcs_ref = (*lhs_ddcs_map_iter).second;
		bool lhs_update_declaration_flag = lhs_res1.second;

		auto rhs_res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection.m_ddecl_cptr));
		auto rhs_ddcs_map_iter = rhs_res1.first;
		auto& rhs_ddcs_ref = (*rhs_ddcs_map_iter).second;
		bool rhs_update_declaration_flag = rhs_res1.second;

#ifndef NDEBUG
		auto SR = nice_source_range(m_ddecl_indirection.m_ddecl_cptr->getSourceRange(), Rewrite);
		//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

		//RETURN_IF_FILTERED_OUT_BY_LOCATION1;

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(":207:")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		{
			std::string& lhs_pointer_target_state = (CDDeclIndirection::no_indirection == m_ddecl_indirection2.m_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_current_pointer_target_state : lhs_ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).m_current_pointer_target_state;
			std::string& rhs_pointer_target_state = (CDDeclIndirection::no_indirection == m_ddecl_indirection.m_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_current_pointer_target_state : rhs_ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection.m_indirection_level).m_current_pointer_target_state;

			if ("pointer target" == lhs_pointer_target_state) {
				if ("" == rhs_pointer_target_state) {
					rhs_pointer_target_state = lhs_pointer_target_state;
					rhs_update_declaration_flag |= true;
					state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				}
			} else if ("" == lhs_pointer_target_state) {
				if ("pointer target" == rhs_pointer_target_state) {
					lhs_pointer_target_state = rhs_pointer_target_state;
					lhs_update_declaration_flag |= true;
					state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				}
			}

			if ((CDDeclIndirection::no_indirection != m_ddecl_indirection2.m_indirection_level) && (CDDeclIndirection::no_indirection != m_ddecl_indirection.m_indirection_level)) {
				CSameTypeArray2ReplacementAction(Rewrite, MR, m_ddecl_indirection2, m_ddecl_indirection).do_replacement(state1);
			}
		}

		if (apply_to_redeclarations_t::yes == m_apply_to_redeclarations) {
			homogenize_redeclaration_types(m_ddecl_indirection2.m_ddecl_cptr, state1, Rewrite, MR);
			homogenize_redeclaration_types(m_ddecl_indirection.m_ddecl_cptr, state1, Rewrite, MR);
		}

		if (lhs_update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
		if (rhs_update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CMallocArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const BinaryOperator* BO = m_BO;
		const DeclaratorDecl* DD = m_DD;

		if ((BO != nullptr) && (DD != nullptr))
		{
			auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);

			if ((*this).ddecl_indirection_cref().m_ddecl_cptr) {
				auto res1 = state1.m_ddecl_conversion_state_map.insert(*((*this).ddecl_indirection_cref().m_ddecl_cptr));
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				bool update_declaration_flag = res1.second;

				if (ddcs_ref.m_indirection_state_stack.size() >= (*this).ddecl_indirection_cref().m_indirection_level) {
					if ("inferred array" == ddcs_ref.indirection_current((*this).ddecl_indirection_cref().m_indirection_level)) {
						ddcs_ref.set_indirection_current((*this).ddecl_indirection_cref().m_indirection_level, "dynamic array");
						update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
					} else {
						int q = 5;
					}
				} else {
					int q = 7;
				}

				if (update_declaration_flag) {
					//update_declaration(*((*this).ddecl_indirection_cref().m_ddecl_cptr), Rewrite, state1);
				}
			} else {
				int q = 7;
			}

			if (ConvertToSCPP && (BOSR.isValid())) {
				update_declaration(*DD, Rewrite, state1);

				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
				//state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());

				state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, BOSR, (*this).m_bo_replacement_code);
				//auto res2 = Rewrite.ReplaceText(BOSR, (*this).m_bo_replacement_code);

				int q = 3;
			} else {
				int q = 7;
			}
		}
	}

	void CInitializerArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const DeclStmt* DS = m_DS;
		const DeclaratorDecl* DD = m_DD;

		if ((DS != nullptr) && (DD != nullptr))
		{
			auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);

			auto res1 = state1.m_ddecl_conversion_state_map.insert(*DD);
			auto ddcs_map_iter = res1.first;
			auto& ddcs_ref = (*ddcs_map_iter).second;

			std::string current_direct_qtype_str = ddcs_ref.current_direct_qtype_str();
			std::string initializer_info_str = m_current_initialization_expr_str;
			static const std::string void_str = "void";
			auto void_pos = initializer_info_str.find(void_str);
			if (std::string::npos != void_pos) {
				initializer_info_str.replace(void_pos, void_str.length(), current_direct_qtype_str);
			}

			ddcs_ref.m_current_initialization_expr_str = initializer_info_str;

			if (ConvertToSCPP && decl_source_range.isValid()) {
				update_declaration(*DD, Rewrite, state1);

				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
				//state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());

				int q = 3;
			} else {
				int q = 7;
			}
		}
	}

	void CFreeDynamicArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const CallExpr* CE = m_CE;
		const DeclaratorDecl* DD = m_DD;

		if ((CE != nullptr) && (DD != nullptr))
		{
			auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
			auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
			if (!decl_source_range.isValid()) {
				return;
			}
			DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
			DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

			if (ConvertToSCPP && decl_source_range.isValid() && (CESR.isValid())) {
				update_declaration(*DD, Rewrite, state1);

				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection_cref());
				//state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection_cref());

				state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, CESR, m_ce_replacement_code);
				//auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);
			} else {
				int q = 7;
			}
		}
	}

	void CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		auto res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection2.m_ddecl_cptr));
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;
		bool update_declaration_flag = res1.second;

		if (ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection2.m_indirection_level) {
			if (true) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "dynamic array");
				update_declaration_flag |= true;
				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			}
		} else {
			int q = 7;
		}

		if (update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CMemsetArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const CallExpr* CE = m_CE;
		const DeclaratorDecl* DD = m_DD;

		if ((CE != nullptr) && (DD != nullptr))
		{
			auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);

			if (ConvertToSCPP && CESR.isValid()) {

				update_declaration(*DD, Rewrite, state1);

				state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, CESR, m_ce_replacement_code);
				//auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);
			} else {
				int q = 7;
			}
		}
	}

	void CMemcpyArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const CallExpr* CE = m_CE;
		const DeclaratorDecl* DD = m_DD;

		if (CE != nullptr)
		{
			auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);

			if (ConvertToSCPP && (CESR.isValid())) {
				state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, CESR, m_ce_replacement_code);
				//auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);

				if (DD != nullptr) {
					update_declaration(*DD, Rewrite, state1);
				}
			} else {
				int q = 7;
			}
		}
	}

	void CAssignmentTargetConstrainsSourceArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		auto res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection2.m_ddecl_cptr));
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;
		bool update_declaration_flag = res1.second;

		if (ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection2.m_indirection_level) {
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "dynamic array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else {
				int q = 3;
			}
		} else {
			int q = 7;
		}

		if (update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CAssignmentSourceConstrainsTargetArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		if (m_ddecl_indirection2.m_ddecl_cptr) {
			std::string variable_name = (m_ddecl_indirection2.m_ddecl_cptr)->getNameAsString();
			if ("buffer" == variable_name) {
				std::string qtype_str = (m_ddecl_indirection2.m_ddecl_cptr)->getType().getAsString();
				if ("const unsigned char *" == qtype_str) {
					int q = 5;
				}
			}
		}

		auto res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection2.m_ddecl_cptr));
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;
		bool update_declaration_flag = res1.second;

		if (ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection2.m_indirection_level) {
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current_species()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "dynamic array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else {
				int q = 3;
			}
		} else {
			int q = 7;
		}

		if (update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CSameTypeArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		auto lhs_res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection2.m_ddecl_cptr));
		auto lhs_ddcs_map_iter = lhs_res1.first;
		auto& lhs_ddcs_ref = (*lhs_ddcs_map_iter).second;
		bool lhs_update_declaration_flag = lhs_res1.second;

		auto rhs_res1 = state1.m_ddecl_conversion_state_map.insert(*(m_ddecl_indirection.m_ddecl_cptr));
		auto rhs_ddcs_map_iter = rhs_res1.first;
		auto& rhs_ddcs_ref = (*rhs_ddcs_map_iter).second;
		bool rhs_update_declaration_flag = rhs_res1.second;

		if ((lhs_ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection2.m_indirection_level) &&
				(rhs_ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection.m_indirection_level)){
			auto lhs_indirection_level = m_ddecl_indirection2.m_indirection_level;
			auto rhs_indirection_level = m_ddecl_indirection.m_indirection_level;
			auto& lhs_current_cref = lhs_ddcs_ref.indirection_current(lhs_indirection_level);
			auto& rhs_current_cref = rhs_ddcs_ref.indirection_current(rhs_indirection_level);

			if ("native pointer" == lhs_current_cref) {
				if ("native pointer" == rhs_current_cref) {
				} else if ("malloc target" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
				} else if ("inferred array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				} else if ("dynamic array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				} else if ("native array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				}
			} else if ("malloc target" == lhs_current_cref) {
				if ("native pointer" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
				} else if ("malloc target" == rhs_current_cref) {
				} else if ("inferred array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "dynamic array");
					lhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);

					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "dynamic array");
					rhs_update_declaration_flag |= true;
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("dynamic array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				} else if ("native array" == rhs_current_cref) {
					// could be a problem
					int q = 7;
				}
			} else if ("inferred array" == lhs_current_cref) {
				if ("native pointer" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("malloc target" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "dynamic array");
					lhs_update_declaration_flag |= true;
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);

					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "dynamic array");
					rhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("inferred array" == rhs_current_cref) {
				} else if ("dynamic array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
				} else if ("native array" == rhs_current_cref) {
					lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
					lhs_update_declaration_flag |= true;
				}
			} else if ("dynamic array" == lhs_current_cref) {
				if ("native pointer" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("malloc target" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("inferred array" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("dynamic array" == rhs_current_cref) {
				} else if ("native array" == rhs_current_cref) {
					// could be a problem
					int q = 7;
				}
			} else if ("native array" == lhs_current_cref) {
				if ("native pointer" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
					state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection);
				} else if ("malloc target" == rhs_current_cref) {
					// could be a problem
					int q = 7;
				} else if ("inferred array" == rhs_current_cref) {
					rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
					rhs_update_declaration_flag |= true;
				} else if ("dynamic array" == rhs_current_cref) {
					// could be a problem
					int q = 7;
				} else if ("native array" == rhs_current_cref) {
				}
			}

			/*
			if ("pointer target" == lhs_ddcs_ref.direct_type_state_ref().current_pointer_target_state()) {
				if ("pointer target" == rhs_ddcs_ref.direct_type_state_ref().current_pointer_target_state()) {
				} else {
					rhs_ddcs_ref.direct_type_state_ref().set_current_pointer_target_state("pointer target");
					rhs_update_declaration_flag = true;
				}
			} else if ("" == lhs_ddcs_ref.direct_type_state_ref().current_pointer_target_state()) {
				if ("pointer target" == rhs_ddcs_ref.direct_type_state_ref().current_pointer_target_state()) {
					lhs_ddcs_ref.direct_type_state_ref().set_current_pointer_target_state("pointer target");
					lhs_update_declaration_flag = true;
				}
			}
			*/
		} else {
			int q = 7;
		}

		if (lhs_update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
		if (rhs_update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CAddressofArraySubscriptExprReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const ArraySubscriptExpr* ASE = m_arraysubscriptexpr_cptr;
		if (UO && ASE) {
			auto UOSR = nice_source_range(UO->getSourceRange(), (*this).m_Rewrite);
			auto ase_SR = nice_source_range(ASE->getSourceRange(), (*this).m_Rewrite);
			if ((UOSR.isValid()) && (ase_SR.isValid())) {
				auto uocs_iter = state1.m_expr_conversion_state_map.find(UO);
				if (state1.m_expr_conversion_state_map.end() == uocs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CAddressofExprConversionState>(*UO, m_Rewrite, *ASE);
					uocs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& uocs_shptr_ref = (*uocs_iter).second;

				auto index_expr_cptr = ASE->getIdx();
				auto array_expr_cptr = ASE->getBase();
				if (index_expr_cptr && array_expr_cptr) {
					auto index_cs_iter = state1.m_expr_conversion_state_map.find(index_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == index_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*index_expr_cptr, m_Rewrite);
						index_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& index_cs_shptr_ref = (*index_cs_iter).second;

					auto array_cs_iter = state1.m_expr_conversion_state_map.find(array_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == array_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*array_expr_cptr, m_Rewrite);
						array_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& array_cs_shptr_ref = (*array_cs_iter).second;

					std::string UO_replacement_text = "((" + (*array_cs_shptr_ref).m_current_text_str + ") + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					if (ConvertToSCPP) {
						std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(UO_replacement_text);
						(*uocs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
						(*uocs_shptr_ref).update_current_text();

						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).m_current_text_str);
						//(*this).m_Rewrite.ReplaceText(UOSR, (*uocs_shptr_ref).m_current_text_str);
					}
				}

			}
		}
	}

	void CAddressofSubscriptOperatorCallExprReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const clang::CXXOperatorCallExpr* ASE = m_arraysubscriptexpr_cptr;
		if (UO && ASE) {
			auto UOSR = nice_source_range(UO->getSourceRange(), (*this).m_Rewrite);
			auto ase_SR = nice_source_range(ASE->getSourceRange(), (*this).m_Rewrite);
			if ((UOSR.isValid()) && (ase_SR.isValid())) {
				auto uocs_iter = state1.m_expr_conversion_state_map.find(UO);
				if (state1.m_expr_conversion_state_map.end() == uocs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CAddressofExprConversionState>(*UO, m_Rewrite, *ASE);
					uocs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& uocs_shptr_ref = (*uocs_iter).second;

				const clang::Expr* index_expr_cptr = nullptr;
				const clang::Expr* array_expr_cptr =  nullptr;
				int count1 = 0;
				for (const auto& arg : ASE->arguments()) {
					if (0 == count1) {
						array_expr_cptr = arg;
					} else if (1 == count1) {
						index_expr_cptr = arg;
					} else {
						break;
					}
					count1 += 1;
				}
				if (!index_expr_cptr) {
					assert(false);
					return;
				}
				//auto array_expr_cptr = ASE->getCallee();
				if (index_expr_cptr && array_expr_cptr) {
					auto index_cs_iter = state1.m_expr_conversion_state_map.find(index_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == index_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*index_expr_cptr, m_Rewrite);
						index_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& index_cs_shptr_ref = (*index_cs_iter).second;

					auto array_cs_iter = state1.m_expr_conversion_state_map.find(array_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == array_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*array_expr_cptr, m_Rewrite);
						array_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& array_cs_shptr_ref = (*array_cs_iter).second;

					std::string UO_replacement_text;
					if ("Dual" == ConvertMode) {
						UO_replacement_text = "(mse::make_nullable_any_random_access_iterator(std::begin(" + (*array_cs_shptr_ref).m_current_text_str + ")) + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					} else if ("FasterAndStricter" == ConvertMode) {
						if (true) {
							return;
						} else {
							UO_replacement_text = (*this).m_Rewrite.getRewrittenText(UOSR);
							if ("" == UO_replacement_text) {
								UO_replacement_text = "&(" + (*array_cs_shptr_ref).m_current_text_str + "[" + (*index_cs_shptr_ref).m_current_text_str + "])";
							}
						}
					} else {
						UO_replacement_text = "(mse::make_nullable_any_random_access_iterator(std::begin(" + (*array_cs_shptr_ref).m_current_text_str + ")) + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					}

					if (ConvertToSCPP) {
						std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(UO_replacement_text);
						(*uocs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
						(*uocs_shptr_ref).update_current_text();

						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).m_current_text_str);
						//(*this).m_Rewrite.ReplaceText(UOSR, (*uocs_shptr_ref).m_current_text_str);
					}
				}

			}
		}
	}

	void CExprTextDDIReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const Expr* EX = m_EX;
		CExprTextReplacementAction(Rewrite, MR, EX, (*this).m_replacement_code).do_replacement(state1);
	}

	void CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const ArraySubscriptExpr* ASE = m_arraysubscriptexpr_cptr;
		if (false && UO && ASE) {
			auto UOSR = nice_source_range(UO->getSourceRange(), (*this).m_Rewrite);
			auto ase_SR = nice_source_range(ASE->getSourceRange(), (*this).m_Rewrite);
			if ((UOSR.isValid()) && (ase_SR.isValid())) {
				auto uocs_iter = state1.m_expr_conversion_state_map.find(UO);
				if (state1.m_expr_conversion_state_map.end() == uocs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CAddressofExprConversionState>(*UO, m_Rewrite, *ASE);
					uocs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& uocs_shptr_ref = (*uocs_iter).second;

				auto index_expr_cptr = ASE->getIdx();
				auto array_expr_cptr = ASE->getBase();
				if (index_expr_cptr && array_expr_cptr) {
					auto index_cs_iter = state1.m_expr_conversion_state_map.find(index_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == index_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*index_expr_cptr, m_Rewrite);
						index_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& index_cs_shptr_ref = (*index_cs_iter).second;

					auto array_cs_iter = state1.m_expr_conversion_state_map.find(array_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == array_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*array_expr_cptr, m_Rewrite);
						array_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& array_cs_shptr_ref = (*array_cs_iter).second;

					std::string UO_replacement_text = "((" + (*array_cs_shptr_ref).m_current_text_str + ") + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					if (ConvertToSCPP) {
						std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(UO_replacement_text);
						(*uocs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
						(*uocs_shptr_ref).update_current_text();

						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).m_current_text_str);
						//(*this).m_Rewrite.ReplaceText(UOSR, (*uocs_shptr_ref).m_current_text_str);
					}
				}

			}
		}
	}

	void CAssignmentTargetConstrainsAddressofSubscriptOperatorCallExprArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const clang::CXXOperatorCallExpr* ASE = m_arraysubscriptexpr_cptr;
		if (UO && ASE) {
			auto UOSR = nice_source_range(UO->getSourceRange(), (*this).m_Rewrite);
			auto ase_SR = nice_source_range(ASE->getSourceRange(), (*this).m_Rewrite);
			if ((UOSR.isValid()) && (ase_SR.isValid())) {
				auto uocs_iter = state1.m_expr_conversion_state_map.find(UO);
				if (state1.m_expr_conversion_state_map.end() == uocs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CAddressofExprConversionState>(*UO, m_Rewrite, *ASE);
					uocs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& uocs_shptr_ref = (*uocs_iter).second;

				const clang::Expr* index_expr_cptr = nullptr;
				const clang::Expr* array_expr_cptr =  nullptr;
				int count1 = 0;
				for (const auto& arg : ASE->arguments()) {
					if (0 == count1) {
						array_expr_cptr = arg;
					} else if (1 == count1) {
						index_expr_cptr = arg;
					} else {
						break;
					}
					count1 += 1;
				}
				if (!index_expr_cptr) {
					assert(false);
					return;
				}
				//auto array_expr_cptr = ASE->getCallee();
				if (index_expr_cptr && array_expr_cptr) {
					auto index_cs_iter = state1.m_expr_conversion_state_map.find(index_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == index_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*index_expr_cptr, m_Rewrite);
						index_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& index_cs_shptr_ref = (*index_cs_iter).second;

					auto array_cs_iter = state1.m_expr_conversion_state_map.find(array_expr_cptr);
					if (state1.m_expr_conversion_state_map.end() == array_cs_iter) {
						std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*array_expr_cptr, m_Rewrite);
						array_cs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
					auto& array_cs_shptr_ref = (*array_cs_iter).second;

					std::string UO_replacement_text;
					if ("Dual" == ConvertMode) {
						UO_replacement_text = "(mse::make_nullable_any_random_access_iterator(std::begin(" + (*array_cs_shptr_ref).m_current_text_str + ")) + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					} else if ("FasterAndStricter" == ConvertMode) {
						if (true) {
							return;
						} else {
							UO_replacement_text = (*this).m_Rewrite.getRewrittenText(UOSR);
							if ("" == UO_replacement_text) {
								UO_replacement_text = "&(" + (*array_cs_shptr_ref).m_current_text_str + "[" + (*index_cs_shptr_ref).m_current_text_str + "])";
							}
						}
					} else {
						UO_replacement_text = "(mse::make_nullable_any_random_access_iterator(std::begin(" + (*array_cs_shptr_ref).m_current_text_str + ")) + (" + (*index_cs_shptr_ref).m_current_text_str + "))";
					}

					if (ConvertToSCPP) {
						std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(UO_replacement_text);
						(*uocs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
						(*uocs_shptr_ref).update_current_text();

						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).m_current_text_str);
						//(*this).m_Rewrite.ReplaceText(UOSR, (*uocs_shptr_ref).m_current_text_str);
					}
				}

			}
		}
	}

	void CUpdateIndirectFunctionTypeParamsArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const clang::CallExpr* CE = m_CE;
		if (CE) {
			auto res1 = state1.m_ddecl_conversion_state_map.insert(*((*this).m_indirect_function_DD));
			auto ddcs_map_iter = res1.first;
			auto& ddcs_ref = (*ddcs_map_iter).second;
			//ddcs_ref.current_direct_qtype_ref();

			assert(ddcs_ref.direct_type_state_ref().current_qtype_if_any().has_value());
			if (llvm::isa<const clang::FunctionType>(ddcs_ref.direct_type_state_ref().current_qtype_if_any().value())) {
				auto FNQT = llvm::cast<const clang::FunctionType>(ddcs_ref.direct_type_state_ref().current_qtype_if_any().value());
				std::string new_function_type_code = FNQT->getReturnType().getAsString();
				new_function_type_code += "(";

				for (size_t i = 0; (i < CE->getNumArgs()); i += 1) {
					if (1 <= i) {
						new_function_type_code += ", ";
					}

					auto arg = CE->getArg(i);
					auto rhs_res2 = infer_array_type_info_from_stmt(*arg, "", state1);
					bool rhs_is_an_indirect_type = is_an_indirect_type(arg->getType());
					if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
						update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, state1);
					}

					if (rhs_res2.ddecl_cptr && rhs_res2.ddecl_conversion_state_ptr) {
						if (false) {
							CIndirectionStateStack indirection_state_stack;
							for (size_t i = rhs_res2.indirection_level; (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size() > i; i += 1) {
								indirection_state_stack.push_back((*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.at(i));
							}
							assert((*(rhs_res2.ddecl_conversion_state_ptr)).direct_type_state_ref().current_qtype_if_any().has_value());
							auto rhs_direct_qtype = (*(rhs_res2.ddecl_conversion_state_ptr)).direct_type_state_ref().current_qtype_if_any().value();

							bool rhs_is_a_function_parameter = true;

							std::string rhs_direct_qtype_str = (*(rhs_res2.ddecl_conversion_state_ptr)).current_direct_qtype_str();

							if ("_Bool" == rhs_direct_qtype_str) {
								rhs_direct_qtype_str = "bool";
							} else if ("const _Bool" == rhs_direct_qtype_str) {
								rhs_direct_qtype_str = "const bool";
							}
							auto rhs_non_const_direct_qtype = rhs_direct_qtype;
							rhs_non_const_direct_qtype.removeLocalConst();
							std::string rhs_non_const_direct_qtype_str = rhs_non_const_direct_qtype.getAsString();
							if ("_Bool" == rhs_non_const_direct_qtype_str) {
								rhs_non_const_direct_qtype_str = "bool";
							}

							auto res3 = generate_type_indirection_prefix_and_suffix(indirection_state_stack, Rewrite, 
								rhs_is_a_function_parameter);

							if (res3.m_direct_type_must_be_non_const) {
								rhs_direct_qtype_str = rhs_non_const_direct_qtype_str;
							}

							std::string new_param_type_str = res3.m_prefix_str + rhs_direct_qtype_str + res3.m_suffix_str;

							new_function_type_code += new_param_type_str;
						} else {
							auto res3 = generate_type_indirection_prefix_and_suffix((*rhs_res2.ddecl_conversion_state_ptr).m_indirection_state_stack, Rewrite, 
								true/*rhs_is_a_function_parameter*/);

							auto rhs_direct_qtype_str = (*rhs_res2.ddecl_conversion_state_ptr).current_direct_qtype_str();

							std::string new_param_type_str = res3.m_prefix_str + rhs_direct_qtype_str + res3.m_suffix_str;

							new_function_type_code += new_param_type_str;
						}
					} else {
						new_function_type_code += arg->getType().getAsString();
						int q = 7;
					}
				}
				new_function_type_code += ")";

				ddcs_ref.set_current_direct_qtype_str(new_function_type_code);

				update_declaration(*((*this).m_indirect_function_DD), (*this).m_Rewrite, state1);
			} else {
				int q = 7;
			}
		} else {
			int q = 7;
		}
	}

	void CTargetConstrainsCStyleCastExprArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		assert((*this).m_ddecl_indirection.m_ddecl_cptr);
		assert((*this).m_c_style_cast_expr_cptr);
		auto res1 = generate_declaration_replacement_code((*this).m_ddecl_indirection.m_ddecl_cptr, Rewrite, state1.m_ddecl_conversion_state_map);

		if (true || res1.m_changed_from_original) {
			auto whole_cast_expression_SR = write_once_source_range({ m_c_style_cast_expr_cptr->getBeginLoc(), m_c_style_cast_expr_cptr->getEndLoc() });
			auto cast_operation_SR = write_once_source_range({ m_c_style_cast_expr_cptr->getLParenLoc(), m_c_style_cast_expr_cptr->getRParenLoc() });
			auto cast_target_expression_SR = write_once_source_range({ m_c_style_cast_expr_cptr->getRParenLoc().getLocWithOffset(+1), m_c_style_cast_expr_cptr->getEndLoc() });

			if (whole_cast_expression_SR.isValid()) {
				if (ConvertToSCPP) {
					IF_DEBUG(auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);)
					IF_DEBUG(auto whole_cast_expression_text = Rewrite.getRewrittenText(whole_cast_expression_SR);)
					auto cast_target_expression_text = Rewrite.getRewrittenText(cast_target_expression_SR);

					auto replacement_qtype_str = res1.m_replacement_type_str;
					/* remove trailing whitespace */
					while (!replacement_qtype_str.empty()) {
						if (isspace(replacement_qtype_str.back())) {
							replacement_qtype_str.pop_back();
						} else {
							break;
						}
					}

					std::string whole_cast_expression_replacement_text;
					std::string cast_target_expression_extra_prefix;
					std::string cast_target_expression_extra_suffix;
					if ((*this).m_ddecl_indirection.m_ddecl_cptr->getType()->isPointerType() && m_c_style_cast_expr_cptr->getSubExpr()->getType()->isPointerType()) {
						const std::string og_target_pointee_qtype_str = (*this).m_ddecl_indirection.m_ddecl_cptr->getType()->getPointeeType().getAsString();
						const std::string og_csce_pointee_qtype_str = m_c_style_cast_expr_cptr->getSubExpr()->getType()->getPointeeType().getAsString();
						const bool og_target_is_char_star = ("char" == og_target_pointee_qtype_str) || ("const char" == og_target_pointee_qtype_str);
						const bool og_target_is_unsigned_char_star = ("unsigned char" == og_target_pointee_qtype_str) || ("const unsigned char" == og_target_pointee_qtype_str);
						const bool og_csce_is_char_star = ("char" == og_csce_pointee_qtype_str) || ("const char" == og_csce_pointee_qtype_str);
						const bool og_csce_is_unsigned_char_star = ("unsigned char" == og_csce_pointee_qtype_str) || ("const unsigned char" == og_csce_pointee_qtype_str);

						/* Currently, we leave 'char*'s as raw pointers, so (C-style) casting between them
						and pointers that have been converted to (safe) pointer/iterator objects might not
						work without some massaging. */
						if (og_target_is_unsigned_char_star && og_csce_is_char_star) {
							/* We're (unsafely) casting from a 'char*' to a (safe) pointer object to an 'unsigned char'. */
							std::string const_qual_str;
							if (m_c_style_cast_expr_cptr->getSubExpr()->getType()->getPointeeType().isConstQualified()) {
								const_qual_str += "const ";
							}
							if ("Dual" == ConvertMode) {
								cast_target_expression_extra_prefix = "MSE_LH_CAST(MSE_LH_ARRAY_ITERATOR_TYPE(";
								cast_target_expression_extra_prefix += const_qual_str + "char), ";
							} else if ("FasterAndStricter" == ConvertMode) {
								cast_target_expression_extra_prefix = "mse::TXScopeCSSSXSTERAIterator<";
								cast_target_expression_extra_prefix += const_qual_str + "char>(";
							} else {
								cast_target_expression_extra_prefix = "mse::lh::TLHNullableAnyRandomAccessIterator<";
								cast_target_expression_extra_prefix += const_qual_str + "char>(";
							}
							cast_target_expression_extra_suffix = ")";
						} else if (og_target_is_char_star) {
							/* We're (unsafely) casting to a 'char*'. */
							std::string addressof_str;
							if ("Dual" == ConvertMode) {
								addressof_str = "MSE_LH_UNSAFE_MAKE_RAW_POINTER_TO";
							} else {
								addressof_str = "std::addressof";
							}
							if (og_csce_is_char_star) {
								addressof_str = "&";
							}
							whole_cast_expression_replacement_text = "(" + og_target_pointee_qtype_str
								+ "*)" + addressof_str + "(*(" + cast_target_expression_text + "))";
						}
					}
					if (whole_cast_expression_replacement_text.empty()) {
						if ("Dual" == ConvertMode) {
							whole_cast_expression_replacement_text = "MSE_LH_UNSAFE_CAST(" + replacement_qtype_str
								+ ", " + cast_target_expression_extra_prefix + cast_target_expression_text
								+ cast_target_expression_extra_suffix + ")";
						} else {
							whole_cast_expression_replacement_text = "mse::us::lh::unsafe_cast<" + replacement_qtype_str
								+ ">(" + cast_target_expression_extra_prefix + cast_target_expression_text
								+ cast_target_expression_extra_suffix+ ")";
						}
					}
					/* This is not the proper way to modify an expression. See the function
					* CConditionalOperatorReconciliation2ReplacementAction::do_replacement() for an example of
					* the proper way to do it. But for now this is good enough. */
					state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite
						, whole_cast_expression_SR, whole_cast_expression_replacement_text);
				}
			} else {
				int q = 5;
				assert(false);
			}
		}
	}

	void CConditionalOperatorReconciliation2ReplacementAction::do_replacement(CTUState& state1) const {
		const clang::ConditionalOperator* CO = m_CO;
		const Expr* COND = nullptr;
		const Expr* LHS = nullptr;
		const Expr* RHS = nullptr;
		if (CO) {
			COND = CO->getCond();
			LHS = CO->getLHS();
			RHS = CO->getRHS();
		}
		const DeclaratorDecl* lhs_DD = m_lhs_DD;
		const DeclaratorDecl* rhs_DD = m_rhs_DD;
		if ((COND != nullptr) && (LHS != nullptr) && (RHS != nullptr) && (lhs_DD != nullptr) && (rhs_DD != nullptr)) {
			bool lhs_is_array = false;
			bool lhs_is_dynamic_array = false;
			bool lhs_is_native_array = false;

			auto COSR = nice_source_range(CO->getSourceRange(), (*this).m_Rewrite);
			auto cond_SR = nice_source_range(COND->getSourceRange(), (*this).m_Rewrite);
			auto lhs_SR = nice_source_range(LHS->getSourceRange(), (*this).m_Rewrite);
			auto rhs_SR = nice_source_range(RHS->getSourceRange(), (*this).m_Rewrite);
			if ((COSR.isValid()) && (cond_SR.isValid()) && (lhs_SR.isValid()) && (rhs_SR.isValid())) {
				auto res1 = state1.m_ddecl_conversion_state_map.insert(*lhs_DD);
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				/* At the moment we only support the case where the value option expressions are
				* just declared variables. */
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						lhs_is_dynamic_array = true;
						lhs_is_array = true;
					} else if ("native array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						lhs_is_native_array = true;
						lhs_is_array = true;
					} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						lhs_is_array = true;
					}
				} else {
					int q = 3;
				}
			}

			bool rhs_is_array = false;
			bool rhs_is_dynamic_array = false;
			bool rhs_is_native_array = false;
			{
				auto res1 = state1.m_ddecl_conversion_state_map.insert(*rhs_DD);
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						rhs_is_dynamic_array = true;
						rhs_is_array = true;
					} else if ("native array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						rhs_is_native_array = true;
						rhs_is_array = true;
					} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
						rhs_is_array = true;
					}
				} else {
					int q = 3;
				}
			}
			if (lhs_is_array && rhs_is_array) {
				if (m_var_DD) {
					update_declaration(*m_var_DD, (*this).m_Rewrite, state1);
				}

				bool lhs_needs_to_be_wrapped = false;
				if ((lhs_is_dynamic_array && (!rhs_is_dynamic_array)) || (lhs_is_native_array && (!rhs_is_native_array))) {
					lhs_needs_to_be_wrapped = true;
				}
				bool rhs_needs_to_be_wrapped = false;
				if ((rhs_is_dynamic_array && (!lhs_is_dynamic_array)) || (rhs_is_native_array && (!lhs_is_native_array))) {
					rhs_needs_to_be_wrapped = true;
				}
				auto cocs_iter = state1.m_expr_conversion_state_map.end();
				if (lhs_needs_to_be_wrapped || rhs_needs_to_be_wrapped) {
					cocs_iter = state1.m_expr_conversion_state_map.find(CO);
					if (state1.m_expr_conversion_state_map.end() == cocs_iter) {
						auto shptr1 = make_expr_conversion_state_shared_ptr<CConditionalOperatorExprConversionState>(*CO, m_Rewrite);
						cocs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
					}
				}

				if (lhs_needs_to_be_wrapped) {
					assert(state1.m_expr_conversion_state_map.end() != cocs_iter);
					auto lhscs_iter = state1.m_expr_conversion_state_map.find(LHS);
					if (state1.m_expr_conversion_state_map.end() != lhscs_iter) {
						auto& lhscs_shptr_ref = (*lhscs_iter).second;
						bool already_wrapped_flag = false;
						if (1 <= (*lhscs_shptr_ref).m_expr_text_modifier_stack.size()) {
							auto& last_modifier_ref = (*(*lhscs_shptr_ref).m_expr_text_modifier_stack.back());
							if ("nullable any random access iter cast" == last_modifier_ref.species_str()) {
								already_wrapped_flag = true;
							}
						}
						if (ConvertToSCPP && (!already_wrapped_flag)) {
							auto shptr1 = std::make_shared<CNullableAnyRandomAccessIterCastExprTextModifier>(LHS->getType()->getPointeeType());
							(*lhscs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
							(*lhscs_shptr_ref).update_current_text();
						}
					} else {
						int q = 7;
					}
				}
				if (rhs_needs_to_be_wrapped) {
					assert(state1.m_expr_conversion_state_map.end() != cocs_iter);
					auto rhscs_iter = state1.m_expr_conversion_state_map.find(RHS);
					if (state1.m_expr_conversion_state_map.end() != rhscs_iter) {
						auto& rhscs_shptr_ref = (*rhscs_iter).second;
						bool already_wrapped_flag = false;
						if (1 <= (*rhscs_shptr_ref).m_expr_text_modifier_stack.size()) {
							auto& last_modifier_ref = (*(*rhscs_shptr_ref).m_expr_text_modifier_stack.back());
							if ("nullable any random access iter cast" == last_modifier_ref.species_str()) {
								already_wrapped_flag = true;
							}
						}
						if (ConvertToSCPP && (!already_wrapped_flag)) {
							auto shptr1 = std::make_shared<CNullableAnyRandomAccessIterCastExprTextModifier>(RHS->getType()->getPointeeType());
							(*rhscs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
							(*rhscs_shptr_ref).update_current_text();
						}
					} else {
						int q = 7;
					}
				}

				if (m_var_DD && (true)) {
					assert(state1.m_expr_conversion_state_map.end() != cocs_iter);
					auto& cocs_shptr_ref = (*(*cocs_iter).second);
					cocs_shptr_ref.update_current_text();

					auto res1 = state1.m_ddecl_conversion_state_map.insert(*m_var_DD);
					auto ddcs_map_iter = res1.first;
					auto& ddcs_ref = (*ddcs_map_iter).second;

					ddcs_ref.m_current_initialization_expr_str = cocs_shptr_ref.m_current_text_str;

					update_declaration(*m_var_DD, m_Rewrite, state1);
				}

			}
		}
	}


	/**********************************************************************************************************************/

	class MCSSSRecordDecl : public MatchFinder::MatchCallback
	{
	public:
		MCSSSRecordDecl (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const RecordDecl* RD = MR.Nodes.getNodeAs<clang::RecordDecl>("mcsssrecorddecl");
			//const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssrecorddecl");
			//const Expr* RHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssrecorddecl2");
			//const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssrecorddecl3");
			//const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssrecorddecl4");

			if ((RD != nullptr))
			{
				auto SR = nice_source_range(RD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(RD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				std::string name = RD->getNameAsString();

				auto qualified_name = RD->getQualifiedNameAsString();
				static const std::string mse_namespace_str1 = "mse::";
				static const std::string mse_namespace_str2 = "::mse::";
				if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
						|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
					int q = 5;
					//return;
				}

				if (RD->isThisDeclarationADefinition()) {
					if (false) {
						auto res1 = (*this).m_state1.m_recdecl_conversion_state_map.insert(*RD, Rewrite);
						auto rdcs_map_iter = res1.first;
						if ((*this).m_state1.m_recdecl_conversion_state_map.end() == rdcs_map_iter) {
							return;
						}
						auto& rdcs_ref = (*rdcs_map_iter).second;
						//bool update_declaration_flag = res1.second;
					}
					{
						auto res1 = (*this).m_state1.m_recdecl_map.insert(*RD, Rewrite);
						auto rdcs_map_iter = res1.first;
						if ((*this).m_state1.m_recdecl_map.end() == rdcs_map_iter) {
							return;
						}
						auto& rdcs_ref = (*rdcs_map_iter).second;
						//bool update_declaration_flag = res1.second;
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

	/**********************************************************************************************************************/

	struct CAllocFunctionInfo {
		bool m_seems_to_be_some_kind_of_malloc_or_realloc = false;
		bool m_seems_to_be_some_kind_of_realloc = false;
		clang::CallExpr::const_arg_iterator m_num_bytes_arg_iter;
		std::string m_num_bytes_arg_source_text;
		std::string m_realloc_pointer_arg_source_text;
	};
	CAllocFunctionInfo analyze_malloc_resemblance(const clang::CallExpr& call_expr, Rewriter &Rewrite) {
		CAllocFunctionInfo retval;

		auto CE = &call_expr;
		auto function_decl = CE->getDirectCallee();
		auto num_args = CE->getNumArgs();
		if (function_decl && ((1 == num_args) || (2 == num_args))) {
			std::string function_name = function_decl->getNameAsString();
			static const std::string alloc_str = "alloc";
			static const std::string realloc_str = "realloc";
			auto lc_function_name = tolowerstr(function_name);
			bool ends_with_alloc = ((lc_function_name.size() >= alloc_str.size())
					&& (0 == lc_function_name.compare(lc_function_name.size() - alloc_str.size(), alloc_str.size(), alloc_str)));
			bool ends_with_realloc = (ends_with_alloc && (lc_function_name.size() >= realloc_str.size())
					&& (0 == lc_function_name.compare(lc_function_name.size() - realloc_str.size(), realloc_str.size(), realloc_str)));
			bool still_potentially_valid1 = (ends_with_alloc && (1 == num_args)) || (ends_with_realloc && (2 == num_args));
			if (still_potentially_valid1) {
				std::string realloc_pointer_arg_source_text;
				auto arg_iter = CE->arg_begin();

				if (ends_with_realloc) {
					auto arg_source_range = nice_source_range((*arg_iter)->getSourceRange(), Rewrite);
					if (arg_source_range.isValid()) {
						realloc_pointer_arg_source_text = Rewrite.getRewrittenText(arg_source_range);
					}

					arg_iter++;
				}
				bool argIsIntegerType = false;
				if (*arg_iter) {
					argIsIntegerType = (*arg_iter)->getType()->isIntegerType();
				}
				if (argIsIntegerType) {
					auto arg_source_range = nice_source_range((*arg_iter)->getSourceRange(), Rewrite);
					std::string arg_source_text;
					if (arg_source_range.isValid()) {
						arg_source_text = Rewrite.getRewrittenText(arg_source_range);
						//auto arg_source_text_sans_ws = with_whitespace_removed(arg_source_text);

						bool asterisk_found = false;
						auto sizeof_start_index = arg_source_text.find("sizeof(");
						if (std::string::npos != sizeof_start_index) {
							auto sizeof_end_index = arg_source_text.find(")", sizeof_start_index);
							if (std::string::npos != sizeof_end_index) {
								assert(sizeof_end_index > sizeof_start_index);
								std::string before_str = arg_source_text.substr(0, sizeof_start_index);
								std::string after_str;
								if (sizeof_end_index + 1 < arg_source_text.size()) {
									after_str = arg_source_text.substr(sizeof_end_index + 1);
								}

								auto index = before_str.size() - 1;
								while (0 <= index) {
									if ('*' == before_str[index]) {
										asterisk_found = true;
									}
									if (!std::isspace(before_str[index])) {
										break;
									}

									index -= 1;
								}
								if (asterisk_found) {
									before_str = before_str.substr(0, index);
								} else {
									size_t index2 = 0;
									while (after_str.size() > index2) {
										if ('*' == after_str[index2]) {
											asterisk_found = true;
										}
										if (!std::isspace(after_str[index2])) {
											break;
										}

										index2 += 1;
									}
									if (asterisk_found) {
										after_str = after_str.substr(index2 + 1);
									}
								}
							}
						}
						if (true || asterisk_found) {
							retval.m_num_bytes_arg_iter = arg_iter;
							retval.m_seems_to_be_some_kind_of_malloc_or_realloc = true;
							retval.m_num_bytes_arg_source_text = arg_source_text;
							if (ends_with_realloc) {
								retval.m_seems_to_be_some_kind_of_realloc = true;
								retval.m_realloc_pointer_arg_source_text = realloc_pointer_arg_source_text;
							}
						}
					}
				}
			}
		}
		return retval;
	}

	class MCSSSVarDecl2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSVarDecl2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssvardecl");
			const Expr* RHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssvardecl2");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssvardecl3");
			//const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssvardecl4");

			if ((DD != nullptr))
			{
				auto SR = nice_source_range(DD->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":56:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DD->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				std::string variable_name = DD->getNameAsString();

				auto qualified_name = DD->getQualifiedNameAsString();
				static const std::string mse_namespace_str1 = "mse::";
				static const std::string mse_namespace_str2 = "::mse::";
				if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
						|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
					int q = 5;
					//return;
				} else {
					auto res1 = (*this).m_state1.m_ddecl_conversion_state_map.insert(*DD);
					auto ddcs_map_iter = res1.first;
					auto& ddcs_ref = (*ddcs_map_iter).second;
					//bool update_declaration_flag = res1.second;

					for (size_t i = 0; (i < ddcs_ref.m_indirection_state_stack.size()); i += 1) {
						if ("native array" == ddcs_ref.m_indirection_state_stack.at(i).current_species()) {
							m_state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*DD, i));
						}
					}

					if (nullptr != RHS) {
						auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
						bool lhs_is_an_indirect_type = is_an_indirect_type(DD->getType());
						bool rhs_is_an_indirect_type = is_an_indirect_type(RHS->getType());
						if (lhs_is_an_indirect_type != rhs_is_an_indirect_type) {
							int q = 5;
						}

						if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
							update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
						}

						if ((nullptr != CCE) && (rhs_res2.ddecl_conversion_state_ptr)) {
							auto cce_QT = CCE->getType();
							auto rhs_QT = RHS->getType();
							if (cce_QT == rhs_QT) {
								CIndirectionStateStack rhs_qtype_indirection_state_stack;
								auto direct_rhs_qtype = populateQTypeIndirectionStack(rhs_qtype_indirection_state_stack, rhs_QT);
								auto direct_rhs_qtype_str = direct_rhs_qtype.getAsString();
								auto casted_expr_ptr = CCE->IgnoreCasts();
								if (llvm::isa<const clang::CallExpr>(casted_expr_ptr->IgnoreParenCasts())) {
									auto CE = llvm::cast<const clang::CallExpr>(casted_expr_ptr->IgnoreParenCasts());
									auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
									if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
										/* This seems to be some kind of malloc/realloc function. These case should not be
										* handled here. They are handled elsewhere. */
										return;
									}
								}

								if ((rhs_qtype_indirection_state_stack.size() + rhs_res2.indirection_level
										== (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
										&& (1 <= (*rhs_res2.ddecl_conversion_state_ptr).m_indirection_state_stack.size())
										&& (nullptr != casted_expr_ptr)) {

									std::string rhs_ddecl_current_direct_qtype_str = (*rhs_res2.ddecl_conversion_state_ptr).current_direct_qtype_str();
									auto casted_expr_SR = nice_source_range(casted_expr_ptr->getSourceRange(), Rewrite);
									auto CCESR = nice_source_range(CCE->getSourceRange(), Rewrite);
									auto cast_operation_SR = clang::SourceRange(CCE->getLParenLoc(), CCE->getRParenLoc());

									if (cast_operation_SR.isValid()
											&& (("void" == rhs_ddecl_current_direct_qtype_str) || ("const void" == rhs_ddecl_current_direct_qtype_str))) {
										if (ConvertToSCPP) {
											(*rhs_res2.ddecl_conversion_state_ptr).set_current_direct_qtype(direct_rhs_qtype);

											auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
											/* This is not the proper way to modify an expression. See the function
											* CConditionalOperatorReconciliation2ReplacementAction::do_replacement() for an example of
											* the proper way to do it. But for now this is good enough. */
											m_state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, "");
											//auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");

											static const std::string void_str = "void";
											auto void_pos = (*rhs_res2.ddecl_conversion_state_ptr).m_current_initialization_expr_str.find(void_str);
											if (std::string::npos != void_pos) {
												(*rhs_res2.ddecl_conversion_state_ptr).m_current_initialization_expr_str.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
											}

											update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
										}
									} else {
										if (ConvertToSCPP) {
											if (false) {
												(*rhs_res2.ddecl_conversion_state_ptr).set_current_direct_qtype(direct_rhs_qtype);
											}
										}
									}
								} else {
									int q = 7;
								}
							} else {
								int q = 7;
							}
						}

						int lhs_indirection_level_adjustment = 0;
						auto rhs_res3 = leading_addressof_operator_info_from_stmt(*RHS);
						if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
							assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

							RHS = rhs_res3.without_leading_addressof_operator_expr_cptr;
							rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
							lhs_indirection_level_adjustment += 1;
						}

						if (llvm::isa<const clang::CallExpr>(RHS->IgnoreParenCasts())) {
							auto CE = llvm::cast<const clang::CallExpr>(RHS->IgnoreParenCasts());
							auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
							if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
								/* This seems to be some kind of malloc/realloc function. These case should not be
								* handled here. They are handled elsewhere. */
								return;
							}
						}

						if (ConvertToSCPP && (rhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
							for (size_t i = 0; (rhs_res2.indirection_level + i < ddcs_ref.m_indirection_state_stack.size())
														&& (rhs_res2.indirection_level + i < (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size()); i += 1) {
								{
									/* Here we're establishing and "enforcing" the constraint that the rhs value must
									* be of an (array) type that can be assigned to the lhs. */
									auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0 + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
									auto cr_shptr2 = std::make_shared<CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0 + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));

									if (ddcs_ref.has_been_determined_to_be_an_array(0 + i)) {
										(*cr_shptr).do_replacement(m_state1);
										if (ddcs_ref.has_been_determined_to_be_a_dynamic_array(0 + i)) {
											(*cr_shptr2).do_replacement(m_state1);
										} else {
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
										}
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
									}
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*DD, 0 + i));

									if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(rhs_res2.indirection_level + i)) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							}
						}
					}

					update_declaration(*DD, Rewrite, m_state1);
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

	/**********************************************************************************************************************/

	class MCSSSPointerArithmetic2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSPointerArithmetic2 (Rewriter &Rewrite, CTUState& state1)
	: Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcssspointerarithmetic");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcssspointerarithmetic2");
			const Expr* E = MR.Nodes.getNodeAs<clang::Expr>("mcssspointerarithmetic3");

			if ((DRE != nullptr) && (E != nullptr))
			{
				const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcssspointerarithmetic");

				auto SR = nice_source_range(DRE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":1558:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DRE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto decl = DRE->getDecl();
				auto DD = dyn_cast<const DeclaratorDecl>(decl);

				const clang::FieldDecl* FD = nullptr;
				if (nullptr != ME) {
					auto member_decl = ME->getMemberDecl();
					FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
				}
				if (nullptr != FD) {
					DD = FD;
				}

				if (!DD) {
					return;
				} else {
					auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
					if (!decl_source_range.isValid()) {
						return;
					}
					DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
					DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

					auto QT = DD->getType();
					std::string variable_name = DD->getNameAsString();

					auto qualified_name = DD->getQualifiedNameAsString();
					static const std::string mse_namespace_str1 = "mse::";
					static const std::string mse_namespace_str2 = "::mse::";
					if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
							|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
						return;
					}

					auto res2 = infer_array_type_info_from_stmt(*E, "pointer arithmetic", (*this).m_state1, DD);

					if (res2.update_declaration_flag) {
						update_declaration(*DD, Rewrite, m_state1);
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/**********************************************************************************************************************/

	class MCSSSNullToPointer : public MatchFinder::MatchCallback
	{
	public:
		MCSSSNullToPointer (Rewriter &Rewrite, CTUState& state1)
	: Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const Expr* E = MR.Nodes.getNodeAs<clang::Expr>("a");

			if ((E != nullptr))
			{
				auto SR = nice_source_range(E->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":889:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(E->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				if (ConvertToSCPP) {
					std::string null_value_str;
					if ("Dual" == ConvertMode) {
						null_value_str = "MSE_LH_NULL_POINTER";
					} else {
						null_value_str = "nullptr";
					}
					CExprTextReplacementAction(Rewrite, MR, E, null_value_str).do_replacement(m_state1);
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/**********************************************************************************************************************/

	class MCSSSAddressOf : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAddressOf (Rewriter &Rewrite, CTUState& state1)
	: Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssaddressof");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssaddressof2");
			const Expr* E = MR.Nodes.getNodeAs<clang::Expr>("mcsssaddressof3");
			const UnaryOperator* UO = MR.Nodes.getNodeAs<clang::UnaryOperator>("mcsssaddressof4");

			if ((DRE != nullptr) && (E != nullptr))
			{
				const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssaddressof");

				auto SR = nice_source_range(DRE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":6039:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DRE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				DEBUG_SOURCE_LOCATION_STR(expr_debug_source_location_str, E->getSourceRange(), MR);
				DEBUG_SOURCE_TEXT_STR(expr_debug_source_text, E->getSourceRange(), Rewrite);
				const clang::Expr* subE = nullptr;
				if (UO) {
					DEBUG_SOURCE_LOCATION_STR(uo_debug_source_location_str, UO->getSourceRange(), MR);
					DEBUG_SOURCE_TEXT_STR(uo_debug_source_text, UO->getSourceRange(), Rewrite);

					subE = UO->getSubExpr();
					if (subE) {
						if (subE->getSourceRange() == UO->getSourceRange()) {
							/* If the ('&') unary operator has the same source location as its
							subexpression, then we're going to assume this is just some implicit use of
							the operator (that doesn't concern us). We're doing this check for now
							because we don't know the proper way to determine if this use of the
							operator is implicit or not. */
							return;
						}
					} else {
						int q = 5;
					}

					auto D_UO = UO->getReferencedDeclOfCallee();
					if (D_UO) {
						DEBUG_SOURCE_LOCATION_STR(decl_uo_debug_source_location_str, D_UO->getSourceRange(), MR);
						DEBUG_SOURCE_TEXT_STR(decl_uo_debug_source_text, D_UO->getSourceRange(), Rewrite);
						IF_DEBUG(auto b1 = D_UO->isImplicit();)
						/* We assume this means that the ('&') operator has been explicitly overloaded.
						(This assumption needs to be verified.) We'll just leave this case alone for
						now. */
						return;
					}
				}

				auto decl = DRE->getDecl();
				auto DD = dyn_cast<const DeclaratorDecl>(decl);

				const clang::FieldDecl* FD = nullptr;
				if (nullptr != ME) {
					auto member_decl = ME->getMemberDecl();
					FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
				}
				if (nullptr != FD) {
					DD = FD;
				}

				if (!DD) {
					return;
				} else {
					auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
					if (!decl_source_range.isValid()) {
						return;
					}
					DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
					DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

#ifndef NDEBUG
					if (std::string::npos != decl_debug_source_location_str.find(":6086:")) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					auto QT = DD->getType();
					std::string variable_name = DD->getNameAsString();

					auto qualified_name = DD->getQualifiedNameAsString();
					static const std::string mse_namespace_str1 = "mse::";
					static const std::string mse_namespace_str2 = "::mse::";
					if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
							|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
						//return;
					}

					if (subE) {
						auto subE_ii = subE->IgnoreParenImpCasts();
						assert(subE_ii);
						if (clang::Stmt::StmtClass::ArraySubscriptExprClass == subE_ii->getStmtClass()) {
							assert(llvm::isa<const clang::ArraySubscriptExpr>(subE_ii));
							auto ASE = dyn_cast<const clang::ArraySubscriptExpr>(subE_ii);
							CAddressofArraySubscriptExprReplacementAction(Rewrite, MR,
									CDDeclIndirection(*DD, 0), *UO, *ASE).do_replacement(m_state1);
							return;
						} else if (clang::Stmt::StmtClass::CXXOperatorCallExprClass == subE_ii->getStmtClass()) {
							assert(llvm::isa<const clang::CXXOperatorCallExpr>(subE_ii));
							auto operator_subscript_expr_cptr = llvm::cast<const clang::CXXOperatorCallExpr>(subE_ii);
							if (clang::OverloadedOperatorKind::OO_Subscript == operator_subscript_expr_cptr->getOperator()) {
								CAddressofSubscriptOperatorCallExprReplacementAction(Rewrite, MR,
										CDDeclIndirection(*DD, 0), *UO, *operator_subscript_expr_cptr).do_replacement(m_state1);
								return;
							}
						} else {
							auto adjusted_DRE_ii = DRE->IgnoreParenImpCasts();
							if (ME) {
								adjusted_DRE_ii = ME->IgnoreParenImpCasts();
							}
							if (adjusted_DRE_ii != subE_ii) {
								/* for now we'll only support the case where the "address of" operator is
								applied directly to the DeclRefExpr (as opposed to, say, a dereference of the
								DeclRefExpr). */
								std::string adjusted_DRE_ii_source_text =  Rewrite.getRewrittenText(adjusted_DRE_ii->IgnoreParenImpCasts()->getSourceRange());
								std::string subE_ii_source_text =  Rewrite.getRewrittenText(subE_ii->getSourceRange());
								return;
							}
						}
					}

					auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*DD);
					auto ddcs_map_iter = res1.first;
					auto& ddcs_ref = (*ddcs_map_iter).second;
					bool update_declaration_flag = res1.second;

					int i_target_indirection_index = -1;
					if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
						i_target_indirection_index = int(ddcs_ref.m_indirection_state_stack.size()) - 1;
						while ((0 <= i_target_indirection_index)
							&& ("native reference" == ddcs_ref.m_indirection_state_stack.at(i_target_indirection_index).current_species())) {
							/* Since taking the address of a native reference actually takes the address of the
							reference's target, we adjust the indirection_index accordingly. */
							i_target_indirection_index -= 1;
						}
					}
					if (0 <= i_target_indirection_index) {
						ddcs_ref.m_indirection_state_stack.at(i_target_indirection_index).set_original_pointer_target_state("native pointer target");
						ddcs_ref.m_indirection_state_stack.at(i_target_indirection_index).set_current_pointer_target_state("pointer target");
					} else {
						ddcs_ref.direct_type_state_ref().set_original_pointer_target_state("native pointer target");
						ddcs_ref.direct_type_state_ref().set_current_pointer_target_state("pointer target");
					}

					size_t target_indirection_index = (0 <= i_target_indirection_index) ? i_target_indirection_index : CDDeclIndirection::no_indirection;
					m_state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*ddcs_ref.m_ddecl_cptr, target_indirection_index));

					update_declaration(*ddcs_ref.m_ddecl_cptr, Rewrite, m_state1);

					homogenize_redeclaration_types(ddcs_ref.m_ddecl_cptr, m_state1, Rewrite, MR);
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/**********************************************************************************************************************/

	class MCSSSMalloc2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMalloc2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcsssmalloc1");
			const Expr* LHS = nullptr;
			if (BO != nullptr) {
				LHS = BO->getLHS();
			}
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmalloc2");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmalloc3");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmalloc4");

			if ((BO != nullptr) && (LHS != nullptr) && (CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(BO->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DRE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */
					std::string num_elements_text/* = before_str + after_str*/;
					QualType QT;
					std::string element_type_str;
					clang::SourceRange decl_source_range;
					std::string variable_name;
					std::string bo_replacement_code;
					const clang::DeclaratorDecl* DD = nullptr;

					auto lhs_QT = LHS->getType();

					auto decl = DRE->getDecl();
					DD = dyn_cast<const DeclaratorDecl>(decl);
					auto VD = dyn_cast<const VarDecl>(decl);

					const clang::FieldDecl* FD = nullptr;
					if (nullptr != ME) {
						auto member_decl = ME->getMemberDecl();
						FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
					}
					if (nullptr != FD) {
						DD = FD;
					} else if (nullptr != VD) {
						DD = VD;
					} else {
						int q = 7;
					}

					if (nullptr != DD) {
						auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
						if (!decl_source_range.isValid()) {
							return;
						}
						DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
						DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

						QT = DD->getType();
						variable_name = DD->getNameAsString();

						auto qualified_name = DD->getQualifiedNameAsString();
						static const std::string mse_namespace_str1 = "mse::";
						static const std::string mse_namespace_str2 = "::mse::";
						if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
								|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
							return;
						}

						auto res2 = infer_array_type_info_from_stmt(*LHS, "malloc target", (*this).m_state1, DD);

						if (res2.update_declaration_flag) {
							update_declaration(*DD, Rewrite, m_state1);
						}

						const clang::Type* lhs_TP = lhs_QT.getTypePtr();
						auto lhs_type_str = lhs_QT.getAsString();

						std::string lhs_element_type_str;
						if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = element_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						} else if (llvm::isa<const clang::PointerType>(lhs_TP)) {
							auto TPP = llvm::cast<const clang::PointerType>(lhs_TP);
							assert(nullptr != TPP);
							auto target_type = TPP->getPointeeType();
							auto type_str = target_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						}
						if ("" != lhs_element_type_str) {
							auto lhs_source_range = nice_source_range(LHS->getSourceRange(), Rewrite);
							auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								if ("Dual" == ConvertMode) {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = MSE_LH_REALLOC(";
									bo_replacement_code += lhs_element_type_str + ", ";
									bo_replacement_code += alloc_function_info1.m_realloc_pointer_arg_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = mse::lh::reallocate(";
									bo_replacement_code += alloc_function_info1.m_realloc_pointer_arg_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = mse::lh::reallocate(";
									bo_replacement_code += alloc_function_info1.m_realloc_pointer_arg_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								}
							} else {
								if ("Dual" == ConvertMode) {
									bo_replacement_code = "MSE_LH_ALLOC(";
									bo_replacement_code += lhs_element_type_str + ", ";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									bo_replacement_code = "mse::lh::allocate(";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else {
									bo_replacement_code = "mse::lh::allocate(";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
								}
							}

							auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
							std::string decl_source_text;
							if (decl_source_range.isValid()) {
								IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
							} else {
								return;
							}

							if (ConvertToSCPP && decl_source_range.isValid() && (SR.isValid())
									&& (nullptr != res2.ddecl_conversion_state_ptr)) {
								auto cr_shptr = std::make_shared<CMallocArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), BO, bo_replacement_code);

								if (true || ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level))) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
							} else {
								int q = 7;
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

	class MCSSSMallocInitializer2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMallocInitializer2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssmallocinitializer1");
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmallocinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssmallocinitializer3");

			if ((DS != nullptr) && (CE != nullptr) && (DD != nullptr))
			{
				auto SR = nice_source_range(DS->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":2803:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DS->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */
					std::string num_elements_text/* = before_str + after_str*/;

					if (nullptr != DD) {
						auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
						if (!decl_source_range.isValid()) {
							return;
						}
						DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
						DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

						QualType QT = DD->getType();
						auto variable_name = DD->getNameAsString();

						auto qualified_name = DD->getQualifiedNameAsString();
						static const std::string mse_namespace_str1 = "mse::";
						static const std::string mse_namespace_str2 = "::mse::";
						if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
								|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
							return;
						}

						if (!is_an_indirect_type(DD->getType())) {
							return;
						}

						auto res1 = state1.m_ddecl_conversion_state_map.insert(*DD);
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						bool update_declaration_flag = res1.second;

						bool lhs_has_been_determined_to_be_an_array = false;
						if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
							ddcs_ref.set_indirection_current(0, "malloc target");
						} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
							ddcs_ref.set_indirection_current(0, "dynamic array");
							lhs_has_been_determined_to_be_an_array = true;
							//update_declaration_flag = true;
							state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*DD, 0));
						} else if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current_species()) {
							lhs_has_been_determined_to_be_an_array = true;
						} else {
							assert("native array" != ddcs_ref.m_indirection_state_stack.at(0).current_species());
						}

						const clang::Type* TP = QT.getTypePtr();
						auto lhs_type_str = QT.getAsString();

						std::string element_type_str;
						if (TP->isArrayType()) {
							auto ATP = llvm::cast<const clang::ArrayType>(TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = element_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								element_type_str = type_str;
							}
						} else if (TP->isPointerType()) {
							auto TPP = llvm::cast<const clang::PointerType>(TP);
							assert(nullptr != TPP);
							auto target_type = TPP->getPointeeType();
							auto type_str = target_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								element_type_str = type_str;
							}
						}
						if ("" != element_type_str) {
							std::string array_initializer_info_str;
							std::string pointer_initializer_info_str;

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								array_initializer_info_str = "MSE_LH_REALLOC(" + element_type_str + ", " + alloc_function_info1.m_realloc_pointer_arg_source_text;
								array_initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";

								if ("Dual" == ConvertMode) {
									array_initializer_info_str = "MSE_LH_REALLOC(" + element_type_str + ", " + alloc_function_info1.m_realloc_pointer_arg_source_text;
									array_initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									array_initializer_info_str = "mse::lh::reallocate(" + alloc_function_info1.m_realloc_pointer_arg_source_text;
									array_initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";
								} else {
									array_initializer_info_str = "mse::lh::reallocate(" + alloc_function_info1.m_realloc_pointer_arg_source_text;
									array_initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";
								}

								/* We're going to assume here that any "realloc()"ed memory is a (dynamic)
								array. */
								pointer_initializer_info_str = array_initializer_info_str;
							} else {
								if ("Dual" == ConvertMode) {
									array_initializer_info_str = "MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(" + element_type_str + ")";
									//array_initializer_info_str = "MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_ARRAY_ITERATOR_TYPE(" + element_type_str + ")";
									array_initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";

									pointer_initializer_info_str = "MSE_LH_ALLOC_POINTER1(" + element_type_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<" + element_type_str + "> >(";
									//array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TLHNullableAnyRandomAccessIterator<" + element_type_str + "> >(";
									array_initializer_info_str += alloc_function_info1.m_num_bytes_arg_source_text + ")";

									pointer_initializer_info_str = "mse::TRefCountingPointer<" + element_type_str + ">()";
								} else {
									array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<" + element_type_str + "> >(";
									//array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TLHNullableAnyRandomAccessIterator<" + element_type_str + "> >(";
									array_initializer_info_str += alloc_function_info1.m_num_bytes_arg_source_text + ")";

									pointer_initializer_info_str = "mse::lh::allocate<mse::TNullableAnyPointer<" + element_type_str + "> >()";
								}
							}

							auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
							std::string decl_source_text;
							if (decl_source_range.isValid()) {
								IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
							} else {
								return;
							}

							if (ConvertToSCPP && decl_source_range.isValid() && (SR.isValid())) {
								auto cr_shptr = std::make_shared<CInitializerArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), DS, array_initializer_info_str);

								if (lhs_has_been_determined_to_be_an_array) {
									(*cr_shptr).do_replacement(state1);
								} else {
									/* lhs has not (yet) been determined to be an array iterator. Here we apply the
									"replacement action" for non-array iterators. */
									CInitializerArray2ReplacementAction(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), DS, pointer_initializer_info_str).do_replacement(state1);

									/* But we'll queue up an action to overwrite that action if lhs is subsequently determined
									to be an array iterator. */
									state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}

								int q = 3;
							} else {
								int q = 7;
							}
						}
					}
					int q = 5;
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssmallocinitializer1");
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmallocinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssmallocinitializer3");

			if ((DS != nullptr) && (CE != nullptr) && (DD != nullptr))
			{
				auto SR = nice_source_range(DS->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DS->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */

					if (ConvertToSCPP && SR.isValid()) {
						auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
						/* This modification needs to be queued so that it will be executed after any other
						modifications that might affect the relevant part of the source text. */
						(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSNullInitializer : public MatchFinder::MatchCallback
	{
	public:
		MCSSSNullInitializer (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssnullinitializer1");
			const Expr* RHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssnullinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssnullinitializer3");

			if ((DS != nullptr) && (RHS != nullptr) && (DD != nullptr))
			{
				auto SR = nice_source_range(DS->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DS->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				Expr::NullPointerConstantKind kind = RHS->IgnoreParenCasts()->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
				if (clang::Expr::NPCK_NotNull != kind) {
					if (false) {
						{
							if (true) {
								if (true) {
									if (nullptr != DD) {
										auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
										if (!decl_source_range.isValid()) {
											return;
										}
										DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
										DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

										QualType QT = DD->getType();
										auto variable_name = DD->getNameAsString();

										auto qualified_name = DD->getQualifiedNameAsString();
										static const std::string mse_namespace_str1 = "mse::";
										static const std::string mse_namespace_str2 = "::mse::";
										if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
												|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
											return;
										}

										if (!is_an_indirect_type(DD->getType())) {
											return;
										}

										auto res1 = (*this).m_state1.m_ddecl_conversion_state_map.insert(*DD);
										auto ddcs_map_iter = res1.first;
										auto& ddcs_ref = (*ddcs_map_iter).second;
										bool update_declaration_flag = res1.second;

										const clang::Type* TP = QT.getTypePtr();
										auto lhs_type_str = QT.getAsString();

										std::string element_type_str;
										if (TP->isArrayType()) {
											auto ATP = llvm::cast<const clang::ArrayType>(TP);
											assert(nullptr != ATP);
											auto element_type = ATP->getElementType();
											auto type_str = element_type.getAsString();
											if (("char" != type_str) && ("const char" != type_str)) {
												element_type_str = type_str;
											}
										} else if (TP->isPointerType()) {
											auto TPP = llvm::cast<const clang::PointerType>(TP);
											assert(nullptr != TPP);
											auto target_type = TPP->getPointeeType();
											auto type_str = target_type.getAsString();
											if (("char" != type_str) && ("const char" != type_str)) {
												element_type_str = type_str;
											}
										}
										if ("" != element_type_str) {
											auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
											std::string decl_source_text;
											if (decl_source_range.isValid()) {
												IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
											} else {
												return;
											}

											if (ConvertToSCPP && decl_source_range.isValid() && (SR.isValid())) {
												std::string null_value_str;
												if ("Dual" == ConvertMode) {
													null_value_str = "MSE_LH_NULL_POINTER";
												} else {
													null_value_str = "nullptr";
												}
												auto cr_shptr = std::make_shared<CInitializerArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), DS, null_value_str);

												if (true || (ddcs_ref.has_been_determined_to_be_an_array(0))) {
													(*cr_shptr).do_replacement(m_state1);
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}

												int q = 3;
											} else {
												int q = 7;
											}
										}
									}
									int q = 5;
								}
							} else {
								int q = 5;
							}
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

	class MCSSSFree2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFree2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfree1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfree3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":3021:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (1 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string free_str = "free";
						auto lc_function_name = tolowerstr(function_name);
						bool ends_with_free = ((lc_function_name.size() >= free_str.size())
								&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));
						if (ends_with_free) {
							auto arg_iter = CE->arg_begin();
							assert((*arg_iter)->getType().getTypePtrOrNull());
							auto arg_source_range = nice_source_range((*arg_iter)->getSourceRange(), Rewrite);
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								arg_source_text = Rewrite.getRewrittenText(arg_source_range);
								//auto arg_source_text_sans_ws = with_whitespace_removed(arg_source_text);

								auto ARG = (*(arg_iter))->IgnoreParenCasts();
								auto arg_res2 = infer_array_type_info_from_stmt(*ARG, "malloc target", state1);
								bool arg_is_an_indirect_type = is_an_indirect_type(ARG->getType());

								if (arg_res2.update_declaration_flag) {
									update_declaration(*(arg_res2.ddecl_cptr), Rewrite, state1);
								}

								auto arg_QT = ARG->getType();
								const clang::Type* arg_TP = arg_QT.getTypePtr();
								auto arg_type_str = arg_QT.getAsString();

								std::string arg_element_type_str;
								if (arg_TP->isArrayType()) {
									if (llvm::isa<const clang::ArrayType>(arg_TP)) {
										auto ATP = llvm::cast<const clang::ArrayType>(arg_TP);
										assert(nullptr != ATP);
										auto element_type = ATP->getElementType();
										auto type_str = element_type.getAsString();
										if (("char" != type_str) && ("const char" != type_str)) {
											arg_element_type_str = type_str;
										}
										auto element_type2 = arg_TP->getArrayElementTypeNoTypeQual();
										if (element_type.getTypePtr() != element_type2) {
											int q = 5;
										}
									} else {
										int q = 5;
									}
								} else if (arg_TP->isPointerType()) {
									if (llvm::isa<const clang::PointerType>(arg_TP)) {
										auto TPP = llvm::cast<const clang::PointerType>(arg_TP);
										assert(nullptr != TPP);
									} else {
										int q = 5;
									}
									auto target_type = arg_TP->getPointeeType();
									auto type_str = target_type.getAsString();
									if ((("char" != type_str) && ("const char" != type_str)) || (!llvm::isa<const clang::PointerType>(arg_TP))) {
										arg_element_type_str = type_str;
									}
								}

								if ("" != arg_element_type_str) {
									if (ConvertToSCPP && (arg_res2.ddecl_conversion_state_ptr) && arg_is_an_indirect_type) {
										auto arg_source_text = Rewrite.getRewrittenText(arg_source_range);
										//std::string ce_replacement_code = "(" + arg_source_text + ").resize(0)";
										std::string ce_replacement_code;

										if ("Dual" == ConvertMode) {
											ce_replacement_code = "MSE_LH_FREE(" + arg_source_text + ")";
										} else if ("FasterAndStricter" == ConvertMode) {
											ce_replacement_code = "mse::lh::impl::CAllocF<typename std::remove_reference<decltype(" + arg_source_text + ")>::type>::free(" + arg_source_text + ")";
										} else {
											ce_replacement_code = "mse::lh::impl::CAllocF<typename std::remove_reference<decltype(" + arg_source_text + ")>::type>::free(" + arg_source_text + ")";
										}

										auto cr_shptr = std::make_shared<CFreeDynamicArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(arg_res2.ddecl_cptr), arg_res2.indirection_level), CE, ce_replacement_code);

										if (true || ((*(arg_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(arg_res2.indirection_level))) {
											(*cr_shptr).do_replacement(state1);
										} else {
											state1.m_array2_contingent_replacement_map.insert(cr_shptr);
											//state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									}
								}
								int q = 5;
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfree1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfree3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (1 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string free_str = "free";
						auto lc_function_name = tolowerstr(function_name);
						bool ends_with_free = ((lc_function_name.size() >= free_str.size())
								&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));
						if (ends_with_free) {
							auto arg_iter = CE->arg_begin();
							assert((*arg_iter)->getType().getTypePtrOrNull());
							auto arg_source_range = nice_source_range((*arg_iter)->getSourceRange(), Rewrite);
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)

								auto ARG = (*(arg_iter))->IgnoreParenCasts();
								auto arg_res2 = infer_array_type_info_from_stmt(*ARG, "malloc target", (*this).m_state1);
								bool arg_is_an_indirect_type = is_an_indirect_type(ARG->getType());

								if (arg_res2.update_declaration_flag) {
									update_declaration(*(arg_res2.ddecl_cptr), Rewrite, m_state1);
								}

								auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
								/* This modification needs to be queued so that it will be executed after any other
								modifications that might affect the relevant part of the source text. */
								(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);

								int q = 5;
							} else {
								int q = 5;
							}
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

	class MCSSSSetToNull2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSSetToNull2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcssssettonull1");
			const Expr* RHS = nullptr;
			const Expr* LHS = nullptr;
			if (BO != nullptr) {
				RHS = BO->getRHS();
				LHS = BO->getLHS();
			}
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcssssettonull3");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcssssettonull4");

			if ((BO != nullptr) && (RHS != nullptr) && (LHS != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(BO->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(BO->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
				if (false && (clang::Expr::NPCK_NotNull != kind)) {
					auto lhs_source_range = nice_source_range(LHS->getSourceRange(), Rewrite);
					std::string lhs_source_text;
					if (lhs_source_range.isValid()) {
						lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
						//auto lhs_source_text_sans_ws = with_whitespace_removed(lhs_source_text);

						auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "set to null", (*this).m_state1);
						bool lhs_is_an_indirect_type = is_an_indirect_type(LHS->getType());

						if (lhs_res2.update_declaration_flag) {
							update_declaration(*(lhs_res2.ddecl_cptr), Rewrite, m_state1);
						}

						auto lhs_QT = LHS->getType();
						const clang::Type* lhs_TP = lhs_QT.getTypePtr();
						auto lhs_type_str = lhs_QT.getAsString();

						std::string lhs_element_type_str;
						if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = element_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						} else if (llvm::isa<const clang::PointerType>(lhs_TP)) {
							auto TPP = llvm::cast<const clang::PointerType>(lhs_TP);
							assert(nullptr != TPP);
							auto target_type = TPP->getPointeeType();
							auto type_str = target_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						}

						if ("" != lhs_element_type_str) {
							if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
								auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
								std::string bo_replacement_code = "( (" + lhs_source_text + ") = typename std::remove_reference<decltype(" + lhs_source_text + ")>::type() )";

								auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level), BO, bo_replacement_code);

								if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(lhs_res2.indirection_level)) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
							}
						}
						int q = 5;
					} else {
						int q = 5;
					}
					int q = 5;
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSCompareWithNull2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSCompareWithNull2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcssscomparewithnull1");
			const Expr* RHS = nullptr;
			const Expr* LHS = nullptr;
			if (BO != nullptr) {
				RHS = BO->getRHS();
				LHS = BO->getLHS();
			}
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcssscomparewithnull3");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcssscomparewithnull4");

			if (false && (BO != nullptr) && (RHS != nullptr) && (LHS != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(BO->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(BO->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
				if (clang::Expr::NPCK_NotNull != kind) {
					auto lhs_source_range = nice_source_range(LHS->getSourceRange(), Rewrite);
					std::string lhs_source_text;
					if (lhs_source_range.isValid()) {
						lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
						//auto lhs_source_text_sans_ws = with_whitespace_removed(lhs_source_text);

						auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "compare with null", (*this).m_state1);
						bool lhs_is_an_indirect_type = is_an_indirect_type(LHS->getType());

						if (lhs_res2.update_declaration_flag) {
							update_declaration(*(lhs_res2.ddecl_cptr), Rewrite, m_state1);
						}

						auto lhs_QT = LHS->getType();
						const clang::Type* lhs_TP = lhs_QT.getTypePtr();
						auto lhs_type_str = lhs_QT.getAsString();

						std::string lhs_element_type_str;
						if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = element_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						} else if (llvm::isa<const clang::PointerType>(lhs_TP)) {
							auto TPP = llvm::cast<const clang::PointerType>(lhs_TP);
							assert(nullptr != TPP);
							auto target_type = TPP->getPointeeType();
							auto type_str = target_type.getAsString();
							if (("char" != type_str) && ("const char" != type_str)) {
								lhs_element_type_str = type_str;
							}
						}

						if ("" != lhs_element_type_str) {
							if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
								std::string bo_replacement_code;

								std::string opcode_str = BO->getOpcodeStr();
								if ("==" == opcode_str) {
									bo_replacement_code += "!";
								} else { assert("!=" == opcode_str); }

								auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);

								if ("Dual" == ConvertMode) {
									bo_replacement_code += "MSE_LH_CAST(bool, " + lhs_source_text + ")";
								} else {
									bo_replacement_code += "bool(" + lhs_source_text + ")";
								}

								auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level), BO, bo_replacement_code);

								if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(lhs_res2.indirection_level)) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
							}
						}
						int q = 5;
					} else {
						int q = 5;
					}
					int q = 5;
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSMemset : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMemset (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemset1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemset2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemset3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (3 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string memset_str = "memset";
						if (memset_str == function_name) {
							auto iter1 = CE->arg_begin();
							assert((*iter1)->getType().getTypePtrOrNull());
							auto arg_source_range1 = nice_source_range((*iter1)->getSourceRange(), Rewrite);

							auto iter2 = iter1;
							iter2++;
							assert((*iter2)->getType().getTypePtrOrNull());
							auto arg_source_range2 = nice_source_range((*iter2)->getSourceRange(), Rewrite);

							auto iter3 = iter2;
							iter3++;
							assert((*iter3)->getType().getTypePtrOrNull());
							auto arg_source_range3 = nice_source_range((*iter3)->getSourceRange(), Rewrite);

							std::string arg_source_text1;
							std::string arg_source_text2;
							std::string arg_source_text3;
							if (arg_source_range1.isValid() && arg_source_range2.isValid() && arg_source_range3.isValid()) {
								arg_source_text1 = Rewrite.getRewrittenText(arg_source_range1);
								arg_source_text2 = Rewrite.getRewrittenText(arg_source_range2);
								arg_source_text3 = Rewrite.getRewrittenText(arg_source_range3);
								QualType QT;
								std::string element_type_str;
								clang::SourceRange decl_source_range;
								std::string variable_name;
								std::string ce_replacement_code;
								const clang::DeclaratorDecl* DD = nullptr;

								auto decl = DRE->getDecl();
								DD = dyn_cast<const DeclaratorDecl>(decl);
								auto VD = dyn_cast<const VarDecl>(decl);

								const clang::FieldDecl* FD = nullptr;
								if (nullptr != ME) {
									auto member_decl = ME->getMemberDecl();
									FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
								}
								if (nullptr != FD) {
									DD = FD;
								} else if (nullptr != VD) {
									DD = VD;
								} else {
									int q = 7;
								}

								if (nullptr != DD) {
									auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
									if (!decl_source_range.isValid()) {
										return;
									}
									DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
									DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

									QT = DD->getType();
									auto qtype_str = QT.getAsString();
									variable_name = DD->getNameAsString();

									auto qualified_name = DD->getQualifiedNameAsString();
									static const std::string mse_namespace_str1 = "mse::";
									static const std::string mse_namespace_str2 = "::mse::";
									if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
											|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
										return;
									}

									auto res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, state1);
									}

									const clang::Type* arg1_TP = QT.getTypePtr();
									auto arg1_type_str = QT.getAsString();

									std::string arg1_element_type_str;
									if (arg1_TP->isArrayType()) {
										auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
										assert(nullptr != ATP);
										auto element_type = ATP->getElementType();
										auto type_str = element_type.getAsString();
										if (("char" != type_str) && ("const char" != type_str)) {
											arg1_element_type_str = type_str;
										}
									} else if (arg1_TP->isPointerType()) {
										auto TPP = llvm::cast<const clang::PointerType>(arg1_TP);
										assert(nullptr != TPP);
										auto target_type = TPP->getPointeeType();
										auto type_str = target_type.getAsString();
										if (("char" != type_str) && ("const char" != type_str)) {
											arg1_element_type_str = type_str;
										}
									}
									if ("" != arg1_element_type_str) {
										if ("Dual" == ConvertMode) {
											ce_replacement_code = "MSE_LH_MEMSET(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
										} else if ("FasterAndStricter" == ConvertMode) {
											ce_replacement_code = "mse::lh::memset(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
										} else {
											ce_replacement_code = "mse::lh::memset(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
										}

										if (ConvertToSCPP && decl_source_range.isValid() && (SR.isValid())
												&& (nullptr != res2.ddecl_conversion_state_ptr)) {
											auto cr_shptr = std::make_shared<CMemsetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);

											if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
												(*cr_shptr).do_replacement(state1);
											} else {
												state1.m_array2_contingent_replacement_map.insert(cr_shptr);
											}
										} else {
											int q = 7;
										}
									}
								}
								int q = 5;
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemset1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemset2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemset3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (3 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string memset_str = "memset";
						if (memset_str == function_name) {
							if (ConvertToSCPP && SR.isValid()) {

								auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
								/* This modification needs to be queued so that it will be executed after any other
								modifications that might affect the relevant part of the source text. */
								(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
							} else {
								int q = 7;
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

	class MCSSSMemcpy : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMemcpy (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemcpy1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemcpy2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemcpy3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (3 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string memcpy_str = "memcpy";
						if (memcpy_str == function_name) {
							auto iter1 = CE->arg_begin();
							assert((*iter1)->getType().getTypePtrOrNull());
							auto arg_source_range1 = nice_source_range((*iter1)->getSourceRange(), Rewrite);

							auto iter2 = iter1;
							iter2++;
							assert((*iter2)->getType().getTypePtrOrNull());
							auto arg_source_range2 = nice_source_range((*iter2)->getSourceRange(), Rewrite);

							auto iter3 = iter2;
							iter3++;
							assert((*iter3)->getType().getTypePtrOrNull());
							auto arg_source_range3 = nice_source_range((*iter3)->getSourceRange(), Rewrite);

							std::string arg_source_text1;
							std::string arg_source_text2;
							std::string arg_source_text3;
							if (arg_source_range1.isValid() && arg_source_range2.isValid() && arg_source_range3.isValid()) {
								arg_source_text1 = Rewrite.getRewrittenText(arg_source_range1);
								arg_source_text2 = Rewrite.getRewrittenText(arg_source_range2);
								arg_source_text3 = Rewrite.getRewrittenText(arg_source_range3);

								QualType QT;
								clang::SourceRange decl_source_range;
								std::string variable_name;
								const clang::DeclaratorDecl* DD = nullptr;
								CArrayInferenceInfo res2;

								auto decl = DRE->getDecl();
								DD = dyn_cast<const DeclaratorDecl>(decl);
								auto VD = dyn_cast<const VarDecl>(decl);

								const clang::FieldDecl* FD = nullptr;
								if (nullptr != ME) {
									auto member_decl = ME->getMemberDecl();
									FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
								}
								if (nullptr != FD) {
									DD = FD;
								} else if (nullptr != VD) {
									DD = VD;
								} else {
									int q = 7;
								}

								if (nullptr != DD) {
									decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
									auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
									std::string decl_source_text;
									if (decl_source_range.isValid()) {
										IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
									} else {
										return;
									}
									QT = DD->getType();
									auto qtype_str = QT.getAsString();
									variable_name = DD->getNameAsString();

									auto qualified_name = DD->getQualifiedNameAsString();
									static const std::string mse_namespace_str1 = "mse::";
									static const std::string mse_namespace_str2 = "::mse::";
									if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
											|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
										return;
									}

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, state1);
									}
								}

								clang::QualType arg1_QT = (*iter1)->getType();
								if (nullptr != DD) {
									arg1_QT = QT;
								}
								const clang::Type* arg1_TP = arg1_QT.getTypePtr();
								auto arg1_type_str = arg1_QT.getAsString();

								std::string arg1_element_type_str;
								if (arg1_TP->isArrayType()) {
									auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
									assert(nullptr != ATP);
									auto element_type = ATP->getElementType();
									auto type_str = element_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								} else if (arg1_TP->isPointerType()) {
									auto TPP = llvm::cast<const clang::PointerType>(arg1_TP);
									assert(nullptr != TPP);
									auto target_type = TPP->getPointeeType();
									auto type_str = target_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								}
								std::string ce_replacement_code;
								if (("" != arg1_element_type_str) && ("void" != arg1_element_type_str) && ("const void" != arg1_element_type_str)) {
									if (false) {
										ce_replacement_code = "for (size_t i = 0; i < (" + arg_source_text3
												+ ")/sizeof(" + arg1_element_type_str + "); i += 1) { ";
										ce_replacement_code += "(" + arg_source_text1 + ")[i] = (" + arg_source_text2 + ")[i]; ";
										ce_replacement_code += "}";
									} else {
										ce_replacement_code = "MSE_LH_MEMCPY(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
									}

									if ("Dual" == ConvertMode) {
										ce_replacement_code = "MSE_LH_MEMCPY(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
									} else if ("FasterAndStricter" == ConvertMode) {
										ce_replacement_code = "mse::lh::memcpy(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
									} else {
										ce_replacement_code = "mse::lh::memcpy(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
									}
								}

								if (ConvertToSCPP && (SR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CMemcpyArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(state1);
										} else {
											state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(state1);
									}
								} else {
									int q = 7;
								}

								int q = 5;
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemcpy1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemcpy2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemcpy3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (3 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string memcpy_str = "memcpy";
						if (memcpy_str == function_name) {
							if (ConvertToSCPP && SR.isValid()) {

								auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
								/* This modification needs to be queued so that it will be executed after any other
								modifications that might affect the relevant part of the source text. */
								(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
							} else {
								int q = 7;
							}

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

	/* This class addresses the initialized declarations in the form "type var = cond ? lhs : rhs;". */
	class MCSSSConditionalInitializer : public MatchFinder::MatchCallback
	{
	public:
		MCSSSConditionalInitializer (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssconditionalinitializer1");
			const clang::ConditionalOperator* CO = MR.Nodes.getNodeAs<clang::ConditionalOperator>("mcsssconditionalinitializer2");
			const Expr* LHS = nullptr;
			const Expr* RHS = nullptr;
			if (CO) {
				LHS = CO->getLHS();
				RHS = CO->getRHS();
			}
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssconditionalinitializer3");

			if ((DS != nullptr) && (LHS != nullptr) && (RHS != nullptr) && (DD != nullptr))
			{
				auto SR = nice_source_range(DS->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(DS->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
				if (!decl_source_range.isValid()) {
					return;
				}
				DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, MR);
				DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

				QualType QT = DD->getType();
				auto variable_name = DD->getNameAsString();

				auto qualified_name = DD->getQualifiedNameAsString();
				static const std::string mse_namespace_str1 = "mse::";
				static const std::string mse_namespace_str2 = "::mse::";
				if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
						|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
					return;
				}

				std::string var_current_state_str;
				{
					auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*DD);
					auto ddcs_map_iter = res1.first;
					auto& ddcs_ref = (*ddcs_map_iter).second;
					if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
						var_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current_species();
					} else {
						int q = 7;
					}
				}
				bool var_has_been_determined_to_be_an_array = false;
				if (("inferred array" == var_current_state_str) ||
						("dynamic array" == var_current_state_str) ||
						("native array" == var_current_state_str)) {
					if ("native array" == var_current_state_str) {
						assert(false); /* right? */
					}
					var_has_been_determined_to_be_an_array = true;
				}

				auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", (*this).m_state1);
				auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
				bool lhs_qualifies = false;
				bool rhs_qualifies = false;

				if (lhs_res2.ddecl_cptr && lhs_res2.update_declaration_flag) {
					update_declaration(*(lhs_res2.ddecl_cptr), Rewrite, m_state1);
				}
				if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
					update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
				}

				{
					auto& res2 = lhs_res2;
					if (res2.ddecl_cptr && res2.declaration_expr_cptr) {
						std::string variable_name = res2.ddecl_cptr->getNameAsString();
						auto QT = res2.ddecl_cptr->getType();
						auto LHS_QT = LHS->getType();
						/* Currently we only support the case where the value expressions are direct
						* references to declared variables. */
						if ((QT == LHS_QT)/* && (1 == res2.indirection_level)*/) {
							lhs_qualifies = true;
							if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
								{
									/* Here we're establishing and "enforcing" the constraint that the lhs value must
									* be of an (array) type that can be assigned to the target variable. */
									auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

									if (var_has_been_determined_to_be_an_array) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

									if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							}
						}
					}
				}

				{
					auto& res2 = rhs_res2;
					if (res2.ddecl_cptr && res2.declaration_expr_cptr) {
						std::string variable_name = res2.ddecl_cptr->getNameAsString();
						auto QT = res2.ddecl_cptr->getType();
						auto RHS_QT = RHS->getType();
						/* Currently we only support the case where the value expressions are direct
						* references to declared variables. */
						if (QT == RHS_QT) {
							rhs_qualifies = true;
							if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
								{
									/* Here we're establishing and "enforcing" the constraint that the rhs value must
									* be of an (array) type that can be assigned to the target variable. */
									auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

									if (var_has_been_determined_to_be_an_array) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

									if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							}
						}
					}
				}

				if (lhs_qualifies && rhs_qualifies) {
					std::string lhs_current_state_str;
					{
						auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*(lhs_res2.ddecl_cptr));
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
							lhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current_species();
						} else {
							int q = 7;
						}
					}
					std::string rhs_current_state_str;
					{
						auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*(rhs_res2.ddecl_cptr));
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
							rhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current_species();
						} else {
							int q = 7;
						}
					}

					if (ConvertToSCPP) {
						/* Here we're establishing and "enforcing" the constraint that the lhs and rhs
						* values of the conditional operator must be the same type. */
						{
							auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*lhs_res2.ddecl_cptr, 0), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, DD);

							if ("dynamic array" == lhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							} else if ("native array" == lhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							} else {
								m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								if ("inferred array" == lhs_current_state_str) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
								}
							}
						}
						{
							auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*rhs_res2.ddecl_cptr, 0), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, DD);

							if ("dynamic array" == rhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							} else if ("native array" == rhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							} else {
								m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								if ("inferred array" == rhs_current_state_str) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
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

	class MCSSSAssignment : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAssignment (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::BinaryOperator* BO = MR.Nodes.getNodeAs<clang::BinaryOperator>("mcsssassignment1");
			const clang::Expr* LHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssassignment5");
			const clang::Expr* RHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssassignment6");
			/*
			const Expr* LHS = nullptr;
			const Expr* RHS = nullptr;
			if (BO) {
				LHS = BO->getLHS();
				RHS = BO->getRHS();
			}
			*/
			const clang::VarDecl* VD = MR.Nodes.getNodeAs<clang::VarDecl>("mcsssassignment7");
			const clang::DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssassignment2");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssassignment4");

			if ((RHS != nullptr) && (
				((LHS != nullptr) && (DRE != nullptr))
				|| (VD != nullptr)
				))
			{
				auto SR = BO ? nice_source_range(BO->getSourceRange(), Rewrite) : 
							VD ? nice_source_range(VD->getSourceRange(), Rewrite) :
							nice_source_range(LHS->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":5408:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto ISR = BO ? instantiation_source_range(BO->getSourceRange(), Rewrite) :
							VD ? instantiation_source_range(VD->getSourceRange(), Rewrite) :
							instantiation_source_range(LHS->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				CArrayInferenceInfo lhs_res2;
				if (LHS) {
					lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", (*this).m_state1);
				} else {
					const DeclaratorDecl* lhs_DD = llvm::cast<const clang::DeclaratorDecl>(VD);
					if (lhs_DD) {
						auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*lhs_DD);
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						bool update_declaration_flag = res1.second;

						lhs_res2.ddecl_cptr = lhs_DD;
						lhs_res2.ddecl_conversion_state_ptr = &ddcs_ref;
						lhs_res2.indirection_level = ddcs_ref.m_indirection_state_stack.size();
						if (1 <= lhs_res2.indirection_level) {
							lhs_res2.indirection_level -= 1;
						}
						lhs_res2.update_declaration_flag = update_declaration_flag;
					} else {
						assert(false);
						return;
					}
				}

				//auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", (*this).m_state1);
				auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
				bool lhs_is_an_indirect_type = LHS ? is_an_indirect_type(LHS->getType()) : is_an_indirect_type(VD->getType());
				bool rhs_is_an_indirect_type = is_an_indirect_type(RHS->getType());
				assert(lhs_is_an_indirect_type == rhs_is_an_indirect_type);

				if (lhs_res2.ddecl_cptr && lhs_res2.update_declaration_flag) {
					update_declaration(*(lhs_res2.ddecl_cptr), Rewrite, m_state1);
				}
				if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
					update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
				}

				if ((nullptr != CCE) && (rhs_res2.ddecl_conversion_state_ptr)) {
					auto cce_QT = CCE->getType();
					auto rhs_QT = RHS->getType();
					if (cce_QT == rhs_QT) {
						CIndirectionStateStack rhs_qtype_indirection_state_stack;
						auto direct_rhs_qtype = populateQTypeIndirectionStack(rhs_qtype_indirection_state_stack, rhs_QT);
						auto direct_rhs_qtype_str = direct_rhs_qtype.getAsString();
						auto casted_expr_ptr = CCE->IgnoreCasts();
						if (llvm::isa<const clang::CallExpr>(casted_expr_ptr->IgnoreParenCasts())) {
							auto CE = llvm::cast<const clang::CallExpr>(casted_expr_ptr->IgnoreParenCasts());
							auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
							if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
								/* This seems to be some kind of malloc/realloc function. These case should not be
								* handled here. They are handled elsewhere. */
								return;
							}
						}

						if ((rhs_qtype_indirection_state_stack.size() + rhs_res2.indirection_level
								== (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
								&& (1 <= (*rhs_res2.ddecl_conversion_state_ptr).m_indirection_state_stack.size())
								&& (nullptr != casted_expr_ptr)) {

							std::string rhs_ddecl_current_direct_qtype_str = (*rhs_res2.ddecl_conversion_state_ptr).current_direct_qtype_str();
							auto casted_expr_SR = nice_source_range(casted_expr_ptr->getSourceRange(), Rewrite);
							auto CCESR = nice_source_range(CCE->getSourceRange(), Rewrite);
							auto cast_operation_SR = clang::SourceRange(CCE->getLParenLoc(), CCE->getRParenLoc());

							if (cast_operation_SR.isValid()
									&& (("void" == rhs_ddecl_current_direct_qtype_str) || ("const void" == rhs_ddecl_current_direct_qtype_str))) {
								if (ConvertToSCPP) {
									(*rhs_res2.ddecl_conversion_state_ptr).set_current_direct_qtype(direct_rhs_qtype);

									IF_DEBUG(auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);)
									/* This is not the proper way to modify an expression. See the function
									* CConditionalOperatorReconciliation2ReplacementAction::do_replacement() for an example of
									* the proper way to do it. But for now this is good enough. */
									m_state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, "");
									//auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");

									static const std::string void_str = "void";
									auto void_pos = (*rhs_res2.ddecl_conversion_state_ptr).m_current_initialization_expr_str.find(void_str);
									if (std::string::npos != void_pos) {
										(*rhs_res2.ddecl_conversion_state_ptr).m_current_initialization_expr_str.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
									}

									update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
								}
							} else {
								if (ConvertToSCPP) {
									if (false) {
										(*rhs_res2.ddecl_conversion_state_ptr).set_current_direct_qtype(direct_rhs_qtype);
									}
								}
							}
						} else {
							int q = 7;
						}
					} else {
						int q = 7;
					}
				}

				int lhs_indirection_level_adjustment = 0;
				auto rhs_res3 = leading_addressof_operator_info_from_stmt(*RHS);
				if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
					assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

					RHS = rhs_res3.without_leading_addressof_operator_expr_cptr;
					rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
					lhs_indirection_level_adjustment += 1;
				}

				if (llvm::isa<const clang::CallExpr>(RHS->IgnoreParenCasts())) {
					auto CE = llvm::cast<const clang::CallExpr>(RHS->IgnoreParenCasts());
					auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
					if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
						/* This seems to be some kind of malloc/realloc function. These case should not be
						* handled here. They are handled elsewhere. */
						return;
					}
				}

				if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && (rhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
					for (size_t i = 0; (rhs_res2.indirection_level + i < (*(lhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
												&& (rhs_res2.indirection_level + i < (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size()); i += 1) {
						{
							/* Here we're establishing and "enforcing" the constraint that the rhs value must
							* be of an (array) type that can be assigned to the lhs. */
							auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
							auto cr_shptr2 = std::make_shared<CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));

							if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(lhs_res2.indirection_level + i)) {
								(*cr_shptr).do_replacement(m_state1);
								if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_a_dynamic_array(lhs_res2.indirection_level + i)) {
									(*cr_shptr2).do_replacement(m_state1);
								} else {
									m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
								}
							} else {
								m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
								m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
							}
						}
						{
							/* Here we're establishing the constraint in the opposite direction as well. */
							auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level + i));

							if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(rhs_res2.indirection_level + i)) {
								(*cr_shptr).do_replacement(m_state1);
							} else {
								m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
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

	/* This class handles cases where (possibly array iterator) pointers are passed as function arguments. */
	class MCSSSArgToParameterPassingArray2 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSArgToParameterPassingArray2 (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssparameterpassing1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssparameterpassing2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssparameterpassing3");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssparameterpassing4");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":5061:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl1 = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl1) {
					std::string function_name = function_decl1->getNameAsString();
					auto lc_function_name = tolowerstr(function_name);

					static const std::string free_str = "free";
					bool ends_with_free = ((lc_function_name.size() >= free_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));

					static const std::string alloc_str = "alloc";
					static const std::string realloc_str = "realloc";
					bool ends_with_alloc = ((lc_function_name.size() >= alloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - alloc_str.size(), alloc_str.size(), alloc_str)));
					bool ends_with_realloc = (ends_with_alloc && (lc_function_name.size() >= realloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - realloc_str.size(), realloc_str.size(), realloc_str)));

					bool begins_with__builtin_ = string_begins_with(function_name, "__builtin_");
					bool is_memcpy = ("memcpy" == function_name);
					bool is_memset = ("memset" == function_name);

					if (ends_with_free || ends_with_alloc || ends_with_realloc || is_memcpy || is_memset || begins_with__builtin_) {
						return void();
					}

					if ("insert" == function_name) {
						int q = 5;
					}

					std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
					for (auto param_VD : function_decl1->parameters()) {
						param_decls_of_first_function_decl.push_back(param_VD);
					}

					/* The constraint(s) need to be applied to all (re)declarations of the function. */
					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						auto fdecl_source_range = nice_source_range(function_decl->getSourceRange(), Rewrite);
						auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(*MR.SourceManager);
#ifndef NDEBUG
						if (std::string::npos != fdecl_source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != fdecl_source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						}
#endif /*!NDEBUG*/

						bool std_vector_insert_flag = false;
						bool std_vector_insert_range_flag = false;
						if (filtered_out_by_location(MR, fdecl_source_range.getBegin())) {
							std::string qualified_function_name = function_decl1->getQualifiedNameAsString();
							if (string_begins_with(qualified_function_name, "std::vector") && ("insert" == function_name)) {
								std_vector_insert_flag = true;
							} else {
								return void();
							}
						}

						for (size_t arg_index = 0; (CE->getNumArgs() > arg_index) && (function_decl->getNumParams() > arg_index); arg_index += 1) {
							auto param_VD = function_decl->getParamDecl(arg_index);
							auto arg_EX = CE->getArg(arg_index);

							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_source_range = nice_source_range(arg_EX->getSourceRange(), Rewrite);
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)
							}

							if ((nullptr != param_VD) && (nullptr != arg_EX) && arg_source_range.isValid()) {
								bool lhs_is_an_indirect_type = is_an_indirect_type(param_VD->getType());
								bool rhs_is_an_indirect_type = is_an_indirect_type(arg_EX->getType());
								//assert(lhs_is_an_indirect_type == rhs_is_an_indirect_type);

								auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*param_VD);
								auto ddcs_map_iter = res1.first;
								auto& ddcs_ref = (*ddcs_map_iter).second;
								bool update_declaration_flag = res1.second;

								auto lhs_QT = param_VD->getType();
								const clang::Type* lhs_TP = lhs_QT.getTypePtr();
								auto lhs_type_str = lhs_QT.getAsString();

								bool lhs_element_type_is_const_char = false;
								std::string lhs_element_type_str;
								if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
									auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
									assert(nullptr != ATP);
									auto element_type = ATP->getElementType();
									auto type_str = element_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										lhs_element_type_str = type_str;
									} else if ("const char" == type_str) {
										lhs_element_type_is_const_char = true;
									}
								} else if (llvm::isa<const clang::PointerType>(lhs_TP)) {
									auto TPP = llvm::cast<const clang::PointerType>(lhs_TP);
									assert(nullptr != TPP);
									auto target_type = TPP->getPointeeType();
									auto type_str = target_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										lhs_element_type_str = type_str;
									} else if ("const char" == type_str) {
										lhs_element_type_is_const_char = true;
									}
								}

								bool aux_arg_has_been_determined_to_be_array = false;
								if (std_vector_insert_flag) {
									if ((3 == CE->getNumArgs()) && (1 == arg_index) && (!(lhs_QT->isIntegerType()))) {
										std_vector_insert_range_flag = true;
									}
									if ((0 == arg_index) || std_vector_insert_range_flag) {
										aux_arg_has_been_determined_to_be_array = true;
										//note_array_determination(Rewrite, m_state1, CDDeclIndirection(*param_VD, 0));
									}
								}
								if (("iterator" == lhs_type_str) || ("const_iterator" == lhs_type_str)) {
									aux_arg_has_been_determined_to_be_array = true;
								}

								auto argii_EX = arg_EX->IgnoreParenImpCasts();
								auto argii_stmt_class = argii_EX->getStmtClass();
								if (rhs_is_an_indirect_type && (argii_EX->getStmtClass() == clang::Stmt::StmtClass::CStyleCastExprClass)) {
									auto CSCE = dyn_cast<const clang::CStyleCastExpr>(argii_EX);
									assert(CSCE);
									auto cast_qtype = CSCE->getTypeAsWritten();
									if ((cast_qtype->isPointerType() && param_VD->getType()->isPointerType()) 
										|| ((cast_qtype->isReferenceType() && param_VD->getType()->isReferenceType()))) {
										auto cast_pointee_qtype = cast_qtype->getPointeeType();
										auto param_VD_pointee_qtype = param_VD->getType()->getPointeeType();

										auto const_adjusted_cast_pointee_qtype = cast_pointee_qtype;
										auto const_cast_pointee_qtype = const_adjusted_cast_pointee_qtype;
										const_cast_pointee_qtype.addConst();
										if (const_cast_pointee_qtype == param_VD_pointee_qtype) {
											const_adjusted_cast_pointee_qtype = const_cast_pointee_qtype;
										}
										IF_DEBUG(std::string const_adjusted_cast_pointee_qtype_str = const_adjusted_cast_pointee_qtype.getAsString();)
										IF_DEBUG(std::string const_cast_pointee_qtype_str = const_cast_pointee_qtype.getAsString();)
										IF_DEBUG(std::string param_VD_qtype_str = param_VD->getType().getAsString();)
										if (param_VD_pointee_qtype == const_adjusted_cast_pointee_qtype) {
											if (ConvertToSCPP) {
												for (size_t i = 0; i < ddcs_ref.m_indirection_state_stack.size(); ++i) {
													std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CTargetConstrainsCStyleCastExprArray2ReplacementAction>(Rewrite, MR,
															CDDeclIndirection(*param_VD, i), *CSCE);

													if (ddcs_ref.has_been_determined_to_be_an_array(i)) {
														(*cr_shptr).do_replacement(m_state1);
													} else {
														m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													}
												}
											}
										}
									}
								}

								int lhs_indirection_level_adjustment = 0;
								auto rhs_res3 = leading_addressof_operator_info_from_stmt(*arg_EX);
								if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
									assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

									arg_EX = rhs_res3.without_leading_addressof_operator_expr_cptr;
									lhs_indirection_level_adjustment += 1;
								}
								auto rhs_res2 = infer_array_type_info_from_stmt(*arg_EX, "", (*this).m_state1);

								if (rhs_res2.ddecl_cptr) {
									/* update the declaration of the of the variable in the argument
									expression (whether it needs updating or not) */
									update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
								}

								if (ddcs_ref.m_ddecl_cptr && update_declaration_flag) {
									update_declaration(*(ddcs_ref.m_ddecl_cptr), Rewrite, m_state1);
								}

								auto CXXTE = dyn_cast<const clang::CXXThisExpr>(arg_EX->IgnoreParenImpCasts());
								if (false && CXXTE) {
									if (ConvertToSCPP) {
										if ("FasterAndStricter" == ConvertMode) {
										} else {
											std::string replacement_code = "mse::us::unsafe_make_any_pointer_to(this)";
										}
									}
								}

								if (ConvertToSCPP && lhs_is_an_indirect_type) {
									if (rhs_res2.ddecl_conversion_state_ptr) {
										int max_indirection_level1 = int((*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
													- int(rhs_res2.indirection_level);
										int int_max_indirection_level = std::min(int(ddcs_ref.m_indirection_state_stack.size()) - int(lhs_indirection_level_adjustment), max_indirection_level1);
										size_t szt_max_indirection_level = 0;
										if (0 <= int_max_indirection_level) {
											szt_max_indirection_level = size_t(int_max_indirection_level);
										}

										for (size_t i = 0; (i < szt_max_indirection_level); i += 1) {
											{
												/* Here we're establishing and "enforcing" the constraint that the rhs value must
												* be of an (array) type that can be assigned to the lhs. */
												std::shared_ptr<CArray2ReplacementAction> cr_shptr;
												if (1 > (i + lhs_indirection_level_adjustment)) {
													cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR,
															CDDeclIndirection(*param_VD, i + lhs_indirection_level_adjustment), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
												} else {
													/* Levels of indirection beyond the first one must be of the same type,
													* not just of "compatible" types. */
													cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
															CDDeclIndirection(*param_VD, i + lhs_indirection_level_adjustment), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
												}

												if (ddcs_ref.has_been_determined_to_be_an_array(i + lhs_indirection_level_adjustment)) {
													(*cr_shptr).do_replacement(m_state1);
													if (!(ddcs_ref.has_been_determined_to_be_a_dynamic_array(i + lhs_indirection_level_adjustment))) {
														m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
													}
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}
											}
											{
												/* Here we're establishing the constraint in the opposite direction as well. */
												std::shared_ptr<CArray2ReplacementAction> cr_shptr;
												if (1 > (i + lhs_indirection_level_adjustment)) {
													cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR,
															CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*param_VD, i + lhs_indirection_level_adjustment));
												} else {
													/* Levels of indirection beyond the first one must be of the same type,
													* not just of "compatible" types. */
													cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
															CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*param_VD, i + lhs_indirection_level_adjustment));
												}

												if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(rhs_res2.indirection_level + i)) {
													(*cr_shptr).do_replacement(m_state1);
													if (!((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_a_dynamic_array(rhs_res2.indirection_level + i))) {
														m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
													}
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}
											}
										}
									}

									if (function_decl1 != function_decl) {
										assert(arg_index < param_decls_of_first_function_decl.size());
										auto param_VD_of_first_function_decl = param_decls_of_first_function_decl.at(arg_index);

										auto res1_ffd = m_state1.m_ddecl_conversion_state_map.insert(*param_VD_of_first_function_decl);
										auto ddcs_map_iter_ffd = res1_ffd.first;
										auto& ddcs_ref_ffd = (*ddcs_map_iter_ffd).second;

										if (ddcs_ref.m_indirection_state_stack.size() != ddcs_ref_ffd.m_indirection_state_stack.size()) {
											int q = 7;
										}
										for (size_t i = 0; (i < ddcs_ref.m_indirection_state_stack.size()) && (i < ddcs_ref_ffd.m_indirection_state_stack.size()); i += 1) {
											{
												/* Here we're establishing and "enforcing" the constraint that the current parameter must
												* be of the same type as the corresponding parameter in the first declaration of this function. */
												std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
														CDDeclIndirection(*param_VD, i), CDDeclIndirection(*param_VD_of_first_function_decl, i));

												if (ddcs_ref.has_been_determined_to_be_an_array(i)) {
													(*cr_shptr).do_replacement(m_state1);
													if (!(ddcs_ref.has_been_determined_to_be_a_dynamic_array(i))) {
														m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
													}
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}
											}
											{
												/* Here we're establishing the constraint in the opposite direction as well. */
												std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
														CDDeclIndirection(*param_VD_of_first_function_decl, i), CDDeclIndirection(*param_VD, i));

												if (ddcs_ref_ffd.has_been_determined_to_be_an_array(i)) {
													(*cr_shptr).do_replacement(m_state1);
													if (!(ddcs_ref_ffd.has_been_determined_to_be_a_dynamic_array(i))) {
														m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
													}
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}
											}
										}
									}

								}
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}
				} else {
					auto D = CE->getCalleeDecl();
					auto DD = dyn_cast<const DeclaratorDecl>(D);
					auto EX = CE->getCallee();

					if (DD && EX) {
						auto DDSR = nice_source_range(DD->getSourceRange(), Rewrite);
						std::string ddecl_source_text;
						if (DDSR.isValid()) {
							IF_DEBUG(ddecl_source_text = Rewrite.getRewrittenText(DDSR);)
						} else {
							return;
						}

						auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
						std::string expr_source_text;
						if (EXSR.isValid()) {
							IF_DEBUG(expr_source_text = Rewrite.getRewrittenText(EXSR);)
						} else {
							return;
						}

						if (ConvertToSCPP) {
							auto args = CE->arguments();
							for (auto& arg : args) {
								auto rhs_res2 = infer_array_type_info_from_stmt(*arg, "", (*this).m_state1);
								bool rhs_is_an_indirect_type = is_an_indirect_type(arg->getType());

								if (rhs_res2.ddecl_cptr && rhs_res2.ddecl_conversion_state_ptr) {
									int int_max_indirection_level = int((*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size()) - int(rhs_res2.indirection_level);
									size_t szt_max_indirection_level = 0;
									if (0 <= int_max_indirection_level) {
										szt_max_indirection_level = size_t(int_max_indirection_level);
									}

									for (size_t i = 0; i < szt_max_indirection_level; i += 1) {
										std::shared_ptr<CUpdateIndirectFunctionTypeParamsArray2ReplacementAction> cr_shptr =
												std::make_shared<CUpdateIndirectFunctionTypeParamsArray2ReplacementAction>(Rewrite, MR,
														CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), *DD, *CE);

										if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(rhs_res2.indirection_level + i)) {
											(*cr_shptr).do_replacement(m_state1);
											if (!((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_a_dynamic_array(rhs_res2.indirection_level + i))) {
												m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											}
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									}
								} else {
									int q = 5;
								}
							}
						}
						int q = 5;
					} else {
						int q = 7;
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/* This class handles cases where arguments are passed to non-const reference parameters. */
	class MCSSSArgToReferenceParameterPassing : public MatchFinder::MatchCallback
	{
	public:
		MCSSSArgToReferenceParameterPassing (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssparameterpassing1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssparameterpassing2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssparameterpassing3");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssparameterpassing4");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":113:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl1 = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl1) {
					std::string function_name = function_decl1->getNameAsString();
					auto lc_function_name = tolowerstr(function_name);

					static const std::string free_str = "free";
					bool ends_with_free = ((lc_function_name.size() >= free_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));

					static const std::string alloc_str = "alloc";
					static const std::string realloc_str = "realloc";
					bool ends_with_alloc = ((lc_function_name.size() >= alloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - alloc_str.size(), alloc_str.size(), alloc_str)));
					bool ends_with_realloc = (ends_with_alloc && (lc_function_name.size() >= realloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - realloc_str.size(), realloc_str.size(), realloc_str)));

					bool begins_with__builtin_ = string_begins_with(function_name, "__builtin_");
					bool is_memcpy = ("memcpy" == function_name);
					bool is_memset = ("memset" == function_name);

					if (ends_with_free || ends_with_alloc || ends_with_realloc || is_memcpy || is_memset || begins_with__builtin_) {
						return void();
					}

					if ("insert" == function_name) {
						int q = 5;
					}

					std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
					for (auto param_VD : function_decl1->parameters()) {
						param_decls_of_first_function_decl.push_back(param_VD);
					}

					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						auto fdecl_source_range = nice_source_range(function_decl->getSourceRange(), Rewrite);
						auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(*MR.SourceManager);
#ifndef NDEBUG
						if (std::string::npos != fdecl_source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != fdecl_source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						}
#endif /*!NDEBUG*/

						bool std_vector_insert_flag = false;
						bool std_vector_insert_range_flag = false;
						if (filtered_out_by_location(MR, fdecl_source_range.getBegin())) {
							std::string qualified_function_name = function_decl1->getQualifiedNameAsString();
							if (string_begins_with(qualified_function_name, "std::vector") && ("insert" == function_name)) {
								std_vector_insert_flag = true;
							} else {
								return void();
							}
						}

						for (size_t arg_index = 0; (CE->getNumArgs() > arg_index) && (function_decl->getNumParams() > arg_index); arg_index += 1) {
							auto param_VD = function_decl->getParamDecl(arg_index);
							auto arg_EX = CE->getArg(arg_index);

							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_source_range = nice_source_range(arg_EX->getSourceRange(), Rewrite);
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)
							}

							if ((nullptr != param_VD) && (nullptr != arg_EX) && arg_source_range.isValid()) {
								IF_DEBUG(std::string param_VD_qtype_str = param_VD->getType().getAsString();)
								if (param_VD->getType()->isReferenceType()) {
									if (!(param_VD->getType()->getPointeeType().isConstQualified())) {
										/* This parameter is of non-const reference type, so the
										passed argument must be of the same type. */
										{
											auto DRE2 = dyn_cast<const clang::DeclRefExpr>(arg_EX->IgnoreParenImpCasts());
											if (DRE2) {
												auto arg_VD2 = dyn_cast<const clang::VarDecl>(DRE2->getDecl());
												if (arg_VD2) {
													if (ConvertToSCPP) {
														/* Here we imposing that the passed argument be of the same type as the (non-const) reference
														parameter. In particular, if the reference parameter is converted to a "safely addressable"
														type (that has its '&' operator overloaded), then any passed argument must also be converted
														to the same "safely addressable" type. Technically, the constraint shouldn't be reciprocal
														(i.e. a "safely addressable" argument does not require that the reference parameter also be
														a "safely addressable" type) but CSameTypeReplacementAction we're using here (due to laziness)
														does apply the constraint both ways. */
														std::shared_ptr<CDDeclIndirectionReplacementAction> cr_shptr = std::make_shared<CSameTypeReplacementAction>(Rewrite, MR, *param_VD, *arg_VD2);

														auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*param_VD);
														auto ddcs_map_iter = res1.first;
														auto& ddcs_ref = (*ddcs_map_iter).second;
														bool update_declaration_flag = res1.second;

														if (ddcs_ref.has_been_determined_to_be_a_pointer_target()) {
															(*cr_shptr).do_replacement(m_state1);
														} else {
															m_state1.m_pointer_target_contingent_replacement_map.insert(cr_shptr);
														}
													}
												}
											}
										}
									}
								}
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}
				} else {
					int q = 5;
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSReturnValue : public MatchFinder::MatchCallback
	{
	public:
		MCSSSReturnValue (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::FunctionDecl* FND = MR.Nodes.getNodeAs<clang::FunctionDecl>("mcsssreturnvalue1");
			const clang::ReturnStmt* RS = MR.Nodes.getNodeAs<clang::ReturnStmt>("mcsssreturnvalue2");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssreturnvalue3");

			if ((FND != nullptr) && (DRE != nullptr) && (RS != nullptr))
			{
				auto SR = nice_source_range(FND->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(FND->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

#ifndef NDEBUG
				std::string source_location_str = SR.getBegin().printToString(*MR.SourceManager);
				if (std::string::npos != source_location_str.find("129")) {
					//walkTheAST1(*RS);
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto function_decl1 = FND;
				auto num_params = FND->getNumParams();
				if (function_decl1) {
					std::string function_name = function_decl1->getNameAsString();
					auto lc_function_name = tolowerstr(function_name);

					static const std::string free_str = "free";
					bool ends_with_free = ((lc_function_name.size() >= free_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));

					static const std::string alloc_str = "alloc";
					static const std::string realloc_str = "realloc";
					bool ends_with_alloc = ((lc_function_name.size() >= alloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - alloc_str.size(), alloc_str.size(), alloc_str)));
					bool ends_with_realloc = (ends_with_alloc && (lc_function_name.size() >= realloc_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - realloc_str.size(), realloc_str.size(), realloc_str)));

					bool begins_with__builtin_ = string_begins_with(function_name, "__builtin_");

					if (ends_with_free || ends_with_alloc || ends_with_realloc || begins_with__builtin_) {
						return void();
					}

					auto retval_EX = RS->getRetValue();

					bool rhs_is_an_indirect_type = is_an_indirect_type(retval_EX->getType());

					auto retvalii_EX = RS->getRetValue()->IgnoreImplicit();
					if (rhs_is_an_indirect_type && (retvalii_EX->getStmtClass() == clang::Stmt::StmtClass::CStyleCastExprClass)) {
						auto CSCE = llvm::cast<const clang::CStyleCastExpr>(retvalii_EX);
						if (CSCE) {
							auto cast_operation_SR = clang::SourceRange(CSCE->getLParenLoc(), CSCE->getRParenLoc());
							if (ConvertToSCPP && cast_operation_SR.isValid()) {
								IF_DEBUG(auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);)
								m_state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, "");
								//auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");
							}

							auto cast_kind_name = CSCE->getCastKindName();
							auto cast_kind = CSCE->getCastKind();
						} else { assert(false); }
					}

					int lhs_indirection_level_adjustment = 0;
					auto rhs_res3 = leading_addressof_operator_info_from_stmt(*retval_EX);
					if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
						assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

						retval_EX = rhs_res3.without_leading_addressof_operator_expr_cptr;
						lhs_indirection_level_adjustment += 1;
					}

					auto rhs_res2 = infer_array_type_info_from_stmt(*(retval_EX), "", (*this).m_state1);
					if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
						update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, m_state1);
					}

					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*function_decl);
						auto ddcs_map_iter = res1.first;
						auto& ddcs_ref = (*ddcs_map_iter).second;
						bool update_declaration_flag = res1.second;
						bool lhs_is_an_indirect_type = is_an_indirect_type(function_decl->getReturnType());
						assert(lhs_is_an_indirect_type == rhs_is_an_indirect_type);

						if (ddcs_ref.m_ddecl_cptr && update_declaration_flag) {
							update_declaration(*(ddcs_ref.m_ddecl_cptr), Rewrite, m_state1);
						}

						if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
							const clang::ArraySubscriptExpr* array_subscript_expr_cptr = nullptr;
							auto without_leading_addressof_operator_expr_cptr = rhs_res3.without_leading_addressof_operator_expr_cptr->IgnoreParenCasts();
							if (clang::Stmt::StmtClass::ArraySubscriptExprClass == (*(without_leading_addressof_operator_expr_cptr)).getStmtClass()) {
								assert(llvm::isa<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr));
								array_subscript_expr_cptr = llvm::cast<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr);
							}
							if (false && array_subscript_expr_cptr) {
								/* This case is now handled in the MCSSSAddressof handler instead. */

								std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction>(Rewrite, MR,
										CDDeclIndirection(*function_decl, 0), *(rhs_res3.addressof_unary_operator_cptr), *array_subscript_expr_cptr);

								if (ddcs_ref.has_been_determined_to_be_an_array(0)) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
								}
							}
						}

						if (ConvertToSCPP && (rhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
							int max_indirection_level1 = int((*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
									- int(rhs_res2.indirection_level);
							int int_max_indirection_level = std::min(int(ddcs_ref.m_indirection_state_stack.size()) - int(lhs_indirection_level_adjustment), max_indirection_level1);
							size_t szt_max_indirection_level = 0;
							if (0 <= int_max_indirection_level) {
								szt_max_indirection_level = size_t(int_max_indirection_level);
							}

							for (size_t i = 0; (i < szt_max_indirection_level); i += 1) {
								{
									/* Here we're establishing and "enforcing" the constraint that the rhs value must
									* be of an (array) type that can be assigned to the lhs. */
									std::shared_ptr<CArray2ReplacementAction> cr_shptr;
									if (1 > (i + lhs_indirection_level_adjustment)) {
										cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*function_decl, i + lhs_indirection_level_adjustment), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
									} else {
										/* Levels of indirection beyond the first one must be of the same type,
										* not just of "compatible" types. */
										cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*function_decl, i + lhs_indirection_level_adjustment), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
									}

									if (ddcs_ref.has_been_determined_to_be_an_array(i + lhs_indirection_level_adjustment)) {
										(*cr_shptr).do_replacement(m_state1);
										if (!(ddcs_ref.has_been_determined_to_be_a_dynamic_array(i + lhs_indirection_level_adjustment))) {
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									std::shared_ptr<CArray2ReplacementAction> cr_shptr;
									if (1 > (i + lhs_indirection_level_adjustment)) {
										cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*function_decl, i + lhs_indirection_level_adjustment));
									} else {
										/* Levels of indirection beyond the first one must be of the same type,
										* not just of "compatible" types. */
										cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*function_decl, i + lhs_indirection_level_adjustment));
									}

									if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(rhs_res2.indirection_level + i)) {
										(*cr_shptr).do_replacement(m_state1);
										if (!((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_a_dynamic_array(rhs_res2.indirection_level + i))) {
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							}

							if (function_decl1 != function_decl) {
								auto res1_ffd = m_state1.m_ddecl_conversion_state_map.insert(*function_decl1);
								auto ddcs_map_iter_ffd = res1_ffd.first;
								auto& ddcs_ref_ffd = (*ddcs_map_iter_ffd).second;

								if (ddcs_ref.m_indirection_state_stack.size() != ddcs_ref_ffd.m_indirection_state_stack.size()) {
									int q = 7;
								}
								for (size_t i = 0; (i < ddcs_ref.m_indirection_state_stack.size()) && (i < ddcs_ref_ffd.m_indirection_state_stack.size()); i += 1) {
									{
										/* Here we're establishing and "enforcing" the constraint that the current parameter must
										* be of the same type as the corresponding parameter in the first declaration of this function. */
										std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*function_decl, i), CDDeclIndirection(*function_decl1, i));

										if (ddcs_ref.has_been_determined_to_be_an_array(i)) {
											(*cr_shptr).do_replacement(m_state1);
											if (!(ddcs_ref.has_been_determined_to_be_a_dynamic_array(i))) {
												m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											}
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									}
									{
										/* Here we're establishing the constraint in the opposite direction as well. */
										std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*function_decl1, i), CDDeclIndirection(*function_decl, i));

										if (ddcs_ref_ffd.has_been_determined_to_be_an_array(i)) {
											(*cr_shptr).do_replacement(m_state1);
											if (!(ddcs_ref_ffd.has_been_determined_to_be_a_dynamic_array(i))) {
												m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											}
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
									}
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

	class MCSSSFRead : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFRead (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfread1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfread2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfread3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string fread_str = "fread";
						if (fread_str == function_name) {
							auto iter1 = CE->arg_begin();
							assert((*iter1)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range1 = nice_source_range((*iter1)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter2 = iter1;
							iter2++;
							assert((*iter2)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range2 = nice_source_range((*iter2)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter3 = iter2;
							iter3++;
							assert((*iter3)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range3 = nice_source_range((*iter3)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter4 = iter3;
							iter4++;
							assert((*iter4)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range4 = nice_source_range((*iter4)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							std::string arg_source_text1;
							std::string arg_source_text2;
							std::string arg_source_text3;
							std::string arg_source_text4;
							if (arg_source_range1.isValid() && arg_source_range2.isValid() && arg_source_range3.isValid() && arg_source_range3.isValid() && arg_source_range4.isValid()) {
								arg_source_text1 = Rewrite.getRewrittenText(arg_source_range1);
								arg_source_text2 = Rewrite.getRewrittenText(arg_source_range2);
								arg_source_text3 = Rewrite.getRewrittenText(arg_source_range3);
								arg_source_text4 = Rewrite.getRewrittenText(arg_source_range4);

								QualType QT;
								clang::SourceRange decl_source_range;
								std::string variable_name;
								const clang::DeclaratorDecl* DD = nullptr;
								CArrayInferenceInfo res2;

								auto decl = DRE->getDecl();
								DD = dyn_cast<const DeclaratorDecl>(decl);
								auto VD = dyn_cast<const VarDecl>(decl);

								const clang::FieldDecl* FD = nullptr;
								if (nullptr != ME) {
									auto member_decl = ME->getMemberDecl();
									FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
								}
								if (nullptr != FD) {
									DD = FD;
								} else if (nullptr != VD) {
									DD = VD;
								} else {
									int q = 7;
								}

								if (nullptr != DD) {
									decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
									auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
									std::string decl_source_text;
									if (decl_source_range.isValid()) {
										IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
									} else {
										return;
									}
									QT = DD->getType();
									auto qtype_str = QT.getAsString();
									variable_name = DD->getNameAsString();

									auto qualified_name = DD->getQualifiedNameAsString();
									static const std::string mse_namespace_str1 = "mse::";
									static const std::string mse_namespace_str2 = "::mse::";
									if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
											|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
										return;
									}

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, state1);
									}
								}

								clang::QualType arg1_QT = (*iter1)->IgnoreParenCasts()->getType();
								if (nullptr != DD) {
									arg1_QT = QT;
								}
								const clang::Type* arg1_TP = arg1_QT.getTypePtr();
								auto arg1_type_str = arg1_QT.getAsString();

								std::string arg1_element_type_str;
								if (arg1_TP->isArrayType()) {
									auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
									assert(nullptr != ATP);
									auto element_type = ATP->getElementType();
									auto type_str = element_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								} else if (arg1_TP->isPointerType()) {
									auto TPP = llvm::cast<const clang::PointerType>(arg1_TP);
									assert(nullptr != TPP);
									auto target_type = TPP->getPointeeType();
									auto type_str = target_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								}
								std::string ce_replacement_code;
								if (("" != arg1_element_type_str) && ("void" != arg1_element_type_str) && ("const void" != arg1_element_type_str)) {
									if ("Dual" == ConvertMode) {
										ce_replacement_code = "MSE_LH_FREAD(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									} else if ("FasterAndStricter" == ConvertMode) {
										ce_replacement_code = "mse::lh::fread(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									} else {
										ce_replacement_code = "mse::lh::fread(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									}
								}

								if (ConvertToSCPP && (SR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(state1);
										} else {
											state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(state1);
									}
								} else {
									int q = 7;
								}

								int q = 5;
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfread1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfread2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfread3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string fread_str = "fread";
						if (fread_str == function_name) {
							if (ConvertToSCPP && SR.isValid()) {

								auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
								/* This modification needs to be queued so that it will be executed after any other
								modifications that might affect the relevant part of the source text. */
								(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
							} else {
								int q = 7;
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

	class MCSSSFWrite : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFWrite (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfwrite1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfwrite2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfwrite3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string fwrite_str = "fwrite";
						if (fwrite_str == function_name) {
							auto iter1 = CE->arg_begin();
							assert((*iter1)->IgnoreParenCasts()->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range1 = nice_source_range((*iter1)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter2 = iter1;
							iter2++;
							assert((*iter2)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range2 = nice_source_range((*iter2)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter3 = iter2;
							iter3++;
							assert((*iter3)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range3 = nice_source_range((*iter3)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							auto iter4 = iter3;
							iter4++;
							assert((*iter4)->IgnoreParenCasts()->getType().getTypePtrOrNull());
							auto arg_source_range4 = nice_source_range((*iter4)->IgnoreParenCasts()->getSourceRange(), Rewrite);

							std::string arg_source_text1;
							std::string arg_source_text2;
							std::string arg_source_text3;
							std::string arg_source_text4;
							if (arg_source_range1.isValid() && arg_source_range2.isValid() && arg_source_range3.isValid() && arg_source_range3.isValid() && arg_source_range4.isValid()) {
								arg_source_text1 = Rewrite.getRewrittenText(arg_source_range1);
								arg_source_text2 = Rewrite.getRewrittenText(arg_source_range2);
								arg_source_text3 = Rewrite.getRewrittenText(arg_source_range3);
								arg_source_text4 = Rewrite.getRewrittenText(arg_source_range4);

								QualType QT;
								clang::SourceRange decl_source_range;
								std::string variable_name;
								const clang::DeclaratorDecl* DD = nullptr;
								CArrayInferenceInfo res2;

								auto decl = DRE->getDecl();
								DD = dyn_cast<const DeclaratorDecl>(decl);
								auto VD = dyn_cast<const VarDecl>(decl);

								const clang::FieldDecl* FD = nullptr;
								if (nullptr != ME) {
									auto member_decl = ME->getMemberDecl();
									FD = dyn_cast<const clang::FieldDecl>(ME->getMemberDecl());
								}
								if (nullptr != FD) {
									DD = FD;
								} else if (nullptr != VD) {
									DD = VD;
								} else {
									int q = 7;
								}

								if (nullptr != DD) {
									decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
									auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
									std::string decl_source_text;
									if (decl_source_range.isValid()) {
										IF_DEBUG(decl_source_text = Rewrite.getRewrittenText(decl_source_range);)
									} else {
										return;
									}
									QT = DD->getType();
									auto qtype_str = QT.getAsString();
									variable_name = DD->getNameAsString();

									auto qualified_name = DD->getQualifiedNameAsString();
									static const std::string mse_namespace_str1 = "mse::";
									static const std::string mse_namespace_str2 = "::mse::";
									if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
											|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
										return;
									}

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, state1);
									}
								}

								clang::QualType arg1_QT = (*iter1)->IgnoreParenCasts()->getType();
								if (nullptr != DD) {
									arg1_QT = QT;
								}
								const clang::Type* arg1_TP = arg1_QT.getTypePtr();
								auto arg1_type_str = arg1_QT.getAsString();

								std::string arg1_element_type_str;
								if (arg1_TP->isArrayType()) {
									auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
									assert(nullptr != ATP);
									auto element_type = ATP->getElementType();
									auto type_str = element_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								} else if (arg1_TP->isPointerType()) {
									auto TPP = llvm::cast<const clang::PointerType>(arg1_TP);
									assert(nullptr != TPP);
									auto target_type = TPP->getPointeeType();
									auto type_str = target_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg1_element_type_str = type_str;
									}
								}
								std::string ce_replacement_code;
								if (("" != arg1_element_type_str) && ("void" != arg1_element_type_str) && ("const void" != arg1_element_type_str)) {
									if ("Dual" == ConvertMode) {
										ce_replacement_code = "MSE_LH_FWRITE(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									} else if ("FasterAndStricter" == ConvertMode) {
										ce_replacement_code = "mse::lh::fwrite(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									} else {
										ce_replacement_code = "mse::lh::fwrite(" + arg_source_text1 + ", " + arg_source_text2
												+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
									}
								}

								if (ConvertToSCPP && (SR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(state1);
										} else {
											state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(state1);
									}
								} else {
									int q = 7;
								}

								int q = 5;
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfwrite1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfwrite2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfwrite3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":2803:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto ISR = instantiation_source_range(CE->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (supress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					{
						std::string function_name = function_decl->getNameAsString();
						static const std::string fwrite_str = "fwrite";
						if (fwrite_str == function_name) {
							if (ConvertToSCPP && SR.isValid()) {

								auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
								/* This modification needs to be queued so that it will be executed after any other
								modifications that might affect the relevant part of the source text. */
								(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
							} else {
								int q = 7;
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

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const clang::Decl* D = MR.Nodes.getNodeAs<clang::Decl>("mcsssdeclutil1");

			if ((D != nullptr))
			{
				auto SR = nice_source_range(D->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":56:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto DISR = instantiation_source_range(D->getSourceRange(), Rewrite);
				auto supress_check_flag = state1.m_suppress_check_region_set.contains(DISR);
				if (supress_check_flag) {
					return;
				}

				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();

					auto VD = dyn_cast<const clang::VarDecl>(D);
					if (VD) {
						if (false) {
							auto l_DD = VD;
							auto TSSL = l_DD->getTypeSpecStartLoc();
							auto l_SR = l_DD->getSourceRange();
							
							std::string text1 = Rewrite.getRewrittenText(l_SR);
							std::string text1b = Rewrite.getRewrittenText({l_SR.getBegin().getLocWithOffset(+1), l_SR.getEnd()});
							std::string text2 = Rewrite.getRewrittenText({TSSL, l_SR.getEnd()});
							std::string text2b = Rewrite.getRewrittenText({TSSL.getLocWithOffset(+1), l_SR.getEnd()});
							if (text1 != text2) {
								int q = 5;
							}

							auto QL = VD->getQualifierLoc();
							auto QSR = QL.getSourceRange();
							if (QSR.isValid()) {
								std::string text3 = Rewrite.getRewrittenText({QSR.getBegin(), l_SR.getEnd()});
								std::string text4 = Rewrite.getRewrittenText(QSR);
								int q = 5;
							}
							std::string text5 = Rewrite.getRewrittenText({VD->getInnerLocStart(), l_SR.getEnd()});
							std::string text6 = Rewrite.getRewrittenText({VD->getLocation(), l_SR.getEnd().getLocWithOffset(+1)});
							std::string text7 = Rewrite.getRewrittenText({VD->getSourceRange().getBegin(), VD->getLocation()});
							auto SLE0 = VD->getLocation();
							auto SLE1 = VD->getLocation().getLocWithOffset(-1);
							auto SLE1b = VD->getLocation().getLocWithOffset(0);
							auto SLE1c = VD->getLocation().getLocWithOffset(+1);
							std::string text8;
							if (VD->getSourceRange().getBegin() < SLE1) {
								Rewrite.getRewrittenText({VD->getSourceRange().getBegin(), SLE1});
							}
							std::string text8b = Rewrite.getRewrittenText({VD->getSourceRange().getBegin(), SLE1b});
							std::string text8c = Rewrite.getRewrittenText({VD->getSourceRange().getBegin(), SLE1c});
							auto name = VD->getNameAsString();
							if ("array1" == name) {
								int q = 5;
							} else if ("array2" == name) {
								int q = 5;
							} else if ("int_ptr1" == name) {
								int q = 5;
							} else if ("str1" == name) {
								int q = 5;
							}
							if (VD->getType()->isArrayType()) {
								auto SL2 = VD->getLocation().getLocWithOffset(VD->getNameAsString().length());
								auto SLE2 = SL2;
								auto init_EX = VD->getAnyInitializer();
								if (init_EX) {
									auto init_SL = init_EX->getSourceRange().getBegin();
									if (init_SL.isValid()) {
										SLE2 = init_SL.getLocWithOffset(-2);
									}
								}
								std::string text9 = Rewrite.getRewrittenText({SL2, SLE2});
								int q = 5;
							}
							int q = 5;
						}

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
									if ((qtype.isConstQualified()) && (is_async_shareable(qtype, state1))) {
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
										auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											//std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}

										if (true) {
											/* Here we're (unjustifiably) assuming that the program is single threaded 
											and changing variables with static duration to thread_local duration. */
											std::string l_source_text1 = Rewrite.getRewrittenText(SR);
											int replace_pos = 0;
											int replace_length = 0;
											if (VD->isFileVarDecl()) {
												{
													static const std::string extern_and_space_str = "extern ";
													auto pos1 = l_source_text1.find(extern_and_space_str);
													if (std::string::npos != pos1) {
														replace_pos = pos1 + extern_and_space_str.length();
													}
												}
												{
													static const std::string inline_and_space_str = "inline ";
													auto pos1 = l_source_text1.find(inline_and_space_str);
													if ((std::string::npos) != pos1 && (pos1 > replace_pos)) {
														replace_pos = pos1 + inline_and_space_str.length();
													}
												}
											} else {
												{
													static const std::string static_and_space_str = "static ";
													auto pos1 = l_source_text1.find(static_and_space_str);
													if (std::string::npos != pos1) {
														replace_pos = pos1;
														replace_length = static_and_space_str.length();
													}
												}
												if (0 == replace_length) {
													static const std::string inline_and_space_str = "inline ";
													auto pos1 = l_source_text1.find(inline_and_space_str);
													if ((std::string::npos) != pos1 && (pos1 > replace_pos)) {
														replace_pos = pos1 + inline_and_space_str.length();
													}
												}
											}

											auto res1 = state1.m_ddecl_conversion_state_map.insert(*VD);
											auto ddcs_map_iter = res1.first;
											auto& ddcs_ref = (*ddcs_map_iter).second;

											if (1 <= replace_length) {
												ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = clang::SourceRange(SR.getBegin().getLocWithOffset(replace_pos), SR.getBegin().getLocWithOffset(replace_pos + replace_length - 1));
											} else {
												ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = SR.getBegin().getLocWithOffset(replace_pos);
											}

											int q = 5;
										}
									}
								} else {
									assert(clang::StorageDuration::SD_Thread == storage_duration);
									if (is_async_shareable(qtype, state1)) {
										satisfies_checks = true;
									} else {
										const std::string error_desc = std::string("Unable to verify the safety of variable '")
											+ var_qualified_name + "' of type '" + qtype_str + "' with 'thread local storage duration'. "
											+ "'thread local storage duration' is supported for eligible types wrapped in the "
											+ "'mse::rsv::TThreadLocalObj<>' transparent template wrapper. Other supported wrappers include: "
											+ "mse::rsv::TStaticImmutableObj<>, mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
											+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
										auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}
								}
							} else if (type_name1 == mse_rsv_ThreadLocalObj_str) {
								if (clang::StorageDuration::SD_Thread != storage_duration) {
									const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
										+ mse_rsv_ThreadLocalObj_str + "' must be declared to have 'thread_local' storage duration.";
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
										auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
										if (res.second) {
											//std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}

										{
											/* Here we're adding a missing initialization value to the variable declaration. */
											auto l_DD = VD;
											auto res1 = state1.m_ddecl_conversion_state_map.insert(*l_DD);
											auto ddcs_map_iter = res1.first;
											auto& ddcs_ref = (*ddcs_map_iter).second;

											/* We're noting that originally there was no initializer. */
											ddcs_ref.m_original_initialization_expr_str = "";
											ddcs_ref.m_original_initialization_has_been_noted = true;

											std::string initializer_info_str;
											if (qtype.getTypePtr()->isEnumeralType()) {
												initializer_info_str += qtype.getAsString();
												initializer_info_str += "(0)/*auto-generated init val*/";
											} else if (qtype.getTypePtr()->isPointerType()) {
												if ("Dual" == ConvertMode) {
													initializer_info_str += "MSE_LH_NULL_POINTER/*auto-generated init val*/";
												} else {
													initializer_info_str += "nullptr/*auto-generated init val*/";
												}
											} else {
												initializer_info_str += "0/*auto-generated init val*/";
											}
											ddcs_ref.m_current_initialization_expr_str = initializer_info_str;

											/* Specify that the new initialization string should be
											inserted at the end of the declaration. */
											//ddcs_ref.m_maybe_embedded_initializer_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
											ddcs_ref.m_initializer_SR_or_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);

											update_declaration(*l_DD, Rewrite, state1);
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
								auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									//std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
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
											auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
											if (res.second) {
												//std::cout << (*(res.first)).as_a_string1() << " \n\n";
											}

											{
												/* Here we're adding a missing initialization value to the field declaration. */
												auto l_DD = FD;
												auto res1 = state1.m_ddecl_conversion_state_map.insert(*l_DD);
												auto ddcs_map_iter = res1.first;
												auto& ddcs_ref = (*ddcs_map_iter).second;

												std::string initializer_info_str;
												if (qtype.getTypePtr()->isEnumeralType()) {
													initializer_info_str += qtype.getAsString();
													initializer_info_str += "(0)/*auto-generated init val*/";
												} else if (qtype.getTypePtr()->isPointerType()) {
													if ("Dual" == ConvertMode) {
														initializer_info_str += "MSE_LH_NULL_POINTER/*auto-generated init val*/";
													} else {
														initializer_info_str += "nullptr/*auto-generated init val*/";
													}
												} else {
													initializer_info_str += "0/*auto-generated init val*/";
												}
												ddcs_ref.m_current_initialization_expr_str = initializer_info_str;

												/* Specify that the new initialization string should be
												inserted at the end of the declaration. */
												ddcs_ref.m_initializer_SR_or_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);

												update_declaration(*l_DD, Rewrite, state1);
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
						if (mse_rsv_TAsyncShareableObj_str1 == name) {
							if (1 == CXXRD->getNumBases()) {
								const auto& base = *(CXXRD->bases_begin());
								const auto base_qtype = base.getType();
								const auto base_qtype_str = base_qtype.getAsString();
								if (!is_async_shareable(base_qtype, state1)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncShareableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely shared (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
								if (!is_async_passable(base_qtype, state1)) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncPassableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely passed (between threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
								if ((!is_async_shareable(base_qtype, state1)) || (!is_async_passable(base_qtype, state1))) {
									const std::string error_desc = std::string("Unable to verify that the ")
										+ "given (adjusted) parameter of mse::rsv::TAsyncShareableAndPassableObj<>, '"
										+ base_qtype_str + "', is eligible to be safely shared and passed (among threads). "
										+ "If it is known to be so, then this error can be suppressed with a "
										+ "'check suppression' directive. ";
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
								auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									//std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						} else if (qtype.getTypePtr()->isUnionType()) {
							const std::string error_desc = std::string("Native unions (such as '" + qtype.getAsString() + "') are not ")
								+ "supported. ";
							auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
							auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
							if (res.second) {
								//std::cout << (*(res.first)).as_a_string1() << " \n\n";
							}
						}
					}

					{
						/* Here we're replacing unsupported types with supported counterparts. */

						struct CUnsupportedElementTypeNameAndReplacement {
							std::string m_element_type_name;
							std::string m_replacement_element_type_name;
							bool operator<(const CUnsupportedElementTypeNameAndReplacement &RHS) const {
								return (m_element_type_name < RHS.m_element_type_name);
							}
						};
						std::set<CUnsupportedElementTypeNameAndReplacement> unsupported_elements_encounterred;

						auto check_for_and_handle_unsupported_element2 = [&MR, &Rewrite, &unsupported_elements_encounterred](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
							IF_DEBUG(auto type_SR = nice_source_range(typeLoc.getSourceRange(), Rewrite);)
							IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(type_SR);)

							auto qtype = typeLoc.getType().getUnqualifiedType();
							std::string element_type_name;
							const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
							if (l_CXXRD) {
								element_type_name = l_CXXRD->getQualifiedNameAsString();
							} else {
								element_type_name = qtype.getAsString();
							}

							{
								auto uei_ptr = unsupported_element_info_ptr(element_type_name);
								if (uei_ptr) {
									const auto& unsupported_element_info = *uei_ptr;
									std::string error_desc = std::string("'") + element_type_name + std::string("' is not ")
											+ "supported (in type '" + qtype.getAsString() + "' used in this declaration). ";
									if ("" != unsupported_element_info.m_recommended_alternative) {
										error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
									}
									auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, tsi.getTypeLoc().getSourceRange().getBegin(), error_desc));
									if (res.second) {
										//std::cout << (*(res.first)).as_a_string1() << " \n\n";
									}

									if (true) {
										const auto& f_replacement_element_type_name = [&unsupported_element_info]() {
											if ("Dual" == ConvertMode) {
												return unsupported_element_info.m_slow_mode_replacement;
											} else if ("FasterAndStricter" == ConvertMode) {
												return unsupported_element_info.m_fast_mode_replacement;
											} else {
												return unsupported_element_info.m_slow_mode_replacement;
											}
										};
										std::string replacement_element_type_name = f_replacement_element_type_name();

										/* This modification needs to be queued so that it will be executed after any other
										modifications that might affect the relevant part of the source text. */
										state1.m_pending_code_modification_actions.add_replacement_of_whole_instances_of_given_string_action(Rewrite, l_SR, element_type_name, replacement_element_type_name);

										unsupported_elements_encounterred.insert({element_type_name, replacement_element_type_name});
									}
								}
							}
						};
						auto tsi_ptr = DD->getTypeSourceInfo();
						if (tsi_ptr) {
							check_for_and_handle_unsupported_element2(tsi_ptr->getTypeLoc(), SR, state1);
							apply_to_component_types_if_any(tsi_ptr->getTypeLoc(), check_for_and_handle_unsupported_element2, state1);
						}

						{
							auto res1 = state1.m_ddecl_conversion_state_map.insert(*DD);
							auto ddcs_map_iter = res1.first;
							auto& ddcs_ref = (*ddcs_map_iter).second;
							for (const auto& unsupported_element_encounterred : unsupported_elements_encounterred) {
								std::string tmp_str = ddcs_ref.current_direct_qtype_str();
								replace_whole_instances_of_given_string(tmp_str, unsupported_element_encounterred.m_element_type_name, unsupported_element_encounterred.m_replacement_element_type_name);
								ddcs_ref.set_current_direct_qtype_str(tmp_str);
								update_declaration(*DD, Rewrite, state1);
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
								auto res = std::pair<bool, bool>(); //state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, SR.getBegin(), error_desc));
								if (res.second) {
									//std::cout << (*(res.first)).as_a_string1() << " \n\n";
								}
							}
						}
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Decl* D = MR.Nodes.getNodeAs<clang::Decl>("mcsssdeclutil1");

			if ((D != nullptr))
			{
				auto SR = nice_source_range(D->getSourceRange(), Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, MR);

				RETURN_IF_FILTERED_OUT_BY_LOCATION1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":74:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto DISR = instantiation_source_range(D->getSourceRange(), Rewrite);
				auto supress_check_flag = m_state1.m_suppress_check_region_set.contains(DISR);
				if (supress_check_flag) {
					return;
				}

				if (ConvertToSCPP && SR.isValid()) {

					auto lambda = [MR, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1); };
					/* This modification needs to be queued so that it will be executed after any other
					modifications that might affect the relevant part of the source text. */
					(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);
				} else {
					int q = 7;
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

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(":327:")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

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
								if (field_qtype.getTypePtr()->isPointerType()) {
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
										auto res = std::pair<bool, bool>(); //(*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, ICISR.getBegin(), error_desc));
										if (res.second) {
											//std::cout << (*(res.first)).as_a_string1() << " \n\n";
										}
									}


									{
										/*  */
										auto l_DD = field;
										auto res1 = m_state1.m_ddecl_conversion_state_map.insert(*l_DD);
										auto ddcs_map_iter = res1.first;
										auto& ddcs_ref = (*ddcs_map_iter).second;

										update_declaration(*l_DD, Rewrite, m_state1);
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
										auto res = std::pair<bool, bool>(); //(*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, constructor_SR.getBegin(), error_desc));
										if (res.second) {
											//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
							if (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled()) {
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
							auto res = std::pair<bool, bool>(); //(*this).m_state1.m_error_records.emplace(CErrorRecord(*MR.SourceManager, FDISR.getBegin(), error_desc));
							if (res.second) {
								//std::cout << (*(res.first)).as_a_string1() << " \n\n";
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
						//D->dump();

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
	MyASTConsumer(Rewriter &R, CompilerInstance &CI, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param),
		HandlerForSSSRecordDecl(R, tu_state()), HandlerForSSSVarDecl2(R, tu_state()), HandlerForSSSPointerArithmetic2(R, tu_state()), 
		HandlerForSSSNullToPointer(R, tu_state()), HandlerForSSSMalloc2(R, tu_state()),
		HandlerForSSSMallocInitializer2(R, tu_state()), HandlerForSSSNullInitializer(R, tu_state()), HandlerForSSSFree2(R, tu_state()),
		HandlerForSSSSetToNull2(R, tu_state()), HandlerForSSSCompareWithNull2(R, tu_state()), HandlerForSSSMemset(R, tu_state()), HandlerForSSSMemcpy(R, tu_state()),
		HandlerForSSSConditionalInitializer(R, tu_state()), HandlerForSSSAssignment(R, tu_state()), HandlerForSSSArgToParameterPassingArray2(R, tu_state()),
		HandlerForSSSArgToReferenceParameterPassing(R, tu_state()), HandlerForSSSReturnValue(R, tu_state()), HandlerForSSSFRead(R, tu_state()), HandlerForSSSFWrite(R, tu_state()), 
		HandlerMisc1(R, tu_state(), CI),HandlerForSSSAddressOf(R, tu_state()), HandlerForSSSDeclUtil(R, tu_state())
	{
		//Matcher.addMatcher(varDecl(hasType(pointerType())).bind("mcsssnativepointer"), &HandlerForSSSNativePointer);

		//Matcher.addMatcher(castExpr(allOf(hasCastKind(CK_ArrayToPointerDecay), unless(hasParent(arraySubscriptExpr())))).bind("mcsssarraytopointerdecay"), &HandlerForSSSArrayToPointerDecay);

		Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerMisc1);

		Matcher.addMatcher(clang::ast_matchers::recordDecl().bind("mcsssrecorddecl"), &HandlerForSSSRecordDecl);

		//Matcher.addMatcher(clang::ast_matchers::declaratorDecl().bind("mcsssvardecl"), &HandlerForSSSVarDecl2);
		Matcher.addMatcher(varDecl(anyOf(
				hasInitializer(anyOf(
						expr(cStyleCastExpr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl3")).bind("mcsssvardecl2"),
						expr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl2")
						)),
				clang::ast_matchers::anything()
				)).bind("mcsssvardecl"), &HandlerForSSSVarDecl2);

		Matcher.addMatcher(expr(allOf(
				hasParent(expr(anyOf(
						unaryOperator(hasOperatorName("++")), unaryOperator(hasOperatorName("--")),
						binaryOperator(hasOperatorName("+=")), binaryOperator(hasOperatorName("-=")),
						castExpr(hasParent(expr(anyOf(
								binaryOperator(hasOperatorName("+")), binaryOperator(hasOperatorName("+=")),
								binaryOperator(hasOperatorName("-")), binaryOperator(hasOperatorName("-=")),
								binaryOperator(hasOperatorName("<=")), binaryOperator(hasOperatorName("<")),
								binaryOperator(hasOperatorName(">=")), binaryOperator(hasOperatorName(">")),
								arraySubscriptExpr()/*, clang::ast_matchers::castExpr(hasParent(arraySubscriptExpr()))*/
						))))))),
					hasType(pointerType()),
					anyOf(
							memberExpr(expr(hasDescendant(declRefExpr().bind("mcssspointerarithmetic")))).bind("mcssspointerarithmetic2"),
							declRefExpr().bind("mcssspointerarithmetic"),
							hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcssspointerarithmetic")))).bind("mcssspointerarithmetic2")),
							hasDescendant(declRefExpr().bind("mcssspointerarithmetic"))
					)
					)).bind("mcssspointerarithmetic3"), &HandlerForSSSPointerArithmetic2);

		Matcher.addMatcher(expr(anyOf(gnuNullExpr(), cxxNullPtrLiteralExpr(), 
				integerLiteral(equals(0), hasParent(expr(allOf(
					hasType(pointerType()), clang::ast_matchers::implicitCastExpr()
					)))))).bind("a"),
			&HandlerForSSSNullToPointer);

		Matcher.addMatcher(binaryOperator(allOf(
				hasOperatorName("="),
				hasRHS(
						anyOf(
								cStyleCastExpr(has(ignoringParenCasts(callExpr().bind("mcsssmalloc2")))),
								ignoringParenCasts(callExpr().bind("mcsssmalloc2"))
						)
					),
				hasLHS(ignoringParenCasts(anyOf(
						memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmalloc3")))).bind("mcsssmalloc4"),
							declRefExpr().bind("mcsssmalloc3"),
							hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmalloc3")))).bind("mcsssmalloc4")),
							hasDescendant(declRefExpr().bind("mcsssmalloc3"))
					))),
				hasLHS(expr(hasType(pointerType())))
					)).bind("mcsssmalloc1"), &HandlerForSSSMalloc2);

		Matcher.addMatcher(declStmt(hasDescendant(
				varDecl(hasInitializer(ignoringParenCasts(
								callExpr().bind("mcsssmallocinitializer2")
				))).bind("mcsssmallocinitializer3")
					)).bind("mcsssmallocinitializer1"), &HandlerForSSSMallocInitializer2);

		Matcher.addMatcher(declStmt(hasDescendant(
				varDecl(hasInitializer(ignoringParenCasts(
								expr().bind("mcsssnullinitializer2")
				))).bind("mcsssnullinitializer3")
					)).bind("mcsssnullinitializer1"), &HandlerForSSSNullInitializer);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(ignoringParenCasts(
						expr(anyOf(
									memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfree2")))).bind("mcsssfree3"),
									declRefExpr().bind("mcsssfree2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfree2")))).bind("mcsssfree3")),
									hasDescendant(declRefExpr().bind("mcsssfree2"))
							)))),
							argumentCountIs(1),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssfree1"), &HandlerForSSSFree2);

		Matcher.addMatcher(binaryOperator(allOf(
				hasOperatorName("="),
				hasLHS(anyOf(
							ignoringParenCasts(declRefExpr().bind("mcssssettonull3")),
							ignoringParenCasts(expr(hasDescendant(declRefExpr().bind("mcssssettonull3"))))
					)),
					hasLHS(expr(hasType(pointerType())))
					)).bind("mcssssettonull1"), &HandlerForSSSSetToNull2);

		Matcher.addMatcher(binaryOperator(allOf(
				anyOf(hasOperatorName("=="), hasOperatorName("!=")),
				hasLHS(anyOf(
							ignoringParenCasts(declRefExpr().bind("mcssscomparewithnull3")),
							ignoringParenCasts(expr(hasDescendant(declRefExpr().bind("mcssscomparewithnull3"))))
					)),
					hasLHS(expr(hasType(pointerType())))
					)).bind("mcssscomparewithnull1"), &HandlerForSSSCompareWithNull2);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(
								ignoringParenCasts(expr(anyOf(
								memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmemset2")))).bind("mcsssmemset3"),
									declRefExpr().bind("mcsssmemset2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmemset2")))).bind("mcsssmemset3")),
									hasDescendant(declRefExpr().bind("mcsssmemset2"))
							)))),
							argumentCountIs(3),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssmemset1"), &HandlerForSSSMemset);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(
								ignoringParenCasts(expr(anyOf(
								memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmemcpy2")))).bind("mcsssmemcpy3"),
									declRefExpr().bind("mcsssmemcpy2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssmemcpy2")))).bind("mcsssmemcpy3")),
									hasDescendant(declRefExpr().bind("mcsssmemcpy2"))
							)))),
							argumentCountIs(3),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssmemcpy1"), &HandlerForSSSMemcpy);

		Matcher.addMatcher(declStmt(hasDescendant(
				varDecl(hasInitializer(ignoringParenCasts(
						anyOf(
								conditionalOperator(has(declRefExpr())).bind("mcsssconditionalinitializer2"),
								conditionalOperator(hasDescendant(declRefExpr())).bind("mcsssconditionalinitializer2")
						)
				))).bind("mcsssconditionalinitializer3")
					)).bind("mcsssconditionalinitializer1"), &HandlerForSSSConditionalInitializer);

		Matcher.addMatcher(binaryOperator(allOf(
				anyOf(hasOperatorName("="), hasOperatorName("=="), hasOperatorName("!="),
						hasOperatorName("<"), hasOperatorName(">"), hasOperatorName("<="), hasOperatorName(">=")),
				hasLHS(expr(anyOf(
						hasDescendant(declRefExpr().bind("mcsssassignment2")),
						declRefExpr().bind("mcsssassignment2")
						)).bind("mcsssassignment5")),
				hasRHS(expr(anyOf(
						cStyleCastExpr(hasDescendant(declRefExpr().bind("mcsssassignment3"))).bind("mcsssassignment4"),
						expr(hasDescendant(declRefExpr().bind("mcsssassignment3")))
						)).bind("mcsssassignment6"))
					)).bind("mcsssassignment1"), &HandlerForSSSAssignment);

		Matcher.addMatcher(declStmt(hasDescendant(
				varDecl(hasInitializer(ignoringParens(
						expr(anything()).bind("mcsssassignment6")
				))).bind("mcsssassignment7")
					)), &HandlerForSSSAssignment);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(anyOf(
								cStyleCastExpr(anyOf(
										memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3"),
											declRefExpr().bind("mcsssparameterpassing2"),
											hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3")),
											hasDescendant(declRefExpr().bind("mcsssparameterpassing2"))
								)).bind("mcsssparameterpassing4"),
								expr(anyOf(
										memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3"),
											declRefExpr().bind("mcsssparameterpassing2"),
											hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3")),
											hasDescendant(declRefExpr().bind("mcsssparameterpassing2"))
								))
							)),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssparameterpassing1"), &HandlerForSSSArgToParameterPassingArray2);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(anyOf(
								cStyleCastExpr(anyOf(
										memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3"),
											declRefExpr().bind("mcsssparameterpassing2"),
											hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3")),
											hasDescendant(declRefExpr().bind("mcsssparameterpassing2"))
								)).bind("mcsssparameterpassing4"),
								expr(anyOf(
										memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3"),
											declRefExpr().bind("mcsssparameterpassing2"),
											hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssparameterpassing2")))).bind("mcsssparameterpassing3")),
											hasDescendant(declRefExpr().bind("mcsssparameterpassing2"))
								))
							)),
							anything()
				)).bind("mcsssparameterpassing1"), &HandlerForSSSArgToReferenceParameterPassing);

		Matcher.addMatcher(returnStmt(allOf(
				hasAncestor(functionDecl().bind("mcsssreturnvalue1")),
					hasDescendant(declRefExpr().bind("mcsssreturnvalue3"))
					)).bind("mcsssreturnvalue2"), &HandlerForSSSReturnValue);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(
								ignoringParenCasts(expr(anyOf(
								memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfread2")))).bind("mcsssfread3"),
									declRefExpr().bind("mcsssfread2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfread2")))).bind("mcsssfread3")),
									hasDescendant(declRefExpr().bind("mcsssfread2"))
							)))),
							argumentCountIs(4),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssfread1"), &HandlerForSSSFRead);

		Matcher.addMatcher(
				callExpr(allOf(
						hasAnyArgument(
								ignoringParenCasts(expr(anyOf(
								memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfwrite2")))).bind("mcsssfwrite3"),
									declRefExpr().bind("mcsssfwrite2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssfwrite2")))).bind("mcsssfwrite3")),
									hasDescendant(declRefExpr().bind("mcsssfwrite2"))
							)))),
							argumentCountIs(4),
							hasAnyArgument(hasType(pointerType()))
				)).bind("mcsssfwrite1"), &HandlerForSSSFWrite);

		Matcher.addMatcher(decl().bind("mcsssdeclutil1"), &HandlerForSSSDeclUtil);

		Matcher.addMatcher(expr(allOf(
				hasParent(expr(
						unaryOperator(hasOperatorName("&")).bind("mcsssaddressof4"))),
					anyOf(
							memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssaddressof")))).bind("mcsssaddressof2"),
							declRefExpr().bind("mcsssaddressof"),
							hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcsssaddressof")))).bind("mcsssaddressof2")),
							hasDescendant(declRefExpr().bind("mcsssaddressof"))
					)
					)).bind("mcsssaddressof3"), &HandlerForSSSAddressOf);

	}

	void HandleTranslationUnit(ASTContext &Context) override 
	{
		Matcher.matchAST(Context);
	}

	private:

	CTUState *m_tu_state_ptr = nullptr;
	CTUState& tu_state() { return *m_tu_state_ptr;}

	MCSSSRecordDecl HandlerForSSSRecordDecl;
	MCSSSVarDecl2 HandlerForSSSVarDecl2;
	MCSSSPointerArithmetic2 HandlerForSSSPointerArithmetic2;
	MCSSSNullToPointer HandlerForSSSNullToPointer;
	MCSSSMalloc2 HandlerForSSSMalloc2;
	MCSSSMallocInitializer2 HandlerForSSSMallocInitializer2;
	MCSSSNullInitializer HandlerForSSSNullInitializer;
	MCSSSFree2 HandlerForSSSFree2;
	MCSSSSetToNull2 HandlerForSSSSetToNull2;
	MCSSSCompareWithNull2 HandlerForSSSCompareWithNull2;
	MCSSSMemset HandlerForSSSMemset;
	MCSSSMemcpy HandlerForSSSMemcpy;
	MCSSSConditionalInitializer HandlerForSSSConditionalInitializer;
	MCSSSAssignment HandlerForSSSAssignment;
	MCSSSArgToParameterPassingArray2 HandlerForSSSArgToParameterPassingArray2;
	MCSSSArgToReferenceParameterPassing HandlerForSSSArgToReferenceParameterPassing;
	MCSSSReturnValue HandlerForSSSReturnValue;
	MCSSSFRead HandlerForSSSFRead;
	MCSSSFWrite HandlerForSSSFWrite;
	MCSSSDeclUtil HandlerForSSSDeclUtil;
	MCSSSAddressOf HandlerForSSSAddressOf;
	Misc1 HandlerMisc1;

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
		MyPPCallbacks(Rewriter& Rewriter_ref, CompilerInstance &CI_ref, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param), m_Rewriter_ref(Rewriter_ref), CI(CI_ref) {}
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
			if (("msetl.h" == file_name_str) || ("mselegacyhelpers.h" == file_name_str)) {
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
			MD->getDefinition();
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
		if (string_begins_with(full_path_name, "/home")) {
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

	thread_local std::vector<CTUState> g_prepared_initial_tu_states;

	class MyFrontendAction : public ASTFrontendActionCompatibilityWrapper1
	{
	public:
		MyFrontendAction() {
			if (1 > g_prepared_initial_tu_states.size()) {
				//assert(false);
			} else {
				/* Initialize the "state" with information computed in the "checker" pass. */
				(*this).m_tu_state = g_prepared_initial_tu_states.back();
				g_prepared_initial_tu_states.pop_back();
			}
		}
		~MyFrontendAction() {
			if (ConvertToSCPP) {
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
			auto& Rewrite = (*this).TheRewriter;
			auto& SM = Rewrite.getSourceMgr();

			/* Here we're applying all the "queued" code modification actions (in the appropriate order,
			starting with "leaf" elements that don't contain subelements in their source text). Note that
			the execution of any code modification action can result in other modification actions being
			added to the "queue" (in any position). So rather than iterating over the elements in typical
			fashion, we always process the element that is designated as "first" at the time, then remove
			the element after processing. */

			auto rit = m_tu_state.m_pending_code_modification_actions.rbegin();
			while (m_tu_state.m_pending_code_modification_actions.rend() != rit) {
				/* Note that we may cause the container to be modified as we're iterating over it. (So we can't use
				a "traditional" loop.) */

				auto retained_it = --(rit.base());
				assert(std::addressof(*retained_it) == std::addressof(*rit));
				{
					auto& action = *retained_it;
					assert(action.second.size() >= 1);
					if (action.first.is_rewritable()) {
						/* Execute and discard all source code text modification functions associated with this source range. */
						auto sub_it = action.second.begin();
						while (action.second.size() >= 1) {
							/* Note that we may cause the container to be modified as we're iterating over it. (So we can't use
							a "traditional" loop.) */

							auto& modification_function = *sub_it;
							modification_function();
							action.second.erase(sub_it);
							sub_it = action.second.begin();
						}
					} else
					{
						/* This source range is marked as supporting only one text replacement operation before
						becoming "stale"/"invalid". So here we execute only the last associated source code text
						modification function (and hope that any other ones, if present, are redundant). */
						auto& modification_function = action.second.back();
						modification_function();
					}
				}

				auto res1 = m_tu_state.m_pending_code_modification_actions.erase(retained_it);

				rit = m_tu_state.m_pending_code_modification_actions.rbegin();
			}

			if (ConvertToSCPP) {
				/* There are some elements in the source text that cannot be reliably modified/replaced
				/updated more than once. So we hold their updated (and updatable) text so that it can be
				applied (only) once at the end of processing. So here (at the end of processing) we're
				iterating through all the "declaration state" objects and applying these updates. */

				for (const auto& ddecl_conversion_state : m_tu_state.m_ddecl_conversion_state_map) {
					auto& ddcs_ref = ddecl_conversion_state.second;
					auto maybe_direct_typeLoc = ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_maybe_typeLoc;

					auto SR = nice_source_range(ddcs_ref.m_ddecl_cptr->getSourceRange(), Rewrite);

					IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

#ifndef NDEBUG
					if (std::string::npos != debug_source_location_str.find(":2803:")) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

					auto maybe_direct_qtype_SR = ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_maybe_source_range_including_any_const_qualifier;
					if (false && !maybe_direct_qtype_SR) {
						if (0 == ddcs_ref.m_indirection_state_stack.size()) {
							assert(ddcs_ref.m_ddecl_cptr);
							auto directTypeLoc = ddcs_ref.m_ddecl_cptr->getTypeSourceInfo()->getTypeLoc();
							auto directTypeSR = nice_source_range(directTypeLoc.getSourceRange(), Rewrite);
							IF_DEBUG(auto old_text1 = Rewrite.getRewrittenText(directTypeSR);)
							auto cq_direct_type_SR = extended_to_include_west_const_if_any(Rewrite, directTypeSR);
							IF_DEBUG(auto old_text2 = Rewrite.getRewrittenText(cq_direct_type_SR);)
							cq_direct_type_SR = extended_to_include_east_const_if_any(Rewrite, cq_direct_type_SR);
							IF_DEBUG(auto old_text3 = Rewrite.getRewrittenText(cq_direct_type_SR);)
							maybe_direct_qtype_SR = cq_direct_type_SR;
						}
					}
					if (maybe_direct_qtype_SR.has_value()) {
						auto direct_qtype_SR = maybe_direct_qtype_SR.value();

						/* If a superset of the region that would be modified here has already been
						modified then this region can no longer be (reliably) modified. */
						if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.properly_contains(direct_qtype_SR)) {
							if (ddcs_ref.direct_qtype_has_been_changed()
								|| ("" != ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_current_params_str)) {

								TheRewriter.ReplaceText(direct_qtype_SR
									, ddcs_ref.current_direct_qtype_str()/* + ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_current_params_str*/);
								m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(direct_qtype_SR);
							}
						} else {
							int q = 5;
						}
					}
					{
						auto initializer_SR_ptr = std::get_if<clang::SourceRange>(&(ddcs_ref.m_initializer_SR_or_insert_before_point));
						auto insert_before_point_ptr = std::get_if<clang::SourceLocation>(&(ddcs_ref.m_initializer_SR_or_insert_before_point));
						if (initializer_SR_ptr) {
							/* If a superset of the region that would be modified here has already been
							modified then this region can no longer be (reliably) modified. */
							if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.properly_contains(*initializer_SR_ptr)) {
								if (ddcs_ref.initializer_has_been_changed()) {
									std::string initializer_str = ddecl_conversion_state.second.m_current_initialization_expr_str;
									TheRewriter.ReplaceText(*initializer_SR_ptr, initializer_str);
									m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(*initializer_SR_ptr);
								}
							} else {
								int q = 5;
							}
						} else if (insert_before_point_ptr) {
							const auto& insert_after_point = (*insert_before_point_ptr).getLocWithOffset(-1);
							/* If a region that contains (both sides of) the insertion point has already been modified
							then this insertion point is no longer (reliably) valid. */
							if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.contains({ insert_after_point, *insert_before_point_ptr })) {
								std::string initializer_str = " = " + ddecl_conversion_state.second.m_current_initialization_expr_str;
								TheRewriter.InsertTextAfterToken(insert_after_point, initializer_str);
							} else {
								int q = 5;
							}
						}
					}
					const auto& indirection_state_stack = ddecl_conversion_state.second.m_indirection_state_stack;
					for (int j = 0; j < indirection_state_stack.size(); j+=1) {
						int i = indirection_state_stack.size() - 1 - j;

						{
							auto suffix_SR_ptr = std::get_if<clang::SourceRange>(&(indirection_state_stack[i].m_suffix_SR_or_insert_before_point));
							auto insert_before_point_ptr = std::get_if<clang::SourceLocation>(&(indirection_state_stack[i].m_suffix_SR_or_insert_before_point));
							if (suffix_SR_ptr) {
								/* If a superset of the region that would be modified here has already been
								modified then this region can no longer be (reliably) modified. */
								if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.properly_contains(*suffix_SR_ptr)) {
									TheRewriter.ReplaceText(*suffix_SR_ptr, indirection_state_stack[i].m_suffix_str);
									DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
									m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(*suffix_SR_ptr);
								}
							} else if (insert_before_point_ptr) {
								const auto& insert_after_point = (*insert_before_point_ptr).getLocWithOffset(-1);
								/* If a region that contains (both sides of) the insertion point has already been modified
								then this insertion point is no longer (reliably) valid. */
								if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.contains({ insert_after_point, *insert_before_point_ptr })) {
										TheRewriter.InsertTextAfterToken((*insert_before_point_ptr).getLocWithOffset(-1)
											, indirection_state_stack[i].m_suffix_str);
								}
							}
						}

						{
							auto prefix_SR_ptr = std::get_if<clang::SourceRange>(&(indirection_state_stack[i].m_prefix_SR_or_insert_before_point));
							auto insert_before_point_ptr = std::get_if<clang::SourceLocation>(&(indirection_state_stack[i].m_prefix_SR_or_insert_before_point));
							if (prefix_SR_ptr) {
								/* If a superset of the region that would be modified here has already been
								modified then this region can no longer be (reliably) modified. */
								if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.properly_contains(*prefix_SR_ptr)) {
									TheRewriter.ReplaceText(*prefix_SR_ptr, indirection_state_stack[i].m_prefix_str);
									m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(*prefix_SR_ptr);
								}
							} else if (insert_before_point_ptr) {
								const auto& insert_after_point = (*insert_before_point_ptr).getLocWithOffset(-1);
								/* If a region that contains (both sides of) the insertion point has already been modified
								then this insertion point is no longer (reliably) valid. */
								if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.contains({ insert_after_point, *insert_before_point_ptr })) {
									TheRewriter.InsertTextBefore(*insert_before_point_ptr, indirection_state_stack[i].m_prefix_str);
									DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
								}
							}
						}
					}
					{
						auto thread_local_specifier_SR_ptr = std::get_if<clang::SourceRange>(&(ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point));
						auto insert_before_point_ptr = std::get_if<clang::SourceLocation>(&(ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point));
						if (thread_local_specifier_SR_ptr) {
							/* If a superset of the region that would be modified here has already been
							modified then this region can no longer be (reliably) modified. */
							if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.properly_contains(*thread_local_specifier_SR_ptr)) {
								if (true) {
									static const std::string thread_local_specifier_str = "thread_local ";
									TheRewriter.ReplaceText(*thread_local_specifier_SR_ptr, thread_local_specifier_str);
									m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(*thread_local_specifier_SR_ptr);
								}
							} else {
								int q = 5;
							}
						} else if (insert_before_point_ptr) {
							const auto& insert_after_point = (*insert_before_point_ptr).getLocWithOffset(-1);
							/* If a region that contains (both sides of) the insertion point has already been modified
							then this insertion point is no longer (reliably) valid. */
							if (!m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.contains({ insert_after_point, *insert_before_point_ptr })) {
								static const std::string thread_local_specifier_str = "thread_local ";
								//TheRewriter.InsertTextAfterToken(insert_after_point, thread_local_specifier_str);
								TheRewriter.InsertTextBefore(*insert_before_point_ptr, thread_local_specifier_str);
							} else {
								int q = 5;
							}
						}
					}
				}
			}

			TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(llvm::outs());

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

	int number_of_instances_of_mse(const std::string& str1) {
		int mse_count = 0;
		{
			static const std::string mse_str = "mse::";
			auto search_start_index = str1.find(mse_str, 0);
			while (std::string::npos != search_start_index) {
				mse_count += 1;
				search_start_index = str1.find(mse_str, search_start_index + mse_str.size());
			}
		}
		{
			static const std::string mse_str = "MSE_";
			auto search_start_index = str1.find(mse_str, 0);
			while (std::string::npos != search_start_index) {
				mse_count += 1;
				search_start_index = str1.find(mse_str, search_start_index + mse_str.size());
			}
		}
		return mse_count;
	}

	int number_of_instances_of_iterator(const std::string& str1) {
		int iterator_count = 0;
		{
			static const std::string iterator_str = "Iterator";
			auto search_start_index = str1.find(iterator_str, 0);
			while (std::string::npos != search_start_index) {
				iterator_count += 1;
				search_start_index = str1.find(iterator_str, search_start_index + iterator_str.size());
			}
		}
		{
			static const std::string iterator_str = "_ITERATOR";
			auto search_start_index = str1.find(iterator_str, 0);
			while (std::string::npos != search_start_index) {
				iterator_count += 1;
				search_start_index = str1.find(iterator_str, search_start_index + iterator_str.size());
			}
		}
		return iterator_count;
	}

	auto& chosen_merge_option_ref(const std::string& first_option, const std::string& second_option) {
		/* Here we're using a very rudimentary heuristic for guessing which merge option might be the
		better one. */
		auto mse_count1 = number_of_instances_of_mse(first_option);
		auto mse_count2 = number_of_instances_of_mse(second_option);
		if (mse_count1 > mse_count2) {
			return first_option;
		} else if (mse_count2 > mse_count1) {
			return second_option;
		} else {
			auto iterator_count1 = number_of_instances_of_iterator(first_option);
			auto iterator_count2 = number_of_instances_of_iterator(second_option);
			if (iterator_count1 > iterator_count2) {
				return first_option;
			} else if (iterator_count2 > iterator_count1) {
				return second_option;
			} else if (first_option.length() > second_option.length()) {
				return first_option;
			} else if (second_option.length() > first_option.length()) {
				return second_option;
			}
		}
		return first_option;
	}

	auto resolve_merge_conflicts_with_best_guess_text(const std::string& source_file_text) {
		std::string retval = source_file_text;
		auto conflict_start_index = retval.find("<<<<<<< ", 0);
		while (std::string::npos != conflict_start_index) {
			auto conflict_start_eol_index = retval.find("\n", conflict_start_index + 1);
			if (std::string::npos == conflict_start_eol_index) {
				break;
			}
			auto conflict_divider_index = retval.find("=======\n", conflict_start_eol_index + 1);
			if (std::string::npos == conflict_divider_index) {
				break;
			}
			auto conflict_divider_eol_index = retval.find("\n", conflict_divider_index + 1);
			auto conflict_end_index = retval.find(">>>>>>> ", conflict_divider_eol_index + 1);
			if (std::string::npos == conflict_end_index) {
				break;
			}
			auto conflict_end_eol_index = retval.find("\n", conflict_end_index + 1);
			if (std::string::npos == conflict_end_eol_index) {
				break;
			}

			auto first_option = retval.substr(conflict_start_eol_index + 1, conflict_divider_index - (conflict_start_eol_index + 1));
			auto second_option = retval.substr(conflict_divider_eol_index + 1, conflict_end_index - (conflict_divider_eol_index + 1));

			retval.replace(conflict_start_index, conflict_end_eol_index - conflict_start_index, chosen_merge_option_ref(first_option, second_option));

			conflict_start_index += first_option.size();
			conflict_start_index = retval.find("<<<<<<< ", conflict_start_index);
		}

		return retval;
	}

	/* At some point the source code file will be the result of a merge of other source code files. (One
	for each translation unit.) So the file might have merge conflicts in it (making it uncompilable). The
	following function will "resolve" any merge conflict in the given file by using a (crude) heuristic to
	choose between the available options. */
	bool resolve_merge_conflicts_with_best_guess_file(const std::string& filepathname) {
		std::fstream fs;
		fs.open(filepathname);
		if (fs.fail()) {
			return false;
		}
		std::string content;

        std::string line;
        while (std::getline(fs, line)) {
			content += line + "\n";
        }

		fs.close();

		auto resolved_content = resolve_merge_conflicts_with_best_guess_text(content);

		fs.open(filepathname, std::ios_base::out | std::ios::trunc);
		if (fs.fail()) {
			return false;
		}
		fs.write(resolved_content.c_str(), resolved_content.size());

		fs.close();

		return true;
	}

	auto buildASTs_and_run(ClangTool& Tool, Options options = Options()) {

        CheckSystemHeader = options.CheckSystemHeader;
        MainFileOnly = options.MainFileOnly;
        ConvertToSCPP = options.ConvertToSCPP;
        CTUAnalysis = options.CTUAnalysis;
        EnableNamespaceImport = options.EnableNamespaceImport;
        SuppressPrompts = options.SuppressPrompts;
        DoNotReplaceOriginalSource = options.DoNotReplaceOriginalSource;
        MergeCommand = options.MergeCommand;
		DoNotResolveMergeConflicts = options.DoNotResolveMergeConflicts;
        ConvertMode = options.ConvertMode;
        ScopeTypeFunctionParameters = options.ScopeTypeFunctionParameters;
        ScopeTypePointerFunctionParameters = options.ScopeTypePointerFunctionParameters || options.ScopeTypeFunctionParameters;

		int Status = Tool.buildASTs(Misc1::s_multi_tu_state_ref().ast_units);

		int ASTStatus = 0;
		if (Status == 1) {
			// Building ASTs failed.
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

		std::cout << "\nThe specified and dependent source files will be replaced/modified. Make sure you have appropriate backup copies before proceeding. \n";
		std::cout << "Continue [y/n]? \n";
		int ich = 0;
		if (SuppressPrompts) {
			ich = int('Y');
		} else {
			do {
				ich = std::getchar();
				//std::putchar(ich);
			} while ((int('y') != ich) && (int('n') != ich) && (int('Y') != ich) && (int('N') != ich));
		}
		if (((int('y') != ich) && (int('Y') != ich)) || (DoNotReplaceOriginalSource)) {
			std::cout << "\n\nThe original source files were not replaced/modified. \n";
		} else {
			for (auto& item_ref : MyFrontendAction::s_file_conversion_record_map) {
				CFileConversionRecord& file_conversion_record_ref = item_ref.second;
				const std::string& original_filename_cref = item_ref.first;
				std::string original_pathname = file_conversion_record_ref.m_path + "/" + original_filename_cref;
				if (1 <= file_conversion_record_ref.m_converted_version_tu_numbers.size()) {
					std::vector<size_t> converted_version_tu_numbers = file_conversion_record_ref.m_converted_version_tu_numbers;
					std::reverse(converted_version_tu_numbers.begin(), converted_version_tu_numbers.end());
					auto first_tu_num = converted_version_tu_numbers.back();
					converted_version_tu_numbers.pop_back();
					std::string first_converted_version_filename = file_conversion_record_ref.m_target_filename
							+ ".converted_" + std::to_string(first_tu_num);
					std::string first_converted_version_pathname = file_conversion_record_ref.m_path + "/" + first_converted_version_filename;

					std::string merge_target_filename = file_conversion_record_ref.m_target_filename + ".converted";
					std::string merge_target_pathname = file_conversion_record_ref.m_path + "/" + merge_target_filename;
					std::rename(first_converted_version_pathname.c_str(), merge_target_pathname.c_str());

					for (auto tu_num : converted_version_tu_numbers) {
						std::string converted_version_filename = file_conversion_record_ref.m_target_filename
								+ ".converted_" + std::to_string(tu_num);
						std::string converted_version_pathname = file_conversion_record_ref.m_path + "/" + converted_version_filename;

						std::string merge_command_str = "merge ";
						if ("" != MergeCommand) {
							merge_command_str = MergeCommand + " ";
						}
						merge_command_str += merge_target_pathname + " ";
						merge_command_str += original_pathname + " ";
						merge_command_str += converted_version_pathname;
						exec(merge_command_str.c_str());
						std::remove(converted_version_pathname.c_str());

						if (!DoNotResolveMergeConflicts) {
							resolve_merge_conflicts_with_best_guess_file(merge_target_pathname);
						}
					}
					std::remove(original_pathname.c_str());
					std::rename(merge_target_pathname.c_str(), original_pathname.c_str());
				} else {
					int q = 7;
				}
			}
			std::cout << "\n\nThe specified and dependent source files have been replaced. \n";
		}

		return retval;
	}
}

#endif //__CONVERTER_MODE1_H
