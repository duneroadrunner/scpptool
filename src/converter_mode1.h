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

	/* This class specifies a declaration and a level of "indirection"(/"dereference") relative to the declared
	* object. For example, given the declaration "int **var1[5];", (*var1) and (**var1) are 1 and 2 "levels of
	* indirection", respectively, relative to var1. */
	class CDDeclIndirection {
	public:
		CDDeclIndirection(const clang::DeclaratorDecl& ddecl_cref, size_t indirection_level = 0) :
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

	class CIndirectionState {
	public:
		CIndirectionState(const std::string& original, const std::string& current,
				bool current_is_function_type = false, const std::vector<std::string>& params = std::vector<std::string>())
			: m_original(original), m_current(current), m_current_is_function_type(current_is_function_type),
				m_params_original(params), m_params_current(params) {}
		CIndirectionState(const std::string& original, const std::string& current,
				const std::string& array_size_expr)
			: m_original(original), m_current(current), m_original_array_size_expr(array_size_expr) {}
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
		void set_current(const std::string& new_current) {
			m_current = new_current;
		}
		const std::string& current() const {
			return m_current;
		}
		const std::string& original() const {
			return m_original;
		}

	private:
		std::string m_original;
		std::string m_current;

	public:
		std::string m_original_array_size_expr;

		bool m_current_is_function_type = false;
		std::vector<std::string> m_params_original;
		std::vector<std::string> m_params_current;
	};

	class CIndirectionStateStack : public std::vector<CIndirectionState> {};

	/* Given a type and an (empty) CIndirectionStateStack, this function will fill the stack with indications of
	* whether each level of indirection (if any) of the type is of the pointer or the array variety. Pointers
	* can, of course, function as arrays, but context is required to identify those situations. Such identification
	* is not done in this function. It is done elsewhere.  */
	clang::QualType populateQTypeIndirectionStack(CIndirectionStateStack& stack, clang::QualType qtype, int depth = 0) {
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
						return populateQTypeIndirectionStack(stack, PNQT->getInnerType(), depth+1);
					} else {
						assert(false);
					}
				} else {
					int q = 7;
				}
			} else if (llvm::isa<const clang::FunctionType>(l_qtype)) {
				auto FNQT = llvm::cast<const clang::FunctionType>(l_qtype);
				if (FNQT) {
					is_function_type = true;
					l_qtype = FNQT->getReturnType();
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
					auto array_size = CATP->getSize();
					size_text = array_size.toString(10, false);/*check this*/

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
				auto l_type_str = QT.getAsString();

				stack.push_back(CIndirectionState("native array", "native array", size_text));

				return populateQTypeIndirectionStack(stack, QT, depth+1);
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
			auto l_type_str = QT.getAsString();

			stack.push_back(CIndirectionState("native pointer", "native pointer", is_function_type, param_strings));

			return populateQTypeIndirectionStack(stack, QT, depth+1);
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
			assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
			if (QT->isFunctionType()) {
				m_is_a_function = true;
			}
			m_original_direct_qtype = populateQTypeIndirectionStack(m_indirection_state_stack, QT);
			set_current_direct_qtype(m_original_direct_qtype);
			//std::reverse(m_indirection_state_stack.begin(), m_indirection_state_stack.end());
		}
		void set_current_direct_qtype(clang::QualType& new_direct_qtype) {
			m_current_direct_qtype = new_direct_qtype;
			m_current_direct_qtype_is_current = true;
			m_current_direct_qtype_str = m_current_direct_qtype.getAsString();
		}
		void set_current_direct_qtype_str(const std::string& str) {
			m_current_direct_qtype_is_current = false;
			m_current_direct_qtype_str = str;
		}
		void set_indirection_current(size_t indirection_level, const std::string& new_current) {
			if ((*this).m_ddecl_cptr) {
				std::string variable_name = m_ddecl_cptr->getNameAsString();
				if ("buffer" == variable_name) {
					std::string qtype_str = m_ddecl_cptr->getType().getAsString();
					if ("const unsigned char *" == qtype_str) {
						int q = 5;
					}
				}
			}
			(*this).m_indirection_state_stack.at(indirection_level).set_current(new_current);
		}
		const std::string& indirection_current(size_t indirection_level) const {
			return (*this).m_indirection_state_stack.at(indirection_level).current();
		}
		bool is_an_indirect_type(size_t indirection_level = 0) const { return (m_indirection_state_stack.size() > indirection_level); }
		bool has_been_determined_to_be_an_array(size_t indirection_level = 0) const {
			bool retval = false;
			assert((0 == indirection_level) || (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				const auto& current_state = m_indirection_state_stack.at(indirection_level).current();
				if (("inferred array" == current_state) || ("dynamic array" == current_state) || ("native array" == current_state)) {
					retval = true;
				}
			}
			return retval;
		}
		bool has_been_determined_to_be_a_dynamic_array(size_t indirection_level = 0) const {
			bool retval = false;
			assert((0 == indirection_level) || (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				const auto& current_state = m_indirection_state_stack.at(indirection_level).current();
				if ("dynamic array" == current_state) {
					retval = true;
				}
			}
			return retval;
		}

		const DeclaratorDecl* m_ddecl_cptr = nullptr;
		CIndirectionStateStack m_indirection_state_stack;
		clang::QualType m_original_direct_qtype;
		bool m_current_direct_qtype_is_current = false;
		clang::QualType m_current_direct_qtype;
		std::string m_current_direct_qtype_str;

		std::string m_initializer_info_str;
		bool m_original_initialization_has_been_noted = false;
		std::string m_original_initialization_expr_str;
		bool m_original_initialization_expr_is_a_constructor_call = false;
		bool m_original_source_text_has_been_noted = false;
		std::string m_original_source_text_str;

		bool m_is_a_function = false;
		std::vector<const clang::ParmVarDecl*> m_original_function_parameter_decl_cptrs;
		std::string m_function_return_type_original_source_text_str;
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
			CWrapExprTextModifier("mse::TNullableAnyRandomAccessIterator<" + qtype.getAsString() + " >(", ")"),
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

	class CAddressofArraySubscriptExprConversionState : public CExprConversionState {
	public:
		CAddressofArraySubscriptExprConversionState(const clang::UnaryOperator& addrofexpr_cref, Rewriter &Rewrite, const clang::ArraySubscriptExpr& arraysubscriptexpr_cref) : CExprConversionState(addrofexpr_cref, Rewrite), m_arraysubscriptexpr_cptr(&arraysubscriptexpr_cref) {
			assert(clang::UnaryOperatorKind::UO_AddrOf == addrofexpr_cref.getOpcode());
			m_arraysubscriptexpr_shptr = std::make_shared<CExprConversionState>(*m_arraysubscriptexpr_cptr, Rewrite);
			m_children_shptrs.push_back(m_arraysubscriptexpr_shptr);
		}
		const clang::UnaryOperator& addrofexpr_cref() const {
			return (*(static_cast<const clang::UnaryOperator *>(m_expr_cptr)));
		}
		virtual void update_current_text() {
			if (true) {
				/* Do we need to update the kids first? */
				arraysubscriptexpr_shptr()->update_current_text();
			}
			std::string updated_text = "&(" + arraysubscriptexpr_shptr()->m_current_text_str + ")";
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}

		std::shared_ptr<CExprConversionState>& arraysubscriptexpr_shptr() {
			if (1 != m_children_shptrs.size()) {
				assert(false);
				return m_arraysubscriptexpr_shptr;
			} else {
				return m_children_shptrs[0];
			}
		}

	private:
		std::shared_ptr<CExprConversionState> m_arraysubscriptexpr_shptr;
		const clang::ArraySubscriptExpr* m_arraysubscriptexpr_cptr = nullptr;
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


	class CTUState;

	class CReplacementAction {
	public:
		virtual ~CReplacementAction() {}
		virtual void do_replacement(CTUState& state1) const = 0;
	};

	class CDDecl2ReplacementAction : public CReplacementAction {
	public:
		CDDecl2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR,
				const CDDeclIndirection& ddecl_indirection) : m_Rewrite(Rewrite), m_MR(MR), m_ddecl_indirection(ddecl_indirection) {}
		virtual ~CDDecl2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const = 0;
		virtual const clang::DeclaratorDecl* get_ddecl_cptr() const { return m_ddecl_indirection.m_ddecl_cptr; }
		virtual const CDDeclIndirection& ddecl_indirection_cref() const { return m_ddecl_indirection; }

		clang::SourceRange source_range() {
			clang::SourceRange retval = m_ddecl_indirection.m_ddecl_cptr->getSourceRange();
			return retval;
		}
		clang::SourceLocation start_location() {
			clang::SourceLocation retval = source_range().getBegin();
			return retval;
		}
		std::string get_var_name() {
			std::string retval = m_ddecl_indirection.m_ddecl_cptr->getNameAsString();
			return retval;
		}

		Rewriter& m_Rewrite;
		const MatchFinder::MatchResult m_MR;
		CDDeclIndirection m_ddecl_indirection;
	};

	class CExprTextReplacementAction : public CDDecl2ReplacementAction {
	public:
		CExprTextReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const Expr* EX, const std::string& replacement_code) :
					CDDecl2ReplacementAction(Rewrite, MR, ddecl_indirection), m_EX(EX), m_replacement_code(replacement_code) {}
		virtual ~CExprTextReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;
		/*
		{
			Rewriter &Rewrite = m_Rewrite;
			const MatchFinder::MatchResult &MR = m_MR;
			const Expr* EX = m_EX;

			if (EX != nullptr)
			{
				auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);

				if (ConvertToSCPP && (EXSR.isValid())) {
					auto res2 = Rewrite.ReplaceText(EXSR, m_replacement_code);
				} else {
					int q = 7;
				}
			}
		}
		*/

		const Expr* m_EX = nullptr;
		std::string m_replacement_code;
	};

	class CArray2ReplacementAction : public CDDecl2ReplacementAction {
	public:
		using CDDecl2ReplacementAction::CDDecl2ReplacementAction;
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

	class CDDecl2ReplacementActionMap : public std::multimap<CDDeclIndirection, std::shared_ptr<CDDecl2ReplacementAction>> {
	public:
		typedef std::multimap<CDDeclIndirection, std::shared_ptr<CDDecl2ReplacementAction>> base_class;
		iterator insert( const std::shared_ptr<CDDecl2ReplacementAction>& cr_shptr ) {
			iterator retval(end());
			if (!cr_shptr) { assert(false); } else {
				value_type val((*cr_shptr).ddecl_indirection_cref(), cr_shptr);
				retval = base_class::insert(val);
			}
			return retval;
		}
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
					m_initializer_info_str(initializer_info_str) {}
		virtual ~CInitializerArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const DeclStmt* m_DS = nullptr;
		//const CallExpr* m_CE = nullptr;
		const DeclaratorDecl* m_DD = nullptr;
		std::string m_initializer_info_str;
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

	class CDynamicArray2ReplacementActionMap : public CDDecl2ReplacementActionMap {
	public:
		iterator insert( const std::shared_ptr<CDynamicArray2ReplacementAction>& cr_shptr ) {
			return CDDecl2ReplacementActionMap::insert(static_cast<std::shared_ptr<CDDecl2ReplacementAction> >(cr_shptr));
		}
	};

	class CArray2ReplacementActionMap : public CDDecl2ReplacementActionMap {
	public:
		/*
		iterator insert( const std::shared_ptr<CArray2ReplacementAction>& cr_shptr ) {
			return CDDecl2ReplacementActionMap::insert(static_cast<std::shared_ptr<CDDecl2ReplacementAction> >(cr_shptr));
		}
		*/
	};

	class CTUState {
	public:
		/* This container holds the locations of regions of code for which checking is
		(indicated to be) suppressed. */
		CSuppressCheckRegionSet m_suppress_check_region_set;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be a dynamic array. */
		CArray2ReplacementActionMap m_dynamic_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be an array (dynamic or otherwise). */
		CArray2ReplacementActionMap m_array2_contingent_replacement_map;

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
	};

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

	static CTypeIndirectionPrefixAndSuffixItem generate_type_indirection_prefix_and_suffix(CIndirectionStateStack& indirection_state_stack,
			bool direct_type_is_char_type, bool direct_type_is_function_type, bool is_a_function_parameter) {
		CTypeIndirectionPrefixAndSuffixItem retval;

		bool changed_from_original = false;
		std::string replacement_code;
		std::string prefix_str;
		std::string suffix_str;
		std::string post_name_suffix_str;

		if (true) {
			for (size_t i = 0; i < indirection_state_stack.size(); i += 1) {
				bool l_changed_from_original = (indirection_state_stack[i].current() != indirection_state_stack[i].original());

				bool is_char_star = false;
				bool is_function_pointer = false;
				bool is_last_indirection = (indirection_state_stack.size() == (i+1));
				if (is_last_indirection && (direct_type_is_char_type)) {
					is_char_star = true;
					/* For the moment, we  leave "char *" types alone. This will changeat some point. */
					l_changed_from_original = ("native pointer" != indirection_state_stack[i].original());
				} else if (is_last_indirection && direct_type_is_function_type) {
					is_function_pointer = true;
				} else if ((!is_last_indirection) && indirection_state_stack[i+1].current_is_function_type()) {
					is_function_pointer = true;
				}

				if (indirection_state_stack[i].current_is_function_type()) {
					//suffix_str = indirection_state_stack[i].current_params_string() + suffix_str;
				}

				if ("inferred array" == indirection_state_stack[i].current()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						//prefix_str = prefix_str + "";
						suffix_str = "* " + suffix_str;
						retval.m_action_species = "char*";
					} else {
						if (is_last_indirection) {
							//retval.m_direct_type_must_be_non_const = true;
						}
						prefix_str = prefix_str + "MSE_LH_ARRAY_ITERATOR_TYPE(";
						suffix_str = ") " + suffix_str;
						retval.m_action_species = "native pointer to MSE_LH_ARRAY_ITERATOR_TYPE";
					}
				} else if ("dynamic array" == indirection_state_stack[i].current()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						//prefix_str = prefix_str + "";
						suffix_str = "* " + suffix_str;
						retval.m_action_species = "char*";
					} else {
						if (is_last_indirection) {
							retval.m_direct_type_must_be_non_const = true;
						}
						//prefix_str = prefix_str + "mse::lh::TIPointerWithBundledVector<";
						prefix_str = prefix_str + "MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(";
						if (is_a_function_parameter) {
							suffix_str = ") " + suffix_str;
							retval.m_action_species = "native pointer parameter to DYNAMIC_ARRAY_ITERATOR_TYPE";
						} else {
							suffix_str = ") " + suffix_str;
							retval.m_action_species = "native pointer to DYNAMIC_ARRAY_ITERATOR_TYPE";
						}
					}
				} else if ("native array" == indirection_state_stack[i].current()) {
					std::string size_text = indirection_state_stack[i].m_original_array_size_expr;

					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char[] for now. At some point we'll replace it with an mse::string or whatever. */
						//prefix_str = prefix_str + "";
						if (1 == indirection_state_stack.size()) {
							post_name_suffix_str = post_name_suffix_str + "[" + size_text + "]";
						} else {
							suffix_str = "[" + size_text + "]" + suffix_str;
						}
					} else {
						l_changed_from_original = true;
						if (is_a_function_parameter) {
							prefix_str = prefix_str + "MSE_LH_ARRAY_ITERATOR_TYPE(";
							suffix_str = ") " + suffix_str;
							retval.m_action_species = "native array parameter to MSE_LH_ARRAY_ITERATOR_TYPE";
						} else {
							if (is_last_indirection) {
								retval.m_direct_type_must_be_non_const = true;
							}
							prefix_str = prefix_str + "MSE_LH_FIXED_ARRAY_TYPE_PREFIX(" + size_text + ") ";
							suffix_str = "MSE_LH_FIXED_ARRAY_TYPE_SUFFIX(" + size_text + ") " + suffix_str;
							post_name_suffix_str = post_name_suffix_str + " MSE_LH_FIXED_ARRAY_TYPE_POST_NAME_SUFFIX(" + size_text + ")";
							retval.m_action_species = "native array to MSE_LH_FIXED_ARRAY_TYPE";
							if (1 == indirection_state_stack.size()) {
								retval.m_just_a_native_array = true;
								retval.m_native_array_size_text = size_text;
							}
						}
					}
				} else if ("native pointer" == indirection_state_stack[i].current()) {
					if (is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						//prefix_str = prefix_str + "";
						suffix_str = "* " + suffix_str;
						retval.m_action_species = "char*";
					} else if (is_function_pointer) {
						l_changed_from_original = true;
						prefix_str = prefix_str + "std::function<";
						suffix_str = "> " + suffix_str;
						retval.m_action_species = "function pointer to std::function";
					} else {
						if (false/*for now*/) {
							l_changed_from_original = true;
							prefix_str = prefix_str + "mse::TAnyPointer<";
							suffix_str = "> " + suffix_str;
							retval.m_action_species = "native pointer to TAnyPointer";
						} else {
							//prefix_str = prefix_str + "";
							suffix_str = "* " + suffix_str;
							retval.m_action_species = "native pointer";
						}
					}
				} else if ("malloc target" == indirection_state_stack[i].current()) {
					/* We'll just leaving it as a native pointer for now. Ultimately, this won't be the case. */
					if ("native pointer" == indirection_state_stack[i].original()) {
						l_changed_from_original = false;
					}
					//prefix_str = prefix_str + "";
					suffix_str = "* " + suffix_str;
					retval.m_action_species = "malloc target";
				} else {
					int q = 5;
				}

				changed_from_original |= l_changed_from_original;
			}
		}
		retval.m_prefix_str = prefix_str;
		retval.m_suffix_str = suffix_str;
		retval.m_post_name_suffix_str = post_name_suffix_str;
		retval.m_changed_from_original = changed_from_original;

		return retval;
	}

	class CDeclarationReplacementCodeItem {
	public:
		std::string m_replacement_code;
		std::string m_action_species;
		bool m_changed_from_original_type = false;
		bool m_individual_from_compound_declaration = false;
	};

	static CDeclarationReplacementCodeItem generate_declaration_replacement_code(const DeclaratorDecl* DD,
			Rewriter &Rewrite, CDDeclConversionStateMap& ddecl_conversion_state_map, std::string options_str = "") {
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
		} else if ("out" == variable_name) {
			int q = 5;
		}

		clang::StorageClass storage_class = clang::StorageClass::SC_None;
		bool is_extern = false;
		clang::StorageDuration storage_duration = clang::StorageDuration::SD_Automatic;
		bool has_dynamic_storage_duration = false;
		bool is_a_temporary = false;
		bool is_static = false;
		bool is_a_function_parameter = false;
		bool is_member = false;
		bool is_vardecl = false;
		std::string initialization_expr_str;
		bool initialization_expr_is_a_constructor_call = false;
		bool is_function = DD->isFunctionOrFunctionTemplate();

		auto VD = dyn_cast<const clang::VarDecl>(DD);
		if (VD) {
			is_vardecl = true;
			storage_class = VD->getStorageClass();
			is_extern = (clang::StorageClass::SC_Extern == storage_class);
			storage_duration = VD->getStorageDuration();
			has_dynamic_storage_duration = (clang::StorageDuration::SD_Dynamic == storage_duration);
			is_a_temporary = (clang::StorageDuration::SD_FullExpression == storage_duration);
			//is_static = (clang::StorageDuration::SD_Static == storage_duration);
			is_static = (clang::StorageClass::SC_Static == storage_class);
			if ((clang::StorageDuration::SD_Static == storage_duration) && (!is_static)) {
				int q = 5;
			}
			is_a_function_parameter = (VD->isLocalVarDeclOrParm() && (!VD->isLocalVarDecl()));

			if (ddcs_ref.m_original_initialization_has_been_noted) {
				initialization_expr_str = ddcs_ref.m_original_initialization_expr_str;
				if ("" != ddcs_ref.m_original_initialization_expr_str) {
					int q = 5;
				}
			} else {
				if (VD->hasInit()) {
					auto pInitExpr = VD->getInit();
					if (pInitExpr) {
						auto init_expr_source_range = nice_source_range(pInitExpr->getSourceRange(), Rewrite);
						if (init_expr_source_range.isValid()) {
							initialization_expr_str = Rewrite.getRewrittenText(init_expr_source_range);
							if (pInitExpr->IgnoreImplicit()->getStmtClass() == pInitExpr->CXXConstructExprClass) {
								initialization_expr_is_a_constructor_call = true;
								if (string_begins_with(initialization_expr_str, variable_name)) {
									initialization_expr_str = initialization_expr_str.substr(variable_name.length());
								} else {
									int q = 5;
								}
							} else if (variable_name == initialization_expr_str) {
								/* Does this ever occur? */
								initialization_expr_str = "";
							}
						} else {
							int q = 3;
						}
					} else {
						int q = 3;
					}
					ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
					ddcs_ref.m_original_initialization_expr_is_a_constructor_call = initialization_expr_is_a_constructor_call;
				}
				ddcs_ref.m_original_initialization_has_been_noted = true;
			}
		} else {
			auto FD = dyn_cast<const clang::FieldDecl>(DD);
			if (FD) {
				is_member = true;

				if (ddcs_ref.m_original_initialization_has_been_noted) {
					initialization_expr_str = ddcs_ref.m_original_initialization_expr_str;
					if ("" != ddcs_ref.m_original_initialization_expr_str) {
						int q = 5;
					}
				} else {
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
								}
							} else {
								int q = 3;
							}
						} else {
							int q = 3;
						}
						ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
					}
					ddcs_ref.m_original_initialization_has_been_noted = true;
				}
			}
		}
		ddcs_ref.m_original_initialization_has_been_noted = true;

		const clang::Type* TP = QT.getTypePtr();
		auto qtype_str = QT.getAsString();
		auto direct_qtype_str = ddcs_ref.m_current_direct_qtype_str;
		if ("_Bool" == direct_qtype_str) {
			direct_qtype_str = "bool";
		} else if ("const _Bool" == direct_qtype_str) {
			direct_qtype_str = "const bool";
		}

		auto non_const_direct_qtype_str = ddcs_ref.m_current_direct_qtype_str;
		if (ddcs_ref.m_current_direct_qtype_is_current) {
			auto non_const_direct_qtype = ddcs_ref.m_current_direct_qtype;
			non_const_direct_qtype.removeLocalConst();
			non_const_direct_qtype_str = non_const_direct_qtype.getAsString();
		} else {
			static const std::string const_space_str = "const ";
			if (string_begins_with(non_const_direct_qtype_str, const_space_str)) {
				non_const_direct_qtype_str = non_const_direct_qtype_str.substr(const_space_str.size());
			}
		}
		if ("_Bool" == non_const_direct_qtype_str) {
			non_const_direct_qtype_str = "bool";
		}

		auto direct_TP = ddcs_ref.m_current_direct_qtype.getTypePtr();
		if (!direct_TP) {
			return retval;
		}
		bool direct_type_is_function_type = direct_TP->isFunctionType();

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

		bool direct_type_is_char_type = (("char" == direct_qtype_str) || ("const char" == direct_qtype_str));
		bool changed_from_original_type = false;
		std::string replacement_code;
		std::string prefix_str;
		std::string suffix_str;
		std::string post_name_suffix_str;

		auto res4 = generate_type_indirection_prefix_and_suffix(ddcs_ref.m_indirection_state_stack,
				direct_type_is_char_type, direct_type_is_function_type, is_a_function_parameter);
		retval.m_action_species = res4.m_action_species;

		if (res4.m_direct_type_must_be_non_const) {
			direct_qtype_str = non_const_direct_qtype_str;
		}
		prefix_str = res4.m_prefix_str;
		suffix_str = res4.m_suffix_str;
		post_name_suffix_str = res4.m_post_name_suffix_str;

		bool discard_initializer_option_flag = (std::string::npos != options_str.find("[discard-initializer]"));
		std::string initializer_append_str;
		if (!discard_initializer_option_flag) {
			initializer_append_str = ddcs_ref.m_initializer_info_str;
			if (("" == initializer_append_str) && ("" != initialization_expr_str)) {
				if (ddcs_ref.m_original_initialization_expr_is_a_constructor_call) {
					initializer_append_str = initialization_expr_str;
				} else {
					initializer_append_str = " = " + initialization_expr_str;
				}
			}
		}

		bool individual_from_compound_declaration = false;
		//if (("" != prefix_str) || ("" != suffix_str)/* || ("" != post_name_suffix_str)*/)
		if (res4.m_changed_from_original) {
			changed_from_original_type = true;
		} else if (("" != ddcs_ref.m_initializer_info_str) || (discard_initializer_option_flag)) {
			changed_from_original_type = true;
		} else if (true && (2 <= IndividualDeclaratorDecls(DD, Rewrite).size())) {
			/* There is more than one declaration in the declaration statement. We split
			* them so that each has their own separate declaration statement. This counts
			* as a change from the original source code. */
			individual_from_compound_declaration = true;
		}

		if (FND) {
			assert(type_is_function_type);
			if (changed_from_original_type || individual_from_compound_declaration) {
				replacement_code += prefix_str + direct_qtype_str + suffix_str;
			} else {
				replacement_code = ddcs_ref.m_function_return_type_original_source_text_str;
			}
		} else {
			if (changed_from_original_type || individual_from_compound_declaration) {
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
		}

		retval.m_replacement_code = replacement_code;
		retval.m_changed_from_original_type = changed_from_original_type;
		retval.m_individual_from_compound_declaration = individual_from_compound_declaration;
		return retval;
	}

	static void update_declaration(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, std::string options_str = "") {
		const DeclaratorDecl* DD = &ddecl;
		auto SR = nice_source_range(DD->getSourceRange(), Rewrite);

		QualType QT = DD->getType();
		const clang::Type* TP = QT.getTypePtr();
		auto qtype_str = QT.getAsString();

		std::string source_text;
		if (SR.isValid()) {
			source_text = Rewrite.getRewrittenText(SR);
			if ("" == source_text) {
				return;
			}
		} else {
			return;
		}

		std::string variable_name = DD->getNameAsString();

		if (("" == variable_name) || (!TP)) {
			return;
		}

		bool changed_from_original_type = false;

		assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if ((TP->isFunctionType()) || (false)) {
			const clang::FunctionDecl* FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				auto name_str = FND->getNameAsString();
				if (std::string::npos != name_str.find("lodepng_chunk_data_const")) {
					int q = 5;
				}
				auto return_type_source_range = nice_source_range(FND->getReturnTypeSourceRange(), Rewrite);
				if (!(return_type_source_range.isValid())) {
					return;
				}
				auto res = generate_declaration_replacement_code(&ddecl, Rewrite, state1.m_ddecl_conversion_state_map, options_str);
				changed_from_original_type |= res.m_changed_from_original_type;

				static const std::string const_space_str = "const ";
				if (string_begins_with(res.m_replacement_code, const_space_str)) {
					/* FunctionDecl::getReturnTypeSourceRange() seems to not include prefix qualifiers, like
					* "const". Since the source range to be replaced doesn't include the "const " qualifier,
					* if present, here we make sure it's not included in the replacement code either. */
					res.m_replacement_code = res.m_replacement_code.substr(const_space_str.length());
				}

				if (ConvertToSCPP && return_type_source_range.isValid() && (1 <= res.m_replacement_code.size())
						&& changed_from_original_type) {
					auto res2 = Rewrite.ReplaceText(return_type_source_range, res.m_replacement_code);
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
						replacement_code += rdcs_ref.m_current_text_str + "; \n";
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
					int q = 5;
				}
				std::vector<std::string> action_species_list;
				for (const auto& ddecl_cref : ddecls) {
					auto res = generate_declaration_replacement_code(ddecl_cref, Rewrite, state1.m_ddecl_conversion_state_map, options_str);
					changed_from_original_type |= res.m_changed_from_original_type;

					action_species_list.push_back(res.m_action_species);
					replacement_code += res.m_replacement_code;
					replacement_code += "; \n";
				}
				if (replacement_code.size() >= 3) {
					replacement_code = replacement_code.substr(0, replacement_code.size() - 3);
				}

				/* (Only) the source range of the last individual declaration in the declaration statement
				* should encompass the whole statement. */
				auto last_ddecl = ddecls.back();
				auto last_decl_source_range = nice_source_range(last_ddecl->getSourceRange(), Rewrite);

				std::string last_decl_source_text;
				if (last_decl_source_range.isValid()) {
					last_decl_source_text = Rewrite.getRewrittenText(last_decl_source_range);
					if ("" == last_decl_source_text) {
						return;
					}
				} else {
					return;
				}

				if (ConvertToSCPP && last_decl_source_range.isValid() && (3 <= replacement_code.size())
						&& changed_from_original_type) {
					auto res2 = Rewrite.ReplaceText(last_decl_source_range, replacement_code);
				} else {
					int q = 7;
				}
			} else {
				int q = 7;
			}
		}
	}

	void note_array_determination(Rewriter &Rewrite, CTUState& state1, const CDDeclIndirection& ddecl_indirection) {

		auto res1 = state1.m_ddecl_conversion_state_map.insert(*(ddecl_indirection.m_ddecl_cptr));
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;
		bool update_declaration_flag = res1.second;

		if (ddcs_ref.m_indirection_state_stack.size() >= ddecl_indirection.m_indirection_level) {
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(ddecl_indirection.m_indirection_level).current()) {
				ddcs_ref.set_indirection_current(ddecl_indirection.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(ddecl_indirection.m_indirection_level).current()) {
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
			} else if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(i).current()) {
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
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(i).current()) {
				if (("ArraySubscriptExpr" == stmt_indirection_stack[i])
						|| ("pointer arithmetic" == stmt_indirection_stack[i])) {
					ddcs_ref.set_indirection_current(i, "dynamic array");
					retval.update_declaration_flag = true;
					state1_ref.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
					state1_ref.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				}
			} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(i).current()) {
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
			} else if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(i).current()) {
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

	void CMallocArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;
		const BinaryOperator* BO = m_BO;
		const DeclaratorDecl* DD = m_DD;

		if ((BO != nullptr) && (DD != nullptr))
		{
			auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
			auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
			auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
			std::string decl_source_text;
			if (decl_source_range.isValid()) {
				decl_source_text = Rewrite.getRewrittenText(decl_source_range);
			} else {
				return;
			}

			if (ddecl_indirection_cref().m_ddecl_cptr) {
				auto res1 = state1.m_ddecl_conversion_state_map.insert(*(ddecl_indirection_cref().m_ddecl_cptr));
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				bool update_declaration_flag = res1.second;

				if (ddcs_ref.m_indirection_state_stack.size() >= ddecl_indirection_cref().m_indirection_level) {
					if ("inferred array" == ddcs_ref.indirection_current(ddecl_indirection_cref().m_indirection_level)) {
						ddcs_ref.set_indirection_current(ddecl_indirection_cref().m_indirection_level, "dynamic array");
						update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection_cref());
					} else {
						int q = 5;
					}
				} else {
					int q = 7;
				}

				if (update_declaration_flag) {
					//update_declaration(*(ddecl_indirection_cref().m_ddecl_cptr), Rewrite, state1);
				}
			} else {
				int q = 7;
			}

			if (ConvertToSCPP && decl_source_range.isValid() && (BOSR.isValid())) {

				update_declaration(*DD, Rewrite, state1);

				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
				//state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());

				auto res2 = Rewrite.ReplaceText(BOSR, m_bo_replacement_code);
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
			auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
			std::string decl_source_text;
			if (decl_source_range.isValid()) {
				decl_source_text = Rewrite.getRewrittenText(decl_source_range);
			} else {
				return;
			}

			auto res1 = state1.m_ddecl_conversion_state_map.insert(*DD);
			auto ddcs_map_iter = res1.first;
			auto& ddcs_ref = (*ddcs_map_iter).second;

			std::string current_direct_qtype_str = ddcs_ref.m_current_direct_qtype_str;
			std::string initializer_info_str = m_initializer_info_str;
			static const std::string void_str = "void";
			auto void_pos = initializer_info_str.find(void_str);
			if (std::string::npos != void_pos) {
				initializer_info_str.replace(void_pos, void_str.length(), current_direct_qtype_str);
			}

			ddcs_ref.m_initializer_info_str = initializer_info_str;

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
			auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
			std::string decl_source_text;
			if (decl_source_range.isValid()) {
				decl_source_text = Rewrite.getRewrittenText(decl_source_range);
			} else {
				return;
			}

			if (ConvertToSCPP && decl_source_range.isValid() && (CESR.isValid())) {

				update_declaration(*DD, Rewrite, state1);

				//state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
				//state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());

				auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);
				int q = 3;
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
			auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
			auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
			std::string decl_source_text;
			if (decl_source_range.isValid()) {
				decl_source_text = Rewrite.getRewrittenText(decl_source_range);
			} else {
				return;
			}

			if (ConvertToSCPP && decl_source_range.isValid() && (CESR.isValid())) {

				update_declaration(*DD, Rewrite, state1);

				auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);
				int q = 3;
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
				auto res2 = Rewrite.ReplaceText(CESR, m_ce_replacement_code);

				if (DD != nullptr) {
					auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
					auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
					std::string decl_source_text;
					if (decl_source_range.isValid()) {
						decl_source_text = Rewrite.getRewrittenText(decl_source_range);
					} else {
						return;
					}

					if (decl_source_range.isValid()) {
						update_declaration(*DD, Rewrite, state1);
					}
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
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current()) {
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
			if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current()) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "inferred array");
				update_declaration_flag |= true;
				state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			} else if ("malloc target" == ddcs_ref.m_indirection_state_stack.at(m_ddecl_indirection2.m_indirection_level).current()) {
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

	void CExprTextReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const MatchFinder::MatchResult &MR = m_MR;

		const Expr* EX = m_EX;
		if (EX) {
			auto EXSR = nice_source_range(EX->getSourceRange(), (*this).m_Rewrite);
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

					(*this).m_Rewrite.ReplaceText(EXSR, (*excs_shptr_ref).m_current_text_str);
				}
			}
		}
	}

	void CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction::do_replacement(CTUState& state1) const {
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
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CAddressofArraySubscriptExprConversionState>(*UO, m_Rewrite, *ASE);
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

						(*this).m_Rewrite.ReplaceText(UOSR, (*uocs_shptr_ref).m_current_text_str);
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
			//ddcs_ref.m_current_direct_qtype;

			if (llvm::isa<const clang::FunctionType>(ddcs_ref.m_current_direct_qtype)) {
				auto FNQT = llvm::cast<const clang::FunctionType>(ddcs_ref.m_current_direct_qtype);
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
						CIndirectionStateStack indirection_state_stack;
						for (size_t i = rhs_res2.indirection_level; (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size() > i; i += 1) {
							indirection_state_stack.push_back((*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.at(i));
						}
						auto rhs_direct_qtype = (*(rhs_res2.ddecl_conversion_state_ptr)).m_current_direct_qtype;

						bool rhs_direct_type_is_function_type = rhs_direct_qtype->isFunctionType();
						bool rhs_is_a_function_parameter = true;

						std::string rhs_direct_qtype_str = rhs_direct_qtype.getAsString();
						if ("_Bool" == rhs_direct_qtype_str) {
							rhs_direct_qtype_str = "bool";
						} else if ("const _Bool" == rhs_direct_qtype_str) {
							rhs_direct_qtype_str = "const bool";
						}
						bool rhs_direct_type_is_char_type = (("char" == rhs_direct_qtype_str) || ("const char" == rhs_direct_qtype_str));
						auto rhs_non_const_direct_qtype = rhs_direct_qtype;
						rhs_non_const_direct_qtype.removeLocalConst();
						std::string rhs_non_const_direct_qtype_str = rhs_non_const_direct_qtype.getAsString();
						if ("_Bool" == rhs_non_const_direct_qtype_str) {
							rhs_non_const_direct_qtype_str = "bool";
						}

						auto res3 = generate_type_indirection_prefix_and_suffix(indirection_state_stack, rhs_direct_type_is_char_type,
								rhs_direct_type_is_function_type, rhs_is_a_function_parameter);

						if (res3.m_direct_type_must_be_non_const) {
							rhs_direct_qtype_str = rhs_non_const_direct_qtype_str;
						}

						std::string new_param_type_str = res3.m_prefix_str + rhs_direct_qtype_str + res3.m_suffix_str;

						new_function_type_code += new_param_type_str;
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
					if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
						lhs_is_dynamic_array = true;
						lhs_is_array = true;
					} else if ("native array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
						lhs_is_native_array = true;
						lhs_is_array = true;
					} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
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
					if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
						rhs_is_dynamic_array = true;
						rhs_is_array = true;
					} else if ("native array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
						rhs_is_native_array = true;
						rhs_is_array = true;
					} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
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

				std::string CO_prior_text = (*this).m_Rewrite.getRewrittenText(COSR);
				std::string CO_replacement_text;

				std::string cond_prior_text = (*this).m_Rewrite.getRewrittenText(cond_SR);
				std::string cond_replacement_text = cond_prior_text;

				std::string lhs_prior_text = (*this).m_Rewrite.getRewrittenText(lhs_SR);
				std::string lhs_replacement_text = lhs_prior_text;

				std::string rhs_prior_text = (*this).m_Rewrite.getRewrittenText(rhs_SR);
				std::string rhs_replacement_text = rhs_prior_text;
				static const std::string ara_iter_prefix = "mse::TNullableAnyRandomAccessIterator<";

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

					ddcs_ref.m_initializer_info_str = " = " + cocs_shptr_ref.m_current_text_str;

					update_declaration(*m_var_DD, m_Rewrite, state1);
				}

			}
		}
	}


	/**********************************************************************************************************************/
	class MCSSSArrayToPointerDecay : public MatchFinder::MatchCallback
	{
	public:
		MCSSSArrayToPointerDecay (Rewriter &Rewrite, CTUState& state1)
	: Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			if ((MR.Nodes.getNodeAs<clang::CastExpr>("mcsssarraytopointerdecay") != nullptr))
			{
				const CastExpr* CE = MR.Nodes.getNodeAs<clang::CastExpr>("mcsssarraytopointerdecay");

				auto SR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation SL = SR.getBegin();
				SourceLocation SLE = SR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSL = ASTC->getFullLoc(SL);

				auto source_location_str = SL.printToString(*MR.SourceManager);
				std::string source_text;
				if (SL.isValid() && SLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(SL, SLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, SL))
				{
					/*intentionally left blank*/
				}
				else
				{
					{
						if (false) {
							std::cout << "sss1.2:" << "array to pointer decay:";
							std::cout << SL.printToString(*MR.SourceManager) << ":" << std::endl;

							//XMLDocOut.XMLAddNode(MR.Context, SL, "sss1.2", "array to pointer decay: ");
							//JSONDocOUT.JSONAddElement(MR.Context, SL, "sss1.2", "array to pointer decay: ");
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};
	/**********************************************************************************************************************/
	class MCSSSNativePointer : public MatchFinder::MatchCallback
	{
	public:
		MCSSSNativePointer (Rewriter &Rewrite) : Rewrite(Rewrite) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			if ((MR.Nodes.getNodeAs<clang::VarDecl>("mcsssnativepointer") != nullptr))
			{
				const VarDecl *VD = MR.Nodes.getNodeAs<clang::VarDecl>("mcsssnativepointer");

				auto SR = nice_source_range(VD->getSourceRange(), Rewrite);
				SourceLocation SL = SR.getBegin();
				SourceLocation SLE = SR.getEnd();

				ASTContext* const ASTC = MR.Context;
				FullSourceLoc FSL = ASTC->getFullLoc(SL);

				auto source_location_str = SL.printToString(*MR.SourceManager);
				std::string source_text;
				if (SL.isValid() && SLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(SL, SLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, SL))
				{
					/*intentionally left blank*/
				}
				else
				{
					{
						if (false) {
							std::cout << "sss1.1:" << "native pointer:";
							std::cout << SL.printToString(*MR.SourceManager) << ":" << std::endl;

							//XMLDocOut.XMLAddNode(MR.Context, SL, "sss1.1", "native pointer: ");
							//JSONDocOUT.JSONAddElement(MR.Context, SL, "sss1.1", "native pointer: ");
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
	};

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

				if (std::string::npos != source_location_str.find("163")) {
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
				auto decl_source_range = SR;
				SourceLocation SL = SR.getBegin();
				SourceLocation SLE = SR.getEnd();

				QualType QT = DD->getType();
				const clang::Type* TP = QT.getTypePtr();
				auto qtype_str = QT.getAsString();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSL = ASTC->getFullLoc(SL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = SL.printToString(*MR.SourceManager);

				if (filtered_out_by_location(MR, SL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("147")) {
					int q = 5;
				}

				std::string source_text;
				if (SR.isValid()) {
					source_text = Rewrite.getRewrittenText(SR);
				} else {
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
				}

				auto res1 = (*this).m_state1.m_ddecl_conversion_state_map.insert(*DD);
				auto ddcs_map_iter = res1.first;
				auto& ddcs_ref = (*ddcs_map_iter).second;
				//bool update_declaration_flag = res1.second;

				for (size_t i = 0; (i < ddcs_ref.m_indirection_state_stack.size()); i += 1) {
					if ("native array" == ddcs_ref.m_indirection_state_stack.at(i).current()) {
						m_state1.m_array2_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*DD, i));
					}
				}

				if (nullptr != RHS) {
					auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
					bool lhs_is_an_indirect_type = is_an_indirect_type(DD->getType());
					bool rhs_is_an_indirect_type = is_an_indirect_type(RHS->getType());
					assert(lhs_is_an_indirect_type == rhs_is_an_indirect_type);

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

								std::string rhs_ddecl_current_direct_qtype_str = (*rhs_res2.ddecl_conversion_state_ptr).m_current_direct_qtype_str;
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
										auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");

										static const std::string void_str = "void";
										auto void_pos = (*rhs_res2.ddecl_conversion_state_ptr).m_initializer_info_str.find(void_str);
										if (std::string::npos != void_pos) {
											(*rhs_res2.ddecl_conversion_state_ptr).m_initializer_info_str.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
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

						const clang::ArraySubscriptExpr* array_subscript_expr_cptr = nullptr;
						auto without_leading_addressof_operator_expr_cptr = rhs_res3.without_leading_addressof_operator_expr_cptr->IgnoreParenCasts();
						if (clang::Stmt::StmtClass::ArraySubscriptExprClass == (*(without_leading_addressof_operator_expr_cptr)).getStmtClass()) {
							assert(llvm::isa<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr));
							array_subscript_expr_cptr = llvm::cast<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr);
						}
						if (ConvertToSCPP && array_subscript_expr_cptr) {
							std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction>(Rewrite, MR,
									CDDeclIndirection(*DD, 0), *(rhs_res3.addressof_unary_operator_cptr), *array_subscript_expr_cptr);

							if (ddcs_ref.has_been_determined_to_be_an_array()) {
								(*cr_shptr).do_replacement(m_state1);
							} else {
								m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
							}
						}
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
				SourceLocation SL = SR.getBegin();
				SourceLocation SLE = SR.getEnd();

				QualType QT = DRE->getType();

				const clang::Type* TP = QT.getTypePtr();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FSL = ASTC->getFullLoc(SL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = SL.printToString(*MR.SourceManager);
				std::string source_text;
				if (SL.isValid() && SLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(SL, SLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, SL)) {
					return void();
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
					auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
					std::string decl_source_text;
					if (decl_source_range.isValid()) {
						decl_source_text = Rewrite.getRewrittenText(decl_source_range);
					} else {
						return;
					}
					QT = DD->getType();
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
				auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
				SourceLocation BOSL = BOSR.getBegin();
				SourceLocation BOSLE = BOSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FBOSL = ASTC->getFullLoc(BOSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = BOSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (BOSL.isValid() && BOSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(BOSL, BOSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, BOSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("130")) {
					int q = 5;
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
						auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
						std::string decl_source_text;
						if (decl_source_range.isValid()) {
							decl_source_text = Rewrite.getRewrittenText(decl_source_range);
						} else {
							return;
						}
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

							if (false) {
								num_elements_text = "(";
								num_elements_text += alloc_function_info1.m_num_bytes_arg_source_text;
								num_elements_text += ") / sizeof(";
								num_elements_text += lhs_element_type_str;
								num_elements_text += ")";

								bo_replacement_code += "(" + lhs_source_text + ")";
								bo_replacement_code += ".resize(";
								bo_replacement_code += num_elements_text;
								bo_replacement_code += ")";
							} else {
								if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
									bo_replacement_code = "MSE_LH_REALLOC(";
								} else {
									bo_replacement_code = "MSE_LH_ALLOC(";
								}
								bo_replacement_code += lhs_element_type_str + ", ";
								bo_replacement_code += lhs_source_text + ", ";
								bo_replacement_code += alloc_function_info1.m_num_bytes_arg_source_text + ")";
							}

							auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
							std::string decl_source_text;
							if (decl_source_range.isValid()) {
								decl_source_text = Rewrite.getRewrittenText(decl_source_range);
							} else {
								return;
							}

							auto BOSR = clang::SourceRange(BOSL, BOSLE);
							if (ConvertToSCPP && decl_source_range.isValid() && (BOSR.isValid())
									&& (nullptr != res2.ddecl_conversion_state_ptr)) {
								auto cr_shptr = std::make_shared<CMallocArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), BO, bo_replacement_code);

								if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssmallocinitializer1");
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmallocinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssmallocinitializer3");

			if ((DS != nullptr) && (CE != nullptr) && (DD != nullptr))
			{
				auto DSSR = nice_source_range(DS->getSourceRange(), Rewrite);
				SourceLocation DSSL = DSSR.getBegin();
				SourceLocation DSSLE = DSSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FDSSL = ASTC->getFullLoc(DSSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = DSSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (DSSL.isValid() && DSSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(DSSL, DSSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, DSSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("288")) {
					int q = 5;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */
					std::string num_elements_text/* = before_str + after_str*/;

					if (nullptr != DD) {
						auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
						auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
						std::string decl_source_text;
						if (decl_source_range.isValid()) {
							decl_source_text = Rewrite.getRewrittenText(decl_source_range);
						} else {
							return;
						}
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

						bool lhs_has_been_determined_to_be_an_array = false;
						if ("native pointer" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
							ddcs_ref.set_indirection_current(0, "malloc target");
						} else if ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
							ddcs_ref.set_indirection_current(0, "dynamic array");
							lhs_has_been_determined_to_be_an_array = true;
							//update_declaration_flag = true;
							m_state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*DD, 0));
						} else if ("dynamic array" == ddcs_ref.m_indirection_state_stack.at(0).current()) {
							lhs_has_been_determined_to_be_an_array = true;
						} else {
							assert("native array" != ddcs_ref.m_indirection_state_stack.at(0).current());
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
							std::string initializer_info_str;

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								initializer_info_str = " = MSE_LH_REALLOC(" + element_type_str + ", " + alloc_function_info1.m_realloc_pointer_arg_source_text;
								initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";
							} else {
								if (false) {
									num_elements_text = "(";
									num_elements_text += alloc_function_info1.m_num_bytes_arg_source_text;
									if (true || (("void" != element_type_str) && ("const void" != element_type_str))) {
										num_elements_text += ") / sizeof(";
										num_elements_text += element_type_str;
									} else {
										/* todo: something */
									}
									num_elements_text += ")";

									initializer_info_str = "(" + num_elements_text + ")";
								} else {
									initializer_info_str = " = MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(" + element_type_str + ")";
									initializer_info_str += ", " + alloc_function_info1.m_num_bytes_arg_source_text + ")";
								}
							}

							auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
							std::string decl_source_text;
							if (decl_source_range.isValid()) {
								decl_source_text = Rewrite.getRewrittenText(decl_source_range);
							} else {
								return;
							}

							auto DSSR = clang::SourceRange(DSSL, DSSLE);
							if (ConvertToSCPP && decl_source_range.isValid() && (DSSR.isValid())) {
								auto cr_shptr = std::make_shared<CInitializerArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), DS, initializer_info_str);

								if (lhs_has_been_determined_to_be_an_array) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
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
				auto DSSR = nice_source_range(DS->getSourceRange(), Rewrite);
				SourceLocation DSSL = DSSR.getBegin();
				SourceLocation DSSLE = DSSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FDSSL = ASTC->getFullLoc(DSSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = DSSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (DSSL.isValid() && DSSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(DSSL, DSSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, DSSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("99")) {
					int q = 5;
				}

				Expr::NullPointerConstantKind kind = RHS->IgnoreParenCasts()->isNullPointerConstant(*ASTC, Expr::NullPointerConstantValueDependence());
				if (clang::Expr::NPCK_NotNull != kind) {
					if (false) {
						{
							if (true) {
								if (true) {
									if (nullptr != DD) {
										auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
										auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
										std::string decl_source_text;
										if (decl_source_range.isValid()) {
											decl_source_text = Rewrite.getRewrittenText(decl_source_range);
										} else {
											return;
										}
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
											/* We use a space here because we are currently using the empty string to
											* indicate that the initializer_info_str should be ignored. */
											std::string initializer_info_str = " ";

											auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
											std::string decl_source_text;
											if (decl_source_range.isValid()) {
												decl_source_text = Rewrite.getRewrittenText(decl_source_range);
											} else {
												return;
											}

											auto DSSR = clang::SourceRange(DSSL, DSSLE);
											if (ConvertToSCPP && decl_source_range.isValid() && (DSSR.isValid())) {
												auto cr_shptr = std::make_shared<CInitializerArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), DS, initializer_info_str);

												if (ddcs_ref.has_been_determined_to_be_an_array(0)) {
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfree1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfree3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
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
								auto arg_res2 = infer_array_type_info_from_stmt(*ARG, "malloc target", (*this).m_state1);
								bool arg_is_an_indirect_type = is_an_indirect_type(ARG->getType());

								if (arg_res2.update_declaration_flag) {
									update_declaration(*(arg_res2.ddecl_cptr), Rewrite, m_state1);
								}

								auto arg_QT = ARG->getType();
								const clang::Type* arg_TP = arg_QT.getTypePtr();
								auto arg_type_str = arg_QT.getAsString();

								std::string arg_element_type_str;
								if (arg_TP->isArrayType()) {
									auto ATP = llvm::cast<const clang::ArrayType>(arg_TP);
									assert(nullptr != ATP);
									auto element_type = ATP->getElementType();
									auto type_str = element_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg_element_type_str = type_str;
									}
								} else if (arg_TP->isPointerType()) {
									auto TPP = llvm::cast<const clang::PointerType>(arg_TP);
									assert(nullptr != TPP);
									auto target_type = TPP->getPointeeType();
									auto type_str = target_type.getAsString();
									if (("char" != type_str) && ("const char" != type_str)) {
										arg_element_type_str = type_str;
									}
								}

								if ("" != arg_element_type_str) {
									if (ConvertToSCPP && (arg_res2.ddecl_conversion_state_ptr) && arg_is_an_indirect_type) {
										auto arg_source_text = Rewrite.getRewrittenText(arg_source_range);
										//std::string ce_replacement_code = "(" + arg_source_text + ").resize(0)";
										std::string ce_replacement_code = "MSE_LH_FREE(" + arg_source_text + ")";

										auto cr_shptr = std::make_shared<CFreeDynamicArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(arg_res2.ddecl_cptr), arg_res2.indirection_level), CE, ce_replacement_code);

										if ((*(arg_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(arg_res2.indirection_level)) {
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
				auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
				SourceLocation BOSL = BOSR.getBegin();
				SourceLocation BOSLE = BOSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FBOSL = ASTC->getFullLoc(BOSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = BOSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (BOSL.isValid() && BOSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(BOSL, BOSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, BOSL)) {
					return void();
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*ASTC, Expr::NullPointerConstantValueDependence());
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

								auto cr_shptr = std::make_shared<CExprTextReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level), BO, bo_replacement_code);

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

			if ((BO != nullptr) && (RHS != nullptr) && (LHS != nullptr) && (DRE != nullptr))
			{
				auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
				SourceLocation BOSL = BOSR.getBegin();
				SourceLocation BOSLE = BOSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FBOSL = ASTC->getFullLoc(BOSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = BOSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (BOSL.isValid() && BOSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(BOSL, BOSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, BOSL)) {
					return void();
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*ASTC, Expr::NullPointerConstantValueDependence());
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
								bo_replacement_code += "bool(" + lhs_source_text + ")";

								auto cr_shptr = std::make_shared<CExprTextReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level), BO, bo_replacement_code);

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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemset1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemset2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemset3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
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
									auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
									std::string decl_source_text;
									if (decl_source_range.isValid()) {
										decl_source_text = Rewrite.getRewrittenText(decl_source_range);
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

									auto res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", (*this).m_state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, m_state1);
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
										if (false) {
											ce_replacement_code = "for (size_t i = 0; i < (" + arg_source_text3
													+ ")/sizeof(" + arg1_element_type_str + "); i += 1) { ";
											ce_replacement_code += "(" + arg_source_text1 + ")[i] = " + arg_source_text2 + "; ";
											ce_replacement_code += "}";
										} else {
											ce_replacement_code = "MSE_LH_MEMSET(" + arg_source_text1 + ", " + arg_source_text2 + ", " + arg_source_text3 + ")";
										}

										if (ConvertToSCPP && decl_source_range.isValid() && (CESR.isValid())
												&& (nullptr != res2.ddecl_conversion_state_ptr)) {
											auto cr_shptr = std::make_shared<CMemsetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);

											if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
												(*cr_shptr).do_replacement(m_state1);
											} else {
												m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
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

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSMemcpy : public MatchFinder::MatchCallback
	{
	public:
		MCSSSMemcpy (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssmemcpy1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssmemcpy2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssmemcpy3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
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
										decl_source_text = Rewrite.getRewrittenText(decl_source_range);
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

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", (*this).m_state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, m_state1);
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
								}

								if (ConvertToSCPP && (CESR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CMemcpyArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(m_state1);
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
				auto DSSR = nice_source_range(DS->getSourceRange(), Rewrite);
				SourceLocation DSSL = DSSR.getBegin();
				SourceLocation DSSLE = DSSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FDSSL = ASTC->getFullLoc(DSSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = DSSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (DSSL.isValid() && DSSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(DSSL, DSSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, DSSL)) {
					return void();
				}

				auto decl_source_range = nice_source_range(DD->getSourceRange(), Rewrite);
				auto decl_source_location_str = decl_source_range.getBegin().printToString(*MR.SourceManager);
				std::string decl_source_text;
				if (decl_source_range.isValid()) {
					decl_source_text = Rewrite.getRewrittenText(decl_source_range);
				} else {
					return;
				}
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
						var_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current();
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
							lhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current();
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
							rhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current();
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
			const Expr* LHS = nullptr;
			const Expr* RHS = nullptr;
			if (BO) {
				LHS = BO->getLHS();
				RHS = BO->getRHS();
			}
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssassignment2");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssassignment4");

			if ((BO != nullptr) && (LHS != nullptr) && (RHS != nullptr) && (DRE != nullptr))
			{
				auto BOSR = nice_source_range(BO->getSourceRange(), Rewrite);
				SourceLocation BOSL = BOSR.getBegin();
				SourceLocation BOSLE = BOSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FBOSL = ASTC->getFullLoc(BOSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = BOSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (BOSL.isValid() && BOSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(BOSL, BOSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, BOSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("457")) {
					int q = 5;
				}

				auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", (*this).m_state1);
				auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
				bool lhs_is_an_indirect_type = is_an_indirect_type(LHS->getType());
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

							std::string rhs_ddecl_current_direct_qtype_str = (*rhs_res2.ddecl_conversion_state_ptr).m_current_direct_qtype_str;
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
									auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");

									static const std::string void_str = "void";
									auto void_pos = (*rhs_res2.ddecl_conversion_state_ptr).m_initializer_info_str.find(void_str);
									if (std::string::npos != void_pos) {
										(*rhs_res2.ddecl_conversion_state_ptr).m_initializer_info_str.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
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

					const clang::ArraySubscriptExpr* array_subscript_expr_cptr = nullptr;
					auto without_leading_addressof_operator_expr_cptr = rhs_res3.without_leading_addressof_operator_expr_cptr->IgnoreParenCasts();
					if (clang::Stmt::StmtClass::ArraySubscriptExprClass == (*(without_leading_addressof_operator_expr_cptr)).getStmtClass()) {
						assert(llvm::isa<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr));
						array_subscript_expr_cptr = llvm::cast<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr);
					}
					if (ConvertToSCPP && array_subscript_expr_cptr) {
						std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction>(Rewrite, MR,
								CDDeclIndirection(*(lhs_res2.ddecl_cptr), 0), *(rhs_res3.addressof_unary_operator_cptr), *array_subscript_expr_cptr);

						if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array()) {
							(*cr_shptr).do_replacement(m_state1);
						} else {
							m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
						}
					}
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

	class MCSSSParameterPassing : public MatchFinder::MatchCallback
	{
	public:
		MCSSSParameterPassing (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssparameterpassing1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssparameterpassing2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssparameterpassing3");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssparameterpassing4");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("1507")) {
					int q = 5;
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
						if (std::string::npos != fdecl_source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != fdecl_source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						}

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
								arg_source_text = Rewrite.getRewrittenText(arg_source_range);
							}

							if ((nullptr != param_VD) && (nullptr != arg_EX) && arg_source_range.isValid()) {
								bool lhs_is_an_indirect_type = is_an_indirect_type(param_VD->getType());
								bool rhs_is_an_indirect_type = is_an_indirect_type(arg_EX->getType());
								assert(lhs_is_an_indirect_type == rhs_is_an_indirect_type);

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

								Expr::NullPointerConstantKind kind = arg_EX->isNullPointerConstant(*ASTC, Expr::NullPointerConstantValueDependence());
								if (false && (clang::Expr::NPCK_NotNull != kind)) {

									if ("" != lhs_element_type_str) {
										auto lhs_source_range = nice_source_range(param_VD->getSourceRange(), Rewrite);
										if (ConvertToSCPP && lhs_is_an_indirect_type && lhs_source_range.isValid()) {
											auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);

											auto res4 = generate_type_indirection_prefix_and_suffix(ddcs_ref.m_indirection_state_stack, false/*direct_type_is_char_type*/, false/*direct_type_is_function_type*/, true/*is_a_function_parameter*/);
											if (true) {
												auto direct_qtype = ddcs_ref.m_current_direct_qtype;
												if (res4.m_direct_type_must_be_non_const) {
													direct_qtype.removeLocalConst();
												}
												std::string direct_qtype_str = direct_qtype.getAsString();
												std::string arg_replacement_code = res4.m_prefix_str + direct_qtype_str + res4.m_suffix_str + "()";

												auto cr_shptr = std::make_shared<CExprTextReplacementAction>(Rewrite, MR, CDDeclIndirection(*(param_VD), 0/*indirection_level*/), arg_EX, arg_replacement_code);

												if (ddcs_ref.has_been_determined_to_be_an_array()) {
													(*cr_shptr).do_replacement(m_state1);
												} else {
													m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
													//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												}
											}
										}
									}
									return;
								}

								auto argii_EX = arg_EX->IgnoreParenImpCasts();
								auto argii_stmt_class = argii_EX->getStmtClass();
								if (rhs_is_an_indirect_type && (argii_EX->getStmtClass() == clang::Stmt::StmtClass::CStyleCastExprClass)) {
									bool special_case1_flag = false;
									auto casted_expr = argii_EX->IgnoreParenCasts();
									std::string casted_expr_type_str = casted_expr->getType().getAsString();
									if ("const unsigned char" == lhs_element_type_str) {
										if (("char *" == casted_expr_type_str) || ("const char *" == casted_expr_type_str)) {
											special_case1_flag = true;
										}
									} else if (lhs_element_type_is_const_char) {
										//lhs_element_type_str = "const char";
										if (("unsigned char *" == casted_expr_type_str) || ("const unsigned char *" == casted_expr_type_str)) {
											special_case1_flag = true;
										}
									}
									if (special_case1_flag) {
										auto casted_expr_SR = nice_source_range(casted_expr->getSourceRange(), Rewrite);
										std::string casted_expr_text = Rewrite.getRewrittenText(casted_expr_SR);
										if (ConvertToSCPP) {
											std::string replacement_casted_expr_text = "std::addressof(" + casted_expr_text + "[0])";
											Rewrite.ReplaceText(casted_expr_SR, replacement_casted_expr_text);
										}
										return;
									} else {
										auto CSCE = llvm::cast<const clang::CStyleCastExpr>(argii_EX);
										if (CSCE) {
											auto cast_operation_SR = clang::SourceRange(CSCE->getLParenLoc(), CSCE->getRParenLoc());
											if (ConvertToSCPP && cast_operation_SR.isValid()) {
												auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
												/* This is not the proper way to modify an expression. See the function
												* CConditionalOperatorReconciliation2ReplacementAction::do_replacement() for an example of
												* the proper way to do it. But for now this is good enough. */
												auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");
											}

											auto cast_kind_name = CSCE->getCastKindName();
											auto cast_kind = CSCE->getCastKind();
										} else { assert(false); }
									}
								}

								int lhs_indirection_level_adjustment = 0;
								auto rhs_res3 = leading_addressof_operator_info_from_stmt(*arg_EX);
								if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
									assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

									arg_EX = rhs_res3.without_leading_addressof_operator_expr_cptr;
									lhs_indirection_level_adjustment += 1;

									const clang::ArraySubscriptExpr* array_subscript_expr_cptr = nullptr;
									auto without_leading_addressof_operator_expr_cptr = rhs_res3.without_leading_addressof_operator_expr_cptr->IgnoreParenCasts();
									if (clang::Stmt::StmtClass::ArraySubscriptExprClass == (*(without_leading_addressof_operator_expr_cptr)).getStmtClass()) {
										assert(llvm::isa<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr));
										array_subscript_expr_cptr = llvm::cast<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr);
									}
									if (ConvertToSCPP && array_subscript_expr_cptr) {
										std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CAssignmentTargetConstrainsAddressofArraySubscriptExprArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*param_VD, 0), *(rhs_res3.addressof_unary_operator_cptr), *array_subscript_expr_cptr);

										if (ddcs_ref.has_been_determined_to_be_an_array() || aux_arg_has_been_determined_to_be_array) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									}
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
							ddecl_source_text = Rewrite.getRewrittenText(DDSR);
						} else {
							return;
						}

						auto EXSR = nice_source_range(EX->getSourceRange(), Rewrite);
						std::string expr_source_text;
						if (EXSR.isValid()) {
							expr_source_text = Rewrite.getRewrittenText(EXSR);
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
				auto FNDSR = nice_source_range(FND->getSourceRange(), Rewrite);
				SourceLocation FNDSL = FNDSR.getBegin();
				SourceLocation FNDSLE = FNDSR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FFNDSL = ASTC->getFullLoc(FNDSL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = FNDSL.printToString(*MR.SourceManager);
				std::string source_text;
				if (FNDSL.isValid() && FNDSLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(FNDSL, FNDSLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, FNDSL)) {
					return void();
				}

				if (std::string::npos != source_location_str.find("129")) {
					walkTheAST1(*RS);
					int q = 5;
				}

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
								auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
								/* This is not the proper way to modify an expression. See the function
								* CConditionalOperatorReconciliation2ReplacementAction::do_replacement() for an example of
								* the proper way to do it. But for now this is good enough. */
								auto res2 = Rewrite.ReplaceText(cast_operation_SR, "");
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
							if (array_subscript_expr_cptr) {
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfread1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfread2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfread3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
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
										decl_source_text = Rewrite.getRewrittenText(decl_source_range);
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

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", (*this).m_state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, m_state1);
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
									ce_replacement_code = "MSE_LH_FREAD(" + arg_source_text1 + ", " + arg_source_text2
											+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
								}

								if (ConvertToSCPP && (CESR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CExprTextReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(m_state1);
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

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSFWrite : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFWrite (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfwrite1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfwrite2");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssfwrite3");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto CESR = nice_source_range(CE->getSourceRange(), Rewrite);
				SourceLocation CESL = CESR.getBegin();
				SourceLocation CESLE = CESR.getEnd();

				ASTContext *const ASTC = MR.Context;
				FullSourceLoc FCESL = ASTC->getFullLoc(CESL);

				SourceManager &SM = ASTC->getSourceManager();

				auto source_location_str = CESL.printToString(*MR.SourceManager);
				std::string source_text;
				if (CESL.isValid() && CESLE.isValid()) {
					source_text = Rewrite.getRewrittenText(SourceRange(CESL, CESLE));
				} else {
					return;
				}

				if (filtered_out_by_location(MR, CESL)) {
					return void();
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
										decl_source_text = Rewrite.getRewrittenText(decl_source_range);
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

									res2 = infer_array_type_info_from_stmt(*(*(CE->arg_begin())), "memset/cpy target", (*this).m_state1, DD);

									if (res2.update_declaration_flag) {
										update_declaration(*DD, Rewrite, m_state1);
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
									ce_replacement_code = "MSE_LH_FWRITE(" + arg_source_text1 + ", " + arg_source_text2
											+ ", "+ arg_source_text3 + ", "+ arg_source_text4 + ")";
								}

								if (ConvertToSCPP && (CESR.isValid()) && ("" != ce_replacement_code)) {
									auto cr_shptr = std::make_shared<CExprTextReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
									if ((nullptr != res2.ddecl_conversion_state_ptr)) {
										if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										(*cr_shptr).do_replacement(m_state1);
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
	MyASTConsumer(Rewriter &R, CompilerInstance &CI, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param), HandlerForSSSNativePointer(R), HandlerForSSSArrayToPointerDecay(R, tu_state()),
		HandlerForSSSRecordDecl(R, tu_state()), HandlerForSSSVarDecl2(R, tu_state()), HandlerForSSSPointerArithmetic2(R, tu_state()), HandlerForSSSMalloc2(R, tu_state()),
		HandlerForSSSMallocInitializer2(R, tu_state()), HandlerForSSSNullInitializer(R, tu_state()), HandlerForSSSFree2(R, tu_state()),
		HandlerForSSSSetToNull2(R, tu_state()), HandlerForSSSCompareWithNull2(R, tu_state()), HandlerForSSSMemset(R, tu_state()), HandlerForSSSMemcpy(R, tu_state()),
		HandlerForSSSConditionalInitializer(R, tu_state()), HandlerForSSSAssignment(R, tu_state()), HandlerForSSSParameterPassing(R, tu_state()),
		HandlerForSSSReturnValue(R, tu_state()), HandlerForSSSFRead(R, tu_state()), HandlerForSSSFWrite(R, tu_state()), HandlerMisc1(R, tu_state(), CI)
	{
		//Matcher.addMatcher(varDecl(hasType(pointerType())).bind("mcsssnativepointer"), &HandlerForSSSNativePointer);

		//Matcher.addMatcher(castExpr(allOf(hasCastKind(CK_ArrayToPointerDecay), unless(hasParent(arraySubscriptExpr())))).bind("mcsssarraytopointerdecay"), &HandlerForSSSArrayToPointerDecay);

		Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerMisc1);

		Matcher.addMatcher(clang::ast_matchers::recordDecl().bind("mcsssrecorddecl"), &HandlerForSSSRecordDecl);

		Matcher.addMatcher(clang::ast_matchers::declaratorDecl().bind("mcsssvardecl"), &HandlerForSSSVarDecl2);
		/*
		Matcher.addMatcher(varDecl(anyOf(
				hasInitializer(anyOf(
						expr(cStyleCastExpr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl3")).bind("mcsssvardecl2"),
						expr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl2")
						)),
					clang::ast_matchers::anything()
					)).bind("mcsssvardecl"), &HandlerForSSSVarDecl2);
		*/

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
				hasLHS(anyOf(
						hasDescendant(declRefExpr().bind("mcsssassignment2")),
						declRefExpr().bind("mcsssassignment2")
							)),
				hasRHS(anyOf(
						cStyleCastExpr(hasDescendant(declRefExpr().bind("mcsssassignment3"))).bind("mcsssassignment4"),
						expr(hasDescendant(declRefExpr().bind("mcsssassignment3")))
						))
					)).bind("mcsssassignment1"), &HandlerForSSSAssignment);

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
				)).bind("mcsssparameterpassing1"), &HandlerForSSSParameterPassing);

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

	}

	void HandleTranslationUnit(ASTContext &Context) override 
	{
		Matcher.matchAST(Context);
	}

	private:

	CTUState *m_tu_state_ptr = nullptr;
	CTUState& tu_state() { return *m_tu_state_ptr;}

	MCSSSNativePointer HandlerForSSSNativePointer;
	MCSSSArrayToPointerDecay HandlerForSSSArrayToPointerDecay;
	MCSSSRecordDecl HandlerForSSSRecordDecl;
	MCSSSVarDecl2 HandlerForSSSVarDecl2;
	MCSSSPointerArithmetic2 HandlerForSSSPointerArithmetic2;
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
	MCSSSParameterPassing HandlerForSSSParameterPassing;
	MCSSSReturnValue HandlerForSSSReturnValue;
	MCSSSFRead HandlerForSSSFRead;
	MCSSSFWrite HandlerForSSSFWrite;
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
			if ("mselegacyhelpers.h" == file_name_str) {
				current_fii_shptr()->m_legacyhelpers_include_directive_found = true;
			}
			int q = 5;
		}
	}

	void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) override {
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
