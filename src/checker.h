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

	static const bool g_enforce_scpp_type_indicator_tags = false;

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


	class CElementBeingAnalyzedInfo {
	public:
		auto source_range() const { return m_SR; }
		clang::SourceRange m_SR;
		std::optional<CCPPElement1> m_maybe_corresponding_cpp_element;
		std::string m_relevance_str;
	};
	typedef std::vector<CElementBeingAnalyzedInfo> CElementBeingAnalyzedInfoStack;

	class CErrorRecord {
		public:
		CErrorRecord() : m_id(next_available_id()) {}
		CErrorRecord(SourceManager& SM, clang::SourceRange const& SR, CElementBeingAnalyzedInfoStack ebai_stack, std::string error_description_str = "", std::string tag_name = "")
			: m_SR(SR), m_source_location_str(s_source_location_str(SM, SR.getBegin())), m_error_description_str(error_description_str)
			, m_tag_name(tag_name), m_id(next_available_id()) {

#ifndef NDEBUG
			if (std::string::npos != m_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			std::reverse(ebai_stack.begin(), ebai_stack.end());

			auto last_file_id = SM.getFileID(SR.getBegin());
			auto last_line_num = SM.getPresumedLineNumber(SR.getBegin());
			auto last_column_num = SM.getPresumedColumnNumber(SR.getBegin());
			auto previous_last_file_id = last_file_id;
			auto previous_last_line_num = last_line_num;
			auto previous_last_column_num = last_column_num;

			for (auto& ebai : ebai_stack) {
				auto SL = ebai.source_range().getBegin();
				CElementBeingAnalyzedInfoEx ebaiex{ ebai }; 
				ebaiex.m_file_id = SM.getFileID(SL);
				ebaiex.m_filename_str = SM.getFilename(SL);
				ebaiex.m_line_num = SM.getPresumedLineNumber(SL);
				ebaiex.m_column_num = SM.getPresumedColumnNumber(SL);
				ebaiex.m_source_location_str = s_source_location_str(SM, SL);

				bool far_enough_apart = true;
				if (((ebaiex.m_file_id == last_file_id) && (3/*arbitrary*/ > abs(int(ebaiex.m_line_num) - int(last_line_num))) && (75/*arbitrary*/ > abs(int(ebaiex.m_column_num) - int(last_column_num))))
					|| ((ebaiex.m_file_id == previous_last_file_id) && (3/*arbitrary*/ > abs(int(ebaiex.m_line_num) - int(previous_last_line_num))) && (75/*arbitrary*/ > abs(int(ebaiex.m_column_num) - int(previous_last_column_num))))
					) {
					far_enough_apart = false;
				}
				if (far_enough_apart) {
					m_element_being_analyzed_info_ex_stack.push_back(ebaiex);

					previous_last_file_id = last_file_id;
					previous_last_line_num = last_line_num;
					previous_last_column_num = last_column_num;
					last_file_id = ebaiex.m_file_id;
					last_line_num = ebaiex.m_line_num;
					last_column_num = ebaiex.m_column_num;
				}
			}
		}

		std::string as_a_string1() const {
			std::string retval = m_source_location_str + ": " + m_severity_str + ": " + m_error_description_str;
			if ("" != m_tag_name) {
				retval += " (" + m_tag_name + ") ";
			}
			for (auto& ebaiex : (*this).m_element_being_analyzed_info_ex_stack) {
				std::string str1 = "\n  " + ebaiex.m_source_location_str + ": ";
				if ("" != ebaiex.m_relevance_str) {
					str1 += ebaiex.m_relevance_str;
				} else {
					str1 += "used";
				}
				str1 += " here";
				retval += str1;
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

		clang::SourceRange m_SR;
		std::string m_source_location_str;
		class CElementBeingAnalyzedInfoEx : public CElementBeingAnalyzedInfo {
			public:
			typedef CElementBeingAnalyzedInfo base_class;
			int m_line_num = 0;
			int m_column_num = 0;
			clang::FileID m_file_id;
			std::string m_filename_str;
			std::string m_source_location_str;
		};
		std::vector<CElementBeingAnalyzedInfoEx> m_element_being_analyzed_info_ex_stack;
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

	std::string get_as_quoted_string_for_errmsg(clang::QualType qtype) {
		std::string qtype_str = qtype.getAsString();
		std::string retval = "'" + qtype_str + "'";
		const auto cannonical_qtype = get_cannonical_type(qtype);
		if (cannonical_qtype != qtype) {
			const auto cannonical_qtype_str = cannonical_qtype.getAsString();
			if (cannonical_qtype_str != qtype_str) {
				auto cannonical_qtype_wo_class_space_str = cannonical_qtype_str;
				static const std::string class_space_str = "class ";
				auto class_index = cannonical_qtype_wo_class_space_str.find(class_space_str);
				while (std::string::npos != class_index) {
					cannonical_qtype_wo_class_space_str.replace(class_index, class_space_str.length(), "");
					class_index = cannonical_qtype_wo_class_space_str.find(class_space_str);
				}
				if (cannonical_qtype_wo_class_space_str != qtype_str) {
					auto cannonical_qtype_wo_struct_space_str = cannonical_qtype_wo_class_space_str;
					static const std::string struct_space_str = "struct ";
					auto struct_index = cannonical_qtype_wo_struct_space_str.find(struct_space_str);
					while (std::string::npos != struct_index) {
						cannonical_qtype_wo_struct_space_str.replace(struct_index, struct_space_str.length(), "");
						struct_index = cannonical_qtype_wo_struct_space_str.find(struct_space_str);
					}
					if (cannonical_qtype_wo_struct_space_str != qtype_str) {
						if (("std::string" != qtype_str) && ("std::string_view" != qtype_str)) {
							retval += std::string(" (aka '") + cannonical_qtype_str + "')";
						}
					}
				}
			}
		}
		return retval;
	}

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
		virtual std::string species_str() const override { return "encompasses"; }
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
	struct CFirstCanBeAssignedToSecond : public CPairwiseLifetimeConstraint {
		typedef CPairwiseLifetimeConstraint base_class;
		CFirstCanBeAssignedToSecond(const CAbstractLifetime& first, const CAbstractLifetime& second) : base_class{ first, second } {}
		virtual std::string species_str() const override { return "first_can_be_assigned_to_second"; }
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

	/* The ordinal of the "first" parameter has a value of 1 (as opposed to zero). */
	struct param_ordinal_t {
		/* For some reason the clang AST represents member operator call expressions as non-member
		call expressions with the implicit `this` object expression as the first argument. It does
		not seem to do the corresponding transformation in its representation of (non-static) member 
		operator declarations and their parameters. This class notes if it was initialized with an 
		indication of belonging to a (non-static) member operator and provides separate functions for 
		obtaining  the correctly  adjusted zero based index depending on whether you're accessing a 
		parameter or an argument. */
		struct ns_member_operator_tag {};
		//param_ordinal_t() : m_zb_index(1) {}
		param_ordinal_t(const param_ordinal_t&) = default;
		param_ordinal_t(size_t ordinal) : m_zb_index(int(ordinal) - 1) {}
		param_ordinal_t(ns_member_operator_tag, size_t ordinal) : m_zb_index(int(ordinal) - 1), m_belongs_to_ns_member_operator(true) {}
		operator size_t() const {
			return (m_zb_index + 1);
		};
		std::optional<size_t> as_a_zero_based_argument_index_if_valid() const {
			int i1 = m_belongs_to_ns_member_operator ? (m_zb_index + 1) : m_zb_index;
			if (0 <= i1) {
				return size_t(i1);
			}
			return {};
		}
		static auto as_a_zero_based_argument_index_if_valid(const param_ordinal_t& param_ordinal) {
			return param_ordinal.as_a_zero_based_argument_index_if_valid();
		}
		std::optional<size_t> as_a_zero_based_parameter_index_if_valid() const {
			if (0 <= m_zb_index) {
				return size_t(m_zb_index);
			}
			return {};
		}
		static auto as_a_zero_based_parameter_index_if_valid(const param_ordinal_t& param_ordinal) {
			return param_ordinal.as_a_zero_based_parameter_index_if_valid();
		}
		auto& operator+= (int rhs) {
			m_zb_index += rhs;
			return (*this);
		}
		param_ordinal_t& operator=(const param_ordinal_t&) = default;
		param_ordinal_t& operator=(const size_t& ordinal) {
			(*this) = m_belongs_to_ns_member_operator ? param_ordinal_t(ns_member_operator_tag{}, ordinal)
				: param_ordinal_t(ordinal);
			return (*this);
		}
		bool operator==(const param_ordinal_t& rhs) const { return (m_zb_index == rhs.m_zb_index); };
		bool operator!=(const param_ordinal_t& rhs) const { return !((*this) == rhs); };

		int m_zb_index = 0;
		bool m_belongs_to_ns_member_operator = false;
	};
#define IMPLICIT_THIS_PARAM_ORDINAL param_ordinal_t(0)
 
    inline auto operator+ (param_ordinal_t lhs, int rhs) {
        lhs += (rhs);
        return lhs;
    }
    inline auto operator- (param_ordinal_t lhs, int rhs) {
        lhs += (-rhs);
        return lhs;
    }
}
namespace std
{
  template<> struct hash<checker::param_ordinal_t>
  {
    inline size_t operator()(const checker::param_ordinal_t & v) const
    {
		size_t seed = 0;
		impl::hash_combine(seed, v.m_zb_index);
		//impl::hash_combine(seed, v.m_belongs_to_ns_member_operator);
		return seed;
    }
  };
}

namespace checker {
	class CFunctionLifetimeAnnotations {
	public:
		CAbstractLifetimeSet m_lifetime_set;
		std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> m_param_lifetime_map;
		std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > m_lifetime_constraint_shptrs;
		CAbstractLifetimeSet m_return_value_lifetimes;
		bool m_return_value_lifetimes_is_elided = false;
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

		/* The "element_being_analyzed_info stack" is used to keep track of (the location of) items (and 
		subitems) currently being analyzed. Used for the purpose of providing a call/instantiation stack 
		along with any errors reported during the analysis. */
		CElementBeingAnalyzedInfoStack m_element_being_analyzed_info_stack;

		struct CElementBeingAnalyzedInfoScopeObj {
			CElementBeingAnalyzedInfoScopeObj(CTUState& state1, CElementBeingAnalyzedInfo const& ebai) : m_state1_ptr(&state1), m_ebai(ebai) {
				m_state1_ptr->m_element_being_analyzed_info_stack.push_back(m_ebai);
			}
			CElementBeingAnalyzedInfoScopeObj(const CElementBeingAnalyzedInfoScopeObj& src) : m_state1_ptr(src.m_state1_ptr), m_ebai(src.m_ebai) {
				m_state1_ptr->m_element_being_analyzed_info_stack.push_back(m_ebai);
			}
			~CElementBeingAnalyzedInfoScopeObj() {
				assert(m_state1_ptr);
				m_state1_ptr->m_element_being_analyzed_info_stack.pop_back();
			}
			CTUState* m_state1_ptr = nullptr;
			CElementBeingAnalyzedInfo m_ebai;
		};
		/* An "element_being_analyzed_info scope object", while it exists, indicates that we are in the 
		process of analyzing the item at the given source location. This is useful for providing a 
		call/instantiation stack along with any errors reported during the analysis. */
		auto make_element_being_analyzed_info_scope_obj(CElementBeingAnalyzedInfo const& ebai) {
			return CElementBeingAnalyzedInfoScopeObj(*this, ebai);
		}
		void register_error(SourceManager& SM, clang::SourceRange const& SR, std::string error_description_str = "", std::string tag_name = "") {
			auto res = m_error_records.emplace(CErrorRecord(SM, SR, m_element_being_analyzed_info_stack, error_description_str, tag_name));
			if (res.second) {
				std::cout << (*(res.first)).as_a_string1() << " \n\n";
			}
		}

		std::unordered_map<clang::FunctionDecl const *, CFunctionLifetimeAnnotations> m_function_lifetime_annotations_map;

		std::unordered_map<const clang::Type *, CTypeLifetimeAnnotations> m_type_lifetime_annotations_map;
		void insert_or_assign_type_lifetime_annotations(const clang::Type * TypePtr, CTypeLifetimeAnnotations const& tlta) {
			(*this).m_type_lifetime_annotations_map.insert_or_assign(TypePtr, tlta);

			const auto cannonical_TypePtr = get_cannonical_type_ptr(TypePtr);
			if (cannonical_TypePtr != TypePtr) {
				auto found_it = (*this).m_type_lifetime_annotations_map.find(cannonical_TypePtr);
				if ((*this).m_type_lifetime_annotations_map.end() != found_it) {
					if (tlta.m_lifetime_set.m_primary_lifetimes.size() > found_it->second.m_lifetime_set.m_primary_lifetimes.size()) {
						/* Ideally this won't happen. It seems that we've processed this type before and came up with a 
						different result. This would sometimes occur when we used to evaluate uninstantiated templates. */
						(*this).m_type_lifetime_annotations_map.insert_or_assign(cannonical_TypePtr, tlta);
					}
				} else {
					(*this).m_type_lifetime_annotations_map.insert_or_assign(cannonical_TypePtr, tlta);
				}
			}
		}

		std::unordered_map<const clang::Type *, CTypeLifetimeAnnotations> m_implicit_type_lifetime_annotations_map;
		void insert_or_assign_implicit_type_lifetime_annotations(const clang::Type * TypePtr, CTypeLifetimeAnnotations const& tlta) {
			(*this).m_implicit_type_lifetime_annotations_map.insert_or_assign(TypePtr, tlta);

			const auto cannonical_TypePtr = get_cannonical_type_ptr(TypePtr);
			if (cannonical_TypePtr != TypePtr) {
				auto found_it = (*this).m_implicit_type_lifetime_annotations_map.find(cannonical_TypePtr);
				if ((*this).m_implicit_type_lifetime_annotations_map.end() != found_it) {
					if (tlta.m_lifetime_set.m_primary_lifetimes.size() > found_it->second.m_lifetime_set.m_primary_lifetimes.size()) {
						/* Ideally this won't happen. It seems that we've processed this type before and came up with a 
						different result. This would sometimes occur when we used to evaluate uninstantiated templates. */
						(*this).m_implicit_type_lifetime_annotations_map.insert_or_assign(cannonical_TypePtr, tlta);
					}
				} else {
					(*this).m_implicit_type_lifetime_annotations_map.insert_or_assign(cannonical_TypePtr, tlta);
				}
			}
		}

		std::unordered_map<std::string, CTypeLifetimeAnnotations> m_const_qualified_type_lifetime_annotations_map;
		void insert_or_assign_const_qualified_type_lifetime_annotations(const clang::QualType qtype, CTypeLifetimeAnnotations const& tlta) {
			(*this).m_const_qualified_type_lifetime_annotations_map.insert_or_assign(qtype.getAsString(), tlta);

			const auto cannonical_qtype = get_cannonical_type(qtype);
			if (cannonical_qtype != qtype) {
				auto found_it = (*this).m_const_qualified_type_lifetime_annotations_map.find(cannonical_qtype.getAsString());
				if ((*this).m_const_qualified_type_lifetime_annotations_map.end() != found_it) {
					if (tlta.m_lifetime_set.m_primary_lifetimes.size() > found_it->second.m_lifetime_set.m_primary_lifetimes.size()) {
						/* Ideally this won't happen. It seems that we've processed this type before and came up with a 
						different result. This would sometimes occur when we used to evaluate uninstantiated templates. */
						(*this).m_const_qualified_type_lifetime_annotations_map.insert_or_assign(cannonical_qtype.getAsString(), tlta);
					}
				} else {
					(*this).m_const_qualified_type_lifetime_annotations_map.insert_or_assign(cannonical_qtype.getAsString(), tlta);
				}
			}
		}

		std::unordered_multimap<CAbstractLifetime, std::shared_ptr<CPairwiseLifetimeConstraint> > m_lhs_to_lifetime_constraint_shptr_mmap;
		std::unordered_multimap<CAbstractLifetime, std::shared_ptr<CPairwiseLifetimeConstraint> > m_rhs_to_lifetime_constraint_shptr_mmap;
		std::unordered_map<CAbstractLifetime, CAbstractLifetimeSet> m_lifetime_alias_map;

		std::unordered_map<clang::VarDecl const *, CVariableLifetimeAnnotations> m_vardecl_lifetime_annotations_map;
		std::unordered_map<clang::VarDecl const *, CVariableLifetimeValues> m_vardecl_rhs_lb_lifetime_values_map;
		std::unordered_map<clang::VarDecl const *, CVariableLifetimeValues> m_vardecl_lhs_lb_lifetime_values_map;
		std::unordered_map<clang::Expr const *, CExpressionLifetimeValues> m_expr_rhs_lb_lifetime_values_map;
		std::unordered_map<clang::Expr const *, CExpressionLifetimeValues> m_expr_lhs_lb_lifetime_values_map;

		std::unordered_map<clang::FieldDecl const *, CAbstractLifetimeSet> m_fielddecl_to_abstract_lifetime_map;
		std::unordered_map<clang::CXXBaseSpecifier const *, CAbstractLifetimeSet> m_base_class_to_abstract_lifetime_map;

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
		std::optional<CAbstractLifetimeSet> corresponding_abstract_lifetime_set_if_any(clang::CXXBaseSpecifier const * CXXBS) const {
			if (CXXBS) {
				auto iter1 = this->m_base_class_to_abstract_lifetime_map.find(CXXBS);
				if (m_base_class_to_abstract_lifetime_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CAbstractLifetimeSet>{};
		}

		std::optional<CVariableLifetimeValues> corresponding_rhs_lb_lifetime_values_if_any(clang::VarDecl const * VD) const {
			if (VD) {
				auto iter1 = m_vardecl_rhs_lb_lifetime_values_map.find(VD);
				if (m_vardecl_rhs_lb_lifetime_values_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CVariableLifetimeValues>{};
		}
		std::optional<CVariableLifetimeValues> corresponding_lhs_lb_lifetime_values_if_any(clang::VarDecl const * VD) const {
			if (VD) {
				auto iter1 = m_vardecl_lhs_lb_lifetime_values_map.find(VD);
				if (m_vardecl_lhs_lb_lifetime_values_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CVariableLifetimeValues>{};
		}
		std::optional<CExpressionLifetimeValues> corresponding_rhs_lb_lifetime_values_if_any(clang::Expr const * E) const {
			if (E) {
				auto iter1 = m_expr_rhs_lb_lifetime_values_map.find(E);
				if (m_expr_rhs_lb_lifetime_values_map.end() != iter1) {
					return iter1->second;
				}
			}
			return std::optional<CExpressionLifetimeValues>{};
		}
		std::optional<CExpressionLifetimeValues> corresponding_lhs_lb_lifetime_values_if_any(clang::Expr const * E) const {
			if (E) {
				auto iter1 = m_expr_lhs_lb_lifetime_values_map.find(E);
				if (m_expr_lhs_lb_lifetime_values_map.end() != iter1) {
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

	bool is_raw_pointer_or_equivalent(clang::Type const * type_ptr) {
		if (!type_ptr) {
			return false;
		}
		type_ptr = get_cannonical_type_ptr(type_ptr);

		IF_DEBUG(std::string type_str = get_as_string(type_ptr);)
		IF_DEBUG(std::string type_classname_str = type_ptr->getTypeClassName();)

		bool is_pointer_or_equivalent = true;
		if (!type_ptr->isPointerType()) {
			const auto RD = type_ptr->getAsRecordDecl();
			if (!RD) {
				is_pointer_or_equivalent = false;
			} else {
				/* `mse::us::impl::TPointerForLegacy<>` is sometimes used as (a functionally
				equivalent) substitute for native pointers that can act as a base class. */
				const auto CXXCE_rw_type_ptr_str = RD->getQualifiedNameAsString();
				DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
				DECLARE_CACHED_CONST_STRING(TPointer_str, mse_namespace_str() + "::us::impl::TPointer");
				if ((TPointer_str == CXXCE_rw_type_ptr_str) || (TPointerForLegacy_str == CXXCE_rw_type_ptr_str)) {
					int q = 5;
				} else {
					is_pointer_or_equivalent = false;
				}
			}
		}
		return is_pointer_or_equivalent;
	}
	bool is_raw_pointer_or_equivalent(const clang::QualType& qtype) {
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, false);
		return is_raw_pointer_or_equivalent(qtype.getTypePtr());
	}

	inline std::optional<clang::QualType> pointee_type_if_any(clang::QualType const& qtype) {
		std::optional<clang::QualType> retval;
		auto peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(peeled_qtype, retval);
		if (is_raw_pointer_or_equivalent(peeled_qtype)) {
			if (peeled_qtype->isPointerType()) {
				retval = get_cannonical_type(peeled_qtype->getPointeeType());
			} else {
				auto RD = peeled_qtype->getAsRecordDecl();
				if (RD) {
					const auto RD_qtype_str = RD->getQualifiedNameAsString();
					DECLARE_CACHED_CONST_STRING(TPointerForLegacy_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
					DECLARE_CACHED_CONST_STRING(TPointer_str, mse_namespace_str() + "::us::impl::TPointer");
					if ((TPointer_str == RD_qtype_str) || (TPointerForLegacy_str == RD_qtype_str)) {
						auto maybe_qtype2 = get_first_template_parameter_if_any(peeled_qtype);
						if (maybe_qtype2.has_value()) {
							retval = maybe_qtype2.value();
						}
					}
				}
			}
		} else if (peeled_qtype->isReferenceType()) {
			retval = get_cannonical_type(peeled_qtype->getPointeeType());
		}
		return retval;
	}

	inline std::vector<std::string> known_fixed_owning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back("const std::unique_ptr");
			tl_names.push_back("std::tuple");
			tl_names.push_back("std::pair");
			tl_names.push_back("std::array");

			tl_names.push_back(mse_namespace_str() + "::TXScopeOwnerPointer");
			tl_names.push_back(mse_namespace_str() + "::mstd::tuple");
			tl_names.push_back(mse_namespace_str() + "::xscope_tuple");

			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_fixed_optional");
			tl_names.push_back(mse_namespace_str() + "::xscope_fixed_optional");
			tl_names.push_back(mse_namespace_str() + "::fixed_optional");

			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_array");
			tl_names.push_back(mse_namespace_str() + "::xscope_nii_array");
			tl_names.push_back(mse_namespace_str() + "::nii_array");
			tl_names.push_back(mse_namespace_str() + "::mstd::array");

			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_fixed_vector");
			tl_names.push_back(mse_namespace_str() + "::xscope_fixed_nii_vector");
			tl_names.push_back(mse_namespace_str() + "::fixed_nii_vector");
			tl_names.push_back(mse_namespace_str() + "::us::impl::fixed_nii_vector_base");

			tl_names.push_back(mse_namespace_str() + "::xscope_fixed_nii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::fixed_nii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::us::impl::fixed_nii_basic_string_base");

			tl_names.push_back(mse_namespace_str() + "::xscope_fixed_any");
			tl_names.push_back(mse_namespace_str() + "::fixed_any");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_fixed_nonowning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back(mse_namespace_str() + "::xscope_borrowing_fixed_nii_vector");
			tl_names.push_back(mse_namespace_str() + "::xscope_borrowing_fixed_nii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::xscope_borrowing_fixed_optional");
			tl_names.push_back(mse_namespace_str() + "::xscope_borrowing_fixed_any");
			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_borrowing_fixed_vector");

			tl_names.push_back(mse_namespace_str() + "::xscope_accessing_fixed_nii_vector");
			tl_names.push_back(mse_namespace_str() + "::xscope_accessing_fixed_nii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::xscope_accessing_fixed_optional");
			tl_names.push_back(mse_namespace_str() + "::xscope_accessing_fixed_any");
			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_accessing_fixed_vector");
		}
		return tl_names;
	}
	inline std::vector<std::string> known_unprotected_dynamic_owning_pointer_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back("std::unique_ptr");
			tl_names.push_back("std::shared_ptr");

			tl_names.push_back(mse_namespace_str() + "::TRefCountingPointer");
			tl_names.push_back(mse_namespace_str() + "::TRefCountingNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TRefCountingFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TRefCountingConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TRefCountingNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TRefCountingFixedConstPointer");

			tl_names.push_back(mse_namespace_str() + "::TAnyPointer");
			tl_names.push_back(mse_namespace_str() + "::TAnyNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TAnyFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TAnyConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TAnyNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TAnyFixedConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopeAnyFixedConstPointer");

			tl_names.push_back(mse_namespace_str() + "::TPolyPointer");
			tl_names.push_back(mse_namespace_str() + "::TPolyNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TPolyFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TPolyConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TPolyNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TPolyFixedConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TXScopePolyFixedConstPointer");

			tl_names.push_back(mse_namespace_str() + "::us::impl::TAsyncSharedV2ReadWritePointerBase");
			tl_names.push_back(mse_namespace_str() + "::us::impl::TAsyncSharedV2ReadWriteConstPointerBase");
			tl_names.push_back(mse_namespace_str() + "::us::impl::TAsyncSharedV2ReadOnlyPointerBase");
			tl_names.push_back(mse_namespace_str() + "::us::impl::TAsyncSharedV2ReadOnlyConstPointerBase");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_string_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back("std::basic_string");
			tl_names.push_back("std::string");

			tl_names.push_back(mse_namespace_str() + "::us::impl::gnii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::nii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::stnii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::mtnii_basic_string");
			tl_names.push_back(mse_namespace_str() + "::mstd::basic_string");
			tl_names.push_back(mse_namespace_str() + "::impl::ns_gnii_basic_string::Tgnii_basic_string_ss_iterator_type");
			tl_names.push_back(mse_namespace_str() + "::impl::ns_gnii_basic_string::Tgnii_basic_string_ss_const_iterator_type");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_benign_move_unprotected_dynamic_owning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names = known_unprotected_dynamic_owning_pointer_names();
			{
				auto tmp_container_names = known_string_container_names();
				tl_names.insert(tl_names.end(), tmp_container_names.begin(), tmp_container_names.end());
			}

			tl_names.push_back("std::vector");
			tl_names.push_back("std::list");
			tl_names.push_back("std::map");
			tl_names.push_back("std::set");
			tl_names.push_back("std::multimap");
			tl_names.push_back("std::multiset");

			tl_names.push_back(mse_namespace_str() + "::us::impl::gnii_vector");
			tl_names.push_back(mse_namespace_str() + "::nii_vector");
			tl_names.push_back(mse_namespace_str() + "::stnii_vector");
			tl_names.push_back(mse_namespace_str() + "::mtnii_vector");
			tl_names.push_back(mse_namespace_str() + "::mstd::vector");
			tl_names.push_back(mse_namespace_str() + "::impl::ns_gnii_vector::Tgnii_vector_ss_iterator_type");
			tl_names.push_back(mse_namespace_str() + "::impl::ns_gnii_vector::Tgnii_vector_ss_const_iterator_type");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_unprotected_dynamic_owning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names = known_benign_move_unprotected_dynamic_owning_container_names();

			tl_names.push_back("std::optional");
			tl_names.push_back("std::unordered_map");
			tl_names.push_back("std::unordered_set");
			tl_names.push_back("std::unordered_multimap");
			tl_names.push_back("std::unordered_multiset");
			tl_names.push_back("std::dequeue");

			tl_names.push_back(mse_namespace_str() + "::us::impl::ns_optional::optional_base2");
			tl_names.push_back(mse_namespace_str() + "::xscope_optional");
			tl_names.push_back(mse_namespace_str() + "::optional");
			tl_names.push_back(mse_namespace_str() + "::mstd::optional");
			tl_names.push_back(mse_namespace_str() + "::st_optional");
			tl_names.push_back(mse_namespace_str() + "::mt_optional");
			tl_names.push_back(mse_namespace_str() + "::TOptionalElementFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TOptionalElementFixedConstPointer");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_benign_move_protected_dynamic_owning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_vector");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_protected_dynamic_owning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names = known_benign_move_protected_dynamic_owning_container_names();

			tl_names.push_back(mse_namespace_str() + "::rsv::xslta_optional");
		}
		return tl_names;
	}

	inline std::vector<std::string> known_dynamic_nonowning_container_names() {
		thread_local std::vector<std::string> tl_names;
		if (0 == tl_names.size()) {
			tl_names.push_back("std::basic_string_view");
			tl_names.push_back("std::string_view");
			tl_names.push_back("std::span");

			tl_names.push_back("__gnu_cxx::__normal_iterator");
			//__gnu_cxx::__normal_iterator<int *, std::vector<int>>
			//std::vector<int>::iterator
			//tl_names.push_back("const char *");

			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDRegisteredFixedConstPointer");

			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredNotNullPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredNotNullConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TNDCRegisteredFixedConstPointer");

			tl_names.push_back(mse_namespace_str() + "::TNoradPointer");
			tl_names.push_back(mse_namespace_str() + "::TNoradNotNullPointer");
			//tl_names.push_back(mse_namespace_str() + "::TNoradFixedPointer");
			tl_names.push_back(mse_namespace_str() + "::TNoradConstPointer");
			tl_names.push_back(mse_namespace_str() + "::TNoradNotNullConstPointer");
			//tl_names.push_back(mse_namespace_str() + "::TNoradFixedConstPointer");
		}
		return tl_names;
	}

	struct known_containers_state_t {
		std::vector<std::string> tl_known_container_names;
		std::unordered_set<std::string_view> known_container_name_svs;
		std::unordered_set<std::string_view> known_container_truncated_name_svs;
		size_t length_of_shortest_container_name = 0;
	};

	inline void set_up_known_containers_state(known_containers_state_t& known_containers_state_ref) {
		if (known_containers_state_ref.tl_known_container_names.size() > 0) {
			known_containers_state_ref.length_of_shortest_container_name = known_containers_state_ref.tl_known_container_names.front().length();
		}

		for (auto& name : known_containers_state_ref.tl_known_container_names) {
			known_containers_state_ref.known_container_name_svs.insert(name);
			if (name.length() < known_containers_state_ref.length_of_shortest_container_name) {
				known_containers_state_ref.length_of_shortest_container_name = name.length();
			}
		}
		for (auto name_sv : known_containers_state_ref.known_container_name_svs) {
			known_containers_state_ref.known_container_truncated_name_svs.insert(name_sv.substr(0, known_containers_state_ref.length_of_shortest_container_name));
		}
	}

	inline void set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state_t& known_containers_state_ref, std::vector<std::function<std::vector<std::string>()> > const& nameset_generating_functions) {
		if (0 == known_containers_state_ref.known_container_name_svs.size()) {
			for (auto const& nameset_generating_function : nameset_generating_functions) {
				auto tmp_container_names = nameset_generating_function();
				known_containers_state_ref.tl_known_container_names.insert(known_containers_state_ref.tl_known_container_names.end(), tmp_container_names.begin(), tmp_container_names.end());
			}

			set_up_known_containers_state(known_containers_state_ref);
		}
	}

	inline bool is_container_recognized_from_given_set_state(clang::QualType const& qtype, known_containers_state_t const & known_containers_state) {
		bool retval = false;
		auto const peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(peeled_qtype, retval);

		const auto CXXRD = peeled_qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();
			std::string_view qname_sv{ qname };

			if (string_begins_with(qname_sv, mse_namespace_str()) || (string_begins_with(qname_sv, "std"))
				|| (string_begins_with(qname_sv, "__gnu_cxx"))) {

				auto truncated_qname_sv = qname_sv.substr(0, known_containers_state.length_of_shortest_container_name);
				auto found_it = known_containers_state.known_container_truncated_name_svs.find(truncated_qname_sv);
				if (known_containers_state.known_container_truncated_name_svs.end() != found_it) {
					for (auto const & name : known_containers_state.tl_known_container_names) {
						if (string_begins_with(qname_sv, name)) {
							return true;
						}
					}
				}
			}
		}

		return retval;
	}

	inline bool is_recognized_nonowning_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_dynamic_nonowning_container_names
				, known_fixed_nonowning_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_unprotected_dynamic_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_unprotected_dynamic_owning_container_names
				, known_dynamic_nonowning_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_protected_dynamic_owning_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_protected_dynamic_owning_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_owning_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_unprotected_dynamic_owning_container_names
				, known_protected_dynamic_owning_container_names
				, known_fixed_owning_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_benign_move_dynamic_owning_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_benign_move_unprotected_dynamic_owning_container_names
				, known_benign_move_protected_dynamic_owning_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_dynamic_owning_pointer(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_unprotected_dynamic_owning_pointer_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline bool is_recognized_string_container(clang::QualType const& qtype) {
		thread_local known_containers_state_t known_containers_state;
		set_up_known_containers_state_from_given_nameset_generating_functions_if_necessary(known_containers_state, {
				known_string_container_names
			});

		return is_container_recognized_from_given_set_state(qtype, known_containers_state);
	}

	inline void apply_to_all_owned_types(clang::QualType qtype, const std::function<void(clang::QualType qtype, std::optional<clang::Decl const *>, std::optional<clang::CXXBaseSpecifier const *>)>& fn1
		, std::optional<clang::Decl const *> maybe_D = {}, std::optional<clang::CXXBaseSpecifier const *> maybe_CXXBS = {}, int depth = 0) {

		MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
		IF_DEBUG(const auto qtype_str = qtype.getAsString();)

		depth += 1;
		thread_local std::unordered_set<clang::Type const *> tl_already_processed_set;
		if (20/*arbitrary*/ < depth) {
			/* to prevent infinite recursion */
			auto found_it = tl_already_processed_set.find(qtype.getTypePtr());
			if (tl_already_processed_set.end() != found_it) {
				int q = 5;
				return;
			} else {
				tl_already_processed_set.insert(qtype.getTypePtr());
			}
		}

		if (is_recognized_owning_container(qtype)) {
			auto template_args_maybe_types = get_template_args_maybe_types(qtype);
			for (auto& template_arg_maybe_type: template_args_maybe_types) {
				if (template_arg_maybe_type.has_value()) {
					auto template_arg_qtype = template_arg_maybe_type.value();
					IF_DEBUG(auto template_arg_qtype_str = template_arg_qtype.getAsString();)

					apply_to_all_owned_types(template_arg_qtype, fn1, maybe_D, {}, depth);
				}
			}
		}

		const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
		if (CXXRD && CXXRD->hasDefinition()) {
			for (auto& CXXBS : CXXRD->bases()) {
				const auto base_qtype = CXXBS.getType();
				IF_DEBUG(const auto base_qtype_str = base_qtype.getAsString();)

				apply_to_all_owned_types(base_qtype, fn1, CXXRD, &CXXBS, depth);
			}
		}

		auto RD = qtype.getTypePtr()->getAsRecordDecl();
		if (RD) {
			auto qname = RD->getQualifiedNameAsString();

			for (const auto FD : RD->fields()) {
				const auto field_qtype = FD->getType();
				IF_DEBUG(const auto field_qtype_str = field_qtype.getAsString();)

				apply_to_all_owned_types(field_qtype, fn1, FD, {}, depth);
			}
		}

		fn1(qtype, maybe_D, maybe_CXXBS);
	}

	inline bool contains_non_owning_scope_reference(const clang::QualType qtype, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline bool contains_non_owning_scope_reference(const clang::Type& type, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		bool retval = false;

		const auto qtype = get_cannonical_type(clang::QualType(&type, 0/*I'm just assuming zero specifies no qualifiers*/));
		IF_DEBUG(std::string qtype_str = qtype.getAsString();)

		auto contains_non_owning_scope_reference_shallow = [&retval, &tu_state_cref, &MR_ptr, &Rewrite_ptr](clang::QualType qtype_param, std::optional<clang::Decl const *> maybe_D = {}, std::optional<clang::CXXBaseSpecifier const *> maybe_CXXBS = {}) {
			if (retval) {
				return;
			}
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype_param);

			const auto qtype = get_cannonical_type(qtype_param);
			IF_DEBUG(std::string qtype_str = qtype.getAsString();)

			if (!(maybe_D.has_value())) {
				auto RD = qtype->getAsRecordDecl();
				if (RD) {
					maybe_D = RD;
				}
			}
			if (maybe_D.has_value()) {
				auto D = maybe_D.value();

				auto SR = Rewrite_ptr ? nice_source_range(D->getSourceRange(), *Rewrite_ptr)
					: D->getSourceRange();

				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
				if (MR_ptr) {
					auto MR = *MR_ptr;
					RETURN_IF_FILTERED_OUT_BY_LOCATION1;
				}

				if (MR_ptr && MR_ptr->Context && Rewrite_ptr) {
					auto suppress_check_flag = tu_state_cref.m_suppress_check_region_set.contains(D, *Rewrite_ptr, *(MR_ptr->Context));
					if (suppress_check_flag) {
						return;
					}
				}

				auto RD = clang::dyn_cast<const clang::RecordDecl>(D);
				if (RD) {
					auto qname = RD->getQualifiedNameAsString();

					DECLARE_CACHED_CONST_STRING(tpfl_str, mse_namespace_str() + "::us::impl::TPointerForLegacy");
					if ((tpfl_str == qname) && (!tu_state_cref.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype))) {
						retval |= true;
						return;
					}
				}
			}

			DECLARE_CACHED_CONST_STRING(ContainsNonOwningScopeReference_tag_str, mse_namespace_str() + "::us::impl::ContainsNonOwningScopeReferenceTagBase");
			if (has_ancestor_base_class(qtype, ContainsNonOwningScopeReference_tag_str)) {
				retval |= true;
				return;
			}
			if ((!tu_state_cref.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(qtype))
				&& ((qtype->isPointerType()) || (qtype->isReferenceType()))) {

				retval |= true;
				return;
			}
		};

		apply_to_all_owned_types(qtype, contains_non_owning_scope_reference_shallow, {});

		return retval;
	}
	inline bool contains_non_owning_scope_reference(const clang::QualType qtype, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = contains_non_owning_scope_reference(*TP, tu_state_cref, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}

	inline bool referenceable_by_scope_pointer(const clang::QualType qtype, const CCommonTUState1& tu_state_cref);
	inline bool referenceable_by_scope_pointer(const clang::Type& type, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		DECLARE_CACHED_CONST_STRING(ReferenceableByScopePointer_tag_str, mse_namespace_str() + "::us::impl::ReferenceableByScopePointerTagBase");
		if (has_ancestor_base_class(type, ReferenceableByScopePointer_tag_str)) {
			return true;
		}
		return retval;
	}
	inline bool referenceable_by_scope_pointer(const clang::QualType qtype, const CCommonTUState1& tu_state_cref) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = referenceable_by_scope_pointer(*TP, tu_state_cref);
		}
		return retval;
	}

	inline bool is_xscope_type(const clang::QualType qtype, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline bool is_xscope_type(const clang::Type& type, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
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
		if (contains_non_owning_scope_reference(type, tu_state_cref, MR_ptr, Rewrite_ptr)
			|| referenceable_by_scope_pointer(type, tu_state_cref)) {
			return true;
		}

		return retval;
	}
	inline bool is_xscope_type(const clang::QualType qtype, const CCommonTUState1& tu_state_cref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		bool retval = false;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		const auto TP = qtype.getTypePtr();
		if (!TP) { assert(false); } else {
			retval = is_xscope_type(*TP, tu_state_cref, MR_ptr, Rewrite_ptr);
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
				if (CXXOCE->getNumArgs() < 1) {
					/* This should never happen, right? */
				} else {
					/* For CXXOperatorCallExpr, they just make the "ImplicitObjectArgument" (if any) the first
					argument. I think. */
					retval = CXXOCE->getArg(0);
				}
				//retval = CXXOCE->getImplicitObjectArgument();
			} else {
				//todo: report error?
			}
		} else {
			auto zb_index = param_ordinal_t::as_a_zero_based_argument_index_if_valid(param_ordinal).value();
			if (CE->getNumArgs() <= zb_index) {
				/* If this happens then either an error should be reported elsewhere
				or there should be a compile error. */
			} else {
				retval = CE->getArg(zb_index);
			}
		}
		return retval;
	};

	auto qtype_from_param_ordinal_if_available(clang::FunctionDecl const * FND, checker::param_ordinal_t param_ordinal) {
		std::optional<clang::QualType> retval;
		if (!FND) {
			return retval;
		}

		if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
			auto CXXMD = dyn_cast<clang::CXXMethodDecl>(FND);

			if (CXXMD) {
				auto this_qtype = CXXMD->getThisType();
				//IF_DEBUG(auto this_qtype_str = this_qtype.getAsString();)
				retval = get_cannonical_type(this_qtype);
			} else {
				if (FND->getNumParams() < 1) {
					/* This should never happen, right? */
				} else {
					/* For CXXOperatorCallExpr, they just make the "ImplicitObjectArgument" (if any) the first
					argument. I think. */
					auto PVD = FND->getParamDecl(0);
					if (PVD) {
						auto qtype = get_cannonical_type(PVD->getType());
						retval = qtype;
					}
				}
			}
		} else {
			auto zb_index = param_ordinal_t::as_a_zero_based_parameter_index_if_valid(param_ordinal).value();

			if (FND->getNumParams() <= zb_index) {
				/* If this happens then either an error should be reported elsewhere
				or there should be a compile error. */
			} else {
				auto PVD = FND->getParamDecl(zb_index);
				if (PVD) {
					auto qtype = get_cannonical_type(PVD->getType());
					retval = qtype;
				}
			}
		}
		return retval;
	};
	template<typename TCallExpr>
	auto qtype_from_param_ordinal_if_available(TCallExpr const * CE, checker::param_ordinal_t param_ordinal) {
		std::optional<clang::QualType> retval;

		auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);
		auto CE2 = dyn_cast<CallExpr>(CE);
		if (CXXCE) {
			auto CXXCD = CXXCE->getConstructor();
			auto FND = cast<clang::FunctionDecl>(CXXCD);
			if (!FND) {
				int q = 3;
			}
			retval = qtype_from_param_ordinal_if_available(FND, param_ordinal);
		} else if (CE2) {
			auto FND = CE2->getDirectCallee();
			retval = qtype_from_param_ordinal_if_available(FND, param_ordinal);
		} else {
			int q = 3;
		}
		return retval;
	};

	template<typename TCallExpr>
	auto name_from_param_ordinal_if_available(TCallExpr const * CE, checker::param_ordinal_t param_ordinal) {
		std::optional<std::string> retval;

		if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
		} else {
			auto zb_index = param_ordinal_t::as_a_zero_based_parameter_index_if_valid(param_ordinal).value();

			auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);
			auto CE2 = dyn_cast<CallExpr>(CE);
			if (CXXCE) {
				auto CXXCD = CXXCE->getConstructor();
				if (CXXCD) {
					if (CXXCD->getNumParams() <= zb_index) {
						/* If this happens then either an error should be reported elsewhere
						or there should be a compile error. */
					} else {
						auto PVD = CXXCD->getParamDecl(zb_index);
						if (PVD) {
							retval = PVD->getNameAsString();
						}
					}
				}
			} else if (CE2) {
				auto FND = CE2->getDirectCallee();
				if (FND) {
					if (FND->getNumParams() <= zb_index) {
						/* If this happens then either an error should be reported elsewhere
						or there should be a compile error. */
					} else {
						auto PVD = FND->getParamDecl(zb_index);
						if (PVD) {
							retval = PVD->getNameAsString();
						}
					}
				}
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
		std::optional<clang::RecordDecl *> maybe_containing_RD;
		if (std::holds_alternative<clang::FunctionDecl const *>(context)) {
			auto FD = std::get<clang::FunctionDecl const *>(context);
			auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(FD);
			if (CXXMD) {
				if ((CXXMD->getThisType().isNull()) || (CXXMD->getThisType()->getPointeeType().isNull())) {
					int q = 3;
					return retval;
				}
				IF_DEBUG(const std::string qtype_str = CXXMD->getThisType()->getPointeeType().getAsString();)
				auto Type_ptr = get_cannonical_type(CXXMD->getThisType()->getPointeeType()).getTypePtr();
				auto containing_RD = Type_ptr->getAsRecordDecl();
				if (containing_RD) {
					maybe_containing_RD = containing_RD;
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
						state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
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
						state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
					}
				}
				std::string sublifetime_label_ids = lifetime_label_ids.substr(lbracket_index + 1, int(rbracket_index) - int(lbracket_index + 1));
				auto sub_alts = parse_lifetime_ids(sublifetime_label_ids, context, attr_SR, state1, MR_ptr, Rewrite_ptr);
				*(abstract_lifetime.m_sublifetimes_vlptr) = sub_alts;
			}

			abstract_lifetime.m_id = primary_lifetime_label_id_str;

			auto check_and_handle_label_referring_to_lifetime_of_parent_type = [&](CAbstractLifetime& alt) {
				if (maybe_containing_type_alts.has_value()) {
					/* This lifetime label is attached to a member function. So we check to see if the same 
					label has been used by a lifetime label that has already been defined for the parent type. 
					If so, then we'll presume that this label refers to that one. */
					auto maybe_existing_alt = maybe_containing_type_alts.value().lifetime_from_label_id_if_present(alt.m_id);
					if (maybe_existing_alt.has_value()) {
						auto sublifetimes_bak = *(alt.m_sublifetimes_vlptr);
						alt = maybe_existing_alt.value();
						*(alt.m_sublifetimes_vlptr) = sublifetimes_bak;
					}
				}
				if (maybe_containing_RD.has_value()) {
					/* This lifetime label is attached to a member function. So we check to see if the same 
					label has been used by a lifetime set alias that has already been defined for the parent
					type. If so, then we'll presume that this label refers to that alias. */
					auto containing_RD = maybe_containing_RD.value();
					auto abstract_lifetime2 = alt;
					abstract_lifetime2.m_context = containing_RD;
					auto found_it = state1.m_lifetime_alias_map.find(abstract_lifetime2);
					if (state1.m_lifetime_alias_map.end() != found_it) {
						auto sublifetimes_bak = *(alt.m_sublifetimes_vlptr);
						alt = abstract_lifetime2;
						*(alt.m_sublifetimes_vlptr) = sublifetimes_bak;
					}
				}
			};

			apply_to_all_lifetimes(abstract_lifetime, check_and_handle_label_referring_to_lifetime_of_parent_type);

			if (false) {
				if (maybe_containing_type_alts.has_value()) {
					/* This lifetime label is attached to a member function. So we check to see if the same 
					label has been used by a lifetime label that has already been defined for the parent type. 
					If so, then we'll presume that this label refers to that one. */
					auto maybe_existing_alt = maybe_containing_type_alts.value().lifetime_from_label_id_if_present(primary_lifetime_label_id_str);
					if (maybe_existing_alt.has_value()) {
						abstract_lifetime = maybe_existing_alt.value();
					}
				}
				if (maybe_containing_RD.has_value()) {
					/* This lifetime label is attached to a member function. So we check to see if the same 
					label has been used by a lifetime set alias that has already been defined for the parent
					type. If so, then we'll presume that this label refers to that alias. */
					auto containing_RD = maybe_containing_RD.value();
					auto abstract_lifetime2 = abstract_lifetime;
					abstract_lifetime2.m_context = containing_RD;
					auto found_it = state1.m_lifetime_alias_map.find(abstract_lifetime2);
					if (state1.m_lifetime_alias_map.end() != found_it) {
						abstract_lifetime = abstract_lifetime2;
					}
				}
			}

			retval.m_primary_lifetimes.push_back(abstract_lifetime);

			one_after_last_comma_index = next_comma_index + 1;
		}

		return retval;
	}

	inline auto type_lifetime_annotations_if_available(const clang::Type * TypePtr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr, bool use_implicit_lifetime_annotation_for_pointer = false)
		-> std::optional<CTypeLifetimeAnnotations const *>;
	inline auto type_lifetime_annotations_if_available(clang::QualType const & qtype, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr, bool use_implicit_lifetime_annotation_for_pointer = false)
		-> std::optional<CTypeLifetimeAnnotations const *>;

	auto populate_lifetime_alias_map(const clang::Type * TypePtr2, const CAbstractLifetimeSet& alts, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};
		if (TypePtr2) {
			if (TypePtr2->isUndeducedType()) {
				int q = 5;
			}
		}
		clang::SourceRange SR;
		auto TD = TypePtr2->getAsTagDecl();
		if (TD) {
			SR = TD->getSourceRange();
		}

		auto template_args_maybe_types = get_template_args_maybe_types(TypePtr2);
		auto num_lt_aliases = alts.m_primary_lifetimes.size();
		size_t count = 0;
		for (size_t i = 0; i < num_lt_aliases; i += 1) {
			auto& alias = alts.m_primary_lifetimes.at(i);
			CAbstractLifetimeSet unaliased_lifetimes;
			if (template_args_maybe_types.size() > i) {
				if (!(template_args_maybe_types.at(i).has_value())) {
					continue;
				}
				auto template_arg_type = template_args_maybe_types.at(i).value();
				if (template_arg_type.isNull()) {
					continue;
				}
				if ("" != alias.m_id) {
					auto maybe_tlta_ptr = type_lifetime_annotations_if_available(template_arg_type, state1, MR_ptr, Rewrite_ptr);
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
					get_template_args_maybe_types() won't return any parameters. In this case we'll just map the
					specified aliases to the empty set. */
					int q = 5;
				}
				auto found_it = state1.m_lifetime_alias_map.find(alias);
				if (state1.m_lifetime_alias_map.end() != found_it) {
					if (found_it->second != unaliased_lifetimes) {
						/* Hopefully this doesn't happen. A mapping for this alias has previously been established and seems 
						to be different from the new mapping. */
						if (MR_ptr) {
							std::string error_desc = std::string("'lifetime set' label '") + alias.m_id + "', specified in 'lifetime_set_aliases_from_template_parameters' annotation,";
							error_desc += " is already being used as a 'lifetime set' label. Please choose a different one.";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}

						if (found_it->second.m_primary_lifetimes.size() < unaliased_lifetimes.m_primary_lifetimes.size()) {
							state1.m_lifetime_alias_map.insert_or_assign(alias, unaliased_lifetimes);
						}
					}
				} else {
					state1.m_lifetime_alias_map.insert_or_assign(alias, unaliased_lifetimes);
				}

				retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), unaliased_lifetimes.m_primary_lifetimes.begin(), unaliased_lifetimes.m_primary_lifetimes.end());

				if (!(alias.m_sublifetimes_vlptr->is_empty())) {
					auto res1 = populate_lifetime_alias_map(template_arg_type.getTypePtr(), *(alias.m_sublifetimes_vlptr), state1, MR_ptr, Rewrite_ptr);

					retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), res1.m_primary_lifetimes.begin(), res1.m_primary_lifetimes.end());
				}
			}
		}
		return retval;
	};

	auto template_arg_type_by_name_if_available(clang::TemplateParameterList const * tparam_list, std::vector<std::optional<clang::QualType> > const & template_args_maybe_types, clang::SourceRange const& SR, std::string_view target_tparam_name_sv, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> std::optional<clang::QualType> {

		auto retval = std::optional<clang::QualType>{};
		if (!tparam_list) {
			return retval;
		}

		//auto tparam_list = CTD2->getTemplateParameters();
		auto tpl_size = tparam_list->size();
		std::optional<size_t> maybe_found_index;
		size_t count = 0;
		for (auto& tparam_ND : *tparam_list) {
			const auto tp_name = tparam_ND->getNameAsString();
			if (tp_name == target_tparam_name_sv) {
				maybe_found_index = count;
				break;
			}
			count += 1;
		}
		if (maybe_found_index.has_value()) {
			auto target_targ_index = maybe_found_index.value();

			{
				//auto template_args_maybe_types = get_template_args_maybe_types(TypePtr2);
				if ((template_args_maybe_types.size() > target_targ_index)
					&& (template_args_maybe_types.at(target_targ_index).has_value())) {

					auto template_arg_type = template_args_maybe_types.at(target_targ_index).value();
					if (!(template_arg_type.isNull())) {
						return template_arg_type;
					} else {
						/* In the case of a template definition (as opposed to a template instantiation),
						get_template_args_maybe_types() won't return any parameters. In this case we'll just map the
						specified aliases to the empty set. */
						int q = 5;
					}
				}
			}
		} else {
			if (MR_ptr) {
				std::string template_parameters_str;
				for (auto& tparam_ND : *tparam_list) {
					const auto tp_name = tparam_ND->getNameAsString();
					template_parameters_str += tp_name;
					template_parameters_str += ", ";
				}
				if (std::string(", ").length() < template_parameters_str.length()) {
					template_parameters_str = template_parameters_str.substr(0, int(template_parameters_str.length()) - 2);
				}

				std::string error_desc = std::string("The specified template parameter name '")
					+ std::string(target_tparam_name_sv) + "' was not recognized.";
				if (256/*arbitrary*/ > template_parameters_str.length()) {
					error_desc += " The recognized template parameters are: " + template_parameters_str + ".";
				}

				state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
			}
		}
		int q = 5;

		return retval;
	}

	auto template_arg_type_by_name_if_available(const clang::Type * TypePtr2, std::string_view target_tparam_name_sv, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> std::optional<clang::QualType> {

		auto retval = std::optional<clang::QualType>{};
		if (!TypePtr2) {
			return retval;
		}
		if (TypePtr2->isUndeducedType()) {
			int q = 5;
		}

		const auto CXXRD = TypePtr2->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				auto CTD2 = CTSD->getSpecializedTemplate();
				if (CTD2) {
					auto tparam_list = CTD2->getTemplateParameters();
					auto template_args_maybe_types = get_template_args_maybe_types(TypePtr2);

					retval = template_arg_type_by_name_if_available(tparam_list, template_args_maybe_types, CTD2->getSourceRange(), target_tparam_name_sv, state1, MR_ptr, Rewrite_ptr);
				}
			}
		}
		return retval;
	}

	auto template_arg_type_by_name_if_available(const clang::FunctionDecl& func_decl, std::string_view target_tparam_name_sv, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> std::optional<clang::QualType> {

		auto retval = std::optional<clang::QualType>{};

		auto FTD = func_decl.getPrimaryTemplate();
		if (!FTD) {
			return retval;
		}
		auto tparam_list = FTD->getTemplateParameters();
		if (tparam_list) {
			const auto template_args_ptr = func_decl.getTemplateSpecializationArgs();
			if (template_args_ptr) {
				const auto& template_args = *template_args_ptr;
				std::vector<std::optional<clang::QualType> > template_args_maybe_types;

				const auto num_args = template_args.size();
				for (int i = 0; i < int(num_args); i += 1) {
					const auto template_arg = template_args[i];
					if ((clang::TemplateArgument::ArgKind::Type != template_arg.getKind()) || template_arg.isNull()) {
						template_args_maybe_types.push_back({});
					} else {
						const auto ta_qtype = template_arg.getAsType();
						IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
						template_args_maybe_types.push_back(ta_qtype);
					}
				}
				retval = template_arg_type_by_name_if_available(tparam_list, template_args_maybe_types, func_decl.getSourceRange(), target_tparam_name_sv, state1, MR_ptr, Rewrite_ptr);
			}
		}
		return retval;
	}

	auto infer_alias_mapping_from_template_arg(clang::TemplateParameterList const * tparam_list, std::vector<std::optional<clang::QualType> > const & template_args_maybe_types, clang::SourceRange const& SR, std::string_view target_tparam_name_sv, const CAbstractLifetime& alias, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};
		if (!tparam_list) {
			return retval;
		}

		//auto tparam_list = CTD2->getTemplateParameters();
		auto tpl_size = tparam_list->size();
		std::optional<size_t> maybe_found_index;
		size_t count = 0;
		for (auto& tparam_ND : *tparam_list) {
			const auto tp_name = tparam_ND->getNameAsString();
			if (tp_name == target_tparam_name_sv) {
				maybe_found_index = count;
				break;
			}
			count += 1;
		}
		if (maybe_found_index.has_value()) {
			auto target_targ_index = maybe_found_index.value();

			if ("" != alias.m_id) {
				//auto template_args_maybe_types = get_template_args_maybe_types(TypePtr2);
				CAbstractLifetimeSet unaliased_lifetimes;
				if ((template_args_maybe_types.size() > target_targ_index)
					&& (template_args_maybe_types.at(target_targ_index).has_value())) {

					auto template_arg_type = template_args_maybe_types.at(target_targ_index).value();
					if (!(template_arg_type.isNull())) {
						auto maybe_tlta_ptr = type_lifetime_annotations_if_available(template_arg_type, state1, MR_ptr, Rewrite_ptr);
						if (maybe_tlta_ptr.has_value()) {
							auto unaliased_prefix1 = "__lifetime_set_" + alias.m_id;
							auto& tlta = *(maybe_tlta_ptr.value());

							size_t count = 0;
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
						get_template_args_maybe_types() won't return any parameters. In this case we'll just map the
						specified aliases to the empty set. */
						int q = 5;
					}
					auto found_it = state1.m_lifetime_alias_map.find(alias);
					if (state1.m_lifetime_alias_map.end() != found_it) {
						if (found_it->second != unaliased_lifetimes) {
							/* Hopefully this doesn't happen. A mapping for this alias has previously been established and seems 
							to be different from the new mapping. */
							if (MR_ptr) {
								std::string error_desc = std::string("'lifetime set' label '") + alias.m_id + "', specified in 'lifetime_set_aliases_from_template_parameters' annotation,";
								error_desc += " is already being used as a 'lifetime set' label. Please choose a different one.";
								state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
							}

							if (found_it->second.m_primary_lifetimes.size() < unaliased_lifetimes.m_primary_lifetimes.size()) {
								state1.m_lifetime_alias_map.insert_or_assign(alias, unaliased_lifetimes);
							}
						}
					} else {
						state1.m_lifetime_alias_map.insert_or_assign(alias, unaliased_lifetimes);
					}

					retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), unaliased_lifetimes.m_primary_lifetimes.begin(), unaliased_lifetimes.m_primary_lifetimes.end());
				}
			}
		} else {
			if (MR_ptr) {
				std::string template_parameters_str;
				for (auto& tparam_ND : *tparam_list) {
					const auto tp_name = tparam_ND->getNameAsString();
					template_parameters_str += "'" + tp_name + "'";
					template_parameters_str += ", ";
				}
				if (std::string(", ").length() < template_parameters_str.length()) {
					template_parameters_str = template_parameters_str.substr(0, int(template_parameters_str.length()) - 2);
				}

				std::string error_desc = std::string("The specified template parameter name '")
					+ std::string(target_tparam_name_sv) + "' was not recognized.";
				if (256/*arbitrary*/ > template_parameters_str.length()) {
					error_desc += " The recognized template parameters are: " + template_parameters_str + ".";
				}

				state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
			}
		}
		int q = 5;

		return retval;
	}

	auto infer_alias_mapping_from_template_arg(const clang::Type * TypePtr2, std::string_view target_tparam_name_sv, const CAbstractLifetime& alias, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};
		if (TypePtr2) {
			if (TypePtr2->isUndeducedType()) {
				int q = 5;
			}
		}

		const auto CXXRD = TypePtr2->getAsCXXRecordDecl();
		if (CXXRD) {
			auto qname = CXXRD->getQualifiedNameAsString();

			auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
			if (CTSD) {
				auto CTD2 = CTSD->getSpecializedTemplate();
				if (CTD2) {
					auto tparam_list = CTD2->getTemplateParameters();
					auto template_args_maybe_types = get_template_args_maybe_types(TypePtr2);

					retval = infer_alias_mapping_from_template_arg(tparam_list, template_args_maybe_types, CTD2->getSourceRange(), target_tparam_name_sv, alias, state1, MR_ptr, Rewrite_ptr);
				}
			}
		}
		return retval;
	}

	auto infer_alias_mapping_from_template_arg(const clang::FunctionDecl& func_decl, std::string_view target_tparam_name_sv, const CAbstractLifetime& alias, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};

		std::vector<std::optional<clang::QualType> > template_args_maybe_types;
		clang::TemplateParameterList * tparam_list_ptr = nullptr;
		auto FTD = func_decl.getPrimaryTemplate();
		if (FTD) {
			tparam_list_ptr = FTD->getTemplateParameters();
			if (tparam_list_ptr) {
				const auto template_args_ptr = func_decl.getTemplateSpecializationArgs();
				if (template_args_ptr) {
					const auto& template_args = *template_args_ptr;

					const auto num_args = template_args.size();
					for (int i = 0; i < int(num_args); i += 1) {
						const auto template_arg = template_args[i];
						if ((clang::TemplateArgument::ArgKind::Type != template_arg.getKind()) || template_arg.isNull()) {
							template_args_maybe_types.push_back({});
						} else {
							const auto ta_qtype = template_arg.getAsType();
							IF_DEBUG(const auto ta_qtype_str = ta_qtype.getAsString();)
							template_args_maybe_types.push_back(ta_qtype);
						}
					}
					/* Here we look for the alias of interest against the set of the function's template arguments, with
					error messages suppressed (for example in the case that the specified template argument is not one of 
					the function's template arguments). */
					auto res1 = infer_alias_mapping_from_template_arg(tparam_list_ptr, template_args_maybe_types, func_decl.getSourceRange(), target_tparam_name_sv, alias, state1);
					if (!(res1.is_empty())) {
						/* The alias of interest seems to have been found. We will reobtain the alias without suppressing any 
						potential error messages this time. */
						retval = infer_alias_mapping_from_template_arg(tparam_list_ptr, template_args_maybe_types, func_decl.getSourceRange(), target_tparam_name_sv, alias, state1, MR_ptr, Rewrite_ptr);
					}
				}
			}
		}
		if (retval.is_empty()) {
			/* We did not successfully find the alias of interest when searching against the set of the 
			function's template arguments (if any). */

			auto CXXCD = dyn_cast<const clang::CXXConstructorDecl>(&func_decl);
			auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(&func_decl);
			if (CXXMD) {
				/* The given function seems to be a member function. So we'll see if we can find the alias of interest
				among the parent type's template arguments (if any) (with error messages suppressed). */
				auto This_qtype = CXXMD->getThisType();
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(This_qtype, retval);
				assert(This_qtype->isPointerType());
				auto This_pointee_qtype = This_qtype->getPointeeType();
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(This_pointee_qtype, retval);
				auto This_pointee_type_ptr = This_pointee_qtype.getTypePtr();
				assert(This_pointee_type_ptr);
				auto res2 = infer_alias_mapping_from_template_arg(This_pointee_type_ptr, target_tparam_name_sv, alias, state1);
				if (!(res2.is_empty())) {
					/* The alias of interest seems to have been found. We will reobtain the alias without suppressing any 
					potential error messages this time. */
					retval = infer_alias_mapping_from_template_arg(This_pointee_type_ptr, target_tparam_name_sv, alias, state1, MR_ptr, Rewrite_ptr);
				} else {
					/* We do not seem to have successfully found the alias of interest. */
					if (1 <= template_args_maybe_types.size()) {
						/* We will run the (presumably futile) search against the function template arguments again, but this 
						time with error messages enabled. */
						retval = infer_alias_mapping_from_template_arg(tparam_list_ptr, template_args_maybe_types, func_decl.getSourceRange(), target_tparam_name_sv, alias, state1, MR_ptr, Rewrite_ptr);
					}
				}
			}
		}
		return retval;
	}

	auto with_any_lifetime_aliases_dealiased(const CAbstractLifetimeSet& alts1, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr)
		-> CAbstractLifetimeSet {

		auto retval = CAbstractLifetimeSet{};

		for (auto lifetime : alts1.m_primary_lifetimes) {
			auto found_it = state1.m_lifetime_alias_map.find(lifetime);
			if (state1.m_lifetime_alias_map.end() != found_it) {
				/* This lifetime is actually an alias (for a set of lifetimes). */
				retval.m_primary_lifetimes.insert(retval.m_primary_lifetimes.end(), found_it->second.m_primary_lifetimes.begin(), found_it->second.m_primary_lifetimes.end());
			} else {
				if (1 <= lifetime.m_sublifetimes_vlptr->m_primary_lifetimes.size()) {
					auto dealiased_sublifetimes = with_any_lifetime_aliases_dealiased(*(lifetime.m_sublifetimes_vlptr), state1, MR_ptr, Rewrite_ptr);
					*(lifetime.m_sublifetimes_vlptr) = dealiased_sublifetimes;
				}
				retval.m_primary_lifetimes.push_back(lifetime);
			}
		}

		return retval;
	}

	void process_type_lifetime_annotations(const clang::TypeDecl& type_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		const auto type_ptr = type_decl.getTypeForDecl();
		if (!type_ptr) {
			return;
		}
		const auto cn_qtype = type_ptr->getCanonicalTypeInternal();
		IF_DEBUG(auto cn_qtype_str = cn_qtype.getAsString();)
		auto iter = state1.m_type_lifetime_annotations_map.find(type_ptr);
		if (state1.m_type_lifetime_annotations_map.end() != iter) {
			if (iter->second.m_parse_errors_noted) {
				/* already processed */
				return;
			}
		}

		IF_DEBUG(auto type_name_str = type_decl.getNameAsString();)
#ifndef NDEBUG
		if (std::string::npos != type_name_str.find("TXScopeSpecializedFirstAndLast")) {
			int q = 5;
		}
		if (std::string::npos != cn_qtype_str.find("TXScopeSpecializedFirstAndLast")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		CTypeLifetimeAnnotations tlta;
		tlta.m_parse_errors_noted = (nullptr != MR_ptr);

		if (type_decl.hasAttrs()) {
			auto with_lifetime_prefix = [](std::string_view sv1) -> std::string {
				auto sv2 = sv1.substr(Parse::find_non_whitespace(sv1));
				return std::string("lifetime_") + std::string(sv2);
			};
			DECLARE_CACHED_CONST_STRING(lifetime_notes_str, "lifetime_notes");
			DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
			DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
			DECLARE_CACHED_CONST_STRING(lifetime_encompasses, "lifetime_encompasses");
			DECLARE_CACHED_CONST_STRING(lifetime_first_can_be_assigned_to_second, "lifetime_first_can_be_assigned_to_second");
			DECLARE_CACHED_CONST_STRING(lifetime_set_alias_from_template_parameter_by_name, "lifetime_set_alias_from_template_parameter_by_name");
			DECLARE_CACHED_CONST_STRING(lifetime_set_alias_from_template_parameter, "lifetime_set_alias_from_template_parameter");
			DECLARE_CACHED_CONST_STRING(lifetime_set_aliases_from_template_parameters_in_order, "lifetime_set_aliases_from_template_parameters_in_order");
			DECLARE_CACHED_CONST_STRING(lifetime_labels_for_base_class_by_type_as_written, "lifetime_labels_for_base_class_by_type_as_written");
			DECLARE_CACHED_CONST_STRING(lifetime_labels_for_base_class, "lifetime_labels_for_base_class");
			DECLARE_CACHED_CONST_STRING(lifetime_label_for_base_class, "lifetime_label_for_base_class");
			DECLARE_CACHED_CONST_STRING(lifetime_labels_for_base_class_by_zb_index, "lifetime_labels_for_base_class_by_zb_index");
			DECLARE_CACHED_CONST_STRING(lifetime_scope_types_prohibited_for_template_parameter_by_name, "lifetime_scope_types_prohibited_for_template_parameter_by_name");

			auto vec = type_decl.getAttrs();
			struct CLTAStatementInfo {
				CLTAStatementInfo(std::string_view text, clang::Attr const * attr_ptr, bool is_a_lifetime_note = false)
					: m_text(text), m_attr_ptr(attr_ptr), m_is_a_lifetime_note(is_a_lifetime_note) {}
				std::string m_text;
				clang::Attr const * m_attr_ptr = nullptr;
				bool m_is_a_lifetime_note = false;
			};
			std::vector<CLTAStatementInfo> lta_statement_infos;
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
				auto first_mse_range = Parse::find_token_sequence({ mse_namespace_str(), "::" }, raw_pretty_str, first_quote_index + 1);
				if (raw_pretty_str.length() <= first_mse_range.begin) {
					continue;
				}
				std::string pretty_str = raw_pretty_str.substr(first_mse_range.end);
				auto index1 = pretty_str.find(lifetime_notes_str);
				if (decltype(pretty_str)::npos != index1) {
					static const std::string lbrace = "{";
					static const std::string rbrace = "}";
					static const std::string lparenthesis = "(";
					static const std::string rparenthesis = ")";
					static const std::string semicolon = ";";
					index1 = pretty_str.find(lbrace, index1 + lifetime_notes_str.length());
					if (decltype(pretty_str)::npos == index1) {
						continue;
					}
					auto last_delimiter_index = index1;
					auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					while (decltype(pretty_str)::npos != next_delimiter_index) {
						std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));
						auto prefixed_str = with_lifetime_prefix(sv1);
						lta_statement_infos.push_back( CLTAStatementInfo{ prefixed_str, attr, true } );

						last_delimiter_index = next_delimiter_index;
						next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					}
				} else {
					lta_statement_infos.push_back( CLTAStatementInfo{ pretty_str, attr } );
				}
			}

			for (const auto& lta_statement_info : lta_statement_infos) {
				std::string_view pretty_str = lta_statement_info.m_text;
				std::string_view sv1 = lta_statement_info.m_text;
				auto attr_SR = lta_statement_info.m_attr_ptr->getRange();

				do {
					auto encompasses_range = Parse::find_token_sequence({ lifetime_encompasses }, sv1);
					if (pretty_str.length() > encompasses_range.begin) {
						std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
						std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, encompasses_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						if ((2 != alts2.m_primary_lifetimes.size()) && (0 != alts2.m_primary_lifetimes.size())) {
							if (MR_ptr) {
								std::string error_desc = std::string("The 'encompasses' lifetime constraint takes two lifetime id arguments. Two valid arguments were not found.");
								error_desc += " \n(A valid example might look something like 'mse::lifetime_encompasses(42, 99)'.)";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else if (0 == alts2.m_primary_lifetimes.size()) {
						} else {
							CAbstractLifetimeSet first_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(0), state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet second_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(1), state1, MR_ptr, Rewrite_ptr);
							if ((1 <= first_alts.m_primary_lifetimes.size()) && (1 <= second_alts.m_primary_lifetimes.size())) {
								if (first_alts.m_primary_lifetimes.size() == second_alts.m_primary_lifetimes.size()) {
									for (size_t i = 0; first_alts.m_primary_lifetimes.size() > i; i += 1) {
										tlta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
											CEncompasses{ first_alts.m_primary_lifetimes.at(i), second_alts.m_primary_lifetimes.at(i) }));
									}
								} else {
									/* todo: report error */;
								}
							}
						}
						break;
					}

					auto first_can_be_assigned_to_second_range = Parse::find_token_sequence({ lifetime_first_can_be_assigned_to_second }, sv1);
					if (pretty_str.length() > first_can_be_assigned_to_second_range.begin) {
						std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
						std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, first_can_be_assigned_to_second_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if ((2 != alts1.m_primary_lifetimes.size()) && (0 != alts1.m_primary_lifetimes.size())) {
							if (MR_ptr) {
								std::string error_desc = std::string("The 'first_can_be_assigned_to_second' lifetime constraint takes two lifetime id arguments. Two valid arguments were not found.");
								error_desc += " \n(A valid example might look something like 'mse::lifetime_first_can_be_assigned_to_second(42, 99)'.)";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else if (0 == alts2.m_primary_lifetimes.size()) {
						} else {
							CAbstractLifetimeSet first_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(0), state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet second_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(1), state1, MR_ptr, Rewrite_ptr);
							if ((1 <= first_alts.m_primary_lifetimes.size()) && (1 <= second_alts.m_primary_lifetimes.size())) {
								auto first_alt = alts1.m_primary_lifetimes.at(0);

								auto second_alt = alts1.m_primary_lifetimes.at(1);

								tlta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CFirstCanBeAssignedToSecond>(
									CFirstCanBeAssignedToSecond{ first_alt, second_alt }));
							}
						}
						break;
					}

					auto lifetime_labels_range = Parse::find_token_sequence({ lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if (!(alts1.is_empty())) {
							tlta.m_lifetime_set = alts1;
						} else {
							if (false && MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						}
						break;
					}

					auto lsftpbn_range = Parse::find_token_sequence({ lifetime_set_alias_from_template_parameter_by_name }, pretty_str);
					if (pretty_str.length() > lsftpbn_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lsftpbn_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						if (2 != alts1.m_primary_lifetimes.size()) {
							if (MR_ptr) {
								std::string error_desc = std::string("'lifetime_set_alias_from_template_parameter_by_name()' annotation requires two arguments,");
								error_desc += " the template parameter name (followed by a comma), followed by a (unique) alias name (of your choosing).";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else {
							auto template_name = std::string(alts1.m_primary_lifetimes.at(0).m_id);
							auto set_alias = alts1.m_primary_lifetimes.at(1);

							clang::Type const * TypePtr = nullptr;
							if (MR_ptr && MR_ptr->Context) {
								auto qtype = MR_ptr->Context->getTypeDeclType(&type_decl);
								if (!qtype.isNull()) {
									TypePtr = qtype.getTypePtr();
								}
							} else {
								TypePtr = type_decl.getTypeForDecl();
							}
							if (TypePtr) {
								auto lifetimes_from_specified_template_params = infer_alias_mapping_from_template_arg(TypePtr, template_name, set_alias, state1, MR_ptr, Rewrite_ptr);
							}
						}
						break;
					}

					auto lsftp_range = Parse::find_token_sequence({ lifetime_set_aliases_from_template_parameters_in_order }, pretty_str);
					if (pretty_str.length() <= lsftp_range.begin) {
						lsftp_range = Parse::find_token_sequence({ lifetime_set_alias_from_template_parameter }, pretty_str);
					}
					if (pretty_str.length() > lsftp_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lsftp_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						if (alts1.is_empty()) {
							int q = 5;
						} else {
							clang::Type const * TypePtr = nullptr;
							if (MR_ptr && MR_ptr->Context) {
								auto qtype = MR_ptr->Context->getTypeDeclType(&type_decl);
								if (!qtype.isNull()) {
									TypePtr = qtype.getTypePtr();
								}
							} else {
								TypePtr = type_decl.getTypeForDecl();
							}
							if (TypePtr) {
								auto lifetimes_from_specified_template_params = populate_lifetime_alias_map(TypePtr, alts1, state1, MR_ptr, Rewrite_ptr);

								if (alts1.is_empty()) {
									if (false && MR_ptr) {
										std::string error_desc = std::string("No valid 'lifetime set aliases' specified in 'lifetime_set_aliases_from_template_parameters' annotation.");
										error_desc += " (A valid use of the 'lifetime_set_aliases_from_template_parameters' annotation might look something like: ";
										error_desc += " 'mse::lifetime_set_aliases_from_template_parameters<51>' or 'mse::lifetime_set_aliases_from_template_parameters<51,52[521,522]>'.)";
										state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
									}
								}
							}
						}
						break;
					}

					auto bcltl_range = Parse::find_token_sequence({ lifetime_labels_for_base_class }, pretty_str);
					if (pretty_str.length() <= bcltl_range.begin) {
						bcltl_range = Parse::find_token_sequence({ lifetime_label_for_base_class }, pretty_str);
					}
					if (pretty_str.length() > bcltl_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, bcltl_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);
						auto& abstract_lifetime_set = alts1;

						if (!(alts1.is_empty())) {
							auto type_decl_Type_ptr = type_decl.getTypeForDecl();
							clang::CXXRecordDecl const * CXXRD = type_decl_Type_ptr ? type_decl_Type_ptr->getAsCXXRecordDecl() : (decltype(CXXRD))(nullptr);
							if (CXXRD) {
								if (1 <= CXXRD->getNumBases()) {
									for (auto& CXXBS : CXXRD->bases()) {
										state1.m_base_class_to_abstract_lifetime_map.insert_or_assign(&CXXBS, abstract_lifetime_set);
										break;
									}
								} else {
									if (MR_ptr) {
										std::string error_desc = std::string("A base class lifetime label was seemingly specified without a corresponding base class.");
										state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
									}
								}
							} else {
								if (MR_ptr) {
									std::string error_desc = std::string("A base class lifetime label was seemingly specified for a type with no base classes.");
									state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
								}
							}
						} else {
							if (false && MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						}
						break;
					}

					auto lstpftpbn_range = Parse::find_token_sequence({ lifetime_scope_types_prohibited_for_template_parameter_by_name }, pretty_str);
					if (pretty_str.length() > lstpftpbn_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lstpftpbn_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						for (auto& lifetime : alts1.m_primary_lifetimes) {
							auto template_name = std::string(lifetime.m_id);

							clang::Type const * TypePtr = nullptr;
							if (MR_ptr && MR_ptr->Context) {
								auto qtype = MR_ptr->Context->getTypeDeclType(&type_decl);
								if (!qtype.isNull()) {
									TypePtr = qtype.getTypePtr();
								}
							} else {
								TypePtr = type_decl.getTypeForDecl();
							}
							if (TypePtr) {
								auto maybe_template_arg_type = template_arg_type_by_name_if_available(TypePtr, template_name, state1, MR_ptr, Rewrite_ptr);
								if (maybe_template_arg_type.has_value()) {
									auto template_arg_type = maybe_template_arg_type.value();
									bool res1 = is_xscope_type(remove_reference(template_arg_type), state1, MR_ptr, Rewrite_ptr);
									if (res1) {
										if (MR_ptr) {
											std::string error_desc = std::string("Template parameter '") + template_name + "' instantiated with scope type '";
											error_desc += template_arg_type.getAsString() + "' prohibited by a 'scope_types_prohibited_for_template_parameter_by_name()' lifetime constraint.";
											state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
										}
									}
								} else {
									int q = 5;
								}
							}
						}
						break;
					}

				} while (false);
			}
		} else if (type_ptr->isReferenceType()) {
			/* Adding lifetime annotations to a (non-const) raw pointer can change whether a (re-)assignment
			operation is valid or not. But since (raw) references don't support (re-)assignment, there's no 
			risk of (unintentionally) invalidating any assignment operation by adding a lifetime annotation. 
			So, we're going to give reference types an implicit lifetime label. */
			CAbstractLifetime alt1{ "__implicit raw reference lifetime__", &type_decl };
			auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(type_ptr->getPointeeType(), state1, MR_ptr, Rewrite_ptr);
			if (maybe_pointee_tlta_ptr.has_value()) {
				auto const & pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
				if (!(pointee_tlta_ptr->m_lifetime_set.is_empty())) {
					/* The target of this raw pointer or reference has lifetime annotations */
					*(alt1.m_sublifetimes_vlptr) = pointee_tlta_ptr->m_lifetime_set;
				}
			}
			tlta.m_lifetime_set = { alt1 };
		}

		state1.insert_or_assign_type_lifetime_annotations(type_ptr, tlta);

		clang::RecordDecl const * RD = type_ptr ? type_ptr->getAsRecordDecl() : (decltype(RD))(nullptr);
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
							static const std::string lparenthesis = "(";
							static const std::string rparenthesis = ")";

							auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_labels_range.end);
							if (pretty_str.length() <= lparenthesis_range.begin) {
								continue;
							}
							auto lparenthesis_index = lparenthesis_range.begin;
							auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
							if (pretty_str.length() <= rparenthesis_index) {
								int q = 3;
								continue;
							}
							std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

							CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

							if (!(alts1.is_empty())) {
								abstract_lifetime_set = alts1;
							} else {
								if (false && MR_ptr) {
									std::string error_desc = std::string("No valid 'lifetime label id' specified.");
									state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
								}
							}
						} else {
							auto lifetime_set_range = Parse::find_token_sequence({ mse_namespace_str(), "::", lifetime_set }, pretty_str);
							if (pretty_str.length() > lifetime_set_range.begin) {
								static const std::string lparenthesis = "(";
								static const std::string rparenthesis = ")";

								auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_set_range.end);
								if (pretty_str.length() <= lparenthesis_range.begin) {
									continue;
								}
								auto lparenthesis_index = lparenthesis_range.begin;
								auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
								if (pretty_str.length() <= rparenthesis_index) {
									int q = 3;
									continue;
								}
								std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

								CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &type_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
								CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

								if (!(alts1.is_empty())) {
									abstract_lifetime_set = alts1;
								} else {
									if (false && MR_ptr) {
										std::string error_desc = std::string("No valid 'lifetime label id' specified.");
										state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
									}
								}
							}
						}

					}
					state1.m_fielddecl_to_abstract_lifetime_map.insert_or_assign(FD, abstract_lifetime_set);

					if (FD->hasInClassInitializer()) {
						if (MR_ptr) {
							std::string error_desc = std::string("Currently, member fields with lifetime annotation aren't permitted to have default values (with field of type ") + get_as_quoted_string_for_errmsg(FD->getType()) + ").";
							state1.register_error(*(MR_ptr->SourceManager), FD->getSourceRange(), error_desc);
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
	void process_type_lifetime_annotations(const clang::Type * TypePtr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		if (TypePtr) {
			auto TD = TypePtr->getAsRecordDecl();
			if (TD) {
				IF_DEBUG(auto type_str = TD->getNameAsString();)
				process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
			} else if (TypePtr->isReferenceType()) {
				/* Adding lifetime annotations to a (non-const) raw pointer can change whether a (re-)assignment
				operation is valid or not. But since (raw) references don't support (re-)assignment, there's no 
				risk of (unintentionally) invalidating any assignment operation by adding a lifetime annotation. 
				So, we're going to give reference types an implicit lifetime label. */
				CTypeLifetimeAnnotations tlta;
				CAbstractLifetime alt1{ "__implicit raw reference lifetime__", (clang::Decl const *)(nullptr) };
				auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(TypePtr->getPointeeType(), state1, MR_ptr, Rewrite_ptr);
				if (maybe_pointee_tlta_ptr.has_value()) {
					auto const & pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
					if (!(pointee_tlta_ptr->m_lifetime_set.is_empty())) {
						/* The target of this raw pointer or reference has lifetime annotations */
						*(alt1.m_sublifetimes_vlptr) = pointee_tlta_ptr->m_lifetime_set;
					}
				}
				tlta.m_lifetime_set = { alt1 };

				state1.insert_or_assign_type_lifetime_annotations(TypePtr, tlta);
			}
		}
	}
	void process_type_lifetime_annotations(clang::QualType const & qtype, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		if (qtype.isNull()) {
			return;
		}
		auto TypePtr = qtype.getTypePtr();
		if (is_raw_pointer_or_equivalent(qtype) && qtype.isConstQualified()) {
			/* Adding lifetime annotations to a (non-const) raw pointer can change whether a (re-)assignment
			operation is valid or not. But since const pointers (not to be confused with pointer-to-const) 
			don't support (re-)assignment, there's no risk of (unintentionally) invalidating any assignment 
			operation by adding a lifetime annotation. So, we're going to give const pointer types an implicit 
			lifetime label. */
			CTypeLifetimeAnnotations tlta;
			auto const label_id_str = std::string("_implicit const pointer lifetime for ") + qtype.getAsString() + "_";
			CAbstractLifetime alt1{ label_id_str, (clang::Decl const *)(nullptr) };
			auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(TypePtr->getPointeeType(), state1, MR_ptr, Rewrite_ptr);
			if (maybe_pointee_tlta_ptr.has_value()) {
				auto const & pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
				if (!(pointee_tlta_ptr->m_lifetime_set.is_empty())) {
					/* The target of this raw pointer or reference has lifetime annotations */
					*(alt1.m_sublifetimes_vlptr) = pointee_tlta_ptr->m_lifetime_set;
				}
			}
			tlta.m_lifetime_set = { alt1 };

			state1.insert_or_assign_const_qualified_type_lifetime_annotations(qtype, tlta);
			return;
		}
		process_type_lifetime_annotations(TypePtr, state1, MR_ptr, Rewrite_ptr);
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

		IF_DEBUG(const auto FND = &func_decl;)
		IF_DEBUG(const auto debug_func_name = func_decl.getNameAsString();)
		IF_DEBUG(const auto debug_func_qname = func_decl.getQualifiedNameAsString();)
#ifndef NDEBUG
		if (std::string::npos != debug_func_qname.find("operator!=")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		std::optional<CAbstractLifetimeSet> maybe_containing_type_alts;
		auto CXXCD = dyn_cast<const clang::CXXConstructorDecl>(&func_decl);
		auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(&func_decl);
		if (CXXMD) {
			auto This_qtype = CXXMD->getThisType();
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(This_qtype);
			auto This_pointee_qtype = get_cannonical_type(This_qtype->getPointeeType());
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(This_pointee_qtype);
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
		bool no_elided_flag = false;

		std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> param_lifetime_map;
		DECLARE_CACHED_CONST_STRING(lifetime_notes_str, "lifetime_notes");
		DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
		DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
		DECLARE_CACHED_CONST_STRING(clang_lifetimebound_str, "clang::lifetimebound");

		if (func_decl.hasAttrs()) {
			auto with_lifetime_prefix = [](std::string_view sv1) -> std::string {
				auto sv2 = sv1.substr(Parse::find_non_whitespace(sv1));
				return std::string("lifetime_") + std::string(sv2);
			};
			DECLARE_CACHED_CONST_STRING(lifetime_set_alias_from_template_parameter_by_name, "lifetime_set_alias_from_template_parameter_by_name");
			DECLARE_CACHED_CONST_STRING(lifetime_set_alias_from_template_parameter, "lifetime_set_alias_from_template_parameter");
			DECLARE_CACHED_CONST_STRING(lifetime_set_aliases_from_template_parameters_in_order, "lifetime_set_aliases_from_template_parameters_in_order");
			DECLARE_CACHED_CONST_STRING(lifetime_scope_types_prohibited_for_template_parameter_by_name, "lifetime_scope_types_prohibited_for_template_parameter_by_name");
			DECLARE_CACHED_CONST_STRING(lifetime_return_value, "lifetime_return_value");
			DECLARE_CACHED_CONST_STRING(lifetime_this, "lifetime_this");
			DECLARE_CACHED_CONST_STRING(lifetime_encompasses, "lifetime_encompasses");
			DECLARE_CACHED_CONST_STRING(lifetime_first_can_be_assigned_to_second, "lifetime_first_can_be_assigned_to_second");
			DECLARE_CACHED_CONST_STRING(lifetime_no_elided, "lifetime_no_elided");

			auto vec = func_decl.getAttrs();
			struct CLTAStatementInfo {
				CLTAStatementInfo(std::string_view text, clang::Attr const * attr_ptr, bool is_a_lifetime_note = false)
					: m_text(text), m_attr_ptr(attr_ptr), m_is_a_lifetime_note(is_a_lifetime_note) {}
				std::string m_text;
				clang::Attr const * m_attr_ptr = nullptr;
				bool m_is_a_lifetime_note = false;
			};
			std::vector<CLTAStatementInfo> lta_statement_infos;
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
				auto first_mse_range = Parse::find_token_sequence({ mse_namespace_str(), "::" }, raw_pretty_str, first_quote_index + 1);
				if (raw_pretty_str.length() <= first_mse_range.begin) {
					continue;
				}
				std::string pretty_str = raw_pretty_str.substr(first_mse_range.end);
				auto index1 = pretty_str.find(lifetime_notes_str);
				if (decltype(pretty_str)::npos != index1) {
					static const std::string lbrace = "{";
					static const std::string rbrace = "}";
					static const std::string lparenthesis = "(";
					static const std::string rparenthesis = ")";
					static const std::string semicolon = ";";
					index1 = pretty_str.find(lbrace, index1 + lifetime_notes_str.length());
					if (decltype(pretty_str)::npos == index1) {
						continue;
					}
					auto last_delimiter_index = index1;
					auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					while (decltype(pretty_str)::npos != next_delimiter_index) {
						std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));
						auto prefixed_str = with_lifetime_prefix(sv1);
						lta_statement_infos.push_back( CLTAStatementInfo{ prefixed_str, attr, true } );

						last_delimiter_index = next_delimiter_index;
						next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
					}
				} else {
					lta_statement_infos.push_back( CLTAStatementInfo{ pretty_str, attr } );
				}
			}

			for (const auto& lta_statement_info : lta_statement_infos) {
				std::string_view pretty_str = lta_statement_info.m_text;
				std::string_view sv1 = lta_statement_info.m_text;
				auto attr_SR = lta_statement_info.m_attr_ptr->getRange();

				do {
					auto index2 = sv1.find(lifetime_return_value);
					if (decltype(sv1)::npos != index2) {
						CAbstractLifetimeSet return_value_lifetimes;

						{
							const auto lparenthesis_index = sv1.find('(');
							if (std::string::npos != lparenthesis_index) {
								const auto rparenthesis_index = sv1.find(')', lparenthesis_index+1);
								if ((std::string::npos != rparenthesis_index) && (lparenthesis_index + 1 < rparenthesis_index)) {
									auto sv2 = sv1.substr(lparenthesis_index + 1, int(rparenthesis_index) - (int(lparenthesis_index) + 1));
									CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
									return_value_lifetimes = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);
								}
							}
						}
						if (!(return_value_lifetimes.is_empty())) {
							flta.m_return_value_lifetimes = return_value_lifetimes;
						}

						break;
					}

					index2 = sv1.find(lifetime_this);
					if (decltype(sv1)::npos != index2) {
						CAbstractLifetimeSet this_lifetimes;

						{
							const auto lparenthesis_index = sv1.find('(');
							if (std::string::npos != lparenthesis_index) {
								const auto rparenthesis_index = sv1.find(')', lparenthesis_index+1);
								if ((std::string::npos != rparenthesis_index) && (lparenthesis_index + 1 < rparenthesis_index)) {
									auto sv2 = sv1.substr(lparenthesis_index + 1, int(rparenthesis_index) - (int(lparenthesis_index) + 1));
									CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv2, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
									this_lifetimes = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);
								}
							}
						}
						if (!(this_lifetimes.is_empty())) {
							param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, this_lifetimes);
						}

						break;
					}

					auto encompasses_range = Parse::find_token_sequence({ lifetime_encompasses }, sv1);
					if (pretty_str.length() > encompasses_range.begin) {
						std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
						std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, encompasses_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						if ((2 != alts2.m_primary_lifetimes.size()) && (0 != alts2.m_primary_lifetimes.size())) {
							if (MR_ptr) {
								std::string error_desc = std::string("The 'encompasses' lifetime constraint takes two lifetime id arguments. Two valid arguments were not found.");
								error_desc += " \n(A valid example might look something like 'mse::lifetime_encompasses(42, 99)'.)";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else if (0 == alts2.m_primary_lifetimes.size()) {
						} else {
							CAbstractLifetimeSet first_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(0), state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet second_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(1), state1, MR_ptr, Rewrite_ptr);
							if ((1 <= first_alts.m_primary_lifetimes.size()) && (1 <= second_alts.m_primary_lifetimes.size())) {
								if (first_alts.m_primary_lifetimes.size() == second_alts.m_primary_lifetimes.size()) {
									for (size_t i = 0; first_alts.m_primary_lifetimes.size() > i; i += 1) {
										flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
											CEncompasses{ first_alts.m_primary_lifetimes.at(i), second_alts.m_primary_lifetimes.at(i) }));
									}
								} else {
									/* todo: report error */;
								}
							}
						}
						break;
					}

					auto first_can_be_assigned_to_second_range = Parse::find_token_sequence({ lifetime_first_can_be_assigned_to_second }, sv1);
					if (pretty_str.length() > first_can_be_assigned_to_second_range.begin) {
						std::optional<lifetime_id_t> maybe_first_lifetime_label_id;
						std::optional<lifetime_id_t> maybe_second_lifetime_label_id;

						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, first_can_be_assigned_to_second_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if ((2 != alts1.m_primary_lifetimes.size()) && (0 != alts1.m_primary_lifetimes.size())) {
							if (MR_ptr) {
								std::string error_desc = std::string("The 'first_can_be_assigned_to_second' lifetime constraint takes two lifetime id arguments. Two valid arguments were not found.");
								error_desc += " \n(A valid example might look something like 'mse::lifetime_first_can_be_assigned_to_second(42, 99)'.)";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else if (0 == alts2.m_primary_lifetimes.size()) {
						} else {
							CAbstractLifetimeSet first_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(0), state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet second_alts = with_any_lifetime_aliases_dealiased(alts2.m_primary_lifetimes.at(1), state1, MR_ptr, Rewrite_ptr);
							if ((1 <= first_alts.m_primary_lifetimes.size()) && (1 <= second_alts.m_primary_lifetimes.size())) {
								auto first_alt = alts1.m_primary_lifetimes.at(0);

								auto second_alt = alts1.m_primary_lifetimes.at(1);

								flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CFirstCanBeAssignedToSecond>(
									CFirstCanBeAssignedToSecond{ first_alt, second_alt }));
							}
						}
						break;
					}

					auto lifetime_labels_range = Parse::find_token_sequence({ lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if (!(alts1.is_empty())) {
							flta.m_lifetime_set = alts1;
						} else {
							if (false && MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						}
						break;
					}

					auto lsftpbn_range = Parse::find_token_sequence({ lifetime_set_alias_from_template_parameter_by_name }, pretty_str);
					if (pretty_str.length() > lsftpbn_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lsftpbn_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						if (2 != alts1.m_primary_lifetimes.size()) {
							if (MR_ptr) {
								std::string error_desc = std::string("'lifetime_set_alias_from_template_parameter_by_name()' annotation requires two arguments,");
								error_desc += " the template parameter name (followed by a comma), followed by a (unique) alias name (of your choosing).";
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						} else {
							auto template_name = std::string(alts1.m_primary_lifetimes.at(0).m_id);
							auto set_alias = alts1.m_primary_lifetimes.at(1);

							auto lifetimes_from_specified_template_params = infer_alias_mapping_from_template_arg(func_decl, template_name, set_alias, state1, MR_ptr, Rewrite_ptr);
						}
						break;
					}

					auto lstpftpbn_range = Parse::find_token_sequence({ lifetime_scope_types_prohibited_for_template_parameter_by_name }, pretty_str);
					if (pretty_str.length() > lstpftpbn_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lstpftpbn_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts1 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);

						for (auto& lifetime : alts1.m_primary_lifetimes) {
							auto template_name = std::string(lifetime.m_id);

							{
								auto maybe_template_arg_type = template_arg_type_by_name_if_available(func_decl, template_name, state1, MR_ptr, Rewrite_ptr);
								if (maybe_template_arg_type.has_value()) {
									auto template_arg_type = maybe_template_arg_type.value();
									bool res1 = is_xscope_type(remove_reference(template_arg_type), state1, MR_ptr, Rewrite_ptr);
									if (res1) {
										if (MR_ptr) {
											std::string error_desc = std::string("Template parameter '") + template_name + "' instantiated with scope type '";
											error_desc += template_arg_type.getAsString() + "' prohibited by a 'scope_types_prohibited_for_template_parameter_by_name()' lifetime constraint.";
											state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
										}
									}
								} else {
									int q = 5;
								}
							}
						}
						break;
					}

					auto lni_range = Parse::find_token_sequence({ lifetime_no_elided }, pretty_str);
					if (pretty_str.length() > lni_range.begin) {
						no_elided_flag = true;
					}

					index2 = pretty_str.find(clang_lifetimebound_str);
					if (false && (decltype(pretty_str)::npos != index2)) {
						/* A [[clang::lifetimebound]] attribute on the function will be interpreted as an
						MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<1>; this<1> }") attribute. */
						flta.m_return_value_lifetimes = CAbstractLifetime{ lifetime_id_t("1"), &func_decl };
						param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, CAbstractLifetime{ lifetime_id_t("1"), &func_decl });
						break;
					}

				} while (false);
			}
		}

		/* For some reason the clang AST represents member operator call expressions as non-member
		call expressions with the implicit `this` parameter as the first parameter. So will indicate
		whether this is a member operator in the param_ordinal_t's constructor call. */
		bool is_ns_member_operator = false;
		if (CXXMD && CXXMD->isOverloadedOperator() && (!(CXXMD->isStatic()))) {
			is_ns_member_operator = true;
		}
		auto param_ordinal = is_ns_member_operator ? param_ordinal_t(param_ordinal_t::ns_member_operator_tag{}, 1)
			: param_ordinal_t(1);

		for (auto param_iter = func_decl.param_begin(); func_decl.param_end() != param_iter; ++param_iter, param_ordinal += 1) {
			auto param = (*param_iter);
			auto PVD = (*param_iter);
			if (!PVD) {
				assert(false); continue;
			}
			auto PVD_qtype = PVD->getType();
			IF_DEBUG(const std::string PVD_qtype_str = PVD->getType().getAsString();)
			if (PVD_qtype.isNull()) {
				continue;
			}

			if (param->hasAttrs()) {
				auto with_lifetime_prefix = [](std::string_view sv1) -> std::string {
					auto sv2 = sv1.substr(Parse::find_non_whitespace(sv1));
					return std::string("lifetime_") + std::string(sv2);
				};
				DECLARE_CACHED_CONST_STRING(lifetime_set, "lifetime_set");

				auto vec = param->getAttrs();
				struct CLTAStatementInfo {
					CLTAStatementInfo(std::string_view text, clang::Attr const * attr_ptr, bool is_a_lifetime_note = false)
						: m_text(text), m_attr_ptr(attr_ptr), m_is_a_lifetime_note(is_a_lifetime_note) {}
					std::string m_text;
					clang::Attr const * m_attr_ptr = nullptr;
					bool m_is_a_lifetime_note = false;
				};
				std::vector<CLTAStatementInfo> lta_statement_infos;
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
					auto first_mse_range = Parse::find_token_sequence({ mse_namespace_str(), "::" }, raw_pretty_str, first_quote_index + 1);
					if (raw_pretty_str.length() <= first_mse_range.begin) {
						continue;
					}
					std::string pretty_str = raw_pretty_str.substr(first_mse_range.end);
					auto index1 = pretty_str.find(lifetime_notes_str);
					if (decltype(pretty_str)::npos != index1) {
						static const std::string lbrace = "{";
						static const std::string rbrace = "}";
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";
						static const std::string semicolon = ";";
						index1 = pretty_str.find(lbrace, index1 + lifetime_notes_str.length());
						if (decltype(pretty_str)::npos == index1) {
							continue;
						}
						auto last_delimiter_index = index1;
						auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
						while (decltype(pretty_str)::npos != next_delimiter_index) {
							std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));
							auto prefixed_str = with_lifetime_prefix(sv1);
							lta_statement_infos.push_back( CLTAStatementInfo{ prefixed_str, attr, true } );

							last_delimiter_index = next_delimiter_index;
							next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
						}
					} else {
						lta_statement_infos.push_back( CLTAStatementInfo{ pretty_str, attr } );
					}
				}

				for (const auto& lta_statement_info : lta_statement_infos) {
					std::string_view pretty_str = lta_statement_info.m_text;
					std::string_view sv1 = lta_statement_info.m_text;
					auto attr_SR = lta_statement_info.m_attr_ptr->getRange();

					auto lifetime_labels_range = Parse::find_token_sequence({ lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if (!(alts1.is_empty())) {
							size_t num_declared_primary_lifetimes = 0;

							auto found_iter2 = state1.m_type_lifetime_annotations_map.find(get_cannonical_type(param->getType()).getTypePtr());
							if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
								auto const & tlta = found_iter2->second;
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
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
							}
						}
					} else {
						auto lifetime_set_range = Parse::find_token_sequence({ lifetime_set }, pretty_str);
						if (pretty_str.length() > lifetime_set_range.begin) {
							static const std::string lparenthesis = "(";
							static const std::string rparenthesis = ")";

							auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_set_range.end);
							if (pretty_str.length() <= lparenthesis_range.begin) {
								continue;
							}
							auto lparenthesis_index = lparenthesis_range.begin;
							auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
							if (pretty_str.length() <= rparenthesis_index) {
								int q = 3;
								continue;
							}
							std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

							CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
							CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

							if (!(alts1.is_empty())) {
								size_t num_declared_primary_lifetimes = 0;

								auto found_iter2 = state1.m_type_lifetime_annotations_map.find(get_cannonical_type(param->getType()).getTypePtr());
								if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
									auto const & tlta = found_iter2->second;
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
									state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
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
							MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
							std::string element_name;
							const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
							if (l_CXXRD) {
								element_name = l_CXXRD->getQualifiedNameAsString();
							} else {
								element_name = qtype.getAsString();
							}

							if ((mse_rsv_ltn_parameter_lifetime_labels_str == element_name)) {
								auto SR = Rewrite_ptr ? nice_source_range(param->getSourceRange(), *Rewrite_ptr)
									: param->getSourceRange();

								std::unordered_map<param_ordinal_t, CAbstractLifetimeSet> param_lifetime_map;

								auto process_parameter_lifetime = [&mse_rsv_ltn_pll_str, &param_lifetime_map, &func_decl, &MR_ptr](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
									auto qtype = typeLoc.getType();
									MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
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
														auto param_ordinal = size_t(atoi(param_ordinal_str.c_str()));
														if (int(func_decl.getNumParams()) < int(param_ordinal)) {
															if (MR_ptr) {
																std::string error_desc = std::string("The specified parameter ordinal ('") + std::to_string(param_ordinal)
																	+ "') is greater than the number of parameters (" + std::to_string(func_decl.getNumParams()) + ").";
																state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
															}
														} else if ((1 <= param_ordinal) || (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal_t(param_ordinal))) {
															maybe_param_ordinal = param_ordinal_t(param_ordinal);
														} else {
															if (MR_ptr) {
																std::string error_desc = std::string("'") + param_ordinal_str + "' is not a valid parameter ordinal value.";
																state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
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
												MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
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
											apply_to_template_arg_types_if_any(typeLoc, process_parameter_lifetime_component, state1);
										}

										bool is_valid = true;
										if ((!maybe_param_ordinal.has_value()) || ((1 > maybe_param_ordinal.value()) && (IMPLICIT_THIS_PARAM_ORDINAL != maybe_param_ordinal.value()))) {
											is_valid = false;
											if (MR_ptr) {
												std::string error_desc = std::string("The first template argument of the 'mse::rsv::pll<parameter_ordinal_t param_ordinal, lifetime_label_t lifetime_label_id>' ")
													+ "template must be an integer greater than zero or MSE_IMPLICIT_THIS_PARAM_ORDINAL.";
												state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
											}
										}
										if ((!maybe_lifetime_label_id.has_value()) || (1 > maybe_lifetime_label_id.value().length())) {
											is_valid = false;
											if (MR_ptr) {
												std::string error_desc = std::string("The second template argument of the 'mse::rsv::pll<size_t param_ordinal, std::string lifetime_label_id>' ")
													+ "template must be a valid non-empty string.";
												state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
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
									apply_to_template_arg_types_if_any(tsi_ptr->getTypeLoc(), process_parameter_lifetime, state1);
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
														state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
													}
													break;
												}
												std::string lifetime_label_id_str = qtype_str.substr(last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index) - 1);
												auto lifetime_label_id = with_whitespace_removed(lifetime_label_id_str);

												if (1 <= lifetime_label_id.length()) {
													maybe_return_value_lifetime = new_or_existing_lifetime_from_label_id(lifetime_id_t(lifetime_label_id), &func_decl, maybe_containing_type_alts);
												} else {
													if (false && MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
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
													if (false && MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
													}
												}

												std::string second_lifetime_label_id_str = qtype_str.substr(comma_index + 1, int(rangle_bracket_index) - int(comma_index) - 1);
												auto second_lifetime_label_id = with_whitespace_removed(second_lifetime_label_id_str);
												maybe_second_lifetime_label_id = lifetime_id_t(second_lifetime_label_id);
												if (1 <= second_lifetime_label_id.length()) {
												} else {
													if (false && MR_ptr) {
														std::string error_desc = std::string("No valid 'lifetime label id' specified.");
														state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
													}
												}
											}
										}
									}
								} else {
									size_t component_index = 0;
									auto process_parameter_lifetime_component = [&maybe_first_lifetime_label_id, &maybe_second_lifetime_label_id, &component_index, &MR_ptr](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
										auto qtype = typeLoc.getType();
										MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
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
										apply_to_template_arg_types_if_any(tsi_ptr->getTypeLoc(), process_parameter_lifetime_component, state1);
									}
								}

								if (maybe_first_lifetime_label_id.has_value() && maybe_second_lifetime_label_id.has_value()) {
									flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
										CEncompasses{ CAbstractLifetime{ maybe_first_lifetime_label_id.value(), &func_decl }
											, CAbstractLifetime{ maybe_second_lifetime_label_id.value(), &func_decl } }));
								} else {
									if (MR_ptr) {
										std::string error_desc = std::string("Parse error in 'encompassing' lifetime constraint specification.");
										state1.register_error(*(MR_ptr->SourceManager), typeLoc.getSourceRange(), error_desc);
									}
								}

								int q = 5;
							}
							int q = 5;
						};
						auto tsi_ptr = param->getTypeSourceInfo();
						if (tsi_ptr) {
							//process_parameter_lifetime(tsi_ptr->getTypeLoc(), SR, state1);
							apply_to_template_arg_types_if_any(tsi_ptr->getTypeLoc(), process_lifetime_note, state1);
						}
					}
				}
			}
		}

		if (1 <= param_lifetime_map.size()) {
			flta.m_param_lifetime_map = param_lifetime_map;
		}

		for (const auto& param_lifetime : flta.m_param_lifetime_map) {
			auto param_iter = func_decl.param_begin();
			auto param_ordinal = param_lifetime.first;
			auto maybe_param_zb_index = param_ordinal.as_a_zero_based_parameter_index_if_valid();
			if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
				int q = 5;
			} else if (maybe_param_zb_index.has_value()) {
				auto param_zb_index = maybe_param_zb_index.value();
				if (func_decl.param_size() <= param_zb_index) {
					assert(false);
					continue;
				} else if (0 <= param_zb_index) {
					param_iter += param_zb_index;
					auto PVD = *param_iter;
					CVariableLifetimeAnnotations vlta;
					vlta.m_lifetime_set = param_lifetime.second;
					vlta.m_parse_errors_noted = flta.m_parse_errors_noted;
					state1.m_vardecl_lifetime_annotations_map.insert_or_assign(PVD, vlta);

					if (PVD->hasInit()) {
						if (MR_ptr) {
							std::string error_desc = std::string("Currently, parameters with lifetime annotation aren't permitted to have default values (with parameter of type ") + get_as_quoted_string_for_errmsg(PVD->getType()) + ").";
							state1.register_error(*(MR_ptr->SourceManager), PVD->getSourceRange(), error_desc);
						}
					}
				}
			}
		}

		struct CB {
			void build_implied_ancestor_descendant_constraints(CAbstractLifetime const& abstract_lifetime, std::vector<CAbstractLifetime> const & ancestor_lifetimes) {
				for (auto const& ancestor_lifetime : ancestor_lifetimes) {
					if (!(ancestor_lifetime == abstract_lifetime)) {
						m_lifetime_constraint_shptrs.push_back(std::make_shared<CEncompasses>(
							CEncompasses{ abstract_lifetime, ancestor_lifetime }));
					}
				}
				auto new_ancestor_lifetimes = ancestor_lifetimes;
				new_ancestor_lifetimes.push_back(abstract_lifetime);
				for (auto const& sub_abstract_lifetime : abstract_lifetime.m_sublifetimes_vlptr->m_primary_lifetimes) {
					build_implied_ancestor_descendant_constraints(sub_abstract_lifetime, new_ancestor_lifetimes);
				}
			}

			std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > m_lifetime_constraint_shptrs;
		};

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

		if (flta.m_return_value_lifetimes.is_empty() && (0 == flta.m_param_lifetime_map.size())) {
			/* This function doesn't seem to have lifetime annotations. Here we'll check to see if it is a recognized
			"legacy" function and if so add some implicit lifetime annotations as appropriate. */

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
			static const std::string operator_star_str = "operator*";
			static const std::string operator_arrow_str = "operator->";
			static const std::string operator_subscript_str = "operator[]";

			static const std::string method_value_str = "value";
			static const std::string method_at_str = "at";
			static const std::string method_front_str = "front";
			static const std::string method_back_str = "back";

			auto func_qname_str = func_decl.getQualifiedNameAsString();
			auto func_name_str = func_decl.getNameAsString();
			std::string operator_name = func_name_str;
			std::string method_name = func_name_str;

			if (CXXMD) {
				auto this_ptr_qtype = CXXMD->getThisType();
				if ((!this_ptr_qtype.isNull()) && (this_ptr_qtype->isPointerType()) && (!(this_ptr_qtype->getPointeeType().isNull()))) {
					auto this_obj_qtype = this_ptr_qtype->getPointeeType();

					const auto arg_EX_qtype = this_obj_qtype;
					IF_DEBUG(const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();)

					const auto CXXRD = remove_mse_transparent_wrappers(arg_EX_qtype).getTypePtr()->getAsCXXRecordDecl();
					if (CXXRD) {
						auto qname = CXXRD->getQualifiedNameAsString();

						bool return_value_has_this_sublifetime_flag = false;
						bool return_value_has_this_lifetime_flag = false;

						if (((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (0 == CXXMD->getNumParams())) {
							{
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

									return_value_has_this_sublifetime_flag = true;
								} else if (xscope_owner_ptr_str == qname) {
									return_value_has_this_lifetime_flag = true;
								} else if (unique_ptr_str == qname) {
									if (arg_EX_qtype.isConstQualified()) {
										/* We're treating `const std::unique_ptr<>`s as similar to mse::TXScopeOwnerPointer<>s. */
										return_value_has_this_lifetime_flag = true;
									}
								}
							}
						} else if ((((method_value_str == method_name)) && (0 == CXXMD->getNumParams()))
							|| (((method_at_str == method_name)) && (1 == CXXMD->getNumParams()))
							|| (((method_front_str == method_name)) && (0 == CXXMD->getNumParams()))
							|| (((method_back_str == method_name)) && (0 == CXXMD->getNumParams()))
							|| (((operator_subscript_str == method_name)) && (1 == CXXMD->getNumParams()))
							) {

							DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");

							static const std::string std_unique_ptr_str = "std::unique_ptr";
							static const std::string std_tuple_str = "std::tuple";
							static const std::string std_pair_str = "std::pair";
							static const std::string std_array_str = "std::array";

							DECLARE_CACHED_CONST_STRING(mstd_tuple_str, mse_namespace_str() + "::mstd::tuple");
							DECLARE_CACHED_CONST_STRING(xscope_tuple_str, mse_namespace_str() + "::xscope_tuple");
							DECLARE_CACHED_CONST_STRING(nii_array_str, mse_namespace_str() + "::nii_array");
							DECLARE_CACHED_CONST_STRING(mstd_array_str, mse_namespace_str() + "::mstd::array");
							DECLARE_CACHED_CONST_STRING(xscope_nii_array_str, mse_namespace_str() + "::xscope_nii_array");
							DECLARE_CACHED_CONST_STRING(fixed_nii_vector_str, mse_namespace_str() + "::fixed_nii_vector");
							DECLARE_CACHED_CONST_STRING(xscope_fixed_nii_vector_str, mse_namespace_str() + "::xscope_fixed_nii_vector");
							DECLARE_CACHED_CONST_STRING(us_impl_fixed_nii_vector_base_str, mse_namespace_str() + "::us::impl::fixed_nii_vector_base");
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

							if ((fixed_optional_str == qname) || (xscope_fixed_optional_str == qname) || (xscope_borrowing_fixed_optional_str == qname)
								|| (fixed_any_str == qname) || (xscope_fixed_any_str == qname) || (xscope_borrowing_fixed_any_str == qname)
								) {

								return_value_has_this_lifetime_flag = true;
							} else if ((std_tuple_str == qname) || (std_pair_str == qname) || (std_array_str == qname)

								|| (mstd_tuple_str == qname) || (xscope_tuple_str == qname) || (nii_array_str == qname) || (mstd_array_str == qname) || (xscope_nii_array_str == qname)
								|| (fixed_nii_vector_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
								|| (fixed_nii_basic_string_str == qname) || (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)
								|| (us_impl_fixed_nii_vector_base_str == qname)
								) {

								return_value_has_this_lifetime_flag = true;
							}
						}

						if (return_value_has_this_lifetime_flag) {
							auto this_abstract_lifetime = CAbstractLifetime{ "_implict this lifetime label for legacy element_"
								, &func_decl, true/*is_elided*/ };
							flta.m_param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, CAbstractLifetimeSet{ this_abstract_lifetime });
							assert(0 == flta.m_return_value_lifetimes.m_primary_lifetimes.size());
							flta.m_return_value_lifetimes.m_primary_lifetimes.push_back(this_abstract_lifetime);
						}
					} else if (arg_EX_qtype->isReferenceType()) {
						int q = 5;
					}
				}
			} else {
				static const std::string std_swap_str = "std::swap";

				static const std::string std_move_str = "std::move";
				static const std::string function_get_str = "std::get";
				static const std::string std_begin_str = "std::begin";
				static const std::string std_end_str = "std::end";
				static const std::string std_cbegin_str = "std::cbegin";
				static const std::string std_cend_str = "std::cend";
				if (((std_move_str == func_qname_str) || (function_get_str == func_qname_str) || (std_begin_str == func_qname_str) || (std_end_str == func_qname_str)
					|| (std_cbegin_str == func_qname_str) || (std_cend_str == func_qname_str)) && (1 == func_decl.getNumParams())) {
					
					auto abstract_lifetime = CAbstractLifetime{ "_implict lifetime label for legacy element_"
						, &func_decl, true/*is_elided*/ };
					flta.m_lifetime_set.m_primary_lifetimes.push_back(abstract_lifetime);
					flta.m_param_lifetime_map.insert_or_assign(param_ordinal_t(1), CAbstractLifetimeSet{ abstract_lifetime });
					assert(0 == flta.m_return_value_lifetimes.m_primary_lifetimes.size());
					flta.m_return_value_lifetimes.m_primary_lifetimes.push_back(abstract_lifetime);
				} else if ((std_swap_str == func_qname_str) && (2 == func_decl.getNumParams())) {
					const std::string label1 = "_implict 1_";
					const std::string label2 = "_implict 2_";
					auto abstract_lifetime1 = CAbstractLifetime{ label1, &func_decl, true/*is_elided*/ };
					auto abstract_lifetime2 = CAbstractLifetime{ label2, &func_decl, true/*is_elided*/ };
					flta.m_lifetime_set.m_primary_lifetimes.push_back(abstract_lifetime1);
					flta.m_lifetime_set.m_primary_lifetimes.push_back(abstract_lifetime2);
					flta.m_param_lifetime_map.insert_or_assign(param_ordinal_t(1), CAbstractLifetimeSet{ abstract_lifetime1 });
					flta.m_param_lifetime_map.insert_or_assign(param_ordinal_t(2), CAbstractLifetimeSet{ abstract_lifetime2 });
					flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CFirstCanBeAssignedToSecond>(
									CFirstCanBeAssignedToSecond{ abstract_lifetime1, abstract_lifetime2 }));
					flta.m_lifetime_constraint_shptrs.push_back(std::make_shared<CFirstCanBeAssignedToSecond>(
									CFirstCanBeAssignedToSecond{ abstract_lifetime2, abstract_lifetime1 }));
				} else if ((((operator_star_str == operator_name) || (operator_arrow_str == operator_name)) && (1 == func_decl.getNumParams()))
					|| (((operator_subscript_str == operator_name)) && (2 == func_decl.getNumParams()))
					) {
					//potential_owner_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
					int q = 5;
				}
			}
		}

		if (!no_elided_flag) {
			/* In certain cases we add implicit ("elided") lifetime annotations, mostly as described in the 
			"Lifetime elision" section of the document: https://discourse.llvm.org/t/rfc-lifetime-annotations-for-c/61377 .
			Quoting from that document:
			<excerpt>

			As in Rust, to avoid unnecessary annotation clutter, we allow lifetime annotations to be elided 
			(omitted) from a function signature when they conform to certain regular patterns. Lifetime 
			elision is merely a shorthand for these regular lifetime patterns. Elided lifetimes are treated 
			exactly as if they had been spelled out explicitly; in particular, they are subject to lifetime 
			verification, so they are just as safe as explicitly annotated lifetimes.

			We propose to use the same rules as in Rust, as these transfer naturally to C++. We call 
			lifetimes on parameters input lifetimes and lifetimes on return values output lifetimes. (Note 
			that all lifetimes on parameters are called input lifetimes, even if those parameters are output 
			parameters.) Here are the rules:

			  1. Each input lifetime that is elided (i.e., not stated explicitly) becomes a distinct 
			  	lifetime.
		      2 If there is exactly one input lifetime (whether stated explicitly or elided), that lifetime 
			  	is assigned to all elided output lifetimes.
		      3. If there are multiple input lifetimes but one of them applies to the implicit this 
			  	parameter, that lifetime is assigned to all elided output lifetimes.

			</excerpt>

			We extend the elision somewhat by including cases where the input lifetime has "nested sublifetimes"
			that can be matched to corresponding nested lifetimes of the return type.
			*/

			bool could_be_a_dynamic_container_accessor = false;
			if (CXXMD && (!(CXXMD->isStatic())) && (!CXXCD) && (std::string("operator&") != func_decl.getNameAsString())) {
				auto this_qtype = CXXMD->getThisType();
				IF_DEBUG(const std::string this_qtype_str = this_qtype.getAsString();)
				if (!(this_qtype.isNull())) {
					auto this_pointee_qtype = this_qtype->getPointeeType();
					could_be_a_dynamic_container_accessor |= is_recognized_unprotected_dynamic_container(this_pointee_qtype);
				}
			}
			if (!could_be_a_dynamic_container_accessor) {
				auto& param_lifetime_map = flta.m_param_lifetime_map;

				auto add_elided_lifetime_annotation_if_necessary = [&](CAbstractLifetimeSet& palts1, clang::QualType const& qtype, std::string const& elided_lifetime_label_prefix) {
					CAbstractLifetimeSet const* taltas1_ptr = nullptr;

					CTypeLifetimeAnnotations tlta_for_nonconst_pointer_parameter;

					if (is_raw_pointer_or_equivalent(qtype) && (!qtype.isConstQualified())) {
						/* Reference and const pointer (not to be confused with pointer-to-const) types automatically (always) 
						get implicit lifetime annotations. This is not the case for non-const pointers, because it could change
						which assignment operations are valid. But in the case where a direct function parameter is a non-const
						pointer, we will add an elided lifetime. */

						tlta_for_nonconst_pointer_parameter = CTypeLifetimeAnnotations{ CAbstractLifetime{ elided_lifetime_label_prefix + " non-const pointer_", &func_decl } };
						tlta_for_nonconst_pointer_parameter.m_parse_errors_noted = true;

						auto maybe_pointee_qtype = pointee_type_if_any(qtype);
						if (maybe_pointee_qtype.has_value()) {
							auto& pointee_qtype = maybe_pointee_qtype.value();
							auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(pointee_qtype, state1, MR_ptr, Rewrite_ptr);
							if (maybe_pointee_tlta_ptr.has_value()) {
								auto& pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
								*(tlta_for_nonconst_pointer_parameter.m_lifetime_set.m_primary_lifetimes.front().m_sublifetimes_vlptr) = pointee_tlta_ptr->m_lifetime_set;
								tlta_for_nonconst_pointer_parameter.m_lifetime_constraint_shptrs = pointee_tlta_ptr->m_lifetime_constraint_shptrs;
							}
						} else {
							/* unexpected */
							int q = 3;
						}
						taltas1_ptr = &(tlta_for_nonconst_pointer_parameter.m_lifetime_set);
					} else {
						auto maybe_taltas_ptr = type_lifetime_annotations_if_available(get_cannonical_type(qtype), state1, MR_ptr, Rewrite_ptr);
						if (maybe_taltas_ptr.has_value()) {
							taltas1_ptr = &(maybe_taltas_ptr.value()->m_lifetime_set);
						}
					}

					if (taltas1_ptr) {
						auto const & talts1 = *taltas1_ptr;
						/* The parameter's type seems to have (implict or explicit) annotated lifetimes. */

						struct CB {
							static void add_elided_lifetime_annotation_where_necessary(CAbstractLifetimeSet& palts1, CAbstractLifetimeSet const & talts1, clang::FunctionDecl const& func_decl2, std::string_view id_prefix) {
								for (auto i2 = palts1.m_primary_lifetimes.size(); talts1.m_primary_lifetimes.size() > i2; i2 += 1) {
									palts1.m_primary_lifetimes.push_back(CAbstractLifetime{ std::string(id_prefix) + std::to_string(i2) + "_" + talts1.m_primary_lifetimes.at(i2).m_id + "_"
										, &func_decl2, true/*is_elided*/ });
								}
								for (auto i2 = size_t(0); talts1.m_primary_lifetimes.size() > i2; i2 += 1) {
									add_elided_lifetime_annotation_where_necessary(*(palts1.m_primary_lifetimes.at(i2).m_sublifetimes_vlptr), *(talts1.m_primary_lifetimes.at(i2).m_sublifetimes_vlptr), func_decl2, std::string(id_prefix) + std::to_string(i2));
								}
							}
						};

						CB::add_elided_lifetime_annotation_where_necessary(palts1, talts1, func_decl, elided_lifetime_label_prefix);
					}
				};

				auto add_elided_lifetime_annotation_to_param_if_necessary = [&](checker::param_ordinal_t param_ordinal, clang::QualType const& PVD_qtype, std::string_view param_name) {
					CAbstractLifetimeSet palts1;
					auto found_it = param_lifetime_map.find(param_ordinal);
					if (param_lifetime_map.end() != found_it) {
						palts1 = found_it->second;
					}

					std::string param_index_string = "";
					auto maybe_param_zb_index = param_ordinal.as_a_zero_based_parameter_index_if_valid();
					if (maybe_param_zb_index.has_value()) {
						param_index_string = std::to_string(maybe_param_zb_index.value()) + " ";
					}
					std::string elided_lifetime_label_prefix = "_elided parameter lifetime " + param_index_string + std::string(param_name) + " ";

					add_elided_lifetime_annotation_if_necessary(palts1, PVD_qtype, elided_lifetime_label_prefix);

					if (!(palts1.is_empty())) {
						param_lifetime_map.insert_or_assign(param_ordinal, palts1);
					}
				};

				auto param_ordinal = is_ns_member_operator ? param_ordinal_t(param_ordinal_t::ns_member_operator_tag{}, 1)
					: param_ordinal_t(1);

				for (auto param_iter = func_decl.param_begin(); func_decl.param_end() != param_iter; ++param_iter, param_ordinal += 1) {
					auto param = (*param_iter);
					auto PVD = (*param_iter);
					if (!PVD) {
						assert(false); continue;
					}
					auto PVD_qtype = PVD->getType();
					IF_DEBUG(const std::string PVD_qtype_str = PVD->getType().getAsString();)
					if (PVD_qtype.isNull()) {
						continue;
					}

					if (!no_elided_flag) {
						if (!(PVD->hasDefaultArg())) {
							add_elided_lifetime_annotation_to_param_if_necessary(param_ordinal, PVD_qtype, PVD->getNameAsString());
						}
					}
				}
				if (CXXMD && (!(CXXMD->isStatic())) && (!CXXCD)) {
					auto this_qtype = CXXMD->getThisType();
					IF_DEBUG(const std::string this_qtype_str = this_qtype.getAsString();)
					param_ordinal = IMPLICIT_THIS_PARAM_ORDINAL;

					if (!no_elided_flag) {
						CAbstractLifetimeSet palts1;
						auto found_it = param_lifetime_map.find(IMPLICIT_THIS_PARAM_ORDINAL);
						if (param_lifetime_map.end() != found_it) {
							palts1 = found_it->second;
						}
						if (1 <= palts1.m_primary_lifetimes.size()) {
							if (1 < palts1.m_primary_lifetimes.size()) {
								int q = 3;
								assert(false);
							};
						} else {
							palts1.m_primary_lifetimes.push_back(CAbstractLifetime{ "_elided implicit_this_parameter lifetime_"
										, &func_decl, true/*is_elided*/ });
						}

						CAbstractLifetimeSet const* taltas1_ptr = nullptr;
						auto maybe_pointee_qtype = pointee_type_if_any(this_qtype);
						if (maybe_pointee_qtype.has_value()) {
							auto& pointee_qtype = maybe_pointee_qtype.value();
							auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(pointee_qtype, state1, MR_ptr, Rewrite_ptr);
							if (maybe_pointee_tlta_ptr.has_value()) {
								auto& pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
								taltas1_ptr = &(pointee_tlta_ptr->m_lifetime_set);
							}
						} else {
							/* unexpected */
							int q = 3;
						}
						if (taltas1_ptr) {
							auto const & talts1 = *taltas1_ptr;
							/* The implicit `this` parameter's type seems to have (implict or explicit) annotated lifetimes. */

							struct CB {
								static void add_elided_lifetime_annotation_where_necessary(CAbstractLifetimeSet& palts1, CAbstractLifetimeSet const & talts1) {
									for (auto i2 = palts1.m_primary_lifetimes.size(); talts1.m_primary_lifetimes.size() > i2; i2 += 1) {
										palts1.m_primary_lifetimes.push_back(talts1.m_primary_lifetimes.at(i2));
									}
									for (auto i2 = size_t(0); talts1.m_primary_lifetimes.size() > i2; i2 += 1) {
										add_elided_lifetime_annotation_where_necessary(*(palts1.m_primary_lifetimes.at(i2).m_sublifetimes_vlptr), *(talts1.m_primary_lifetimes.at(i2).m_sublifetimes_vlptr));
									}
								}
							};

							CB::add_elided_lifetime_annotation_where_necessary(*(palts1.m_primary_lifetimes.at(0).m_sublifetimes_vlptr), talts1);

							if (!(palts1.is_empty())) {
								param_lifetime_map.insert_or_assign(IMPLICIT_THIS_PARAM_ORDINAL, palts1);
							}
						} else {
							int q = 5;
						}

						//add_elided_lifetime_annotation_to_param_if_necessary(IMPLICIT_THIS_PARAM_ORDINAL, this_qtype, "implicit_this_parameter");
					}
				}

				if (flta.m_return_value_lifetimes.is_empty()) {
					auto rv_qtype = func_decl.getReturnType();
					IF_DEBUG(const std::string rv_qtype_str = rv_qtype.getAsString();)

					CAbstractLifetimeSet talts1;
					add_elided_lifetime_annotation_if_necessary(talts1, rv_qtype, "rv_lifetime_label_prefix");

					if (!(talts1.is_empty())) {
						/* The return value doesn't seem to have any explicitly annotated lifetimes. Depending on the 
						scenario, we may may give it elided ones. */
						std::optional<CAbstractLifetime> maybe_param_lifetime;
						std::optional<clang::QualType> maybe_param_qtype;

						auto& param_lifetime_map = flta.m_param_lifetime_map;

						auto found_it = param_lifetime_map.find(IMPLICIT_THIS_PARAM_ORDINAL);
						if (param_lifetime_map.end() != found_it) {
							if (1 == found_it->second.m_primary_lifetimes.size()) {
								/* An implicit `this` parameter lifetime is present. We'll use it as the return value lifetime if we can. */
								maybe_param_lifetime = found_it->second.m_primary_lifetimes.front();
								maybe_param_qtype = qtype_from_param_ordinal_if_available(&func_decl, IMPLICIT_THIS_PARAM_ORDINAL);
							} else {
								/* unexpected */
								int q = 3;
							}
						}
						if (!(maybe_param_lifetime.has_value())) {
							if (1 == param_lifetime_map.size()) {
								auto param_lifetimes_ref = param_lifetime_map.begin()->second;
								if (1 == param_lifetimes_ref.m_primary_lifetimes.size()) {
									maybe_param_qtype = qtype_from_param_ordinal_if_available(&func_decl, param_lifetime_map.begin()->first);
									if (maybe_param_qtype.has_value() && (!(rv_qtype.isNull()))) {
										auto param_qtype = maybe_param_qtype.value();

										/* This function seems to have one (elided or explictly annotated) input parameter lifetime. We'll
										use it as the return value lifetime if we can. */
										maybe_param_lifetime = param_lifetimes_ref.m_primary_lifetimes.front();
									}
								}
							};
						}
						if (maybe_param_lifetime.has_value() && maybe_param_qtype.has_value()) {
							auto const & param_lifetime = maybe_param_lifetime.value();
							auto const & param_qtype = maybe_param_qtype.value();
							IF_DEBUG(const std::string param_qtype_str = param_qtype.getAsString();)

							bool insufficiently_matching_lifetime_flag = false;

							struct CB {
								/* Returns a representation of the tree structure of lifetimes and sublifetimes. The specific value of 
								the lifetimes doesn't affect the representation, just their position in the heirarchy. */
								static std::string lifetime_set_tree_structure_representation_str(CAbstractLifetimeSet const& alts1) {
									std::string tree_structure_representation_str;

									bool has_one_or_more_primary_lifetimes = (1 <= alts1.m_primary_lifetimes.size());
									if (has_one_or_more_primary_lifetimes) {
										for (const auto& alt1 : alts1.m_primary_lifetimes) {
											tree_structure_representation_str.append("l");
											auto branch_representation_str = lifetime_set_tree_structure_representation_str(*(alt1.m_sublifetimes_vlptr));
											if (1 <= branch_representation_str.length()) {
												tree_structure_representation_str.append("(");
												tree_structure_representation_str.append(std::move(branch_representation_str));
												tree_structure_representation_str.append(")");
											}
											tree_structure_representation_str.append(",");
										}
										{
											/* remove the last comma */
											tree_structure_representation_str.pop_back();
										}
									}

									return tree_structure_representation_str;
								}
								/* Returns a representation of the tree structure of lifetimes and sublifetimes. The specific value of 
								the lifetimes doesn't affect the representation, just their position in the heirarchy. */
								static std::string lifetime_tree_structure_representation_str(CAbstractLifetime const& alt1) {
									return lifetime_set_tree_structure_representation_str(CAbstractLifetimeSet{ alt1 });
								}
							};

							auto const param_lifetime_tree_structure_representation_str = CB::lifetime_tree_structure_representation_str(param_lifetime);
							if (2 <= param_lifetime_tree_structure_representation_str.length()) {
								int q = 5;
							}

							auto elided_rv_alts = talts1;
							for (auto& elided_rv_lifetime_ref : elided_rv_alts.m_primary_lifetimes) {

								auto const elided_rv_lifetime_tree_structure_representation_str = CB::lifetime_tree_structure_representation_str(elided_rv_lifetime_ref);
								if (2 <= elided_rv_lifetime_tree_structure_representation_str.length()) {
									int q = 5;
								}

								if (param_lifetime_tree_structure_representation_str == elided_rv_lifetime_tree_structure_representation_str) {
									/* We'll use the (lone) parameter lifetime as the return value lifetime. */
									elided_rv_lifetime_ref = param_lifetime;
								} else if (param_qtype->isReferenceType() || is_raw_pointer_or_equivalent(param_qtype)) {
									auto const param_sublifetime_tree_structure_representation1_str = CB::lifetime_set_tree_structure_representation_str(*(param_lifetime.m_sublifetimes_vlptr));

									if (param_sublifetime_tree_structure_representation1_str == elided_rv_lifetime_tree_structure_representation_str) {
										/* The parameter lifetime level of indirection (i.e. lifetime depth) is one greater than that of this 
										return type lifetime. This could be a situation where the parameter is passed by reference, but the 
										corresponding return value isn't. */
										if (1 == param_lifetime.m_sublifetimes_vlptr->m_primary_lifetimes.size()) {
											auto & param_sublifetime = param_lifetime.m_sublifetimes_vlptr->m_primary_lifetimes.front();
											elided_rv_lifetime_ref = param_sublifetime;
										} else {
											/* unexpected? */
											int q = 3;
											insufficiently_matching_lifetime_flag = true;
											break;
										}
									} else {
										insufficiently_matching_lifetime_flag = true;
										break;
									}
								} else {
									/* The level of indirection (i.e. the (sub)lifetime depth) of the parameter and return type don't match. */
									insufficiently_matching_lifetime_flag = true;
									break;
								}
							}
							if ((!(elided_rv_alts.is_empty())) && (!insufficiently_matching_lifetime_flag)) {
								flta.m_return_value_lifetimes = elided_rv_alts;
								flta.m_return_value_lifetimes_is_elided = true;
							}
						}
					}
				}
			} else {
				int q = 5;
			}
		}

		state1.m_function_lifetime_annotations_map.insert_or_assign(&func_decl, flta);

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

			DECLARE_CACHED_CONST_STRING(lifetime_notes_str, "lifetime_notes");
			DECLARE_CACHED_CONST_STRING(lifetime_labels, "lifetime_labels");
			DECLARE_CACHED_CONST_STRING(lifetime_label, "lifetime_label");
			DECLARE_CACHED_CONST_STRING(clang_lifetimebound_str, "clang::lifetimebound");

			if (param->hasAttrs()) {
				auto with_lifetime_prefix = [](std::string_view sv1) -> std::string {
					auto sv2 = sv1.substr(Parse::find_non_whitespace(sv1));
					return std::string("lifetime_") + std::string(sv2);
				};
				DECLARE_CACHED_CONST_STRING(lifetime_set, "lifetime_set");

				auto vec = param->getAttrs();
				struct CLTAStatementInfo {
					CLTAStatementInfo(std::string_view text, clang::Attr const * attr_ptr, bool is_a_lifetime_note = false)
						: m_text(text), m_attr_ptr(attr_ptr), m_is_a_lifetime_note(is_a_lifetime_note) {}
					std::string m_text;
					clang::Attr const * m_attr_ptr = nullptr;
					bool m_is_a_lifetime_note = false;
				};
				std::vector<CLTAStatementInfo> lta_statement_infos;
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
					auto first_mse_range = Parse::find_token_sequence({ mse_namespace_str(), "::" }, raw_pretty_str, first_quote_index + 1);
					if (raw_pretty_str.length() <= first_mse_range.begin) {
						continue;
					}
					std::string pretty_str = raw_pretty_str.substr(first_mse_range.end);
					auto index1 = pretty_str.find(lifetime_notes_str);
					if (decltype(pretty_str)::npos != index1) {
						static const std::string lbrace = "{";
						static const std::string rbrace = "}";
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";
						static const std::string semicolon = ";";
						index1 = pretty_str.find(lbrace, index1 + lifetime_notes_str.length());
						if (decltype(pretty_str)::npos == index1) {
							continue;
						}
						auto last_delimiter_index = index1;
						auto next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
						while (decltype(pretty_str)::npos != next_delimiter_index) {
							std::string_view sv1(pretty_str.data() + last_delimiter_index + 1, int(next_delimiter_index) - int(last_delimiter_index + 1));
							auto prefixed_str = with_lifetime_prefix(sv1);
							lta_statement_infos.push_back( CLTAStatementInfo{ prefixed_str, attr, true } );

							last_delimiter_index = next_delimiter_index;
							next_delimiter_index = pretty_str.find_first_of(semicolon + rbrace, last_delimiter_index + 1);
						}
					} else {
						lta_statement_infos.push_back( CLTAStatementInfo{ pretty_str, attr } );
					}
				}

				for (const auto& lta_statement_info : lta_statement_infos) {
					std::string_view pretty_str = lta_statement_info.m_text;
					std::string_view sv1 = lta_statement_info.m_text;
					auto attr_SR = lta_statement_info.m_attr_ptr->getRange();

					auto lifetime_labels_range = Parse::find_token_sequence({ lifetime_labels }, pretty_str);
					if (pretty_str.length() <= lifetime_labels_range.begin) {
						lifetime_labels_range = Parse::find_token_sequence({ lifetime_label }, pretty_str);
					}
					if (pretty_str.length() > lifetime_labels_range.begin) {
						static const std::string lparenthesis = "(";
						static const std::string rparenthesis = ")";

						auto lparenthesis_range = Parse::find_token_at_same_nesting_depth1(lparenthesis, pretty_str, lifetime_labels_range.end);
						if (pretty_str.length() <= lparenthesis_range.begin) {
							continue;
						}
						auto lparenthesis_index = lparenthesis_range.begin;
						auto rparenthesis_index = Parse::find_matching_right_parenthesis(pretty_str, lparenthesis_range.end);
						if (pretty_str.length() <= rparenthesis_index) {
							int q = 3;
							continue;
						}
						std::string_view sv1(pretty_str.data() + lparenthesis_index + 1, int(rparenthesis_index) - int(lparenthesis_index + 1));

						CAbstractLifetimeSet alts2 = parse_lifetime_ids(sv1, &func_decl, attr_SR, state1, MR_ptr, Rewrite_ptr);
						CAbstractLifetimeSet alts1 = with_any_lifetime_aliases_dealiased(alts2, state1, MR_ptr, Rewrite_ptr);

						if (true /*!(alts1.is_empty())*/) {
							int num_declared_primary_lifetimes = 0;

							if (!(param->getType().isNull())) {
								auto found_iter2 = state1.m_type_lifetime_annotations_map.find(get_cannonical_type(param->getType()).getTypePtr());
								if (state1.m_type_lifetime_annotations_map.end() != found_iter2) {
									auto const & tlta = found_iter2->second;
									num_declared_primary_lifetimes = tlta.m_lifetime_set.m_primary_lifetimes.size();
								}
							}

							CVariableLifetimeAnnotations vlta;
							vlta.m_lifetime_set = alts1;
							vlta.m_parse_errors_noted = (nullptr != MR_ptr);
							state1.m_vardecl_lifetime_annotations_map.insert_or_assign(&var_decl, vlta);
						} else {
							if (false && MR_ptr) {
								std::string error_desc = std::string("No valid 'lifetime label id' specified.");
								state1.register_error(*(MR_ptr->SourceManager), attr_SR, error_desc);
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

				bool errors_suppressed_by_location_flag = errors_suppressed_by_location(MR, SR.getBegin());
				auto suppress_check_flag = errors_suppressed_by_location_flag;
				suppress_check_flag |= state1.m_suppress_check_region_set.contains(RD, Rewrite, *(MR.Context));
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
									const auto* CE = Tget_immediately_containing_element_of_type<clang::CallExpr>(
										LE, *MR.Context);
									if (!CE) {
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
											CE = Tget_immediately_containing_element_of_type<clang::CallExpr>(
												MTE->IgnoreImpCasts(), *MR.Context);
										}
									}
									if (CE) {
										auto FND = CE->getDirectCallee();
										if (FND) {
											const auto qname = FND->getQualifiedNameAsString();
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
										} else {
											//auto b1 = CXXRD->isTemplateDecl();
											auto b2 = CXXRD->isTemplated();
											bool b3 = false;
											{
												auto CTSD = clang::dyn_cast<const clang::ClassTemplateSpecializationDecl>(CXXRD);
												if (CTSD) {
													auto CTD2 = CTSD->getSpecializedTemplate();
													if (CTD2) {
														//auto tparam_list = CTD2->getTemplateParameters();
														b3 = true;
													}
												} else {
													int q = 5;
												}
											}
											if (b2 && !b3) {
												/* In this case, we suspect that the reason the function declaration of the direct callee is not 
												available may be because this is part of an uninstantiated template. In this case we can't 
												reliably determine if our imposed requirements are satisfied or not. We'll defer verification to
												the actual instantiations of this template. */
												return;
											} else {
												int q = 5;
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
										auto field_suppress_check_flag = state1.m_suppress_check_region_set.contains(FD, Rewrite, *(MR.Context));
										if (field_suppress_check_flag) {
											continue;
										}

										const auto ICIEX = FD->getInClassInitializer();
										if (is_raw_pointer_or_equivalent(field_qtype)
											&& (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype))
											&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
											) {
											if (!ICIEX) {
												unverified_pointer_fields.push_back(FD);
											} else if (is_nullptr_literal(ICIEX, *(MR.Context)) && (!suppress_check_flag)) {
												auto ICISR = nice_source_range(ICIEX->getSourceRange(), Rewrite);
												if (!ICISR.isValid()) {
													ICISR = SR;
												}
												const std::string error_desc = std::string("Null initialization of ")
													+ "native pointer fields (such as '" + FD->getNameAsString()
													+ "') is not supported.";
												state1.register_error(*MR.SourceManager, ICISR, error_desc);
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
															if (((!res) || (FD == FD2)) && (!suppress_check_flag)) {
																auto MESR = nice_source_range(ME->getSourceRange(), Rewrite);
																if (!MESR.isValid()) {
																	MESR = SR;
																}

																const std::string error_desc = std::string("The field '") + FD2->getNameAsString()
																	+ "' may be being referenced before it has been constructed.";
																state1.register_error(*MR.SourceManager, MESR, error_desc);
															}
														} else {
															const auto CXXMD = dyn_cast<const CXXMethodDecl>(ME->getMemberDecl());
															if (CXXMD && (!suppress_check_flag)) {
																/* error: Unable to verify that the member function used here can't access part of
																the object that hasn't been constructed yet. */
																const std::string error_desc = std::string("Calling non-static member functions ")
																+ "(such as '" + CXXMD->getQualifiedNameAsString() + "') of an object is not supported in "
																+ "constructor initializers or direct field initializers of the object. Consider "
																+ "using a static member or free function instead. ";
																state1.register_error(*MR.SourceManager, SR, error_desc);
															} else {
																/* Since this MemberExpr was obtained from a CXXThisExpr, if it doesn't refer to
																a FD, then presumably it refers to a (non-static) member function.
																So arriving here would be unexpected. */
																int q = 5;
															}
														}
													} else if (!suppress_check_flag) {
														const std::string error_desc = std::string("Unable to verify that the 'this' pointer ")
														+ "used here can't be used to access part of the object that hasn't been constructed yet.";
														state1.register_error(*MR.SourceManager, SR, error_desc);
													}
												}
												if (field_qtype->isPointerType()
													&& (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype))
													&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
													) {
													if (is_nullptr_literal(EX, *(MR.Context)) && (!suppress_check_flag)) {
														auto CISR = nice_source_range(EX->getSourceRange(), Rewrite);
														if (!CISR.isValid()) {
															CISR = SR;
														}
														const std::string error_desc = std::string("Null initialization of ")
															+ "native pointer field '" + FD->getNameAsString()
															+ "' is not supported.";
														state1.register_error(*MR.SourceManager, CISR, error_desc);
													}
												}
											}
										}
									}
								}
								if (1 <= unverified_pointer_fields.size()) {
									if ((CXXRD->ctors().end() == CXXRD->ctors().begin()) && (!suppress_check_flag)) {
										/* There don't seem to be any constructors. */
										auto field_SR = nice_source_range(unverified_pointer_fields.front()->getSourceRange(), Rewrite);
										if (!field_SR.isValid()) {
											field_SR = SR;
										}
										const std::string error_desc = std::string("Missing constructor initializer (or ")
										+ "direct initializer) required for '" + unverified_pointer_fields.front()->getNameAsString()
										+ "' (raw) pointer field.";
										state1.register_error(*MR.SourceManager, field_SR, error_desc);
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
										if ((1 <= l_unverified_pointer_fields.size()) && (!suppress_check_flag)) {
											auto constructor_SR = nice_source_range(constructor->getSourceRange(), Rewrite);
											if (!constructor_SR.isValid()) {
												constructor_SR = SR;
											}
											const std::string error_desc = std::string("Missing constructor initializer (or ")
											+ "direct initializer) required for '" + l_unverified_pointer_fields.front()->getNameAsString()
											+ "' (raw) pointer field.";
											state1.register_error(*MR.SourceManager, constructor_SR, error_desc);
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

						for (const auto FD : RD->fields()) {
							const auto field_qtype = FD->getType();
							IF_DEBUG(auto field_qtype_str = field_qtype.getAsString());
							auto field_suppress_check_flag = state1.m_suppress_check_region_set.contains(FD, Rewrite, *(MR.Context));
							if (field_suppress_check_flag) {
								continue;
							}

							if (!(field_qtype.isNull())) {
								std::string error_desc;
								if (field_qtype.getTypePtr()->isPointerType()) {
									if (!state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(field_qtype)
										&& (!state1.m_suppress_check_region_set.contains(instantiation_source_range(FD->getSourceRange(), Rewrite)))
										) {
										if (has_xscope_tag_base) {
											/*
											error_desc = std::string("Native pointers are not (yet) supported as fields of xscope ")
												+ "structs or classes.";
											*/
										} else {
											if (is_lambda) {
												error_desc = std::string("Native pointers (such as those of type ") + get_as_quoted_string_for_errmsg(field_qtype)
													+ ") are not supported as captures of (non-xscope) lambdas. ";
											} else {
												error_desc = std::string("Native pointers (such as those of type ") + get_as_quoted_string_for_errmsg(field_qtype)
													+ ") are not supported as fields of (non-xscope) structs or classes.";
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
											error_desc = std::string("Native references (such as those of type ") + get_as_quoted_string_for_errmsg(field_qtype)
												+ ") are not supported as captures of (non-xscope) lambdas. ";
										} else {
											error_desc = std::string("Native references (such as those of type ") + get_as_quoted_string_for_errmsg(field_qtype)
												+") are not supported as fields of (non-xscope) structs or classes.";
										}
									}
								}

								if (g_enforce_scpp_type_indicator_tags && (!has_xscope_tag_base) && is_xscope_type(field_qtype, state1)) {
									if (is_lambda) {
										error_desc = std::string("Lambdas that capture variables of xscope type (such as ")
											+ get_as_quoted_string_for_errmsg(field_qtype) + ") must be scope lambdas (usually created via an "
											+  "'mse::rsv::make_xscope_*_lambda()' wrapper function).";
									} else {
										error_desc = std::string("Structs or classes containing fields of xscope type (such as ")
											+ get_as_quoted_string_for_errmsg(field_qtype) + ") must inherit from mse::rsv::XScopeTagBase.";
									}
								}
								if (g_enforce_scpp_type_indicator_tags && (!has_ContainsNonOwningScopeReference_tag_base)
									&& contains_non_owning_scope_reference(field_qtype, state1)) {
									if (is_lambda) {
										error_desc = std::string("Lambdas that capture items (such as those of type ")
											+ get_as_quoted_string_for_errmsg(field_qtype) + ") that are, or contain, non-owning scope references must be "
											+ "scope 'reference or pointer capture' lambdas (created via the "
											+ "'mse::rsv::make_xscope_reference_or_pointer_capture_lambda()' "
											+ "wrapper function).";
									} else {
										error_desc = std::string("Structs or classes containing fields (such as those of type ")
											+ get_as_quoted_string_for_errmsg(field_qtype) + ") that are, or contain, non-owning scope references must inherit from "
											+ "mse::rsv::ContainsNonOwningScopeReferenceTagBase.";
									}
								}
								if (g_enforce_scpp_type_indicator_tags && (!has_ReferenceableByScopePointer_tag_base)
									&& referenceable_by_scope_pointer(field_qtype, state1)) {
									if (is_lambda) {
										/* The assumption is that we don't have to worry about scope pointers targeting
										lambda capture variables(/fields) from outside the lambda, because they're not 
										directly accessible from outside? */
									} else {
										error_desc = std::string("Structs or classes containing fields (such as ") + get_as_quoted_string_for_errmsg(field_qtype)
											+ ") that yield scope pointers (from their overloaded 'operator&'), or contain an element "
											+ "that does, must inherit from mse::rsv::ReferenceableByScopePointerTagBase.";
									}
								}
								if (("" != error_desc) && (!suppress_check_flag)) {
									auto FDISR = instantiation_source_range(FD->getSourceRange(), Rewrite);
									state1.register_error(*MR.SourceManager, FDISR, error_desc);
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
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
			auto found_iter = state1.m_type_lifetime_annotations_map.find(get_cannonical_type_ptr(Type_ptr));
			if (state1.m_type_lifetime_annotations_map.end() != found_iter) {
				auto const & tlta = found_iter->second;
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

	inline auto type_lifetime_annotations_if_available(const clang::Type * TypePtr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/, bool use_implicit_lifetime_annotation_for_pointer/* = false*/)
		-> std::optional<CTypeLifetimeAnnotations const *> {

		std::optional<CTypeLifetimeAnnotations const *> retval;
		if (TypePtr) {
			process_type_lifetime_annotations(TypePtr, state1, MR_ptr, Rewrite_ptr);
			auto TD = TypePtr->getAsRecordDecl();
			if (TD) {
				IF_DEBUG(auto type_str = TD->getNameAsString();)
				process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
				auto TypePtr2 = TD->getTypeForDecl();
				if (TypePtr2) {
					TypePtr = TypePtr2;
				}
			}
			const auto cn_type_ptr = get_cannonical_type_ptr(TypePtr);
			IF_DEBUG(auto cn_qtype_str = cn_type_ptr->getCanonicalTypeInternal().getAsString();)
			auto tlta_iter1 = state1.m_type_lifetime_annotations_map.find(cn_type_ptr);
			if (state1.m_type_lifetime_annotations_map.end() != tlta_iter1) {
				retval = &(tlta_iter1->second);
			} else if ((use_implicit_lifetime_annotation_for_pointer && is_raw_pointer_or_equivalent(cn_type_ptr))
				|| cn_type_ptr->isReferenceType()) {
				/* Adding lifetime annotations to a (non-const) raw pointer can change whether a (re-)assignment
				operation is valid or not. But in this case we've been explicitly instructed to use an implicit 
				lifetime annotation if the type is a raw pointer. Raw references on the other hand, don't support 
				(re-)assignment, there's no risk of (unintentionally) invalidating any assignment operation by 
				adding a lifetime annotation. */

				auto implicit_tlta_iter1 = state1.m_implicit_type_lifetime_annotations_map.find(cn_type_ptr);
				if (state1.m_implicit_type_lifetime_annotations_map.end() != implicit_tlta_iter1) {
					retval = &(implicit_tlta_iter1->second);
				} else {
					/* Since it doesn't seem to have been already done, here we'll generate and store implicit lifetime 
					annotations for this raw pointer or reference type. */

					auto const label_id_str = std::string("_implicit pointer/reference lifetime for ") + get_as_string(cn_type_ptr) + "_";
					auto implicit_tlta = CTypeLifetimeAnnotations{ CAbstractLifetime{ label_id_str, (clang::Decl const *)(nullptr) } };
					implicit_tlta.m_parse_errors_noted = true;

					auto maybe_pointee_qtype = pointee_type_if_any(clang::QualType(cn_type_ptr, 0/*I'm just assuming zero specifies no qualifiers*/));
					if (maybe_pointee_qtype.has_value()) {
						auto& pointee_qtype = maybe_pointee_qtype.value();
						auto maybe_pointee_tlta_ptr = type_lifetime_annotations_if_available(pointee_qtype, state1, MR_ptr, Rewrite_ptr);
						if (maybe_pointee_tlta_ptr.has_value()) {
							auto& pointee_tlta_ptr = maybe_pointee_tlta_ptr.value();
							*(implicit_tlta.m_lifetime_set.m_primary_lifetimes.front().m_sublifetimes_vlptr) = pointee_tlta_ptr->m_lifetime_set;
							implicit_tlta.m_lifetime_constraint_shptrs = pointee_tlta_ptr->m_lifetime_constraint_shptrs;
						}
					} else {
						/* unexpected */
						int q = 3;
					}
					state1.insert_or_assign_implicit_type_lifetime_annotations(TypePtr, implicit_tlta);

					implicit_tlta_iter1 = state1.m_implicit_type_lifetime_annotations_map.find(cn_type_ptr);
					if (state1.m_implicit_type_lifetime_annotations_map.end() != implicit_tlta_iter1) {
						retval = &(implicit_tlta_iter1->second);
					} else {
						/* unexpected */
						int q = 3;
					}
				}

			}

		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(clang::QualType const & qtype, CTUState& state1, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/, bool use_implicit_lifetime_annotation_for_pointer/* = false*/)
		-> std::optional<CTypeLifetimeAnnotations const *> {

		std::optional<CTypeLifetimeAnnotations const *> retval;
		if (qtype.isNull()) {
			return retval;
		}

		process_type_lifetime_annotations(qtype, state1, MR_ptr, Rewrite_ptr);

		if (is_raw_pointer_or_equivalent(qtype) && qtype.isConstQualified()) {
			/* Adding lifetime annotations to a (non-const) raw pointer can change whether a (re-)assignment
			operation is valid or not. But since const pointers (not to be confused with pointer-to-const) 
			don't support (re-)assignment, there's no risk of (unintentionally) invalidating any assignment 
			operation by adding a lifetime annotation. So, we give const pointer types an implicit lifetime 
			label (but store it in a separate map for const qualified types). */

			const auto cn_qtype = get_cannonical_type(qtype);
			IF_DEBUG(auto cn_qtype_str = cn_qtype.getAsString();)
			auto tlta_iter1 = state1.m_const_qualified_type_lifetime_annotations_map.find(cn_qtype.getAsString());
			if (state1.m_const_qualified_type_lifetime_annotations_map.end() != tlta_iter1) {
				retval = &(tlta_iter1->second);
				return retval;
			} else {
				/* unexpected? */
				int q = 3;
			}
		}

		retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr, use_implicit_lifetime_annotation_for_pointer);
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::VarDecl& var_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations const *> {

		std::optional<CTypeLifetimeAnnotations const *> retval;
		auto qtype = var_decl.getType();
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		if (is_raw_pointer_or_equivalent(qtype) || qtype->isReferenceType()) {
			process_variable_lifetime_annotations(var_decl, state1, MR_ptr, Rewrite_ptr);
			auto vlta_iter1 = state1.m_vardecl_lifetime_annotations_map.find(&var_decl);
			if (state1.m_vardecl_lifetime_annotations_map.end() != vlta_iter1) {
				/* Even though (non-const) raw pointer types don't have expliicit lifetime annotations in their 
				definition (indeed they are built in types so their definition is not available at all), we will 
				consider the type to have an implied lifetime annotation when it is the type of a member or 
				variable declaration that has a lifetime annotation. */

				retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr
					, true/*use_implicit_lifetime_annotation_for_pointer*/);
			} else {
				retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
			}
		} else {
			retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::FieldDecl& field_decl, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations const *> {
		std::optional<CTypeLifetimeAnnotations const *> retval;
		auto qtype = field_decl.getType();
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
		if (is_raw_pointer_or_equivalent(qtype) || qtype->isReferenceType()) {
			process_type_lifetime_annotations(field_decl, state1, MR_ptr, Rewrite_ptr);
			auto fltv_iter1 = state1.m_fielddecl_to_abstract_lifetime_map.find(&field_decl);
			if (state1.m_fielddecl_to_abstract_lifetime_map.end() != fltv_iter1) {
				/* Even though (non-const) raw pointer types don't have expliicit lifetime annotations in their 
				definition (indeed they are built in types so their definition is not available at all), we will 
				consider the type to have an implied lifetime annotation when it is the type of a member or 
				variable declaration that has a lifetime annotation. */

				retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr
					, true/*use_implicit_lifetime_annotation_for_pointer*/);
			}
		} else {
			retval = type_lifetime_annotations_if_available(qtype.getTypePtr(), state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}
	inline auto type_lifetime_annotations_if_available(const clang::Expr& expr, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) 
		-> std::optional<CTypeLifetimeAnnotations const *> {
		std::optional<CTypeLifetimeAnnotations const *> retval;
		auto E_ii = &expr;
		if (MR_ptr) {
			auto E_ii2 = IgnoreParenImpNoopCasts(&expr, *(MR_ptr->Context));
			if (E_ii2 && (E_ii2->getType() == E_ii->getType())) {
				E_ii = E_ii2;
			} else {
				auto E_ii3 = IgnoreParenNoopCasts(&expr, *(MR_ptr->Context));
				if (E_ii3 && (E_ii3->getType() == E_ii->getType())) {
					E_ii = E_ii3;
				}
			}
		}
		auto qtype = E_ii->getType();
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		IF_DEBUG(auto qtype_str = qtype.getAsString();)
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
			if (is_raw_pointer_or_equivalent(qtype) || qtype->isReferenceType()) {
				auto CE = dyn_cast<const clang::CallExpr>(E_ii);
				if (CE) {
					const clang::FunctionDecl* function_decl = CE->getDirectCallee();
					if (function_decl) {
						process_function_lifetime_annotations(*function_decl, state1, MR_ptr, Rewrite_ptr);
						auto flta_iter = state1.m_function_lifetime_annotations_map.find(function_decl);
						if (state1.m_function_lifetime_annotations_map.end() != flta_iter) {
							auto flta = flta_iter->second;
							if (!(flta.m_return_value_lifetimes.is_empty())) {
								/* Even though (non-const) raw pointer types don't have expliicit lifetime annotations in their 
								definition (indeed they are built in types so their definition is not available at all), we will 
								consider the type to have an implied lifetime annotation when it is the return type of a function 
								that has a return value lifetime annotation. */

								retval = type_lifetime_annotations_if_available(qtype, state1, MR_ptr, Rewrite_ptr
									, true/*use_implicit_lifetime_annotation_for_pointer*/);
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
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(sf_ILE_qtype, retval);
					if (sf_ILE_qtype->isArrayType()) {
						/* We're going to have homogeneous initializer lists implicitly inherit any lifetime
						annotations of their element type. */
						auto elem_qtype = sf_ILE_qtype->getPointeeType();
						retval = type_lifetime_annotations_if_available(elem_qtype, state1, MR_ptr, Rewrite_ptr);
						return retval;
					}
				} else if (CXXILE) {
					auto sub_E_ii = CXXILE->getSubExpr();
					if (MR_ptr) {
						sub_E_ii = IgnoreParenImpNoopCasts(CXXILE->getSubExpr(), *(MR_ptr->Context));
					}
					if (sub_E_ii && (sub_E_ii != CXXILE)) {
						/* We're expecting sub_E_ii to be a clang::InitListExpr (pointer). */
						retval = type_lifetime_annotations_if_available(get_cannonical_type(sub_E_ii->getType()), state1, MR_ptr, Rewrite_ptr);
						return retval;
					}
				}
			}
			retval = type_lifetime_annotations_if_available(qtype, state1, MR_ptr, Rewrite_ptr);
		}
		return retval;
	}

	inline void slti_set_default_lower_bound_lifetimes_where_needed(CScopeLifetimeInfo1& slti, clang::QualType const& qtype) {
		MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
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
	inline void slti_set_default_lower_bound_lifetimes_where_needed(CScopeLifetimeInfo1& slti, clang::QualType const& qtype, CTUState& state1) {
		MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
		auto peeled_qtype = remove_mse_transparent_wrappers(qtype);
		IF_DEBUG(auto peeled_qtype_str = peeled_qtype.getAsString();)
		if (is_raw_pointer_or_equivalent(peeled_qtype)) {
			slti_set_default_lower_bound_lifetimes_where_needed(slti, peeled_qtype);
		} else if (peeled_qtype->isReferenceType()) {
			slti_set_default_lower_bound_lifetimes_where_needed(slti, peeled_qtype->getPointeeType(), state1);
		} else {
			if ((*(slti.m_sublifetimes_vlptr)).is_empty()) {
				auto maybe_tlta_ptr = type_lifetime_annotations_if_available(peeled_qtype, state1);
				if (maybe_tlta_ptr.has_value()) {
					auto const & tlta_cref = *(maybe_tlta_ptr.value());
					auto num_lifetimes = tlta_cref.m_lifetime_set.m_primary_lifetimes.size();

					/* If an object with associated (annotated) lifetimes doesn't already have sublifetime values,
					we'll use the primary lifetime of the object itself as a lower bound for its sublifetimes. */
					auto shallow_slti = slti;
					shallow_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
					for (auto& unused_ref : tlta_cref.m_lifetime_set.m_primary_lifetimes) {
						(*(slti.m_sublifetimes_vlptr)).m_primary_lifetime_infos.push_back(shallow_slti);
					}
				}
			}
		}
	}
	inline void slti_set_default_lower_bound_lifetimes_where_needed(CScopeLifetimeInfo1& slti, const clang::Type& type) {
		const auto qtype = clang::QualType(&type, 0/*I'm just assuming zero specifies no qualifiers*/);
		slti_set_default_lower_bound_lifetimes_where_needed(slti, qtype);
	}
	inline void slti_set_default_lower_bound_lifetimes_where_needed(CScopeLifetimeInfo1& slti, const clang::Type& type, CTUState& state1) {
		const auto qtype = clang::QualType(&type, 0/*I'm just assuming zero specifies no qualifiers*/);
		slti_set_default_lower_bound_lifetimes_where_needed(slti, qtype, state1);
	}

	/* We support assignment between annotated and non-annotated pointers. But the structure of their
	lifetime values can be different, so they may need to be adjusted as necessary to match. */
	inline void slti_set_default_lower_bound_lifetimes_to_match_where_needed(CScopeLifetimeInfo1& sli1, CScopeLifetimeInfo1& sli2) {
		auto diff1 = int(sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos.size()) - int(sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.size());
		auto& smaller_sli_ref = (0 <= diff1) ? sli1 : sli2;
		auto diff2 = abs(diff1);
		for (size_t i = 0; int(i) < diff2; i += 1) {
			auto shallow_sli = smaller_sli_ref;
			shallow_sli.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
			smaller_sli_ref.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(shallow_sli);
		}
		assert(sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.size() == sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos.size());
		for (size_t i = 0; i < sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.size(); i += 1) {
			slti_set_default_lower_bound_lifetimes_to_match_where_needed(sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos.at(i), sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos.at(i));
		}
	}

	CScopeLifetimeInfo1 scope_lifetime_info_from_lifetime_owner(const CStaticLifetimeOwnerInfo1& sloiv, ASTContext& Ctx, const CTUState& state1, bool for_lhs_of_assignment = false) {
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
				auto maybe_ltvs = for_lhs_of_assignment ? state1.corresponding_lhs_lb_lifetime_values_if_any(slov)
					: state1.corresponding_rhs_lb_lifetime_values_if_any(slov);
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
				auto maybe_ltvs = for_lhs_of_assignment ? state1.corresponding_lhs_lb_lifetime_values_if_any(slov)
					: state1.corresponding_rhs_lb_lifetime_values_if_any(slov);
				if (maybe_ltvs.has_value()) {
					lifetime_info_result = maybe_ltvs.value().m_scope_lifetime_info;
				} else {
					/* There's not much we can infer about the (scope) lifetime of an object if all we know
					is that it's the result of some expression. */
					lifetime_info_result.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
					lifetime_info_result.m_maybe_containing_scope = get_containing_scope(slov, Ctx);
					lifetime_info_result.m_maybe_source_range = slov->getSourceRange();
					lifetime_info_result.m_maybe_corresponding_cpp_element = slov;
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
	CScopeLifetimeInfo1 rhs_scope_lifetime_info_from_lifetime_owner(const CStaticLifetimeOwnerInfo1& sloiv, ASTContext& Ctx, const CTUState& state1) {
		return scope_lifetime_info_from_lifetime_owner(sloiv, Ctx, state1);
	}
	CScopeLifetimeInfo1 lhs_scope_lifetime_info_from_lifetime_owner(const CStaticLifetimeOwnerInfo1& sloiv, ASTContext& Ctx, const CTUState& state1) {
		return scope_lifetime_info_from_lifetime_owner(sloiv, Ctx, state1, true/*for_lhs_of_assignment*/);
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
		} else if (CScopeLifetimeInfo1::ECategory::ContainedDynamic == sli2.m_category) {
			/* ContainedDynamic means like an element in a dynamic container (like a vector) (that could 
			potentially be deleted at any time). */
			return false;
		} else if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli2.m_category) {
			if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli1.m_category) {
				if (sli1 == sli2) {
					return true;
				} else if ((sli1.m_maybe_containing_scope.has_value()) && (sli2.m_maybe_containing_scope.has_value())) {
					auto scope1 = sli1.m_maybe_containing_scope.value();
					const auto scope2 = sli2.m_maybe_containing_scope.value();
					if (scope2 == scope1) {
						if (sli1.m_maybe_source_range.has_value() && sli2.m_maybe_source_range.has_value()) {
							const auto SR1 = sli1.m_maybe_source_range.value();
							const auto SR2 = sli2.m_maybe_source_range.value();
							if (SR1.isValid() && SR2.isValid()) {
								if ( ((SR1.getBegin() < SR2.getBegin()) || (SR1.getBegin() == SR2.getBegin()))
									&& ((SR2.getEnd() < SR1.getEnd()) || (SR1.getEnd() == SR2.getEnd())) ) {
									/* The second item and first item are the same item, or the second item is a subexpression
									of the first item. Temporary subexpressions outlive their parent expressions so, perhaps
									unintuitively, the lifetime of parent expressions are contained within the lifetime of their
									subexpressions, even though it's the reverse with their source ranges. Right? */
									retval = true;
								} else {
									retval = false;
								}
							} else {
								retval = false;
							}
							return retval;
						} else {
							/* I don't know if there are any situations where we would know the containing scope
							of each item, but not the their source range. */
							int q = 3;
							return false;
						}
					}
					while (scope2 != scope1) {
						scope1 = get_containing_scope(scope1, context);
						if (!scope1) {
							break;
						}
					}
					retval = (scope2 == scope1);
					return retval;
				}
			} else {
				return false;
			}
		} else if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == sli1.m_category) {
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

					/* The "primary" abstract lifetimes satisfy the condition. Since an abstract lifetime represents
					a lifetime (sub)tree that includes all sublifetimes (of the "primary" lifetime), this implies 
					that all sublifetimes also satisfy the condition. So now the only way we'd consider the overall 
					condition as not satisfied is if sublifetimes are present for both arguments and (at least) one of 
					the sublifetimes does not satisfy the condition (thus contradicting the previous implication). Any 
					missing sublifetimes are assumed to be ones that would satisfy the condition, as the "primary" 
					lifetimes implied. */

					auto& sli1_primary_sublifetimes = sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos;
					auto& sli2_primary_sublifetimes = sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos;

					if (sli1_primary_sublifetimes.size() != sli2_primary_sublifetimes.size()) {
						/* The sublifetime structures don't match. In this situation we would be unable to establish
						any information that contradicts the previously determined implication that the condition is 
						satisfied. */
						return true;
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
					if (true) {
						/* Automatic lifetimes refer to (non-static/non-thread_local) local variables that never outlive the
						function call they were declared in. Abstract lifetimes available within a function are introduced 
						at the beginning of the function (or before) and are associated with lifetimes that are already 
						active when the function call is executed. So an abstract lifetime should always outlive an 
						automatic one. Right? */
						retval = true;
					} else {
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
								if (true) {
									/* So at this point we're assuming the two objects live to the end of their (common) containing
									scope, so the end of their source range doesn't correspond to the end of their lifeime. We're
									assuming the beginning of their source range does, however, correspond to the start of their
									lifetime. */
									if (SR1.getBegin() < SR2.getBegin()) {
										retval = false;
									} else {
										retval = true;
									}
								} else {
									if (SR2.getEnd() < SR1.getBegin()) {
										/* The second item occurs before the first item. */
										retval = true;
									} else if ( (SR1.getBegin() == SR2.getBegin()) && (SR1.getEnd() == SR2.getEnd()) ) {
										if (true) {
											/* We used to distinguish between the lifetime of, say, an object and a member field of that
											object, or between a container object and an element in that container. But since the
											introduced lifetime annotations are not (yet) specific enough to make the distinction, we
											will suspend making the distinction here (for now). */
											retval = true;
										} else {
											/* The second item and first item seem to have the same "scope lifetime owner". */
											auto possession_lifetime_info_chain1 = sli1.m_possession_lifetime_info_chain;
											std::reverse(possession_lifetime_info_chain1.begin(), possession_lifetime_info_chain1.end());
											auto possession_lifetime_info_chain2 = sli2.m_possession_lifetime_info_chain;
											std::reverse(possession_lifetime_info_chain2.begin(), possession_lifetime_info_chain2.end());

											auto lhs_possession_chain_size = std::max(possession_lifetime_info_chain1.size(), possession_lifetime_info_chain2.size());
											retval = true;
											for (size_t i = 0; i < lhs_possession_chain_size; i += 1) {
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

	inline bool second_is_a_sublifetime_of_first(CAbstractLifetime first, CAbstractLifetime second) {
		bool retval = false;
		if (second == first) {
			return true;
		}
		for (auto sublifetime : first.m_sublifetimes_vlptr->m_primary_lifetimes) {
			if (second_is_a_sublifetime_of_first(sublifetime, second)) {
				return true;
			}
		}
		return retval;
	}

	inline bool slti_second_can_be_assigned_to_first(const CScopeLifetimeInfo1& sli1, const CScopeLifetimeInfo1& sli2, clang::ASTContext& context, const CTUState& tu_state_cref) {
		bool retval = true;

		bool abstract_lifetimes_flag = ((CScopeLifetimeInfo1::ECategory::AbstractLifetime == sli1.m_category)
			&& (CScopeLifetimeInfo1::ECategory::AbstractLifetime == sli2.m_category));
		if (abstract_lifetimes_flag) {
			if (!(sli1.m_maybe_abstract_lifetime.has_value() && sli2.m_maybe_abstract_lifetime.has_value())) {
				assert(false);
				return false;
			}
			auto abstract1 = sli1.m_maybe_abstract_lifetime.value();
			auto abstract2 = sli2.m_maybe_abstract_lifetime.value();

			if (second_is_a_sublifetime_of_first(abstract1, abstract2)) {
				return true;
			}

			retval &= first_is_known_to_be_contained_in_scope_of_second_shallow(sli1, sli2, context, tu_state_cref);
			if (!retval) {
				return retval;
			}

			/* The "primary" abstract lifetimes satisfy the condition. Since an abstract lifetime represents
			a lifetime (sub)tree that includes all sublifetimes (of the "primary" lifetime), this implies 
			that all sublifetimes also satisfy the condition. So now the only way we'd consider the overall 
			condition as not satisfied is if sublifetimes are present for both arguments and (at least) one of 
			the sublifetimes does not satisfy the condition (thus contradicting the previous implication). Any 
			missing sublifetimes are assumed to be ones that would satisfy the condition, as the "primary" 
			lifetimes implied. */

			auto& sub_lifetime_infos1 = (sli1.m_sublifetimes_vlptr->m_primary_lifetime_infos);
			auto& sub_lifetime_infos2 = (sli2.m_sublifetimes_vlptr->m_primary_lifetime_infos);
			if (sub_lifetime_infos1.size() != sub_lifetime_infos2.size()) {
				/* The sublifetime structures don't match. In this situation we would be unable to establish
				any information that contradicts the previously determined implication that the condition is 
				satisfied. */
				return retval;
			}
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

		/* Here the lifetime values are not abstract. Unlike abstract lifetimes, relationships between 
		non-abstract lifetimes do not imply a similar relationship between their sublifetimes (if any). 
		So here actually the "primary" lifetime values of the argument objects don't matter for 
		assignment. It's the "sublifetimes" (i.e. the lifetime values of any objects being referenced),
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
			auto lhs_lifetime_info = rhs_scope_lifetime_info_from_lifetime_owner(lhs_slo.value(), Ctx, tu_state_cref);
			auto rhs_lifetime_info = rhs_scope_lifetime_info_from_lifetime_owner(rhs_slo.value(), Ctx, tu_state_cref);

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

	CScopeLifetimeInfo1 bound_lifetime_slti(CScopeLifetimeInfo1 lhs_lifetime_info, CScopeLifetimeInfo1 rhs_lifetime_info, ASTContext& Ctx, const CTUState& tu_state_cref, bool for_lhs_of_assignment = false) {
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

			auto as_sublifetime_of_tempexpr = [](const CScopeLifetimeInfo1& slti) {
				CScopeLifetimeInfo1 indirect_lifetime_value;
				indirect_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
				indirect_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(slti);
				return indirect_lifetime_value;
			};

			/* First we determine the "shallow" lower bound (without sublifetimes). */
			bool b1 = slti_second_can_be_assigned_to_first(as_sublifetime_of_tempexpr(shallow_lhs_lifetime_info), as_sublifetime_of_tempexpr(shallow_rhs_lifetime_info), Ctx, tu_state_cref);
			if (for_lhs_of_assignment) {
				b1 = !b1;
			}
			if (b1) {
				lbl = shallow_lhs_lifetime_info;
			} else {
				lbl = shallow_rhs_lifetime_info;
			}

			/* Next we determine the lower bound of each pair of corresponding sublifetimes. */
			auto lhs_sublt_iter1 = lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.begin();
			auto rhs_sublt_iter1 = rhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.begin();
			for (; lhs_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos.end() != lhs_sublt_iter1; lhs_sublt_iter1++, rhs_sublt_iter1++) {
				lbl.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(bound_lifetime_slti(*lhs_sublt_iter1, *rhs_sublt_iter1, Ctx, tu_state_cref, for_lhs_of_assignment));
			}
		}
		return lbl;
	}
	/* Given a pair of "scope lifetime info" elements, it returns a "scope lifetime info" element
	representing the lower bound of the two. The returned element may or may not be equivalent to
	one of the two given elements. */
	CScopeLifetimeInfo1 lower_bound_of_lifetimes_slti(const CScopeLifetimeInfo1& lhs_lifetime_info, const CScopeLifetimeInfo1& rhs_lifetime_info, ASTContext& Ctx, const CTUState& tu_state_cref) {
		return bound_lifetime_slti(lhs_lifetime_info, rhs_lifetime_info, Ctx, tu_state_cref);
	}
	CScopeLifetimeInfo1 upper_bound_of_lifetimes_slti(const CScopeLifetimeInfo1& lhs_lifetime_info, const CScopeLifetimeInfo1& rhs_lifetime_info, ASTContext& Ctx, const CTUState& tu_state_cref) {
		return bound_lifetime_slti(lhs_lifetime_info, rhs_lifetime_info, Ctx, tu_state_cref, true/*for_lhs_of_assignment*/);
	}

	CScopeLifetimeInfo1 bound_lifetime_slti(const std::vector<CScopeLifetimeInfo1>& lifetime_infos, ASTContext& Ctx, const CTUState& tu_state_cref, bool for_lhs_of_assignment = false) {
		CScopeLifetimeInfo1 retval;
		if (1 <= lifetime_infos.size()) {
			retval = lifetime_infos.front();
			for (const auto& lifetime_info : lifetime_infos) {
				retval = bound_lifetime_slti(retval, lifetime_info, Ctx, tu_state_cref, for_lhs_of_assignment);
			}
		}
		return retval;
	}
	/* Given a set of "scope lifetime info" elements, it returns a lower bound for all of them. The
	returned element may or may not be equivalent to one of the given elements. */
	CScopeLifetimeInfo1 lower_bound_of_lifetimes_slti(const std::vector<CScopeLifetimeInfo1>& lifetime_infos, ASTContext& Ctx, const CTUState& tu_state_cref) {
		return bound_lifetime_slti(lifetime_infos, Ctx, tu_state_cref);
	}
	CScopeLifetimeInfo1 upper_bound_of_lifetimes_slti(const std::vector<CScopeLifetimeInfo1>& lifetime_infos, ASTContext& Ctx, const CTUState& tu_state_cref) {
		return bound_lifetime_slti(lifetime_infos, Ctx, tu_state_cref, true/*for_lhs_of_assignment*/);
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

	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_rhs_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_rhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD);
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lhs_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_rhs_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_rhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::Expr* E);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lhs_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
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

	std::optional<CStaticLifetimeOwnerInfo1> rhs_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);
	CMaybeStaticLifetimeOwnerWithHints rhs_lifetime_owner_of_pointer_target_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr);

	/* This function is meant to return the part of a given expression that directly refers to the declared
	object (i.e. the `DeclRefExpr`) of interest, if it is present in the expression. The object of interest
	is the one from which we can infer the lifetime (or a lower bound of the lifetime) of the intended
	reference target (indicated by the given expression). If the intended reference target is itself a
	declared object (as opposed to, for example, a member of another object, or an element in a container),
	then it itself would be the object of interest. The object of interest could also be a (declared)
	(scope) reference/pointer to the target object, as target objects must outlive any corresponding scope
	references, so the lifetime of a (scope) reference is a lower bound for the lifetime of the
	corresponding target object. */
	CMaybeStaticLifetimeOwnerWithHints rhs_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		CMaybeStaticLifetimeOwnerWithHints retval;
		if (!EX1) {
			return retval;
		}
		const auto EX1_qtype = EX1->getType();
		if (EX1_qtype.isNull() || EX1_qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			return retval;
		}

		/* We're generally not interested in implicit nodes, but we do need to handle certain
		`ImplicitCastExpr`s. So we check if the node is an ImplicitCastExpr of a species that we're
		interested in, and if not, skip over any implicit nodes. */
		auto EX2 = IgnoreParenNoopCasts(EX1, Ctx);
		auto ICE = dyn_cast<const clang::ImplicitCastExpr>(EX2);
		const auto EX2_qtype = EX2->getType();
		if (EX2_qtype.isNull()) {
			return retval;
		}
		/* Apparently calling "EX2_qtype.getAsString()" risks segfault here. */
		//IF_DEBUG(auto EX2_qtype_str = EX2_qtype.getAsString();)
		bool condition1 = (ICE && ((clang::CastKind::CK_UncheckedDerivedToBase == ICE->getCastKind())
			|| (clang::CastKind::CK_DerivedToBase == ICE->getCastKind() || (clang::CastKind::CK_ArrayToPointerDecay == ICE->getCastKind()))));
		condition1 |= (EX2_qtype != EX1_qtype);
		if (condition1) {
			int q = 5;
		} else {
			EX2 = IgnoreParenImpNoopCasts(EX1, Ctx);
		}

		const auto EX = EX2;

		auto qtype = EX->getType();
		if (qtype.isNull() || qtype->isDependentType()) {
			return retval;
		}
		IF_DEBUG(auto qtype_str = qtype.getAsString();)

		bool satisfies_checks = false;

		auto MTE = dyn_cast<const clang::MaterializeTemporaryExpr>(IgnoreExprWithCleanups(EX1));

		auto DRE1 = dyn_cast<const clang::DeclRefExpr>(EX);
		auto CXXTE = dyn_cast<const clang::CXXThisExpr>(EX);
		auto SL = dyn_cast<const clang::StringLiteral>(EX);
		if (MTE) {
			{
				CScopeLifetimeInfo1 sloi1;

				auto const TE = MTE->getSubExpr();
				auto const TE_qtype = TE->getType();

				auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(tu_state_ref, TE, Ctx, MR_ptr, Rewrite_ptr);
				//MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
				if (maybe_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
					sloi1 = expr_slti;
				} else {
					/* We generally don't expect to get here, but if for some reason a lower bound for the argument
					lifetime isn't available, we'll just use the shortest viable lifetime. */
					sloi1.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
					sloi1.m_maybe_containing_scope = get_containing_scope(MTE, Ctx);
					sloi1.m_maybe_source_range = MTE->getSourceRange();
					sloi1.m_maybe_corresponding_cpp_element = MTE;
				}
				slti_set_default_lower_bound_lifetimes_where_needed(sloi1, TE_qtype, tu_state_ref);

				auto lifetime_extending_decl = MTE->getExtendingDecl();
				if (lifetime_extending_decl) {
					/* The expression has "scope lifetime" by virtue of being a "lifetime-extended" temporary. */
					auto le_VD = dyn_cast<const clang::VarDecl>(lifetime_extending_decl);
					if (le_VD) {
						sloi1.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
						sloi1.m_maybe_containing_scope = get_containing_scope(le_VD, Ctx);
						sloi1.m_maybe_source_range = le_VD->getSourceRange();
						sloi1.m_maybe_corresponding_cpp_element = MTE;
					}
				}

				retval = sloi1;
			}
			return retval;
		} else if (DRE1) {
			const auto DRE1_qtype = DRE1->getType();
			IF_DEBUG(const auto DRE1_qtype_str = DRE1_qtype.getAsString();)
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(DRE1_qtype, retval);

			auto D1 = DRE1->getDecl();
			const auto D1_qtype = D1->getType();
			IF_DEBUG(const auto D1_qtype_str = D1_qtype.getAsString();)
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(D1_qtype, retval);

			auto VD = dyn_cast<const clang::VarDecl>(D1);
			if (VD) {
				const auto VD_qtype = VD->getType();
				IF_DEBUG(const auto VD_qtype_str = VD_qtype.getAsString();)
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);

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
								retval = rhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
								auto maybe_res1 = rhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (maybe_res1.has_value()) {
									if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
										retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
										return retval;
									}
								}
								auto maybe_res2 = rhs_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
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
							auto maybe_tlta_ptr = type_lifetime_annotations_if_available(get_cannonical_type_ptr(RD->getTypeForDecl()), tu_state_ref);
							if (maybe_tlta_ptr.has_value()) {
								auto const & tlta = *(maybe_tlta_ptr.value());
								if (VD->hasInit()) {
									auto maybe_res1 = rhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
													auto maybe_res1 = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
																		auto maybe_res1 = rhs_lifetime_owner_if_available(arg1_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
																		if (maybe_res1.has_value()) {
																			auto param_index = int(param_lifetime1.first) - 1;
																			if ((0 <= param_index) && (int(CD->getNumParams()) > param_index)) {
																				auto PVD = CD->getParamDecl(param_index);
																				if (PVD) {
																					const auto PVD_qtype = PVD->getType();
																					MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(PVD_qtype, retval);
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
									auto maybe_res2 = rhs_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
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
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);
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
								+ "' (of type " + get_as_quoted_string_for_errmsg(VD_qtype)
								+ ") has 'static' storage duration and so may be accessible from different "
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
			auto maybe_abstract_lifetime = tu_state_ref.corresponding_abstract_lifetime_if_any(CXXTE, Ctx);
			if (maybe_abstract_lifetime) {
				CScopeLifetimeInfo1 sli1;
				const auto CS = Tget_containing_element_of_type<clang::CompoundStmt>(CXXTE, Ctx);
				if (CS) {
					sli1.m_maybe_containing_scope = CS;
				}
				sli1.m_maybe_source_range = CXXTE->getSourceRange();

				struct CB {
					CB(CScopeLifetimeInfo1 new_template_sli) : m_new_template_sli(new_template_sli) {}

					void set_sli_from_abstract_lifetime(CScopeLifetimeInfo1& sli_ref, CAbstractLifetime const& abstract_lifetime) {
						sli_ref.m_maybe_abstract_lifetime = abstract_lifetime;
						sli_ref.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;

						for (auto const& sub_abstract_lifetime : abstract_lifetime.m_sublifetimes_vlptr->m_primary_lifetimes) {
							auto new_sli = m_new_template_sli;
							set_sli_from_abstract_lifetime(new_sli, sub_abstract_lifetime);
							sli_ref.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(new_sli);
						}
					}

					CScopeLifetimeInfo1 m_new_template_sli;
				};
				auto b = CB(sli1);

				b.set_sli_from_abstract_lifetime(sli1, maybe_abstract_lifetime.value());
				if (EX2_qtype->isPointerType()) {
					auto as_sublifetime_of_tempexpr = [](const CScopeLifetimeInfo1& slti) {
						CScopeLifetimeInfo1 indirect_lifetime_value;
						indirect_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
						indirect_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(slti);
						return indirect_lifetime_value;
					};
					sli1 = as_sublifetime_of_tempexpr(sli1);
				} else {
					int q = 5;
				}

				retval = CStaticLifetimeOwnerInfo1{ sli1 };
			}
		} else if (SL) {
			retval = SL;
		} else {
			auto CSTE = dyn_cast<const clang::CastExpr>(EX);
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
			if (ME || CSTE) {
				auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(tu_state_ref, EX, Ctx, MR_ptr, Rewrite_ptr);
				if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
				} else if (maybe_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
					retval = expr_slti;
					return retval;
				}
			}
			if (false) {
				if (ME) {
					const auto VLD = ME->getMemberDecl();
					auto FD = dyn_cast<const clang::FieldDecl>(VLD);
					auto VD = dyn_cast<const clang::VarDecl>(VLD); /* for static members */
					if (FD && !(ME->isBoundMemberFunction(Ctx))) {
						auto parent_RD = FD->getParent();
						auto found_iter = tu_state_ref.m_fielddecl_to_abstract_lifetime_map.find(FD);
						if (tu_state_ref.m_fielddecl_to_abstract_lifetime_map.end() != found_iter) {
							const auto FD_qtype = FD->getType();
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(FD_qtype, retval);
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
						retval = rhs_lifetime_owner_if_available(containing_ref_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
				} else if (CSTE) {
					auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(tu_state_ref, EX, Ctx, MR_ptr, Rewrite_ptr);
					if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
					} else if (maybe_expr_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
						retval = expr_slti;
						return retval;
					}
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
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(UOSE_qtype, retval);

							if (is_raw_pointer_or_equivalent(UOSE_qtype)) {
								/* The declrefexpression is a direct dereference of a native pointer. */
								auto maybe_slo1 = rhs_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (maybe_slo1.has_value()) {
									auto sloi1 = rhs_scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);
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
							MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(UOSE_qtype, retval);

							auto maybe_slo1 = rhs_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
							if (maybe_slo1.has_value()) {
								auto sloi1 = rhs_scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);

								auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
								expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
								expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(UO, Ctx);
								expr_scope_lifetime_info.m_maybe_source_range = UO->getSourceRange();
								expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = UO;

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
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
						const auto CXXCE_rw_type_ptr = remove_mse_transparent_wrappers(qtype).getTypePtr();
						if (is_raw_pointer_or_equivalent(qtype)) {
							const auto numArgs = CXXCE->getNumArgs();
							if (1 == CXXCE->getNumArgs()) {
								const auto arg_EX = CXXCE->getArg(0);
								assert(arg_EX);

								retval = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								if (retval.has_value()) {
									/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
									retval.value().m_possession_lifetime_info_chain.push_back({});
								}
							}
						}
					} else if (CO) {
						auto res1 = rhs_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
						auto res2 = rhs_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
									retval = rhs_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
									return retval;
								}
								{
									const clang::FunctionDecl* function_decl = CE->getDirectCallee();
									if (function_decl) {
										function_call_handler2(tu_state_ref, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr);
										auto eltv_iter = tu_state_ref.m_expr_rhs_lb_lifetime_values_map.find(CE);
										if (tu_state_ref.m_expr_rhs_lb_lifetime_values_map.end() != eltv_iter) {
											auto const& esli = eltv_iter->second.m_scope_lifetime_info;
											retval = eltv_iter->second.m_scope_lifetime_info;
											return retval;
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
												CMaybeStaticLifetimeOwnerWithHints maybe_lblo = rhs_lifetime_owner_if_available(potential_rv_source_args.front(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
												for (auto arg_EX : potential_rv_source_args) {
													auto maybe_lblo2 = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
										MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(arg_EX_qtype, retval);

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
												retval = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
														retval = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
									MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(CXXMCE_qtype, retval);
									if (contains_non_owning_scope_reference(CXXMCE_qtype, tu_state_ref)) {
										auto maybe_lb_lifetime_owner = rhs_lifetime_owner_of_returned_reference_object_if_available(CXXMCE, Ctx, tu_state_ref);
										if (maybe_lb_lifetime_owner.has_value()) {
											return maybe_lb_lifetime_owner;
										}
									}
								}
							} else if (CE) {
								std::string function_qname;
								auto FND = CE->getDirectCallee();
								if (FND) {
									function_qname = FND->getQualifiedNameAsString();
								}

								static const std::string std_move_str = "std::move";
								if ((std_move_str == function_qname) && (1 == CE->getNumArgs())) {
									return rhs_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
								}

								static const std::string function_get_str = "std::get";
								if (((function_get_str == function_qname)) && (1 == CE->getNumArgs())) {
									potential_owner_EX = IgnoreParenImpNoopCasts(CE->getArg(0), Ctx);
								} else {
									const auto CE_qtype = CE->getType();
									MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(CE_qtype, retval);
									if (contains_non_owning_scope_reference(CE_qtype, tu_state_ref)) {
										auto maybe_lb_lifetime_owner = rhs_lifetime_owner_of_returned_reference_object_if_available(CE, Ctx, tu_state_ref);
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
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(potential_owner_EX_ii_qtype, retval);

								const auto CXXRD = remove_mse_transparent_wrappers(potential_owner_EX_ii_qtype).getTypePtr()->getAsCXXRecordDecl();
								if (CXXRD) {
									/* static structure containers */
									DECLARE_CACHED_CONST_STRING(xscope_owner_ptr_str, mse_namespace_str() + "::TXScopeOwnerPointer");

									static const std::string std_unique_ptr_str = "std::unique_ptr";
									static const std::string std_tuple_str = "std::tuple";
									static const std::string std_pair_str = "std::pair";
									static const std::string std_array_str = "std::array";

									DECLARE_CACHED_CONST_STRING(mstd_tuple_str, mse_namespace_str() + "::mstd::tuple");
									DECLARE_CACHED_CONST_STRING(xscope_tuple_str, mse_namespace_str() + "::xscope_tuple");
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
										retval = rhs_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
										if (retval.has_value()) {
											/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
											retval.value().m_possession_lifetime_info_chain.push_back({});
										}
									} else if ((std_tuple_str == qname) || (std_pair_str == qname) || (std_array_str == qname)

										|| (mstd_tuple_str == qname) || (xscope_tuple_str == qname) || (nii_array_str == qname) || (mstd_array_str == qname) || (xscope_nii_array_str == qname)
										|| (fixed_nii_vector_str == qname) || (xscope_fixed_nii_vector_str == qname) || (xscope_borrowing_fixed_nii_vector_str == qname)
										|| (fixed_nii_basic_string_str == qname) || (xscope_fixed_nii_basic_string_str == qname) || (xscope_borrowing_fixed_nii_basic_string_str == qname)
										) {
										retval = rhs_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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

	std::optional<CStaticLifetimeOwnerInfo1> rhs_lifetime_owner_of_returned_reference_object_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		std::optional<CStaticLifetimeOwnerInfo1> retval;
		if (!EX1) {
			return retval;
		}
		const auto EX1_qtype = EX1->getType();
		if (EX1_qtype.isNull() || EX1_qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			return retval;
		}

		/* We're generally not interested in implicit nodes, but we do need to handle certain
		`ImplicitCastExpr`s. So we check if the node is an ImplicitCastExpr of a species that we're
		interested in, and if not, skip over any implicit nodes. */
		auto EX2 = IgnoreParenNoopCasts(EX1, Ctx);
		auto ICE = dyn_cast<const clang::ImplicitCastExpr>(EX2);
		const auto EX2_qtype = EX2->getType();
		if (EX2_qtype.isNull()) {
			return retval;
		}
		/* Apparently calling "EX2_qtype.getAsString()" risks segfault here. */
		//IF_DEBUG(auto EX2_qtype_str = EX2_qtype.getAsString();)
		bool condition1 = (ICE && ((clang::CastKind::CK_UncheckedDerivedToBase == ICE->getCastKind())
			|| (clang::CastKind::CK_DerivedToBase == ICE->getCastKind() || (clang::CastKind::CK_ArrayToPointerDecay == ICE->getCastKind()))));
		condition1 |= (EX2_qtype != EX1_qtype);
		if (condition1) {
			int q = 5;
		} else {
			EX2 = IgnoreParenImpNoopCasts(EX1, Ctx);
		}

		const auto EX = EX2;

		auto qtype = EX->getType();
		if (qtype.isNull() || qtype->isDependentType()) {
			return retval;
		}
		IF_DEBUG(auto qtype_str = qtype.getAsString();)

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
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(arg_EX_ii_qtype, retval);
				if (contains_non_owning_scope_reference(arg_EX_ii_qtype, tu_state_ref)) {
					lifetime_owners.push_back(rhs_lifetime_owner_if_available(CXXMCE->getArg(i), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
				}
			}
			lifetime_owners.push_back(rhs_lifetime_owner_if_available(CXXMCE->getImplicitObjectArgument(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
			retval = lower_bound_lifetime_owner(lifetime_owners, Ctx, tu_state_ref);
		} else if (CE) {
			IF_DEBUG(auto function_qname = CE->getDirectCallee() ? CE->getDirectCallee()->getQualifiedNameAsString() : std::string("");)

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
					lifetime_owners.push_back(rhs_lifetime_owner_if_available(CE->getArg(i), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr));
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

	/* Similar to `rhs_lifetime_owner_if_available()`, this function is meant to return the part of a given
	expression that directly refers to the declared object (i.e. the `DeclRefExpr`) of interest, if such an
	object is present. Unlike rhs_lifetime_owner_if_available(), the given expression is presumed to
	indicate the (scope) reference/pointer object to be retargeted, rather than the target object. So the
	object of interest in this case is the one from which we can infer the lifetime (or an upper bound of the
	lifetime) of the reference object to be retargeted. If the indicated reference object is itself a declared
	object (as opposed to, for example, a member of another object, or an element in a container), then it
	itself would be the object of interest. */
	CMaybeStaticLifetimeOwnerWithHints lhs_lifetime_owner_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		CMaybeStaticLifetimeOwnerWithHints retval;
		if (!EX1) {
			return retval;
		}
		const auto EX1_qtype = EX1->getType();
		if (EX1_qtype.isNull() || EX1_qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			return retval;
		}

		/* We're generally not interested in implicit nodes, but we do need to handle certain
		`ImplicitCastExpr`s. So we check if the node is an ImplicitCastExpr of a species that we're
		interested in, and if not, skip over any implicit nodes. */
		auto EX2 = IgnoreParenNoopCasts(EX1, Ctx);
		auto ICE = dyn_cast<const clang::ImplicitCastExpr>(EX2);
		const auto EX2_qtype = EX2->getType();
		if (EX2_qtype.isNull()) {
			return retval;
		}
		/* Apparently calling "EX2_qtype.getAsString()" risks segfault here. */
		//IF_DEBUG(auto EX2_qtype_str = EX2_qtype.getAsString();)
		bool condition1 = (ICE && ((clang::CastKind::CK_UncheckedDerivedToBase == ICE->getCastKind())
			|| (clang::CastKind::CK_DerivedToBase == ICE->getCastKind() || (clang::CastKind::CK_ArrayToPointerDecay == ICE->getCastKind()))));
		condition1 |= (EX2_qtype != EX1_qtype);
		if (condition1) {
			int q = 5;
		} else {
			EX2 = IgnoreParenImpNoopCasts(EX1, Ctx);
		}

		const auto EX = EX2;

		auto qtype = EX->getType();
		if (qtype.isNull() || qtype->isDependentType()) {
			return retval;
		}
		IF_DEBUG(auto qtype_str = qtype.getAsString();)


		auto maybe_lb_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(tu_state_ref, EX, Ctx, MR_ptr, Rewrite_ptr);
		if (maybe_lb_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
			/* Cannot properly evaluate because this is a template definition. Proper evaluation should
			occur in any instantiation of the template. */
		} else if (maybe_lb_expr_lifetime_value.has_value()) {
			CScopeLifetimeInfo1& expr_slti = maybe_lb_expr_lifetime_value.value().m_scope_lifetime_info;
			if (CScopeLifetimeInfo1::ECategory::AbstractLifetime == expr_slti.m_category) {
				/* If the lifetime of a pointer's target is abstract, it can be used as the ("upper bound")
				lifetime of the lhs of a (pointer) assignment. Right? */
				retval = expr_slti;
				return retval;
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

						auto maybe_slo1 = lhs_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref);
						if (maybe_slo1.has_value()) {
							auto sloi1 = lhs_scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);

							auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
							expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
							expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(UO, Ctx);
							expr_scope_lifetime_info.m_maybe_source_range = UO->getSourceRange();
							expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = UO;

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
								auto maybe_decl_lifetime_value = evaluate_declaration_lhs_lower_bound_lifetimes(tu_state_ref, VD, Ctx, MR_ptr, Rewrite_ptr);
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
											retval = lhs_lifetime_owner_if_available(init_E, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
											if (retval.has_value()) {
												return retval;
											}
										}
									}
								}
							}
						}


						auto maybe_slo1 = rhs_lifetime_owner_if_available(UOSE, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
						if (maybe_slo1.has_value()) {
							auto sloi1 = lhs_scope_lifetime_info_from_lifetime_owner(maybe_slo1.value(), Ctx, tu_state_ref);
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
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);

					CScopeLifetimeInfo1 var_shallow_slti;
					var_shallow_slti.m_category = (clang::StorageDuration::SD_Automatic == storage_duration) 
						? CScopeLifetimeInfo1::ECategory::Automatic : CScopeLifetimeInfo1::ECategory::Immortal;
					var_shallow_slti.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
					var_shallow_slti.m_maybe_source_range = VD->getSourceRange();

					CScopeLifetimeInfo1 var_default_slti = var_shallow_slti;
					slti_set_default_lower_bound_lifetimes_where_needed(var_default_slti, VD_qtype, tu_state_ref);

					auto maybe_tlta = type_lifetime_annotations_if_available(*VD, tu_state_ref, MR_ptr, Rewrite_ptr);
					if (maybe_tlta.has_value()) {
						if (maybe_lb_expr_lifetime_value.has_value()) {
							auto& lbe_slti = maybe_lb_expr_lifetime_value.value().m_scope_lifetime_info;
							retval = lbe_slti;
							return retval;
						} else {
							/* We seem to be unable to determine an upper bound lifetime. */
						}
					} else {
						if (VD_qtype->isReferenceType()) {
							satisfies_checks = false;
							retval = {};
							auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
							if (PVD) {
								/* This appears to be a reference parameter. But we previously determined that it doesn't seem to
								have an associated abstract lifetime. In this situation we can't really determine an upper bound 
								lifetime (for the object the reference is bound to). */
								if (false) {
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
								}
								return retval;
							} else {
								if (VD->hasInit()) {
									retval = lhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref);
									return retval;
								}
							}
						} else {
							/* So at this point the variable is not a reference and is of a type that does not have lifetime
							annottions. */
							retval = VD;
							retval = var_default_slti;

							if (false) {
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
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);

								if (VD_qtype->isPointerType()) {
									if (VD->hasInit()) {
										auto maybe_res1 = rhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
										if (maybe_res1.has_value()) {
											if (maybe_res1.value().m_maybe_sublifetime_owners_vlptr.has_value()) {
												retval.value().m_maybe_sublifetime_owners_vlptr = maybe_res1.value().m_maybe_sublifetime_owners_vlptr;
												return retval;
											}
										}
										auto maybe_res2 = rhs_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
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
									auto maybe_tlta_ptr = type_lifetime_annotations_if_available(get_cannonical_type_ptr(RD->getTypeForDecl()), tu_state_ref);
									if (maybe_tlta_ptr.has_value()) {
										auto const & tlta = *(maybe_tlta_ptr.value());
										if (VD->hasInit()) {
											auto maybe_res1 = rhs_lifetime_owner_if_available(VD->getInit(), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
															auto maybe_res1 = rhs_lifetime_owner_if_available(arg_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
																				auto maybe_res1 = rhs_lifetime_owner_if_available(arg1_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
																				if (maybe_res1.has_value()) {
																					auto param_index = int(param_lifetime1.first) - 1;
																					if ((0 <= param_index) && (int(CD->getNumParams()) > param_index)) {
																						auto PVD = CD->getParamDecl(param_index);
																						if (PVD) {
																							auto PVD_qtype = PVD->getType();
																							IF_DEBUG(auto PVD_qtype_str = PVD_qtype.getAsString();)
																							MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(PVD_qtype, retval);
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
											auto maybe_res2 = rhs_lifetime_owner_of_pointer_target_if_available(VD->getInit(), Ctx, tu_state_ref);
											if (maybe_res2.has_value()) {
												retval.value().m_maybe_sublifetime_owners_vlptr = { maybe_res2.value() };
											}
											*/
											return retval;
										}
									}
								}
							}
						}
						return retval;
					}
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
								+ "' (of type " + get_as_quoted_string_for_errmsg(VD_qtype)
								+ ") has 'static' storage duration and so may be accessible from different "
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
			auto CSTE = dyn_cast<const clang::CastExpr>(EX);
			auto ME = dyn_cast<const clang::MemberExpr>(EX);
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

					retval = lhs_lifetime_owner_if_available(containing_ref_EX, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
					if (retval.has_value()) {
						auto& retval_slo_ref = retval.value();

						/* Here we're noting that the returned lifetime is of the "owner" of the item, not exactly the item itself. */
						retval_slo_ref.m_possession_lifetime_info_chain.push_back({ FD->getSourceRange() });
						CScopeLifetimeInfo1 owner_slti = lhs_scope_lifetime_info_from_lifetime_owner(retval_slo_ref, Ctx, tu_state_ref);

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

						auto maybe_tlta = type_lifetime_annotations_if_available(containing_qtype, tu_state_ref, MR_ptr, Rewrite_ptr);
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

							auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(FD);
							if (maybe_abstract_lifetime_set.has_value()) {
								auto& abstract_lifetime_set1 = maybe_abstract_lifetime_set.value();
								/* The member field's type also seems to have (zero or more) annotated lifetimes. Presumably, its
								abstract lifetimes are a subset of its parent/containing object's abstract lifetimes. */

								/* For each of the member field's abstract lifetimes, we'll look for the corresponding abstract
								lifetime of the parent/containing object, and its corresponding evaluated (non-abstract) value,
								if available, and assign that value to the member field accordingly. */
								for (auto const& field_abstract_lifetime1 : abstract_lifetime_set1.m_primary_lifetimes) {
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

						retval = expr_slti;
						return retval;
					}
				} else if (VD) {
					retval = VD; /* static member */
				} else {
					int q = 5;
				}
			} else if (CSTE) {
				const auto sub_E_ip = IgnoreParenNoopCasts(CSTE->getSubExpr(), Ctx);
				if (sub_E_ip && (sub_E_ip != CSTE)) {
					auto CSTE_qtype = CSTE->getType();
					IF_DEBUG(auto CSTE_qtype_str = CSTE_qtype.getAsString();)
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(CSTE_qtype, retval);

					auto cast_kind = CSTE->getCastKind();
					IF_DEBUG(auto cast_kind_str = CSTE->getCastKindName();)
					auto cf_ND = CSTE->getConversionFunction();

					auto maybe_expr_slo = lhs_lifetime_owner_if_available(sub_E_ip, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
					if (maybe_expr_slo.has_value()) {
						auto& src_slo = maybe_expr_slo.value();
						retval = src_slo;

						auto maybe_tlta_ptr = type_lifetime_annotations_if_available(*CSTE, tu_state_ref);
						if (maybe_tlta_ptr.has_value()) {
							auto const & tlta = *(maybe_tlta_ptr.value());
							/* The result type of the cast has abstract (annotated) lifetimes associated with it. We cannot in
							general assume that any (sub)lifetime values of the source object correspond to those of the
							resulting object. */

							/* Until we determine the lifetime values corresponding to those abstract lifetimes, we cannot
							establish an upper bound for the object's lifetime(s). */
							retval = {};

							auto maybe_ltvs = tu_state_ref.corresponding_lhs_lb_lifetime_values_if_any(CSTE);
							if (maybe_ltvs.has_value()) {
								/* The lifetime values of the resulting object seem to have been already evaluated elsewhere. */
								/* Does this ever happen? */
								auto& ltvs = maybe_ltvs.value().m_scope_lifetime_info;
								retval = { ltvs };
							} else if ((clang::CastKind::CK_UncheckedDerivedToBase == cast_kind) || (clang::CastKind::CK_DerivedToBase == cast_kind)) {
								/* This particular (implicit) cast seems to be just using an object as its base class. This is
								similar to using a member field of an object via member expression, and so we handle it in
								similar fashion. */
								auto sub_E_ip_qtype = sub_E_ip->getType();
								IF_DEBUG(auto sub_E_ip_qtype_str = sub_E_ip_qtype.getAsString();)
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(sub_E_ip_qtype, retval);

								auto CXXRD = sub_E_ip_qtype->getAsCXXRecordDecl();
								if (CXXRD) {
									for (auto& CXXBS : CXXRD->bases()) {
										auto base_qtype = CXXBS.getType();
										IF_DEBUG(auto base_qtype_str = base_qtype.getAsString();)
										MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(base_qtype, retval);

										auto base_cnqtype = base_qtype.getCanonicalType(); base_cnqtype.removeLocalConst();
										IF_DEBUG(auto base_cnqtype_str = base_cnqtype.getAsString();)
										auto CSTE_cnqtype = CSTE_qtype.getCanonicalType(); CSTE_cnqtype.removeLocalConst();
										IF_DEBUG(auto CSTE_cnqtype_str = CSTE_cnqtype.getAsString();)

										if (base_cnqtype == CSTE_cnqtype) {
											auto iter1 = tu_state_ref.m_base_class_to_abstract_lifetime_map.find(&CXXBS);
											if (tu_state_ref.m_base_class_to_abstract_lifetime_map.end() != iter1) {
												auto owner_slti = lhs_scope_lifetime_info_from_lifetime_owner(src_slo, Ctx, tu_state_ref);
												auto const& owner_type_lifetime_values = owner_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos;

												/* expr_stli will (hopefully) be the returned lifetime value of the base class. We will start
												by initializing it with the lifetime value of its parent/containing object. */
												CScopeLifetimeInfo1 expr_slti = owner_slti;

												CPossessionLifetimeInfo1 pli;
												pli.m_maybe_field_source_range = CXXBS.getSourceRange();
												/* Here we're adding info about the base class's relationship to the parent from which its lifetime
												value is based. */
												expr_slti.m_possession_lifetime_info_chain.push_back(pli);

												/* While the base class's primary lifetime is based on its parent/containing object, its
												sublifetimes are, in general, not. We still need to evaluate them. */
												expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();

												auto maybe_tlta = type_lifetime_annotations_if_available(*sub_E_ip, tu_state_ref, MR_ptr, Rewrite_ptr);
												if (maybe_tlta.has_value()) {
													/* The parent/containing type seems to have (zero or more) annotated lifetimes. */

													auto CXXTE = dyn_cast<const clang::CXXThisExpr>(sub_E_ip);
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

													auto maybe_abstract_lifetime_set = tu_state_ref.corresponding_abstract_lifetime_set_if_any(&CXXBS);
													if (maybe_abstract_lifetime_set.has_value()) {
														auto& abstract_lifetime_set1 = maybe_abstract_lifetime_set.value();
														/* The base class's type also seems to have (zero or more) annotated lifetimes. Presumably, its
														abstract lifetimes are a subset of its parent/containing object's abstract lifetimes. */

														/* For each of the base class's abstract lifetimes, we'll look for the corresponding abstract
														lifetime of the parent/containing object, and its corresponding evaluated (non-abstract) value,
														if available, and assign that value to the base class accordingly. */
														for (auto const& base_class_abstract_lifetime1 : abstract_lifetime_set1.m_primary_lifetimes) {
															auto owner_tlv_iter1 = owner_type_lifetime_values.begin();
															auto owner_talt_iter1 = owner_type_abstract_lifetimes.begin();
															for (; (owner_type_abstract_lifetimes.end() != owner_talt_iter1) && (owner_type_lifetime_values.end() != owner_tlv_iter1)
																; ++owner_talt_iter1, ++owner_tlv_iter1) {

																if (base_class_abstract_lifetime1 == (*owner_talt_iter1)) {
																	expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(*owner_tlv_iter1);
																	break;
																}
															}
														}
														if (expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.size() == abstract_lifetime_set1.m_primary_lifetimes.size()) {
															retval = { expr_slti };
														}
													}
													int q = 5;
												}

												return retval;
											}
											break;
										}
									}
								}
							} else {
								/* The resulting object of the cast has associated (annotated) lifetimes. Unfortunately we haven't
								been able to determine the values of those lifetimes. (And we can't just assume they're related to
								any lifetime values of the source object.) */
								retval = {};
							}
							return retval;
						}
					}
					int q = 5;
				}
				return retval;
			} else {
				{
					auto CO = dyn_cast<const clang::ConditionalOperator>(EX);
					if (CO) {
						auto res1 = lhs_lifetime_owner_if_available(CO->getTrueExpr(), Ctx, tu_state_ref);
						auto res2 = lhs_lifetime_owner_if_available(CO->getFalseExpr(), Ctx, tu_state_ref);
						return upper_bound_lifetime_owner(res1, res2, Ctx, tu_state_ref);
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
									retval = lhs_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
									return retval;
								}
								{
									const clang::FunctionDecl* function_decl = CE->getDirectCallee();
									if (function_decl) {
										function_call_handler2(tu_state_ref, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr);
										auto eltv_iter = tu_state_ref.m_expr_lhs_lb_lifetime_values_map.find(CE);
										if (tu_state_ref.m_expr_lhs_lb_lifetime_values_map.end() != eltv_iter) {
											auto const& esli = eltv_iter->second.m_scope_lifetime_info;
											retval = eltv_iter->second.m_scope_lifetime_info;
											return retval;
										} else {
											retval = CE;
										}
									} else {
										retval = CE;
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
										retval = lhs_lifetime_owner_if_available(CE->getArg(0), Ctx, tu_state_ref);
										return retval;
									}
								}
							}
							if (potential_owner_EX) {
								auto potential_owner_EX_ii = IgnoreParenImpNoopCasts(potential_owner_EX, Ctx);
								if (potential_owner_EX_ii) {
									const auto potential_owner_EX_ii_qtype = potential_owner_EX_ii->getType();
									IF_DEBUG(const auto potential_owner_EX_ii_qtype_str = potential_owner_EX_ii_qtype.getAsString();)
									MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(potential_owner_EX_ii_qtype, retval);

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
										DECLARE_CACHED_CONST_STRING(xscope_tuple_str, mse_namespace_str() + "::xscope_tuple");
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
											retval = lhs_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref);
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
											|| (mstd_tuple_str == qname) || (xscope_tuple_str == qname)
											|| (nii_array_str == qname) || (mstd_array_str == qname)
											|| (nii_vector_str == qname) || (stnii_vector_str == qname) || (mtnii_vector_str == qname)
											|| (mstd_vector_str == qname)
											*/
											) {
											retval = lhs_lifetime_owner_if_available(potential_owner_EX_ii, Ctx, tu_state_ref);
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
		auto res1 = rhs_lifetime_owner_if_available(EX1, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
			const auto DirectCallee = CE->getDirectCallee();
			if (DirectCallee) {
				auto function_qname = DirectCallee->getQualifiedNameAsString();

				static const std::string std_addressof_str = "std::addressof";
				if ((std_addressof_str == function_qname) && (1 == CE->getNumArgs())) {
					retval = CE->getArg(0);
				}
			}
		}

		return retval;
	}

	CMaybeStaticLifetimeOwnerWithHints rhs_lifetime_owner_of_pointer_target_if_available(const clang::Expr* EX1, ASTContext& Ctx, CTUState& tu_state_ref, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		auto res1 = rhs_lifetime_owner_if_available(raw_pointer_target_expression_if_available(EX1, Ctx, tu_state_ref), Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
		if (res1.has_value()) {
			return res1;
		} else {
			/* The lifetime owner of the pointer target directly is not available. But the lifetime of
			the pointer itself serves as a lower bound for the lifetime of its target. */
			auto retval = rhs_lifetime_owner_if_available(EX1, Ctx, tu_state_ref, MR_ptr, Rewrite_ptr);
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
		auto res1 = rhs_lifetime_owner_of_pointer_target_if_available(EX1, Ctx, tu_state_ref);
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
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
				if (qtype->isReferenceType()) {
					if (clang::StorageDuration::SD_Automatic != VD->getStorageDuration()) {
						const std::string error_desc = std::string("Native references (such as those of type ")
							+ get_as_quoted_string_for_errmsg(qtype)
							+ ") that are not local variables (or function parameters) are not supported.";
						(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
								+ "native reference (of type " + get_as_quoted_string_for_errmsg(qtype) + ") is safe here.";
							const auto hints_str = res1.hints_str();
							if (!hints_str.empty()) {
								error_desc += " (" + hints_str + ")";
							} else {
								error_desc += " (Possibly due to being unable to verify that the target object outlives the (scope) reference.)";
							}
							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

			/* Handling of this situation has been moved to function_call_handler2(). */
			if (false && (CE != nullptr)/* && (DRE != nullptr)*/)
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
									MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(MTE_qtype);
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
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

				if (!ST->getRetValue()) {
					return;
				}
				auto E = dyn_cast<const clang::Expr>(ST->getRetValue());
				if (!E) {
					return;
				}
				auto E_qtype = E->getType();
				IF_DEBUG(auto E_qtype_str = E_qtype.getAsString();)
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(E_qtype);

				bool rv_lifetime_annotation_is_present = false;

				auto FND = Tget_containing_element_of_type<clang::FunctionDecl>(E, *(MR.Context));
				if (!FND) {
					return;
				}
				auto return_qtype = FND->getReturnType();
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(return_qtype);
				IF_DEBUG(auto return_qtype_str = return_qtype.getAsString();)
				auto return_type_ptr = return_qtype.getTypePtr();
				if (!return_type_ptr) {
					return;
				}
				auto& return_type_ref = *return_type_ptr;

				if (is_xscope_type(return_qtype, (*this).m_state1)
					&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type(return_qtype))
					) {

					process_function_lifetime_annotations(*FND, m_state1, &MR, &Rewrite);
					auto flta_iter = m_state1.m_function_lifetime_annotations_map.find(FND);
					if (m_state1.m_function_lifetime_annotations_map.end() != flta_iter) {
						if (!(flta_iter->second.m_return_value_lifetimes.is_empty())) {
							rv_lifetime_annotation_is_present = true;
							bool satisfies_checks = false;
							auto rv_abstract_lifetimes = flta_iter->second.m_return_value_lifetimes;
							auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(MR, Rewrite, m_state1, E);
							MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value);
							if (maybe_expr_lifetime_value.has_value()) {
								auto &lbsli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

								CScopeLifetimeInfo1 rvsli;
								if (!(return_qtype->isReferenceType())) {
									/* If the return type is not a reference type, then the value returned by the function is 
									conceptually a (temporary) copy of the return value expression. The function's return value
									lifetime annotations apply to this temporary copy, so we can use the 
									slti_second_can_be_assigned_to_first() function to verify that the return value expression 
									can be safely assigned to the temporary copy given the copy's annotated (abstract) lifetime 
									values. */
									rvsli.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
									*(rvsli.m_sublifetimes_vlptr) = rv_abstract_lifetimes;
								} else {
									if (1 < rv_abstract_lifetimes.m_primary_lifetimes.size()) {
										/* unexpected, references should not have more than one annotated primary lifetime */
										int q = 3;
									}

									rvsli.m_category = CScopeLifetimeInfo1::ECategory::AbstractLifetime;
									if (1 <= rv_abstract_lifetimes.m_primary_lifetimes.size()) {
										rvsli.m_maybe_abstract_lifetime = rv_abstract_lifetimes.m_primary_lifetimes.front();
									} else {
										int q = 3;
									}

									/* If, on the other hand, the return type is a reference type, then there is no conceptual 
									temporary copy. The returned reference directly targets the return value expression, so 
									applying the slti_second_can_be_assigned_to_first() function directly (to the (annotated) 
									returned reference and the return value expression) will not reliably verify conformance as 
									the slti_second_can_be_assigned_to_first() function ignores the direct lifetimes and only 
									considers the sublifetimes. However, if we add a level of indirection to the lifetime 
									values (of the (annotated) returned reference and the return value expression), as if we
									were returning a pointer to the thing we're actually returning, then we can use the 
									slti_second_can_be_assigned_to_first() function to verify conformance. */
									{
										CScopeLifetimeInfo1 rvsli2;
										rvsli2.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
										*(rvsli2.m_sublifetimes_vlptr) = { rvsli };
										rvsli = rvsli2;
									}
									{
										CScopeLifetimeInfo1 lbsli2;
										lbsli2.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
										*(lbsli2.m_sublifetimes_vlptr) = { lbsli };
										lbsli = lbsli2;
									}
								}
								satisfies_checks = slti_second_can_be_assigned_to_first(rvsli, lbsli, *(MR.Context), m_state1);
							}
							if (!satisfies_checks) {
								std::string error_desc = std::string("Unable to verify that the return value")
									+ " (of type " + get_as_quoted_string_for_errmsg(return_qtype) + ") conforms to the lifetime specified.";

								assert(1 <= rv_abstract_lifetimes.m_primary_lifetimes.size());
								if (rv_abstract_lifetimes.m_primary_lifetimes.front().m_is_elided) {
									error_desc += std::string(" (The specified return value lifetime seems to be an implicit/'elided' one. If this elided")
									+ " return value lifetime does match the intended one, an explicit return value lifetime annotation, or"
									+ " the 'lifetime_no_elided' annotation can be used to prevent the elided lifetime from being applied.)";
								}

								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
							}
							return;
						}
					}
					if (rv_lifetime_annotation_is_present) {
						return;
					}

					/* The SaferCPlusPlus library has "transparent" template wrappers such as mse::rsv::TReturnableFParam<>
					which were used to to help ensure the safety of reference parameters (that might be used in the return 
					value). (This tool largely renders that functionality redundant, but anyway they still exist.) When 
					these template wrappers are used with function parameters, the return value needs to be passed through
					the mse::return_value() "transparent" function. Here we check for any violations of this rule. */
					bool has_an_mse_fparam_parameter = false;
					for (auto& PVD : FND->parameters()) {
						auto qtype = PVD->getType();
						if (qtype.isNull()) {
							continue;
						}
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
								continue;
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
										//return remove_fparam_wrappers(ta_qtype);

										has_an_mse_fparam_parameter = true;
										break;
									} else {
										/*unexpected*/
										int q = 5;
									}
								}
							}
						}
						if (has_an_mse_fparam_parameter) {
							break;
						}
					}
					if (has_an_mse_fparam_parameter) {
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
							+ return_qtype.getAsString() + "'), in functions that have 'FParam<>' parameters, need to be "
							+ "wrapped in the mse::return_value() function wrapper.";

							auto FND = enclosing_function_if_any(ST->getRetValue(), *(MR.Context));
							if (FND) {
								if (FND->isDefaulted()) {
									error_desc += " (This return statement is contained in the (possibly implicit/default member) function '" + FND->getNameAsString() + "' .)";
								}
							}

							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
						+ "native array subscripts) is not supported. (The expression in question here is of type "
						+ get_as_quoted_string_for_errmsg(EX->getType()) + ".)";
					(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
						(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

				bool errors_suppressed_by_location_flag = errors_suppressed_by_location(MR, SR.getBegin());
				auto suppress_check_flag = errors_suppressed_by_location_flag;
				suppress_check_flag |= m_state1.m_suppress_check_region_set.contains(VD, Rewrite, *(MR.Context));
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
						(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
					}
					*/
					const auto* EX = VD->getInit();
					if (EX) {
						bool null_initialization = is_nullptr_literal(EX, *(MR.Context));

						if (null_initialization && (!suppress_check_flag)) {
							const std::string error_desc = std::string("Null initialization of ")
								+ "native pointers (such as those of type " + get_as_quoted_string_for_errmsg(qtype)
								+ ") is not supported.";
							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
						}
					} else {
						auto *PVD = dyn_cast<const ParmVarDecl>(VD);
						if (!PVD) {
							if ((!VD->isExternallyDeclarable()) && (!suppress_check_flag)) {
								const std::string error_desc = std::string("Uninitialized ")
									+ "native pointer variables are not supported.";
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
					do {
						if (clang::CK_IntegralToPointer == CSTE->getCastKind()) {
							cast_type_str = "Integral-to-pointer";
							break;
						}
						if (clang::CK_NoOp == CSTE->getCastKind()) {
							/* If the cast is a no-op, I guess we won't complain. */
							//cast_type_str = "";
							break;
						}

						auto const * const CSCE = dyn_cast<const CStyleCastExpr>(EXii);
						if (CSCE) {
							cast_type_str = "'C-style'";
							break;
						}
						auto const * const CXXRCE = dyn_cast<const CXXReinterpretCastExpr>(EXii);
						if (CXXRCE) {
							cast_type_str = "Reinterpret";
							break;
						}
						auto const * const CXXCCE = dyn_cast<const CXXConstCastExpr>(EXii);
						if (CXXCCE) {
							cast_type_str = "Const";
							break;
						}
						{
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
					} while (false);
				}
				if ("" != cast_type_str) {
					assert(CSTE);
					const std::string error_desc = cast_type_str
						+ " casts are not supported (in expression of type "
						+ get_as_quoted_string_for_errmsg(CSTE->getType()) + ").";
					(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

			if (false && ((CXXMCE != nullptr) || (CXXOCE != nullptr)))
			{
				/* This case is now handled in function_call_handler2(). */

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
						method_decl = dyn_cast<const clang::CXXMethodDecl>(method_decl1);
					}
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
						state1.register_error(*MR.SourceManager, SR, error_desc);
					}
				} else {
					static const std::string tilda_str = "~";
					if (string_begins_with(method_name, tilda_str)) {
						const std::string error_desc =  std::string("'") + method_name
							+ "' looks like a destructor. Explicitly calling destructors is not supported.";
						state1.register_error(*MR.SourceManager, SR, error_desc);
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
						state1.register_error(*MR.SourceManager, SR, error_desc);
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
		auto qtype = VD->getType();
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, retval);
		retval = qtype.isConstQualified();
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

	bool has_trivial_move_constructor(const clang::QualType& qtype) {
		bool retval = false;

		auto CXXRD = qtype->getAsCXXRecordDecl();
		if (CXXRD) {
			/*** left off here ***/
		} else {
			retval = true;
		}
		return retval;
	}

	auto remove_reference_qtype = [](clang::QualType const& qtype) {
		MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype, qtype);
		if (qtype->isReferenceType()) {
			return qtype->getPointeeType();
		}
		return qtype;
	};

	template<class _Fn, class... Args>
	static bool check_all_explicit_parameter_qtypes(const clang::FunctionDecl* function_decl, _Fn _Func, const Args&... args) {
		if (!function_decl) {
			return false;
		}
		bool retval = true;

		for (auto const PVD : function_decl->parameters()) {
			if (PVD) {
				auto PVD_qtype = PVD->getType();
				IF_DEBUG(const std::string PVD_qtype_str = PVD_qtype.getAsString();)
				if (!_Func(remove_reference_qtype(PVD_qtype), args...)) {
					retval = false;
					break;
				}
			}
		}
		return retval;
	}
	template<class _Fn, class... Args>
	static bool check_the_specified_explicit_parameter_qtype(size_t param_index, const clang::FunctionDecl* function_decl, _Fn _Func, const Args&... args) {
		if (!function_decl) {
			return false;
		}
		if (!(function_decl->getNumParams() > param_index)) {
			return false;
		}
		bool retval = true;
		auto const PVD = function_decl->getParamDecl(param_index);
		{
			if (PVD) {
				auto PVD_qtype = PVD->getType();
				IF_DEBUG(const std::string PVD_qtype_str = PVD_qtype.getAsString();)
				if (!_Func(remove_reference_qtype(PVD_qtype), args...)) {
					retval = false;
					//break;
				}
			}
		}
		return retval;
	}
	template<class _Fn, class... Args>
	static bool check_implicit_parameter_qtype(const clang::FunctionDecl* function_decl, _Fn _Func, const Args&... args) {
		if (!function_decl) {
			return false;
		}
		bool retval = true;

		auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
		if (CXXMD) {
			/* The function seems to be a member function so it would have an implict `this` parameter. */
			auto CXXRD = CXXMD->getParent();
			if (CXXRD) {
				auto TypePtr = CXXRD->getTypeForDecl();
				if (TypePtr) {
					auto PVD_qtype = clang::QualType(TypePtr, 0/*I'm just assuming zero specifies no qualifiers*/);
					IF_DEBUG(const std::string PVD_qtype_str = PVD_qtype.getAsString();)
					if (!_Func(remove_reference_qtype(PVD_qtype), args...)) {
						retval = false;
						//break;
					}
				}
			} else {
				int q = 3;
			}
		}
		return retval;
	}
	template<class _Fn, class... Args>
	static bool check_all_implicit_and_explicit_parameter_qtypes(const clang::FunctionDecl* function_decl, _Fn _Func, const Args&... args) {
		bool retval = check_implicit_parameter_qtype(function_decl, _Func, args...);
		if (retval) {
			retval = check_all_explicit_parameter_qtypes(function_decl, _Func, args...);
		}
		return retval;
	}

	template<typename TCallOrConstuctorExpr>
	inline bool is_known_to_never_deallocate_its_arguments(CTUState& state1, const clang::FunctionDecl* function_decl
		, const TCallOrConstuctorExpr* CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		/* We're just using rough heuristics for the moment. This needs to be done 
		properly at some point. */

		bool retval = false;

		if (CE == nullptr) {
			return retval;
		}
		if (!function_decl) {
			return retval;
		}

		const std::string function_name = function_decl->getNameAsString();
		const std::string qfunction_name = function_decl->getQualifiedNameAsString();

		auto CXXMCE = dyn_cast<clang::CXXMemberCallExpr>(CE);
		auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);

		auto CXXOCE = dyn_cast<clang::CXXOperatorCallExpr>(CE);
		auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
		auto CXXCD = dyn_cast<const clang::CXXConstructorDecl>(function_decl);
		if (CXXCE) {
			int q = 5;
		}

		bool std_flag = string_begins_with(qfunction_name, "std::");
		bool mse_flag = std_flag ? false : string_begins_with(qfunction_name, "mse::");
		if (std_flag || mse_flag) {
			/* So in general we wouldn't know whether a user-defined function might deallocate
			an object targeted by one of its (implicit or explicit) reference parameters 
			(before the end of the function call). But we can probably assume that standard 
			library code doesn't generally do that, unless it invokes (mischievous) 
			user-defined code. */

			struct CB {
				static auto& standard_functions_that_may_invoke_a_user_defined_code() {
					/* Some standard functions explicitly take a user-provided predicate. But also, 
					some functions (many from `<algorithm>`) could invoke a user-defined comparison
					operator, or iterator increment operator, etc. */
					static const auto l_sc_standard_functions_that_may_invoke_a_user_defined_code = std::vector<std::string>{
						"sort"
						, "for_each"
						, "find", "find_if", "find_if_not"
						, "begin", "end", "cbegin", "cend"
						,
					};
					return l_sc_standard_functions_that_may_invoke_a_user_defined_code;
				}
				static auto& non_invoking_standard_functions() {
					static const auto l_sc_non_invoking_standard_functions = std::vector<std::string>{
						"operator*"
						, "operator->"
						, "operator[]"
						, "value"
						, "at"
						, "front"
						, "move"
						, "get"
						, "addressof"
						, "operator&"
					};
					return l_sc_non_invoking_standard_functions;
				}
				static bool is_known_to_be_well_behaved(clang::QualType qtype, const ASTContext &Ctx) {
					IF_DEBUG(const std::string qtype_str = qtype.getAsString();)
					bool retval = false;
					if (qtype.isTrivialType(Ctx)) {
						return true;
					} else if (is_recognized_nonowning_container(qtype)) {
						return true;
					} else {
						auto b2 = is_recognized_owning_container(qtype);
						if (b2) {
							auto template_args = shallow_template_arg_types_if_any(qtype);
							if (1 <= template_args.size()) {
								auto first_targ_qtype = template_args.front();
								retval = is_known_to_be_well_behaved(first_targ_qtype, Ctx);
							} else {
								int q = 3;
							}
						}
					}
					return retval;
				}
				static bool is_known_to_have_benign_move(clang::QualType qtype, const ASTContext &Ctx) {
					IF_DEBUG(const std::string qtype_str = qtype.getAsString();)
					bool retval = false;
					/* Unfortunately there is no `isTriviallyMovableType()` that is only concerned with
					move constructors. And "trivially copyable" does not only require that the copy 
					constructor be trivial, but also the move constructor and the copy and move 
					assignment operators, and the destructor. Apparently. */
					if (qtype.isTriviallyCopyableType(Ctx)) {
						return true;
					} else if (is_recognized_benign_move_dynamic_owning_container(qtype)) {
						return true;
					} else if (is_recognized_nonowning_container(qtype)) {
						return true;
					} else {
						auto b2 = is_recognized_owning_container(qtype);
						if (b2) {
							auto template_args = shallow_template_arg_types_if_any(qtype);
							if (1 <= template_args.size()) {
								auto first_targ_qtype = template_args.front();
								/*  */
								retval = is_known_to_have_benign_move(first_targ_qtype, Ctx);
							} else {
								int q = 3;
							}
						}
					}
					return retval;
				}
				static bool is_known_to_have_benign_copy(clang::QualType qtype, const ASTContext &Ctx) {
					IF_DEBUG(const std::string qtype_str = qtype.getAsString();)
					bool retval = false;
					/* In C++, "trivially copyable" does not only require that the copy 
					constructor be trivial, but also the move constructor and the copy and move 
					assignment operators, and the destructor. Apparently. */
					if (qtype.isTriviallyCopyableType(Ctx)) {
						return true;
					} else if (is_recognized_dynamic_owning_pointer(qtype)) {
						return true;
					} else if (is_recognized_nonowning_container(qtype)) {
						return true;
					} else {
						auto b2 = is_recognized_owning_container(qtype);
						if (b2) {
							auto template_args = shallow_template_arg_types_if_any(qtype);
							if (1 <= template_args.size()) {
								auto first_targ_qtype = template_args.front();
								/*  */
								retval = is_known_to_have_benign_copy(first_targ_qtype, Ctx);
							} else {
								int q = 3;
							}
						}
					}
					return retval;
				}
			};

			auto matches_one_of = [](std::string_view needle, std::vector<std::string> const& haystack) {
				for (auto const & straw : haystack) {
					if (needle == straw) {
						return true;
					}
				}
				return false;
			};

			if (matches_one_of(function_name, CB::non_invoking_standard_functions())) {
				return true;
			}

			if (matches_one_of(function_name, CB::standard_functions_that_may_invoke_a_user_defined_code())) {
				/* todo: maybe be more selective at some point */
				return false;
			}

			if (("operator<<" == function_name) || ("operator>>" == function_name)) {
				/* Potentially standard streaming operator. Should be safe, right? */
				auto is_recognized_string_container_or_char_star_or_char = [](clang::QualType const& qtype) {
					auto const qtype_str = qtype.getAsString();
					if (("const char *" == qtype_str) || ("char *" == qtype_str) 
						|| ("const char" == qtype_str) || ("char" == qtype_str)) {
						return true;
					}
					return is_recognized_string_container(qtype);
				};
				auto res1 = check_the_specified_explicit_parameter_qtype(1/*index of 2nd parameter*/, function_decl, is_recognized_string_container_or_char_star_or_char);
				if (res1) {
					return true;
				}
			}

			/* Probably the most common invocation of user-defined code by the standard 
			libraries are (copy and move) constructors. So we check all the function 
			parameter types, and see if we can verify that all their constructors and
			destructors are known to be well-behaved (or trivial). */

			if ((CXXCD && CXXCD->isMoveConstructor()) || (CXXMD && CXXMD->isMoveAssignmentOperator())){
				/* Here we check to see if all the parameter types are known to have benign move
				constructors and move assignment operators. If so, this move operation should be safe. */

				retval = check_all_explicit_parameter_qtypes(function_decl, CB::is_known_to_have_benign_move, Ctx);
				if (retval) {
					return retval;
				}
			}

			if ((CXXCD && CXXCD->isCopyConstructor()) || (CXXMD && CXXMD->isCopyAssignmentOperator())){
				/* Here we check to see if all the parameter types are known to have benign copy
				constructors and copy assignment operators. If so, this copy operation should be safe. */

				retval = check_all_explicit_parameter_qtypes(function_decl, CB::is_known_to_have_benign_copy, Ctx);
				if (retval) {
					return retval;
				}
			}

			/* In the move and copy operations above we know that that the only user provided 
			code that might be invoked are the move/copy constructors or assignment operators.
			In other functions in the standard libraries, it might be the case that a default 
			constructor could be invoked? So we'll just make sure those are trivial or 
			well-behaved as well. */

			retval = check_all_implicit_and_explicit_parameter_qtypes(function_decl, CB::is_known_to_be_well_behaved, Ctx);
		}

		return retval;
	}

	template<typename TCallOrConstuctorExpr>
	inline bool is_known_to_never_deallocate_its_return_value(CTUState& state1, const clang::FunctionDecl* function_decl
		, const TCallOrConstuctorExpr* CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		/* For now we'll use the set of functions that are known_to_never_deallocate_its_arguments
		to approximation for the the set of functions that are known_to_never_deallocate_its_return_value. */
		return is_known_to_never_deallocate_its_arguments(state1, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr);
	}

	template<typename TCallOrConstuctorExpr>
	inline std::string function_call_handler2(CTUState& state1, const clang::FunctionDecl* function_decl
		, const TCallOrConstuctorExpr* CE, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		if (CE == nullptr) {
			return {};
		}
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
			auto eltv_iter = state1.m_expr_rhs_lb_lifetime_values_map.find(CE);
			bool needs_processing = (state1.m_expr_rhs_lb_lifetime_values_map.end() == eltv_iter);
			/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
			needs_processing = (needs_processing || ((!(eltv_iter->second.m_errors_noted)) && bool(MR_ptr)));
			if (!needs_processing) {
				/* Already processed (and any errors noted). */
				return {};
			}
		}

		auto raw_SR = CE->getSourceRange();
		auto SR = Rewrite_ptr ? nice_source_range(raw_SR, *Rewrite_ptr) : raw_SR;

		//bool filtered_out_by_location_flag = filtered_out_by_location(Ctx, SR.getBegin());

		bool errors_suppressed_by_location_flag = errors_suppressed_by_location(Ctx, SR.getBegin());
		bool errors_suppressed_flag = errors_suppressed_by_location_flag;
		if ((!errors_suppressed_flag) && Rewrite_ptr) {
			errors_suppressed_flag = state1.m_suppress_check_region_set.contains(CE, *Rewrite_ptr, Ctx);
		}

		if (!function_decl) {
			return {};
		}

		IF_DEBUG(const std::string function_name = function_decl->getNameAsString();)
		const std::string qfunction_name = function_decl->getQualifiedNameAsString();

		auto function_declSR = Rewrite_ptr ? nice_source_range(function_decl->getSourceRange(), *Rewrite_ptr)
			: function_decl->getSourceRange();
		SourceLocation function_declSL = function_declSR.getBegin();
#ifndef NDEBUG
		std::string debug_function_decl_source_location_str;
		if (Rewrite_ptr) {
			DEBUG_SET_SOURCE_LOCATION_STR(debug_function_decl_source_location_str, function_declSR, *Rewrite_ptr);
		}
#endif /*!NDEBUG*/

		static const std::vector<std::string> ignore_qnames = { "__assert_fail" };
		for (auto const& ignore_qname : ignore_qnames) {
			if (ignore_qname == qfunction_name) {
				return {};
			}
		}

		/* Here we're going to instantiate one or more "element_being_analyzed_info scope objects". These 
		objects help keep track of the path in the AST tree we followed to arrive at the AST node we are 
		currently analyzing. This path will be reported, in some form, as part any error messages to help 
		inform the user of which parts of the code contribute to the manifestation of the error. */

		std::vector<CTUState::CElementBeingAnalyzedInfoScopeObj> extra_ebai_scope_objs;
		{
			/* Here we note/report the call stack (i.e. the list of containing/ancestor call expressions). */
			auto l_CE = Tget_containing_element_of_type<clang::CallExpr>(CE, Ctx);
			while (l_CE) {
				auto l_CESR = l_CE->getSourceRange();
				std::string str1;
				if (MR_ptr) {
					str1 = l_CESR.getBegin().printToString(*(MR_ptr->SourceManager));
				}
				if (!errors_suppressed_by_location(Ctx, l_CESR.getBegin())) {
					extra_ebai_scope_objs.push_back(state1.make_element_being_analyzed_info_scope_obj({l_CESR, CE, "called"}));
				}

				l_CE = Tget_containing_element_of_type<clang::CallExpr>(l_CE, Ctx);
			}
		}
		auto FTD = function_decl->getPrimaryTemplate();
		if (FTD) {
			/* This function seems to be a function template. Here we note/report the template instantiation stack. */
			auto FND = function_decl;
			const auto instantiation_FND = FND->getTemplateInstantiationPattern();
			if (instantiation_FND != FND) {
				FND = instantiation_FND;

				while (FND) {
					auto FNDSR = FND->getSourceRange();
					std::string str1;
					if (MR_ptr) {
						str1 = FNDSR.getBegin().printToString(*(MR_ptr->SourceManager));
					}
					if (true || !errors_suppressed_by_location(Ctx, FNDSR.getBegin())) {
						extra_ebai_scope_objs.push_back(state1.make_element_being_analyzed_info_scope_obj({FNDSR, CE, "function template instantiated"}));
					}

					auto FTD = function_decl->getPrimaryTemplate();
					if (FTD) {
						const auto instantiation_FND = FND->getTemplateInstantiationPattern();
						if (instantiation_FND == FND) {
							break;
						}
						FND = instantiation_FND;
					} else {
						break;
					}
				}
			}
		}
		/* Here we note/report the declaration of the function. */
		extra_ebai_scope_objs.push_back(state1.make_element_being_analyzed_info_scope_obj({function_declSR, CE, "function declared"}));
		/* Here we note/report the call expression. */
		auto ebai_scope_obj = state1.make_element_being_analyzed_info_scope_obj({SR, CE});

		auto CXXMCE = dyn_cast<clang::CXXMemberCallExpr>(CE);
		auto CXXCE = dyn_cast<clang::CXXConstructExpr>(CE);

		auto CXXOCE = dyn_cast<clang::CXXOperatorCallExpr>(CE);
		auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
		auto CXXCD = dyn_cast<const clang::CXXConstructorDecl>(function_decl);


		/* First some preliminary checks that don't involve annotated lifetimes. */

		if (CXXMCE) {
			const auto CXXDD = dyn_cast<const clang::CXXDestructorDecl>(function_decl);
			if (CXXDD) {
				if (MR_ptr && (!errors_suppressed_flag)) {
					const std::string error_desc =  std::string("Explicitly calling destructors (such as '") + qfunction_name
						+ "') is not supported.";
					state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
				}
			}
		} else if (false) {
			static const std::string tilda_str = "~";
			if (string_begins_with(function_name, tilda_str)) {
				if (MR_ptr && (!errors_suppressed_flag)) {
					const std::string error_desc =  std::string("'") + qfunction_name
						+ "' looks like a destructor. Explicitly calling destructors is not supported.";
					state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
				}
			}
		}

		auto lifetime_of_the_function_call = CScopeLifetimeInfo1{};
		lifetime_of_the_function_call.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
		lifetime_of_the_function_call.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
		lifetime_of_the_function_call.m_maybe_source_range = CE->getSourceRange();
		lifetime_of_the_function_call.m_maybe_corresponding_cpp_element = CE;

		std::vector<param_ordinal_t> param_ordinals;
		bool is_ns_member_operator = false;
		if (CXXMD && (!(CXXMD->isStatic())) && (!CXXCD)) {
			if (CXXMD->isOverloadedOperator()) {
				is_ns_member_operator = true;
			}
			auto param_ordinal = is_ns_member_operator ? param_ordinal_t(param_ordinal_t::ns_member_operator_tag{}, IMPLICIT_THIS_PARAM_ORDINAL)
				: param_ordinal_t(IMPLICIT_THIS_PARAM_ORDINAL);
			param_ordinals.push_back(param_ordinal);
		}
		const auto num_params = function_decl->getNumParams();
		for (int i = 0; i < int(num_params); i += 1) {
			auto param_ordinal = is_ns_member_operator ? param_ordinal_t(param_ordinal_t::ns_member_operator_tag{}, 1 + i)
				: param_ordinal_t(1 + i);
			param_ordinals.push_back(param_ordinal);
		}

		for (auto& param_ordinal : param_ordinals) {
			clang::Expr const * arg_EX = arg_from_param_ordinal(CE, param_ordinal);
			if ((!arg_EX) || arg_EX->isDefaultArgument()) {
				continue;
			}
			auto maybe_param_qtype = qtype_from_param_ordinal_if_available(CE, param_ordinal);
			if (!(maybe_param_qtype.has_value())) {
				continue;
			}
			auto param_qtype = maybe_param_qtype.value();
			IF_DEBUG(auto param_qtype_str = param_qtype.getAsString();)

			auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(state1, arg_EX, Ctx, MR_ptr, Rewrite_ptr);
			MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, {});
			if (maybe_expr_lifetime_value.has_value()) {

				auto& expr_lifetime_value_ref = maybe_expr_lifetime_value.value();

				CScopeLifetimeInfo1Set lifetime_values;

				if (param_qtype->isReferenceType() || (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal)) {
					/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
					same as the object it refers to (without an added level of indirection). */
					lifetime_values.m_primary_lifetime_infos.push_back(expr_lifetime_value_ref.m_scope_lifetime_info);
				} else {
					CScopeLifetimeInfo1& arg_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
					/* Here we set the evaluated expression sublifetimes. */
					lifetime_values = *(arg_slti.m_sublifetimes_vlptr);
				}
				for (auto& lifetime_value : lifetime_values.m_primary_lifetime_infos) {
					auto shallow_lifetime_value = lifetime_value;
					shallow_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
					if (!first_is_known_to_be_contained_in_scope_of_second_shallow(lifetime_of_the_function_call, shallow_lifetime_value, Ctx, state1)) {
						if (!is_known_to_never_deallocate_its_arguments(state1, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr)) {
							if (MR_ptr && (!errors_suppressed_flag)) {
								std::string arg_str;
								if (IMPLICIT_THIS_PARAM_ORDINAL == param_ordinal) {
									arg_str += std::string(" 'this' pointer");
									//arg_str += std::string(" 'implicit object argument' referenced by the 'this' pointer");
								} else {
									arg_str += std::string(" argument passed to the parameter");
									auto maybe_param_name = name_from_param_ordinal_if_available(CE, param_ordinal);
									if (maybe_param_name.has_value()) {
										const auto& param_name = maybe_param_name.value();
										arg_str += std::string(" '") + param_name + "'";
									}
								}
								std::string function_species_str = (CXXOCE) ? "operator" :
									((CXXMCE) ? "member function" : (CXXCE ? "constructor" : "function"));
								if (CXXCE) {
									auto CXXCD = CXXCE->getConstructor();
									if (CXXCD) {
										std::string new_function_species_str;
										if (!(CXXCD->isExplicit())) {
											new_function_species_str += "(implicit) ";
										}
										if ((CXXCD->isCopyConstructor())) {
											new_function_species_str += "copy ";
										} else if ((CXXCD->isMoveConstructor())) {
											new_function_species_str += "move ";
										}
										new_function_species_str += "constructor";
										function_species_str = new_function_species_str;
									}
								}

								auto arg_SR = arg_EX->getSourceRange();

								const std::string error_desc = std::string("Unable to verify that the")
									+ arg_str + " (of type " + get_as_quoted_string_for_errmsg(param_qtype) + ") outlives the "
									+ function_species_str + " call '" + function_decl->getQualifiedNameAsString()
									+ "'. (This is often addressed by obtaining a scope pointer/reference to the intended argument."
									+ " If the argument is a reference to an element in a dynamic container, you might instead access"
									+ " the element via a corresponding non-dynamic/'fixed' interface object which borrows (exclusive"
									+ " access to) the container's contents.)";
								state1.register_error(*(MR_ptr->SourceManager), arg_SR, error_desc);
							}
						}
					}
				}
			}
		}

		if (2 <= function_decl->getNumParams()) {
			auto first_PVD = function_decl->getParamDecl(0);
			auto second_PVD = function_decl->getParamDecl(1);
			auto qtype1 = first_PVD->getType();
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype1, {});
			IF_DEBUG(auto qtype_str = qtype1.getAsString();)
			auto qtype2 = second_PVD->getType();
			MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(qtype2, {});
			IF_DEBUG(auto qtype2_str = qtype2.getAsString();)
			auto qtype = remove_reference(qtype1);
			if ((qtype1 == qtype2) && (qtype->isPointerType())) {
				/* The first two parameters are pointers of the same type. We want to disallow 
				passing pointer iterators to algorithm functions.  */
				static const std::string std_qsort_str = "std::qsort";
				static const std::string std_sort_str = "std::sort";
				DECLARE_CACHED_CONST_STRING(mse_sort_str, mse_namespace_str() + "::sort");
				static const std::string std_for_each_str = "std::for_each";
				DECLARE_CACHED_CONST_STRING(mse_for_each_str, mse_namespace_str() + "::for_each");
				DECLARE_CACHED_CONST_STRING(mse_for_each_ptr_str, mse_namespace_str() + "::for_each_ptr");
				static const std::string std_equal_str = "std::equal";
				DECLARE_CACHED_CONST_STRING(mse_equal_str, mse_namespace_str() + "::equal");
				/* prefixes */
				static const std::string std_find_str = "std::find";
				DECLARE_CACHED_CONST_STRING(mse_find_str, mse_namespace_str() + "::find");
				std::string pointer_iterator_unsupported_function_str;
				if ((std_qsort_str == qfunction_name)
					|| (std_sort_str == qfunction_name)
					|| (mse_sort_str == qfunction_name)
					|| (std_for_each_str == qfunction_name)
					|| (mse_for_each_str == qfunction_name)
					|| (mse_for_each_ptr_str == qfunction_name)
					|| (std_equal_str == qfunction_name)
					|| (mse_equal_str == qfunction_name)
					|| (string_begins_with(qfunction_name, std_find_str))
					|| (string_begins_with(qfunction_name, mse_find_str))
					) {

					pointer_iterator_unsupported_function_str = qfunction_name;
				}
				if (MR_ptr && ("" != pointer_iterator_unsupported_function_str)) {
					auto& MR = *MR_ptr;
					const std::string error_desc = std::string("Passing raw pointer (iterators) (of type ") 
						+ get_as_quoted_string_for_errmsg(qtype) + ") to the '" + pointer_iterator_unsupported_function_str
						+ "' function is not supported.";
					state1.register_error(*MR.SourceManager, SR, error_desc);
				}
			}
		}


		/* Here we're going to try to evaluate and store the lifetimes of (the resulting value of)
		this (call) expression. We're presuming that the direct lifetime of any expression is a
		"temporary lifetime". But the expression may have other associated lifetimes defined by
		lifetime annotations on the type of the expression. */

		struct CLifetimeMaybeValueMappings : public std::vector<std::pair<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > > {
			typedef std::vector<std::pair<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > > base_class;
			using base_class::base_class;
			auto find(const CAbstractLifetime& alt) {
				auto iter = (*this).begin();
				for (; (*this).end() != iter; iter++) {
					if (alt == iter->first) {
						break;
					}
				}
				return iter;
			}
		};
		CLifetimeMaybeValueMappings CE_type_rhs_maybe_lifetime_value_mappings;
		CLifetimeMaybeValueMappings CE_type_lhs_maybe_lifetime_value_mappings;

		std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > IOA_lt1_constraint_shptrs;
		CScopeLifetimeInfo1Set expr_rhs_scope_sublifetimes;
		CScopeLifetimeInfo1Set expr_lhs_scope_sublifetimes;

		IF_DEBUG(const auto FD_qtype_str = function_decl->getType().getAsString();)
		IF_DEBUG(const auto retval_qtype_str = function_decl->getReturnType().getAsString();)
		IF_DEBUG(const auto CE_qtype_str = CE->getType().getAsString();)
		const auto CE_TypePtr1 = CE->getType().getTypePtr();

		auto as_sublifetime_of_tempexpr = [](const CScopeLifetimeInfo1& slti) {
			CScopeLifetimeInfo1 indirect_lifetime_value;
			indirect_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
			indirect_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(slti);
			return indirect_lifetime_value;
		};

		if (CXXCE) {
			auto maybe_tla_ptr = type_lifetime_annotations_if_available(*CE, state1, MR_ptr, Rewrite_ptr);
			if (maybe_tla_ptr.has_value()) {
				auto& tla_ref = *(maybe_tla_ptr.value());

				IOA_lt1_constraint_shptrs = tla_ref.m_lifetime_constraint_shptrs;

				for (auto& abstract_lifetime : tla_ref.m_lifetime_set.m_primary_lifetimes) {
					CE_type_rhs_maybe_lifetime_value_mappings.push_back({ abstract_lifetime, {} });
					CE_type_lhs_maybe_lifetime_value_mappings.push_back({ abstract_lifetime, {} });
				}
			}
		}

		if (CXXCE && CXXCE->getConstructor() && CXXCE->getConstructor()->isCopyOrMoveConstructor()) {
			if (1 == CE->getNumArgs()) {
				auto lambda1 = [&](bool for_lhs_of_assignment = false) -> CMaybeExpressionLifetimeValuesWithHints {
					IF_DEBUG(auto arg_qtype_str = CE->getArg(0)->getType().getAsString();)

					auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, CE->getArg(0), Ctx, MR_ptr, Rewrite_ptr)
						: evaluate_expression_rhs_lower_bound_lifetimes(state1, CE->getArg(0), Ctx, MR_ptr, Rewrite_ptr);
					MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
					if (maybe_expr_lifetime_value.has_value()) {
						auto& expr_lifetime_value_ref = maybe_expr_lifetime_value.value();
						auto CE_qtype = CE->getType();
						IF_DEBUG(auto CE_qtype_str = CE_qtype.getAsString();)
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(CE_qtype, {});

						auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
						expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
						expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
						expr_scope_lifetime_info.m_maybe_source_range = CE->getSourceRange();
						expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = CE;

						if (CE_qtype->isReferenceType()) {
							/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
							same as the object it refers to (without an added level of indirection). */
							expr_scope_lifetime_info = expr_lifetime_value_ref.m_scope_lifetime_info;
						} else {
							CScopeLifetimeInfo1& arg_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
							/* Here we set the evaluated expression sublifetimes. */
							*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = *(arg_slti.m_sublifetimes_vlptr);
						}
						/* Here we put the evaluated expression lifetimes in "persistent" storage. */
						auto& expr_lb_lifetime_values_map = for_lhs_of_assignment ? state1.m_expr_lhs_lb_lifetime_values_map: state1.m_expr_rhs_lb_lifetime_values_map;
						expr_lb_lifetime_values_map.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
					}
					return {};
				};

				auto rhs_res = lambda1(false/*for_lhs_of_assignment*/);
				MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(rhs_res, {});

				auto lhs_res = lambda1(true/*for_lhs_of_assignment*/);
				MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(lhs_res, {});
				return {};
			} else {
				int q = 3;
			}
		} else {
			process_function_lifetime_annotations(*function_decl, state1, MR_ptr, Rewrite_ptr);
			auto flta_iter = state1.m_function_lifetime_annotations_map.find(function_decl);
			if (state1.m_function_lifetime_annotations_map.end() != flta_iter) {
				auto flta = flta_iter->second;

				if (!(flta.m_return_value_lifetimes.is_empty())) {
					CE_type_rhs_maybe_lifetime_value_mappings.clear();
					CE_type_lhs_maybe_lifetime_value_mappings.clear();
					for (auto& abstract_lifetime : flta.m_return_value_lifetimes.m_primary_lifetimes) {
						CE_type_rhs_maybe_lifetime_value_mappings.push_back({ abstract_lifetime, {} });
						CE_type_lhs_maybe_lifetime_value_mappings.push_back({ abstract_lifetime, {} });
					}
				}

				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > initialized_rhs_maybe_lifetime_value_map(CE_type_rhs_maybe_lifetime_value_mappings.begin(), CE_type_rhs_maybe_lifetime_value_mappings.end());
				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > initialized_lhs_maybe_lifetime_value_map(CE_type_lhs_maybe_lifetime_value_mappings.begin(), CE_type_lhs_maybe_lifetime_value_mappings.end());
				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > present_rhs_maybe_lifetime_value_map(CE_type_rhs_maybe_lifetime_value_mappings.begin(), CE_type_rhs_maybe_lifetime_value_mappings.end());
				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > present_lhs_maybe_lifetime_value_map(CE_type_lhs_maybe_lifetime_value_mappings.begin(), CE_type_lhs_maybe_lifetime_value_mappings.end());

				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > IOA_type_rhs_maybe_lifetime_value_map;
				std::unordered_map<CAbstractLifetime, std::optional<CScopeLifetimeInfo1> > IOA_type_lhs_maybe_lifetime_value_map;
				std::optional<CScopeLifetimeInfo1> maybe_this_rhs_slti;
				std::optional<CScopeLifetimeInfo1> maybe_this_lhs_slti;

				if (CXXMCE || (CXXOCE && (1 <= CXXOCE->getNumArgs()))) {

#ifndef NDEBUG
					auto fdnp1 = function_decl ? function_decl->getNumParams() : 99;
					auto mdnp1 = CXXMD ? CXXMD->getNumParams() : 99;
					auto cena1 = CE ? CE->getNumArgs() : 99;
					auto mcena1 = CXXMCE ? CXXMCE->getNumArgs() : 99;
					auto ocena1 = CXXOCE ? CXXOCE->getNumArgs() : 99;
#endif /*!NDEBUG*/

					/* This is a member function call. */
					auto IOA_E = CXXMCE ? CXXMCE->getImplicitObjectArgument() : CXXOCE->getArg(0);

					IF_DEBUG(const auto IOA_qtype_str = IOA_E->getType().getAsString();)
					const auto IOA_TypePtr1 = IOA_E->getType().getTypePtr();

					auto maybe_tla_ptr = type_lifetime_annotations_if_available(*IOA_E, state1, MR_ptr, Rewrite_ptr);
					if (!(maybe_tla_ptr.has_value())) {
						if (IOA_TypePtr1) {
							auto TD = IOA_TypePtr1->getAsTagDecl();
							if (TD) {
								process_type_lifetime_annotations(*TD, state1, MR_ptr, Rewrite_ptr);
							}
						}
						maybe_tla_ptr = type_lifetime_annotations_if_available(*IOA_E, state1, MR_ptr, Rewrite_ptr);
					}

					if (maybe_tla_ptr.has_value()) {
						auto& tla_ref = *(maybe_tla_ptr.value());

						IOA_lt1_constraint_shptrs = tla_ref.m_lifetime_constraint_shptrs;

						auto& IOA_type_lifetime_set_cref = tla_ref.m_lifetime_set;

						if (0 != IOA_type_lifetime_set_cref.m_primary_lifetimes.size()) {
							auto lambda1 = [&](bool for_lhs_of_assignment = false) -> CMaybeExpressionLifetimeValuesWithHints {
								auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr)
									: evaluate_expression_rhs_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr);
								MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
								if (maybe_expr_lifetime_value.has_value()) {
									auto &this_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									(for_lhs_of_assignment ? maybe_this_lhs_slti : maybe_this_rhs_slti) = this_slti;

									/* The actual lifetime values for this object are available. */
									auto sublifetime_values_ptr = &(this_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos);
									auto& tmp1_sublifetime_values_ref = this_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos;
									auto sltv_size1 = (*sublifetime_values_ptr).size();

									auto default_sublifetime_values = std::vector<CScopeLifetimeInfo1>{ IOA_type_lifetime_set_cref.m_primary_lifetimes.size(), this_slti };

									if ((*sublifetime_values_ptr).size() != IOA_type_lifetime_set_cref.m_primary_lifetimes.size()) {
										/* For some reason the number of evaluated lifetime values does not match the number of declared lifetimes. */
										if (0 != (*sublifetime_values_ptr).size()) {
											if (MR_ptr && (!errors_suppressed_flag)) {
												std::string error_desc = std::string("Unable to deduce all the lifetime values associated with the implicit object (of type ") + get_as_quoted_string_for_errmsg(IOA_E->getType()) + ").";
												state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
											}
										}
										sublifetime_values_ptr = &default_sublifetime_values;
									}
									auto& sublifetime_values_ref = *sublifetime_values_ptr;

									const auto IOA_type_abstract_lifetime_end_iter = IOA_type_lifetime_set_cref.m_primary_lifetimes.cend();
									auto IOA_type_abstract_lifetime_iter1 = IOA_type_lifetime_set_cref.m_primary_lifetimes.cbegin();
									const auto IOA_lifetime_values_end_iter = sublifetime_values_ref.end();
									auto IOA_lifetime_values_iter1 = sublifetime_values_ref.begin();
									/* Here we are iterating over each of the object type's (annotated) abstract lifetimes. */
									for (; (IOA_type_abstract_lifetime_end_iter != IOA_type_abstract_lifetime_iter1) && (IOA_lifetime_values_end_iter != IOA_lifetime_values_iter1)
										; IOA_type_abstract_lifetime_iter1++, IOA_lifetime_values_iter1++) {

										/* Here we're adding a mapping between each abstract lifetime (of the object's type) and the actual lifetime
										value (assigned at object initialization) associated with it. */
										auto& IOA_type_maybe_lifetime_value_map = for_lhs_of_assignment ? IOA_type_lhs_maybe_lifetime_value_map : IOA_type_rhs_maybe_lifetime_value_map;
										IOA_type_maybe_lifetime_value_map.insert_or_assign( *IOA_type_abstract_lifetime_iter1, *IOA_lifetime_values_iter1 );
									}
								}
								return {};
							};

							auto rhs_res = lambda1(false/*for_lhs_of_assignment*/);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(rhs_res, {});

							auto lhs_res = lambda1(true/*for_lhs_of_assignment*/);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(lhs_res, {});
						}
					} else {
						int q = 3;
					}
				}
				for (auto const& item : IOA_type_rhs_maybe_lifetime_value_map) {
					initialized_rhs_maybe_lifetime_value_map.insert_or_assign(item.first, item.second);
					present_rhs_maybe_lifetime_value_map.insert_or_assign(item.first, item.second);
				}
				for (auto const& item : IOA_type_lhs_maybe_lifetime_value_map) {
					initialized_lhs_maybe_lifetime_value_map.insert_or_assign(item.first, item.second);
					present_lhs_maybe_lifetime_value_map.insert_or_assign(item.first, item.second);
				}

				{
					/* Here we iterate over each function parameter with an associated abstract lifetime (annotation). */
					for (const auto& param_lifetime_mapping1 : flta.m_param_lifetime_map) {
						clang::Expr const * arg1_EX = arg_from_param_ordinal(CE, param_lifetime_mapping1.first);
						auto maybe_param_qtype = qtype_from_param_ordinal_if_available(CE, param_lifetime_mapping1.first);
						if ((!(maybe_param_qtype.has_value())) || (!arg1_EX)) {
						} else {
							auto param_qtype = maybe_param_qtype.value();
							IF_DEBUG(const std::string param_qtype_str = param_qtype.getAsString();)

							auto lambda1 = [&](bool for_lhs_of_assignment = false) -> CMaybeExpressionLifetimeValuesWithHints {
								CScopeLifetimeInfo1 sloi1;
								/* Now we try to evaluate the "concrete" lifetime of the corresponding argument in the contructor expression. */
								auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, arg1_EX, Ctx, MR_ptr, Rewrite_ptr)
									: evaluate_expression_rhs_lower_bound_lifetimes(state1, arg1_EX, Ctx, MR_ptr, Rewrite_ptr);
								MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
								if (maybe_expr_lifetime_value.has_value()) {
									CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									sloi1 = expr_slti;
								} else {
									/* We generally don't expect to get here, but if for some reason a lower bound for the argument
									lifetime isn't available, we'll just use the shortest viable lifetime. */
									sloi1.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
									sloi1.m_maybe_containing_scope = get_containing_scope(arg1_EX, Ctx);
									sloi1.m_maybe_source_range = arg1_EX->getSourceRange();
									sloi1.m_maybe_corresponding_cpp_element = arg1_EX;
								}
								slti_set_default_lower_bound_lifetimes_where_needed(sloi1, param_qtype, state1);
								auto lifetime_values_ptr = &(sloi1.m_sublifetimes_vlptr->m_primary_lifetime_infos);

								std::vector<CScopeLifetimeInfo1> ref_lifetime_values;
								if (param_qtype->isReferenceType() || (IMPLICIT_THIS_PARAM_ORDINAL == param_lifetime_mapping1.first)) {
									/* Generally, a type's annotated (primary) lifetimes correspond to a lifetime value's
									"sublifetimes". But in the case of (raw) references, its sole (primary) lifetime corresponds
									to lifetime value's primary lifetime. So instead of having a separate code path for (raw)
									reference types, we'll create a "stand-in" set of "sublifetimes" for the reference type 
									consisting of just the primary lifetime value. */
									/* In the case of the implicit `this` parameter, the parameter type is given as the direct
									type rather than a reference or pointer to the type. But the lifetime annotations of the 
									implicit `this` parameter are defined to be as if it were a reference parameter, so we treat 
									it as such here. */
									ref_lifetime_values.push_back(sloi1);
									lifetime_values_ptr = &ref_lifetime_values;
								}
								auto& lifetime_values_ref = (*lifetime_values_ptr);

								struct CB {
									static void handle_params_with_lifetime_annotations(
										std::vector<CScopeLifetimeInfo1> const/*&*/ lifetime_values_ref, 
										CAbstractLifetimeSet const & alts, 
										decltype(initialized_rhs_maybe_lifetime_value_map)& initialized_maybe_lifetime_value_map_ref, 
										decltype(present_rhs_maybe_lifetime_value_map)& present_maybe_lifetime_value_map_ref, 
										decltype(IOA_type_rhs_maybe_lifetime_value_map)& IOA_type_maybe_lifetime_value_map_ref, 
										decltype(IOA_type_lhs_maybe_lifetime_value_map)& IOA_type_lhs_maybe_lifetime_value_map_ref, 
										decltype(as_sublifetime_of_tempexpr)& as_sublifetime_of_tempexpr, 
										decltype(function_decl) const & function_decl, 
										decltype(errors_suppressed_flag) const & errors_suppressed_flag,
										clang::SourceRange const & SR, CTUState& state1, 
										ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr,
										bool for_lhs_of_assignment = false
										) {

										size_t lv_index = 0;
										for (const auto& abstract_lifetime1 : alts.m_primary_lifetimes) {
											if (lifetime_values_ref.size() <= lv_index) {
												/* todo: report error? */
												break;
											}
											auto const/*&*/ lifetime_value_ref = lifetime_values_ref.at(lv_index);
											lv_index += 1;

											auto found_it3 = IOA_type_lhs_maybe_lifetime_value_map_ref.find(abstract_lifetime1);
											if (IOA_type_lhs_maybe_lifetime_value_map_ref.end() != found_it3) {
												/* The annotated lifetime of the parameter is an annotated lifetime of the object type. */
												if (!(found_it3->second.has_value())) {
													if (MR_ptr && (!errors_suppressed_flag)) {
														std::string error_desc = std::string("Use of type lifetime '") + found_it3->first.m_id + "' whose value could not be deduced.";
														state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
													}
												} else {
													auto corresponding_initialized_lhs_scope_lifetime = found_it3->second.value();

													if (!slti_second_can_be_assigned_to_first(as_sublifetime_of_tempexpr(corresponding_initialized_lhs_scope_lifetime)
														, as_sublifetime_of_tempexpr(lifetime_value_ref), Ctx, state1)) {

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
														if (MR_ptr && (!errors_suppressed_flag)) {
															std::string error_desc = std::string("Unable to verify that in the '") + function_decl->getQualifiedNameAsString()
																+ "' member function call expression, the argument corresponding to a parameter with lifetime label id '"
																+ abstract_lifetime1.m_id + "' has a lifetime (including any sublifetimes) that meets the (minimum required) lifetime set when the object was initialized.";
															state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
														}
													}
												}
											}

											std::optional<CScopeLifetimeInfo1> corresponding_initialized_maybe_scope_lifetime/* = abstract_lifetime1*/;
											std::optional<CScopeLifetimeInfo1> corresponding_present_maybe_scope_lifetime = corresponding_initialized_maybe_scope_lifetime;

											auto found_it2 = IOA_type_maybe_lifetime_value_map_ref.find(abstract_lifetime1);
											if (IOA_type_maybe_lifetime_value_map_ref.end() != found_it2) {
												/* The annotated lifetime of the parameter is an annotated lifetime of the object type. */
												corresponding_initialized_maybe_scope_lifetime = found_it2->second;

												corresponding_present_maybe_scope_lifetime = lifetime_value_ref;
												if (corresponding_present_maybe_scope_lifetime.has_value()) {
													auto& corresponding_present_scope_lifetime = corresponding_present_maybe_scope_lifetime.value();
													/* We're determining the lower/upper bound of the lifetime values bound to this abstract lifetime label. */
													corresponding_present_maybe_scope_lifetime = for_lhs_of_assignment ? upper_bound_of_lifetimes_slti(corresponding_present_scope_lifetime, lifetime_value_ref, Ctx, state1) : lower_bound_of_lifetimes_slti(corresponding_present_scope_lifetime, lifetime_value_ref, Ctx, state1);
												}
											} else {
												/* The annotated lifetime of the parameter is not an annotated lifetime of the object type, if any. */

												auto found_it = initialized_maybe_lifetime_value_map_ref.find(abstract_lifetime1);
												if (initialized_maybe_lifetime_value_map_ref.end() != found_it) {
													corresponding_initialized_maybe_scope_lifetime = found_it->second;
												}
												corresponding_present_maybe_scope_lifetime = corresponding_initialized_maybe_scope_lifetime;

												if ((corresponding_present_maybe_scope_lifetime.has_value())
													&& (CScopeLifetimeInfo1::ECategory::AbstractLifetime == corresponding_present_maybe_scope_lifetime.value().m_category)) {

													corresponding_present_maybe_scope_lifetime = lifetime_value_ref;
													corresponding_initialized_maybe_scope_lifetime = lifetime_value_ref;
												} else {
													/* We're determining the lower/upper bound of the lifetime values bound to this abstract lifetime label. */
													auto corresponding_initialized_scope_lifetime = lifetime_value_ref;
													if (corresponding_initialized_maybe_scope_lifetime.has_value()) {
														corresponding_initialized_scope_lifetime = for_lhs_of_assignment ? upper_bound_of_lifetimes_slti(corresponding_initialized_maybe_scope_lifetime.value(), lifetime_value_ref, Ctx, state1)
															: lower_bound_of_lifetimes_slti(corresponding_initialized_maybe_scope_lifetime.value(), lifetime_value_ref, Ctx, state1);
													}
													corresponding_initialized_maybe_scope_lifetime = corresponding_initialized_scope_lifetime;

													auto corresponding_present_scope_lifetime = lifetime_value_ref;
													if (corresponding_present_maybe_scope_lifetime.has_value()) {
														corresponding_present_scope_lifetime = for_lhs_of_assignment ? upper_bound_of_lifetimes_slti(corresponding_present_maybe_scope_lifetime.value(), lifetime_value_ref, Ctx, state1)
															: lower_bound_of_lifetimes_slti(corresponding_present_maybe_scope_lifetime.value(), lifetime_value_ref, Ctx, state1);
													}
													corresponding_present_maybe_scope_lifetime = corresponding_present_scope_lifetime;
												}
											}
											initialized_maybe_lifetime_value_map_ref.insert_or_assign( abstract_lifetime1, corresponding_initialized_maybe_scope_lifetime );
											present_maybe_lifetime_value_map_ref.insert_or_assign( abstract_lifetime1, corresponding_present_maybe_scope_lifetime );

											/* recursively handle the sublifetimes */
											handle_params_with_lifetime_annotations(lifetime_value_ref.m_sublifetimes_vlptr->m_primary_lifetime_infos, *(abstract_lifetime1.m_sublifetimes_vlptr)
												, initialized_maybe_lifetime_value_map_ref, present_maybe_lifetime_value_map_ref, IOA_type_maybe_lifetime_value_map_ref, IOA_type_lhs_maybe_lifetime_value_map_ref, as_sublifetime_of_tempexpr
												, function_decl, errors_suppressed_flag, SR, state1, Ctx, MR_ptr,Rewrite_ptr, for_lhs_of_assignment);
										}
									}
								};

								auto& initialized_maybe_lifetime_value_map_ref = for_lhs_of_assignment ? initialized_lhs_maybe_lifetime_value_map : initialized_rhs_maybe_lifetime_value_map;
								auto& present_maybe_lifetime_value_map_ref = for_lhs_of_assignment ? present_lhs_maybe_lifetime_value_map : present_rhs_maybe_lifetime_value_map;
								auto& IOA_type_maybe_lifetime_value_map_ref = for_lhs_of_assignment ? IOA_type_lhs_maybe_lifetime_value_map : IOA_type_rhs_maybe_lifetime_value_map;

								CB::handle_params_with_lifetime_annotations(lifetime_values_ref, param_lifetime_mapping1.second
									, initialized_maybe_lifetime_value_map_ref, present_maybe_lifetime_value_map_ref, IOA_type_maybe_lifetime_value_map_ref, IOA_type_lhs_maybe_lifetime_value_map, as_sublifetime_of_tempexpr
									, function_decl, errors_suppressed_flag, SR, state1, Ctx, MR_ptr,Rewrite_ptr, for_lhs_of_assignment);
								return {};
							};

							auto rhs_res = lambda1(false/*for_lhs_of_assignment*/);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(rhs_res, {});

							auto lhs_res = lambda1(true/*for_lhs_of_assignment*/);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(lhs_res, {});
						}
					}
				}

				for (const auto& present_rhs_maybe_lifetime_value_mapping1 : present_rhs_maybe_lifetime_value_map) {
					const auto& abstract_lifetime1 = present_rhs_maybe_lifetime_value_mapping1.first;
					const auto& maybe_lifetime_value1 = present_rhs_maybe_lifetime_value_mapping1.second;

					std::vector<std::shared_ptr<CPairwiseLifetimeConstraint> > lt1_constraint_shptrs;
					for (auto& constraint_shptr : flta.m_lifetime_constraint_shptrs) {
						if (abstract_lifetime1 == constraint_shptr->m_first) {
							lt1_constraint_shptrs.push_back(constraint_shptr);
						}
					}
					for (auto& constraint_shptr : IOA_lt1_constraint_shptrs) {
						if (abstract_lifetime1 == constraint_shptr->m_first) {
							lt1_constraint_shptrs.push_back(constraint_shptr);
						}
					}

					for (const auto& initialized_lhs_maybe_lifetime_value_mapping2 : initialized_lhs_maybe_lifetime_value_map) {
						const auto& abstract_lifetime2 = initialized_lhs_maybe_lifetime_value_mapping2.first;
						const auto& maybe_lifetime_value2 = initialized_lhs_maybe_lifetime_value_mapping2.second;

						for (const auto& constraint_shptr : lt1_constraint_shptrs) {
							bool abstract_lifetime1_can_be_assigned_to_abstract_lifetime2 = false;
							if (abstract_lifetime2 == constraint_shptr->m_second) {
								if (CPairwiseLifetimeConstraint::EYesNoDontKnow::Yes == constraint_shptr->second_can_be_assigned_to_first(abstract_lifetime2, abstract_lifetime1)) {
									abstract_lifetime1_can_be_assigned_to_abstract_lifetime2 = true;
								}
							}
							if (abstract_lifetime1_can_be_assigned_to_abstract_lifetime2) {
								if (maybe_lifetime_value1.has_value() && maybe_lifetime_value2.has_value()) {
									auto& lifetime_value1 = maybe_lifetime_value1.value();
									auto& lifetime_value2 = maybe_lifetime_value2.value();

									bool satisfies_checks = false;

									CScopeLifetimeInfo1 lhs_lifetime_value;
									CScopeLifetimeInfo1 rhs_lifetime_value;
									auto constraint_ptr = &(*constraint_shptr);
									static const auto CFirstCanBeAssignedToSecond_species_str = CFirstCanBeAssignedToSecond(CAbstractLifetime(), CAbstractLifetime()).species_str();
									if (CFirstCanBeAssignedToSecond_species_str == constraint_ptr->species_str()) {
										lhs_lifetime_value = lifetime_value2;
										rhs_lifetime_value = lifetime_value1;
									} else {
										lhs_lifetime_value = as_sublifetime_of_tempexpr(lifetime_value2);
										rhs_lifetime_value = as_sublifetime_of_tempexpr(lifetime_value1);
									}

									satisfies_checks = slti_second_can_be_assigned_to_first(lhs_lifetime_value
										, rhs_lifetime_value, Ctx, state1);

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
										if (MR_ptr && (!errors_suppressed_flag)) {
											std::string error_desc = std::string("Unable to verify that in the '") + function_decl->getQualifiedNameAsString()
												+ "' " + implicit_or_explicit_str + function_or_constructor_str + "call expression, the specified '" + constraint_shptr->species_str()
												+ "' lifetime constraint (applied to lifetime label ids '"
												+ abstract_lifetime1.m_id + "' and '" + abstract_lifetime2.m_id + "') is satisfied.";
											state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
										}
									}

									int q = 5;
								} else {
									std::string implicit_or_explicit_str;
									auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(function_decl);
									if (CXXMD) {
										implicit_or_explicit_str = "(implicit or explicit) member ";
									}
									std::string function_or_constructor_str = "function ";
									if (std::is_same_v<clang::CXXConstructExpr, TCallOrConstuctorExpr>) {
										function_or_constructor_str = "constructor ";
									}
									if (MR_ptr && (!errors_suppressed_flag)) {
										std::string error_desc = std::string("Unable to deduce the lifetime value(s) used by the '") + constraint_shptr->species_str()
											+ "' lifetime constraint (applied to lifetime label ids '" + abstract_lifetime1.m_id + "' and '" + abstract_lifetime2.m_id
											+ "') in the '" + function_decl->getQualifiedNameAsString() + "' " + implicit_or_explicit_str + function_or_constructor_str
											+ "call expression.";
										state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
									}
								}
							}
						}
					}
				}

				/* Any object referenced by a return value must outlive the call expression itself, or
				similarly, a hypothetical local variable declared just before the call expression. */
				CScopeLifetimeInfo1 hard_lower_bound_shallow_lifetime;
				//hard_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
				hard_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
				hard_lower_bound_shallow_lifetime.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
				hard_lower_bound_shallow_lifetime.m_maybe_source_range = CE->getSourceRange();
				hard_lower_bound_shallow_lifetime.m_maybe_corresponding_cpp_element = CE;

				bool could_be_a_dynamic_container_accessor = false;
				{
					clang::Expr const * IOA_E = nullptr;
					if (CXXMCE && (!CXXCD) && (std::string("operator&") != function_decl->getNameAsString())) {
						IOA_E = CXXMCE->getImplicitObjectArgument();
					} else if (CXXOCE) {
						if (1 <= CXXOCE->getNumArgs()) {
							IOA_E = CXXOCE->getArg(0);
						} else {
							int q = 3;
						}
					}
					if (IOA_E) {
						auto adjusted_IOA_E_qtype = IOA_E->getType();
						IF_DEBUG(const std::string IOA_E_qtype_str = adjusted_IOA_E_qtype.getAsString();)
						if (adjusted_IOA_E_qtype->isPointerType()) {
							const auto parent_E = IgnoreParenNoopCasts(IOA_E, Ctx);
							if (parent_E) {
								auto parent_E_qtype = parent_E->getType();
								IF_DEBUG(const std::string parent_E_qtype_str = parent_E_qtype.getAsString();)

								auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(parent_E);
								if (CXXOCE) {
									static const std::string operator_arrow_str = "operator->";
									auto operator_fdecl = CXXOCE->getDirectCallee();
									std::string operator_name;
									if (operator_fdecl) {
										operator_name = operator_fdecl->getNameAsString();
									} else {
										int q = 3;
									}

									if (((operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
										auto arg_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
										if (arg_EX) {
											const auto arg_EX_qtype = arg_EX->getType();
											IF_DEBUG(const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();)
											//MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(arg_EX_qtype, retval);

											adjusted_IOA_E_qtype = arg_EX_qtype;
										}
									}
								}
							}
						}
						IF_DEBUG(const std::string adjusted_IOA_E_qtype_str = adjusted_IOA_E_qtype.getAsString();)
						if (!is_known_to_be_const_declared_variable(IOA_E)) {
							could_be_a_dynamic_container_accessor |= is_recognized_unprotected_dynamic_container(adjusted_IOA_E_qtype);
						}
					}
				}

				auto lambda1 = [&](bool for_lhs_of_assignment = false) -> CMaybeExpressionLifetimeValuesWithHints {

					static CScopeLifetimeInfo1 s_lhs_default_lower_bound_shallow_lifetime = [&](){
							CScopeLifetimeInfo1 lhs_default_lower_bound_shallow_lifetime;
							lhs_default_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::Literal;
							lhs_default_lower_bound_shallow_lifetime.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
							lhs_default_lower_bound_shallow_lifetime.m_maybe_source_range = CE->getSourceRange();
							lhs_default_lower_bound_shallow_lifetime.m_maybe_corresponding_cpp_element = CE;
							return lhs_default_lower_bound_shallow_lifetime;
						}();

					auto& expr_scope_sublifetimes_ref = for_lhs_of_assignment ? expr_lhs_scope_sublifetimes : expr_rhs_scope_sublifetimes;
					expr_scope_sublifetimes_ref.m_primary_lifetime_infos.clear();
					auto& CE_type_maybe_lifetime_value_mappings_ref = for_lhs_of_assignment ? CE_type_lhs_maybe_lifetime_value_mappings : CE_type_rhs_maybe_lifetime_value_mappings;
					for (auto& CE_type_maybe_lifetime_value_mapping : CE_type_maybe_lifetime_value_mappings_ref) {
						auto& initialized_maybe_lifetime_value_map_ref = for_lhs_of_assignment ? initialized_lhs_maybe_lifetime_value_map : initialized_rhs_maybe_lifetime_value_map;
						auto found_it = initialized_maybe_lifetime_value_map_ref.find(CE_type_maybe_lifetime_value_mapping.first);
						if (initialized_maybe_lifetime_value_map_ref.end() != found_it) {
							CE_type_maybe_lifetime_value_mapping.second = found_it->second;
						}
						if (CE_type_maybe_lifetime_value_mapping.second.has_value()) {
							expr_scope_sublifetimes_ref.m_primary_lifetime_infos.push_back(CE_type_maybe_lifetime_value_mapping.second.value());
						} else {
							if (for_lhs_of_assignment) {
								expr_scope_sublifetimes_ref.m_primary_lifetime_infos.push_back(s_lhs_default_lower_bound_shallow_lifetime);
							} else {
								expr_scope_sublifetimes_ref.m_primary_lifetime_infos.push_back(hard_lower_bound_shallow_lifetime);
							}
						}
						if (CE_type_maybe_lifetime_value_mapping.second == CE_type_maybe_lifetime_value_mapping.first) {
							int q = 3;
						}

						auto check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow
							= [&](const CScopeLifetimeInfo1& slti) {
							if (CScopeLifetimeInfo1::ECategory::AbstractLifetime != slti.m_category) {
								auto shallow_slti = slti;
								shallow_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
								if (!first_is_known_to_be_contained_in_scope_of_second_shallow(hard_lower_bound_shallow_lifetime, shallow_slti, Ctx, state1)) {
									if (!is_known_to_never_deallocate_its_return_value(state1, function_decl, CE, Ctx, MR_ptr, Rewrite_ptr)) {
										if (MR_ptr && (!errors_suppressed_flag)) {
											std::string error_desc = std::string("At least one of the return value's direct or indirect (referenced object)")
												+ " lifetimes cannot be verified to (sufficiently) outlive the ('" + qfunction_name + "') call expression.";
											state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
										}
									}
								}
							}
						};
						if (CE_type_maybe_lifetime_value_mapping.second.has_value()) {
							auto const & slti1 = CE_type_maybe_lifetime_value_mapping.second.value();
							check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow(slti1);
							slti1.m_sublifetimes_vlptr->apply_to_all_lifetimes_const(check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow);
						}
					}

					if (true || !could_be_a_dynamic_container_accessor) {
						if (is_raw_pointer_or_equivalent(function_decl->getReturnType())
							|| function_decl->getReturnType()->isReferenceType()) {
							if (1 > expr_scope_sublifetimes_ref.m_primary_lifetime_infos.size()) {
								if (for_lhs_of_assignment) {
									expr_scope_sublifetimes_ref.m_primary_lifetime_infos.push_back(s_lhs_default_lower_bound_shallow_lifetime);
								} else {
									/* If we have not determined a lifetime for this pointer return value, then we will just use
									the minimum (allowable) value (for now). */
									expr_scope_sublifetimes_ref.m_primary_lifetime_infos.push_back(hard_lower_bound_shallow_lifetime);
								}
							}
						}

						std::vector<CScopeLifetimeInfo1> arg_lifetimes;
						for (size_t i = 0; i < CE->getNumArgs(); i += 1) {
							auto arg_E = CE->getArg(i);
							auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, arg_E, Ctx, MR_ptr, Rewrite_ptr)
								: evaluate_expression_rhs_lower_bound_lifetimes(state1, arg_E, Ctx, MR_ptr, Rewrite_ptr);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
							if (maybe_expr_lifetime_value.has_value()) {
								auto &arg_sli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
								arg_lifetimes.push_back(arg_sli);
							}
						}
						if (CXXMCE) {
							auto IOA_E = CXXMCE->getImplicitObjectArgument();
							auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr)
								: evaluate_expression_rhs_lower_bound_lifetimes(state1, IOA_E, Ctx, MR_ptr, Rewrite_ptr);
							MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value, maybe_expr_lifetime_value);
							if (maybe_expr_lifetime_value.has_value()) {
								auto &IOA_sli = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
								arg_lifetimes.push_back(IOA_sli);
							}
						}

						CScopeLifetimeInfo1 args_lower_bound_shallow_lifetime;
						args_lower_bound_shallow_lifetime.m_category = CScopeLifetimeInfo1::ECategory::Immortal;
						args_lower_bound_shallow_lifetime.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
						args_lower_bound_shallow_lifetime.m_maybe_source_range = CE->getSourceRange();
						bool args_lower_bound_shallow_lifetime_has_been_set = false;

						for (auto& arg_lifetime : arg_lifetimes) {
							auto sli_shallow = arg_lifetime;
							sli_shallow.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
							if (first_is_known_to_be_contained_in_scope_of_second_shallow(hard_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
								if (!first_is_known_to_be_contained_in_scope_of_second_shallow(args_lower_bound_shallow_lifetime, sli_shallow, Ctx, state1)) {
									if (!first_is_known_to_be_contained_in_scope_of_second_shallow(sli_shallow, args_lower_bound_shallow_lifetime, Ctx, state1)) {
										/* We don't seem to be able to unambiguously determine which argument lifetime is the
										shortest. So we'll just use the hard_lower_bound_shallow_lifetime. */
										args_lower_bound_shallow_lifetime = hard_lower_bound_shallow_lifetime;
										args_lower_bound_shallow_lifetime_has_been_set = true;
										break;
									}
									/* The shallow part of the given lifetime is not known to outlive the current value of
									args_lower_bound_shallow_lifetime, so we will consider it to be the new
									args_lower_bound_shallow_lifetime value. */
									args_lower_bound_shallow_lifetime = sli_shallow;
									args_lower_bound_shallow_lifetime_has_been_set = true;
								}
							}
						}
						if (!args_lower_bound_shallow_lifetime_has_been_set) {
							args_lower_bound_shallow_lifetime = hard_lower_bound_shallow_lifetime;
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

						expr_scope_sublifetimes_ref.apply_to_all_lifetimes(make_lifetime_at_least_args_lower_bound_shallow_lifetime);
					} else {
						int q = 5;
					}

					/* set the expression lifetime values */
					auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
					expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
					expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(CE, Ctx);
					expr_scope_lifetime_info.m_maybe_source_range = CE->getSourceRange();
					expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = CE;

					/* Here we set the previously evaluated expression sublifetimes. */
					*(expr_scope_lifetime_info.m_sublifetimes_vlptr) = expr_scope_sublifetimes_ref;

					slti_set_default_lower_bound_lifetimes_where_needed(expr_scope_lifetime_info, CE->getType());

					if (function_decl->getReturnType()->isReferenceType()) {
						/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
						same as the object it refers to (without an added level of indirection). */
						/* So we will attempt to remove one level of indirection from the expression lifetime. */
						auto& sublifetimes_ref = expr_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;
						if (1 == sublifetimes_ref.size()) {
							auto adj_expr_scope_lifetime_info = sublifetimes_ref.at(0);
							expr_scope_lifetime_info = adj_expr_scope_lifetime_info;
							if (could_be_a_dynamic_container_accessor && (std::string("operator&") != function_decl->getNameAsString())) {
								expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::ContainedDynamic;
							}
						} else {
							if (0 != sublifetimes_ref.size()) {
								/* unexpected */
								int q = 3;
							} else {
								/* The lifetime of the target object is not availiable. */
							}
						}
					}
					if (function_decl->getReturnType()->isPointerType() && ("operator->" == function_decl->getNameAsString())) {
						auto& sublifetimes_ref = expr_scope_lifetime_info.m_sublifetimes_vlptr->m_primary_lifetime_infos;
						if (1 == sublifetimes_ref.size()) {
							if (could_be_a_dynamic_container_accessor && (std::string("operator&") != function_decl->getNameAsString())) {
								sublifetimes_ref.at(0).m_category = CScopeLifetimeInfo1::ECategory::ContainedDynamic;
								/* Even if the 'nominal' primary lifetime would otherwise be in a different category, 
								its 'valid' lifetime cannot exceed that of any of the associated sublifetimes, even 
								that means that the primary lifetime category has to be downgraded. */
								expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::ContainedDynamic;
							}
						} else {
							if (0 != sublifetimes_ref.size()) {
								/* unexpected */
								int q = 3;
							} else {
								/* The lifetime of the target object is not availiable. */
							}
						}
					}

					/* Here we put the evaluated expression lifetimes in "persistent" storage. */
					auto& expr_lb_lifetime_values_map_ref = for_lhs_of_assignment ? state1.m_expr_lhs_lb_lifetime_values_map : state1.m_expr_rhs_lb_lifetime_values_map;
					expr_lb_lifetime_values_map_ref.insert_or_assign( CE, CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) } );
					return {};
				};

				auto rhs_res = lambda1(false/*for_lhs_of_assignment*/);
				MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(rhs_res, {});

				auto lhs_res = lambda1(true/*for_lhs_of_assignment*/);
				MSE_RETURN_VALUE_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(lhs_res, {});
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

				/*
				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				if (suppress_check_flag) {
					return;
				}
				*/

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
				if (constructor_decl) {
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
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/, bool for_lhs_of_assignment = false) {
		CMaybeExpressionLifetimeValuesWithHints retval;
		if (!E) {
			return retval;
		}
		const auto E_qtype = E->getType();
		if (E_qtype.isNull() || E_qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			retval.m_failure_due_to_dependent_type_flag = true;
			return retval;
		}
		/* Apparently calling "E_qtype.getAsString()" risks segfault here. */
		//IF_DEBUG(auto E_qtype_str = E_qtype.getAsString();)

		auto raw_SR = E->getSourceRange();
		auto SR = raw_SR;

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
		if (MR_ptr && Rewrite_ptr) {
			auto& MR = *MR_ptr;
			auto& Rewrite = *Rewrite_ptr;

			SR = nice_source_range(raw_SR, Rewrite);
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

		//auto ebai_scope_obj = state1.make_element_being_analyzed_info_scope_obj(SR);

		auto& expr_lifetime_values_map_ref = for_lhs_of_assignment ? state1.m_expr_lhs_lb_lifetime_values_map : state1.m_expr_rhs_lb_lifetime_values_map;

		{
			auto eltv_iter = expr_lifetime_values_map_ref.find(E);
			bool needs_processing = (expr_lifetime_values_map_ref.end() == eltv_iter);
			/* If errors could not be noted during the previous processing (if any) and can be now, then we will process again. */
			needs_processing = (needs_processing || ((!(eltv_iter->second.m_errors_noted)) && bool(MR_ptr)));
			if (!needs_processing) {
				/* Already processed (and any errors noted). */
				return eltv_iter->second;
			}
		}

		/* We're generally not interested in implicit nodes, but we do need to handle certain
		`ImplicitCastExpr`s. So we check if the node is an ImplicitCastExpr of a species that we're
		interested in, and if not, skip over any implicit nodes. */
		auto EX2 = IgnoreParenNoopCasts(E, Ctx);
		auto ICE = dyn_cast<const clang::ImplicitCastExpr>(EX2);
		const auto EX2_qtype = EX2->getType();
		if (EX2_qtype.isNull()) {
			return retval;
		}
		/* Apparently calling "EX2_qtype.getAsString()" risks segfault here. */
		//IF_DEBUG(auto EX2_qtype_str = EX2_qtype.getAsString();)
		bool condition1 = (ICE && ((clang::CastKind::CK_UncheckedDerivedToBase == ICE->getCastKind())
			|| (clang::CastKind::CK_DerivedToBase == ICE->getCastKind() || (clang::CastKind::CK_ArrayToPointerDecay == ICE->getCastKind()))));
		condition1 |= (EX2_qtype != E_qtype);
		if (condition1) {
			IF_DEBUG(const std::string E_qtype_str = E_qtype.getAsString();)
			IF_DEBUG(const std::string E_qtype_cann_str = get_cannonical_type(E_qtype).getAsString();)
			IF_DEBUG(const std::string E_qtype_cann_tail_str = E_qtype_cann_str.substr((E_qtype_cann_str.length() > 64) ? (E_qtype_cann_str.length() - 64) : 0);)
			IF_DEBUG(const std::string EX2_qtype_str = EX2_qtype.getAsString();)
			IF_DEBUG(const std::string EX2_qtype_cann_str = get_cannonical_type(EX2_qtype).getAsString();)
			IF_DEBUG(const std::string EX2_qtype_cann_tail_str = EX2_qtype_cann_str.substr((EX2_qtype_cann_str.length() > 64) ? (EX2_qtype_cann_str.length() - 64) : 0);)
			int q = 5;
		} else {
			EX2 = IgnoreParenImpNoopCasts(E, Ctx);
		}

		const auto E_ip = EX2;

		auto qtype = E_ip->getType();
		if (qtype.isNull() || qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			retval.m_failure_due_to_dependent_type_flag = true;
			return retval;
		}
		IF_DEBUG(auto qtype_str = qtype.getAsString();)

		bool errors_suppressed_flag = errors_suppressed_by_location(Ctx, SR.getBegin());
		if ((!errors_suppressed_flag) && Rewrite_ptr) {
			errors_suppressed_flag = state1.m_suppress_check_region_set.contains(E, *Rewrite_ptr, Ctx);
		}

		std::optional<CScopeLifetimeInfo1Set> maybe_sublifetimes;

		do {
			auto CXXCE = dyn_cast<const clang::CXXConstructExpr>(E_ip);
			if (CXXCE) {
				auto elv_iter1 = expr_lifetime_values_map_ref.find(CXXCE);
				if (expr_lifetime_values_map_ref.end() == elv_iter1) {
					const clang::FunctionDecl* function_decl = CXXCE->getConstructor();
					if (function_decl) {
						function_call_handler2(state1, function_decl, CXXCE, Ctx, MR_ptr, Rewrite_ptr);
					}
				}

				elv_iter1 = expr_lifetime_values_map_ref.find(CXXCE);
				if (expr_lifetime_values_map_ref.end() != elv_iter1) {
					retval = elv_iter1->second;
					return retval;
				}
				break;
			}

			auto DRE = dyn_cast<const clang::DeclRefExpr>(E_ip);
			if (DRE) {
				auto value_decl = DRE->getDecl();
				if (value_decl) {
					auto VD = dyn_cast<const clang::VarDecl>(value_decl);
					if (VD) {
						auto maybe_decl_lifetime_value = for_lhs_of_assignment ? evaluate_declaration_lhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr)
							: evaluate_declaration_rhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							retval.m_failure_due_to_dependent_type_flag = true;
							return retval;
						}
						if (maybe_decl_lifetime_value.has_value()) {
							CScopeLifetimeInfo1& decl_slti = maybe_decl_lifetime_value.value().m_scope_lifetime_info;

							auto res1 = expr_lifetime_values_map_ref.insert_or_assign( E_ip, CExpressionLifetimeValues{ decl_slti, bool(MR_ptr) } );
							return (res1.first)->second;
						}
					} else {
						auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(value_decl);
						auto FND = dyn_cast<const clang::FunctionDecl>(value_decl);
						if (FND) {
							/* This (presumably static?) function declaration could be the target of a function pointer. */
							CScopeLifetimeInfo1 slti1;
							slti1.m_category = CScopeLifetimeInfo1::ECategory::Literal;
							/* I can't imagine any of these fields would be relevant, but anyway... */
							slti1.m_maybe_containing_scope = get_containing_scope(FND, Ctx);
							slti1.m_maybe_source_range = FND->getSourceRange();
							slti1.m_maybe_corresponding_cpp_element = E_ip;

							retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };
						}
					}
				}
				break;
			}

			auto DSDRE = dyn_cast<const clang::DependentScopeDeclRefExpr>(E_ip);
			if (DSDRE) {
				retval.m_failure_due_to_dependent_type_flag = true;
				return retval;
				//break;
			}

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

					auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, containing_ref_EX, Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
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

						auto maybe_tlta = type_lifetime_annotations_if_available(containing_qtype, state1, MR_ptr, Rewrite_ptr);
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

							auto maybe_abstract_lifetime_set = state1.corresponding_abstract_lifetime_set_if_any(FD);
							if (maybe_abstract_lifetime_set.has_value()) {
								auto& abstract_lifetime_set1 = maybe_abstract_lifetime_set.value();
								/* The member field's type also seems to have (zero or more) annotated lifetimes. Presumably, its
								abstract lifetimes are a subset of its parent/containing object's abstract lifetimes. */

								/* For each of the member field's abstract lifetimes, we'll look for the corresponding abstract
								lifetime of the parent/containing object, and its corresponding evaluated (non-abstract) value,
								if available, and assign that value to the member field accordingly. */
								for (auto const& field_abstract_lifetime1 : abstract_lifetime_set1.m_primary_lifetimes) {
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

						retval = CExpressionLifetimeValues{ expr_slti, bool(MR_ptr) };
					}
				} else if (VD) {
					auto maybe_decl_lifetime_value = for_lhs_of_assignment ? evaluate_declaration_lhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr)
						: evaluate_declaration_rhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
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
				break;
			}

			auto CXXDSME = dyn_cast<const clang::CXXDependentScopeMemberExpr>(E_ip);
			if (CXXDSME) {
				retval.m_failure_due_to_dependent_type_flag = true;
				return retval;
				//break;
			}

			auto ILE = dyn_cast<const clang::InitListExpr>(E_ip);
			if (ILE) {
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
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(sf_ILE_qtype, retval);
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
							auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, sf_ILE->getInit(i), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
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
						retval = CExpressionLifetimeValues{ lower_bound_of_lifetimes_slti(element_slis, Ctx, state1), bool(MR_ptr) };
					}
				}
				break;
			}

			auto CXXILE = dyn_cast<const clang::CXXStdInitializerListExpr>(E_ip);
			if (CXXILE) {
				const auto sub_E_ip = IgnoreParenNoopCasts(CXXILE->getSubExpr(), Ctx);
				if (sub_E_ip && (sub_E_ip != CXXILE)) {
					/* We're expecting sub_E_ip to be a clang::InitListExpr (pointer). */
					retval = evaluate_expression_lower_bound_lifetimes(state1, sub_E_ip, Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
				}
				break;
			}

			auto CSTE = dyn_cast<const clang::CastExpr>(E_ip);
			if (CSTE) {
				const auto sub_E_ip = IgnoreParenNoopCasts(CSTE->getSubExpr(), Ctx);
				if (sub_E_ip && (sub_E_ip != CSTE)) {
					const auto CSTE_qtype = CSTE->getType();
					IF_DEBUG(auto CSTE_qtype_str = CSTE_qtype.getAsString();)
					MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(CSTE_qtype, retval);
					const auto sub_E_ip_qtype = sub_E_ip->getType();
					IF_DEBUG(auto sub_E_ip_qtype_str = sub_E_ip_qtype.getAsString();)
					const auto cn_sub_E_ip_qtype = get_cannonical_type(sub_E_ip_qtype);
					IF_DEBUG(auto cn_sub_E_ip_qtype_str = cn_sub_E_ip_qtype.getAsString();)

					const auto cast_kind = CSTE->getCastKind();
					IF_DEBUG(auto cast_kind_str = CSTE->getCastKindName();)
					const auto cf_ND = CSTE->getConversionFunction();

					if (false && clang::CastKind::CK_NullToPointer == cast_kind) {
						/* We may need to enable this error at some point, but so far these cases seem to be already covered by
						the "reference does not live long enough" errors. */
						if (MR_ptr) {
							std::string error_desc = std::string("(Implicit or explicit) 'NULL (/zero literal) to (raw) pointer' cast.");
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}

					bool reference_cast_or_equivalent = CSTE_qtype->isReferenceType();
					reference_cast_or_equivalent |= (clang::CastKind::CK_UncheckedDerivedToBase == cast_kind);
					reference_cast_or_equivalent |= (clang::CastKind::CK_DerivedToBase == cast_kind);
					reference_cast_or_equivalent |= (clang::CastKind::CK_BaseToDerived == cast_kind);
					reference_cast_or_equivalent |= (clang::CastKind::CK_LValueToRValue == cast_kind); // ?
					reference_cast_or_equivalent |= (clang::CastKind::CK_NoOp == cast_kind);

					CScopeLifetimeInfo1 slti1;
					slti1.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
					slti1.m_maybe_containing_scope = get_containing_scope(CSTE, Ctx);
					slti1.m_maybe_source_range = CSTE->getSourceRange();
					slti1.m_maybe_corresponding_cpp_element = CSTE;

					auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, sub_E_ip, Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
					if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						retval.m_failure_due_to_dependent_type_flag = true;
						return retval;
					}
					if (maybe_expr_lifetime_value.has_value()) {
						auto& src_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
						if (reference_cast_or_equivalent) {
							slti1 = src_slti;
						} else if (clang::CastKind::CK_ArrayToPointerDecay == cast_kind) {
							*(slti1.m_sublifetimes_vlptr) = src_slti;
						} else {
							slti_set_default_lower_bound_lifetimes_where_needed(src_slti, CSTE_qtype, state1);
							*(slti1.m_sublifetimes_vlptr) = *(src_slti.m_sublifetimes_vlptr);
						}
						retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };

						auto maybe_tlta_ptr = type_lifetime_annotations_if_available(*CSTE, state1);
						if (maybe_tlta_ptr.has_value()) {
							auto const & tlta = *(maybe_tlta_ptr.value());
							/* The result type of the cast has abstract (annotated) lifetimes associated with it. We cannot in
							general assume that any (sub)lifetime values of the source object correspond to those of the
							resulting object. */
							auto maybe_ltvs = for_lhs_of_assignment ? state1.corresponding_lhs_lb_lifetime_values_if_any(CSTE)
								: state1.corresponding_rhs_lb_lifetime_values_if_any(CSTE);
							if (maybe_ltvs.has_value()) {
								/* The lifetime values of the resulting object seem to have been already evaluated elsewhere. */
								/* Does this ever happen? */
								auto& ltvs = maybe_ltvs.value().m_scope_lifetime_info;
								*(slti1.m_sublifetimes_vlptr) = *(ltvs.m_sublifetimes_vlptr);
								retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };
							} else if ((clang::CastKind::CK_UncheckedDerivedToBase == cast_kind) || (clang::CastKind::CK_DerivedToBase == cast_kind)) {
								/* This particular (implicit) cast seems to be just using an object as its base class. This is
								similar to using a member field of an object via member expression, and so we handle it in
								similar fashion. */
								auto sub_E_ip_qtype = sub_E_ip->getType();
								IF_DEBUG(auto sub_E_ip_qtype_str = sub_E_ip_qtype.getAsString();)
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(sub_E_ip_qtype, retval);

								auto CXXRD = sub_E_ip_qtype->getAsCXXRecordDecl();
								if (CXXRD) {
									for (auto& CXXBS : CXXRD->bases()) {
										auto base_qtype = CXXBS.getType();
										IF_DEBUG(auto base_qtype_str = base_qtype.getAsString();)
										MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(base_qtype, retval);

										auto base_cnqtype = base_qtype.getCanonicalType(); base_cnqtype.removeLocalConst();
										IF_DEBUG(auto base_cnqtype_str = base_cnqtype.getAsString();)
										auto CSTE_cnqtype = CSTE_qtype.getCanonicalType(); CSTE_cnqtype.removeLocalConst();
										IF_DEBUG(auto CSTE_cnqtype_str = CSTE_cnqtype.getAsString();)

										if (base_cnqtype == CSTE_cnqtype) {
											auto iter1 = state1.m_base_class_to_abstract_lifetime_map.find(&CXXBS);
											if (state1.m_base_class_to_abstract_lifetime_map.end() != iter1) {
												auto& owner_slti = src_slti;
												auto const& owner_type_lifetime_values = owner_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos;

												/* expr_stli will (hopefully) be the returned lifetime value of the base class. We will start
												by initializing it with the lifetime value of its parent/containing object. */
												CScopeLifetimeInfo1 expr_slti = owner_slti;

												CPossessionLifetimeInfo1 pli;
												pli.m_maybe_field_source_range = CXXBS.getSourceRange();
												/* Here we're adding info about the base class's relationship to the parent from which its lifetime
												value is based. */
												expr_slti.m_possession_lifetime_info_chain.push_back(pli);

												/* While the base class's primary lifetime is based on its parent/containing object, its
												sublifetimes are, in general, not. We still need to evaluate them. */
												expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();

												auto maybe_tlta = type_lifetime_annotations_if_available(*sub_E_ip, state1, MR_ptr, Rewrite_ptr);
												if (maybe_tlta.has_value()) {
													/* The parent/containing type seems to have (zero or more) annotated lifetimes. */

													auto CXXTE = dyn_cast<const clang::CXXThisExpr>(sub_E_ip);
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

													auto maybe_abstract_lifetime_set = state1.corresponding_abstract_lifetime_set_if_any(&CXXBS);
													if (maybe_abstract_lifetime_set.has_value()) {
														auto& abstract_lifetime_set1 = maybe_abstract_lifetime_set.value();
														/* The base class's type also seems to have (zero or more) annotated lifetimes. Presumably, its
														abstract lifetimes are a subset of its parent/containing object's abstract lifetimes. */

														/* For each of the base class's abstract lifetimes, we'll look for the corresponding abstract
														lifetime of the parent/containing object, and its corresponding evaluated (non-abstract) value,
														if available, and assign that value to the base class accordingly. */
														for (auto const& base_class_abstract_lifetime1 : abstract_lifetime_set1.m_primary_lifetimes) {
															auto owner_tlv_iter1 = owner_type_lifetime_values.begin();
															auto owner_talt_iter1 = owner_type_abstract_lifetimes.begin();
															for (; (owner_type_abstract_lifetimes.end() != owner_talt_iter1) && (owner_type_lifetime_values.end() != owner_tlv_iter1)
																; ++owner_talt_iter1, ++owner_tlv_iter1) {

																if (base_class_abstract_lifetime1 == (*owner_talt_iter1)) {
																	expr_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.push_back(*owner_tlv_iter1);
																	break;
																}
															}
														}
													}
													int q = 5;
												}
												slti_set_default_lower_bound_lifetimes_where_needed(expr_slti, CXXBS.getType());

												retval = CExpressionLifetimeValues{ expr_slti, bool(MR_ptr) };
											}
											break;
										}
									}
								}
							} else if (clang::CastKind::CK_ArrayToPointerDecay == cast_kind) {
								/* We've already mode the adjustment for this case. */;
							} else if (!for_lhs_of_assignment) {
								/* Since it's the lower bound that we're trying to determine, for now we'll just set all the
								(sub)lifetime values to the (lower bound) lifetime of object itself. */
								retval = {};
								auto src_slti2 = src_slti;
								src_slti2.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
								slti_set_default_lower_bound_lifetimes_where_needed(src_slti2, CSTE_qtype, state1);
								*(slti1.m_sublifetimes_vlptr) = *(src_slti2.m_sublifetimes_vlptr);
								retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };
							}
						}
					}
					int q = 5;
				}
				return retval;
				//break;
			}

			auto CO = dyn_cast<const clang::ConditionalOperator>(E_ip);
			if (CO) {
				auto maybe_true_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, CO->getTrueExpr(), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
				auto maybe_false_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, CO->getFalseExpr(), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
				if (maybe_true_expr_lifetime_value.m_failure_due_to_dependent_type_flag || maybe_false_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
					retval.m_failure_due_to_dependent_type_flag = true;
					return retval;
				}
				if ((maybe_true_expr_lifetime_value.has_value()) && (maybe_false_expr_lifetime_value.has_value())) {
					/* The conditional operator requires both alternatives to be of the same type. Conceptually, we could
					consider a type's (lower bound) lifetime values, if any, as part of the type and therefore complain 
					if the alternatives have different (lower bound) lifetime values. But instead, since we know whether 
					or not this (lower bound) lifetime value is being used for the lhs or the rhs of an assignment, we 
					can instead return the upper bound or lower bound of the two alternatives as appropriate. Note that 
					the upper or lower bound may not be the same as either of the two alternatives. It may be a 
					frankenstein combination of selected (sub)lifetime values from each. */
					auto& true_slti = maybe_true_expr_lifetime_value.value().m_scope_lifetime_info;
					auto& false_slti = maybe_false_expr_lifetime_value.value().m_scope_lifetime_info;
					auto slti1 = for_lhs_of_assignment ? upper_bound_of_lifetimes_slti(true_slti, false_slti, Ctx, state1) : lower_bound_of_lifetimes_slti(true_slti, false_slti, Ctx, state1);
					retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };
					//break;
				}
				break;
			}

			auto SL = dyn_cast<const clang::StringLiteral>(E_ip);
			if (SL) {
				auto slti1 = CScopeLifetimeInfo1{};
				slti1.m_category = CScopeLifetimeInfo1::ECategory::Literal;
				slti1.m_maybe_containing_scope = get_containing_scope(SL, Ctx);
				slti1.m_maybe_source_range = SL->getSourceRange();
				slti1.m_maybe_corresponding_cpp_element = SL;

				IF_DEBUG(const std::string E_qtype_str = E_qtype.getAsString();)
				slti_set_default_lower_bound_lifetimes_where_needed(slti1, E_qtype, state1);

				retval = CExpressionLifetimeValues{ slti1, bool(MR_ptr) };
				break;
			}

			auto CXXBTE = dyn_cast<const clang::CXXBindTemporaryExpr>(E_ip);
			if (CXXBTE) {
				/* Even though CXXBindTemporaryExpr is an implicit node, it may still get presented for evaluation 
				because its input and output type may be different. */
				return evaluate_expression_lower_bound_lifetimes(state1, CXXBTE->getSubExpr(), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
			}

			auto CXXFCE = dyn_cast<const clang::CXXFunctionalCastExpr>(E_ip);
			if (CXXFCE) {
				/* Unexpected. This is an implicit node that we expect to have been skipped over. But we'll leave this 
				handling code just in case. */
				int i = 3;
				return evaluate_expression_lower_bound_lifetimes(state1, CXXFCE->getSubExpr(), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
			}
			auto EWC = dyn_cast<const clang::ExprWithCleanups>(E_ip);
			if (EWC) {
				/* Unexpected. This is an implicit node that we expect to have been skipped over. But we'll leave this 
				handling code just in case. */
				int i = 3;
				return evaluate_expression_lower_bound_lifetimes(state1, EWC->getSubExpr(), Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
			}

			if (is_raw_pointer_or_equivalent(qtype)) {
				auto target_EX = raw_pointer_target_expression_if_available(E_ip, Ctx, state1);
				auto maybe_expr_lifetime_value = evaluate_expression_lower_bound_lifetimes(state1, target_EX, Ctx, MR_ptr, Rewrite_ptr, for_lhs_of_assignment);
				if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
					retval.m_failure_due_to_dependent_type_flag = true;
					return retval;
				}
				if (maybe_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
					if (false && ((CScopeLifetimeInfo1::ECategory::ContainedDynamic == expr_slti.m_category) || (CScopeLifetimeInfo1::ECategory::None == expr_slti.m_category))) {
						/* This case is handled by the MCSSSAddressOf::run() handler. */
						if (MR_ptr) {
							std::string of_type_str;
							auto target_qtype = target_EX->getType();
							if (!(target_qtype.isNull())) {
								of_type_str = " (of type " + get_as_quoted_string_for_errmsg(target_qtype) + ")";
							}

							std::string error_desc = std::string("Unable to verify that the argument")
								+ of_type_str + " lives long enough to take its address.";
							if (CScopeLifetimeInfo1::ECategory::ContainedDynamic == expr_slti.m_category) {
								error_desc += std::string(" (If the argument is a reference to an element in a dynamic container, you might instead access")
								+ " the element via a corresponding non-dynamic/'fixed' interface object which borrows (exclusive"
								+ " access to) container's contents.)";
							}
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}
					maybe_sublifetimes = { expr_slti };
				}
				break;
			}
		} while(false);

		if (!(retval.has_value())) {
			auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
			expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::TemporaryExpression;
			expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(E, Ctx);
			expr_scope_lifetime_info.m_maybe_source_range = E->getSourceRange();
			expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = E;
#ifndef NDEBUG
			const auto E_ip_containing_scope = get_containing_scope(E_ip, Ctx);
			if (E_ip_containing_scope != expr_scope_lifetime_info.m_maybe_containing_scope) {
				/* E_ip is just supposed to be E with any enclosing "parentheses and implicit and no-op casts" 
				removed. But there seem to be times when E_ip and E are not sufficiently equivalent. For example,
				we encounterred expressions involving the use of literals in template funtions where the in the 
				second instantion of the template function, the "containing scope" of E_ip, unlike E, remained 
				the same as that of the the first instantiation. */
				int q = 5;
			}
#endif /*!NDEBUG*/

			/* Here we try to evaluate the direct lifetime of the expression. */
			auto maybe_expr_owner = for_lhs_of_assignment ? lhs_lifetime_owner_if_available(E_ip, Ctx, state1, MR_ptr, Rewrite_ptr)
				: rhs_lifetime_owner_if_available(E_ip, Ctx, state1, MR_ptr, Rewrite_ptr);
			if (maybe_expr_owner.has_value()) {
				auto& expr_owner = maybe_expr_owner.value();
				if (std::holds_alternative<const VarDecl*>(expr_owner)) {
					auto VD = std::get<const VarDecl*>(expr_owner);
					auto maybe_decl_lifetime_value = for_lhs_of_assignment ? evaluate_declaration_lhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr)
						: evaluate_declaration_rhs_lower_bound_lifetimes(state1, VD, Ctx, MR_ptr, Rewrite_ptr);
					if (maybe_decl_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						retval.m_failure_due_to_dependent_type_flag = true;
						return retval;
					}
				}

				auto sloi1 = rhs_scope_lifetime_info_from_lifetime_owner(maybe_expr_owner.value(), Ctx, state1);
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
			retval = CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) };
		}
		if (E_qtype->isPointerType()) {
			auto CXXOCE = dyn_cast<const clang::CXXOperatorCallExpr>(E_ip);
			if (CXXOCE) {
				static const std::string operator_arrow_str = "operator->";
				auto operator_fdecl = CXXOCE->getDirectCallee();
				std::string operator_name;
				if (operator_fdecl) {
					operator_name = operator_fdecl->getNameAsString();
				} else {
					int q = 3;
				}

				if (((operator_arrow_str == operator_name)) && (1 == CXXOCE->getNumArgs())) {
					auto arg_EX = IgnoreParenImpNoopCasts(CXXOCE->getArg(0), Ctx);
					if (arg_EX) {
						const auto arg_EX_qtype = arg_EX->getType();
						IF_DEBUG(const auto arg_EX_qtype_str = arg_EX_qtype.getAsString();)
						//MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(arg_EX_qtype, retval);

						bool could_be_a_dynamic_container_accessor = is_recognized_unprotected_dynamic_container(arg_EX_qtype);
						if (could_be_a_dynamic_container_accessor) {
							auto expr_scope_lifetime_info = CScopeLifetimeInfo1{};
							expr_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::ContainedDynamic;
							expr_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(E, Ctx);
							expr_scope_lifetime_info.m_maybe_source_range = E->getSourceRange();
							expr_scope_lifetime_info.m_maybe_corresponding_cpp_element = E;
							slti_set_default_lower_bound_lifetimes_where_needed(expr_scope_lifetime_info, E_qtype, state1);

							retval = CExpressionLifetimeValues{ expr_scope_lifetime_info, bool(MR_ptr) };
						}
					}
				}
			}
		}

		if (retval.has_value()) {
			auto& eltvs = retval.value();

			/* Here we put the evaluated expression lifetimes in "persistent" storage. */
			expr_lifetime_values_map_ref.insert_or_assign( E, eltvs );
			if (E_ip != E) {
				auto res1 = expr_lifetime_values_map_ref.insert_or_assign( E_ip, eltvs );
			}
		}

		return retval;
	}
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_rhs_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		return evaluate_expression_lower_bound_lifetimes(state1, E, Ctx, MR_ptr, Rewrite_ptr);
	}
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_rhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const clang::Expr* E) {
		return evaluate_expression_rhs_lower_bound_lifetimes(state1, E, *(MR.Context), &MR, &Rewrite);
	}
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lhs_lower_bound_lifetimes(CTUState& state1
			, const clang::Expr* E, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {

		return evaluate_expression_lower_bound_lifetimes(state1, E, Ctx, MR_ptr, Rewrite_ptr, true/*for_lhs_of_assignment*/);
	}
	inline CMaybeExpressionLifetimeValuesWithHints evaluate_expression_lhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const clang::Expr* E) {
		return evaluate_expression_lhs_lower_bound_lifetimes(state1, E, *(MR.Context), &MR, &Rewrite);
	}

	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/, bool for_lhs_of_assignment = false) {
		CMaybeVariableLifetimeValuesWithHints retval;
		if (!DD) {
			return retval;
		}

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
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
		}

		auto SR = Rewrite_ptr ? nice_source_range(DD->getSourceRange(), *Rewrite_ptr) : DD->getSourceRange();

		const auto qtype = DD->getType();
		if (qtype.isNull() || qtype->isDependentType()) {
			/* Cannot properly evaluate (presumably) because this is a template definition. Proper
			evaluation should occur in any instantiation of the template. */
			retval.m_failure_due_to_dependent_type_flag = true;
			return retval;
		}
		IF_DEBUG(const std::string qtype_str = qtype.getAsString();)

		bool errors_suppressed_flag = errors_suppressed_by_location(Ctx, SR.getBegin());
		if ((!errors_suppressed_flag) && Rewrite_ptr) {
			errors_suppressed_flag = state1.m_suppress_check_region_set.contains(DD, *Rewrite_ptr, Ctx);
		}

		auto VD = dyn_cast<const clang::VarDecl>(DD);
		if (VD) {
			auto vltv_iter1 = state1.m_vardecl_rhs_lb_lifetime_values_map.find(VD);
			bool needs_processing = (state1.m_vardecl_rhs_lb_lifetime_values_map.end() == vltv_iter1);
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

						/* For some reason the clang AST represents member operator call expressions as non-member
						call expressions with the implicit `this` parameter as the first parameter. So will indicate
						whether this is a member operator in the param_ordinal_t's constructor call. */
						bool is_ns_member_operator = false;
						auto CXXMD = dyn_cast<const clang::CXXMethodDecl>(FD);
						if (CXXMD && CXXMD->isOverloadedOperator() && (!(CXXMD->isStatic()))) {
							is_ns_member_operator = true;
						}
						auto param_ordinal = is_ns_member_operator ? param_ordinal_t(param_ordinal_t::ns_member_operator_tag{}, 1)
							: param_ordinal_t(1);

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
										auto VD_conatining_scope = get_containing_scope(VD, Ctx);
										auto VDSR = VD->getSourceRange();

										const auto sl_storage_duration = VD->getStorageDuration();
										const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
										sli2.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
										sli2.m_maybe_containing_scope = VD_conatining_scope;
										sli2.m_maybe_source_range = VDSR;

										/* Since, unlike regular variables, the initialization value of parameter variables (i.e. the
										corresponding argument) is not available in the declaration. So we use the annotated abstract
										lifetimes themselves as the lifetime (initialization) values. */
										*(sli2.m_sublifetimes_vlptr) = plm_iter->second;
										for (auto& slti_ref : (*(sli2.m_sublifetimes_vlptr)).m_primary_lifetime_infos) {
											slti_ref.m_maybe_containing_scope = VD_conatining_scope;
											slti_ref.m_maybe_source_range = VDSR;
										}
									}
									auto& vardecl_lb_lifetime_values_map_ref = for_lhs_of_assignment ? state1.m_vardecl_lhs_lb_lifetime_values_map
										: state1.m_vardecl_rhs_lb_lifetime_values_map;;
									auto res1 = vardecl_lb_lifetime_values_map_ref.insert_or_assign(VD, CVariableLifetimeValues{ sli2, bool(MR_ptr) });
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

				if (maybe_tlta.has_value() || is_raw_pointer_or_equivalent(qtype)) {
					if (VD->hasInit()) {
						auto init_E = VD->getInit();
						assert(init_E);
						auto init_E_qtype = init_E->getType();
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(init_E_qtype, retval);
						IF_DEBUG(auto init_E_qtype_str = init_E_qtype.getAsString();)

						auto ILE = dyn_cast<const clang::InitListExpr>(init_E);
						auto VD_qtype = VD->getType();
						IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
						MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);
						if (ILE && VD_qtype->isAggregateType()) {
							if (!(VD_qtype->isPointerType() || VD_qtype->isReferenceType())) {
								if (MR_ptr && (!errors_suppressed_flag)) {
									const std::string error_desc = std::string("Aggregate initialization (of '") + var_qualified_name
										+ "') is not (currently) supported for types (like " + get_as_quoted_string_for_errmsg(qtype) + ") that have (explicit or "
										+ "implicit) lifetime annotations.";
									state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
								}
							}
						}

						auto maybe_expr_lifetime_value = for_lhs_of_assignment ? evaluate_expression_lhs_lower_bound_lifetimes(state1, init_E, Ctx, MR_ptr, Rewrite_ptr)
							: evaluate_expression_rhs_lower_bound_lifetimes(state1, init_E, Ctx, MR_ptr, Rewrite_ptr);
						if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
							/* Cannot properly evaluate because this is a template definition. Proper evaluation should
							occur in any instantiation of the template. */
							retval.m_failure_due_to_dependent_type_flag = true;
							//return retval;
						}
						if (maybe_expr_lifetime_value.has_value()) {
							CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;

							CScopeLifetimeInfo1 shallow_var_slti;
							const auto sl_storage_duration = VD->getStorageDuration();
							const auto sl_is_immortal = ((clang::StorageDuration::SD_Static == sl_storage_duration) || (clang::StorageDuration::SD_Thread == sl_storage_duration)) ? true : false;
							shallow_var_slti.m_category = sl_is_immortal ? CScopeLifetimeInfo1::ECategory::Immortal : CScopeLifetimeInfo1::ECategory::Automatic;
							shallow_var_slti.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
							shallow_var_slti.m_maybe_source_range = VD->getSourceRange();

							if (VD_qtype->isReferenceType()) {
								auto VD_pointee_qtype = VD_qtype->getPointeeType();
								MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_pointee_qtype, retval);
								if (VD_pointee_qtype.isConstQualified()) {

									if (CScopeLifetimeInfo1::ECategory::TemporaryExpression == expr_slti.m_category) {
										/* todo: verify that init_E is actually a temporary? */
										/* Temporaries assigned to const references are subject to life extension. */
										expr_slti.m_category = shallow_var_slti.m_category;
										expr_slti.m_maybe_containing_scope = shallow_var_slti.m_maybe_containing_scope;
										expr_slti.m_maybe_source_range = shallow_var_slti.m_maybe_source_range;
									}
								}
							}

							auto& hard_lower_bound_shallow_lifetime = shallow_var_slti;

							auto check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow
								= [&hard_lower_bound_shallow_lifetime, &VD, &errors_suppressed_flag, &SR, &state1, &Ctx, &MR_ptr, &Rewrite_ptr](const CScopeLifetimeInfo1& slti) {
								if (CScopeLifetimeInfo1::ECategory::AbstractLifetime != slti.m_category) {
									auto shallow_slti = slti;
									shallow_slti.m_sublifetimes_vlptr->m_primary_lifetime_infos.clear();
									if (!first_is_known_to_be_contained_in_scope_of_second_shallow(hard_lower_bound_shallow_lifetime, shallow_slti, Ctx, state1)) {
										if (MR_ptr && (!errors_suppressed_flag)) {
											std::string error_desc = std::string("At least one of the initialization value's direct or indirect (referenced object)")
												+ " lifetimes cannot be verified to (sufficiently) outlive the ('" + VD->getQualifiedNameAsString() + "') variable (of type "
												+ get_as_quoted_string_for_errmsg(VD->getType()) + ").";
											state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
										}
									}
								}
							};

							auto& vardecl_lb_lifetime_values_map_ref = for_lhs_of_assignment ? state1.m_vardecl_lhs_lb_lifetime_values_map
								: state1.m_vardecl_rhs_lb_lifetime_values_map;

							slti_set_default_lower_bound_lifetimes_where_needed(expr_slti, init_E_qtype, state1);

							expr_slti.m_sublifetimes_vlptr->apply_to_all_lifetimes_const(check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow);

							if (qtype->isReferenceType()) {
								/* Unlike other pointer/reference objects, the lifetime of a native reference variable is the
								same as the object it refers to (without an added level of indirection). */
								retval = CVariableLifetimeValues{ expr_slti, bool(MR_ptr) };
								if (!for_lhs_of_assignment) {
									check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow(expr_slti);
								}
							} else if (is_raw_pointer_or_equivalent(qtype) && (!(maybe_tlta.has_value()))) {
								auto slti1 = shallow_var_slti;
								slti_set_default_lower_bound_lifetimes_where_needed(slti1, qtype, state1);
								auto res1 = vardecl_lb_lifetime_values_map_ref.insert_or_assign(VD, CVariableLifetimeValues{ slti1, bool(MR_ptr) });
								retval = res1.first->second;
							} else {
								CScopeLifetimeInfo1 sli2 = shallow_var_slti;
								*(sli2.m_sublifetimes_vlptr) = *(expr_slti.m_sublifetimes_vlptr);

								auto res1 = vardecl_lb_lifetime_values_map_ref.insert_or_assign(VD, CVariableLifetimeValues{ sli2, bool(MR_ptr) });
								retval = res1.first->second;
								//return retval;
							}

							expr_slti.m_sublifetimes_vlptr->apply_to_all_lifetimes_const(check_that_lifetime_is_abstract_or_outlives_hard_lower_bound_shallow);
						} else {
							int q = 5;
						}
					} else {
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("(Non-parameter) variable '")
								+ var_qualified_name + "' of type " + get_as_quoted_string_for_errmsg(qtype) + " has as an associated lifetime label which "
								+ "requires that the decalaration have an initialization value (that doesn't seem to be present).";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
						DECLARE_CACHED_CONST_STRING(std_once_flag_str, "std::once_flag");
						if ((qtype.isConstQualified()) && (is_async_shareable(qtype))) {
							satisfies_checks = true;
						} else if (qtype.getAsString() == const_char_star_str) {
							/* This isn't technically safe, but presumably this is likely
							to be a string literal, which should be fine, so for now we'll
							let it go. */
							satisfies_checks = true;
						} else if (qtype.getAsString() == std_once_flag_str) {
							satisfies_checks = true;
						} else {
							if (MR_ptr && (!errors_suppressed_flag)) {
								const std::string error_desc = std::string("Unable to verify the safety of variable '")
									+ var_qualified_name + "' of type " + get_as_quoted_string_for_errmsg(qtype) + " with 'static storage duration'. "
									+ "'static storage duration' is supported for eligible types wrapped in the "
									+ "'mse::rsv::TStaticImmutableObj<>' transparent template wrapper. Other supported wrappers include: "
									+ "mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
									+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>. "
									+ "Note that objects with 'static storage duration' may be simultaneously accessible from different threads "
									+ "and so have more stringent safety requirements than objects with 'thread_local storage duration'.";
								state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
							}
						}
					} else {
						assert(clang::StorageDuration::SD_Thread == storage_duration);
						if (true || is_async_shareable(qtype)) {
							satisfies_checks = true;
						} else {
							if (MR_ptr && (!errors_suppressed_flag)) {
								const std::string error_desc = std::string("Unable to verify the safety of variable '")
									+ var_qualified_name + "' of type " + get_as_quoted_string_for_errmsg(qtype) + " with 'thread local storage duration'. "
									+ "'thread local storage duration' is supported for eligible types wrapped in the "
									+ "'mse::rsv::TThreadLocalObj<>' transparent template wrapper. Other supported wrappers include: "
									+ "mse::rsv::TStaticImmutableObj<>, mse::rsv::TStaticAtomicObj<>, mse::TAsyncSharedV2ReadWriteAccessRequester<>, mse::TAsyncSharedV2ReadOnlyAccessRequester<>, "
									+ "mse::TAsyncSharedV2ImmutableFixedPointer<> and mse::TAsyncSharedV2AtomicFixedPointer<>.";
								state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
								+ mse_rsv_static_immutable_obj_str1 + "' must be declared to have 'static' storage duration.";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}
				} else if (type_name1 == mse_rsv_ThreadLocalObj_str) {
					if (clang::StorageDuration::SD_Thread != storage_duration) {
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Variable '") + var_qualified_name + "' of type '"
								+ mse_rsv_ThreadLocalObj_str + "' must be declared to have 'thread_local' storage duration.";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
							if (MR_ptr && (!errors_suppressed_flag)) {
								const std::string error_desc = std::string("Uninitialized ")
									+ "scalar variable '" + VD->getNameAsString() + "' (of type '"
									+ qtype.getAsString() + "') ";
								state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
					if (MR_ptr && (!errors_suppressed_flag)) {
						const std::string error_desc = std::string("Reference to variable '")
							+ VD->getNameAsString() + "' before the completion of its "
							+ "construction/initialization is not supported.";
						state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
					}
				}
			} else if (false && VD->isExternallyDeclarable()) {
				if (MR_ptr && (!errors_suppressed_flag)) {
					const std::string error_desc = std::string("\"External\"/inline ")
						+ "variable declarations (such as the declaration of "
						+ VD->getNameAsString() + "' of type " + get_as_quoted_string_for_errmsg(qtype)
						+ ") are not currently supported.";
					state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
				}
			}

			if ((!(retval.has_value())) && (!for_lhs_of_assignment)) {
				/* set the expression lifetime values */
				auto vardecl_scope_lifetime_info = CScopeLifetimeInfo1{};
				vardecl_scope_lifetime_info.m_category = CScopeLifetimeInfo1::ECategory::Automatic;
				vardecl_scope_lifetime_info.m_maybe_containing_scope = get_containing_scope(VD, Ctx);
				vardecl_scope_lifetime_info.m_maybe_source_range = VD->getSourceRange();
				vardecl_scope_lifetime_info.m_maybe_corresponding_cpp_element = VD;

				auto reference_removed_qtype = VD->getType();
				auto VD_qtype = VD->getType();
				IF_DEBUG(auto VD_qtype_str = VD_qtype.getAsString();)
				MSE_RETURN_VALUE_IF_TYPE_IS_NULL_OR_AUTO(VD_qtype, retval);
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
				state1.m_vardecl_rhs_lb_lifetime_values_map.insert_or_assign(VD, vltvs );
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
								if (MR_ptr && (!errors_suppressed_flag)) {
									const std::string error_desc = std::string("(Non-pointer) scalar fields (such those of type ")
										+ get_as_quoted_string_for_errmsg(qtype) + ") require direct initializers.";
									state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
					IF_DEBUG(const auto base_qtype_str = base_qtype.getAsString();)
					if (!is_async_shareable(base_qtype)) {
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableObj<> template, "
								+ get_as_quoted_string_for_errmsg(base_qtype) + ", is eligible to be safely shared (among threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}
				} else {
					/* This branch shouldn't happen. Unless the library's been changed somehow. */
				}
			} else if (mse_rsv_TAsyncPassableObj_str1 == name) {
				if (1 == CXXRD->getNumBases()) {
					const auto& base = *(CXXRD->bases_begin());
					const auto base_qtype = base.getType();
					IF_DEBUG(const auto base_qtype_str = base_qtype.getAsString();)
					if (!is_async_passable(base_qtype)) {
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncPassableObj<> template, "
								+ get_as_quoted_string_for_errmsg(base_qtype) + ", is eligible to be safely passed (between threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}
				} else {
					/* This branch shouldn't happen. Unless the library's been changed somehow. */
				}
			} else if (mse_rsv_TAsyncShareableAndPassableObj_str1 == name) {
				if (1 == CXXRD->getNumBases()) {
					const auto& base = *(CXXRD->bases_begin());
					const auto base_qtype = base.getType();
					IF_DEBUG(const auto base_qtype_str = base_qtype.getAsString();)
					if ((!is_async_shareable(base_qtype)) || (!is_async_passable(base_qtype))) {
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Unable to verify that the ")
								+ "given (adjusted) parameter of the mse::rsv::TAsyncShareableAndPassableObj<> template, "
								+ get_as_quoted_string_for_errmsg(base_qtype) + ", is eligible to be safely shared and passed (among threads). "
								+ "If it is known to be so, then this error can be suppressed with a "
								+ "'check suppression' directive. ";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
					if (MR_ptr && (!errors_suppressed_flag)) {
						const std::string error_desc = std::string("Unsupported use of ")
							+ "mse::rsv::TFParam<> (in type '" + name + "'). ";
						state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
					}
				}
			} else if (qtype.getTypePtr()->isUnionType()) {
				if (MR_ptr && (!errors_suppressed_flag)) {
					const std::string error_desc = std::string("Native unions (such as " + get_as_quoted_string_for_errmsg(qtype) + ") are not ")
						+ "supported. ";
					state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
				}
			} else if (true && (std_unique_ptr_str == name)) {
				if (!qtype.isConstQualified()) {
					if (MR_ptr && (!errors_suppressed_flag)) {
						const std::string error_desc = std::string("std::unique_ptr<>s that are not const qualified are not supported. ")
							+ "Consider using a reference counting pointer from the SaferCPlusPlus library. ";
						state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
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
						if (MR_ptr && (!errors_suppressed_flag)) {
							const std::string error_desc = std::string("Null/default initialization of ")
								+ "std::unique_ptr<>s (such as those of type " + get_as_quoted_string_for_errmsg(qtype)
								+ ") is not supported.";
							state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
						}
					}
				}
			} else {
				auto check_for_and_handle_unsupported_element = [MR_ptr, errors_suppressed_flag, &SR](const clang::QualType& qtype, CTUState& state1) {
					std::string element_name;
					std::string quoted_element_name_for_errmsg;
					const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
					if (l_CXXRD) {
						element_name = l_CXXRD->getQualifiedNameAsString();
						quoted_element_name_for_errmsg = "'" + element_name + "'";
					} else {
						element_name = qtype.getAsString();
						quoted_element_name_for_errmsg = get_as_quoted_string_for_errmsg(qtype);
					}

					{
						auto uei_ptr = unsupported_element_info_ptr(element_name);
						if (uei_ptr) {
							const auto& unsupported_element_info = *uei_ptr;
							if (MR_ptr && (!errors_suppressed_flag)) {
								std::string error_desc = quoted_element_name_for_errmsg + std::string(" is not ")
									+ "supported (in type " + get_as_quoted_string_for_errmsg(qtype) + " used in this declaration). ";
								if ("" != unsupported_element_info.m_recommended_alternative) {
									error_desc += "Consider using " + unsupported_element_info.m_recommended_alternative + " instead.";
								}
								state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
							}
						}
					}
				};
				check_for_and_handle_unsupported_element(qtype, state1);
				apply_to_template_arg_types_if_any(qtype, check_for_and_handle_unsupported_element, state1);
			}
		} else {
			std::string unsupported_type_str;
			if (qtype.getTypePtr()->isArrayType()) {
				unsupported_type_str = "Native array";
			} else if (qtype.getTypePtr()->isUnionType()) {
				unsupported_type_str = "Native union";
			}
			if ("" != unsupported_type_str) {
				if (MR_ptr && (!errors_suppressed_flag)) {
					const std::string error_desc = unsupported_type_str + std::string("s are not ")
						+ "supported (in this declaration of type " + get_as_quoted_string_for_errmsg(qtype) + "). ";
					state1.register_error(*(MR_ptr->SourceManager), SR, error_desc);
				}
			}
		}
		return retval;
	}
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_rhs_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		return evaluate_declaration_lower_bound_lifetimes(state1, DD, Ctx, MR_ptr, Rewrite_ptr);
	}
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_rhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD) {
		return evaluate_declaration_rhs_lower_bound_lifetimes(state1, DD, *(MR.Context), &MR, &Rewrite);
	}
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lhs_lower_bound_lifetimes(CTUState& state1
			, const clang::DeclaratorDecl* DD, ASTContext& Ctx, MatchFinder::MatchResult const * MR_ptr/* = nullptr*/, Rewriter* Rewrite_ptr/* = nullptr*/) {
		return evaluate_declaration_lower_bound_lifetimes(state1, DD, Ctx, MR_ptr, Rewrite_ptr, true/*for_lhs_of_assignment*/);
	}
	inline CMaybeVariableLifetimeValuesWithHints evaluate_declaration_lhs_lower_bound_lifetimes(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::DeclaratorDecl* DD) {
		return evaluate_declaration_lhs_lower_bound_lifetimes(state1, DD, *(MR.Context), &MR, &Rewrite);
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

				bool errors_suppressed_by_location_flag = errors_suppressed_by_location(MR, SR.getBegin());
				auto suppress_check_flag = errors_suppressed_by_location_flag;
				suppress_check_flag |= m_state1.m_suppress_check_region_set.contains(CXXCI, Rewrite, *(MR.Context));
				if (suppress_check_flag) {
					return;
				}
				if (!(CXXCI->isWritten())) {
					/* We think this construction initializer is implicit and generated by the compiler. */
					return;
				}

				std::optional<clang::FieldDecl const *> maybe_FD;
				std::optional<clang::Type const *> maybe_base_class_TypePtr;
				{
					const auto FD = CXXCI->getMember();
					if (FD) {
						IF_DEBUG(const auto name = FD->getNameAsString();)
						maybe_FD = FD;
						process_type_lifetime_annotations(*FD, m_state1, &MR, &Rewrite);
					} else {
						auto base_class_TypePtr = CXXCI->getBaseClass();
						if (base_class_TypePtr) {
							maybe_base_class_TypePtr = base_class_TypePtr;
							auto RD = base_class_TypePtr->getAsRecordDecl();
							if (RD) {
								process_type_lifetime_annotations(*RD, m_state1, &MR, &Rewrite);
							}
							return;
						} else {
							/* unexpected? */
							return;
						}
					}
				}

				const auto init_E = CXXCI->getInit();
				if (init_E) {
					auto CXXCD = Tget_containing_element_of_type<clang::CXXConstructorDecl>(init_E, *(MR.Context));
					if (CXXCD) {
						process_function_lifetime_annotations(*CXXCD, m_state1, &MR, &Rewrite);
					}
					if (maybe_base_class_TypePtr.has_value()) {
						auto CXXRD = Tget_containing_element_of_type<clang::CXXRecordDecl>(init_E, *(MR.Context));
						if (CXXRD) {
							process_type_lifetime_annotations(*CXXRD, m_state1, &MR, &Rewrite);
						}
					}

					auto CXXThisExpr_range = get_contained_elements_of_type_CXXThisExpr(*init_E);
					for (const auto CXXTE : CXXThisExpr_range) {
						assert(CXXTE);
						const auto ME = get_immediately_containing_MemberExpr_from_CXXThisExpr_if_any(*CXXTE, *(MR.Context));
						if (ME) {
							const auto FD2 = get_FieldDecl_from_MemberExpr_if_any(*ME);
							if (FD2) {
								bool res = false;
								if (maybe_FD.has_value()) {
									res = first_is_contained_in_scope_of_second((maybe_FD.value()), FD2, *(MR.Context));
									res &= ((maybe_FD.value()) != FD2);
								}
								if ((!res) && (!suppress_check_flag)) {
									auto MESR = nice_source_range(ME->getSourceRange(), Rewrite);
									if (!MESR.isValid()) {
										MESR = SR;
									}

									const std::string error_desc = std::string("The field '") + FD2->getNameAsString()
										+ "' may be being referenced before it has been constructed.";
									(*this).m_state1.register_error(*MR.SourceManager, MESR, error_desc);
								}
							} else {
								const auto CXXMD = dyn_cast<const CXXMethodDecl>(ME->getMemberDecl());
								if (CXXMD && (!suppress_check_flag)) {
									/* error: Unable to verify that the member function used here can't access part of
									the object that hasn't been constructed yet. */
									const std::string error_desc = std::string("Calling non-static member functions ")
									+ "(such as '" + CXXMD->getQualifiedNameAsString() + "') of an object is not supported in "
									+ "constructor initializers or direct field initializers of the object. Consider "
									+ "using a static member or free function instead. ";
									(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
								} else {
									/* Since this MemberExpr was obtained from a CXXThisExpr, if it doesn't refer to
									a field, then presumably it refers to a (non-static) member function.
									So arriving here would be unexpected. */
									int q = 5;
								}
							}
						} else if (!suppress_check_flag) {
							const std::string error_desc = std::string("Unable to verify that the 'this' pointer ")
							+ "used here can't be used to access part of the object that hasn't been constructed yet.";
							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
						}
					}
					if (maybe_FD.has_value() && is_raw_pointer_or_equivalent((maybe_FD.value())->getType())
						&& (!(*this).m_state1.raw_pointer_scope_restrictions_are_disabled_for_this_pointer_type((maybe_FD.value())->getType()))
						&& (!m_state1.m_suppress_check_region_set.contains(instantiation_source_range((maybe_FD.value())->getSourceRange(), Rewrite)))
						) {
						if (is_nullptr_literal(init_E, *(MR.Context)) && (!suppress_check_flag)) {
							auto CISR = nice_source_range(init_E->getSourceRange(), Rewrite);
							if (!CISR.isValid()) {
								CISR = SR;
							}
							const std::string error_desc = std::string("Null initialization of ")
								+ "native pointer field '" + (maybe_FD.value())->getNameAsString()
								+ "' is not supported.";
							(*this).m_state1.register_error(*MR.SourceManager, CISR, error_desc);
						}

						auto maybe_alts = m_state1.corresponding_abstract_lifetime_set_if_any((maybe_FD.value()));
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
								//slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, (maybe_FD.value())->getType());
								lhs_lifetime_value.m_sublifetimes_vlptr->m_primary_lifetime_infos = { alts.m_primary_lifetimes.front() };

								CScopeLifetimeInfo1 rhs_lifetime_value;
								bool rhs_lifetime_values_evaluated = false;
								std::string rhs_hints;
								auto adj_RHSEX = remove_cast_from_TPointerForLegacy_to_raw_pointer(init_E, *(MR.Context));
								auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(MR, Rewrite, m_state1, adj_RHSEX);
								MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(maybe_expr_lifetime_value);
								if (maybe_expr_lifetime_value.has_value()) {
									CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
									rhs_lifetime_value = expr_slti;
									rhs_lifetime_values_evaluated = true;
								} else {
									auto maybe_rhs_slo = rhs_lifetime_owner_of_pointer_target_if_available(
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
									rhs_lifetime_value = rhs_scope_lifetime_info_from_lifetime_owner(maybe_rhs_slo.value(), *(MR.Context), m_state1);
								}

								bool satisfies_checks = slti_second_can_be_assigned_to_first(lhs_lifetime_value, rhs_lifetime_value, *(MR.Context), m_state1);

								if ((!satisfies_checks) && (!suppress_check_flag)) {
									std::string error_desc = std::string("Unable to verify that this pointer assignment (of type ")
										+ get_as_quoted_string_for_errmsg((maybe_FD.value())->getType()) + ") is safe and valid.";
									std::string hints;
									if ((!rhs_lifetime_values_evaluated) && (!rhs_hints.empty())) {
										hints += " (" + rhs_hints + ")";
									}
									if (hints.empty()) {
										hints += " (Possibly due to being unable to verify that the object(s) referenced by the new pointer live long enough.)";
									}
									error_desc += hints;

									error_desc += " (This pointer assignment occurs in the constructor initializer for member field '" + (maybe_FD.value())->getNameAsString() + "' .)";

									(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
								}

							}
						}
					}
				} else if (maybe_FD.has_value()) {
					auto FD_qtype = (maybe_FD.value())->getType();
					IF_DEBUG(auto FD_qtype_str = FD_qtype.getAsString();)
					MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(FD_qtype);
					if (FD_qtype->isPointerType() && (!suppress_check_flag)) {
						{
							const std::string error_desc = std::string("Default initialization of ")
								+ "native pointer field '" + (maybe_FD.value())->getNameAsString()
								+ "' is not supported.";
							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

				bool errors_suppressed_by_location_flag = errors_suppressed_by_location(MR, SR.getBegin());
				auto suppress_check_flag = errors_suppressed_by_location_flag;
				suppress_check_flag |= state1.m_suppress_check_region_set.contains(LHSEX, Rewrite, *(MR.Context));
				if (suppress_check_flag) {
					return;
				}

				/* In our case, it's significantly harder to verify the safety of a pointer assignment than say,
				a value initialization of a pointer. With initialization, we only need to verify that the target
				object has scope lifetime, as that alone is sufficient to conclude that the target object
				outlives the pointer. Not so with pointer assignment. There we need to obtain an "upper bound"
				for the (scope) lifetime of the (lhs) pointer being modified and a lower bound for the (scope)
				lifetime of the new target object. */

				if (!(LHSEX && RHSEX)) {
					assert(false);
					return;
				}
				auto LHSEX_qtype = LHSEX->getType();
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(LHSEX_qtype);

				IF_DEBUG(const auto LHSEX_qtype_str = LHSEX_qtype.getAsString();)
				const auto LHSEX_rw_type_ptr = remove_mse_transparent_wrappers(LHSEX_qtype).getTypePtr();
				assert(LHSEX_rw_type_ptr);
				if (!is_raw_pointer_or_equivalent(LHSEX_rw_type_ptr)) {
					return;
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
				auto maybe_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(MR, Rewrite, state1, adj_RHSEX);
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
					auto maybe_rhs_slo = rhs_lifetime_owner_of_pointer_target_if_available(
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
					rhs_lifetime_value = rhs_scope_lifetime_info_from_lifetime_owner(maybe_rhs_slo.value(), *(MR.Context), state1);
				}

				CScopeLifetimeInfo1 lhs_lifetime_value;
				lhs_lifetime_value.m_category = CScopeLifetimeInfo1::ECategory::Literal;
				lhs_lifetime_value.m_maybe_containing_scope = get_containing_scope(LHSEX, *(MR.Context));
				lhs_lifetime_value.m_maybe_source_range = LHSEX->getSourceRange();
				lhs_lifetime_value.m_maybe_corresponding_cpp_element = LHSEX;
				slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, LHSEX_qtype);
				bool lhs_lifetime_values_evaluated = false;

				{
					auto maybe_expr_lifetime_value = evaluate_expression_lhs_lower_bound_lifetimes(MR, Rewrite, state1, LHSEX);
					if (maybe_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
						/* Cannot properly evaluate because this is a template definition. Proper evaluation should
						occur in any instantiation of the template. */
						return;
					} else if (maybe_expr_lifetime_value.has_value()) {
						CScopeLifetimeInfo1& expr_slti = maybe_expr_lifetime_value.value().m_scope_lifetime_info;
						slti_set_default_lower_bound_lifetimes_where_needed(expr_slti, LHSEX_qtype);
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
					auto maybe_lhs_slo = lhs_lifetime_owner_if_available(LHSEX, *(MR.Context), state1);

					if (maybe_lhs_slo.has_value()) {
						auto& lhs_slo = maybe_lhs_slo.value();

						lhs_lifetime_value = lhs_scope_lifetime_info_from_lifetime_owner(lhs_slo, *(MR.Context), state1);
						slti_set_default_lower_bound_lifetimes_where_needed(lhs_lifetime_value, LHSEX_qtype);
						lhs_lifetime_values_evaluated = true;
					} else {
						lhs_hints = maybe_lhs_slo.hints_str();
					}
				}

				/* We support assignment between annotated and non-annotated pointers. But the structure of their
				lifetime values can be different, so we have to adjust them as necessary to match before comparing
				them. */
				slti_set_default_lower_bound_lifetimes_to_match_where_needed(lhs_lifetime_value, rhs_lifetime_value);

				satisfies_checks = slti_second_can_be_assigned_to_first(lhs_lifetime_value, rhs_lifetime_value, *(MR.Context), state1);

				if (!satisfies_checks) {
					std::string error_desc = std::string("Unable to verify that this pointer assignment (of type ")
						+ get_as_quoted_string_for_errmsg(LHSEX_qtype) + ") is safe and valid.";
					std::string hints;
					if ((!lhs_lifetime_values_evaluated) && (!lhs_hints.empty())) {
						hints += " (" + lhs_hints + ")";
					}
					if ((!rhs_lifetime_values_evaluated) && (!rhs_hints.empty())) {
						hints += " (" + rhs_hints + ")";
					}
					if (hints.empty()) {
						hints += " (Possibly due to being unable to verify that the object(s) referenced by the new pointer live long enough.)";
					}
					error_desc += hints;

					auto FND = enclosing_function_if_any(LHSEX, *(MR.Context));
					if (FND) {
						if (FND->isDefaulted()) {
							error_desc += " (This pointer assignment is contained in the (possibly implicit/default member) function '" + FND->getNameAsString() + "' .)";
						}
					}

					state1.register_error(*MR.SourceManager, SR, error_desc);
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
				if (!(LHSEX && RHSEX)) {
					assert(false);
					return;
				}
				auto LHSEX_qtype = LHSEX->getType();
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(LHSEX_qtype);

				IF_DEBUG(const auto LHSEX_qtype_str = LHSEX_qtype.getAsString();)
				const auto LHSEX_rw_type_ptr = remove_mse_transparent_wrappers(LHSEX_qtype).getTypePtr();
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

				auto maybe_lhs_expr_lifetime_value = evaluate_expression_lhs_lower_bound_lifetimes(MR, Rewrite, m_state1, LHSEX);
				if (maybe_lhs_expr_lifetime_value.m_failure_due_to_dependent_type_flag) {
					/* Cannot properly evaluate because this is a template definition. Proper evaluation should
					occur in any instantiation of the template. */
					return;
				}
				if (maybe_lhs_expr_lifetime_value.has_value()) {
					CScopeLifetimeInfo1& lhs_expr_slti = maybe_lhs_expr_lifetime_value.value().m_scope_lifetime_info;
					lhs_lifetime_values_evaluated = true;
					auto maybe_rhs_expr_lifetime_value = evaluate_expression_rhs_lower_bound_lifetimes(MR, Rewrite, m_state1, RHSEX);
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
					std::string error_desc = std::string("Unable to verify that this assignment (of type ")
						+ get_as_quoted_string_for_errmsg(LHSEX_qtype) + ") satisfies the (annotated) lifetime constraints of the type.";
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

					(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

				auto EX_qtype = EX->getType();
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(EX_qtype);

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
							const auto FNDSR = function_decl->getSourceRange();
							bool is_potentially_from_standard_header = FNDSR.getBegin().isInvalid()
								|| MR.SourceManager->isInSystemHeader(FNDSR.getBegin());
							if (!is_potentially_from_standard_header) {
								/*
								bool filename_is_invalid = false;
								std::string full_path_name = MR.SourceManager->getBufferName(FNDSR.getBegin(), &filename_is_invalid);
								static const std::string built_in_str = "<built-in>";
								is_potentially_from_standard_header |= (built_in_str == full_path_name);
								*/
								/* errors_suppressed_by_location() returns true for SaferCPlusPlus headers as well
								as standard and system headers.  */
								//is_potentially_from_standard_header |= filtered_out_by_location(*(MR.SourceManager), FNDSR.getBegin());
								//is_potentially_from_standard_header |= errors_suppressed_by_location(*(MR.SourceManager), FNDSR.getBegin());
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
												+ "an argument type (" + get_as_quoted_string_for_errmsg(arg_EX->getType()) + ") that yields scope pointers (unconditionally via the "
												+ "'operator &' of some component). "
												+ "(In particular, explicit use of std::move() with 'mse::TXScopeOwnerPointer<>' "
												+ "or any object that might contain an 'mse::TXScopeOwnerPointer<>' is not supported.) ";
											(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
												(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
											}
										}
									}
								} else {
									static const std::string malloc_str = "malloc";
									static const std::string realloc_str = "realloc";
									static const std::string free_str = "free";
									static const std::string calloc_str = "calloc";
									static const std::string alloca_str = "alloca";
									static const std::string std_allocator_str = "std::allocator";
									static const std::string std_allocator_traits_str = "std::allocator_traits";
									static const std::string std_destroy_str = "std::destroy";
									static const std::string std_construct_str = "std::construct";
									static const std::string std_ranges_destroy_str = "std::ranges::destroy";
									static const std::string std_ranges_construct_str = "std::ranges::construct";
									std::string unsupported_function_str;
									if ((malloc_str == qualified_function_name)
										|| (realloc_str == qualified_function_name)
										|| (free_str == qualified_function_name)
										|| (calloc_str == qualified_function_name)
										|| (alloca_str == qualified_function_name)
										/* note that `new` and `delete` are operators (rather than functions) and addressed elsewhere */
										|| (string_begins_with(qualified_function_name, std_allocator_str))
										|| (string_begins_with(qualified_function_name, std_allocator_traits_str))
										|| (string_begins_with(qualified_function_name, std_destroy_str))
										|| (string_begins_with(qualified_function_name, std_construct_str))
										|| (string_begins_with(qualified_function_name, std_ranges_destroy_str))
										|| (string_begins_with(qualified_function_name, std_ranges_construct_str))
										) {

										unsupported_function_str = qualified_function_name;
									}
									if ("" != unsupported_function_str) {
										const std::string error_desc = std::string("The '") + unsupported_function_str
											+ "' function is not supported.";
										(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
													//MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(param_qtype);
													if (qtype.isNull()) {
														return;
													}
													if (param_qtype->isReferenceType()) {
														auto arg_SR = arg_EX_ii->getSourceRange();
														if (arg_SR.isInvalid()) {
															arg_SR = SR;
														}
														const std::string error_desc = std::string("Unable to verify the safety ")
															+ "of the native reference parameter, '" + param->getNameAsString()
															+ "', of the function object being passed (to opaque "
															+ "function/method/operator '" + function_decl->getQualifiedNameAsString() + "') here.";
														(*this).m_state1.register_error(*MR.SourceManager, arg_SR, error_desc);
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
																+ "of a native reference parameter of type " + get_as_quoted_string_for_errmsg(param_type)
																+ ", of the function being passed (to an opaque "
																+ "function/method/operator '" + function_decl->getQualifiedNameAsString() + "') here.";
															(*this).m_state1.register_error(*MR.SourceManager, arg_SR, error_desc);
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
													(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
													(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
							IF_DEBUG(auto CXXSVIE_qtype_str = CXXSVIE->getType().getAsString();)
							unsupported_expression_str = "Default construction of scalar types (such as " + get_as_quoted_string_for_errmsg(CXXSVIE->getType()) + ")";
						}
						if ("" != unsupported_expression_str) {
							const std::string error_desc = unsupported_expression_str
								+ " is not supported.";
							(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
						}

					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	inline void check_for_unannotated_reference_objects_in_dynamic_containers(clang::Decl const& decl_cref, clang::QualType qtype, CTUState& state1, MatchFinder::MatchResult const * MR_ptr = nullptr, Rewriter* Rewrite_ptr = nullptr) {
		auto& root_decl_cref = decl_cref;
		auto lambda1 = [&root_decl_cref, &state1, &MR_ptr, &Rewrite_ptr](clang::QualType qtype, std::optional<clang::Decl const *> maybe_D = {}, std::optional<clang::CXXBaseSpecifier const *> maybe_CXXBS = {}) {
			thread_local std::unordered_set<clang::Type const *> tl_already_processed_set;
			auto found_it = tl_already_processed_set.find(qtype.getTypePtr());
			if (tl_already_processed_set.end() != found_it) {
				return;
			} else {
				tl_already_processed_set.insert(qtype.getTypePtr());
			}

			if (is_recognized_protected_dynamic_owning_container(qtype)) {
				auto& maybe_dynamic_owning_container_D = maybe_D;

				auto check_for_unannotated_reference_shallow = 
					[&root_decl_cref, &maybe_dynamic_owning_container_D, &state1, &MR_ptr, &Rewrite_ptr]
					(clang::QualType qtype, std::optional<clang::Decl const *> maybe_D = {}, std::optional<clang::CXXBaseSpecifier const *> maybe_CXXBS = {}) {

					IF_DEBUG(auto qtype_str = qtype.getAsString();)

					thread_local std::unordered_set<clang::Type const *> tl_already_processed_set;
					auto found_it = tl_already_processed_set.find(qtype.getTypePtr());
					if (tl_already_processed_set.end() != found_it) {
						return;
					}

					auto available_D = &root_decl_cref;
					if (maybe_D.has_value() && maybe_D.value()) {
						available_D = maybe_D.value();
					} else if (maybe_dynamic_owning_container_D.has_value() && maybe_dynamic_owning_container_D.value()) {
						available_D = maybe_dynamic_owning_container_D.value();
					}
					auto SR = Rewrite_ptr ? nice_source_range(available_D->getSourceRange(), *Rewrite_ptr)
						: available_D->getSourceRange();

					RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
					if (MR_ptr) {
						auto MR = *MR_ptr;
						RETURN_IF_FILTERED_OUT_BY_LOCATION1;
					}

					if (MR_ptr && MR_ptr->Context && Rewrite_ptr) {
						auto suppress_check_flag = state1.m_suppress_check_region_set.contains(available_D, *Rewrite_ptr, *(MR_ptr->Context));
						if (suppress_check_flag) {
							return;
						}
					}

					auto maybe_tlt_annotations = type_lifetime_annotations_if_available(qtype, state1, MR_ptr, Rewrite_ptr, true/*use_implicit_lifetime_annotation_for_pointer*/);

					if (maybe_tlt_annotations.has_value() && (1 <= maybe_tlt_annotations.value()->m_lifetime_set.m_primary_lifetimes.size())) {
						bool is_not_lt_annotated = false;
						bool is_field = false;
						bool is_base_class = false;

						if (maybe_D.has_value()) {
							auto D = maybe_D.value();
							auto DD = dyn_cast<DeclaratorDecl>(D);
							if (DD) {
								if (get_cannonical_type(DD->getType()) == get_cannonical_type(qtype)) {
									auto FD = dyn_cast<const clang::FieldDecl>(DD);
									if (FD) {
										is_field = true;
										auto maybe_abstract_lifetime_set = state1.corresponding_abstract_lifetime_set_if_any(FD);
										if ((!maybe_abstract_lifetime_set.has_value()) || (0 == maybe_abstract_lifetime_set.value().m_primary_lifetimes.size())) {
											is_not_lt_annotated = true;
										}
									}
								}
							}
						}
						if ((!is_not_lt_annotated) && maybe_CXXBS.has_value()) {
							auto CXXBS = maybe_CXXBS.value();
							is_base_class = true;
							auto maybe_abstract_lifetime_set = state1.corresponding_abstract_lifetime_set_if_any(CXXBS);
							if ((!maybe_abstract_lifetime_set.has_value()) || (0 == maybe_abstract_lifetime_set.value().m_primary_lifetimes.size())) {
								is_not_lt_annotated = true;
							}
						}
						if ((!is_not_lt_annotated) && (!is_field)) {
							if (qtype->isReferenceType() || is_raw_pointer_or_equivalent(qtype)) {
								is_not_lt_annotated = true;
							}
						}

						if (is_not_lt_annotated) {
							if (MR_ptr) {
								auto dynamic_owning_container_SR = SR;
								std::string of_container_type_str;

								if (maybe_dynamic_owning_container_D.has_value() && maybe_dynamic_owning_container_D.value()) {
									auto dynamic_owning_container_D = maybe_dynamic_owning_container_D.value();

									dynamic_owning_container_SR = Rewrite_ptr ? nice_source_range(dynamic_owning_container_D->getSourceRange(), *Rewrite_ptr)
										: dynamic_owning_container_D->getSourceRange();

									std::optional<clang::QualType> maybe_container_qtype;
									auto VD = dyn_cast<const clang::VarDecl>(dynamic_owning_container_D);
									if (VD) {
										maybe_container_qtype = VD->getType();
									} else {
										auto FD = dyn_cast<const clang::FieldDecl>(dynamic_owning_container_D);
										if (FD) {
											maybe_container_qtype = FD->getType();
										}
									}
									if (maybe_container_qtype.has_value()) {
										auto container_qtype = maybe_container_qtype.value();
	
										of_container_type_str = std::string(" (of type ") + get_as_quoted_string_for_errmsg(container_qtype) + ")";
									}
								}

								const std::string field_str = is_field ? " field" : "";
								const std::string base_class_str = is_base_class ? " base class" : "";

								const std::string error_desc = std::string("Unannotated reference object") + field_str + base_class_str + " (of type " + get_as_quoted_string_for_errmsg(qtype)
									+ ") contained in dynamic container" + of_container_type_str + " is not supported.";
								state1.register_error(*(MR_ptr->SourceManager), dynamic_owning_container_SR, error_desc);
							}
						}
					}
				};

				apply_to_all_owned_types(qtype, check_for_unannotated_reference_shallow, maybe_D);
			}
		};

		apply_to_all_owned_types(qtype, lambda1, &decl_cref);
	}

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

				auto ebai_scope_obj = (*this).m_state1.make_element_being_analyzed_info_scope_obj({SR});

				auto TD = dyn_cast<const clang::TypeDecl>(D);
				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();
					auto DD_qtype = DD->getType();
					IF_DEBUG(auto DD_qtype_str = DD_qtype.getAsString();)
					MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(DD_qtype);
					const auto TST = DD_qtype->getAs<clang::TemplateSpecializationType>();

					check_for_unannotated_reference_objects_in_dynamic_containers(*DD, qtype, m_state1, &MR, &Rewrite);

					auto rhs_res = evaluate_declaration_rhs_lower_bound_lifetimes(MR, Rewrite, m_state1, DD);
					MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(rhs_res);

					auto lhs_res = evaluate_declaration_lhs_lower_bound_lifetimes(MR, Rewrite, m_state1, DD);
					MSE_RETURN_IF_FAILURE_DUE_TO_DEPENDENT_TYPE(lhs_res);
				} else if (false && TD) {
					/* We no longer evaluate uninstantated templates. Too many subtle ways processing can go wrong. */
					clang::Type const * TypePtr = nullptr;
					if (MR.Context) {
						auto qtype = MR.Context->getTypeDeclType(TD);
						MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype);
						TypePtr = qtype.getTypePtr();
					}
					{
						const auto CXXRD = TypePtr->getAsCXXRecordDecl();
						if (CXXRD) {
							IF_DEBUG(auto qname = CXXRD->getQualifiedNameAsString();)
							auto tskind = CXXRD->getTemplateSpecializationKind();
							if (clang::TemplateSpecializationKind::TSK_Undeclared == tskind) {
								return;
							}
						}
					}

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
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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
						MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(D_qtype);
						auto DRE_qtype = DRE->getType();
						IF_DEBUG(auto DRE_qtype_str = DRE_qtype.getAsString();)
						MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(DRE_qtype);
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
								(*this).m_state1.register_error(*MR.SourceManager, SR, error_desc);
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

				{
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
					binaryOperator(hasOperatorName("<=")), binaryOperator(hasOperatorName("(")),
					binaryOperator(hasOperatorName(">=")), binaryOperator(hasOperatorName(")")),
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
