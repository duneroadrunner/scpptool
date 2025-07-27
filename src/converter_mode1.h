// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef __CONVERTER_MODE1_H
#define __CONVERTER_MODE1_H

#include <cctype>
#include <cstddef>
#include <optional>
#include <utility>
#ifndef EXCLUDE_CONVERTER_MODE1

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
#include <unordered_set>
#include <set>
#include <list>
#include <algorithm>
#include <limits>
#include <locale>
#include <type_traits>
#include <string_view>

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

#define RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1 \
				if ((!SR.isValid()) || filtered_out_by_location<options_t<converter_mode_t> >(MR, SR.getBegin())) { \
					return void(); \
				}

#define RETURN_IF_DEPENDENT_TYPE_CONV1(qtype) \
				if (qtype.isNull() || qtype->isDependentType()) { \
					return void(); \
				}

namespace convm1 {
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
	bool AddressableVars = false;

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
		bool AddressableVars = false;
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

	inline auto ignore_parens_qtype(clang::QualType qtype) {
		while (llvm::isa<const clang::ParenType>(qtype)) {
			auto PRNT = llvm::cast<const clang::ParenType>(qtype);
			if (PRNT) {
				qtype = PRNT->getInnerType();
				IF_DEBUG(std::string qtype_str = qtype.getAsString();)
				int q = 5;
			} else { assert(false); }
		}
		return qtype;
	}

	/* If typeLoc refers to a typedef, then we'll return a TypeLoc that refers to the definition
	in the typedef. */
	inline auto definition_TypeLoc(clang::TypeLoc typeLoc) {
		auto tdtl = typeLoc.getAsAdjusted<clang::TypedefTypeLoc>();
		while (tdtl) {
			auto TDND = tdtl.getTypedefNameDecl();
			auto tsi = TDND->getTypeSourceInfo();
			if (tsi) {
				typeLoc = tsi->getTypeLoc();
				tdtl = typeLoc.getAsAdjusted<clang::TypedefTypeLoc>();
			} else {
				assert(false);
			}
		}
		return typeLoc;
	}

	class CDDeclConversionState;

	class CFunctionTypeState {
	public:
		//CFunctionTypeState() = default;
		CFunctionTypeState(const CFunctionTypeState&) = default;
		CFunctionTypeState(CFunctionTypeState&&) = default;
		CFunctionTypeState(const std::vector<clang::QualType>& param_qtypes = std::vector<clang::QualType>(),
				const std::optional<clang::FunctionProtoTypeLoc>& maybe_functionProtoTypeLoc = {})
				: m_param_qtypes_original(param_qtypes), m_param_qtypes_current(param_qtypes), m_current_qtypes_are_current(true),
				m_maybe_functionProtoTypeLoc(maybe_functionProtoTypeLoc) {}
		CFunctionTypeState& operator=(const CFunctionTypeState&) = default;
		CFunctionTypeState& operator=(CFunctionTypeState&&) = default;
		bool current_qtypes_are_current() const { return m_current_qtypes_are_current; }
		bool has_been_changed() const {
			bool retval = false;
			if (("" != m_params_current_str) || (m_function_decl_ptr) || (m_param_qtypes_original != m_param_qtypes_current)) {
				retval = true;
			}
			return retval;
		}
		void set_params_current_str(const std::string& params_current_str) {
			m_params_current_str = params_current_str;
			m_current_qtypes_are_current = false;
		}
		bool operator==(const CFunctionTypeState& other) const {
			return ((other.m_param_qtypes_original == m_param_qtypes_original)
				&& (other.m_param_qtypes_current == m_param_qtypes_current)
				&& (other.m_current_qtypes_are_current == m_current_qtypes_are_current)
				&& (other.m_params_current_str == m_params_current_str)
				&& (other.m_function_decl_ptr == m_function_decl_ptr)
				&& (other.m_maybe_functionProtoTypeLoc.has_value() == m_maybe_functionProtoTypeLoc.has_value())
				);
		}
		std::vector<clang::QualType> m_param_qtypes_original;
		std::vector<clang::QualType> m_param_qtypes_current;
		std::vector<std::shared_ptr<CDDeclConversionState> > m_param_conversion_state_shptrs;
		bool m_current_qtypes_are_current = false;
		std::string m_params_current_str;
		const clang::FunctionDecl* m_function_decl_ptr = nullptr;
		std::optional<clang::FunctionProtoTypeLoc> m_maybe_functionProtoTypeLoc;
	};

	/* We use the term "indirect type" to mean basically a type the "dereferences" to another
	specifiable type. At least initially, we primarily use it to refer to native pointers and arrays
	(that are candidates for being converted to safer counterparts). */
	/* CIndirectionState holds information used to determine what (safe) type, if any, the
	associated "indirect type" instance should be converted to. */
	class CIndirectionState {
	public:
		CIndirectionState() = default;
		CIndirectionState(const CIndirectionState&) = default;
		CIndirectionState(CIndirectionState&&) = default;
		CIndirectionState(const std::optional<clang::TypeLoc>& maybe_typeLoc, const std::string& original, const std::string& current,
				bool current_is_function_type = false, const std::vector<clang::QualType>& param_qtypes = std::vector<clang::QualType>(),
				const std::optional<clang::FunctionProtoTypeLoc>& maybe_functionProtoTypeLoc = {})
			: m_original_species(original), m_current_species(current), m_current_is_function_type(current_is_function_type)
				, m_maybe_typeLoc(maybe_typeLoc), m_function_type_state(param_qtypes, maybe_functionProtoTypeLoc) {
					set_current_species(current);
				}
		CIndirectionState(const std::optional<clang::TypeLoc>& maybe_typeLoc, const std::string& original, const std::string& current,
				const std::string& array_size_expr)
			: m_original_species(original), m_current_species(current), m_array_size_expr(array_size_expr), m_maybe_typeLoc(maybe_typeLoc) {
				set_current_species(current);
			}
		CIndirectionState(const std::string& original, const std::string& current,
				bool current_is_function_type = false, const std::vector<clang::QualType>& param_qtypes = std::vector<clang::QualType>(),
				const std::optional<clang::FunctionProtoTypeLoc>& maybe_functionProtoTypeLoc = {})
			: m_original_species(original), m_current_species(current), m_current_is_function_type(current_is_function_type),
				m_function_type_state(param_qtypes, maybe_functionProtoTypeLoc) {
				set_current_species(current);
			}
		CIndirectionState(const std::string& original, const std::string& current,
				const std::string& array_size_expr)
			: m_original_species(original), m_current_species(current), m_array_size_expr(array_size_expr) {
				set_current_species(current);
			}

		bool current_is_function_type() const { return m_current_is_function_type; }

		void set_current_species(const std::string& new_current) {
			m_current_species = new_current;

			m_indirection_properties1.m_known_to_have_malloc_target = false;
			m_indirection_properties1.m_known_to_have_non_malloc_target = false;
			m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator = false;
			if ("native pointer" == new_current) {
			} else if ("malloc target" == new_current) {
				m_indirection_properties1.m_known_to_have_malloc_target = true;
			} else if ("non-malloc target" == new_current) {
				m_indirection_properties1.m_known_to_have_non_malloc_target = true;
			} else if ("variously malloc and non-malloc target" == new_current) {
				m_indirection_properties1.m_known_to_have_malloc_target = true;
				m_indirection_properties1.m_known_to_have_non_malloc_target = true;
			} else {
				m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator = true;
				if ("inferred array" == new_current) {
				} else if ("dynamic array" == new_current) {
					m_indirection_properties1.m_known_to_have_malloc_target = true;
				} else if ("native array" == new_current) {
					m_indirection_properties1.m_known_to_have_non_malloc_target = true;
				} else if ("variously native and dynamic array" == new_current) {
					m_indirection_properties1.m_known_to_have_malloc_target = true;
					m_indirection_properties1.m_known_to_have_non_malloc_target = true;
				} else if ("native reference" == new_current) {
					int q = 5;
				} else {
					/* unexpected */
					int q = 3;
				}
			}
		}
		std::string generate_current_species_string() const {
			if ("native reference" == m_current_species) {
				return m_current_species;
			}
			std::string generated_species_string;
			if (m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator) {
				if (m_indirection_properties1.m_known_to_have_malloc_target) {
					if (m_indirection_properties1.m_known_to_have_non_malloc_target) {
						generated_species_string = "variously native and dynamic array";
					} else {
						generated_species_string = "dynamic array";
					}
				} else {
					if (m_indirection_properties1.m_known_to_have_non_malloc_target) {
						generated_species_string = "native array";
					} else {
						generated_species_string = "inferred array";
					}
				}
			} else {
				if (m_indirection_properties1.m_known_to_have_malloc_target) {
					if (m_indirection_properties1.m_known_to_have_non_malloc_target) {
						generated_species_string = "variously malloc and non-malloc target";
					} else {
						generated_species_string = "malloc target";
					}
				} else {
					if (m_indirection_properties1.m_known_to_have_non_malloc_target) {
						generated_species_string = "non-malloc target";
					} else {
						generated_species_string = "native pointer";
					}
				}
			}
			return generated_species_string;
		}
		const std::string& current_species() const {
			const auto generated_current_species_string = generate_current_species_string();
			if (("native reference" != m_current_species) && (generated_current_species_string != m_current_species)) {
				int q = 3;
			}
			return m_current_species;
		}
		const std::string& original_species() const {
			return m_original_species;
		}
		struct CIndirectionProperties1 {
			bool m_known_to_have_malloc_target = false;
			bool m_known_to_have_non_malloc_target = false;
			bool m_is_known_to_be_used_as_an_array_iterator = false;
			bool operator==(const CIndirectionProperties1& rhs) const {
				return (m_known_to_have_malloc_target == rhs.m_known_to_have_malloc_target)
					&& (m_known_to_have_non_malloc_target == rhs.m_known_to_have_non_malloc_target)
					&& (m_is_known_to_be_used_as_an_array_iterator == rhs.m_is_known_to_be_used_as_an_array_iterator);
			}
			bool operator!=(const CIndirectionProperties1& rhs) const {
				return !((*this) == rhs);
			}
		};
		bool is_known_to_have_malloc_target() const {
			return m_indirection_properties1.m_known_to_have_malloc_target;
		}
		void set_is_known_to_have_malloc_target(bool val = true) {
			m_indirection_properties1.m_known_to_have_malloc_target = val;
			m_current_species = generate_current_species_string();
		}
		bool is_known_to_have_non_malloc_target() const {
			return m_indirection_properties1.m_known_to_have_non_malloc_target;
		}
		void set_is_known_to_have_non_malloc_target(bool val = true) {
			m_indirection_properties1.m_known_to_have_non_malloc_target = val;
			m_current_species = generate_current_species_string();
		}
		bool is_known_to_be_used_as_an_array_iterator() const {
			return m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator;
		}
		void set_is_known_to_be_used_as_an_array_iterator(bool val = true) {
			m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator = val;
			m_current_species = generate_current_species_string();
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
		bool is_known_to_be_a_pointer_target() const {
			return "pointer target" == m_current_pointer_target_state;
		}
		void set_is_known_to_be_a_pointer_target(bool val = true) {
			if (val) {
				m_current_pointer_target_state = "pointer target";
			} else {
				m_current_pointer_target_state = "";
			}
		}
		void set_xscope_eligibility(bool eligibility) {
			m_is_ineligible_for_xscope_status = (!eligibility);
		}
		auto xscope_eligibility() const {
			return (!m_is_ineligible_for_xscope_status);
		}
		bool has_been_determined_to_point_to_an_array() const {
			return is_known_to_be_used_as_an_array_iterator();
		}
		bool has_been_determined_to_point_to_a_dynamic_array() const {
			bool retval = false;
			if (m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator) {
				if (m_indirection_properties1.m_known_to_have_malloc_target) {
					if (true || (!(m_indirection_properties1.m_known_to_have_non_malloc_target))) {
						retval = true;
					}
				}
			}
			return retval;
		}
		bool has_been_determined_to_point_to_a_native_array() const {
			bool retval = false;
			if (m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator) {
				if (m_indirection_properties1.m_known_to_have_non_malloc_target) {
					if (true || (!(m_indirection_properties1.m_known_to_have_malloc_target))) {
						retval = true;
					}
				}
			}
			return retval;
		}
		bool is_a_pointer_that_has_not_been_determined_to_be_an_array() const {
			bool retval = false;
			if (!m_indirection_properties1.m_is_known_to_be_used_as_an_array_iterator) {
				retval = true;
			}
			return retval;
		}
		std::string original_source_text() const {
			return m_original_source_text;
		}
		std::string return_type_original_source_text() const {
			if ("" != m_return_type_original_source_text) {
				return m_return_type_original_source_text;
			}
			return m_original_source_text;
		}

	private:
		std::string m_original_species;
		std::string m_current_species;

	public:
		CIndirectionProperties1 m_indirection_properties1;
		std::string m_array_size_expr;
		bool m_array_size_expr_read_from_source_text = false;

		bool m_current_is_function_type = false;
		CFunctionTypeState m_function_type_state;
		
		std::optional<clang::QualType> m_maybe_original_qtype;
		std::optional<clang::TypeLoc> m_maybe_typeLoc;
		std::string m_prefix_str;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_prefix_SR_or_insert_before_point;
		std::string m_suffix_str;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_suffix_SR_or_insert_before_point;
		std::optional<clang::SourceRange> m_maybe_source_range;
		std::optional<clang::SourceRange> m_maybe_source_range_including_any_const_qualifier;
		std::optional<clang::SourceRange> m_maybe_typedef_definition_source_range;
		std::optional<clang::SourceRange> m_maybe_typedef_definition_source_range_including_any_const_qualifier;
		std::optional<clang::SourceRange> m_maybe_prefix_source_range;
		std::optional<clang::SourceRange> m_maybe_suffix_source_range;
		std::string m_original_source_text;
		std::string m_return_type_original_source_text;

		std::string m_original_pointer_target_state;
		std::string m_current_pointer_target_state;

		bool m_is_ineligible_for_xscope_status = false;
	};

	inline std::string adjusted_qtype_str(std::string qtype_str, std::optional<clang::QualType> const maybe_qtype = {}) {
		static const std::string clang_system_bool_str = "_Bool";
		static const std::string const_clang_system_bool_str = "const " + clang_system_bool_str;
		if (std::string::npos != qtype_str.find(clang_system_bool_str)) {
			if (clang_system_bool_str == qtype_str) {
				qtype_str = "bool";
			} else if (const_clang_system_bool_str == qtype_str) {
				qtype_str = "const bool";
			} else {
				auto found_range = Parse::find_uncommented_token(clang_system_bool_str, qtype_str);
				while (found_range.begin < qtype_str.length()) {
					qtype_str.replace(found_range.begin, found_range.end - found_range.begin, "bool");
					found_range = Parse::find_uncommented_token(clang_system_bool_str, qtype_str);
				}
			}
		}
		return qtype_str;
	}

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
		std::optional<clang::QualType> maybe_original_qtype() const { return m_maybe_original_qtype; }
		void set_original_source_text(std::string str) {
#ifndef NDEBUG
			if ("void" == str) {
				int q = 5;
			}
#endif /*!NDEBUG*/
			m_original_source_text = std::move(str);
		}
		void set_return_type_original_source_text(std::string str) {
			m_return_type_original_source_text = std::move(str);
		}
		void set_current_non_function_qtype_str(const std::string& qtype_str) {
			m_current_qtype_or_return_qtype_str = adjusted_qtype_str(qtype_str);
			m_current_qtype_is_current = false;
		}
		void set_current_function_qtype_str(const std::string& return_qtype_str, const std::string& params_str) {
			m_current_qtype_or_return_qtype_str = adjusted_qtype_str(return_qtype_str);
			m_function_type_state.set_params_current_str(params_str);
			m_current_qtype_is_current = false;
		}
		std::string original_qtype_str() const {
			std::string retval;
			{
				if (!m_maybe_original_qtype.has_value()) {
					assert(false);
				} else {
					retval = m_maybe_original_qtype.value().getAsString();
				}
			}
			return retval;
		}
		std::string original_definition_qtype_str() const {
			std::string retval;
			{
				if (!m_maybe_original_qtype.has_value()) {
					assert(false);
				} else {
					retval = definition_qtype(m_maybe_original_qtype.value()).getAsString();
				}
			}
			return retval;
		}
		std::string original_source_text() const {
			return m_original_source_text;
		}
		std::string return_type_original_source_text() const {
			if ("" != m_return_type_original_source_text) {
				return m_return_type_original_source_text;
			}
			return m_original_source_text;
		}
		std::string current_qtype_str() const {
			if (m_current_qtype_is_current) {
				if (!m_maybe_current_qtype.has_value()) {
					assert(false);
				} else {
					auto qtype = m_maybe_current_qtype.value();
					if (llvm::isa<const clang::FunctionType>(qtype)) {
						auto FNT = llvm::cast<const clang::FunctionType>(qtype);
						if (FNT && (!(m_function_type_state.m_params_current_str.empty()))) {
							std::string function_type_str = (m_current_qtype_or_return_qtype_str.empty())
								? adjusted_qtype_str(FNT->getReturnType().getAsString()) : m_current_qtype_or_return_qtype_str;
							function_type_str += m_function_type_state.m_params_current_str;
							return function_type_str;
						}
					}
					return adjusted_qtype_str(qtype.getAsString(), qtype);
				}
			}
			if (m_maybe_original_qtype.has_value()) {
				if (m_maybe_original_qtype.value()->isFunctionType()) {
					return m_current_qtype_or_return_qtype_str + m_function_type_state.m_params_current_str;
				} else {
					return m_current_qtype_or_return_qtype_str;
				}
			}
			return m_current_qtype_or_return_qtype_str + m_function_type_state.m_params_current_str;
		}
		/* If the type is a function type, then just the function return type is returned (as a
		string). Otherwise the type is returned (as a string). */
		std::string current_return_qtype_str() const {
			if (m_current_qtype_is_current) {
				if (!m_maybe_current_qtype.has_value()) {
					assert(false);
				} else {
					auto qtype = m_maybe_current_qtype.value();
					if (llvm::isa<const clang::FunctionType>(qtype)) {
						auto FNT = llvm::cast<const clang::FunctionType>(qtype);
						if (FNT) {
							assert(m_current_qtype_or_return_qtype_str.empty());
							return adjusted_qtype_str(FNT->getReturnType().getAsString());
						}
					}
					return adjusted_qtype_str(qtype.getAsString());
				}
			}
			if (!m_current_qtype_or_return_qtype_str.empty()) {
				return m_current_qtype_or_return_qtype_str;
			}
			return current_qtype_str();
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
				} else if (m_maybe_original_qtype.value().getAsString() != m_current_qtype_or_return_qtype_str)  {
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
		bool is_known_to_be_a_pointer_target() const {
			return "pointer target" == m_current_pointer_target_state;
		}
		void set_is_known_to_be_a_pointer_target(bool val = true) {
			if (val) {
				m_current_pointer_target_state = "pointer target";
			} else {
				m_current_pointer_target_state = "";
			}
		}
		void set_xscope_eligibility(bool eligibility) {
			m_is_ineligible_for_xscope_status = (!eligibility);
		}
		auto xscope_eligibility() const {
			return (!m_is_ineligible_for_xscope_status);
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
		bool seems_to_be_a_function_type() const {
			bool retval = false;
			if (m_maybe_current_qtype.has_value()) {
				auto& current_qtype = m_maybe_current_qtype.value();
				if (current_qtype->isFunctionType()) {
					return true;
				}
				return false;
			}
			if (m_maybe_original_qtype.has_value()) {
				auto& original_qtype = m_maybe_original_qtype.value();
				if (original_qtype->isFunctionType()) {
					return true;
				}
				return false;
			}
			if (!(CFunctionTypeState() == m_function_type_state)) {
				return true;
			}
			return false;
		}
		bool is_enum_type() const {
			if (m_maybe_current_qtype.has_value()) {
				return m_maybe_current_qtype.value()->isEnumeralType();
			}
			if (m_maybe_original_qtype.has_value()) {
				return m_maybe_original_qtype.value()->isEnumeralType();
			}
			static const std::string enum_space_str = "enum ";
			auto const l_current_qtype_str = current_qtype_str();
			if (string_begins_with(l_current_qtype_str, enum_space_str)) {
				return true;
			}
			return false;
		}

		std::optional<clang::SourceRange> m_maybe_source_range;
		std::optional<clang::SourceRange> m_maybe_source_range_including_any_const_qualifier;
		std::optional<clang::TypeLoc> m_maybe_typeLoc;
		CFunctionTypeState m_function_type_state;

		std::string m_original_pointer_target_state;
		std::string m_current_pointer_target_state;

		bool m_is_ineligible_for_xscope_status = false;

		private:
		std::optional<clang::QualType> m_maybe_original_qtype;
		bool m_current_qtype_is_current = false;
		std::optional<clang::QualType> m_maybe_current_qtype;
		std::string m_current_qtype_or_return_qtype_str;
		std::string m_original_source_text;
		std::string m_return_type_original_source_text;
	};

	/* CTUState holds any "per translation unit" state information we might need to store. */
	class CTUState;


	class CSourceRangePlus : public clang::SourceRange {
		public:
		typedef clang::SourceRange base_class;
		using base_class::base_class;
		CSourceRangePlus() = default;
		CSourceRangePlus(CSourceRangePlus&& src) = default;
		CSourceRangePlus(CSourceRangePlus const& src) = default;
		CSourceRangePlus(base_class&& src) : base_class(std::forward<decltype(src)>(src)) {}
		CSourceRangePlus(base_class const& src) : base_class(src) {}
		CSourceRangePlus& operator=(CSourceRangePlus&& src) = default;
		CSourceRangePlus& operator=(CSourceRangePlus const& src) = default;
		//CSourceRangePlus& operator=(base_class&& src) { base_class::operator=(std::forward<decltype(src)>(src)); return *this; }
		//CSourceRangePlus& operator=(base_class const& src) { base_class::operator=(src); return *this; }
		void set_source_range(base_class const& src) { base_class::operator=(src); }
		bool isValid() const {
			return (base_class::isValid() && (!(getBegin() > getEnd())));
		}

		clang::SourceRange m_original_source_range;
		std::string m_adjusted_source_text_as_if_expanded;
		bool m_range_is_essentially_the_entire_body_of_a_macro = false;
		bool m_macro_expansion_range_substituted_with_macro_invocation_range = false;

		/* Macro argument values are represented by corresponding macro parameter names in the definition 
		body of the macro. So in the case of nested macro invocations, a macro argument value might have 
		a distinct representation at each level of macro nesting corresponding to a parameter name of the 
		macro corresponding to the nesting level. So any element whose definition depends on one or more 
		macro argument values might also have a distinct representation at each level of macro nesting. 

		So in the event we want to replace an element in an expression resulting from a nested macro 
		invocation, we may choose to do it by replacing its text representation in the definition body of 
		one of the invoked macros. But that representation might itself be another macro. We may want to 
		pull the representation from the "deeper" macro into the "shallower" macro. But in order to do so 
		we may need to replace any macro parameters used in the "deeper" macro representation with their 
		corresponding representation in the "shallower" macro. 

		These representations at the various levels of macro nesting are incidentally computed in the 
		cm1_adjusted_source_range() function. So we'll store those results here (in the m_adjusted_source_text_infos
		member field) in case they are need later at some point. */
		struct CAdjustedSourceTextInfo {
			std::string m_text;
			bool m_can_be_substituted_with_macro_invocation_text = false;
			std::vector<std::string> m_macro_args;
			clang::SourceRange m_macro_invocation_range;
			clang::SourceRange m_macro_definition_range;
			std::string m_macro_name;
		};
		std::vector<CAdjustedSourceTextInfo> m_adjusted_source_text_infos;
	};
	static CSourceRangePlus cm1_adjusted_source_range(const clang::SourceRange& sr, CTUState& state1, clang::Rewriter &Rewrite, bool may_be_a_gnu_attr = false);

	/* You may be given a SourceRange in which the the Begin and End points are at different levels 
	of macro nesting. Given such a SourceRange, this function will try to return the most deeply
	nested corresponding SourceRange in which the Begin and End points are at the same macro nesting
	level. */
	static CSourceRangePlus source_range_with_both_ends_in_the_same_macro_body(clang::SourceRange sr, clang::Rewriter &Rewrite) {
		auto& SM = Rewrite.getSourceMgr();
		CSourceRangePlus retval = sr;
		auto SL = sr.getBegin();
		auto SLE = sr.getEnd();

		if (SL == SLE) {
			return retval;
		}

		auto SL_macro_arg_expansion_start = SL;
		bool SL_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SL, &SL_macro_arg_expansion_start);
		auto SLE_macro_arg_expansion_start = SLE;
		bool SLE_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SLE, &SLE_macro_arg_expansion_start);
		if (SL_isMacroArgExpansion_flag && !SLE_isMacroArgExpansion_flag) {
			/* The begin location of the source range appears to in an expression that is used as a macro function 
			argument, while the end location isn't. The end location is presumably part of a macro expansion. So 
			we replace the begin location with the corresponding location in the macro expansion. */
			sr = { SL_macro_arg_expansion_start, SLE };
		} else if ((!SL_isMacroArgExpansion_flag) && SLE_isMacroArgExpansion_flag) {
			sr = { SL, SLE_macro_arg_expansion_start };
		} else if (SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag && (SL_macro_arg_expansion_start != SLE_macro_arg_expansion_start)) {
			/* Are SL and SLE int two different macro arguments? */
			sr = { SL_macro_arg_expansion_start, SLE_macro_arg_expansion_start };
		}
		auto adj_SPSL = SM.getSpellingLoc(sr.getBegin());
		auto adj_SPSLE = SM.getSpellingLoc(sr.getEnd());
		auto adj_SPSR = clang::SourceRange{ adj_SPSL, adj_SPSLE };
		DEBUG_SOURCE_TEXT_STR(debug_adj_SPSR_source_text, adj_SPSR, Rewrite);

		/* Returns a list of ranges corresponding to nested macros (if any) that contain the given source location. */
		auto nested_macro_ranges = [&SM, &Rewrite](clang::SourceLocation SL, bool is_end_point = false) {
				std::vector<std::pair<clang::SourceLocation, clang::SourceRange> > retval;
				auto last_macro1_SL = SL;
				auto last_macro1_SR = clang::SourceRange{ SL, SL };
				DEBUG_SOURCE_TEXT_STR(last_macro1_source_text, last_macro1_SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(last_macro1_sp_source_text, (clang::SourceRange{ SM.getSpellingLoc(last_macro1_SL), SM.getSpellingLoc(last_macro1_SL) }), Rewrite);
				auto macro1_SR = SM.getExpansionRange(SL).getAsRange();
				DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, macro1_SR, Rewrite);
				retval.push_back(std::pair{ SL, macro1_SR });

				auto macro1_SL = SM.getImmediateMacroCallerLoc(SL);
				last_macro1_SR = macro1_SR;
				DEBUG_SOURCE_TEXT_STR(macro1_source_text, macro1_SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(macro1_sp_source_text, (clang::SourceRange{ SM.getSpellingLoc(macro1_SL), SM.getSpellingLoc(macro1_SL) }), Rewrite);
				macro1_SR = SM.getExpansionRange(macro1_SL).getAsRange();
				DEBUG_SOURCE_TEXT_STR(debug_expansion2_source_text, macro1_SR, Rewrite);

				while (macro1_SL != last_macro1_SL) {
					retval.push_back({ macro1_SL, macro1_SR });

					last_macro1_SL = macro1_SL;
					macro1_SL = SM.getImmediateMacroCallerLoc(macro1_SL);
					DEBUG_SOURCE_TEXT_STR(macro1_source_text, (clang::SourceRange{ macro1_SL, macro1_SL }), Rewrite);
					DEBUG_SOURCE_TEXT_STR(macro1_sp_source_text, (clang::SourceRange{ SM.getSpellingLoc(macro1_SL), SM.getSpellingLoc(macro1_SL) }), Rewrite);

					macro1_SR = SM.getExpansionRange(macro1_SL).getAsRange();
					DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, macro1_SR, Rewrite);

					auto b16 = SM.isMacroArgExpansion(macro1_SL);
					auto b18 = SM.isMacroBodyExpansion(macro1_SL);
					int q = 5;
				}

				return retval;
			};

		auto nested_macro_ranges_of_begin = nested_macro_ranges(sr.getBegin());
		auto nested_macro_ranges_of_end = nested_macro_ranges(sr.getEnd(), true/* is_end_point */);

		/* So we're searching for the most deeply nested macro that contains both the begin and end 
		points of the given range. */
		bool found_flag = false;
		if ((1 <= nested_macro_ranges_of_begin.size()) && (1 <= nested_macro_ranges_of_end.size())) {
			auto ranges_of_begin_iter = nested_macro_ranges_of_begin.begin();
			auto ranges_of_end_iter = nested_macro_ranges_of_end.end();
			for (; nested_macro_ranges_of_begin.end() != ranges_of_begin_iter; ++ranges_of_begin_iter) {
				auto const& current_macro_range_of_begin = ranges_of_begin_iter->second;

				/* For some reason there may be multiple entries of the same range, each with a different corresponding 
				source location. I don't really get why. But presumably we want the least deeply nested one where the
				corresponding source location is distinct from either the begin or end of the range. */
				auto ranges_of_begin_iter2 = ranges_of_begin_iter;
				++ranges_of_begin_iter2;
				while (nested_macro_ranges_of_begin.end() != ranges_of_begin_iter2) {
					if (current_macro_range_of_begin != ranges_of_begin_iter2->second) {
						break;
					};
					if ((ranges_of_begin_iter2->first == current_macro_range_of_begin.getBegin()) && (ranges_of_begin_iter2->first == current_macro_range_of_begin.getEnd())) {
						break;
					}
					ranges_of_begin_iter = ranges_of_begin_iter2;
					++ranges_of_begin_iter2;
				}

				auto l_ranges_of_end_iter = nested_macro_ranges_of_end.begin();
				for (; nested_macro_ranges_of_end.end() != l_ranges_of_end_iter; ++l_ranges_of_end_iter) {
					if (current_macro_range_of_begin == l_ranges_of_end_iter->second) {

						/* For some reason there may be multiple entries of the same range, each with a different corresponding 
						source location. I don't really get why. But presumably we want the least deeply nested one where the
						corresponding source location is distinct from either the begin or end of the range. */
						auto ranges_of_end_iter2 = l_ranges_of_end_iter;
						++ranges_of_end_iter2;
						while (nested_macro_ranges_of_end.end() != ranges_of_end_iter2) {
							if (current_macro_range_of_begin != ranges_of_end_iter2->second) {
								break;
							};
							if ((ranges_of_end_iter2->first == current_macro_range_of_begin.getBegin()) && (ranges_of_end_iter2->first == current_macro_range_of_begin.getEnd())) {
								break;
							}
							l_ranges_of_end_iter = ranges_of_end_iter2;
							++ranges_of_end_iter2;
						}

						/* Ok, so now we have a Begin and End point with an ostensibly common immediate parent macro invocation. But for some reason the 
						immediate parent macro invocation reported (by clang) is not always actually the immediate parent macro invocation. Sometimes it 
						seems to report the immediate parent macro as the grandparent macro. */

						if ((ranges_of_begin_iter->first == l_ranges_of_end_iter->first) && (nested_macro_ranges_of_begin.front().first != nested_macro_ranges_of_end.front().first)) {
							/* The begin and end source locations we ended up with seem to be the same, while the original begin
							and end locations were not the same. It's possible we could settled on a common macro one nesting 
							level (or more) shallower than intended as a result of the aforementioned apparent phenoma of clang 
							reporting the immediate parent macro as the grandparent macro. So here we check for this and revert 
							to using the macro one nesting level (or more) deeper if appropriate. */
							if ((nested_macro_ranges_of_begin.begin() != ranges_of_begin_iter) && (nested_macro_ranges_of_end.begin() != l_ranges_of_end_iter)) {
								auto test_ranges_of_begin_iter1 = ranges_of_begin_iter - 1;
								auto test_ranges_of_end_iter1 = l_ranges_of_end_iter - 1;
								while (test_ranges_of_begin_iter1->second == test_ranges_of_end_iter1->second) {
									if (test_ranges_of_begin_iter1->first != test_ranges_of_end_iter1->first) {
										/* We found a deeper level of macro nesting where the begin and end source locations are still different 
										but still seem to be contained in in the body of a common macro. */
										ranges_of_begin_iter = test_ranges_of_begin_iter1;
										l_ranges_of_end_iter = test_ranges_of_end_iter1;
										break;
									}
									if ((nested_macro_ranges_of_begin.begin() != test_ranges_of_begin_iter1) && (nested_macro_ranges_of_end.begin() != test_ranges_of_end_iter1)) {
										--test_ranges_of_begin_iter1;
										--test_ranges_of_end_iter1;
									} else {
										break;
									}
								}
							}
						}

						/* Because of the aforementioned apparent phenoma of clang unreliably reporting the immediate parent macro, 
						for extra verification we'll check to make sure that all ancestor macro invocations also match. */

						if ((nested_macro_ranges_of_begin.end() - ranges_of_begin_iter2) == (nested_macro_ranges_of_end.end() - ranges_of_end_iter2)) {
							bool all_further_ancestor_ranges_match = true;
							while (nested_macro_ranges_of_begin.end() != ranges_of_begin_iter2) {
								if (!(ranges_of_begin_iter2->second == ranges_of_end_iter2->second)) {
									all_further_ancestor_ranges_match = false;
									break;
								}
								++ranges_of_begin_iter2;
								++ranges_of_end_iter2;
							}

							if (all_further_ancestor_ranges_match) {
								/* Ok, we seem to have found a Begin and End point with an seemingly common immediate parent macro invocation. */
								ranges_of_end_iter = l_ranges_of_end_iter;
								found_flag = true;
								retval.set_source_range({ ranges_of_begin_iter->first, l_ranges_of_end_iter->first });
								auto macro_invocation_depth = size_t(nested_macro_ranges_of_begin.end() - ranges_of_begin_iter) - 1;
								retval.m_adjusted_source_text_infos.resize(1 + macro_invocation_depth);
								break;
							}
						}
					}
				}
				if (found_flag) {
					break;
				}
			}
		} else {
			int q = 3;
		}
		if (true && (!found_flag)) {
			if (2 <= nested_macro_ranges_of_begin.size()) {
				retval.setBegin(nested_macro_ranges_of_begin.back().first);
			};
			if (2 <= nested_macro_ranges_of_end.size()) {
				retval.setEnd(nested_macro_ranges_of_end.back().first);
			};
			DEBUG_SOURCE_TEXT_STR(debug_retval_source_text, retval, Rewrite);
		}
		return retval;
	}

	/* This function generally returns the "File" source range and, in cases where the range partially
	covers a macro, extends the range to cover the entire macro (function) instantiation/call. */
	static clang::SourceRange cm1_nice_source_range(const clang::SourceRange& sr, clang::Rewriter &Rewrite, CTUState* state1_ptr = nullptr) {
		auto& SM = Rewrite.getSourceMgr();

		SourceLocation nice_SL = sr.getBegin();
		SourceLocation nice_SLE = sr.getEnd();
		auto nice_SR = decltype(sr){ nice_SL, nice_SLE };

		SourceLocation SL = sr.getBegin();
		SourceLocation SLE = sr.getEnd();

		auto FLSL = SM.getFileLoc(SL);
		auto FLSLE = SM.getFileLoc(SLE);
		bool FLSR_was_valid = clang::SourceRange({ FLSL, FLSLE }).isValid();
		if (FLSLE < FLSL) {
			FLSLE = FLSL;
			FLSR_was_valid = false;
		}
		auto FLSR = clang::SourceRange({ FLSL, FLSLE });
		if (FLSR_was_valid) {
			nice_SL = FLSL;
			nice_SLE = FLSLE;
			nice_SR = { nice_SL, nice_SLE };
		}

		bool sr_is_valid = sr.isValid();
		if (SLE < SL) {
			//SLE = SL;
			sr_is_valid = false;
		}
		auto same_file = (SM.getFileID(SL) == SM.getFileID(SLE));
		auto b1 = SL.isMacroID();
		auto b2 = SLE.isMacroID();

		if (true && (b1 || b2) && (!filtered_out_by_location(SM, SL))) {
			if (state1_ptr) {
				auto& state1 = *state1_ptr;
				const auto adj_sr = cm1_adjusted_source_range(sr, state1, Rewrite);
				if (!(adj_sr == sr)) {
					return adj_sr;
				}
			}

			/* A macro (or some macros) seem to be involved with the given source range. We don't really
			have a good understanding of macros wrt source ranges, but via trial and error we've ended up
			with a method that seems to mostly work for the (limited range of) situations we've dealt
			with. */

			/* Essentially, if the source range corresponds to (part of) a macro expansion, then we'll
			make it so that the returned range includes the entire macro instantiation/call in the
			original source text, unless we suspect that the range corresponds to (part of) a single
			macro function argument. */
			auto SL_macro_arg_expansion_start = SL;
			bool SL_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SL, &SL_macro_arg_expansion_start);
			auto SL_macro_CSR = SL_isMacroArgExpansion_flag
				? SM.getExpansionRange(SL_macro_arg_expansion_start) : SM.getExpansionRange(SL);

			auto SL_immediate_macro_start = SL;
			bool SL_isAtStartOfImmediateMacroExpansion_flag = SM.isAtStartOfImmediateMacroExpansion(SL, &SL_immediate_macro_start);
			auto SL_immediate_macro_CSR = SM.getExpansionRange(SL_immediate_macro_start);

			bool FLSLE_is_within_SL_immediate_macro = SM.isPointWithin(FLSLE, SL_immediate_macro_CSR.getBegin(), SL_immediate_macro_CSR.getEnd());

			auto SLE_macro_arg_expansion_start = SLE;
			bool SLE_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SLE, &SLE_macro_arg_expansion_start);
			auto SLE_macro_CSR = SLE_isMacroArgExpansion_flag
				? SM.getExpansionRange(SLE_macro_arg_expansion_start) : SM.getExpansionRange(SLE);

			auto SLE_immediate_macro_start = SLE;
			bool SLE_isAtStartOfImmediateMacroExpansion_flag = SM.isAtStartOfImmediateMacroExpansion(SLE, &SLE_immediate_macro_start);
			auto SLE_immediate_macro_CSR = SM.getExpansionRange(SLE_immediate_macro_start);

			bool FLSL_is_within_SLE_immediate_macro = SM.isPointWithin(FLSL, SLE_immediate_macro_CSR.getBegin(), SLE_immediate_macro_CSR.getEnd());

			if (SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag
				&& (SL_immediate_macro_CSR.getBegin() == SLE_immediate_macro_CSR.getBegin()) && (SL_immediate_macro_CSR.getEnd() == SLE_immediate_macro_CSR.getEnd())
				&& FLSLE_is_within_SL_immediate_macro && FLSL_is_within_SLE_immediate_macro
				&& (!(SL_macro_CSR.getAsRange().getBegin() == FLSL))) {
				if (FLSR_was_valid) {
					/* Both the start and end location appear to refer argument(s) in a macro function.
					Here we're going to (unjustifiably) assume that they refer to the same argument. In
					which case we just return the "File" range corresponding to the given range. */
					DEBUG_SOURCE_TEXT_STR(debug_FLSR_text, FLSR, Rewrite);
					return FLSR;
				} else {
					int q = 3;
				}
			}
			if (SL.isMacroID()) {
				nice_SL = SL_macro_CSR.getAsRange().getBegin();
			} else {
				nice_SL = FLSL;
			}
			if (SLE.isMacroID()) {
				nice_SLE = SLE_macro_CSR.getAsRange().getEnd();
			} else {
				nice_SLE = FLSLE;
			}
			if (SL.isMacroID() && (nice_SLE < SL_macro_CSR.getAsRange().getEnd())) {
				/* The end of the range does not extend to the end of the macro (presumably because
				it corresponds to the end of one of the macro function arguments). So we're going to
				extend it to the end of the macro. */
				if ((!FLSLE_is_within_SL_immediate_macro) || (nice_SLE < nice_SL)) {
					int q = 3;
				}
				nice_SLE = SL_macro_CSR.getAsRange().getEnd();
				int q = 5;
			}
			nice_SR = { nice_SL, nice_SLE };
			DEBUG_SOURCE_TEXT_STR(debug_nice_SR_text, nice_SR, Rewrite);

			return nice_SR;
		}

		auto same_file6 = (SM.getFileID(FLSL) == SM.getFileID(FLSLE));
		clang::SourceRange retSR = { FLSL, FLSLE };

#ifndef NDEBUG
		if ((!(FLSLE < FLSL)) && (retSR.isValid()) && same_file6) {
			std::string text6 = Rewrite.getRewrittenText({FLSL, FLSLE});
			int q = 5;
		} else {
			int q = 5;
		}
#endif /*!NDEBUG*/

		return nice_SR;
	}
	/* This function generally returns the "File" source location and, in cases where the location is
	somewhere in the middle of a macro, returns the start location of the macro (function) instantiation/call. */
	static clang::SourceLocation cm1_nice_source_location(const clang::SourceLocation& sl, clang::Rewriter &Rewrite) {
		return cm1_nice_source_range({ sl, sl }, Rewrite).getBegin();
	}
	/* This function generally returns the "File" source range and, in cases where the range is a (part
	of a) macro, it returns either the macro instantiation range or (part of) the definition range,
	depending on whether the macro seems to be just a single expression or not. */
	static clang::SourceRange cm1_adj_nice_source_range(const clang::SourceRange& sr, CTUState& state1, clang::Rewriter &Rewrite, bool may_be_a_gnu_attr = false) {
		const auto adjusted_SR = cm1_adjusted_source_range(sr, state1, Rewrite, may_be_a_gnu_attr);
		if (!(adjusted_SR == sr)) {
			return adjusted_SR;
		}
		const auto nice_SR = cm1_nice_source_range(sr, Rewrite);
		return nice_SR;
	}
	/* This function generally returns the "File" source location and, in cases where the location is
	somewhere in the middle of a macro, returns the start location of the macro (function) instantiation/call. */
	static clang::SourceLocation cm1_adj_nice_source_location(const clang::SourceLocation& sl, CTUState& state1, clang::Rewriter &Rewrite) {
		return cm1_adj_nice_source_range({ sl, sl }, state1, Rewrite).getBegin();
	}

	/* CIndirectionStateStack holds information about an instance of a type that is composed of
	nested "indirect types" (like (native) pointers and/or arrays) and the ultimate direct type.
	So for example, the type 'int*[3]' would be an array (indirect type) of pointers (indirect
	type) to ints (direct type).
	The first element in the stack (with index 0) corresponds to the "outermost" indirection. So 
	for an array of pointers, index 0 corresponds to the array and index 1 corresponds to the 
	pointer.
	Note that currently here we're only supporting indirect types whose dereference operator(s)
	return exactly one type, which is probably sufficient for C code but not C++ code. For example,
	tuples could also be considered indirect types with the corresponding 'std::get<>()' functions
	(potentially returning different types) as their dereference operator(s). In order to support
	multiple dereference types, this CIndirectionStateStack would probably need to be generalized
	to a CIndirectionStateTree. */
	class CIndirectionStateStack : public std::vector<CIndirectionState> {
	public:
		std::optional<const clang::DeclaratorDecl*> m_maybe_DD;
		/* If the (indirect) type being represented is not the type of a DeclaratorDecl (for example, if 
		it's the type specifier of a cast expression) then a "containing" Decl may be provided which, for 
		example, can be used to determine if the type uses, or is represented by, a template parameter. */
		std::optional<const clang::Decl*> m_maybe_containing_D;
		CTypeState1 m_direct_type_state;
		void set_xscope_eligibility(bool eligibility, size_t indirection_level) {
			assert((*this).size() > indirection_level);
			(*this).at(indirection_level).set_xscope_eligibility(eligibility);
			if (false == eligibility) {
				/* If an indirection is ineligible for xscope status, then so are any nested indirections of that
				indirection. */
				for (auto i = indirection_level + 1; (*this).size() > i; ++i) {
					(*this).at(i).set_xscope_eligibility(false);
				}
			}
		}
		auto xscope_eligibility(size_t indirection_level) const {
			bool retval = false;
			assert((*this).size() > indirection_level);
			auto xscope_eligibility = (*this).at(indirection_level).xscope_eligibility();
			return (xscope_eligibility);
		}
	};

	clang::SourceRange extended_to_include_west_const_if_any(Rewriter& Rewrite, const clang::SourceRange& SR);
	clang::SourceRange extended_to_include_east_const_if_any(Rewriter& Rewrite, const clang::SourceRange& SR);

	/* Given a type and an (empty) CIndirectionStateStack, this function will fill the stack with indications of
	* whether each level of indirection (if any) of the type is of the pointer or the array variety. Pointers
	* can, of course, function as arrays, but context is required to identify those situations. Such identification
	* is not done in this function. It is done elsewhere.  */
	clang::QualType populateQTypeIndirectionStack(CIndirectionStateStack& stack, clang::QualType qtype, std::optional<clang::TypeLoc> maybe_typeLoc = {}, Rewriter* Rewrite_ptr = nullptr, CTUState* state1_ptr = nullptr, int depth = 0) {
		auto starting_stack_size = stack.size();
		qtype = definition_qtype(qtype);
		auto l_qtype = qtype;
		IF_DEBUG(auto l_qtype_str2 = l_qtype.getAsString();)
		auto l_maybe_typeLoc = maybe_typeLoc;
		std::optional<clang::FunctionProtoTypeLoc> new_maybe_functionProtoTypeLoc;
		auto type_class = l_qtype->getTypeClass();

		std::string original_source_text;
		std::string return_type_original_source_text;
		if (Rewrite_ptr && l_maybe_typeLoc.has_value()) {
			auto typeLoc = l_maybe_typeLoc.value();
			auto& Rewrite = *Rewrite_ptr;
			const auto l_SR = state1_ptr
				? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
				: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
			if ((l_SR).isValid() && (((l_SR).getBegin() < (l_SR).getEnd()) || ((l_SR).getBegin() == (l_SR).getEnd()))) {
				DEBUG_SOURCE_LOCATION_STR(l_debug_source_location_str, l_SR, Rewrite);
				/* For some reason, typeLoc.getSourceRange() seems to leave out leading (and I assume trailing) `const` qualifiers. */
				auto cq_SR = extended_to_include_west_const_if_any(Rewrite, l_SR);
				cq_SR = extended_to_include_east_const_if_any(Rewrite, cq_SR);
				original_source_text = (Rewrite).getRewrittenText(cq_SR);
				return_type_original_source_text = original_source_text;
			}
		}

		bool is_function_type = false;
		std::vector<clang::QualType> param_qtypes;
		if(l_qtype->isFunctionType()) {
			if (clang::Type::Decayed == type_class) {
				int q = 5;
			} else if (clang::Type::FunctionNoProto == type_class) {
				int q = 5;
			} else if (clang::Type::FunctionProto == type_class) {
				if (llvm::isa<const clang::FunctionProtoType>(l_qtype)) {
					auto FNT = llvm::cast<const clang::FunctionProtoType>(l_qtype);
					if (FNT) {
						auto num_params = FNT->getNumParams();
						auto param_types = FNT->param_types();
						for (auto& param_type : param_types) {
							param_qtypes.push_back(param_type);
						}
						if (l_maybe_typeLoc.has_value()) {
							IF_DEBUG(auto functionProtoTypeLocClass = l_maybe_typeLoc.value().getTypeLocClass();)
							auto functionProtoTypeLoc = definition_TypeLoc(l_maybe_typeLoc.value());
							auto FunctionProtoLoc = functionProtoTypeLoc.getAsAdjusted<clang::FunctionProtoTypeLoc>();
							while (!FunctionProtoLoc) {
								auto etl = functionProtoTypeLoc.getAsAdjusted<clang::ElaboratedTypeLoc>();
								if (etl) {
									functionProtoTypeLoc = definition_TypeLoc(etl.getNamedTypeLoc());
									FunctionProtoLoc = functionProtoTypeLoc.getAsAdjusted<clang::FunctionProtoTypeLoc>();
								} else {
									break;
								}
							}
							if (FunctionProtoLoc) {
								new_maybe_functionProtoTypeLoc = FunctionProtoLoc;
							}
						}


					} else {
						assert(false);
					}
				} else {
					assert(false);
				}
				int q = 5;
			}
		}

		if (clang::Type::Paren == type_class) {
			if (llvm::isa<const clang::ParenType>(l_qtype)) {
				auto PNT = llvm::cast<const clang::ParenType>(l_qtype);
				if (PNT) {
					std::optional<clang::TypeLoc> new_maybe_typeLoc;
					if (maybe_typeLoc.has_value()) {
						IF_DEBUG(auto typeLocClass = maybe_typeLoc.value().getTypeLocClass();)
						auto ParenLoc = definition_TypeLoc(maybe_typeLoc.value()).getAsAdjusted<clang::ParenTypeLoc>();
						if (ParenLoc) {
							new_maybe_typeLoc = ParenLoc.getInnerLoc();
						}
					}
					return populateQTypeIndirectionStack(stack, PNT->getInnerType(), new_maybe_typeLoc, Rewrite_ptr, state1_ptr, depth+1);
				} else {
					assert(false);
				}
			} else {
				int q = 7;
			}
		}
		if (llvm::isa<const clang::FunctionType>(l_qtype)) {
			auto FNT = llvm::cast<const clang::FunctionType>(l_qtype);
			if (FNT) {
				is_function_type = true;
				l_qtype = FNT->getReturnType();

				if (maybe_typeLoc.has_value()) {
					IF_DEBUG(auto typeLocClass = maybe_typeLoc.value().getTypeLocClass();)
					auto FunLoc = definition_TypeLoc(maybe_typeLoc.value()).getAsAdjusted<clang::FunctionTypeLoc>();
					if (FunLoc) {
						l_maybe_typeLoc = FunLoc.getReturnLoc();

						if (Rewrite_ptr && l_maybe_typeLoc.has_value()) {
							auto typeLoc = l_maybe_typeLoc.value();
							auto& Rewrite = *Rewrite_ptr;
							const auto l_SR = state1_ptr
								? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
								: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
							if ((l_SR).isValid() && (((l_SR).getBegin() < (l_SR).getEnd()) || ((l_SR).getBegin() == (l_SR).getEnd()))) {
								DEBUG_SOURCE_LOCATION_STR(l_debug_source_location_str, l_SR, Rewrite);
								return_type_original_source_text = (Rewrite).getRewrittenText(l_SR);
							}
						}
					}
				}
			} else {
				assert(false);
			}
		}

		std::string l_qtype_str = l_qtype.getAsString();
		auto TP = l_qtype.getTypePtr();
		bool processed = false;

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
					//auto SR = cm1_adj_nice_source_range(size_expr->getSourceRange(), Rewrite);
					//size_text = Rewrite.getRewrittenText(SR);
				}
			} else if (llvm::isa<const clang::ConstantArrayType>(TP)) {
				auto CATP = llvm::cast<const clang::ConstantArrayType>(TP);
				if (!CATP) {
					assert(false);
				} else {
					if (true || (!l_maybe_typeLoc.has_value())) {
						/* When there is no source text, we'll generate the array size expression text here. */
						auto array_size = CATP->getSize();
#if MU_LLVM_MAJOR <= 12
						size_text = array_size.toString(10, false);/*check this*/
#elif MU_LLVM_MAJOR > 12
						llvm::SmallVector<char, 0> char_vec1;
						array_size.toString(char_vec1, 10, false);/*check this*/
						for (const auto& ch : char_vec1) {
							size_text.push_back(ch);
						}
#endif /*MU_LLVM_MAJOR*/
					} else {
						/* When there is source text, the array size expression will be read from the source
						(elsewhere) when required. */
					}
				}
			}

			const clang::ArrayType* ATP = TP->getAsArrayTypeUnsafe();
			if (ATP) {
				clang::QualType QT = ATP->getElementType();
				IF_DEBUG(auto l_type_str = QT.getAsString();)

				std::optional<clang::TypeLoc> new_maybe_typeLoc;
				if (l_maybe_typeLoc.has_value()) {
					IF_DEBUG(auto typeLocClass = l_maybe_typeLoc.value().getTypeLocClass();)
					auto ArrayLoc = definition_TypeLoc(l_maybe_typeLoc.value()).getAsAdjusted<clang::ArrayTypeLoc>();
					if (ArrayLoc) {
						new_maybe_typeLoc = ArrayLoc.getElementLoc();
					}
				}
				auto indirection_state = ("" == size_text) ? CIndirectionState(l_maybe_typeLoc, "native pointer", "inferred array", size_text)
					: CIndirectionState(l_maybe_typeLoc, "native array", "native array", size_text);
				indirection_state.m_maybe_original_qtype = l_qtype;
				indirection_state.m_original_source_text = original_source_text;
				indirection_state.m_return_type_original_source_text = return_type_original_source_text;
				if (is_function_type) {
					indirection_state.m_is_ineligible_for_xscope_status = true;
				}
				stack.push_back(indirection_state);

				qtype = populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, Rewrite_ptr, state1_ptr, depth+1);
				processed = true;
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
				auto PT = llvm::cast<const clang::PointerType>(l_qtype);
				if (PT) {
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
			if (l_maybe_typeLoc.has_value()) {
				auto typeLoc = definition_TypeLoc(l_maybe_typeLoc.value());
				IF_DEBUG(auto typeLocClass = typeLoc.getTypeLocClass();)
				auto pointerTypeLoc = typeLoc.getAsAdjusted<clang::PointerTypeLoc>();
				while (!pointerTypeLoc) {
					auto etl = typeLoc.getAsAdjusted<clang::ElaboratedTypeLoc>();
					auto qtl = typeLoc.getAsAdjusted<clang::QualifiedTypeLoc>();
					if (etl) {
						typeLoc = definition_TypeLoc(etl.getNamedTypeLoc());
						pointerTypeLoc = typeLoc.getAsAdjusted<clang::PointerTypeLoc>();
					} else if (qtl) {
						typeLoc = definition_TypeLoc(qtl.getUnqualifiedLoc());
						pointerTypeLoc = typeLoc.getAsAdjusted<clang::PointerTypeLoc>();
					} else {
						break;
					}
				}
				if (pointerTypeLoc) {
					new_maybe_typeLoc = pointerTypeLoc.getPointeeLoc();
				} else {
					auto arrayTypeLoc = typeLoc.getAsAdjusted<clang::ArrayTypeLoc>();
					if (arrayTypeLoc) {
						new_maybe_typeLoc = arrayTypeLoc.getElementLoc();
					} else {
						int q = 5;
					}
				}
			}

			auto indirection_state = CIndirectionState(l_maybe_typeLoc, "native pointer", "native pointer", is_function_type, param_qtypes, new_maybe_functionProtoTypeLoc);
			indirection_state.m_maybe_original_qtype = l_qtype;
			indirection_state.m_original_source_text = original_source_text;
			indirection_state.m_return_type_original_source_text = return_type_original_source_text;
			if (is_function_type) {
				indirection_state.m_is_ineligible_for_xscope_status = true;
			}
			stack.push_back(indirection_state);

			qtype = populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, Rewrite_ptr, state1_ptr, depth+1);
			processed = true;
		} else if (l_qtype->isReferenceType()) {
			auto type_class = l_qtype->getTypeClass();

			if (llvm::isa<const clang::ReferenceType>(l_qtype)) {
				auto PT = llvm::cast<const clang::ReferenceType>(l_qtype);
				if (PT) {
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
			if (l_maybe_typeLoc.has_value()) {
				IF_DEBUG(auto typeLocClass = l_maybe_typeLoc.value().getTypeLocClass();)
				auto typeLoc = definition_TypeLoc(l_maybe_typeLoc.value());
				auto ReferenceLoc = typeLoc.getAsAdjusted<clang::ReferenceTypeLoc>();
				while (!ReferenceLoc) {
					auto etl = typeLoc.getAsAdjusted<clang::ElaboratedTypeLoc>();
					if (etl) {
						typeLoc = definition_TypeLoc(etl.getNamedTypeLoc());
						ReferenceLoc = typeLoc.getAsAdjusted<clang::ReferenceTypeLoc>();
					} else {
						break;
					}
				}
				if (ReferenceLoc) {
					new_maybe_typeLoc = ReferenceLoc.getPointeeLoc();
				}
			}

			auto indirection_state = CIndirectionState(l_maybe_typeLoc, "native reference", "native reference", is_function_type, param_qtypes, new_maybe_functionProtoTypeLoc);
			indirection_state.m_maybe_original_qtype = l_qtype;
			indirection_state.m_original_source_text = original_source_text;
			indirection_state.m_return_type_original_source_text = return_type_original_source_text;
			if (is_function_type) {
				indirection_state.m_is_ineligible_for_xscope_status = true;
			}
			stack.push_back(indirection_state);

			qtype = populateQTypeIndirectionStack(stack, QT, new_maybe_typeLoc, Rewrite_ptr, state1_ptr, depth+1);
			processed = true;
		}

		if ((!processed) && maybe_typeLoc.has_value()) {
			stack.m_direct_type_state.m_maybe_typeLoc = maybe_typeLoc;

			if (Rewrite_ptr) {
				auto typeLoc = maybe_typeLoc.value();
				auto& Rewrite = *Rewrite_ptr;
				const auto l_SR = state1_ptr
					? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
					: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
				if ((l_SR).isValid() && (((l_SR).getBegin() < (l_SR).getEnd()) || ((l_SR).getBegin() == (l_SR).getEnd()))) {
					DEBUG_SOURCE_LOCATION_STR(l_debug_source_location_str, l_SR, Rewrite);
					auto source_text2 = (Rewrite).getRewrittenText(l_SR);
					stack.m_direct_type_state.set_original_source_text(source_text2);
					stack.m_direct_type_state.set_return_type_original_source_text(return_type_original_source_text);
				}
			} else {
				int q = 5;
			}
		}

		if (is_function_type) {
			/* Here we are not permtting functions to return scope types. */
			stack.m_direct_type_state.set_xscope_eligibility(false);
			if (stack.size() > starting_stack_size) {
				stack.at(starting_stack_size).set_xscope_eligibility(false);
			}
		}

		return qtype;
	}

	/* Given an expression (in the form of a clang::Stmt) and an (empty) (string) stack,
	* this function will fill the stack with indications of whether each level of indirection
	* (if any) (in the expression) is a pointer dereference or an array subscript. 
	The maybe_stack_size_adjustment_ref parameter should end up empty or containing a negative 
	number indicating an effectively negative stmt_indirection_stack size. */
	const clang::Expr* populateStmtIndirectionStack(std::vector<std::string>& stack, const clang::Stmt& stmt, std::optional<int>& maybe_stack_size_adjustment_ref, int depth = 0) {
		const clang::Expr* retval = nullptr;
		const clang::Stmt* ST = &stmt;
		auto stmt_class = ST->getStmtClass();
		auto stmt_class_name = ST->getStmtClassName();
		const clang::Stmt* next_ST = nullptr;
		bool process_child_flag = false;
		if (clang::Stmt::StmtClass::ArraySubscriptExprClass == stmt_class) {
			if (!(maybe_stack_size_adjustment_ref.has_value())) {
				stack.push_back("ArraySubscriptExpr");
			} else {
				auto stack_size_adjustment = maybe_stack_size_adjustment_ref.value();
				stack_size_adjustment += 1;
				maybe_stack_size_adjustment_ref = stack_size_adjustment;
				if (0 <= stack_size_adjustment) {
					maybe_stack_size_adjustment_ref = {};
				}
			}
			process_child_flag = true;
		} else if (clang::Stmt::StmtClass::UnaryOperatorClass == stmt_class) {
			auto UO = llvm::cast<const clang::UnaryOperator>(ST);
			if (UO) {
				if (clang::UnaryOperatorKind::UO_Deref == UO->getOpcode()) {
					if (!(maybe_stack_size_adjustment_ref.has_value())) {
						stack.push_back("Deref");
					} else {
						auto stack_size_adjustment = maybe_stack_size_adjustment_ref.value();
						stack_size_adjustment += 1;
						maybe_stack_size_adjustment_ref = stack_size_adjustment;
						if (0 <= stack_size_adjustment) {
							maybe_stack_size_adjustment_ref = {};
						}
					}
					process_child_flag = true;
				} else if (false && (clang::UnaryOperatorKind::UO_AddrOf == UO->getOpcode())) {
					if (1 <= stack.size()) {
						stack.pop_back();
					} else {
						if (!(maybe_stack_size_adjustment_ref.has_value())) {
							maybe_stack_size_adjustment_ref = 0;
						}
						auto& stack_size_adjustment_ref = maybe_stack_size_adjustment_ref.value();
						stack_size_adjustment_ref -= 1;
					}
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
							process_child_flag = true;
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
		} else if ((clang::Stmt::StmtClass::BinaryOperatorClass == stmt_class)) {
			auto BO = llvm::cast<const clang::BinaryOperator>(ST);
			if (BO) {
				next_ST = BO->getLHS();
			} else { assert(false); }
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
			} else { assert(false); }
		} else if(clang::Stmt::StmtClass::CXXConstCastExprClass == stmt_class) {
			auto CXXCCE = llvm::cast<const clang::CXXConstCastExpr>(ST);
			if (CXXCCE) {
				next_ST = CXXCCE->getSubExpr();
			} else { assert(false); }
		} else {
			if (0 == depth) {
				int q = 5;
			}
			int q = 5;
		}
		if (next_ST) {
			const auto noted_stack_size = stack.size();
			auto res = populateStmtIndirectionStack(stack, *next_ST, maybe_stack_size_adjustment_ref, depth+1);
			if ((nullptr == retval) || (stack.size() > noted_stack_size)) {
				retval = res;
			}
		}
		if (process_child_flag) {
			auto child_iter = ST->child_begin();
			if (child_iter != ST->child_end()) {
				if (nullptr != (*child_iter)) {
					const auto noted_stack_size = stack.size();
					auto res = populateStmtIndirectionStack(stack, *(*child_iter), maybe_stack_size_adjustment_ref, depth+1);
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

	enum class EXScopeEligibility { Yes, No };

	static auto typeLoc_if_available(const clang::TypeSourceInfo& tsi) {
		auto tsi_ptr = &tsi;
		QualType QT = tsi.getType();
		auto typeClass = QT->getTypeClass();
		IF_DEBUG(std::string qtype_str = QT.getAsString();)
		std::optional<clang::TypeLoc> maybe_typeLoc;
		{
			IF_DEBUG(auto typeLocClass = tsi_ptr->getTypeLoc().getTypeLocClass();)
			auto typeLocTypeClass = tsi_ptr->getTypeLoc().getType()->getTypeClass();
			IF_DEBUG(std::string tl_qtype_str = tsi_ptr->getTypeLoc().getType().getAsString();)
			bool inconsistent_types_flag = false;
			while (tsi_ptr->getTypeLoc().getType() != QT) {
				if (llvm::isa<const clang::ParenType>(QT)) {
					auto PN = llvm::cast<const clang::ParenType>(QT);
					assert(PN);
					QT = PN->getInnerType();
				} else {
					break;
				}
			}
			if (tsi_ptr->getTypeLoc().getType() != QT) {
				if (tsi_ptr->getTypeLoc().getType().getAsString() != QT.getAsString()) {
					if ((clang::Type::ConstantArray == typeLocTypeClass) && (clang::Type::Decayed == typeClass)) {
						/* This is probably a case of an array function parameter decaying to a pointer. */
						int q = 5;
					} else {
						inconsistent_types_flag = true;
					}
				} else {
					/* ?? */
					int q = 5;
				}
			}
			if (!inconsistent_types_flag) {
				maybe_typeLoc = tsi_ptr->getTypeLoc();
			}
		}
		return maybe_typeLoc;
	}

	static auto typeLoc_if_available(const clang::DeclaratorDecl& ddecl) {
		QualType QT = ddecl.getType();
		auto typeClass = QT->getTypeClass();
		IF_DEBUG(std::string qtype_str = QT.getAsString();)
		std::optional<clang::TypeLoc> maybe_typeLoc;
		auto tsi_ptr = ddecl.getTypeSourceInfo();
		if (tsi_ptr) {
			return typeLoc_if_available(*tsi_ptr);
		}
		return maybe_typeLoc;
	}

	inline std::string params_string_from_qtypes(const std::vector<clang::QualType>& qtypes, Rewriter &Rewrite);
	inline std::string params_string_from_qtypes(const std::vector<clang::QualType>& qtypes);

	struct CExprTextInfoContext {
		CSourceRangePlus m_root_SR;
	};

	class CExprTextInfo {
	public:
		CExprTextInfo(const clang::Expr * expr_ptr, Rewriter &Rewrite, CTUState& state1);
		std::string const& current_text(std::optional<CExprTextInfoContext> maybe_context = {}) const;
		const Expr* m_expr_cptr = nullptr;
		CSourceRangePlus m_SR_plus;
		std::string m_original_source_text_str;
		CTUState& m_state1;
	};

	class CDDeclConversionState {
	public:
		CDDeclConversionState(const clang::DeclaratorDecl& ddecl, Rewriter* Rewrite_ptr = nullptr, CTUState* state1_ptr = nullptr, bool function_return_value_only = false) : m_ddecl_cptr(&ddecl) {
#ifndef NDEBUG
			if ((*this).m_ddecl_cptr) {
				std::string variable_name = m_ddecl_cptr->getNameAsString();
				if ("zalloc" == variable_name) {
					std::string qtype_str = m_ddecl_cptr->getType().getAsString();
					if ("const unsigned char *" == qtype_str) {
						int q = 5;
					}
				}
			}
#endif /*!NDEBUG*/
			QualType QT = ddecl.getType();
			IF_DEBUG(std::string qtype_str = QT.getAsString();)
			assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
			if (QT->isFunctionType()) {
				m_is_a_function = true;
			}
			auto maybe_typeLoc = typeLoc_if_available(ddecl);
			auto original_direct_qtype = populateQTypeIndirectionStack(m_indirection_state_stack, QT, maybe_typeLoc, Rewrite_ptr, state1_ptr);
			IF_DEBUG(auto original_direct_qtype_str = original_direct_qtype.getAsString();)
			if (function_return_value_only) {
				if (original_direct_qtype->isFunctionType()) {
					auto FNT = llvm::cast<const clang::FunctionType>(original_direct_qtype);
					if (FNT) {
						original_direct_qtype = FNT->getReturnType();
					}
				} else {
					int q = 3;
				}
			} else if (m_is_a_function) {
				int q = 5;
			}
			set_original_direct_qtype(original_direct_qtype);

			if (llvm::isa<const clang::FunctionType>(original_direct_qtype)) {
				auto FNT = llvm::cast<const clang::FunctionType>(original_direct_qtype);
				std::string return_type_str = FNT->getReturnType().getAsString();
				std::string params_str;
				std::vector<clang::QualType> param_qtypes;
				if (llvm::isa<const clang::FunctionProtoType>(original_direct_qtype)) {
					auto FNPT = llvm::cast<const clang::FunctionProtoType>(original_direct_qtype);
					auto param_types = FNPT->getParamTypes();
					for (auto& param_type : param_types) {
						param_qtypes.push_back(param_type);
					}
				}
				if (Rewrite_ptr) {
					params_str = params_string_from_qtypes(param_qtypes, *Rewrite_ptr);
				} else {
					params_str = params_string_from_qtypes(param_qtypes);
				}
				set_current_direct_function_qtype_str(return_type_str, params_str);
			
				auto FND = dyn_cast<const clang::FunctionDecl>(&ddecl);
				if (FND) {
					m_indirection_state_stack.m_direct_type_state.m_function_type_state.m_function_decl_ptr = FND;
				} else {
					int q = 3;
				}
			}
			if (llvm::isa<clang::ParmVarDecl>(&ddecl)) {
				auto PVD = dyn_cast<const clang::VarDecl>(&ddecl);
				assert(PVD);
				const auto PVD_qtype = PVD->getType();
				IF_DEBUG(auto PVD_qtype_str = PVD_qtype.getAsString();)

				if ((!ScopeTypeFunctionParameters)/* || (!ScopeTypePointerFunctionParameters)*/) {
					if (PVD_qtype->isPointerType()) {
						auto& ddcs_ref = *this;

						/* Here we are not permtting function parameters to be scope types. */
						ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
						//state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*FND, CDDeclIndirection::no_indirection));
						for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
							indirection_state.set_xscope_eligibility(false);
							//state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, indirection_state);
						}
					}
				}
			}

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
		bool has_been_determined_to_point_to_an_array(size_t indirection_level) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection != indirection_level) && (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				retval = m_indirection_state_stack.at(indirection_level).has_been_determined_to_point_to_an_array();
			}
			return retval;
		}
		bool has_been_determined_to_point_to_a_dynamic_array(size_t indirection_level) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection != indirection_level) && (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				retval = m_indirection_state_stack.at(indirection_level).has_been_determined_to_point_to_a_dynamic_array();
			}
			return retval;
		}
		bool has_been_determined_to_point_to_a_native_array(size_t indirection_level) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection != indirection_level) && (m_indirection_state_stack.size() > indirection_level));
			if (m_indirection_state_stack.size() > indirection_level) {
				retval = m_indirection_state_stack.at(indirection_level).has_been_determined_to_point_to_a_native_array();
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
		bool has_been_determined_to_be_ineligible_for_xscope_status(size_t indirection_level = CDDeclIndirection::no_indirection) const {
			bool retval = false;
			assert((CDDeclIndirection::no_indirection == indirection_level) || (m_indirection_state_stack.size() > indirection_level));
			auto xscope_eligibility = (CDDeclIndirection::no_indirection == indirection_level)
				? direct_type_state_ref().xscope_eligibility() : m_indirection_state_stack.at(indirection_level).xscope_eligibility();
			return (!xscope_eligibility);
		}
		bool direct_qtype_has_been_changed() const {
			return direct_type_state_ref().qtype_has_been_changed();
		}
		bool initializer_has_been_changed(Rewriter &Rewrite, CTUState* state1_ptr, std::optional<CExprTextInfoContext> maybe_context = {}) const {
			if (m_original_initialization_has_been_noted) {
				if (current_initialization_expr_str(Rewrite, state1_ptr, maybe_context) != m_original_initialization_expr_str) {
					return true;
				}
			} else if ("" != current_initialization_expr_str(Rewrite, state1_ptr, maybe_context))  {
				return true;
			}
			return false;
		}
		CTypeState1& direct_type_state_ref() { return m_indirection_state_stack.m_direct_type_state; }
		const CTypeState1& direct_type_state_ref() const { return m_indirection_state_stack.m_direct_type_state; }

		void set_original_direct_qtype(const clang::QualType& qtype) {
			direct_type_state_ref().set_original_qtype(qtype);
		}
		void set_original_direct_type_source_text(std::string str) {
			direct_type_state_ref().set_original_source_text(std::move(str));
		}
		void set_current_direct_qtype(const clang::QualType& qtype) {
			direct_type_state_ref().set_current_qtype(qtype);
		}
		std::string current_direct_qtype_str() const { return direct_type_state_ref().current_qtype_str(); }
		void set_current_direct_non_function_qtype_str(const std::string& new_qtype_str) { direct_type_state_ref().set_current_non_function_qtype_str(new_qtype_str); }
		void set_current_direct_function_qtype_str(const std::string& return_qtype_str, const std::string& params_str) { direct_type_state_ref().set_current_function_qtype_str(return_qtype_str, params_str); }
		std::string non_const_current_direct_qtype_str() const {
			auto non_const_direct_qtype_str = current_direct_qtype_str();
			if (direct_type_state_ref().current_qtype_is_current()) {
				assert(direct_type_state_ref().current_qtype_if_any().has_value());
				auto current_direct_qtype = direct_type_state_ref().current_qtype_if_any().value();
				auto non_const_direct_qtype = current_direct_qtype;
				non_const_direct_qtype.removeLocalConst();
				non_const_direct_qtype_str = adjusted_qtype_str(non_const_direct_qtype.getAsString());
			} else {
				/* hack alert */
				static const std::string const_space_str = "const ";
				if (string_begins_with(non_const_direct_qtype_str, const_space_str)) {
					non_const_direct_qtype_str = non_const_direct_qtype_str.substr(const_space_str.size());
				}
			}
			return non_const_direct_qtype_str;
		}
		/* If the direct type is a function type, then just the function return type is returned (as a
		string). Otherwise the direct type is returned (as a string). */
		std::string current_direct_return_qtype_str() const { return direct_type_state_ref().current_return_qtype_str(); }
		std::string non_const_current_direct_return_qtype_str() const {
			auto non_const_direct_return_qtype_str = current_direct_return_qtype_str();
			if (direct_type_state_ref().current_qtype_is_current()) {
				assert(direct_type_state_ref().current_qtype_if_any().has_value());
				auto current_direct_qtype = direct_type_state_ref().current_qtype_if_any().value();
				auto non_const_direct_return_qtype = current_direct_qtype;
				if (llvm::isa<const clang::FunctionType>(current_direct_qtype)) {
					auto FNT = llvm::cast<const clang::FunctionType>(current_direct_qtype);
					if (FNT) {
						non_const_direct_return_qtype = FNT->getReturnType();
					}
				}
				non_const_direct_return_qtype.removeLocalConst();
				non_const_direct_return_qtype_str = non_const_direct_return_qtype.getAsString();
			} else {
				/* hack alert */
				static const std::string const_space_str = "const ";
				if (string_begins_with(non_const_direct_return_qtype_str, const_space_str)) {
					non_const_direct_return_qtype_str = non_const_direct_return_qtype_str.substr(const_space_str.size());
				}
			}
			return adjusted_qtype_str(non_const_direct_return_qtype_str);
		}
		std::string const& current_initialization_expr_str(Rewriter &Rewrite, CTUState* state1_ptr, std::optional<CExprTextInfoContext> maybe_context = {}) const {
			if (m_maybe_initialization_expr_text_info.has_value()) {
				auto& initialization_expr_text_info = m_maybe_initialization_expr_text_info.value();

				auto maybe_default_context = [&]() -> std::optional<CExprTextInfoContext> {
					if (m_ddecl_cptr) {
						/* Presumably the initialization expression will be rendered as part of the (whole) declaration 
						statement, so the CExprTextInfoContext we supply should presumably reflect that. */
						const auto raw_SR = m_ddecl_cptr->getSourceRange();
						if (state1_ptr && raw_SR.getBegin().isMacroID() && raw_SR.getEnd().isMacroID()) {
							const auto SR_plus = cm1_adjusted_source_range(raw_SR, *state1_ptr, Rewrite);
							if (SR_plus.isValid()) {
								return CExprTextInfoContext{ SR_plus };
							}
						}
					}
					return {};
				};
				auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

				return initialization_expr_text_info.current_text(new_maybe_context);
			}
			return m_fallback_current_initialization_expr_str;
		}

		const DeclaratorDecl* m_ddecl_cptr = nullptr;
		CIndirectionStateStack m_indirection_state_stack;

		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_initializer_SR_or_insert_before_point;
		std::variant<bool, clang::SourceRange, clang::SourceLocation> m_thread_local_specifier_SR_or_insert_before_point;

		std::optional<CExprTextInfo> m_maybe_initialization_expr_text_info;
		std::string m_fallback_current_initialization_expr_str;
		//std::string m_current_initialization_expr_str;
		bool m_original_initialization_has_been_noted = false;
		std::string m_original_initialization_expr_str;
		bool m_original_source_text_has_been_noted = false;
		std::string m_original_source_text_str;

		bool m_is_a_function = false;
		std::vector<const clang::ParmVarDecl*> m_original_function_parameter_decl_cptrs;
		std::string m_function_return_type_original_source_text_str;

		std::optional<clang::StorageDuration> m_maybe_original_storage_duration;
		std::optional<clang::StorageDuration> m_maybe_current_storage_duration;
		bool m_has_been_replaced_as_a_whole = false;

		std::optional<std::string> m_maybe_updated_name;
		std::optional<bool> m_maybe_is_non_modifiable;
	};

	class CDDeclConversionStateMap : public std::unordered_map<const clang::DeclaratorDecl*, CDDeclConversionState> {
	public:
		std::pair<iterator, bool> insert(const clang::DeclaratorDecl& ddecl, Rewriter* Rewrite_ptr = nullptr, CTUState* state1_ptr = nullptr, bool function_return_value_only = false) {
			std::string variable_name = ddecl.getNameAsString();
			value_type item(&ddecl, CDDeclConversionState(ddecl, Rewrite_ptr, state1_ptr, function_return_value_only));
			return std::unordered_map<const clang::DeclaratorDecl*, CDDeclConversionState>::insert(item);
		}
	};

	class CRecordDeclConversionState {
	public:
		CRecordDeclConversionState(const clang::RecordDecl& recdecl, Rewriter &Rewrite, CTUState& state1) : m_recdecl_ptr(&recdecl) {
			m_original_source_text_str = Rewrite.getRewrittenText(cm1_adj_nice_source_range(source_range(), state1, Rewrite));
			m_current_text_str = m_original_source_text_str;
		}

		clang::SourceRange source_range() const {
			return m_recdecl_ptr->getSourceRange();
		}

		const clang::RecordDecl *recdecl_ptr() const {
			return m_recdecl_ptr;
		}

		const clang::RecordDecl *m_recdecl_ptr;
		std::string m_original_source_text_str;
		std::string m_current_text_str;
	};

	class CRecordDeclConversionStateMap : public std::unordered_map<const clang::RecordDecl*, CRecordDeclConversionState> {
	public:
		std::pair<iterator, bool> insert(const clang::RecordDecl& recdecl, Rewriter &Rewrite, CTUState& state1) {
			value_type item(&recdecl, CRecordDeclConversionState(recdecl, Rewrite, state1));
			return std::unordered_map<const clang::RecordDecl*, CRecordDeclConversionState>::insert(item);
		}
	};

	class CLocationToRecordDeclMap : public std::map<clang::SourceLocation, const clang::RecordDecl*> {
	public:
		std::pair<iterator, bool> insert(const clang::SourceLocation& SL, const clang::RecordDecl& recdecl) {
			value_type item(SL, &(recdecl));
			return std::map<clang::SourceLocation, const clang::RecordDecl*>::insert(item);
		}
		std::pair<iterator, bool> insert(const clang::RecordDecl& recdecl, Rewriter &Rewrite, CTUState& state1) {
			auto SR = cm1_adj_nice_source_range(recdecl.getSourceRange(), state1, Rewrite);
			if (!(SR.isValid())) {
				return std::pair<iterator, bool>((*this).end(), false);
			}
			auto SL = cm1_adj_nice_source_range(recdecl.getSourceRange(), state1, Rewrite).getBegin();
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
		CWrapExprTextModifier(std::string_view prefix, std::string_view suffix) :
			m_prefix(prefix), m_suffix(suffix) {}
		virtual ~CWrapExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return m_prefix + input_text + m_suffix;
		}
		virtual std::string species_str() const {
			return "wrap";
		}
		bool is_equal_to(CExprTextModifier const& rhs) const {
			if (species_str() == rhs.species_str()) {
				typedef std::remove_reference_t<decltype(*this)> this_type;
				auto& rhs_as_this_type_cref = static_cast<this_type const&>(rhs);
				return (rhs_as_this_type_cref.m_prefix == m_prefix) && (rhs_as_this_type_cref.m_suffix == m_suffix);
			}
			return false;
		}
		//bool operator==(CStraightReplacementExprTextModifier const& rhs) const = default;

		std::string m_prefix;
		std::string m_suffix;
	};

	class CNullableAnyRandomAccessIterCastExprTextModifier : public CWrapExprTextModifier {
	public:
		CNullableAnyRandomAccessIterCastExprTextModifier(const clang::QualType& qtype, EXScopeEligibility xscope_eligibility/* = EXScopeEligibility::Yes*/) :
			CWrapExprTextModifier(prefix_str(qtype, xscope_eligibility), ")"), m_qtype(qtype), m_xscope_eligibility(xscope_eligibility) {}
		virtual ~CNullableAnyRandomAccessIterCastExprTextModifier() {}
		virtual std::string species_str() const {
			return "nullable any random access iter cast";
		}

		std::string prefix_str(const clang::QualType& qtype, EXScopeEligibility xscope_eligibility/* = EXScopeEligibility::Yes*/) {
			std::string retval;
			if ("Dual" == ConvertMode) {
				retval = "MSE_LH_CAST(";
				if (false && (EXScopeEligibility::Yes == xscope_eligibility)) {
					retval += "MSE_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE(" + qtype.getAsString() + ")";
				} else {
					retval += "MSE_LH_ARRAY_ITERATOR_TYPE(" + qtype.getAsString() + ")";
				}
				retval += ", ";
			} else {
				if (false && EXScopeEligibility::Yes == xscope_eligibility) {
					retval += "mse::lh::TXScopeLHNullableAnyRandomAccessIterator<" + qtype.getAsString() + " >";
				} else {
					retval += "mse::lh::TLHNullableAnyRandomAccessIterator<" + qtype.getAsString() + " >";
				}
				retval += "(";
			}
			return retval;
		}
		clang::QualType m_qtype;
		EXScopeEligibility m_xscope_eligibility = EXScopeEligibility::Yes;
	};

	class CUnsafeCastExprTextModifier : public CWrapExprTextModifier {
	public:
		CUnsafeCastExprTextModifier(const clang::QualType& qtype) :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_UNSAFE_CAST(" + qtype.getAsString() + ", "
				: "mse::us::lh::unsafe_cast<" + qtype.getAsString() + ">(", ")")
				, m_qtype(qtype) {}
		virtual ~CUnsafeCastExprTextModifier() {}
		virtual std::string species_str() const {
			return "unsafe cast";
		}

		clang::QualType m_qtype;
	};

	class CCastExprTextModifier : public CWrapExprTextModifier {
	public:
		CCastExprTextModifier(std::string_view qtype_sv) :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_CAST(" + std::string(qtype_sv) + ", "
				: "(" + std::string(qtype_sv) + ")(", ")")
				, m_qtype_str(qtype_sv) {}
		virtual ~CCastExprTextModifier() {}
		virtual std::string species_str() const {
			return "cast";
		}

		std::string m_qtype_str;
	};

	class CUnsafeMakeRawPointerFromExprTextModifier : public CWrapExprTextModifier {
	public:
		CUnsafeMakeRawPointerFromExprTextModifier() :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_UNSAFE_MAKE_RAW_POINTER_FROM("
				: "mse::us::lh::make_raw_pointer_from("
				, ")") {}
		virtual ~CUnsafeMakeRawPointerFromExprTextModifier() {}
		virtual std::string species_str() const {
			return "unsafe make raw pointer from";
		}
	};

	class CUnsafeMakeTemporaryArrayOfRawPointersFromExprTextModifier : public CWrapExprTextModifier {
	public:
		CUnsafeMakeTemporaryArrayOfRawPointersFromExprTextModifier() :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_UNSAFE_MAKE_TEMPORARY_ARRAY_OF_RAW_POINTERS_FROM("
				: "mse::us::lh::make_temporary_array_of_raw_pointers_from("
				, ")") {}
		virtual ~CUnsafeMakeTemporaryArrayOfRawPointersFromExprTextModifier() {}
		virtual std::string species_str() const {
			return "unsafe make temporary array of raw pointers from";
		}
	};

	class CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier : public CWrapExprTextModifier {
	public:
		CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier() :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_UNSAFE_MAKE_ARRAY_ITERATOR_FROM("
				: "mse::us::lh::unsafe_make_lh_nullable_any_random_access_iterator_from("
				, ")") {}
		virtual ~CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier() {}
		virtual std::string species_str() const {
			return "unsafe make lh_nullable_any_random_access_iterator from";
		}
	};

	class CUnsafeMakeLHNullableAnyPointerFromExprTextModifier : public CWrapExprTextModifier {
	public:
		CUnsafeMakeLHNullableAnyPointerFromExprTextModifier() :
			CWrapExprTextModifier(("Dual" == ConvertMode)
				? "MSE_LH_UNSAFE_MAKE_POINTER_FROM("
				: "mse::us::lh::unsafe_make_lh_nullable_any_pointer_from("
				, ")") {}
		virtual ~CUnsafeMakeLHNullableAnyPointerFromExprTextModifier() {}
		virtual std::string species_str() const {
			return "unsafe make lh_nullable_any_pointer from";
		}
	};

	class CStraightReplacementExprTextModifier : public CExprTextModifier {
	public:
		CStraightReplacementExprTextModifier(std::string_view replacement_text) :
			m_replacement_text(replacement_text) {}
		virtual ~CStraightReplacementExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return m_replacement_text;
		}
		virtual std::string species_str() const {
			return "straight replacement";
		}
		bool is_equal_to(CExprTextModifier const& rhs) const {
			if (species_str() == rhs.species_str()) {
				typedef std::remove_reference_t<decltype(*this)> this_type;
				auto& rhs_as_this_type_cref = static_cast<this_type const&>(rhs);
				return (rhs_as_this_type_cref.m_replacement_text == m_replacement_text);
			}
			return false;
		}
		//bool operator==(CStraightReplacementExprTextModifier const& rhs) const = default;

		std::string m_replacement_text;
	};

	class CGivenFunctionExprTextModifier : public CExprTextModifier {
	public:
		CGivenFunctionExprTextModifier(const std::function<std::string (const std::string&, const clang::Expr*)>& function) :
			m_function(function) {}
		virtual ~CGivenFunctionExprTextModifier() {}
		virtual std::string modified_copy(const std::string& input_text, const clang::Expr* expr_ptr = nullptr) const {
			return m_function(input_text, expr_ptr);
		}
		virtual std::string species_str() const {
			return "given function";
		}
		std::function<std::string (const std::string&, const clang::Expr*)> m_function;
	};

	class CExprTextModifierStack : public std::vector<std::shared_ptr<CExprTextModifier>> {
	public:
	};

	/* Note that the `CExprConversionState` corresponding to an expression should be constructed (generally 
	indirectly via CTUState1::get_expr_conversion_state_ref()) after the establishment of any 
	`CExprConversionState`s corresponding to ancestor expressions, as the `CExprConversionState` constructor 
	will look for and establish a relationship with any existing ancestor `CExprConversionState`s. */
	class CExprConversionState {
	public:
	struct do_not_set_up_child_dependencies_t {};
		CExprConversionState(do_not_set_up_child_dependencies_t, const clang::Expr& expr, Rewriter &Rewrite, CTUState& state1) : m_expr_cptr(&expr), Rewrite(Rewrite), m_state1(state1) {
			m_SR_plus = cm1_adjusted_source_range(expr.getSourceRange(), state1, Rewrite);
			auto& expr_SR = m_SR_plus;
			if (expr_SR.isValid()) {
				bool use_adjusted_source_text_as_if_expanded = false;
				auto rawSR = expr.getSourceRange();
				if (rawSR.isValid()) {
					auto SL = rawSR.getBegin();
					if (SL.isMacroID()) {
						if ("" != expr_SR.m_adjusted_source_text_as_if_expanded) {
							auto& SM = Rewrite.getSourceMgr();
							auto b10 = SM.isMacroArgExpansion(SL);
							auto b13 = SM.isMacroBodyExpansion(SL);
							if (true == expr_SR.m_macro_expansion_range_substituted_with_macro_invocation_range) {
								/* The source range was adjusted to refer to the invocation site of the macro rather than the definition site. So the text we
								want is what the text at the definition site would look like after it has been expanded at the invocation site. */
								use_adjusted_source_text_as_if_expanded = true;
							}
						}
					}
				}
				if (use_adjusted_source_text_as_if_expanded) {
					m_original_source_text_str = expr_SR.m_adjusted_source_text_as_if_expanded;
				} else {
					m_original_source_text_str = Rewrite.getRewrittenText(expr_SR);
					if ("" == m_original_source_text_str) {
						auto nice_SR = cm1_adj_nice_source_range(expr.getSourceRange(), state1, Rewrite);
						if (nice_SR.isValid()) {
							m_original_source_text_str = Rewrite.getRewrittenText(nice_SR);
						}
					}
				}
			}
			m_current_text_str = m_original_source_text_str;
			set_up_relation_to_parent_generically();
		}
		CExprConversionState(const clang::Expr& expr, Rewriter &Rewrite, CTUState& state1) : CExprConversionState(do_not_set_up_child_dependencies_t(), expr, Rewrite, state1) {
			m_SR_plus = cm1_adjusted_source_range(expr.getSourceRange(), state1, Rewrite);
			set_up_child_depenedencies_generically();
		}
		virtual ~CExprConversionState() {}
		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			if (m_child_text_infos.size() + 1 == m_non_child_dependent_text_fragments.size()) {
				std::string working_text;
				for (size_t i = 0; i < m_child_text_infos.size(); i += 1) {
					working_text += m_non_child_dependent_text_fragments.at(i);
					working_text += m_child_text_infos.at(i).current_text(new_maybe_context);
				}
				working_text += m_non_child_dependent_text_fragments.back();
				m_current_text_str = modified_copy(working_text);
			} else {
				m_current_text_str = modified_copy(m_original_source_text_str);
			}
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("");
			return sc_species_str;
		}

		const std::string& current_text(std::optional<CExprTextInfoContext> maybe_context = {}) {
			update_current_text(maybe_context);
			return m_current_text_str;
		}

		clang::SourceRange source_range() const {
			return m_expr_cptr->getSourceRange();
		}

		std::string modified_copy(const std::string& input_text) const {
			std::string retval = input_text;
			for (const auto& modifier_shptr_cref : m_expr_text_modifier_stack) {
				retval = (*modifier_shptr_cref).modified_copy(retval, m_expr_cptr);
			}
			return retval;
		}
		clang::ASTContext* maybe_null_ast_context_ptr() const;
		bool set_up_child_depenedencies_generically();
		bool set_up_relation_to_parent_generically();

		auto maybe_default_context() const -> std::optional<CExprTextInfoContext> {
			if (m_expr_cptr) {
				const auto raw_SR = m_expr_cptr->getSourceRange();
				if (raw_SR.getBegin().isMacroID() && raw_SR.getEnd().isMacroID()) {
					return CExprTextInfoContext{ m_SR_plus };
				}
			}
			return {};
		}

		void add_straight_text_replacement_modifier(std::string_view replacement_text) {
			auto shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(replacement_text);
			if (1 <= m_expr_text_modifier_stack.size()) {
				if ("straight replacement" == (*(m_expr_text_modifier_stack.back())).species_str()) {
					if (shptr1->is_equal_to(*(m_expr_text_modifier_stack.back()))) {
						/* We seem to be adding a repeat of the already existing modifier.*/
						return;
					}
				}
			}
			m_expr_text_modifier_stack.push_back(shptr1);
			update_current_text();
		}

		void add_wrap_text_modifier(std::string_view prefix, std::string_view suffix) {
			auto shptr1 = std::make_shared<CWrapExprTextModifier>(prefix, suffix);
			if (1 <= m_expr_text_modifier_stack.size()) {
				if ("wrap" == (*(m_expr_text_modifier_stack.back())).species_str()) {
					if (shptr1->is_equal_to(*(m_expr_text_modifier_stack.back()))) {
						/* We seem to be adding a repeat of the already existing modifier.*/
						return;
					}
				}
			}
			m_expr_text_modifier_stack.push_back(shptr1);
			update_current_text();
		}

		CExprTextModifierStack m_expr_text_modifier_stack;

		std::vector<CExprTextInfo> m_child_text_infos;
		std::vector<std::string> m_non_child_dependent_text_fragments;

		const clang::Expr* m_expr_cptr = nullptr;
		std::string m_original_source_text_str;
		std::string m_current_text_str;
		CSourceRangePlus m_SR_plus;
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	template<class X, class... Args>
	std::shared_ptr<X> make_expr_conversion_state_shared_ptr(Args&&... args) {
		std::shared_ptr<X> retval = std::make_shared<X>(std::forward<Args>(args)...);
		return retval;
	}

	class CExprConversionStateMap : public std::unordered_map<const clang::Expr*, std::shared_ptr<CExprConversionState>> {
	public:
		typedef std::unordered_map<const clang::Expr*, std::shared_ptr<CExprConversionState>> base_class;
		iterator insert( const std::shared_ptr<CExprConversionState>& cr_shptr ) {
			iterator retval(end());
			if (!cr_shptr) { assert(false); } else {
				value_type val((*cr_shptr).m_expr_cptr, cr_shptr);
				auto res1 = base_class::insert(val);
				retval = res1.first;
			}
			return retval;
		}
		iterator insert_or_assign( const std::shared_ptr<CExprConversionState>& cr_shptr ) {
			iterator retval(end());
			if (!cr_shptr) { assert(false); } else {
				auto res1 = base_class::insert_or_assign((*cr_shptr).m_expr_cptr, cr_shptr);
				retval = res1.first;
			}
			return retval;
		}

		typedef const clang::Expr* Key;

		iterator find( const Key& key ) {
			auto retval = base_class::find(key);
			if ((*this).end() == retval) {
				retval = base_class::find(IgnoreParenImpCasts(key));
			}
			return retval;
		}
		const_iterator find( const Key& key ) const {
			auto retval = base_class::find(key);
			if ((*this).end() == retval) {
				retval = base_class::find(IgnoreParenImpCasts(key));
			}
			return retval;
		}
	};


	/* A "replacement action" object represents an "action" that replaces an element in the source
	text (with something else). There are different types (subclasses) of replacement actions
	depending various factors (like what is being replaced, and "why" it is being replaced). A
	"replacement action" object may exist without ever being used to actually execute the
	replacement action. Some replacement actions, when executed, may trigger the execution of
	other replacement actions. */
	class CReplacementAction {
	public:
		CReplacementAction(Rewriter &Rewrite) : m_Rewrite(Rewrite) {}
		/* with redundant parameter just for backward compatibility */
		CReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR) : m_Rewrite(Rewrite) {}
		virtual ~CReplacementAction() {}
		virtual void do_replacement(CTUState& state1) const = 0;

		Rewriter& m_Rewrite;
	};

	class CExprTextReplacementAction : public CReplacementAction {
	public:
		CExprTextReplacementAction(Rewriter &Rewrite, const Expr* EX,
			const std::string& replacement_code) : CReplacementAction(Rewrite), m_EX(EX), m_replacement_code(replacement_code) {}
		/* with redundant parameter just for backward compatibility */
		CExprTextReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const Expr* EX,
			const std::string& replacement_code) : CReplacementAction(Rewrite), m_EX(EX), m_replacement_code(replacement_code) {}
		virtual ~CExprTextReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const Expr* m_EX = nullptr;
		std::string m_replacement_code;
	};

	/* A CExprTextYieldingReplacementAction is just like a regular CExprTextReplacementAction, except
	that it will not attempt the modification if the expression already has any modifications pending. */
	class CExprTextYieldingReplacementAction : public CReplacementAction {
	public:
		CExprTextYieldingReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const Expr* EX,
			const std::string& replacement_code) : CReplacementAction(Rewrite, MR), m_EX(EX), m_replacement_code(replacement_code) {}
		virtual ~CExprTextYieldingReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const Expr* m_EX = nullptr;
		std::string m_replacement_code;
	};

	class CDDeclIndirectionReplacementAction : public CReplacementAction {
	public:
		CDDeclIndirectionReplacementAction(Rewriter &Rewrite, const CDDeclIndirection& ddecl_indirection)
			: CReplacementAction(Rewrite), m_ddecl_indirection(ddecl_indirection) {}
		/* with redundant parameter just for backward compatibility */
		CDDeclIndirectionReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR,
				const CDDeclIndirection& ddecl_indirection) : CReplacementAction(Rewrite), m_ddecl_indirection(ddecl_indirection) {}
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

	class CAssignmentTargetConstrainsSourceReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CAssignmentTargetConstrainsSourceReplacementAction(Rewriter &Rewrite, const CDDeclIndirection& ddecl_indirection1,
				const CDDeclIndirection& ddecl_indirection2)
				: CDDeclIndirectionReplacementAction(Rewrite, ddecl_indirection1), m_ddecl_indirection2(ddecl_indirection2) {}
		/* with redundant parameter just for backward compatibility */
		CAssignmentTargetConstrainsSourceReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CAssignmentTargetConstrainsSourceReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	class CAssignmentSourceConstrainsTargetReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CAssignmentSourceConstrainsTargetReplacementAction(Rewriter &Rewrite, const CDDeclIndirection& ddecl_indirection1,
				const CDDeclIndirection& ddecl_indirection2)
				: CDDeclIndirectionReplacementAction(Rewrite, ddecl_indirection1), m_ddecl_indirection2(ddecl_indirection2) {}
		/* with redundant parameter just for backward compatibility */
		CAssignmentSourceConstrainsTargetReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CDDeclIndirectionReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CAssignmentSourceConstrainsTargetReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
	};

	class CSameTypeReplacementAction : public CDDeclIndirectionReplacementAction {
	public:
		CSameTypeReplacementAction(Rewriter &Rewrite, const CDDeclIndirection& ddecl_indirection1,
				const CDDeclIndirection& ddecl_indirection2)
				: CDDeclIndirectionReplacementAction(Rewrite, ddecl_indirection1), m_ddecl_indirection2(ddecl_indirection2) {}
		CSameTypeReplacementAction(Rewriter &Rewrite, const clang::DeclaratorDecl& ddecl_cref1,
				const clang::DeclaratorDecl& ddecl_cref2)
				: CDDeclIndirectionReplacementAction(Rewrite, CDDeclIndirection(ddecl_cref1, CDDeclIndirection::no_indirection)), m_ddecl_indirection2(CDDeclIndirection(ddecl_cref2, CDDeclIndirection::no_indirection)) {}
		/* with redundant parameter just for backward compatibility */
		CSameTypeReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection1,
				const CDDeclIndirection& ddecl_indirection2)
				: CDDeclIndirectionReplacementAction(Rewrite, ddecl_indirection1), m_ddecl_indirection2(ddecl_indirection2) {}
		CSameTypeReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const clang::DeclaratorDecl& ddecl_cref1,
				const clang::DeclaratorDecl& ddecl_cref2)
				: CDDeclIndirectionReplacementAction(Rewrite, CDDeclIndirection(ddecl_cref1, CDDeclIndirection::no_indirection)), m_ddecl_indirection2(CDDeclIndirection(ddecl_cref2, CDDeclIndirection::no_indirection)) {}
		virtual ~CSameTypeReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		CDDeclIndirection m_ddecl_indirection2;
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
		CSameTypeArray2ReplacementAction(Rewriter &Rewrite, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		/* with redundant parameter just for backward compatibility */
		CSameTypeArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection,
				const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CSameTypeArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const CDDeclIndirection m_ddecl_indirection2;
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

	class CUpdateDeclIndirectionArray2ReplacementAction : public CArray2ReplacementAction {
	public:
		CUpdateDeclIndirectionArray2ReplacementAction(Rewriter &Rewrite, const MatchFinder::MatchResult &MR, const CDDeclIndirection& ddecl_indirection, const CDDeclIndirection& ddecl_indirection2) :
					CArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_ddecl_indirection2(ddecl_indirection2) {}
		virtual ~CUpdateDeclIndirectionArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		CDDeclIndirection m_ddecl_indirection2;
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
		the given CDDeclIndirection. */
		void execute_matching_actions(CTUState& state1, const CDDeclIndirection& ddecl_indirection) {
			auto [iter, end_iter] = base_class::equal_range(ddecl_indirection);
			for (; end_iter != iter; ++iter) {
				(*((*iter).second)).do_replacement(state1);
			}
		}

		/* This function executes the action of all the "replacement action" objects associated with
		the given CDDeclIndirection, then removes them from the map. */
		void do_and_dispose_matching_replacements(CTUState& state1, const CDDeclIndirection& ddecl_indirection) {
			/* The base class map may be modified during loop iterations. Maybe. */
			auto iter = base_class::find(ddecl_indirection);
			while (base_class::end() != iter) {
				//(*((*iter).second)).do_replacement(state1);
				auto shared_ptr = ((*iter).second);
				base_class::erase(iter);
				(*shared_ptr).do_replacement(state1);

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
				const std::string& initializer_info_str) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_DD(ddecl_indirection.m_ddecl_cptr),
					m_current_initialization_expr_str(initializer_info_str) {}
		virtual ~CInitializerArray2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

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
				const clang::ConditionalOperator* CO, const DeclaratorDecl* lhs_DD, const DeclaratorDecl* rhs_DD, const DeclaratorDecl* var_DD = nullptr, size_t var_indirection_level = 0) :
					CDynamicArray2ReplacementAction(Rewrite, MR, ddecl_indirection), m_CO(CO), m_lhs_DD(lhs_DD), m_rhs_DD(rhs_DD), m_var_DD(var_DD), m_var_indirection_level(var_indirection_level) {}
		virtual ~CConditionalOperatorReconciliation2ReplacementAction() {}

		virtual void do_replacement(CTUState& state1) const;

		const clang::ConditionalOperator* m_CO = nullptr;
		const DeclaratorDecl* m_lhs_DD = nullptr;
		const DeclaratorDecl* m_rhs_DD = nullptr;
		const DeclaratorDecl* m_var_DD = nullptr;
		size_t m_var_indirection_level = 0;
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
		if (0 == old_string.length()) {
			return changed_flag;
		}

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

	struct CCodeModificationActionAndID {
		typedef decltype(std::declval<std::hash<std::string> >()(std::declval<std::string>())) id_type;
		CCodeModificationActionAndID() = default;
		CCodeModificationActionAndID(CCodeModificationActionAndID&& src) = default;
		CCodeModificationActionAndID(CCodeModificationActionAndID const& src) = default;
		template<typename TFun, std::enable_if_t<std::is_constructible_v<std::function<void(void)>, TFun>, bool>* = nullptr >
		CCodeModificationActionAndID(TFun&& action, std::optional<id_type> const& maybe_id = {}) : m_action(std::forward<decltype(action)>(action)) , m_maybe_id(maybe_id) {}
		template<typename TFun, std::enable_if_t<std::is_constructible_v<std::function<void(void)>, TFun>, bool>* = nullptr >
		CCodeModificationActionAndID(TFun const& action, std::optional<id_type> const& maybe_id = {}) : m_action(action) , m_maybe_id(maybe_id) {}
		CCodeModificationActionAndID& operator=(CCodeModificationActionAndID&& src) = default;
		CCodeModificationActionAndID& operator=(CCodeModificationActionAndID const& src) = default;
		void operator()() const { m_action(); }
		void operator()() { m_action(); }

		std::function<void(void)> m_action;
		std::optional<id_type> m_maybe_id;
	};

	/* A CCodeModificationActions object stores an ordered map of source location ranges to function
	objects that modify the source text within the corresponding source range. This container is
	used to (re)order the code modification actions such that any action for a given source range is
	executed before any action for any source range that entirely contains (aka "is a superset of")
	the first source range.
	
	You want to try to ensure this because modifying the contents of a source range could (and often
	does) invalidate any subranges (i.e. contained source ranges). */
	class CCodeModificationActions : public std::map<COrderedSourceRange, std::list<CCodeModificationActionAndID> > {
		public:
		typedef std::map<COrderedSourceRange, std::list<CCodeModificationActionAndID> > base_class;
		using base_class::base_class;

		void set_ReplaceText_supression_mode(bool mode) {
			m_ReplaceText_supression_mode = mode;
		}
		void clear_ReplaceText_supression_mode() {
			m_ReplaceText_supression_mode = false;
		}
		bool ReplaceText(Rewriter &Rewrite, clang::SourceRange range, std::string_view NewStr) const {
			if (!m_ReplaceText_supression_mode) {
				return Rewrite.ReplaceText(range, NewStr);
			}
			return false;
		}

		std::pair<base_class::iterator, bool> add_replacement_action(const COrderedSourceRange& OSR, const CCodeModificationActionAndID& modifier) {
			auto iter1 = base_class::find(OSR);
			if (base_class::end() != iter1) {
				if (modifier.m_maybe_id.has_value()) {
					auto const& id = modifier.m_maybe_id.value();
					auto predicate1 = [&id](auto& item) -> bool {
						if (item.m_maybe_id.has_value()) {
							auto const& item_id = item.m_maybe_id.value();
							if (item_id == id) {
								return true;
							}
						}
						return false;
					};
					auto iter2 = std::find_if((*iter1).second.begin(), (*iter1).second.end(), predicate1);
					if ((*iter1).second.end() != iter2) {
						/* There is already a "modification action" with the same id. We'll consider the given action 
						either redundant or an update and just overwrite the existing action. */
						(*iter2).m_action = modifier.m_action;
						return std::pair<base_class::iterator, bool>(iter1, false);
					}
				}

				(*iter1).second.push_back(modifier);
				return std::pair<base_class::iterator, bool>(iter1, false);
			} else {
				decltype((*iter1).second) function_list1 { modifier };
				auto res2 = base_class::insert(base_class::value_type(OSR, function_list1));
				assert(base_class::end() != res2.first);
				return std::pair<base_class::iterator, bool>(res2.first, true);
			}
		}
		std::pair<base_class::iterator, bool> add_expression_update_replacement_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, CTUState& state1, clang::Expr const * E);
		std::pair<base_class::iterator, bool> add_straight_text_overwrite_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, const std::string& new_text) {

			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
			DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);
#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
				if (std::string::npos != new_text.find("mse::lh::void_star_replacement")) {
					int q = 5;
				}
			}
#endif /*!NDEBUG*/

			auto lambda = [this, &Rewrite, OSR, new_text]() {
					if (OSR.isValid()) {
						DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
						DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);

#ifndef NDEBUG
						if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
							int q = 5;
							if (std::string::npos != new_text.find("mse::lh::void_star_replacement")) {
								int q = 5;
							}
						}
#endif /*!NDEBUG*/

						ReplaceText(Rewrite, OSR, new_text);
						this->m_already_modified_regions.insert(OSR);

#ifndef NDEBUG
						DEBUG_SOURCE_TEXT_STR(debug_source_text2, OSR, Rewrite);
						if (with_whitespace_removed(new_text.substr(0, debug_source_text1.length())) != with_whitespace_removed(debug_source_text2.substr(0, debug_source_text1.length()))) {
							int q = 7;
						}
#endif /*!NDEBUG*/

					}
				};
			return add_replacement_action(OSR, lambda);
		}
		std::pair<base_class::iterator, bool> add_insert_after_token_at_given_location_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, clang::SourceLocation insertion_point, const std::string& new_text, std::optional<CCodeModificationActionAndID::id_type> maybe_id = {}) {
			auto lambda = [this, &Rewrite, OSR, insertion_point, new_text]() {
					if (insertion_point.isValid()) {
						DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
						const auto debug_SR1 = clang::SourceRange({ insertion_point, insertion_point });
						DEBUG_SOURCE_TEXT_STR(debug_source_text1, debug_SR1, Rewrite);
						const auto debug_SR2 = clang::SourceRange({ insertion_point.getLocWithOffset(+1), insertion_point.getLocWithOffset(+1) });
						DEBUG_SOURCE_TEXT_STR(debug_source_text2, debug_SR2, Rewrite);

#ifndef NDEBUG
						if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
							int q = 5;
							if (string_begins_with(new_text, "mse::lh::void_star_replacement")) {
								int q = 5;
							}
						}
#endif /*!NDEBUG*/

						Rewrite.InsertTextAfterToken(insertion_point, new_text);
						if (true) {
							//auto modified_range = COrderedSourceRange{ insertion_point.getLocWithOffset(+1), OSR.getEnd() };
							//this->m_already_modified_regions.insert(modified_range);
							this->m_already_modified_regions.insert(OSR);
						}
					} else {
						int q = 3;
					}
				};
			return add_replacement_action(OSR, CCodeModificationActionAndID(lambda, maybe_id));
		}
		std::pair<base_class::iterator, bool> add_insert_before_given_location_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, clang::SourceLocation insertion_point, const std::string& new_text, std::optional<CCodeModificationActionAndID::id_type> maybe_id = {}) {
			auto lambda = [this, &Rewrite, OSR, insertion_point, new_text]() {
					if (insertion_point.isValid()) {
						DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
						const auto debug_SR1 = clang::SourceRange({ insertion_point.getLocWithOffset(-1), insertion_point.getLocWithOffset(-1) });
						DEBUG_SOURCE_TEXT_STR(debug_source_text1, debug_SR1, Rewrite);
						const auto debug_SR2 = clang::SourceRange({ insertion_point, insertion_point });
						DEBUG_SOURCE_TEXT_STR(debug_source_text2, debug_SR2, Rewrite);

#ifndef NDEBUG
						if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
							int q = 5;
							if (string_begins_with(new_text, "mse::lh::void_star_replacement")) {
								int q = 5;
							}
						}
#endif /*!NDEBUG*/

						Rewrite.InsertTextBefore(insertion_point, new_text);
						if (true) {
							//auto modified_range = COrderedSourceRange{ insertion_point, OSR.getEnd() };
							//this->m_already_modified_regions.insert(modified_range);
							this->m_already_modified_regions.insert(OSR);
						}
					} else {
						int q = 3;
					}
				};
			return add_replacement_action(OSR, CCodeModificationActionAndID(lambda, maybe_id));
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
								DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);

#ifndef NDEBUG
								if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
									int q = 5;
									if (string_begins_with(replacement_code, "mse::lh::void_star_replacement")) {
										int q = 5;
									}
								}
#endif /*!NDEBUG*/

								//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, OSR, replacement_code);
								auto res2 = ReplaceText(Rewrite, OSR, replacement_code);
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
								DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);

#ifndef NDEBUG
								if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
									int q = 5;
									if (string_begins_with(replacement_code, "mse::lh::void_star_replacement")) {
										int q = 5;
									}
								}
#endif /*!NDEBUG*/

							//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, OSR, replacement_code);
							auto res2 = ReplaceText(Rewrite, OSR, replacement_code);
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
		std::unordered_set<decltype(std::declval<clang::TypeLoc>().getOpaqueData())> m_already_modified_typeLocs;
		bool m_ReplaceText_supression_mode = false;
	};


	/* A definition of a preprocessor macro. */
	class CPPMacroDefinitionInfo {
	public:
		CPPMacroDefinitionInfo(const Token &MacroNameTok, const MacroDirective& MD, bool is_function_macro = false
			, std::string_view macro_def_body_str = {}, std::vector<std::string> parameter_names = {})
			: m_MacroNameTok(MacroNameTok), m_MD_ptr(&MD), m_is_function_macro(is_function_macro)
				, m_macro_def_body_str(macro_def_body_str), m_parameter_names(std::move(parameter_names)) {}

		Token m_MacroNameTok;
		const MacroDirective *m_MD_ptr;
		bool m_is_function_macro = false;
		SourceRange definition_SR() const { return SourceRange{ (*m_MD_ptr).getMacroInfo()->getDefinitionLoc(), (*m_MD_ptr).getMacroInfo()->getDefinitionEndLoc() }; }
		std::string m_macro_def_body_str;
		std::vector<std::string> m_parameter_names;
	};
	/* This container holds information about definitions of preprocessor macros. */
	class CPPMacroDefinitions : public std::map<std::string, CPPMacroDefinitionInfo> {
	public:
	};

	/* A usage of a preprocessor macro. */
	class CPPMacroInstanceInfo {
	public:
		CPPMacroInstanceInfo(const Token &MacroNameTok, const MacroDefinition &MD, SourceRange Range,
								const MacroArgs *Args, bool is_function_macro = false)
			: m_MacroNameTok(MacroNameTok), m_MD(MD), m_Range(Range), m_Args(Args), m_is_function_macro(is_function_macro) {}

		Token m_MacroNameTok;
		MacroDefinition m_MD;
		SourceRange m_Range;
		MacroArgs const *m_Args;
		bool m_is_function_macro = false;
		SourceRange instance_SR() const { return m_Range; }
		SourceRange definition_SR() const { return SourceRange{ m_MD.getMacroInfo()->getDefinitionLoc(), m_MD.getMacroInfo()->getDefinitionEndLoc() }; }
	};
	/* This container holds information about usages of preprocessor macros. */
	class CPPMacroInstances : public std::map<SourceLocation, CPPMacroInstanceInfo> {
	public:
	};

	class CTUState : public CCommonTUState1 {
	public:
		/* This container holds (potential) actions that are meant to be executed upon
		* changes to the conversion state of their corresponding item. */
		CDDeclIndirectionReplacementActionMap m_conversion_state_change_action_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to point to a dynamic array. */
		CArray2ReplacementActionMap m_dynamic_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to point to a dynamic array. */
		CArray2ReplacementActionMap m_native_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to point to an array (dynamic or otherwise). */
		CArray2ReplacementActionMap m_array2_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to point to a pointer target. */
		CDDeclIndirectionReplacementActionMap m_pointer_target_contingent_replacement_map;

		/* This container holds (potential) actions that are meant to be executed if/when
		* their corresponding item is determined to be ineligible for xscope status. */
		CDDeclIndirectionReplacementActionMap m_xscope_ineligibility_contingent_replacement_map;

		/* This container holds information about each item's original type and which
		* type it might be converted to.  */
		CDDeclConversionStateMap m_ddecl_conversion_state_map;

		/* Retrieves a reference to the "declaration conversion state" object associated with the given 
		declaration. If no such object exists, one will be created and stored. */
		std::pair<CDDeclConversionState&, bool> get_ddecl_conversion_state_ref_and_update_flag(clang::DeclaratorDecl const& ddecl, Rewriter* Rewrite_ptr = nullptr, bool function_return_value_only = false) {
			auto* DD = &ddecl;
			auto& state1 = (*this);
			auto  res1 = state1.m_ddecl_conversion_state_map.insert(*DD, Rewrite_ptr, &state1, function_return_value_only);
			auto  ddcs_map_iter = res1.first;
			auto&  ddcs_ref = (*ddcs_map_iter).second;
			return std::pair<CDDeclConversionState&, bool> { ddcs_ref, res1.second };
		}

		/* This container maps "clang::SourceLocation"s to "clang::RecordDecl"s at that
		* location. */
		CLocationToRecordDeclMap m_recdecl_map;

		/* This container holds information about all the "clang::RecordDecl"s (i.e.
		* declarations and definitions of structs, classes, etc.). */
		CRecordDeclConversionStateMap m_recdecl_conversion_state_map;

		/* This container holds information about selected expressions' original text and
		* any modifications we might have made.  */
		CExprConversionStateMap m_expr_conversion_state_map;

	private:

		/* Retrieves a reference to the "expression conversion state" object associated with the given 
		expression. If no such object exists, one will be created and stored. If the existing object 
		is an instance of the CExprConversionState base class, it will replaced by a new object of the 
		specified subclass. If the `overwrite` argument value is true, any existing state will be 
		unconditionally overwitten with a newly constructed one. */
		template<typename TExprConversionState = CExprConversionState, typename TExpr, typename... TArgs>
		auto& get_expr_conversion_state_ref_helper1(bool overwrite, TExpr const& expr, Rewriter &Rewrite, TArgs&&... args) {
			auto* EX = &expr;
			auto& state1 = (*this);
			std::shared_ptr<CExprConversionState> shptr1;
			auto excs_iter = state1.m_expr_conversion_state_map.find(EX);
			bool insert_or_assign = overwrite;
			std::optional<CExprTextModifierStack> maybe_modifier_stack;
			if (state1.m_expr_conversion_state_map.end() == excs_iter) {
				insert_or_assign = true;
			} else {
				auto& excs_ref = (*(*excs_iter).second);
				if constexpr (!std::is_same<TExprConversionState, CExprConversionState>::value) {
					shptr1 = make_expr_conversion_state_shared_ptr<TExprConversionState>(*EX, Rewrite, state1, std::forward<TArgs>(args)...);
					/* Since rtti is not available, we emulate it manually here using the species() virtual member function. */
					if (shptr1->species() != excs_ref.species()) {
						/* The existing "expression conversion state" seems to be of a different type than the specified 
						one. Presumably, it's a generic (base class) "expression conversion state". We'll replace it with 
						the (potentially) more specific one indicated by the template parameter. */
						maybe_modifier_stack = (*(*excs_iter).second).m_expr_text_modifier_stack;
						insert_or_assign = true;
					}
				}
			}
			if (insert_or_assign) {
				if (!shptr1) {
					shptr1 = make_expr_conversion_state_shared_ptr<TExprConversionState>(*EX, Rewrite, state1, std::forward<TArgs>(args)...);
				}
				excs_iter = state1.m_expr_conversion_state_map.insert_or_assign(shptr1);
				if (maybe_modifier_stack.has_value()) {
					/* We're replacing a previously existing "expression conversion state", and so we'll copy any "text 
					modifiers" it may have had. */
					(*(*excs_iter).second).m_expr_text_modifier_stack = maybe_modifier_stack.value();
				}
			}
			return *((*excs_iter).second);
		}

	public:
		/* Retrieves a reference to the "expression conversion state" object associated with the given 
		expression. If no such object exists, one will be created and stored. If the existing object 
		is an instance of the CExprConversionState base class, it will replaced by a new object of the 
		specified subclass. */
		/* Note that you should generally use get_expr_conversion_state_ref() to establish an expression conversion 
		state after the expression conversion state of any ancestor expressions have already been established, as 
		the expression conversion state's constructor will look for and establish a relationship with any existing 
		ancestor expression conversion states. */
		template<typename TExprConversionState = CExprConversionState, typename TExpr, typename... TArgs>
		auto& get_expr_conversion_state_ref(TExpr const& expr, Rewriter &Rewrite, TArgs&&... args) {
			return get_expr_conversion_state_ref_helper1<TExprConversionState>(false/*overwrite*/, expr, Rewrite, std::forward<TArgs>(args)...);
		}

		/* Constructs and stores an "expression conversion state" object associated with the given 
		expression. Any existing stored "expression conversion state" object corresponding to the 
		given expression will be replaced. A reference to the object is returned. */
		template<typename TExprConversionState = CExprConversionState, typename TExpr, typename... TArgs>
		auto& set_expr_conversion_state_ref(TExpr const& expr, Rewriter &Rewrite, TArgs&&... args) {
			return get_expr_conversion_state_ref_helper1<TExprConversionState>(true/*overwrite*/, expr, Rewrite, std::forward<TArgs>(args)...);
		}

		/* This container holds, in sorted order, locations of original source code to be modified
		and functions that will execute the modifications. */
		CCodeModificationActions m_pending_code_modification_actions;

		template<typename TExprConversionState = CExprConversionState, typename TExpr, typename... TArgs>
		std::pair<CCodeModificationActions::base_class::iterator, bool> add_pending_expression_update(TExpr const& expr, Rewriter &Rewrite, TArgs&&... args) {
			auto* EX = &expr;
			auto& state1 = *this;
			auto& ecs_ref = state1.get_expr_conversion_state_ref<TExprConversionState>(expr, Rewrite, std::forward<TArgs>(args)...);

			auto EXSR = write_once_source_range(cm1_adj_nice_source_range(EX->getSourceRange(), state1, Rewrite));
			if (EXSR.isValid()) {
				return state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, EXSR, state1, EX);
			}
			return std::pair<CCodeModificationActions::base_class::iterator, bool>{ state1.m_pending_code_modification_actions.end(), false };
		}

		std::pair<CCodeModificationActions::base_class::iterator, bool> add_pending_straight_text_replacement_expression_update(Rewriter &Rewrite, const COrderedSourceRange& OSR, clang::Expr const * E, std::string_view replacement_text) {
			if (!E) {
				return std::pair<CCodeModificationActions::base_class::iterator, bool> { (*this).m_pending_code_modification_actions.end(), false };
			}
			auto& state1 = *this;
			auto& ecs_ref = state1.get_expr_conversion_state_ref(*E, Rewrite);
			ecs_ref.add_straight_text_replacement_modifier(replacement_text);

			return state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, OSR, state1, E);
		}
		void add_pending_straight_text_replacement_expression_update(clang::Expr const& expr, Rewriter &Rewrite, std::string_view new_text) {
			auto* EX = &expr;
			auto& state1 = (*this);
			auto EXSR = write_once_source_range(cm1_adj_nice_source_range(EX->getSourceRange(), state1, Rewrite));
			if (EXSR.isValid()) {
				add_pending_straight_text_replacement_expression_update(Rewrite, EXSR, EX, new_text);
			}
		}

		/* This container holds information about usages of preprocessor macros. */
		CPPMacroInstances m_pp_macro_instances;

		/* This container holds information about definitions of preprocessor macros. */
		CPPMacroDefinitions m_pp_macro_definitions;

		/* This value seems to be required for certain AST traversing operations. We put
		it here so that we don't have to pass it around separately, but we're not totally
		sure that it will be valid for the entire lifetime of this state. */
		clang::ASTContext* m_ast_context_ptr = nullptr;

		/* This value is required for reading from and writing to the source text. We put
		it here so that we don't have to pass it around separately, but we're not totally
		sure that it will be valid for the entire lifetime of this state. */
		clang::Rewriter* m_Rewrite_ptr = nullptr;
	};


	clang::ASTContext* CExprConversionState::maybe_null_ast_context_ptr() const {
		return m_state1.m_ast_context_ptr;
	}

	CExprTextInfo::CExprTextInfo(const clang::Expr * expr_ptr, Rewriter &Rewrite, CTUState& state1) : m_expr_cptr(expr_ptr), m_state1(state1) {
		if (expr_ptr) {
			m_SR_plus = cm1_adjusted_source_range((*expr_ptr).getSourceRange(), state1, Rewrite);
			auto& expr_SR = m_SR_plus;
			if (expr_SR.isValid()) {
				auto text5 = Rewrite.getRewrittenText(expr_SR);
#ifndef NDEBUG
				if (std::string::npos != expr_SR.m_adjusted_source_text_as_if_expanded.find("c_new != cells + (size - 1)")) {
					int q = 5;
				}
#endif /*!NDEBUG*/
				//m_original_source_text_str = Rewrite.getRewrittenText(cm1_adj_nice_source_range(expr.getSourceRange(), state1, Rewrite));
				bool use_adjusted_source_text_as_if_expanded = expr_SR.m_macro_expansion_range_substituted_with_macro_invocation_range;

#ifndef NDEBUG
				/* This part was just for investigational purposes and can be removed. */
				auto rawSR = (*expr_ptr).getSourceRange();
				if (rawSR.isValid()) {
					auto SL = rawSR.getBegin();
					auto SLE = rawSR.getEnd();
					if (SL.isMacroID() || SLE.isMacroID()) {
						auto& SM = Rewrite.getSourceMgr();
						auto b10 = SM.isMacroArgExpansion(SL);
						auto b10b = SM.isMacroArgExpansion(SLE);
						auto b13 = SM.isMacroBodyExpansion(SL);

						auto sr2 = source_range_with_both_ends_in_the_same_macro_body(rawSR, Rewrite);
						auto SPSR2 = clang::SourceRange{ SM.getSpellingLoc(sr2.getBegin()), SM.getSpellingLoc(sr2.getEnd()) };
						auto text4 = Rewrite.getRewrittenText(SPSR2);

						auto sr = rawSR;

						auto SL_macro_arg_expansion_start = SL;
						bool SL_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SL, &SL_macro_arg_expansion_start);
						auto SLE_macro_arg_expansion_start = SLE;
						bool SLE_isMacroArgExpansion_flag = SM.isMacroArgExpansion(SLE, &SLE_macro_arg_expansion_start);
						if (SL_isMacroArgExpansion_flag && !SLE_isMacroArgExpansion_flag) {
							/* The begin location of the source range appears to in an expression that is used as a macro function 
							argument, while the end location isn't. The end location is presumably part of a macro expansion. So 
							we replace the begin location with the corresponding location in the macro expansion. */
							sr = { SL_macro_arg_expansion_start, SLE };
						} else if ((!SL_isMacroArgExpansion_flag) && SLE_isMacroArgExpansion_flag) {
							sr = { SL, SLE_macro_arg_expansion_start };
						} else if (SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag && (SL_macro_arg_expansion_start != SLE_macro_arg_expansion_start)) {
							/* Are SL and SLE in two different macro arguments? */
							sr = { SL_macro_arg_expansion_start, SLE_macro_arg_expansion_start };
						} else if (SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag && (SL_macro_arg_expansion_start == SLE_macro_arg_expansion_start)) {
							/* SL and SLE seem to be in the same macro argument. */
							use_adjusted_source_text_as_if_expanded = true;
						}
						auto adj_SPSL = SM.getSpellingLoc(sr.getBegin());
						auto adj_SPSLE = SM.getSpellingLoc(sr.getEnd());
						auto adj_SPSR = clang::SourceRange{ adj_SPSL, adj_SPSLE };
						DEBUG_SOURCE_TEXT_STR(debug_adj_SPSR_source_text, adj_SPSR, Rewrite);

						if (state1.m_ast_context_ptr && (b10 || b10b)) {
							//auto parent_E = Tget_immediately_containing_element_of_type<clang::Expr>(expr_ptr, *(state1.m_ast_context_ptr));
							auto parent_E = NonImplicitParentOfType<clang::Expr>(expr_ptr, *(state1.m_ast_context_ptr));
							if (parent_E) {
								auto parent_rawSR = parent_E->getSourceRange();
								if (parent_rawSR.isValid()) {
									auto parent_SL = parent_rawSR.getBegin();
									auto parent_SLE = parent_rawSR.getEnd();
									if (parent_SL.isMacroID() || parent_SLE.isMacroID()) {
										auto parent_b10 = SM.isMacroArgExpansion(parent_SL);
										auto parent_b10b = SM.isMacroArgExpansion(parent_SLE);
										auto parent_b13 = SM.isMacroBodyExpansion(parent_SL);
										auto parent_b13b = SM.isMacroBodyExpansion(parent_SLE);
										if (parent_b13 || parent_b13b) {
											auto parent_expr_SR = cm1_adjusted_source_range((*parent_E).getSourceRange(), state1, Rewrite);
											if (parent_expr_SR.isValid() && (false == parent_expr_SR.m_macro_expansion_range_substituted_with_macro_invocation_range)) {

												auto SR2 = SM.getImmediateExpansionRange(SL).getAsRange();
												auto SPSR2 = clang::SourceRange{ SM.getSpellingLoc(SR2.getBegin()), SM.getSpellingLoc(SR2.getEnd()) };
												auto SR3 = SM.getExpansionRange(SL).getAsRange();

												if (SPSR2.isValid() && SM.isPointWithin(SPSR2.getBegin(), expr_SR.getBegin(), expr_SR.getEnd())
													&& SM.isPointWithin(SPSR2.getEnd(), expr_SR.getBegin(), expr_SR.getEnd())) {

													//auto text2 = Rewrite.getRewrittenText(SR2);
													auto text_sp2 = Rewrite.getRewrittenText(SPSR2);
													//auto text3 = Rewrite.getRewrittenText(SR3);
													if ("" != text_sp2) {
														/* This was some code to substitute the expression source range, presumably at the site of the macro invocation, with the 
														corresponding one in the macro definition body in the case that the expression is a function macro argument, and the 
														expression's parent expression is part of the macro definition body. The problem is that which source range location is 
														more appropriate depends on what it's being used for, and we can't really know that here. */
														//clang::SourceRange& expr_SR_base_ref = expr_SR;
														//expr_SR_base_ref = SPSR2;
														//expr_SR.m_adjusted_source_text_as_if_expanded = text_sp2;
														int q = 5;
													}
												}
											} else {
												int q = 5;
											}
										}
										int q = 5;
									}
								}
							}
						}

						if ("" != expr_SR.m_adjusted_source_text_as_if_expanded) {
							//use_adjusted_source_text_as_if_expanded = true;
						}
					}
				}
#endif /*!NDEBUG*/

				if (use_adjusted_source_text_as_if_expanded) {
					m_original_source_text_str = expr_SR.m_adjusted_source_text_as_if_expanded;
				} else {
					m_original_source_text_str = Rewrite.getRewrittenText(expr_SR);
					if ("" == m_original_source_text_str) {
						auto nice_SR = cm1_adj_nice_source_range((*expr_ptr).getSourceRange(), state1, Rewrite);
						if (nice_SR.isValid()) {
							m_original_source_text_str = Rewrite.getRewrittenText(nice_SR);
						}
					}
				}

			/*
			auto SR = cm1_adj_nice_source_range(expr_ptr->getSourceRange(), state1, Rewrite);
			if (SR.isValid()) {
				m_original_source_text_str = Rewrite.getRewrittenText(SR);
			*/
			} else {
				int q = 3;
			}
		} else {
			int q = 3;
		}
	}

	std::string const& CExprTextInfo::current_text(std::optional<CExprTextInfoContext> maybe_context/* = {}*/) const {
		auto iter = m_state1.m_expr_conversion_state_map.find(m_expr_cptr);
		if (m_state1.m_expr_conversion_state_map.end() != iter) {
			return (*iter).second->current_text(maybe_context);
		}
		if (maybe_context.has_value() && maybe_context.value().m_root_SR.isValid()) {
			auto& context = maybe_context.value();
			/* The presence of a context implies that this element may be being rendered as a component of a 
			containing element. The representation of that containing element may be in the definition body of 
			a macro. And the representation of this element may be in the body of another macro that is invoked 
			in the body of the containing macro. In such case the representation of this element will presumably 
			be used in the body of the containing macro, so we'll try to return a representation that is valid 
			in the containing macro. The m_SR_plus member field actually (hopefully) stores representations valid 
			for each containing macro at various levels of (macro invocation) nesting. So we'll try to find one 
			that seems to correspond to the element associated with the context. */

			if (2 <= m_SR_plus.m_adjusted_source_text_infos.size()) {
				if (std::string::npos != m_SR_plus.m_adjusted_source_text_infos.at(m_SR_plus.m_adjusted_source_text_infos.size() - 1).m_text.find("outlen")) {
					int q = 5;
				}
			}

			for (size_t ind = 0; m_SR_plus.m_adjusted_source_text_infos.size() > ind; ind += 1) {
				auto& adjusted_source_text_info_ref = m_SR_plus.m_adjusted_source_text_infos.at(ind);

				bool b1 = adjusted_source_text_info_ref.m_macro_definition_range.isValid() && (!((context.m_root_SR.getBegin() < adjusted_source_text_info_ref.m_macro_definition_range.getBegin())
					|| (adjusted_source_text_info_ref.m_macro_definition_range.getEnd() < context.m_root_SR.getEnd())));

				/* We expect this criteria should actually only be potentially relevant for the last adjusted_source_text_info 
				for which a valid m_macro_definition_range wouldn't be available. */
				bool b2 = adjusted_source_text_info_ref.m_macro_invocation_range.isValid() && (!((context.m_root_SR.getBegin() < adjusted_source_text_info_ref.m_macro_invocation_range.getBegin())
					|| (adjusted_source_text_info_ref.m_macro_invocation_range.getEnd() < context.m_root_SR.getEnd())));

				if (b1 || b2) {
					/* We found a macro whose body seems to contain the element associated with the context. */

					if ("" != adjusted_source_text_info_ref.m_text) {
						/* So we'll return the stored representation that should be valid in the body of that macro. */
						return adjusted_source_text_info_ref.m_text;
					}
				}
			}
			if (2 <= m_SR_plus.m_adjusted_source_text_infos.size()) {
				if (std::string::npos != m_SR_plus.m_adjusted_source_text_infos.at(m_SR_plus.m_adjusted_source_text_infos.size() - 1).m_text.find("outlen")) {
					int q = 5;
				}
				bool context_might_be_outside_any_macro = ((!(m_SR_plus.m_original_source_range.getBegin().isMacroID())) || (!(m_SR_plus.m_original_source_range.getEnd().isMacroID())));
				if (context_might_be_outside_any_macro) {
					/* While this expression seems to be in a macro, it's possible that the context expression isn't, 
					in which case, none of the containing macros would contain the context expression. In such case 
					the outer-most (i.e. least deeply nested) adjusted version of the source text should probably work. */
					auto& adjusted_source_text_info_ref = m_SR_plus.m_adjusted_source_text_infos.at(m_SR_plus.m_adjusted_source_text_infos.size() - 1);
					if ("" != adjusted_source_text_info_ref.m_text) {
						return adjusted_source_text_info_ref.m_text;
					}
				} else {
					/* Do we ever get here? */
					int q = 5;
				}
			}
		}
		if (m_SR_plus.m_macro_expansion_range_substituted_with_macro_invocation_range) {
			if ("" != m_SR_plus.m_adjusted_source_text_as_if_expanded) {
				return m_SR_plus.m_adjusted_source_text_as_if_expanded;
			} else {
				int q = 3;
			}
		}
		return m_original_source_text_str;
	}

	bool CExprConversionState::set_up_child_depenedencies_generically() {
		if ((!m_expr_cptr) || ("" == m_original_source_text_str) || (!m_state1.m_ast_context_ptr)) {
			return false;
		}
		auto& SM = m_state1.m_ast_context_ptr->getSourceManager();

		struct COrderedExpr {
			bool operator<(const COrderedExpr& rhs) const {
				return (m_OSR < rhs.m_OSR);
			}
			clang::Expr const* m_E = nullptr;
			COrderedSourceRange m_OSR;
			std::string m_original_text;
			size_t m_start_pos = std::string::npos;
			CExprTextInfo m_text_info;
		};
		std::set<COrderedExpr> visible_child_infos;

		auto whole_rawSR = m_expr_cptr->getSourceRange();
		if (!(whole_rawSR.isValid())) {
			return false;
		}
		bool b1 = whole_rawSR.getBegin().isMacroID();
		bool b2 = whole_rawSR.getEnd().isMacroID();

		{
			auto whole_SR = write_once_source_range(cm1_adj_nice_source_range(m_expr_cptr->getSourceRange(), m_state1, Rewrite));
			if (!(whole_SR.isValid())) {
				return false;
			}

			auto children_iters = m_expr_cptr->children();
			for (auto child_iter : children_iters) {
				if (child_iter) {
					static_assert(std::is_convertible<decltype(*child_iter), clang::Stmt const&>::value, "");
					auto E = dyn_cast<const clang::Expr>(&(*child_iter));
					if (E) {
						struct CExprBasicInfo {
							CExprBasicInfo(const clang::Expr* E, COrderedSourceRange OSR) : m_E(E), m_OSR(OSR) {}
							const clang::Expr* m_E;
							COrderedSourceRange m_OSR;
						};
						std::vector<CExprBasicInfo> l_descendants_contained_in_range;

						auto rawSR = E->getSourceRange();
						bool b3 = rawSR.getBegin().isMacroID();
						bool b4 = rawSR.getEnd().isMacroID();

						auto OSR = write_once_source_range(cm1_adj_nice_source_range(E->getSourceRange(), m_state1, Rewrite));
						if (OSR.isValid()) {
							bool b5 = OSR.getBegin().isMacroID();
							bool b6 = OSR.getEnd().isMacroID();

							if ((OSR.getBegin() < whole_SR.getBegin()) || (whole_SR.getEnd() < OSR.getEnd())) {
								/* The source range of this element does not seem to be contained inside the source range of the parent 
								expression. This can happen, for example, when macros are involved. But it's possible that some of the 
								element' descendants actually are contained in the source range of the parent expression. (Macro 
								function arguments, for example.) So we're going to search for such descendants and treat them, for 
								rendering purposes, as if they were (direct) children of the parent expression. */

								struct CB {
									static auto descendants_contained_in_range(COrderedSourceRange whole_SR, clang::Expr const& expr_cref, Rewriter& Rewrite, CTUState& state1) -> std::vector<CExprBasicInfo> {
										auto expr_cptr = &expr_cref;

										std::vector<CExprBasicInfo> l_descendants_contained_in_range;

										auto children_iters = expr_cptr->children();
										for (auto child_iter : children_iters) {
											if (child_iter) {
												static_assert(std::is_convertible<decltype(*child_iter), clang::Stmt const&>::value, "");
												auto E = dyn_cast<const clang::Expr>(&(*child_iter));
												if (E) {
													auto rawSR = E->getSourceRange();
													bool b3 = rawSR.getBegin().isMacroID();
													bool b4 = rawSR.getEnd().isMacroID();

													bool no_descendant_does_not_seem_to_be_within_ancestor_source_range = false;
													auto OSR = write_once_source_range(cm1_adj_nice_source_range(E->getSourceRange(), state1, Rewrite));
													if (OSR.isValid()) {
														bool b5 = OSR.getBegin().isMacroID();
														bool b6 = OSR.getEnd().isMacroID();

														if ((OSR.getBegin() < whole_SR.getBegin()) || (whole_SR.getEnd() < OSR.getEnd())) {
															no_descendant_does_not_seem_to_be_within_ancestor_source_range = true;
														} else {
															l_descendants_contained_in_range.push_back({ E, OSR });
														}
													} else {
														no_descendant_does_not_seem_to_be_within_ancestor_source_range = true;
													}
													if (no_descendant_does_not_seem_to_be_within_ancestor_source_range) {
														auto res1 = descendants_contained_in_range(whole_SR, *E, Rewrite, state1);
														for (auto& visible_descendant_ref : res1) {
															l_descendants_contained_in_range.push_back(visible_descendant_ref);
														}
													}
												}
											}
										}
										return l_descendants_contained_in_range;
									}
								};
								auto res1 = CB::descendants_contained_in_range(whole_SR, *E, Rewrite, m_state1);
								for (auto& visible_descendant_ref : res1) {
									l_descendants_contained_in_range.push_back(visible_descendant_ref);
								}
							} else {
								l_descendants_contained_in_range.push_back({ E, OSR });
							}
							for (auto& visible_descendant : l_descendants_contained_in_range) {
								auto E = visible_descendant.m_E;
								auto OSR = visible_descendant.m_OSR;

								std::string child_original_source_text_str;
								auto text_info = CExprTextInfo(E, Rewrite, m_state1);
								auto excs_iter = m_state1.m_expr_conversion_state_map.find(E);
								if (m_state1.m_expr_conversion_state_map.end() != excs_iter) {
									child_original_source_text_str = (*excs_iter).second->m_original_source_text_str;
								} else {
									child_original_source_text_str = text_info.m_original_source_text_str;
								}
								if ("" != child_original_source_text_str) {
									//SM.getCharacterData();
									auto pos = SM.getFileOffset(OSR.getBegin()) - SM.getFileOffset(whole_SR.getBegin());
									if (0 <= pos) {
										visible_child_infos.insert(COrderedExpr{ E, OSR, child_original_source_text_str, pos, text_info });
									}
								}
							}
						}
					}
				}
			}
			if (1 > visible_child_infos.size()) {
				return true;
			}

			{
				auto last_iter = visible_child_infos.begin();
				size_t last_pos = last_iter->m_start_pos + last_iter->m_original_text.length();
				auto iter = last_iter;
				++iter;
				for (; visible_child_infos.end() != iter; ++iter) {
					while (iter->m_start_pos < last_pos) {
						/* The current element seems to overlap the previous one. We assume that the current element is 
						contained inside the previous one. We'll delete the previous one. */
						auto to_be_erased = last_iter;
						if (visible_child_infos.begin() != last_iter) {
							--last_iter;
							last_pos = last_iter->m_start_pos + last_iter->m_original_text.length();
							visible_child_infos.erase(to_be_erased);
						} else {
							last_iter = iter;
							visible_child_infos.erase(to_be_erased);
							break;
						}
					}
					last_iter = iter;
					last_pos = last_iter->m_start_pos + last_iter->m_original_text.length();
				}
			}

			std::vector<CExprTextInfo> child_text_infos;
			std::vector<std::string> non_child_dependent_text_fragments;
			size_t last_pos = 0;
			for (auto& visible_child_info : visible_child_infos) {
				if (m_original_source_text_str.length() <= last_pos) {
					return false;
				}
				std::string text_frag = m_original_source_text_str.substr(last_pos, visible_child_info.m_start_pos - last_pos);
				non_child_dependent_text_fragments.push_back(text_frag);
				last_pos = visible_child_info.m_start_pos + visible_child_info.m_original_text.length();
				child_text_infos.push_back(visible_child_info.m_text_info);
			}
			if (m_original_source_text_str.length() > last_pos) {
				std::string text_frag = m_original_source_text_str.substr(last_pos);
				non_child_dependent_text_fragments.push_back(text_frag);
			} else {
				non_child_dependent_text_fragments.push_back("");
			}
			
			m_non_child_dependent_text_fragments = non_child_dependent_text_fragments;
			m_child_text_infos.clear();
			for (auto& child_text_info : child_text_infos) {
				m_child_text_infos.push_back(child_text_info);
			}
		}
		return true;
	}

	bool CExprConversionState::set_up_relation_to_parent_generically() {
		if (m_state1.m_ast_context_ptr) {
			bool has_ancestor_with_conversion_state = false;
			//auto parent_E = NonParenImpNoopCastParentOfType<clang::Expr>(m_expr_cptr, *(m_state1.m_ast_context_ptr));
			auto parent_E = NonImplicitParentOfType<clang::Expr>(m_expr_cptr, *(m_state1.m_ast_context_ptr));
			auto E1 = parent_E;
			while (E1) {
				auto excs_iter = m_state1.m_expr_conversion_state_map.find(E1);
				if (m_state1.m_expr_conversion_state_map.end() != excs_iter) {
					has_ancestor_with_conversion_state = true;
					break;
				}
				E1 = NonImplicitParentOfType<clang::Expr>(E1, *(m_state1.m_ast_context_ptr));
			}
			if (has_ancestor_with_conversion_state) {
				assert(parent_E);
				/* The following call (as a side-effect) will create and "register" a conversion state object for the 
				parent expression if it does not already have one. Such creation may, in turn, recursively create 
				conversion state objects for a line of ancestor expressions so that in any line of heritage, all 
				conversion state objects will be part of a connected unbroken chain of conversion state objects. In 
				practice, this allows any (potentially) modified expression to incorporate any changes to any of its 
				subexpressions. */
				m_state1.get_expr_conversion_state_ref(*parent_E, Rewrite);
			}
			return has_ancestor_with_conversion_state;
		}
		return false;
	}

	class CConditionalOperatorExprConversionState : public CExprConversionState {
	public:
		CConditionalOperatorExprConversionState(const clang::ConditionalOperator& co_cref, Rewriter &Rewrite, CTUState& state1) 
			: CExprConversionState(co_cref, Rewrite, state1), m_cond_text_info(IgnoreParenImpCasts(co_cref.getCond()), Rewrite, state1), m_lhs_text_info(IgnoreParenImpCasts(co_cref.getLHS()), Rewrite, state1), m_rhs_text_info(IgnoreParenImpCasts(co_cref.getRHS()), Rewrite, state1) {}

		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) override {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			std::string updated_text = m_cond_text_info.current_text(new_maybe_context) + " ? ";
			if (m_lhs_needs_to_be_cast) {
				updated_text += m_arg_prefix_str + m_lhs_text_info.current_text(new_maybe_context) + m_arg_suffix_str;
			} else {
				updated_text += m_lhs_text_info.current_text(new_maybe_context);
			}
			updated_text += " : ";
			if (m_rhs_needs_to_be_cast) {
				updated_text += m_arg_prefix_str + m_rhs_text_info.current_text(new_maybe_context) + m_arg_suffix_str;
			} else {
				updated_text += m_rhs_text_info.current_text(new_maybe_context);
			}
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("conditional operator");
			return sc_species_str;
		}

		std::string m_arg_prefix_str;
		std::string m_arg_suffix_str;
		bool m_lhs_needs_to_be_cast = false;
		bool m_rhs_needs_to_be_cast = false;

	private:
		CExprTextInfo m_cond_text_info;
		CExprTextInfo m_lhs_text_info;
		CExprTextInfo m_rhs_text_info;
	};

	class CCallExprConversionState : public CExprConversionState {
	public:
		CCallExprConversionState(const clang::Expr& call_expr_cref, Rewriter &Rewrite, CTUState& state1, std::vector<const clang::Expr *> arg_expr_cptrs, std::string_view function_name)
			: CExprConversionState(call_expr_cref, Rewrite, state1), m_arg_expr_cptrs(std::move(arg_expr_cptrs)), m_function_name(function_name) {
				for (auto& arg_expr_cptr : m_arg_expr_cptrs) {
					m_arg_text_infos.push_back(CExprTextInfo{ IgnoreParenImpCasts(arg_expr_cptr), Rewrite, state1 });
				}
			}

		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) override {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			std::string updated_text = m_function_name + "(";
			for (auto& arg_text_info : m_arg_text_infos) {
				updated_text += arg_text_info.current_text(new_maybe_context) + ", ";
			}
			if (1 <= m_arg_text_infos.size()) {
				updated_text = updated_text.substr(0, updated_text.length() - 2);
			}
			updated_text += ")";
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("call expression");
			return sc_species_str;
		}

	private:
		std::vector<CExprTextInfo> m_arg_text_infos;
		std::vector<const clang::Expr *> m_arg_expr_cptrs;
		std::string m_function_name;
	};

	class CSingleArgCallExprConversionState : public CExprConversionState {
	public:
		CSingleArgCallExprConversionState(const clang::Expr& call_expr_cref, Rewriter &Rewrite, CTUState& state1, const clang::Expr& arg_expr_cref, std::string_view function_name)
			: CExprConversionState(call_expr_cref, Rewrite, state1), m_arg_text_info(IgnoreParenImpCasts(&arg_expr_cref), Rewrite, state1), m_arg_expr_cptr(&arg_expr_cref), m_function_name(function_name) {}

		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) override {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			std::string updated_text = m_function_name + "(" + m_arg_text_info.current_text(new_maybe_context) + ")";
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("single arg call expression");
			return sc_species_str;
		}

	private:
		CExprTextInfo m_arg_text_info;
		const clang::Expr* m_arg_expr_cptr = nullptr;
		std::string m_function_name;
	};

	class CCastExprConversionState : public CExprConversionState {
	public:
		CCastExprConversionState(const clang::Expr& cast_expr_cref, Rewriter &Rewrite, CTUState& state1, const clang::Expr& arg_expr_cref, std::string_view prefix, std::string_view suffix)
			: CExprConversionState(cast_expr_cref, Rewrite, state1), m_arg_text_info(IgnoreParenImpCasts(&arg_expr_cref), Rewrite, state1), m_arg_expr_cptr(&arg_expr_cref), m_prefix(prefix), m_suffix(suffix) {}

		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) override {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			std::string updated_text = m_prefix + m_arg_text_info.current_text(new_maybe_context) + m_suffix;
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("cast expression");
			return sc_species_str;
		}

	private:
		CExprTextInfo m_arg_text_info;
		const clang::Expr* m_arg_expr_cptr = nullptr;
		std::string m_prefix;
		std::string m_suffix;
	};

	class CAddressofArraySubscriptExprConversionState : public CExprConversionState {
	public:
		CAddressofArraySubscriptExprConversionState(const clang::UnaryOperator& addrofexpr_cref, Rewriter &Rewrite, CTUState& state1, const clang::ArraySubscriptExpr& arraysubscriptexpr_cref) 
			: CExprConversionState(addrofexpr_cref, Rewrite, state1), m_array_text_info(IgnoreParenImpCasts(arraysubscriptexpr_cref.getBase()), Rewrite, state1), m_index_text_info(IgnoreParenImpCasts(arraysubscriptexpr_cref.getIdx()), Rewrite, state1) {}

		virtual void update_current_text(std::optional<CExprTextInfoContext> maybe_context = {}) override {
			auto new_maybe_context = maybe_context.has_value() ? maybe_context : maybe_default_context();

			std::string updated_text = "((" + m_array_text_info.current_text(new_maybe_context) + ") + (" + m_index_text_info.current_text(new_maybe_context) + "))";
			updated_text = modified_copy(updated_text);
			m_current_text_str = updated_text;
		}
		virtual std::string const& species() const {
			static const auto sc_species_str = std::string("address of array subscript");
			return sc_species_str;
		}

	private:
		CExprTextInfo m_array_text_info;
		CExprTextInfo m_index_text_info;
	};

	std::pair<CCodeModificationActions::base_class::iterator, bool> CCodeModificationActions::add_expression_update_replacement_action(Rewriter &Rewrite, const COrderedSourceRange& OSR, CTUState& state1, clang::Expr const * E) {

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);

#ifndef NDEBUG
		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto lambda = [this, &Rewrite, OSR, &state1, E]() {
				if (OSR.isValid()) {
					DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, OSR, Rewrite);
					DEBUG_SOURCE_TEXT_STR(debug_source_text1, OSR, Rewrite);

#ifndef NDEBUG
					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					auto iter = state1.m_expr_conversion_state_map.find(E);
					if (state1.m_expr_conversion_state_map.end() != iter) {
						auto& ecs_ref = *((*iter).second);
						auto& current_text_ref = ecs_ref.current_text();
						if (current_text_ref != ecs_ref.m_original_source_text_str) {
							//ReplaceText(Rewrite, OSR, current_text_ref);
							state1.m_pending_code_modification_actions.ReplaceText(Rewrite, OSR, current_text_ref);
							this->m_already_modified_regions.insert(OSR);
						}
					}
				}
			};
		return add_replacement_action(OSR, lambda);
	}

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

	std::optional<clang::SourceLocation> matching_close_parentheses_if_any(Rewriter& Rewrite, clang::SourceLocation open_paren_SL, std::optional<clang::SourceLocation> maybe_inclusive_bounding_SL = {}) {
		/* Current implementation is a naive hack that generally works for our purposes. */
		std::optional<clang::SourceLocation> retval;
		if (!open_paren_SL.isValid()) {
			return retval;
		}
		const auto default_bounding_offset = 500/*arbitrary*/;

		auto& SM = Rewrite.getSourceMgr();
		auto char_data = SM.getCharacterData(open_paren_SL);
		if (!char_data) {
			return retval;
		}

		int nested_depth = 0;
		int offset = 1;
		auto SL = open_paren_SL.getLocWithOffset(offset);
		std::string text1 = Rewrite.getRewrittenText({ SL, SL });
		while (true) {
			if (maybe_inclusive_bounding_SL.has_value()) {
				if (maybe_inclusive_bounding_SL.value() < SL) {
					return retval;
				}
			} else {
				if (default_bounding_offset < offset) {
					static bool note_given = false;
					if (!note_given) {
						llvm::errs() << "\nnote: matching_close_parentheses_if_any() search cut off due to exceeding the (preset) limit. \n";
						note_given = true;
					}
					return retval;
				}
			}
			DEBUG_SOURCE_LOCATION_STR(SL_debug_source_location_str, clang::SourceRange(SL, SL), Rewrite);

			if (string_begins_with(text1, "(") /* "(" == text1 */) {
				nested_depth -= 1;
			} else if (string_begins_with(text1, ")") /* ")" == text1 */) {
				nested_depth += 1;
			}
			if (1 <= nested_depth) {
				return SL;
			}

			++offset;
			SL = open_paren_SL.getLocWithOffset(offset);
			if (!SL.isValid()) {
				return retval;
			}
			text1 = Rewrite.getRewrittenText({ SL, SL });

			std::string text_from_the_open_paren = Rewrite.getRewrittenText({ open_paren_SL, SL });
			std::string char_data_text_from_the_open_paren;
			char_data_text_from_the_open_paren.reserve(text_from_the_open_paren.length());
			for (size_t i = 0; i < text_from_the_open_paren.length(); ++i) {
				char_data_text_from_the_open_paren.push_back(char_data[i]);
			}
			if (text_from_the_open_paren != char_data_text_from_the_open_paren) {
				/* The text read from the current source location no longer matches the text in the char_data starting 
				from open_paren_SL. This might indicate that the source text was changed so that the source locations 
				no longer properly correspond to their original source text. So can't we be sure that the return value 
				we would have produced would have been correct. So we'll just bail here. */
				return retval;
			}
		};
		return retval;
	}

	static clang::SourceRange extended_to_include_all_directly_enclosing_parens(clang::SourceRange const& SR, CTUState& state1, clang::Rewriter &Rewrite) {

		auto extended_to_include_one_level_of_enclosing_parens_if_present = [](clang::SourceRange const& SR, CTUState& state1, clang::Rewriter &Rewrite) {
			auto retval = SR;
			auto SL1 = SR.getBegin();
			std::string text1 = Rewrite.getRewrittenText({ SL1, SL1 });
			auto left_SL1 = SL1.getLocWithOffset(-1);
			auto right_SL1 = SL1.getLocWithOffset(+text1.length());

			std::string text3;
			if (left_SL1.isValid()) {
				text3 = Rewrite.getRewrittenText({ left_SL1, left_SL1 });
				while ("" == text3) {
					left_SL1 = left_SL1.getLocWithOffset(-1);
					if (!(left_SL1.isValid())) {
						break;
					}
					text3 = Rewrite.getRewrittenText({ left_SL1, left_SL1 });
				}
			}
			if ("(" == text3) {
				auto maybe_close_paren_SL = matching_close_parentheses_if_any(Rewrite, left_SL1);
				if (maybe_close_paren_SL.has_value()) {
					retval = clang::SourceRange{ left_SL1, maybe_close_paren_SL.value() };
				}
			}
			return retval;
		};

		auto last_SR = SR;
		auto SR2 = extended_to_include_one_level_of_enclosing_parens_if_present(SR, state1, Rewrite);
		while (SR2 != last_SR) {
			last_SR = SR2;
			SR2 = extended_to_include_one_level_of_enclosing_parens_if_present(SR2, state1, Rewrite);
		}

		return SR2;
	}

	static std::optional<clang::SourceRange> extended_to_include_prefix_if_present(std::string_view prefix, clang::SourceRange const& SR, CTUState& state1, clang::Rewriter &Rewrite) {
		auto retval = SR;

		auto location_of_first_preceding_non_whitespace_if_any = [](clang::SourceLocation const& SL, CTUState& state1, clang::Rewriter &Rewrite) -> std::optional<clang::SourceLocation> {
			auto SL2 = SL.getLocWithOffset(-1);
			if (SL2.isValid()) {
				auto text1 = Rewrite.getRewrittenText({ SL2, SL2 });
				while (!(("" == text1) || ((1 == text1.length()) && (std::isspace(text1.at(0)))))) {
					SL2 = SL2.getLocWithOffset(-1);
					if (SL2.isValid()) {
						text1 = Rewrite.getRewrittenText({ SL2, SL2 });
						int q = 5;
					} else {
						return {};
					}
				}
			} else {
				return {};
			}
			return SL2;
		};
		auto maybe_location_of_first_preceding_non_whitespace = location_of_first_preceding_non_whitespace_if_any(SR.getBegin(), state1, Rewrite);
		if (maybe_location_of_first_preceding_non_whitespace.has_value()) {
			auto SL2 = maybe_location_of_first_preceding_non_whitespace.value();
			auto SL1 = SL2.getLocWithOffset( -(prefix.length()) );
			if (SL1.isValid()) {
				auto SR2 = clang::SourceRange{ SL1, SL2 };
				if ((SR2).isValid() && (((SR2).getBegin() < (SR2).getEnd()) || ((SR2).getBegin() == (SR2).getEnd()))) {
					auto SR2_source_text = Rewrite.getRewrittenText(SR2);
					if (prefix == SR2_source_text) {
						auto SR3 = clang::SourceRange{ SR2.getBegin(), SR.getEnd() };
						if (SR3.isValid()) {
							retval = SR3;
						}
					}
				}
			}
		}

		return retval;
	}

	/* The problem is that, while we can obtain the source range of a gnu attribute argument, often we
	want the whole "expression" including the `__attribute__` part, but that entity doesn't seem to 
	have a corresponding node in the AST tree, so it's not clear how one would properly obtain the 
	source range. So when the may_be_a_gnu_attr parameter indicates it, (in kind of a hacky way) we'll 
	check here if the given source range seems to be a gnu attribute argument, and if so, try to 
	instead return an extended range that covers the entire gnu attribute specifier. */
	static std::optional<clang::SourceRange> extended_to_include_entire_gnu_attribute_if_any(clang::SourceRange const& SR, CTUState& state1, clang::Rewriter &Rewrite) {
		auto SR2 = extended_to_include_all_directly_enclosing_parens(SR, state1, Rewrite);
		return extended_to_include_prefix_if_present("__attribute__", SR2, state1, Rewrite);
	}

	/* Just returns the given range unmodified unless the source range refers to (part of) a macro,
	in which case it uses a (currently oversimplistic) heuristic to guess whether the macro definition
	is just a single expression (as opposed to, for example, a declaration, or a compound statement,
	or whatever). If so, then it will return the (macro) instantiation source range, otherwise it will
	return the (macro) definition source range. */
	static CSourceRangePlus cm1_adjusted_source_range(const clang::SourceRange& sr, CTUState& state1, clang::Rewriter &Rewrite, bool may_be_a_gnu_attr/* = false*/) {

		/* The may_be_a_gnu_attr parameter is indicates that the given source range may refer to a gnu 
		attribute argument. That is, the argument of `__attribute__(...)`. The problem is that often we
		want the whole "expression" including the `__attribute__` part, but that entity doesn't seem to 
		have a corresponding node in the AST tree, so it's not clear how one would properly obtain the 
		source range. So when the may_be_a_gnu_attr parameter indicates it, (in kind of a hacky way) 
		we'll  check here if the given source range seems to be a gnu attribute argument, and if so, 
		try to instead return an extended range that covers the entire gnu attribute specifier. */

		CSourceRangePlus retval = sr;
		retval.m_original_source_range = sr;

		auto rawSR = sr;
		auto SL = rawSR.getBegin();
		auto SLE = rawSR.getEnd();
		auto b3 = SL.isMacroID();
		auto b4 = SLE.isMacroID();

		auto& SM = Rewrite.getSourceMgr();
		auto FLSL = SM.getFileLoc(SL);
		auto FLSLE = SM.getFileLoc(SLE);
		auto b5 = FLSL.isMacroID();
		auto b6 = FLSLE.isMacroID();
		auto b6b = FLSL.isFileID();
		auto FLSR = clang::SourceRange{ FLSL, FLSLE };

		auto SPSL = SM.getSpellingLoc(SL);
		auto SPSLE = SM.getSpellingLoc(SLE);
		auto b7 = SPSL.isMacroID();
		auto b8 = SPSLE.isMacroID();
		auto b8b = SPSL.isFileID();
		auto b8c = SPSLE.isFileID();
		auto SPSR = clang::SourceRange{ SPSL, SPSLE };

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, sr, Rewrite);

		std::string SPSR_source_text;
		if ((SPSR).isValid() && (((SPSR).getBegin() < (SPSR).getEnd()) || ((SPSR).getBegin() == (SPSR).getEnd()))) {
			SPSR_source_text = Rewrite.getRewrittenText(SPSR);
			if ("" != SPSR_source_text) {
				retval.m_adjusted_source_text_as_if_expanded = SPSR_source_text;
			}
		}

		DEBUG_SOURCE_TEXT_STR(debug_source_text, sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_fl_source_text, FLSR.isValid() ? FLSR : sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_sp_source_text, SPSR.isValid() ? SPSR : sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_sl_source_text, SL.isValid() ? clang::SourceRange({ SPSL, SPSL }) : sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_sle_source_text, SLE.isValid() ? clang::SourceRange({ SPSLE, SPSLE }) : sr, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

		if (b3 || b4) {
			/* The element is part of a macro instance. */
			auto& SM = Rewrite.getSourceMgr();

			auto b10 = SM.isMacroArgExpansion(SL);
			auto b10b = SM.isMacroArgExpansion(SLE);
			auto b11 = SM.isMacroArgExpansion(FLSL);
			auto b12 = SM.isMacroArgExpansion(SPSL);
			auto b12b = SM.isMacroArgExpansion(SPSLE);
			auto b13 = SM.isMacroBodyExpansion(SL);
			auto b14 = SM.isMacroBodyExpansion(FLSL);
			auto b15 = SM.isMacroBodyExpansion(SPSL);
			auto b15b = SM.isMacroBodyExpansion(SPSLE);
			if (b10) {
				if (!b10b) {
					/* It seems that beginning of the source range is a macro function argument, but the end isn't. 
					(Presumably the end is at another part of the macro body.) Rewrite.getRewrittenText() doesn't 
					seem to work on such mismatched source ranges. This should be handled by the 
					source_range_with_both_ends_in_the_same_macro_body() function. */
					int q = 5;
				} else {
					int q = 5;
				}
			}
			if (true && ((b3 && (!b4)) || ((!b3) && b4))) {
				int q = 5;
				/* Note that we specifically omit the (defaulted) CTUState pointer parameter as not omitting it 
				risks infinite recursion with this function. */
				return cm1_nice_source_range(sr, Rewrite);
			}

			auto macro_spelling_range_extended_to_include_any_arguments = [&SM, &Rewrite, &state1](clang::SourceRange const& macro_SR) {
					auto adjusted_macro_SPSR = clang::SourceRange{ SM.getSpellingLoc(macro_SR.getBegin()), SM.getSpellingLoc(macro_SR.getEnd()) };
					std::string macro_name;

					if (("" == macro_name) && adjusted_macro_SPSR.isValid() && ((adjusted_macro_SPSR.getBegin() < adjusted_macro_SPSR.getEnd()) || (adjusted_macro_SPSR.getBegin() == adjusted_macro_SPSR.getEnd()))) {
						//macro_name = Rewrite.getRewrittenText(adjusted_macro_SPSR);
						/* The range could be an expression ending with a macro, in which case the macro name of interest would
						be the last token. */
						macro_name = Rewrite.getRewrittenText( { adjusted_macro_SPSR.getEnd(), adjusted_macro_SPSR.getEnd() } );
						if (")" == macro_name) {
							macro_name = Rewrite.getRewrittenText( { adjusted_macro_SPSR.getBegin(), adjusted_macro_SPSR.getEnd() } );
						}
						auto l_paren_index = macro_name.find("(");
						if (std::string::npos != l_paren_index) {
							macro_name = macro_name.substr(0, l_paren_index);
							rtrim(macro_name);
						}
					}
					DEBUG_SOURCE_LOCATION_STR(adjusted_macro_SPSR1_debug_source_location_str, adjusted_macro_SPSR, Rewrite);

					std::vector<std::string> macro_args;

					auto found_macro_iter = state1.m_pp_macro_definitions.find(macro_name);

					if (true && ((state1.m_pp_macro_definitions.end() == found_macro_iter))) {
						/* The contents of the given range does not seem to be a the name of a recognized macro. So we're
						going to check if the given range is a "MacroArgExpansion", which seems to require special handling. */

						auto SL_macro_arg_expansion_start = macro_SR.getBegin();
						bool SL_isMacroArgExpansion_flag = SM.isMacroArgExpansion(macro_SR.getBegin(), &SL_macro_arg_expansion_start);
						auto SLE_macro_arg_expansion_start = macro_SR.getEnd();
						bool SLE_isMacroArgExpansion_flag = SM.isMacroArgExpansion(macro_SR.getEnd(), &SLE_macro_arg_expansion_start);

						/* (Currently) we only can only handle cases where the given range covers the entire macro argument, 
						so we're going to verify that the adjacent positions are not (an extended) part of the macro argument. */
						auto macro_SPSL = SM.getSpellingLoc(macro_SR.getBegin());
						auto macro_SPSLE = SM.getSpellingLoc(macro_SR.getEnd());
						const std::string text9 = Rewrite.getRewrittenText({ macro_SPSL, macro_SPSLE });
						auto macro_SPSLE2 = macro_SPSL.getLocWithOffset(+text9.length() - 1);
						if (macro_SPSLE < macro_SPSLE2) {
							macro_SPSLE = macro_SPSLE2;
						}
						auto macro_SPSL_bumper = get_previous_non_whitespace_SL(macro_SPSL, Rewrite);
						const std::string text7 = Rewrite.getRewrittenText({ macro_SPSL_bumper, macro_SPSL_bumper });
						bool macro_SPSL_bumper_is_open_paren = ("(" == text7);
						auto macro_SPSLE_bumper = get_next_non_whitespace_SL(macro_SPSLE, Rewrite);
						const std::string text8 = Rewrite.getRewrittenText({ macro_SPSLE_bumper, macro_SPSLE_bumper });
						bool macro_SPSLE_bumper_is_close_paren = (")" == text8);

						if (true && SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag && (SL_macro_arg_expansion_start == SLE_macro_arg_expansion_start)
							&& (macro_SPSL_bumper_is_open_paren) && (macro_SPSLE_bumper_is_close_paren)) {

							/* The given range seems to cover a (whole) macro argument. */

							const auto expansion_SR = SM.getExpansionRange(macro_SR.getBegin()).getAsRange();
							if (expansion_SR.isValid() && ((expansion_SR.getBegin() < expansion_SR.getEnd()) || (expansion_SR.getBegin() == expansion_SR.getEnd()))) {
								DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);
								std::string macro_name2 = Rewrite.getRewrittenText( { expansion_SR.getBegin(), expansion_SR.getBegin() } );
								found_macro_iter = state1.m_pp_macro_definitions.find(macro_name2);
								if ((state1.m_pp_macro_definitions.end() != found_macro_iter) && (!filtered_out_by_location(SM, macro_SR.getBegin()))) {
									/* Ok, we seem to have obtained the associated recognized macro. */

									macro_name = macro_name2;

									/* This is presumably a function macro invocation, but our default handling would return a range 
									that excludes the function arguments. So to conform, we'll try to extract the part of the invocation 
									that excludes the arguments. */

									auto am_SPSL = adjusted_macro_SPSR.getBegin();
									auto am_SPSLE = adjusted_macro_SPSR.getEnd();
									auto exp_SL = expansion_SR.getBegin();
									auto exp_SLE = expansion_SR.getEnd();
									if ((exp_SL <= am_SPSL) && (am_SPSL <= exp_SLE) && (exp_SL <= am_SPSLE) && (am_SPSLE <= exp_SLE)) {
										auto last_non_whitespace_SL = expansion_SR.getBegin();
										if (last_non_whitespace_SL.isValid()) {
											auto current_SL = last_non_whitespace_SL.getLocWithOffset(+1);
											const auto expansion_SLE = expansion_SR.getEnd();
											bool open_paren_found = false;
											while (current_SL.isValid() && (current_SL <= expansion_SLE)) {
												const std::string text3 = Rewrite.getRewrittenText({ current_SL, current_SL });
												if ("(" == text3) {
													open_paren_found = true;
													break;
												}
												if ((1 == text3.length()) && !std::isspace(text3.front())) {
													last_non_whitespace_SL = current_SL;
												}
												current_SL = current_SL.getLocWithOffset(+1);
											}
											if (open_paren_found) {
												adjusted_macro_SPSR.setBegin(expansion_SR.getBegin());
												adjusted_macro_SPSR.setEnd(last_non_whitespace_SL);
											}
										}
									}
								}
							}
						}
					}
					if ((state1.m_pp_macro_definitions.end() != found_macro_iter)) {
						if (found_macro_iter->second.m_is_function_macro) {
							/* If the macro is a function macro, then adjusted_macro_SPSR would not
							currently include the arguments. Here we attempt to extend
							adjusted_macro_SPSR to include any arguments. */
							const auto expansion_SR = SM.getExpansionRange(adjusted_macro_SPSR.getEnd()).getAsRange();
							DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);
							if ((!(adjusted_macro_SPSR.getBegin() < expansion_SR.getBegin())) && (!(expansion_SR.getEnd() < adjusted_macro_SPSR.getEnd()))
								&& (!(expansion_SR == adjusted_macro_SPSR))) {
								adjusted_macro_SPSR = expansion_SR;
							} else {
								auto SL1 = adjusted_macro_SPSR.getEnd();
								std::string text2 = Rewrite.getRewrittenText({ SL1, SL1 });
								SL1 = SL1.getLocWithOffset(+text2.length());
								std::string text1;
								if (SL1.isValid()) {
									text1 = Rewrite.getRewrittenText({ SL1, SL1 });
									while ("" == text1) {
										SL1 = SL1.getLocWithOffset(+1);
										if (!(SL1.isValid())) {
											break;
										}
										text1 = Rewrite.getRewrittenText({ SL1, SL1 });
									}
								}
								DEBUG_SOURCE_LOCATION_STR(SL1_debug_source_location_str, clang::SourceRange(SL1, SL1), Rewrite);
								if ("(" == text1) {
									{
										auto maybe_close_paren_SL = matching_close_parentheses_if_any(Rewrite, SL1);
										if (maybe_close_paren_SL.has_value()) {
											adjusted_macro_SPSR = clang::SourceRange{ adjusted_macro_SPSR.getBegin(), maybe_close_paren_SL.value() };
											DEBUG_SOURCE_LOCATION_STR(adjusted_macro_SPSR2_debug_source_location_str, adjusted_macro_SPSR, Rewrite);

											auto args_SR = clang::SourceRange{ SL1.getLocWithOffset(+1), maybe_close_paren_SL.value().getLocWithOffset(-1) };
											std::string args_text1 = Rewrite.getRewrittenText(args_SR);
											std::string remaining_args_text1 = args_text1;
											auto found_comma_range = Parse::find_token_at_same_nesting_depth1(",", remaining_args_text1);
											while (found_comma_range.end < remaining_args_text1.length()) {
												auto sv1 = Parse::substring_view(remaining_args_text1, Parse::range_t{ 0, found_comma_range.begin });
												auto arg_str = std::string(sv1);
												ltrim(arg_str);
												rtrim(arg_str);
												macro_args.push_back(arg_str);
												remaining_args_text1 = remaining_args_text1.substr(found_comma_range.end);
												found_comma_range = Parse::find_token_at_same_nesting_depth1(",", remaining_args_text1);
											}
											if ("" != remaining_args_text1) {
												auto arg_str = std::string(remaining_args_text1);
												ltrim(arg_str);
												rtrim(arg_str);
												macro_args.push_back(arg_str);
											}
											int q = 5;
										} else {
											int q = 3;
										}
									}
								}
							}
						}
						DEBUG_SOURCE_TEXT_STR(debug_adjusted_macro_source_text2, adjusted_macro_SPSR, Rewrite);
					}
					return std::tuple{ adjusted_macro_SPSR, macro_name, macro_args };
				};

			auto sr2_sl = SL;
			auto sr2_sle = SLE;
			CSourceRangePlus sr2 = sr;
			if (true || ("" == SPSR_source_text)) {
				/* The fact that we're not getting any source text from the "spelling" range may be due to the 
				Begin and End points of the range being at different levels of macro nesting. So we'll attempt 
				to get a corresponding range with the Begin and End points at the same macro nesting level. */
				sr2 = source_range_with_both_ends_in_the_same_macro_body(sr, Rewrite);
			}
			if (sr2.isValid() && (!(sr2 == sr))) {
				retval = sr2;
				sr2_sl = sr2.getBegin();
				sr2_sle = sr2.getEnd();

				auto SP2SL = SM.getSpellingLoc(sr2.getBegin());
				auto SP2SLE = SM.getSpellingLoc(sr2.getEnd());
				auto b17 = SP2SL.isMacroID();
				auto b18 = SP2SLE.isMacroID();
				auto b18b = SP2SL.isFileID();
				auto b18c = SP2SLE.isFileID();
				auto SP2SR = clang::SourceRange{ SP2SL, SP2SLE };

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, sr2, Rewrite);

				std::string SP2SR_source_text;
				if ((SP2SR).isValid() && (((SP2SR).getBegin() < (SP2SR).getEnd()) || ((SP2SR).getBegin() == (SP2SR).getEnd()))) {
					auto [adjusted_macro_SPSR, macro_name, macro_args] = macro_spelling_range_extended_to_include_any_arguments(sr2);
					SP2SR_source_text = Rewrite.getRewrittenText(adjusted_macro_SPSR);
					if ("" != SP2SR_source_text) {
						SPSR = adjusted_macro_SPSR;
						SPSR_source_text = SP2SR_source_text;
						retval.m_adjusted_source_text_as_if_expanded = SPSR_source_text;
						retval.m_adjusted_source_text_infos.resize(1);
						retval.m_adjusted_source_text_infos.at(0).m_text = SPSR_source_text;
					} else {
						std::string FLSR_source_text;
						if ((FLSR).isValid() && (((FLSR).getBegin() < (FLSR).getEnd()) || ((FLSR).getBegin() == (FLSR).getEnd()))) {
							FLSR_source_text = Rewrite.getRewrittenText(FLSR);
							if ("" != FLSR_source_text) {
								SPSR = FLSR;
								SPSR_source_text = FLSR_source_text;
								retval.m_adjusted_source_text_as_if_expanded = SPSR_source_text;
								retval.m_adjusted_source_text_infos.resize(1);
								retval.m_adjusted_source_text_infos.at(0).m_text = SPSR_source_text;
							}
						}
					}
				}
			}

			if (may_be_a_gnu_attr) {
				auto maybe_SR2 = extended_to_include_entire_gnu_attribute_if_any(SPSR, state1, Rewrite);
				if (maybe_SR2.has_value()) {
					auto SPSR2 = maybe_SR2.value();
					auto text2 = Rewrite.getRewrittenText(SPSR2);
					if ("" != text2) {
						SPSR = SPSR2;
						SPSR_source_text = text2;
						retval.m_adjusted_source_text_as_if_expanded = SPSR_source_text;
					} else {
						int q = 3;
					}
				}
			}

			{
				/* The element could be nested within multiple macro instances. We are going to obtain
				and store a list of (the source ranges of) all the (nested) macro instances which contain
				the element. */
				auto nested_macro_ranges = std::vector<clang::SourceRange>{};

				auto last_macro1_SL = sr2_sl;
				auto last_macro1_SLE = sr2_sle;
				DEBUG_SOURCE_TEXT_STR(last_macro1_source_text, (clang::SourceRange{ last_macro1_SL, last_macro1_SLE }), Rewrite);
				DEBUG_SOURCE_TEXT_STR(last_macro1_sp_source_text, (clang::SourceRange{ SM.getSpellingLoc(last_macro1_SL), SM.getSpellingLoc(last_macro1_SLE) }), Rewrite);

				auto macro1_SL = SM.getImmediateMacroCallerLoc(sr2_sl);
				auto macro1_SLE = SM.getImmediateMacroCallerLoc(sr2_sle);
				DEBUG_SOURCE_TEXT_STR(macro1_source_text, (clang::SourceRange{ macro1_SL, macro1_SLE }), Rewrite);
				DEBUG_SOURCE_TEXT_STR(macro1_sp_source_text, (clang::SourceRange{ SM.getSpellingLoc(macro1_SL), SM.getSpellingLoc(macro1_SLE) }), Rewrite);

				const auto expansion_SR = SM.getExpansionRange(macro1_SL).getAsRange();
				DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);

				while (macro1_SL != last_macro1_SL) {
					nested_macro_ranges.push_back({ macro1_SL, macro1_SLE });

					last_macro1_SL = macro1_SL;
					last_macro1_SLE = macro1_SLE;
					macro1_SL = SM.getImmediateMacroCallerLoc(macro1_SL);
					macro1_SLE = SM.getImmediateMacroCallerLoc(macro1_SLE);

					auto new_macro1_SR = clang::SourceRange{ macro1_SL, macro1_SLE };
					DEBUG_SOURCE_TEXT_STR(debug_macro1_source_text, new_macro1_SR, Rewrite);
					auto adjusted_macro_SPSR = clang::SourceRange{ SM.getSpellingLoc(new_macro1_SR.getBegin()), SM.getSpellingLoc(new_macro1_SR.getEnd()) };
					DEBUG_SOURCE_TEXT_STR(debug_adjusted_macro_source_text, adjusted_macro_SPSR, Rewrite);

					const auto expansion_SR = SM.getExpansionRange(macro1_SL).getAsRange();
					DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);

					auto b16 = SM.isMacroArgExpansion(macro1_SL);
					auto b17 = SM.isMacroArgExpansion(adjusted_macro_SPSR.getBegin());
					auto b18 = SM.isMacroBodyExpansion(macro1_SL);
					auto b19 = SM.isMacroBodyExpansion(adjusted_macro_SPSR.getBegin());
					int q = 5;
				}

				std::string adjusted_source_text_as_if_expanded = SPSR_source_text;

				if (1 <= nested_macro_ranges.size()) {
					auto l_SPSR_source_text = SPSR_source_text;

					{
						/* We're going to check if the first nested macro range is a "MacroArgExpansion", which seems to 
						require special handling. */

						auto first_macro_SL = nested_macro_ranges.front().getBegin();
						auto first_macro_SLE = nested_macro_ranges.front().getEnd();
						auto SL_macro_arg_expansion_start = first_macro_SL;
						bool SL_isMacroArgExpansion_flag = SM.isMacroArgExpansion(first_macro_SL, &SL_macro_arg_expansion_start);
						auto SLE_macro_arg_expansion_start = first_macro_SLE;
						bool SLE_isMacroArgExpansion_flag = SM.isMacroArgExpansion(first_macro_SLE, &SLE_macro_arg_expansion_start);

						/* (Currently) we only can only handle cases where the given range covers the entire macro argument, 
						so we're going to verify that the adjacent positions are not (an extended) part of the macro argument. */
						auto first_macro_SPSL = SM.getSpellingLoc(first_macro_SL);
						auto first_macro_SPSLE = SM.getSpellingLoc(first_macro_SLE);
						const std::string text9 = Rewrite.getRewrittenText({ first_macro_SPSL, first_macro_SPSLE });
						auto first_macro_SPSLE2 = first_macro_SPSL.getLocWithOffset(+text9.length() - 1);
						if (first_macro_SPSLE < first_macro_SPSLE2) {
							first_macro_SPSLE = first_macro_SPSLE2;
						}
						auto first_macro_SPSL_bumper = get_previous_non_whitespace_SL(first_macro_SPSL, Rewrite);
						const std::string text7 = Rewrite.getRewrittenText({ first_macro_SPSL_bumper, first_macro_SPSL_bumper });
						bool first_macro_SPSL_bumper_is_open_paren = ("(" == text7);
						auto first_macro_SPSLE_bumper = get_next_non_whitespace_SL(first_macro_SPSLE, Rewrite);
						const std::string text8 = Rewrite.getRewrittenText({ first_macro_SPSLE_bumper, first_macro_SPSLE_bumper });
						bool first_macro_SPSLE_bumper_is_close_paren = (")" == text8);

						if (true && SL_isMacroArgExpansion_flag && SLE_isMacroArgExpansion_flag && (SL_macro_arg_expansion_start == SLE_macro_arg_expansion_start)
							&& (first_macro_SPSL_bumper_is_open_paren) && (first_macro_SPSLE_bumper_is_close_paren)) {

							/* The given range seems to cover a (whole) macro argument. */

							auto macro_arg_expansion_SPSL = SM.getSpellingLoc(SL_macro_arg_expansion_start);
							auto macro_arg_expansion_SPSLE = SM.getSpellingLoc(SLE_macro_arg_expansion_start);
							auto macro_arg_expansion_SR = clang::SourceRange{ macro_arg_expansion_SPSL, macro_arg_expansion_SPSLE };
							std::string macro_arg_expansion_text1 = Rewrite.getRewrittenText(macro_arg_expansion_SR);
							if (("" != macro_arg_expansion_text1) && (!filtered_out_by_location(SM, first_macro_SL))) {
								adjusted_source_text_as_if_expanded = macro_arg_expansion_text1;
								l_SPSR_source_text = macro_arg_expansion_text1;
							}
						}
					}

					retval.set_source_range(SPSR);
					retval.m_adjusted_source_text_as_if_expanded = adjusted_source_text_as_if_expanded;
					if (1 + nested_macro_ranges.size() > retval.m_adjusted_source_text_infos.size()) {
						retval.m_adjusted_source_text_infos.resize(1 + nested_macro_ranges.size());
					}
					retval.m_adjusted_source_text_infos.at(0).m_text = l_SPSR_source_text;
				} else {
					int q = 3;
				}

				int nesting_level = 0;

				/* Here we're going to find (well, estimate/guess) the "outermost" macro that
				contains the element, and consists of only an expression. (As opposed to, for
				example, a declaration, or more than one statement.) */
				for (const auto& macro2_SR : nested_macro_ranges) {
					if (!filtered_out_by_location(SM, macro2_SR.getBegin())) {
						auto [adjusted_macro_SPSR, macro_name, macro_args] = macro_spelling_range_extended_to_include_any_arguments(macro2_SR);
						DEBUG_SOURCE_LOCATION_STR(adjusted_macro_SPSR1_debug_source_location_str, adjusted_macro_SPSR, Rewrite);

						auto found_macro_iter = state1.m_pp_macro_definitions.find(macro_name);
						if ((state1.m_pp_macro_definitions.end() != found_macro_iter)) {
							DEBUG_SOURCE_TEXT_STR(debug_adjusted_macro_source_text2, adjusted_macro_SPSR, Rewrite);

							std::string macro_def_text = Rewrite.getRewrittenText(found_macro_iter->second.definition_SR());

							/* trim leading and trailing whitespace */
							std::string trimmed_macro_def_body_str = found_macro_iter->second.m_macro_def_body_str;
							lrtrim(trimmed_macro_def_body_str);
							std::string trimmed_source_text_as_if_expanded = adjusted_source_text_as_if_expanded;
							lrtrim(trimmed_source_text_as_if_expanded);

							if (string_begins_with(trimmed_macro_def_body_str, "\\") && !string_begins_with(trimmed_source_text_as_if_expanded, "\\")) {
								/* We've encountered this. */
								trimmed_macro_def_body_str = trimmed_macro_def_body_str.substr(1);
								lrtrim(trimmed_macro_def_body_str);
							}

							bool is_essentially_the_whole_macro = false;
							if (trimmed_macro_def_body_str == trimmed_source_text_as_if_expanded) {
								is_essentially_the_whole_macro = true;
							} else if (trimmed_macro_def_body_str == ("(" + adjusted_source_text_as_if_expanded + ")")) {
								is_essentially_the_whole_macro = true;
							}

							bool return_the_macro_invocation_rather_than_the_definition = false;
							if (is_essentially_the_whole_macro) {
								/* It looks like the macro expansion is essentially just the given source 
								range. So we're going to presume that it's likely that any given 
								transformation could be appropriately applied to the invocation of the 
								macro, rather than needing to be applied to the definition of the macro. We 
								would prefer not to modify the definition of a macro if it's not necessary. */
								retval.m_range_is_essentially_the_entire_body_of_a_macro = true;

								if (filtered_out_by_location<options_t<converter_mode_t> >(SM, found_macro_iter->second.definition_SR().getBegin())) {
									/* Unless the macro definition is not elegible for conversion. In this case we probably don't 
									want to return the invocation range that might be elegible for conversion. The macro might be 
									a system or installed 3rd party library macro whose definition might be platform dependent. */
#ifndef NDEBUG
									if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
										int q = 5;
									}
#endif /*!NDEBUG*/
								} else {
									return_the_macro_invocation_rather_than_the_definition = true;
									retval.m_macro_expansion_range_substituted_with_macro_invocation_range = true;
									retval.m_adjusted_source_text_infos.at(nesting_level).m_can_be_substituted_with_macro_invocation_text = true;
								}
							}
							if (return_the_macro_invocation_rather_than_the_definition) {
								retval.set_source_range(adjusted_macro_SPSR);
								adjusted_source_text_as_if_expanded = Rewrite.getRewrittenText(adjusted_macro_SPSR);
								retval.m_adjusted_source_text_as_if_expanded = adjusted_source_text_as_if_expanded;
							} else {
								auto& macro_params = found_macro_iter->second.m_parameter_names;
								if (macro_params.size() <= macro_args.size()) {
									if (macro_params.size() != macro_args.size()) {
										int q = 5;
									}
									auto arg_citer = macro_args.cbegin();
									for (auto& macro_param : macro_params) {
										auto found_range = Parse::find_uncommented_token(macro_param, adjusted_source_text_as_if_expanded);
										while (found_range.begin < adjusted_source_text_as_if_expanded.length()) {
											adjusted_source_text_as_if_expanded.replace(found_range.begin, (found_range.end - found_range.begin), *arg_citer);
											retval.m_adjusted_source_text_as_if_expanded = adjusted_source_text_as_if_expanded;

											found_range = Parse::find_uncommented_token(macro_param, adjusted_source_text_as_if_expanded, found_range.begin + (*arg_citer).length());
										}
										++arg_citer;
									}
								} else {
									int q = 3;
								}
								//break;
							}
							/* Since we're incidentally constructing representations of the source range with their use of macro 
							parameters adjusted so that the representation is valid in the definition bodies of the macros at 
							the various levels of of (macro invocation) nesting, we'll just store those results in case they're 
							needed at some point. */
							auto& adjusted_source_text_info_ref = retval.m_adjusted_source_text_infos.at(1 + nesting_level);
							adjusted_source_text_info_ref.m_text = retval.m_adjusted_source_text_as_if_expanded;
							adjusted_source_text_info_ref.m_macro_args = macro_args;
							adjusted_source_text_info_ref.m_macro_invocation_range = adjusted_macro_SPSR;
							adjusted_source_text_info_ref.m_macro_name = macro_name;

							retval.m_adjusted_source_text_infos.at(nesting_level).m_macro_definition_range = found_macro_iter->second.definition_SR();

							++nesting_level;
						} else {
							int q = 5;
						}
					}
				}
				retval.m_adjusted_source_text_infos.resize(1 + nesting_level);
				DEBUG_SOURCE_TEXT_STR(debug_macro_source_text, retval, Rewrite);

				return retval;
			}
		} else if (may_be_a_gnu_attr) {
			auto maybe_SR2 = extended_to_include_entire_gnu_attribute_if_any(retval, state1, Rewrite);
			if (maybe_SR2.has_value()) {
				retval = maybe_SR2.value();
			}
		}
		return retval;
	}

	static std::string get_source_as_if_preprocessor_expanded(const clang::SourceRange& sr, CTUState& state1, clang::Rewriter &Rewrite) {
		std::string retval;

		auto adjusted_SR = sr;

		auto rawSR = sr;
		auto SL = rawSR.getBegin();
		auto SLE = rawSR.getEnd();
		auto b3 = SL.isMacroID();
		auto b4 = SLE.isMacroID();

		auto& SM = Rewrite.getSourceMgr();
		auto FLSL = SM.getFileLoc(SL);
		auto FLSLE = SM.getFileLoc(SLE);
		auto b5 = FLSL.isMacroID();
		auto b6 = FLSLE.isMacroID();
		auto b6b = FLSL.isFileID();
		auto FLSR = clang::SourceRange{ FLSL, FLSLE };

		auto SPSL = SM.getSpellingLoc(SL);
		auto SPSLE = SM.getSpellingLoc(SLE);
		auto b7 = SPSL.isMacroID();
		auto b8 = SPSLE.isMacroID();
		auto b8b = SPSL.isFileID();
		auto b8c = SPSLE.isFileID();
		auto SPSR = clang::SourceRange{ SPSL, SPSLE };

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, sr, Rewrite);

		std::string SPSR_source_text;
		if ((SPSR).isValid() && (((SPSR).getBegin() < (SPSR).getEnd()) || ((SPSR).getBegin() == (SPSR).getEnd()))) {
			SPSR_source_text = Rewrite.getRewrittenText(SPSR);
			if ("" != SPSR_source_text) {
				retval = SPSR_source_text;
			}
		}

		DEBUG_SOURCE_TEXT_STR(debug_source_text, sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_fl_source_text, FLSR.isValid() ? FLSR : sr, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_sp_source_text, SPSR.isValid() ? SPSR : sr, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

		if (b3) {
			/* The element is part of a macro instance. */
			auto& SM = Rewrite.getSourceMgr();

			auto b10 = SM.isMacroArgExpansion(SL);
			auto b10b = SM.isMacroArgExpansion(SLE);
			auto b11 = SM.isMacroArgExpansion(FLSL);
			auto b12 = SM.isMacroArgExpansion(SPSL);
			auto b13 = SM.isMacroBodyExpansion(SL);
			auto b14 = SM.isMacroBodyExpansion(FLSL);
			auto b15 = SM.isMacroBodyExpansion(SPSL);
			if (b10) {
				if (!b10b) {
					/* It seems that beginning of the source range is a macro function argument, but the end isn't. 
					(Presumably the end is at another part of the macro body.) Rewrite.getRewrittenText() doesn't 
					seem to work on such mismatched source ranges. After some experimentation, it's still unclear 
					how or if one can obtain the beginning corresponding source location in the macro body. */
					int q = 5;
				} else {
					int q = 5;
				}
			}

			std::string SPSR_source_text_with_added_parens = "(" + SPSR_source_text + ")";

			{
				/* The element could be nested within multiple macro instances. We are going to obtain
				and store a list of (the source ranges of) all the (nested) macro instances which contain
				the element. */
				auto nested_macro_ranges = std::vector<clang::SourceRange>{};
				auto last_macro1_SL = SL;
				auto last_macro1_SLE = SLE;
				auto macro1_SL = SM.getImmediateMacroCallerLoc(SL);
				auto macro1_SLE = SM.getImmediateMacroCallerLoc(SLE);

				const auto expansion_SR = SM.getExpansionRange(macro1_SL).getAsRange();
				DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);

				while (macro1_SL != last_macro1_SL) {
					nested_macro_ranges.push_back({ macro1_SL, macro1_SLE });

					last_macro1_SL = macro1_SL;
					last_macro1_SLE = macro1_SLE;
					macro1_SL = SM.getImmediateMacroCallerLoc(macro1_SL);
					macro1_SLE = SM.getImmediateMacroCallerLoc(macro1_SLE);

					auto new_macro1_SR = clang::SourceRange{ macro1_SL, macro1_SLE };
					DEBUG_SOURCE_TEXT_STR(debug_macro1_source_text, new_macro1_SR, Rewrite);
					auto adjusted_macro_SPSR = clang::SourceRange{ SM.getSpellingLoc(new_macro1_SR.getBegin()), SM.getSpellingLoc(new_macro1_SR.getEnd()) };
					DEBUG_SOURCE_TEXT_STR(debug_adjusted_macro_source_text, adjusted_macro_SPSR, Rewrite);

					const auto expansion_SR = SM.getExpansionRange(macro1_SL).getAsRange();
					DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);

					auto b16 = SM.isMacroArgExpansion(macro1_SL);
					auto b17 = SM.isMacroArgExpansion(adjusted_macro_SPSR.getBegin());
					auto b18 = SM.isMacroBodyExpansion(macro1_SL);
					auto b19 = SM.isMacroBodyExpansion(adjusted_macro_SPSR.getBegin());
					int q = 5;
				}

				if (1 <= nested_macro_ranges.size()) {
					adjusted_SR = SPSR;
				} else {
					int q = 3;
				}

				int nesting_level = 0;

				/* Here we're going to find (well, estimate/guess) the "outermost" macro that
				contains the element, and consists of only an expression. (As opposed to, for
				example, a declaration, or more than one statement.) */
				for (const auto& macro2_SR : nested_macro_ranges) {
					{
						auto adjusted_macro_SPSR = clang::SourceRange{ SM.getSpellingLoc(macro2_SR.getBegin()), SM.getSpellingLoc(macro2_SR.getEnd()) };
						std::string macro_name;
						if (adjusted_macro_SPSR.isValid() && ((adjusted_macro_SPSR.getBegin() < adjusted_macro_SPSR.getEnd()) || (adjusted_macro_SPSR.getBegin() == adjusted_macro_SPSR.getEnd()))) {
							macro_name = Rewrite.getRewrittenText(adjusted_macro_SPSR);
							auto l_paren_index = macro_name.find("(");
							if (std::string::npos != l_paren_index) {
								macro_name = macro_name.substr(0, l_paren_index);
								rtrim(macro_name);
							}
						}

						std::vector<std::string> macro_args;

						/* If the macro is a function macro, then adjusted_macro_SPSR would not
						currently include the arguments. Here we attempt to extend
						adjusted_macro_SPSR to include any arguments. */
						const auto expansion_SR = SM.getExpansionRange(adjusted_macro_SPSR.getBegin()).getAsRange();
						DEBUG_SOURCE_TEXT_STR(debug_expansion_source_text, expansion_SR, Rewrite);
						if ((!(adjusted_macro_SPSR.getBegin() < expansion_SR.getBegin())) && (!(expansion_SR.getEnd() < adjusted_macro_SPSR.getEnd()))
							&& (!(expansion_SR == adjusted_macro_SPSR))) {
							adjusted_macro_SPSR = expansion_SR;
						} else {
							auto SL1 = adjusted_macro_SPSR.getBegin();
							std::string text1 = Rewrite.getRewrittenText({ SL1, SL1 });
							SL1 = SL1.getLocWithOffset(+text1.length());
							text1 = Rewrite.getRewrittenText({ SL1, SL1 });
							while ("" == text1) {
								SL1 = SL1.getLocWithOffset(+1);
								if (!(SL1.isValid())) {
									break;
								}
								text1 = Rewrite.getRewrittenText({ SL1, SL1 });
							}
							if ("(" == text1) {
								auto maybe_close_paren_SL = matching_close_parentheses_if_any(Rewrite, SL1);
								if (maybe_close_paren_SL.has_value()) {
									adjusted_macro_SPSR = clang::SourceRange{ adjusted_macro_SPSR.getBegin(), maybe_close_paren_SL.value() };

									auto args_SR = clang::SourceRange{ SL1.getLocWithOffset(+1), maybe_close_paren_SL.value().getLocWithOffset(-1) };
									std::string args_text1 = Rewrite.getRewrittenText(args_SR);
									std::string remaining_args_text1 = args_text1;
									auto found_comma_range = Parse::find_token_at_same_nesting_depth1(",", remaining_args_text1);
									while (found_comma_range.end < remaining_args_text1.length()) {
										auto sv1 = Parse::substring_view(remaining_args_text1, Parse::range_t{ 0, found_comma_range.end });
										macro_args.push_back(std::string(sv1));
										remaining_args_text1 = remaining_args_text1.substr(found_comma_range.end);
										found_comma_range = Parse::find_token_at_same_nesting_depth1(",", remaining_args_text1);
									}
								} else {
									int q = 3;
								}
							}
						}
						DEBUG_SOURCE_TEXT_STR(debug_adjusted_macro_source_text2, adjusted_macro_SPSR, Rewrite);

						if (true) {
							auto found_macro_iter = state1.m_pp_macro_definitions.find(macro_name);
							if (state1.m_pp_macro_definitions.end() != found_macro_iter) {
								std::string macro_def_text = Rewrite.getRewrittenText(found_macro_iter->second.definition_SR());

								bool is_essentially_the_whole_macro = false;
								auto found_iter2 = macro_def_text.find_first_of(";=");
								if (std::string::npos == found_iter2) {
									if (found_macro_iter->second.m_macro_def_body_str == SPSR_source_text) {
										is_essentially_the_whole_macro = true;
									} else if (found_macro_iter->second.m_macro_def_body_str == SPSR_source_text_with_added_parens) {
										is_essentially_the_whole_macro = true;
									}
								}

								if (is_essentially_the_whole_macro) {
									/* It looks like the macro expansion is essentially just the given source 
									range. So we're going to presume that it's likely that any given 
									transformation could be appropriately applied to the instantiation of the 
									macro, rather than needing to be applied to the definition of the macro. We 
									would prefer not to modify the definition of a macro if it's not necessary. */
									adjusted_SR = adjusted_macro_SPSR;
								} else {
									/* The macro seems like it might consist of additional expressions and/or statements other than the
									outside of the source range we were given. */
									break;
								}
							} else {
								int q = 5;
							}
						}
					}

					++nesting_level;
				}
				DEBUG_SOURCE_TEXT_STR(debug_macro_source_text, adjusted_SR, Rewrite);
				//return adjusted_SR;
			}
		}
		//return adjusted_SR;

		return retval;
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
			auto SL4 = SR3.getBegin().getLocWithOffset(-1);
			clang::SourceRange SR4 { SL4, SL4 };
			auto text4 = Rewrite.getRewrittenText(SR4);
			bool text4_is_alphanum_or_underscore = false;
			if (1 <= text4.length()) {
				auto ch = text4.back();
				text4_is_alphanum_or_underscore = Parse::is_alnum_or_underscore(ch);
			}
			if (!text4_is_alphanum_or_underscore) {
				/* (west) const qualifier found */
				/* extend the source range(s) to include the const qualifier */
				cq_SR.setBegin(SR3.getBegin());
				IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(cq_SR);)
			}
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
			auto SL4 = SR3.getEnd().getLocWithOffset(+1);
			clang::SourceRange SR4 { SL4, SL4 };
			auto text4 = Rewrite.getRewrittenText(SR4);
			bool text4_is_alphanum_or_underscore = false;
			if (1 <= text4.length()) {
				auto ch = text4.front();
				text4_is_alphanum_or_underscore = Parse::is_alnum_or_underscore(ch);
			}
			if (!text4_is_alphanum_or_underscore) {
				/* east const qualifier found */
				/* extend the source range(s) to include the const qualifier */
				cq_SR.setEnd(SR3.getEnd());
				IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(cq_SR);)
			}
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

	struct CTParamUsageInfo {
		std::string first_used_param_as_string() const {
			if (!TPL) { assert(false); return ""; }
			return (*TPL).getParam(zbindex_of_first_used_param)->getNameAsString();
		}
		size_t zbindex_of_first_used_param = 0;
		clang::TemplateParameterList const * TPL = nullptr;
		clang::TemplateArgumentList  const * TAL = nullptr;
	};

	std::optional<CTParamUsageInfo> seems_to_contain_an_instantiation_of_a_template_parameter(const clang::Decl& containing_decl, std::string_view source_text_sv, Rewriter &Rewrite, CTUState* state1_ptr = nullptr) {
		/* The current implementation is just kind of a placeholder hack where we're just 
		looking at the (current) source text of the declaration and seeing if it contains
		any instances of the name of any of the template parameters (if any). */
		auto containing_D = &containing_decl;

		bool is_template_instantiation = false;
		clang::TemplateParameterList const * TPL = nullptr;
		clang::TemplateArgumentList  const * TAL = nullptr;
		clang::ASTTemplateArgumentListInfo const * ast_targ_list_info_ptr = nullptr;
		const clang::Decl* template_D = nullptr;
		/* first check if the containing_D itself is a function template. */
		template_D = dyn_cast<const clang::FunctionTemplateDecl>(containing_D);
		if (!template_D) {
			if (state1_ptr && state1_ptr->m_ast_context_ptr) {
				template_D = Tget_containing_template_decl_if_any(containing_D, *(state1_ptr->m_ast_context_ptr));
			} else {
				auto DC = containing_D->getParentFunctionOrMethod();
				if (DC) {
					auto FTD = dyn_cast<const clang::FunctionTemplateDecl>(DC);
					if (FTD) {
						template_D = FTD;
					}
				}
			}
		}
		if (template_D) {
			auto FTD = dyn_cast<const clang::FunctionTemplateDecl>(template_D);
			if (FTD) {
				IF_DEBUG(std::string name = FTD->getNameAsString();)
				TPL = FTD->getTemplateParameters();

				auto DC = containing_D->getParentFunctionOrMethod();
				if (DC) {
					auto FD = dyn_cast<const clang::FunctionDecl>(DC);
					if (FD && FD->isTemplateInstantiation()) {
						TAL = FD->getTemplateSpecializationArgs();
						ast_targ_list_info_ptr = FD->getTemplateSpecializationArgsAsWritten();

#ifndef NDEBUG
						auto templated_kind = FD->getTemplatedKind();
						//auto* FTD2 = FD->getDescribedFunctionTemplate();
						auto b1 = FD->isFunctionTemplateSpecialization();
						auto* tsinfo = FD->getTemplateSpecializationInfo();
						auto b2 = FD->isImplicitlyInstantiable();
						auto b3 = FD->isTemplateInstantiation();
						auto* FTD3 = FD->getPrimaryTemplate();
						const clang::FunctionDecl* FD2 = nullptr;
						if (FTD3) {
							FD2 = FTD3->getTemplatedDecl();
						}
						auto* targ_list = FD->getTemplateSpecializationArgs();
						auto* ast_targ_list_info = FD->getTemplateSpecializationArgsAsWritten();
						auto ts_kind = FD->getTemplateSpecializationKind();
						auto ts_kind2 = FD->getTemplateSpecializationKindForInstantiation();
						int q = 5;
#endif /*!NDEBUG*/
					}
				}
				int q = 5;
			} else {
				auto CTD = dyn_cast<const clang::ClassTemplateDecl>(template_D);
				IF_DEBUG(std::string name = CTD->getNameAsString();)
				TPL = CTD->getTemplateParameters();

				if (state1_ptr) {
					auto CTSD = Tget_containing_element_of_type<clang::ClassTemplateSpecializationDecl>(containing_D, *(state1_ptr->m_ast_context_ptr));
					if (CTSD) {
						TAL = &(CTSD->getTemplateArgs());
						//ast_targ_list_info_ptr = CTSD->getTemplateArgsAsWritten();
					}
				}
			}
			if (TPL) {
				auto contains_as_token_any_template_param = [](std::string_view source_text, clang::TemplateParameterList const * TPL) -> std::optional<size_t> {
					std::optional<size_t> maybe_zbindex_of_contained_tparam;
					if (!TPL) { return maybe_zbindex_of_contained_tparam; }
					auto contains_as_token = [](std::string_view source_text, std::string_view token_name) {
						auto found_range = Parse::find_uncommented_token(token_name, source_text);
						return bool(source_text.length() > found_range.begin);
					};

					size_t zbindex = 0;
					for (auto& ND : (*TPL)) {
						if (contains_as_token(source_text, ND->getNameAsString())) {
							maybe_zbindex_of_contained_tparam = zbindex;
							break;
						}
						++zbindex;
					}
					return maybe_zbindex_of_contained_tparam;
				};
				auto maybe_zbindex_of_contained_tparam = contains_as_token_any_template_param(source_text_sv, TPL);
				if (maybe_zbindex_of_contained_tparam.has_value()) {
					return CTParamUsageInfo{ maybe_zbindex_of_contained_tparam.value(), TPL, TAL };
				}
			}
		}
		return {};
	}
	std::optional<CTParamUsageInfo> seems_to_contain_an_instantiation_of_a_template_parameter(const clang::Decl& containing_decl, clang::SourceRange SR, Rewriter &Rewrite, CTUState* state1_ptr = nullptr) {
		std::string source_text;
		if ((SR).isValid() && (((SR).getBegin() < (SR).getEnd()) || ((SR).getBegin() == (SR).getEnd()))) {
			source_text = (Rewrite).getRewrittenText(SR);
		}
		return seems_to_contain_an_instantiation_of_a_template_parameter(containing_decl, source_text, Rewrite, state1_ptr);
	}
	std::optional<CTParamUsageInfo> seems_to_contain_an_instantiation_of_a_template_parameter(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState* state1_ptr = nullptr) {
		auto DD = &ddecl;
		std::optional<clang::SourceRange> maybe_SR;
		auto maybe_typeLoc = typeLoc_if_available(ddecl);
		if (maybe_typeLoc.has_value()) {
			auto typeLoc = maybe_typeLoc.value();
			const auto l_SR = state1_ptr
				? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
				: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
			if ((l_SR).isValid() && (((l_SR).getBegin() < (l_SR).getEnd()) || ((l_SR).getBegin() == (l_SR).getEnd()))) {
				maybe_SR = l_SR;
			}
		}
		auto SR = maybe_SR.has_value() ? maybe_SR.value() : 
			(state1_ptr ? cm1_adj_nice_source_range(DD->getSourceRange(), *state1_ptr, Rewrite) : cm1_nice_source_range(DD->getSourceRange(), Rewrite));
		return seems_to_contain_an_instantiation_of_a_template_parameter(ddecl, SR, Rewrite, state1_ptr);
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
		bool m_some_addressable_indirection = false;
		bool m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally = false;
		std::string m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str;
		std::string m_complete_type_str;
	};

	class CDeclarationReplacementCodeItem {
	public:
		std::string m_replacement_code;
		std::string m_replacement_type_str;
		std::string m_replacement_return_type_str;
		std::string m_replacement_return_type_post_params_suffix_str;
		std::string m_action_species;
		bool m_changed_from_original = false;
		bool m_individual_from_compound_declaration = false;
	};
	inline CDeclarationReplacementCodeItem generate_declaration_replacement_code(const DeclaratorDecl* DD,
		Rewriter &Rewrite, CTUState* state1_ptr, CDDeclConversionStateMap& ddecl_conversion_state_map, std::string options_str = "");

	enum class EIsFunctionParam { No, Yes };
	enum class ESuppressModifications { No, Yes };

	inline std::string current_params_string(Rewriter &Rewrite, CFunctionTypeState& function_type_state_ref, clang::FunctionProtoTypeLoc functionProtoTypeLoc, bool suppress_modifications = false, CTUState *state1_ptr = nullptr, bool is_declaration = false) {
		std::string retval;
		auto FND = function_type_state_ref.m_function_decl_ptr;
		if (FND && state1_ptr) {
			retval = "(";
			bool is_first_param = true;
			for (const auto& param_VD : FND->parameters()) {
				if (!is_first_param) {
					retval += ", ";
				} else {
					is_first_param = false;
				}
				auto res4 = generate_declaration_replacement_code(param_VD, Rewrite, state1_ptr, state1_ptr->m_ddecl_conversion_state_map);
				retval += res4.m_replacement_type_str;
			}
			retval += ")";
		} else if ((!is_declaration) && function_type_state_ref.current_qtypes_are_current()) {
			retval = params_string_from_qtypes(function_type_state_ref.m_param_qtypes_current, Rewrite);
		} else if (!function_type_state_ref.m_params_current_str.empty()) {
			retval = function_type_state_ref.m_params_current_str;
		} else if (functionProtoTypeLoc) {
			auto parens_SR = write_once_source_range(state1_ptr
						? cm1_adj_nice_source_range(functionProtoTypeLoc.getParensRange(), *state1_ptr, Rewrite)
						: cm1_nice_source_range(functionProtoTypeLoc.getParensRange(), Rewrite));
			if (parens_SR.isValid()) {

#ifndef NDEBUG
				if (parens_SR.isValid()) {
					auto& SM = Rewrite.getSourceMgr();
					IF_DEBUG(std::string debug_source_location_str = parens_SR.getBegin().printToString(SM);)

					DEBUG_SOURCE_TEXT_STR(debug_source_text, parens_SR, Rewrite);

					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
				}
#endif /*!NDEBUG*/

				/* We're taking note of the (current) parameters source text. */
				std::string parens_text = Rewrite.getRewrittenText(parens_SR);
				retval = parens_text;

				if (ConvertToSCPP && (!suppress_modifications) && state1_ptr) {
					/* We've stored the function parameters as a string. Now we're going
					to "blank out"/erase the original source text of the parameters. */
					std::string blank_text = parens_text;
					for (auto& ch : blank_text) {
						ch = ' ';
					}
					state1_ptr->m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, parens_SR, blank_text);
					//Rewrite.ReplaceText(parens_SR, blank_text);

					/* setting pointee_params_current_str_ref to a non-empty string implies that the
					original source text of the parameters has (already) been "blanked out"/erased. */
					function_type_state_ref.set_params_current_str(parens_text);
				}
			} else {
				int q = 3;
			}
		} else {
			int q = 7;
		}
		return retval;
	}

	struct do_not_exclude_functions_with_conversions_t {};
	template<typename TOptions = options_t<> >
	bool is_non_modifiable(clang::Decl const& decl, clang::ASTContext& Ctx, clang::Rewriter &Rewrite, CTUState& state1, clang::Expr const* E = nullptr);

	/* If given a non-null state1_ptr argument, this function will modify the source text to reflect
	any currently indicated changes to the declaration. In any case, it will return an information
	object that can be used to construct a text string of the currently indicated replacement type. */
	static CTypeIndirectionPrefixAndSuffixItem type_indirection_prefix_and_suffix_modifier_and_code_generator(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, EIsFunctionParam is_a_function_parameter_enum, std::optional<clang::StorageDuration> maybe_storage_duration = {}
			, ESuppressModifications suppress_modifications_enum = ESuppressModifications::No, CTUState* state1_ptr = nullptr) {
		CTypeIndirectionPrefixAndSuffixItem retval;

		IF_DEBUG(std::string debug_source_location_str2;)
		IF_DEBUG(std::string debug_source_text2;)
#ifndef NDEBUG
		if (indirection_state_stack.m_maybe_DD.has_value()) {
			auto& SM = Rewrite.getSourceMgr();
			auto SR = state1_ptr
				? cm1_adj_nice_source_range(indirection_state_stack.m_maybe_DD.value()->getSourceRange(), *state1_ptr, Rewrite)
				: cm1_nice_source_range(indirection_state_stack.m_maybe_DD.value()->getSourceRange(), Rewrite);
			IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
			debug_source_location_str2 = debug_source_location_str;
			debug_source_text2 = debug_source_text;
		}
#endif /*!NDEBUG*/

		bool is_a_function_parameter = (EIsFunctionParam::Yes == is_a_function_parameter_enum);
		bool suppress_modifications = (ESuppressModifications::Yes == suppress_modifications_enum);
		if ((!maybe_storage_duration.has_value()) && is_a_function_parameter) {
			maybe_storage_duration = clang::StorageDuration::SD_Automatic;
		}
		bool is_a_local_variable = is_a_function_parameter;
		if (maybe_storage_duration.has_value()) {
			if (clang::StorageDuration::SD_Automatic == maybe_storage_duration.value()) {
				is_a_local_variable = true;
			}
		}

		auto& direct_type_state_ref = indirection_state_stack.m_direct_type_state;
		std::string direct_type_original_qtype_str = direct_type_state_ref.original_qtype_str();
		bool og_direct_type_was_char_type = (("char" == direct_type_original_qtype_str) || ("const char" == direct_type_original_qtype_str));
		bool og_direct_type_was_FILE_type = (("FILE" == direct_type_original_qtype_str) || ("const FILE" == direct_type_original_qtype_str)
											|| ("struct _IO_FILE" == direct_type_original_qtype_str) || ("const struct _IO_FILE" == direct_type_original_qtype_str));
		bool og_direct_type_was_target_of_other_untranslatable_type = false && (("jmp_buf" == direct_type_original_qtype_str) || ("const struct __jmp_buf_tag" == direct_type_original_qtype_str)
											|| ("struct __jmp_buf_tag" == direct_type_original_qtype_str) || ("const jmp_buf" == direct_type_original_qtype_str));
		bool og_direct_type_was_void_or_const_void_type = (("void" == direct_type_original_qtype_str) || ("const void" == direct_type_original_qtype_str));
		bool og_direct_type_was_const_void_type = ("const void" == direct_type_original_qtype_str);
		bool direct_type_is_function_type = false;
		if (direct_type_state_ref.current_qtype_if_any().has_value()) {
			direct_type_is_function_type = direct_type_state_ref.current_qtype_if_any().value().getTypePtr()->isFunctionType();
		} else {
			assert(false);
		}
		std::string direct_type_original_source_text = direct_type_state_ref.original_source_text();

		std::string type_original_source_text;
		if (1 <= indirection_state_stack.size()) {
			auto& indirection_state_ref = indirection_state_stack.at(0);
			type_original_source_text = indirection_state_ref.return_type_original_source_text();
		} else {
			type_original_source_text = direct_type_original_source_text;
		}

		bool has_external_storage = false;
		bool is_dependent_type = false;
		std::optional<CTParamUsageInfo> maybe_tparam_usage_info;
		std::optional<clang::QualType> maybe_DD_qtype;
		bool DD_is_not_modifiable = false;
		if (indirection_state_stack.m_maybe_DD.has_value()) {
			auto DD = indirection_state_stack.m_maybe_DD.value();
			if (DD) {
				maybe_tparam_usage_info = seems_to_contain_an_instantiation_of_a_template_parameter(*DD, type_original_source_text, Rewrite, state1_ptr);

				auto VD = dyn_cast<const clang::VarDecl>(DD);
				 if (VD) {
					has_external_storage = VD->hasExternalStorage();
				 }

				if ("" == direct_type_original_qtype_str) {
					auto maybe_typeLoc = typeLoc_if_available(*DD);
					if (state1_ptr && maybe_typeLoc.has_value()) {
						auto typeLoc = maybe_typeLoc.value();
						const auto l_SR = state1_ptr
							? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
							: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
						if ((l_SR).isValid() && (((l_SR).getBegin() < (l_SR).getEnd()) || ((l_SR).getBegin() == (l_SR).getEnd()))) {
							auto source_text2 = (Rewrite).getRewrittenText(l_SR);
							direct_type_state_ref.set_original_source_text(source_text2);
							direct_type_original_qtype_str = direct_type_state_ref.original_source_text();
						}
					} else {
						int q = 5;
					}
				}
				auto DD_qtype = DD->getType();
				maybe_DD_qtype = DD_qtype;
				if ((!DD_qtype.isNull()) || (!DD_qtype->isUndeducedType())) {
					if (DD_qtype->isInstantiationDependentType()) {
						is_dependent_type = true;

#ifndef NDEBUG
						if (!(maybe_tparam_usage_info.has_value())) {
							seems_to_contain_an_instantiation_of_a_template_parameter(*DD, Rewrite, state1_ptr);
							int q = 5;
						}
#endif /*!NDEBUG*/
					}
				}
			} else { assert(false); }

			if (state1_ptr && (state1_ptr->m_ast_context_ptr)) {
				auto& state1 = *state1_ptr;
				DD_is_not_modifiable = is_non_modifiable(*DD, *(state1.m_ast_context_ptr), Rewrite, state1);
			}

		} else if (indirection_state_stack.m_maybe_containing_D.has_value()) {
			auto& D = indirection_state_stack.m_maybe_containing_D.value();
			if (D) {
				maybe_tparam_usage_info = seems_to_contain_an_instantiation_of_a_template_parameter(*D, type_original_source_text, Rewrite, state1_ptr);
			} else { assert(false); }
		}
		bool seems_to_involve_a_template_param_originally = (maybe_tparam_usage_info.has_value() || is_dependent_type);
		retval.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally |= seems_to_involve_a_template_param_originally;
		retval.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally |= DD_is_not_modifiable;

		if (maybe_tparam_usage_info.has_value()) {
			auto& tparam_usage_info = maybe_tparam_usage_info.value();
			const std::string tparam_name = tparam_usage_info.first_used_param_as_string();
			std::string original_source_text = direct_type_state_ref.return_type_original_source_text();
			if (with_whitespace_removed(tparam_name) == with_whitespace_removed(original_source_text)) {
				/* So this declaration seems to involve a template parameter that corresponds to the direct 
				type. We presumably want to leave template parameters intact, so we'll indicate that the template
				parameter should be used in place of the direct type when rendering a text representation of 
				the declaration (type). */

				bool parameter_seems_to_correspond_with_the_direct_type = true;
				const auto maybe_original_direct_qtype = direct_type_state_ref.maybe_original_qtype();
				if (tparam_usage_info.TAL && maybe_original_direct_qtype.has_value()) {
					const auto arg_qtype = (*(tparam_usage_info.TAL)).get(tparam_usage_info.zbindex_of_first_used_param).getAsType();
					const auto og_qtype = maybe_original_direct_qtype.value();
					IF_DEBUG(std::string arg_qtype_str = arg_qtype.getAsString();)
					IF_DEBUG(std::string og_qtype_str = og_qtype.getAsString();)

					/* Just verifying that the argument corresponding to the template parameter has the same type as 
					the direct type's original type. */
					if (arg_qtype != og_qtype) {
						/* unexpected? */
						int q = 3;
						//parameter_seems_to_correspond_with_the_direct_type = false;
					}
				}
				if (parameter_seems_to_correspond_with_the_direct_type) {
					retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str = tparam_name;
				}
			}
		} else if (DD_is_not_modifiable) {
			std::string original_source_text = direct_type_state_ref.return_type_original_source_text();
			if (1 <= indirection_state_stack.size()) {
				original_source_text = indirection_state_stack.at(0).return_type_original_source_text();
			}

			auto does_not_seem_to_be_a_valid_type = [](std::string_view sv){
				bool retval = false;
				/* The case we're looking out for in particular is the declaration of multiple variables in a single 
				declaration statement. For example, in this declaration: 

				int i = 5, *ptr2 = NULL;

				clang will report the source range for the type of the ptr2 variable as the range that corresponds to 
				the string "int i = 5, *", which of course is not a valid type. */

				if ((std::string_view::npos != sv.find("=")) || (std::string_view::npos != sv.find("\\")) || (std::string_view::npos != sv.find("\n"))) {
					return true;
				}
				if (std::string_view::npos != sv.find(",")) {
					/* In valid types, commas only appear within angle brackets (as a template argument delimiter), right? */
					if (std::string_view::npos == sv.find("<")) {
						return true;
					}
					if (std::string_view::npos == sv.find(">")) {
						return true;
					}
				}
				return retval;
			};

			if (("" != original_source_text) && (!does_not_seem_to_be_a_valid_type(original_source_text))) {
				retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str = original_source_text;
			} else if (maybe_DD_qtype.has_value()) {
				auto& DD_qtype = maybe_DD_qtype.value();
				auto DD_qtype_str = DD_qtype.getAsString();
				retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str = DD_qtype_str;
			}
		}

		auto additional_void_star_replacement_criteria = [&]() {
			/* A (hacky) condition that `void_star_replacement` will only be used when `void*` 
			is literally present in the source text of the declaration being replaced. */
			bool retval = false;
			if (indirection_state_stack.m_maybe_DD.has_value()) {
				auto SR = state1_ptr
					? cm1_adj_nice_source_range(indirection_state_stack.m_maybe_DD.value()->getSourceRange(), *state1_ptr, Rewrite)
					: cm1_nice_source_range(indirection_state_stack.m_maybe_DD.value()->getSourceRange(), Rewrite);

				std::string source_text1;
				if (SR.isValid() && ((SR.getBegin() < SR.getEnd()) || (SR.getBegin() == SR.getEnd()))) {
					source_text1 = Rewrite.getRewrittenText(SR);
				}
				if (std::string::npos != with_whitespace_removed(source_text1).find("void*")) {
					retval = true;
				}
			}
			//return retval;
			return true;
		};

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
				auto& indirection_state_ref = indirection_state_stack.at(i);

				if (maybe_tparam_usage_info.has_value()) {
					auto& tparam_usage_info = maybe_tparam_usage_info.value();
					const std::string tparam_name = tparam_usage_info.first_used_param_as_string();
					std::string original_source_text = indirection_state_ref.return_type_original_source_text();
					if (with_whitespace_removed(tparam_name) == with_whitespace_removed(original_source_text)) {
						/* So this declaration seems to involve a template parameter that corresponds to the current 
						indirection. We presumably want to leave template parameters intact, so we don't want to include 
						this indirection (or nested ones) in any prefix or suffix as they would already be represented 
						in the template parameter. */

						bool parameter_seems_to_correspond_with_this_indirection = true;
						if (tparam_usage_info.TAL && indirection_state_ref.m_maybe_original_qtype.has_value()) {
							const auto arg_qtype = (*(tparam_usage_info.TAL)).get(tparam_usage_info.zbindex_of_first_used_param).getAsType();
							const auto og_qtype = indirection_state_ref.m_maybe_original_qtype.value();
							IF_DEBUG(std::string arg_qtype_str = arg_qtype.getAsString();)
							IF_DEBUG(std::string og_qtype_str = og_qtype.getAsString();)

							/* Just verifying that the argument corresponding to the template parameter has the same type as 
							this indirection's original type. */
							if (arg_qtype != og_qtype) {
								/* unexpected? */
								int q = 3;
								//parameter_seems_to_correspond_with_this_indirection = false;
							}
						}
						if (parameter_seems_to_correspond_with_this_indirection) {
							retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str = tparam_name;
							continue;
						}
					}
				} else if (DD_is_not_modifiable) {
					continue;
				}

				bool l_changed_from_original = (indirection_state_ref.current_species() != indirection_state_ref.original_species());

				bool is_char_star = false;
				bool is_FILE_star = false;
				bool is_other_untranslatable_indirect_type = false;
				bool is_void_star = false;
				bool is_void_star_star = false;
				bool is_function_pointer = false;
				bool is_function = false;
				bool is_outermost_indirection = (0 == i);
				bool is_innermost_indirection = (indirection_state_stack.size() == (i+1));
				if (is_innermost_indirection && (og_direct_type_was_char_type)) {
					is_char_star = true;
					l_changed_from_original = true;
				} else if (is_innermost_indirection && (og_direct_type_was_FILE_type)) {
					is_FILE_star = true;
					if (("struct _IO_FILE" == direct_type_original_qtype_str) || ("const struct _IO_FILE" == direct_type_original_qtype_str)) {
						/* _IO_FILE is just a non-portable alias of FILE. */
						std::string new_type_str = ("struct _IO_FILE" == direct_type_original_qtype_str) ? "FILE" : "const FILE";
						direct_type_state_ref.set_current_non_function_qtype_str(new_type_str);
						l_changed_from_original = true;
					}
					/* For the moment, we leave "FILE *" types alone. This may change at some point. */
					l_changed_from_original |= ("native pointer" != indirection_state_ref.original_species());
				} else if (is_innermost_indirection && (og_direct_type_was_target_of_other_untranslatable_type)) {
					is_other_untranslatable_indirect_type = true;
					/* This indirect type has been deemed untraslatable. */
					l_changed_from_original = false;
				} else if (is_innermost_indirection && (og_direct_type_was_void_or_const_void_type) && additional_void_star_replacement_criteria()) {
					is_void_star = true;
					l_changed_from_original = true;
				} else if (is_innermost_indirection && direct_type_is_function_type) {
					is_function_pointer = true;
				} else if ((!is_innermost_indirection) && indirection_state_stack.at(i+1).current_is_function_type()) {
					is_function_pointer = true;
				} else if (indirection_state_ref.current_is_function_type()) {
					is_function = true;
				}
				if (!is_innermost_indirection) {
					bool is_second_innermost_indirection = (indirection_state_stack.size() == (i+2));
					if (is_second_innermost_indirection && (og_direct_type_was_void_or_const_void_type) && additional_void_star_replacement_criteria()) {
						is_void_star_star = true;
						/* In the case of pointers to void (aka "void*"s), the direct type (i.e. "void")
						will be replaced with "mse::lh::void_star_replacement " and the last indirection
						will be considered a "transparent"/"pass-through"/"no-op"/"collapsed"/"eliminated"
						indirection, so this second-to-last indirection should be considered the last
						indirection. */
						is_innermost_indirection = true;
					}
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

				if (indirection_state_ref.current_is_function_type()) {
					//suffix_str = indirection_state_ref.current_params_string() + suffix_str;
				}

				if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator() || is_char_star) {
					if (indirection_state_ref.is_known_to_have_malloc_target() && (!indirection_state_ref.is_known_to_have_non_malloc_target())) {
						if (false && is_char_star) {
							/* We're assuming this is a null terminated string. We'll just leave it as a
							* char* for now. At some point we'll replace it with an mse::string or whatever. */
							suffix_str = "* ";
							retval.m_action_species = "char*";
						} else if (is_FILE_star) {
							/* We'll just leave it as a FILE* for now. */
							suffix_str = "* ";
							retval.m_action_species = "FILE*";
						} else if (is_other_untranslatable_indirect_type) {
							/* We'll just leave it as a native pointer. */
							suffix_str = "* ";
							retval.m_action_species = "other_untranslatable_indirect_type";
						} else if (is_void_star && (!seems_to_involve_a_template_param_originally)) {
							/* In the case of "void*" we're going to effectively "remove" one level of indirection
							by making it a "transparent"/"pass-through"/"no-op" indirection, and replacing the
							direct type (i.e. "void") with a type that replaces "void*" (i.e. the "void" and its
							indirection). */
							l_changed_from_original = true;
							prefix_str = "";
							suffix_str = " ";

							/* This "void*" might actually be a function with a return value of "void*", in which
							case the direct type should be a function type that includes its parameters. */
							std::string params_str;
							if (indirection_state_ref.current_is_function_type()) {
								clang::FunctionProtoTypeLoc functionProtoTypeLoc;
								if (indirection_state_ref.m_maybe_typeLoc.has_value()) {
									functionProtoTypeLoc = definition_TypeLoc(indirection_state_ref.m_maybe_typeLoc.value()).getAsAdjusted<clang::FunctionProtoTypeLoc>();
								}

								params_str = current_params_string(Rewrite, indirection_state_ref.m_function_type_state
									, functionProtoTypeLoc, suppress_modifications, state1_ptr);
							}

							std::string new_qtype_str;
							if ("Dual" == ConvertMode) {
								new_qtype_str = og_direct_type_was_const_void_type ? "MSE_LH_CONST_VOID_STAR " : "MSE_LH_VOID_STAR ";
							} else {
								new_qtype_str = og_direct_type_was_const_void_type ? "mse::lh::const_void_star_replacement " : "mse::lh::void_star_replacement ";
							}
							if (params_str.empty()) {
								direct_type_state_ref.set_current_non_function_qtype_str(new_qtype_str);
							} else {
								direct_type_state_ref.set_current_function_qtype_str(new_qtype_str , params_str);
							}
							retval.m_action_species = "void*";
						} else {
							if (is_innermost_indirection) {
								//retval.m_direct_type_must_be_non_const = true;
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
					} else if (indirection_state_ref.is_known_to_have_non_malloc_target() && (!indirection_state_ref.is_known_to_have_malloc_target())) {
						/* This is (likely) redundant as the prefix and suffix strings will be reassigned later
						in this function with a potentially updated size_text. */

						std::string size_text = indirection_state_ref.m_array_size_expr;

						if (false && is_char_star) {
							/* We're assuming this is a null terminated string. We'll just leave it as a
							* char[] for now. At some point we'll replace it with an mse::string or whatever. */
							if ((true) || (1 == indirection_state_stack.size())) {
								post_name_suffix_str = "[" + size_text + "]";
							} else {
								assert(1 < indirection_state_stack.size());
								//suffix_str = "[" + size_text + "]";
								/* native array decays to pointer */
								suffix_str = "* ";
							}
							retval.m_action_species = "char[]";
						} else if(is_argv) {
							suffix_str = "* ";
							retval.m_action_species = "char** argv";
						} else if (is_other_untranslatable_indirect_type) {
							/* We'll just leave it as a native array. */
							if ((true) || (1 == indirection_state_stack.size())) {
								post_name_suffix_str = "[" + size_text + "]";
							} else {
								assert(1 < indirection_state_stack.size());
								//suffix_str = "[" + size_text + "]";
								/* native array decays to pointer */
								suffix_str = "* ";
							}
							retval.m_action_species = "other_untranslatable_indirect_type";
						} else {
							l_changed_from_original = true;
							if (is_a_function_parameter || (("native array" != indirection_state_ref.original_species()) && (!has_external_storage))) {
								if ("FasterAndStricter" == ConvertMode) {
									prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
									suffix_str = "> ";
								} else {
									if (indirection_state_ref.xscope_eligibility())
									{
										if ("Dual" == ConvertMode) {
											prefix_str = "MSE_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE(";
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
								if (is_innermost_indirection) {
									//retval.m_direct_type_must_be_non_const = true;
								}

								if ("Dual" == ConvertMode) {
									prefix_str = "MSE_LH_FIXED_ARRAY_TYPE_PREFIX ";
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
						if (false && is_char_star) {
							/* We're assuming this is a null terminated string. We'll just leave it as a
							* char* for now. At some point we'll replace it with an mse::string or whatever. */
							suffix_str = "* ";
							retval.m_action_species = "char*";
						} else if (is_FILE_star) {
							/* We'll just leave it as a FILE* for now. */
							suffix_str = "* ";
							retval.m_action_species = "FILE*";
						} else if (is_other_untranslatable_indirect_type) {
							/* We'll just leave it as a native pointer. */
							suffix_str = "* ";
							retval.m_action_species = "other_untranslatable_indirect_type";
						} else if(is_argv) {
							suffix_str = "* ";
							retval.m_action_species = "char** argv";
						} else {
							if (is_innermost_indirection) {
								//retval.m_direct_type_must_be_non_const = true;
							}
							if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
								suffix_str = "> ";
							} else {
								if (indirection_state_ref.xscope_eligibility())
								{
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE(";
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
					}
				} else if (indirection_state_ref.is_a_pointer_that_has_not_been_determined_to_be_an_array()) {
					if (false && is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else if (is_other_untranslatable_indirect_type) {
						/* We'll just leave it as a native pointer. */
						suffix_str = "* ";
						retval.m_action_species = "other_untranslatable_indirect_type";
					} else if (is_void_star && (!seems_to_involve_a_template_param_originally)) {
						/* In the case of "void*" we're going to effectively "remove" one level of indirection
						by making it a "transparent"/"pass-through"/"no-op" indirection, and replacing the
						direct type (i.e. "void") with a type that replaces "void*" (i.e. the "void" and its
						indirection). */
						l_changed_from_original = true;
						prefix_str = "";
						suffix_str = " ";

						/* This "void*" might actually be a function with a return value of "void*", in which
						case the direct type should be a function type that includes its parameters. */
						std::string params_str;
						if (indirection_state_ref.current_is_function_type()) {
							clang::FunctionProtoTypeLoc functionProtoTypeLoc;
							if (indirection_state_ref.m_maybe_typeLoc.has_value()) {
								functionProtoTypeLoc = definition_TypeLoc(indirection_state_ref.m_maybe_typeLoc.value()).getAsAdjusted<clang::FunctionProtoTypeLoc>();
							}

							params_str = current_params_string(Rewrite, indirection_state_ref.m_function_type_state
								, functionProtoTypeLoc, suppress_modifications, state1_ptr);
						}

						std::string new_qtype_str;
						if ("Dual" == ConvertMode) {
							new_qtype_str = og_direct_type_was_const_void_type ? "MSE_LH_CONST_VOID_STAR " : "MSE_LH_VOID_STAR ";
						} else {
							new_qtype_str = og_direct_type_was_const_void_type ? "mse::lh::const_void_star_replacement " : "mse::lh::void_star_replacement ";
						}
						if (params_str.empty()) {
							direct_type_state_ref.set_current_non_function_qtype_str(new_qtype_str);
						} else {
							direct_type_state_ref.set_current_function_qtype_str(new_qtype_str , params_str);
						}
						retval.m_action_species = "void*";
					} else if (is_function_pointer) {
						l_changed_from_original = true;

						auto& function_type_state_ref = ((i + 1) < indirection_state_stack.size())
							? indirection_state_stack.at(i+1).m_function_type_state
							: indirection_state_stack.m_direct_type_state.m_function_type_state;

						clang::FunctionProtoTypeLoc functionProtoTypeLoc;

						if (indirection_state_ref.m_maybe_typeLoc.has_value()) {
							auto typeLoc = indirection_state_ref.m_maybe_typeLoc.value();
							auto pointerTypeLoc = definition_TypeLoc(typeLoc).getAsAdjusted<clang::PointerTypeLoc>();
							if (pointerTypeLoc) {
								auto l_pointee_typeLoc = definition_TypeLoc(pointerTypeLoc.getPointeeLoc());

								functionProtoTypeLoc = definition_TypeLoc(l_pointee_typeLoc).getAsAdjusted<clang::FunctionProtoTypeLoc>();
							} else {
								int q = 3;
							}
						}

						std::string l_pointee_params_current_str = current_params_string(Rewrite, function_type_state_ref, functionProtoTypeLoc, suppress_modifications, state1_ptr);

						if (true && ("Dual" == ConvertMode)) {
							prefix_str = "MSE_LH_FUNCTION_POINTER_TYPE_PREFIX ";
							suffix_str = " MSE_LH_FUNCTION_POINTER_TYPE_SUFFIX(" + l_pointee_params_current_str + ") ";
							post_name_suffix_str = " MSE_LH_FUNCTION_POINTER_TYPE_POST_NAME_SUFFIX(" + l_pointee_params_current_str + ")";
						} else {
							prefix_str = "mse::lh::TNativeFunctionPointerReplacement<";
							suffix_str = l_pointee_params_current_str + "> ";
						}
						retval.m_action_species = "function pointer to mse::lh::TNativeFunctionPointerReplacement";
					} else {
						if (true/*for now*/) {
							l_changed_from_original = true;

							if ("FasterAndStricter" == ConvertMode) {
								prefix_str = "mse::TXScopeAnyPointer<";
								suffix_str = "> ";
							} else {
								if (indirection_state_ref.xscope_eligibility())
								{
									if ("Dual" == ConvertMode) {
										prefix_str = "MSE_LH_LOCAL_VAR_ONLY_POINTER_TYPE(";
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
				} else if (false && ("malloc target" == indirection_state_ref.current_species())) {
					assert(false);
					if (false && is_char_star) {
						/* We're assuming this is a null terminated string. We'll just leave it as a
						* char* for now. At some point we'll replace it with an mse::string or whatever. */
						suffix_str = "* ";
						retval.m_action_species = "char*";
					} else if (is_FILE_star) {
						/* We'll just leave it as a FILE* for now. */
						suffix_str = "* ";
						retval.m_action_species = "FILE*";
					} else if (is_other_untranslatable_indirect_type) {
						/* We'll just leave it as a native pointer. */
						suffix_str = "* ";
						retval.m_action_species = "other_untranslatable_indirect_type";
					} else if (is_void_star && (!seems_to_involve_a_template_param_originally)) {
						/* In the case of "void*" we're going to effectively "remove" one level of indirection
						by making it a "transparent"/"pass-through"/"no-op" indirection, and replacing the
						direct type (i.e. "void") with a type that replaces "void*" (i.e. the "void" and its
						indirection). */
						l_changed_from_original = true;
						prefix_str = "";
						suffix_str = " ";

						/* This "void*" might actually be a function with a return value of "void*", in which
						case the direct type should be a function type that includes its parameters. */
						std::string params_str;
						if (indirection_state_ref.current_is_function_type()) {
							clang::FunctionProtoTypeLoc functionProtoTypeLoc;
							if (indirection_state_ref.m_maybe_typeLoc.has_value()) {
								functionProtoTypeLoc = definition_TypeLoc(indirection_state_ref.m_maybe_typeLoc.value()).getAsAdjusted<clang::FunctionProtoTypeLoc>();
							}

							params_str = current_params_string(Rewrite, indirection_state_ref.m_function_type_state
								, functionProtoTypeLoc, suppress_modifications, state1_ptr);
						}

						std::string new_qtype_str;
						if ("Dual" == ConvertMode) {
							new_qtype_str = "MSE_LH_VOID_STAR ";
						} else {
							new_qtype_str = "mse::lh::void_star_replacement ";
						}
						if (params_str.empty()) {
							direct_type_state_ref.set_current_non_function_qtype_str(new_qtype_str);
						} else {
							direct_type_state_ref.set_current_function_qtype_str(new_qtype_str , params_str);
						}
						retval.m_action_species = "void*";
					} else {
						{
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
				} else if ("native reference" == indirection_state_ref.current_species()) {
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
					? indirection_state_stack.at(i+1).m_maybe_typeLoc
					: indirection_state_stack.m_direct_type_state.m_maybe_typeLoc;

				bool b1 = indirection_state_ref.m_maybe_typeLoc.has_value();
				bool b2 = pointee_maybe_typeLoc.has_value();

				if (indirection_state_ref.m_maybe_typeLoc.has_value() && pointee_maybe_typeLoc.has_value()) {
					auto& typeLoc = indirection_state_ref.m_maybe_typeLoc.value();
					IF_DEBUG(auto typeLocClass = typeLoc.getTypeLocClass();)

					auto pointee_typeLoc = pointee_maybe_typeLoc.value();
					IF_DEBUG(auto pointee_typeLocClass = pointee_typeLoc.getTypeLocClass();)

					//auto SR = typeLoc.getSourceRange();
					bool needs_processing = false;
					if (!(indirection_state_ref.m_maybe_source_range.has_value())) {
						needs_processing = true;
						const auto l_SR = state1_ptr
							? cm1_adj_nice_source_range(typeLoc.getSourceRange(), *state1_ptr, Rewrite)
							: cm1_nice_source_range(typeLoc.getSourceRange(), Rewrite);
						indirection_state_ref.m_maybe_source_range = l_SR;
						const auto l_definition_SR = state1_ptr
							? cm1_adj_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), *state1_ptr, Rewrite)
							: cm1_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), Rewrite);
						if (!(l_SR == l_definition_SR)) {
							/* This type seems to be a typedef. So its definition is in a different location from
							where it used (to declare a variable).*/
							indirection_state_ref.m_maybe_typedef_definition_source_range = state1_ptr
								? cm1_adj_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), *state1_ptr, Rewrite)
								: cm1_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), Rewrite);
						}
					}
					auto& SR = indirection_state_ref.m_maybe_source_range.value();
					bool is_typedef = indirection_state_ref.m_maybe_typedef_definition_source_range.has_value();
					auto& definition_SR = is_typedef ? indirection_state_ref.m_maybe_typedef_definition_source_range.value() : SR;

#ifndef NDEBUG
					if (definition_SR.isValid()) {
						auto& SM = Rewrite.getSourceMgr();
						IF_DEBUG(std::string debug_source_location_str = definition_SR.getBegin().printToString(SM);)

						//DEBUG_SOURCE_TEXT_STR(debug_source_text, definition_SR, Rewrite);

						if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
							int q = 5;
						}
					}

					if (is_typedef) {
						auto TDDSR = state1_ptr
							? cm1_adj_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), *state1_ptr, Rewrite)
							: cm1_nice_source_range(definition_TypeLoc(typeLoc).getSourceRange(), Rewrite);
						std::string TDD_text = Rewrite.getRewrittenText(TDDSR);

						auto tdtl = typeLoc.getAsAdjusted<clang::TypedefTypeLoc>();
						if (tdtl) {
							auto TDND = tdtl.getTypedefNameDecl();
							if (TDND) {
								if (false) {
									auto name_SL = TDND->getLocation();
									auto name_SR1 = state1_ptr
										? cm1_adj_nice_source_range(clang::SourceRange{ name_SL, name_SL }, *state1_ptr, Rewrite)
										: cm1_nice_source_range(clang::SourceRange{ name_SL, name_SL }, Rewrite);
									std::string name_text = Rewrite.getRewrittenText(name_SR1);
									int offset1 = name_text.length() - 1;
									if (0 > offset1) {
										offset1 = 0;
									}
									auto name_SR = clang::SourceRange{ name_SL, name_SL.getLocWithOffset(offset1) };
									name_text = Rewrite.getRewrittenText(name_SR);
								}
								int q = 5;
							} else {
								int q = 3;
							}
						} else {
							int q = 3;
						}
					}
#endif /*!NDEBUG*/

					auto& maybe_lexical_suffix_SR = indirection_state_ref.m_maybe_suffix_source_range;
					auto& maybe_prefix_SR = indirection_state_ref.m_maybe_prefix_source_range;

					if (needs_processing && SR.isValid()) {
						std::string old_definition_text = Rewrite.getRewrittenText(definition_SR);

						if (indirection_state_stack.m_maybe_DD.has_value()) {
							std::optional<clang::SourceLocation> maybe_name_SL;
							if (is_typedef) {
								auto tdtl = typeLoc.getAsAdjusted<clang::TypedefTypeLoc>();
								if (tdtl) {
									auto TDND = tdtl.getTypedefNameDecl();
									if (TDND) {
										maybe_name_SL = state1_ptr
											? cm1_adj_nice_source_location(TDND->getLocation(), *state1_ptr, Rewrite)
											: cm1_nice_source_location(TDND->getLocation(), Rewrite);
									} else {
										int q = 3;
									}
								} else {
									int q = 3;
								}
							} else {
								auto VD = dyn_cast<const clang::VarDecl>(indirection_state_stack.m_maybe_DD.value());
								auto FD = dyn_cast<const clang::FieldDecl>(indirection_state_stack.m_maybe_DD.value());
								if (VD || FD) {
									auto raw_name_SL = VD ? VD->getLocation() : FD->getLocation();
									maybe_name_SL = state1_ptr
										? cm1_adj_nice_source_location(raw_name_SL, *state1_ptr, Rewrite)
										: cm1_nice_source_location(raw_name_SL, Rewrite);
								}
							}
							if (maybe_name_SL.has_value()) {
								/* This declaration has a variable/field/typedef name. */
								auto name_SL = maybe_name_SL.value();
								auto name_SR1 = state1_ptr
									? cm1_adj_nice_source_range(clang::SourceRange{ name_SL, name_SL }, *state1_ptr, Rewrite)
									: cm1_nice_source_range(clang::SourceRange{ name_SL, name_SL }, Rewrite);
								std::string name_text = Rewrite.getRewrittenText(name_SR1);
								int offset1 = name_text.length() - 1;
								if (0 > offset1) {
									offset1 = 0;
								}
								auto name_SR = clang::SourceRange{ name_SL, name_SL.getLocWithOffset(offset1) };
								name_text = Rewrite.getRewrittenText(name_SR);
								if (!((definition_SR.getEnd() < name_SR.getBegin()) || (name_SR.getEnd() < definition_SR.getBegin()))) {
									/* The given source range of the type (currently) contains the variable/field/typedef name.
									This is not the case for most declarations (such as with 'int a;'), but is for native
									array declarations ('int a[3];') and function pointer declarations ('int (*a)(int)'). */
									auto enclosing_parentheses1 = enclosing_parentheses(Rewrite, name_SR, definition_SR);
									if (1 <= enclosing_parentheses1.size()) {
										/* The variable/field name is enclosed by parentheses (such as with the function
										pointer declaration 'int (*a)(int)'). */
										IF_DEBUG(std::string enclosing_parentheses_text = Rewrite.getRewrittenText(enclosing_parentheses1.back());)
										auto new_SLE = enclosing_parentheses1.back().getBegin().getLocWithOffset(-1);
										if (!(new_SLE < definition_SR.getBegin())) {
											/* Here we truncate the source range of the type to exclude the variable/field
											name and any enclosing parentheses. */
											definition_SR.setEnd(new_SLE);
											old_definition_text = Rewrite.getRewrittenText(definition_SR);
											int q = 5;
										} else {
											int q = 3;
										}
										if (0 == i) {
											if (ConvertToSCPP && (!suppress_modifications) && state1_ptr) {
												/* This is the last "indirection" to be processed. Now we're going
												to "blank out"/erase any parentheses and contained items ((pointer)
												asterisks, bracketed array size expressions, const qualifiers, etc)
												that are enclosing the variable/field name. Their semantics should
												now be expressed in the new converted types. */
												{
													auto left_rawSR = clang::SourceRange{ enclosing_parentheses1.back().getBegin(), name_SR.getBegin().getLocWithOffset(-1) };
													auto left_SR = state1_ptr
														? write_once_source_range(cm1_adj_nice_source_range(left_rawSR, *state1_ptr, Rewrite))
														: write_once_source_range(cm1_nice_source_range(left_rawSR, Rewrite));
													std::string left_blank_text = Rewrite.getRewrittenText(left_SR);
													for (auto& ch : left_blank_text) {
														ch = ' ';
													}
													state1_ptr->m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, left_SR, left_blank_text);
												}
												{
													/* Instead of blanking out the whole range from the end of the name to the
													right parenthesis in one operation, here we blank out each element in the
													range individually. Doing it this ways allows the "blanked out" regions to
													be subsequently overwitten if necessary. In particular, parts of this region
													that may have originally conatined array size expressions (in square '[]'
													brackets) may be subsequently overwritten with
													"MSE_LH_FIXED_ARRAY_TYPE_POST_NAME_SUFFIX(...)" expressions. */
													auto SL2 = name_SR.getEnd().getLocWithOffset(+1);
													while (!(enclosing_parentheses1.back().getEnd() < SL2)) {
														/* "Blanking out"/erasing enclosing items to the right of the
														variable/field name. */
														auto SR2 = state1_ptr
															? cm1_adj_nice_source_range(clang::SourceRange{ SL2, SL2 }, *state1_ptr, Rewrite)
															: cm1_nice_source_range(clang::SourceRange{ SL2, SL2 }, Rewrite);
														std::string text1 = Rewrite.getRewrittenText(SR2);
														for (auto& ch : text1) {
															ch = ' ';
														}
														state1_ptr->m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, SR2, text1);
														//Rewrite.ReplaceText(clang::SourceRange{ SL2, SL2 }, text1);
														SL2 = SL2.getLocWithOffset(+1);
													}
												}
											}
										}
									} else {
										auto new_SLE = name_SR.getBegin().getLocWithOffset(-1);
										if (!(new_SLE < definition_SR.getBegin())) {
											/* Here we truncate the source range of the type to exclude the variable/field/typedef
											name. */
											definition_SR.setEnd(new_SLE);
											old_definition_text = Rewrite.getRewrittenText(definition_SR);
											int q = 5;
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
							? indirection_state_stack.at(i+1).m_maybe_source_range
							: indirection_state_stack.m_direct_type_state.m_maybe_source_range;
						if (!(pointee_maybe_stored_source_range.has_value())) {
							pointee_needs_processing = true;
							pointee_maybe_stored_source_range = state1_ptr ?
								cm1_adj_nice_source_range(pointee_typeLoc.getSourceRange(), *state1_ptr, Rewrite)
								: cm1_nice_source_range(pointee_typeLoc.getSourceRange(), Rewrite);
						}

						if (pointee_maybe_stored_source_range.value().isValid()) {
							auto& pointee_SR = pointee_maybe_stored_source_range.value();
							old_pointee_text = Rewrite.getRewrittenText(pointee_SR);

							if (pointee_needs_processing) {
								if (pointee_SR.getEnd().isMacroID()) {
									if (state1_ptr) {
										pointee_SR = cm1_adjusted_source_range(pointee_SR, *state1_ptr, Rewrite);
									} else {
										/* The pointee appears to be (or end with) a macro. */
										/* It could be a function macro, but at this point pointee_SR (and
										old_pointee_text) will only encompass the macro name, not the arguments
										(or parentheses), if any. We need to obtain the range that includes the
										arguments. */
										/* super hack: We'll just search for the last closing parenthesis (if any)
										and assume it is the closing parentheses of the macro arguments. */
										auto close_parenthesis_pos = old_definition_text.find_last_of(')');
										if (std::string::npos != close_parenthesis_pos) {
											auto new_pointee_SLE = definition_SR.getBegin().getLocWithOffset(close_parenthesis_pos);
											if (pointee_SR.getEnd() < new_pointee_SLE) {
												pointee_SR.setEnd(new_pointee_SLE);
												old_pointee_text = Rewrite.getRewrittenText(pointee_SR);
											}
										}
									}
								}
								if (definition_SR.getEnd() < pointee_SR.getEnd()) {
									if (definition_SR.getEnd() > pointee_SR.getBegin()) {
										pointee_SR.setEnd(definition_SR.getEnd());
										old_pointee_text = Rewrite.getRewrittenText(pointee_SR);
										int q = 5;
									} else {
										int q = 5;
									}
								}
							}

							bool pointee_including_any_const_qualifier_needs_processing = false;
							auto& pointee_maybe_stored_source_range_including_any_const_qualifier = ((i + 1) < indirection_state_stack.size())
								? indirection_state_stack.at(i+1).m_maybe_source_range_including_any_const_qualifier
								: indirection_state_stack.m_direct_type_state.m_maybe_source_range_including_any_const_qualifier;
							if (!(pointee_maybe_stored_source_range_including_any_const_qualifier.has_value())) {
								pointee_including_any_const_qualifier_needs_processing = true;
								pointee_maybe_stored_source_range_including_any_const_qualifier = pointee_SR;
							}
							auto& cq_pointee_SR = pointee_maybe_stored_source_range_including_any_const_qualifier.value();

							if (pointee_including_any_const_qualifier_needs_processing) {
								auto pointee_SR = cq_pointee_SR;
								cq_pointee_SR = extended_to_include_west_const_if_any(Rewrite, cq_pointee_SR);
								old_pointee_text = Rewrite.getRewrittenText(cq_pointee_SR);
								if (cq_pointee_SR.getBegin() < definition_SR.getBegin()) {
									definition_SR.setBegin(cq_pointee_SR.getBegin());
									old_definition_text = Rewrite.getRewrittenText(definition_SR);
									int q = 5;
								}
							}

							auto arrayTypeLoc = typeLoc.getAsAdjusted<clang::ArrayTypeLoc>();
							if (arrayTypeLoc) {
								bool definition_SR_end_adjusted = false;
								if ((!is_typedef) && indirection_state_stack.m_maybe_DD.has_value()) {
									auto VD = dyn_cast<const clang::VarDecl>(indirection_state_stack.m_maybe_DD.value());
									auto FD = dyn_cast<const clang::FieldDecl>(indirection_state_stack.m_maybe_DD.value());
									if (VD || FD) {
										/* In the case of native array variable declarations (like 'int array_name[5];')
										we're choosing to adjust the end of the type's source range to the end of the part
										before the start of the array variable name (rather than at the right bracket). */
										auto raw_name_SL = VD ? VD->getLocation() : FD->getLocation();
										auto name_SL = state1_ptr
											? cm1_adj_nice_source_location(raw_name_SL, *state1_ptr, Rewrite)
											: cm1_nice_source_location(raw_name_SL, Rewrite);
										auto new_SLE = cq_pointee_SR.getEnd();
										if (name_SL < new_SLE) {
											/* We're trying to avoid this situation. If it does occur, we don't really
											handle it very well. */
											new_SLE = name_SL.getLocWithOffset(-2);
										}
										if (!(new_SLE < definition_SR.getBegin())) {
											definition_SR.setEnd(new_SLE);
											definition_SR_end_adjusted = true;
										} else {
											int q = 3;
										}
									}
								}
								if (!definition_SR_end_adjusted) {
									auto brackets_SR = state1_ptr
										? cm1_adj_nice_source_range(arrayTypeLoc.getBracketsRange(), *state1_ptr, Rewrite)
										: cm1_nice_source_range(arrayTypeLoc.getBracketsRange(), Rewrite);
									if (brackets_SR.isValid()) {
										auto new_SLE = brackets_SR.getBegin().getLocWithOffset(-1);
										if (!(new_SLE < definition_SR.getBegin())) {
											definition_SR.setEnd(new_SLE);
										} else {
											int q = 3;
										}
									}
								}
							}

							int lexical_end_offset1 = int(old_definition_text.length()) - 1;
							if (0 > lexical_end_offset1) {
								lexical_end_offset1 = 0;
							}
							clang::SourceRange lexical_SR { definition_SR.getBegin(), definition_SR.getBegin().getLocWithOffset(lexical_end_offset1) };
							std::string old_definition_text2 = Rewrite.getRewrittenText(lexical_SR);

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

							clang::SourceRange prefix_SR { definition_SR.getBegin(), cq_pointee_SR.getBegin().getLocWithOffset(-1) };
							if (prefix_SR.isValid() && (!(prefix_SR.getEnd() < prefix_SR.getBegin()))) {
								maybe_prefix_SR = prefix_SR;
							}
						}
						if (!is_innermost_indirection) {
							int q = 5;
						}
					}

					if (definition_SR.isValid()) {

						if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator() 
							&& indirection_state_ref.is_known_to_have_non_malloc_target() 
							&& !(indirection_state_ref.is_known_to_have_malloc_target())) {

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

							if (!indirection_state_ref.m_array_size_expr_read_from_source_text) {
								auto ArrayLoc = definition_TypeLoc(typeLoc).getAsAdjusted<clang::ArrayTypeLoc>();
								if (ArrayLoc) {
									auto brackets_SR = ArrayLoc.getBracketsRange();
									auto size_expr_SR = clang::SourceRange{ brackets_SR.getBegin().getLocWithOffset(+1), brackets_SR.getEnd().getLocWithOffset(-1) };
									if (size_expr_SR.isValid() && (!(size_expr_SR.getEnd() < size_expr_SR.getBegin()))) {
										std::string size_expr_text = Rewrite.getRewrittenText(size_expr_SR);
										if ("" != size_expr_text) {
											indirection_state_ref.m_array_size_expr = size_expr_text;
											indirection_state_ref.m_array_size_expr_read_from_source_text = true;

											/* We've updated the array size expression text (from the literal number derived from
											the type to the expression read from the source text), so here we're updating the prefix
											and sufix strings accordingly. */

											std::string size_text = indirection_state_ref.m_array_size_expr;

											if (false && is_char_star) {
												/* We're assuming this is a null terminated string. We'll just leave it as a
												* char[] for now. At some point we'll replace it with an mse::string or whatever. */
												if ((true) || (1 == indirection_state_stack.size())) {
													post_name_suffix_str = "[" + size_text + "]";
												} else {
													assert(1 < indirection_state_stack.size());
													//suffix_str = "[" + size_text + "]";
													/* native array decays to pointer */
													suffix_str = "* ";
												}
												retval.m_action_species = "char[]";
											} else if (is_other_untranslatable_indirect_type) {
												/* We'll just leave it as a native array. */
												if ((true) || (1 == indirection_state_stack.size())) {
													post_name_suffix_str = "[" + size_text + "]";
												} else {
													assert(1 < indirection_state_stack.size());
													//suffix_str = "[" + size_text + "]";
													/* native array decays to pointer */
													suffix_str = "* ";
												}
												retval.m_action_species = "other_untranslatable_indirect_type";
											} else {
												l_changed_from_original = true;
												if (is_a_function_parameter || ("native array" != indirection_state_ref.original_species())) {
													if ("FasterAndStricter" == ConvertMode) {
														prefix_str = "mse::TXScopeCSSSXSTERAIterator<";
														suffix_str = "> ";
													} else {
														if (indirection_state_ref.xscope_eligibility())
														{
															if ("Dual" == ConvertMode) {
																prefix_str = "MSE_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE(";
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
													if (is_innermost_indirection) {
														//retval.m_direct_type_must_be_non_const = true;
													}

													if ("Dual" == ConvertMode) {
														prefix_str = "MSE_LH_FIXED_ARRAY_TYPE_PREFIX ";
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
						} else if (is_outermost_indirection && is_function) {
							/* We normally put the function parameters in the suffix or post_name_suffix of
							the parent indirection. But when the "outermost" indirection is a function, it's
							just a function declaration (without a parent indirection), so we put the function
							parameters in the indirection's own post_name_suffix. */
							std::string params_str;
							assert(indirection_state_ref.current_is_function_type());
							if (indirection_state_ref.current_is_function_type()) {
								clang::FunctionProtoTypeLoc functionProtoTypeLoc;
								if (indirection_state_ref.m_function_type_state.m_maybe_functionProtoTypeLoc.has_value()) {
									IF_DEBUG(auto typeLocClass = definition_TypeLoc(indirection_state_ref.m_function_type_state.m_maybe_functionProtoTypeLoc.value()).getTypeLocClass();)
									functionProtoTypeLoc = definition_TypeLoc(indirection_state_ref.m_function_type_state.m_maybe_functionProtoTypeLoc.value()).getAsAdjusted<clang::FunctionProtoTypeLoc>();
									if (functionProtoTypeLoc) {
										int q = 5;
									} else {
										int q = 5;
									}
								}

								bool is_declaration = false;
								if (indirection_state_stack.m_maybe_DD.has_value()) {
									auto DD = indirection_state_stack.m_maybe_DD.value();
									is_declaration = (nullptr != dyn_cast<const clang::FunctionDecl>(DD));
								}

								params_str = current_params_string(Rewrite, indirection_state_ref.m_function_type_state
									, functionProtoTypeLoc, suppress_modifications, state1_ptr, is_declaration);
							}

							post_name_suffix_str = params_str + post_name_suffix_str;
						}
					}

#define REGOBJ_TEST1_FLAG false

					if (!is_argv) {
						if ((REGOBJ_TEST1_FLAG
								|| (("pointer target" == indirection_state_ref.current_pointer_target_state())
									&& ("native pointer target" == indirection_state_ref.original_pointer_target_state()))
							)
							/*
							&& (!string_begins_with(prefix_str, "mse::TRegisteredObj<"))
							&& (!string_begins_with(prefix_str, "MSE_LH_ADDRESSABLE_TYPE("))
							&& (!string_begins_with(prefix_str, "const mse::TRegisteredObj<"))
							&& (!string_begins_with(prefix_str, "const MSE_LH_ADDRESSABLE_TYPE("))
							&& ("" == post_name_suffix_str)
							*/
							) {

							if ("native reference" != indirection_state_ref.current_species()) {
								if ("Dual" == ConvertMode) {
									prefix_str = "MSE_LH_ADDRESSABLE_TYPE(" + prefix_str;
									suffix_str = suffix_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
								} else {
									prefix_str = "mse::TRegisteredObj<" + prefix_str;
									suffix_str = suffix_str + " >";
								}
								retval.m_some_addressable_indirection = true;
							} else {
								int q = 5;
							}
						}
					}

					if (definition_SR.isValid()) {
						if (ConvertToSCPP && (!suppress_modifications) && state1_ptr) {
							auto& state1 = *state1_ptr;
							//suffix_str = indirection_state_ref.m_function_type_state.m_params_current_str + suffix_str;
							if ("" != suffix_str) {
								/* This function generates a prefix, a suffix, and possibly a post_name_suffix
								that can be combined with a direct type and a variable name to construct the
								new converted declaration. If indicated, this function will also place the
								generated prefix and suffix(es) at the proper place in the existing source text
								(sometimes overwriting parts of the existing source text as appropriate). We do
								that here: */

								indirection_state_ref.m_suffix_str = suffix_str;
								if (maybe_lexical_suffix_SR.has_value()) {
									state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(maybe_lexical_suffix_SR.value()), suffix_str);
									indirection_state_ref.m_suffix_SR_or_insert_before_point = maybe_lexical_suffix_SR.value();
								} else {
									const auto insert_after_point = definition_SR.getEnd();
									const auto insert_before_point = definition_SR.getEnd().getLocWithOffset(+1);
									/* We need to indicate the source range to be overwritten. But in this case we're inserting text
									and not overwriting any text. So intuitively, the range should maybe be an empty range located
									adjacent to the insertion point. But we want the range to be unique to each indirection level so
									that marking the range as "write once" won't prevent subsequent insertion operations of other
									indirection levels that may occur at the same location. So, as a hack to make the range unique
									to the indirection level, we're going to extended the range by an amount equal to the
									indirection level. In theory this hack could cause problems. We may be able to get away with it
									in paractice. */
									const auto hacked_range_end = definition_SR.getEnd().getLocWithOffset(+j);
									const auto hacked_SR = write_once_source_range({ definition_SR.getEnd().getLocWithOffset(+1), hacked_range_end });

									DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, hacked_SR, Rewrite);
									const auto debug_SR1 = clang::SourceRange({ insert_before_point.getLocWithOffset(-1), insert_before_point.getLocWithOffset(-1) });
									DEBUG_SOURCE_TEXT_STR(debug_source_text1, debug_SR1, Rewrite);
									const auto debug_SR2 = clang::SourceRange({ insert_before_point, insert_before_point });
									DEBUG_SOURCE_TEXT_STR(debug_source_text2, debug_SR2, Rewrite);
									DEBUG_SOURCE_TEXT_STR(debug_source_text3, definition_SR, Rewrite);

									state1.m_pending_code_modification_actions.add_insert_after_token_at_given_location_action(Rewrite, hacked_SR, insert_after_point, suffix_str);
									indirection_state_ref.m_suffix_SR_or_insert_before_point = definition_SR.getEnd().getLocWithOffset(+1);
								}

								indirection_state_ref.m_prefix_str = prefix_str;
								if (maybe_prefix_SR.has_value()) {
									state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(maybe_prefix_SR.value()), prefix_str);
									indirection_state_ref.m_prefix_SR_or_insert_before_point = maybe_prefix_SR.value();
								} else {
									const auto insert_before_point = definition_SR.getBegin();
									/* We need to indicate the source range to be overwritten. But in this case we're inserting text
									and not overwriting any text. So intuitively, the range should maybe be an empty range located
									adjacent to the insertion point. But we want the range to be unique to each indirection level so
									that marking the range as "write once" won't prevent subsequent insertion operations of other
									indirection levels that may occur at the same location. So, as a hack to make the range unique
									to the indirection level, we're going to extended the range by an amount equal to the
									indirection level. In theory this hack could cause problems. We may be able to get away with it
									in paractice. */
									const auto hacked_range_end = definition_SR.getBegin().getLocWithOffset(-1 + j);
									const auto hacked_SR = write_once_source_range({ definition_SR.getBegin(), hacked_range_end });

									state1.m_pending_code_modification_actions.add_insert_before_given_location_action(Rewrite, write_once_source_range({ definition_SR.getBegin(), hacked_range_end }), insert_before_point, prefix_str);
									indirection_state_ref.m_prefix_SR_or_insert_before_point = definition_SR.getBegin();
								}

								auto arrayTypeLoc = typeLoc.getAsAdjusted<clang::ArrayTypeLoc>();
								if (arrayTypeLoc) {
									auto brackets_SR = state1_ptr
										? cm1_adj_nice_source_range(arrayTypeLoc.getBracketsRange(), *state1_ptr, Rewrite)
										: cm1_nice_source_range(arrayTypeLoc.getBracketsRange(), Rewrite);
									if (brackets_SR.isValid()) {
										state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(brackets_SR), post_name_suffix_str);
									}
								} else if (is_function_pointer) {
									auto pointerTypeLoc = definition_TypeLoc(typeLoc).getAsAdjusted<clang::PointerTypeLoc>();
									if (pointerTypeLoc) {
										auto l_pointee_typeLoc = definition_TypeLoc(pointerTypeLoc.getPointeeLoc());

										auto functionProtoTypeLoc = definition_TypeLoc(l_pointee_typeLoc).getAsAdjusted<clang::FunctionProtoTypeLoc>();
										if (functionProtoTypeLoc) {
											auto parens_SR = state1_ptr
												? cm1_adj_nice_source_range(functionProtoTypeLoc.getParensRange(), *state1_ptr, Rewrite)
												: cm1_nice_source_range(functionProtoTypeLoc.getParensRange(), Rewrite);
											if (parens_SR.isValid()) {
												state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(parens_SR), post_name_suffix_str);
											}
										}
									} else {
										int q = 3;
									}
								}
							}
						}
					}

				}

				cumulative_prefix_str = prefix_str + cumulative_prefix_str;
				cumulative_suffix_str = cumulative_suffix_str + suffix_str;
				cumulative_post_name_suffix_str = post_name_suffix_str + cumulative_post_name_suffix_str;

				changed_from_original |= l_changed_from_original;
			}
		}
		retval.m_prefix_str = cumulative_prefix_str;
		retval.m_suffix_str = cumulative_suffix_str;
		retval.m_post_name_suffix_str = cumulative_post_name_suffix_str;
		retval.m_changed_from_original = changed_from_original;

		auto direct_qtype_str = indirection_state_stack.m_direct_type_state.current_return_qtype_str();

		if (retval.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally) {
			/* The original type seems to involve a template parameter (and so the 
			declaration is presumably in the body of a template). We don't want to
			replace the original (presumably) dependent type with direct_qtype_str 
			which may refer to a specific specialization of the originally expressed 
			type. */
			if ("" != retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str) {
				/* The name of a template parameter that encompasses the direct type has been provided. We'll 
				use it in place of the direct type. Note that the given template parameter may actually 
				encompass more than just the direct type, but any given prefixes and/or suffixes are presumably 
				adjusted to compensate. */
				direct_qtype_str = retval.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str;
			} else {
				/* For some reason, a template parameter that encompasses the direct type was not provided. So we'll 
				use the original expression of the type in the source text, if that's available. */
				auto l_original_source_text = indirection_state_stack.m_direct_type_state.original_source_text();
				if ("" != l_original_source_text) {
					direct_qtype_str = l_original_source_text;
				}
			}
		}

		retval.m_complete_type_str = retval.m_prefix_str + direct_qtype_str + retval.m_suffix_str;

		return retval;
	}

	/* This function will modify the source text to reflect any currently indicated changes to the
	declaration. */
	static CTypeIndirectionPrefixAndSuffixItem type_indirection_prefix_and_suffix_modifier(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, CTUState* state1_ptr, EIsFunctionParam is_a_function_parameter, std::optional<clang::StorageDuration> maybe_storage_duration = {}) {

			return type_indirection_prefix_and_suffix_modifier_and_code_generator(indirection_state_stack, Rewrite, is_a_function_parameter, maybe_storage_duration, ESuppressModifications::No, state1_ptr);
	}

	/* This function will return an information object that can be used to construct a text string 
	of the currently indicated replacement type. */
	static CTypeIndirectionPrefixAndSuffixItem generate_type_indirection_prefix_and_suffix(CIndirectionStateStack& indirection_state_stack,
			Rewriter &Rewrite, EIsFunctionParam is_a_function_parameter, std::optional<clang::StorageDuration> maybe_storage_duration = {}, CTUState* state1_ptr = nullptr) {

		return type_indirection_prefix_and_suffix_modifier_and_code_generator(indirection_state_stack, Rewrite, is_a_function_parameter, maybe_storage_duration, ESuppressModifications::Yes, state1_ptr);
	}

	inline std::string generate_qtype_replacement_code(clang::QualType qtype, Rewriter &Rewrite, CTUState* state1_ptr = nullptr, EIsFunctionParam is_a_function_parameter = EIsFunctionParam::No, std::optional<clang::StorageDuration> maybe_storage_duration = {}, std::optional<const clang::Decl*> maybe_containing_D = {}, std::optional<clang::TypeLoc> maybe_typeLoc = {}) {
		CIndirectionStateStack indirection_state_stack;
		indirection_state_stack.m_maybe_containing_D = maybe_containing_D;

		IF_DEBUG(std::string qtype_str = qtype.getAsString();)
		auto direct_qtype = populateQTypeIndirectionStack(indirection_state_stack, qtype, maybe_typeLoc, &Rewrite, state1_ptr);
		indirection_state_stack.m_direct_type_state.set_original_qtype(direct_qtype);

		for (auto& indirection_state_ref : indirection_state_stack) {
			indirection_state_ref.set_xscope_eligibility(false);
		}
		indirection_state_stack.m_direct_type_state.set_xscope_eligibility(false);

		auto res1 = type_indirection_prefix_and_suffix_modifier_and_code_generator(indirection_state_stack, Rewrite, is_a_function_parameter, maybe_storage_duration, ESuppressModifications::Yes, state1_ptr);
		if (res1.m_direct_type_must_be_non_const) {
			direct_qtype.removeLocalConst();
			indirection_state_stack.m_direct_type_state.set_current_qtype(direct_qtype);
		}

		bool no_indirection = (1 > indirection_state_stack.size());
		/* If the direct type is a function type, then generally we want just the function return type
		without the parameter list, as the parameter list will already be incorporated into the parent
		indirection (suffix). But if there is no indirection, then we can't discard the parameter list. */
		auto direct_type_str = no_indirection
			? indirection_state_stack.m_direct_type_state.current_qtype_str()
			: indirection_state_stack.m_direct_type_state.current_return_qtype_str();

		if (res1.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally) {
			/* The original type seems to involve a template parameter (and so the 
			declaration is presumably in the body of a template). We don't want to
			replace the original (presumably) dependent type with direct_qtype_str 
			which may refer to a specific specialization of the originally expressed 
			type. */
			if ("" != res1.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str) {
				/* The name of a template parameter that encompasses the direct type has been provided. We'll 
				use it in place of the direct type. Note that the given template parameter may actually 
				encompass more than just the direct type, but any given prefixes and/or suffixes are presumably 
				adjusted to compensate. */
				direct_type_str = res1.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str;
			} else {
				/* For some reason, a template parameter that encompasses the direct type was not provided. So we'll 
				use the original expression of the type in the source text, if that's available. */
				auto l_original_source_text = indirection_state_stack.m_direct_type_state.original_source_text();
				if ("" != l_original_source_text) {
					direct_type_str = l_original_source_text;
				}
			}
		}

		std::string retval = res1.m_prefix_str + direct_type_str + res1.m_suffix_str + res1.m_post_name_suffix_str;
		return retval;
	}

	inline std::string params_string_from_qtypes(const std::vector<clang::QualType>& qtypes, Rewriter &Rewrite) {
		std::string retval = "(";
		bool is_first_param = true;
		for (const auto& qtype : qtypes) {
			if (!is_first_param) {
				retval += ", ";
			} else {
				is_first_param = false;
			}
			//retval += qtype.getAsString();
			retval += generate_qtype_replacement_code(qtype, Rewrite, nullptr/*state1_ptr*/, EIsFunctionParam::Yes);
		}
		retval += ")";
		return retval;
	}

	inline std::string params_string_from_qtypes(const std::vector<clang::QualType>& qtypes) {
		std::string retval = "(";
		bool is_first_param = true;
		for (const auto& qtype : qtypes) {
			if (!is_first_param) {
				retval += ", ";
			} else {
				is_first_param = false;
			}
			//retval += qtype.getAsString();
			retval += qtype.getAsString();
		}
		retval += ")";
		return retval;
	}

	inline bool satisfies_restrictions_for_static_storage_duration(clang::QualType qtype) {
		bool satisfies_checks = false;
		static const std::string const_char_star_str = "const char *";
		if ((qtype.isConstQualified()) && (is_async_shareable(qtype))) {
			satisfies_checks = true;
		} else if (qtype.getAsString() == const_char_star_str) {
			/* This isn't technically safe, but presumably this is likely
			to be a string literal, which should be fine, so for now we'll
			let it go. */
			satisfies_checks = true;
		} else {
			const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
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

				if ((type_name1 == mse_rsv_static_immutable_obj_str1)
					|| (type_name1 == std_atomic_str)
					|| (type_name1 == mse_AsyncSharedV2ReadWriteAccessRequester_str)
					|| (type_name1 == mse_AsyncSharedV2ReadOnlyAccessRequester_str)
					|| (type_name1 == mse_TAsyncSharedV2ImmutableFixedPointer_str)
					|| (type_name1 == mse_TAsyncSharedV2AtomicFixedPointer_str)
					) {
					satisfies_checks = true;
				}
			}
		}
		return satisfies_checks;
	}

	inline bool satisfies_restrictions_for_thread_local_storage_duration(clang::QualType qtype) {
		bool satisfies_checks = satisfies_restrictions_for_static_storage_duration(qtype);
		if (!satisfies_checks) {
			const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
			if (CXXRD) {
				auto type_name1 = CXXRD->getQualifiedNameAsString();
				const auto tmplt_CXXRD = CXXRD->getTemplateInstantiationPattern();
				if (tmplt_CXXRD) {
					type_name1 = tmplt_CXXRD->getQualifiedNameAsString();
				}

				DECLARE_CACHED_CONST_STRING(mse_rsv_ThreadLocalObj_str, mse_namespace_str() + "::rsv::TThreadLocalObj");
				if (type_name1 == mse_rsv_ThreadLocalObj_str) {
					satisfies_checks = true;
				}
			}
			if (!satisfies_checks) {
				if (is_async_shareable(qtype)) {
					satisfies_checks = true;
				}
			}
		}
		return satisfies_checks;
	}

	enum class ESuppressComment { No, Yes };

	inline auto default_init_value_str(const clang::QualType& qtype, ESuppressComment suppress_comment = ESuppressComment::No) {
		std::string retval;
		std::string comment1_str = (ESuppressComment::Yes == suppress_comment) ? "" : "/*auto-generated init val*/";

		std::string qtype_str = qtype.getAsString();
		if (qtype.getTypePtr()->isScalarType()) {
			std::string initializer_info_str;
			if (qtype.getTypePtr()->isEnumeralType()) {
				if (std::string::npos != qtype_str.find(" (unnamed ")) {
					/* It appears to be an unnamed type, so replacing the declaration with one that tries
					to explicitly specify the type probably isn't going to work. One strategy might be to 
					give the type a name, but for now I think we're just gonna bail. */
					//initializer_info_str += "0" + comment1_str;
					initializer_info_str = "";
				} else {
					static const auto enum_space_str = std::string("enum ");
					if (string_begins_with(qtype_str, enum_space_str)) {
						qtype_str = qtype_str.substr(enum_space_str.length());
					}
					if ("Dual" == ConvertMode) {
						initializer_info_str += "MSE_LH_CAST(";
						initializer_info_str += qtype_str;
						initializer_info_str += ", 0)" + comment1_str;
					} else {
						initializer_info_str += qtype_str;
						initializer_info_str += "(0)" + comment1_str;
					}
				}
			} else if (qtype.getTypePtr()->isPointerType()) {
				if ("Dual" == ConvertMode) {
					initializer_info_str += "MSE_LH_NULL_POINTER" + comment1_str;
				} else {
					initializer_info_str += "nullptr" + comment1_str;
				}
			} else {
				initializer_info_str += "0" + comment1_str;
			}
			retval = initializer_info_str;
		} else {
			retval = qtype_str + "()";
		}

		return retval;
	}

	static CDeclarationReplacementCodeItem declaration_modifier_helper1(const DeclaratorDecl* DD,
			Rewriter &Rewrite, CTUState* state1_ptr, CDDeclConversionStateMap& ddecl_conversion_state_map, ESuppressModifications suppress_modifications = ESuppressModifications::No, std::string options_str = "") {
		CDeclarationReplacementCodeItem retval;

		if (!DD) {
			return retval;
		}
		
		auto decl_source_range = state1_ptr
			? cm1_adj_nice_source_range(DD->getSourceRange(), *state1_ptr, Rewrite)
			: cm1_nice_source_range(DD->getSourceRange(), Rewrite);
		if (!(decl_source_range.isValid())) {
			return retval;
		}

		auto res1 = ddecl_conversion_state_map.insert(*DD, &Rewrite, state1_ptr);
		auto ddcs_map_iter = res1.first;
		auto& ddcs_ref = (*ddcs_map_iter).second;

		auto qtype = DD->getType();
		auto qtype_str = adjusted_qtype_str(DD->getType().getAsString());

		const clang::FunctionDecl* FND = nullptr;
		bool type_is_function_type = false;
		if (DD->getType()->isFunctionType()) {
			FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				type_is_function_type = true;
			}
		}

		bool changed_from_original = false;
		std::string variable_name = DD->getNameAsString();
		if (ddcs_ref.m_maybe_updated_name.has_value()) {
			variable_name = ddcs_ref.m_maybe_updated_name.value();
			changed_from_original = true;
		}
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
		bool has_static_storage_class = false;
		bool is_a_function_parameter = false;
		bool is_member = false;
		bool is_vardecl = false;
		std::string initialization_expr_str{ ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr) };
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

			has_static_storage_class = (clang::StorageClass::SC_Static == storage_class);
			if ((clang::StorageDuration::SD_Static == storage_duration) && (!has_static_storage_class)) {
				int q = 5;
			}
			is_a_function_parameter = (VD->isLocalVarDeclOrParm() && (!VD->isLocalVarDecl()));

			if (!ddcs_ref.m_original_initialization_has_been_noted) {
				if (VD->hasInit()) {
					auto pInitExpr = VD->getInit();
					if (pInitExpr) {
						auto init_expr_source_range = state1_ptr
						? cm1_adj_nice_source_range(pInitExpr->getSourceRange(), *state1_ptr, Rewrite)
						: cm1_nice_source_range(pInitExpr->getSourceRange(), Rewrite);

						/* It seems that getInit() will return the initalization expression even if it
						was expressed in another redeclaration of the variable rather than this declaration.
						We're using source ranges to determine whether the initializtion expression is
						part of this declaration because it's not immediately clear how else to do it. */
						auto init_expr_located_in_this_decl = first_is_a_subset_of_second(init_expr_source_range, decl_source_range);

						if (init_expr_source_range.isValid() && init_expr_located_in_this_decl) {
							initialization_expr_str = Rewrite.getRewrittenText(init_expr_source_range);
							if (variable_name == initialization_expr_str) {
								/* We encountered a weird bug where the initialization expression sometimes
								* was indicated as being present and the source range set to the variable name
								* when actually no initialization expression was present in the original source. */
								initialization_expr_str = "";
							} else {
								ddcs_ref.m_initializer_SR_or_insert_before_point = init_expr_source_range;
								ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
								if (ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr).empty()) {
									if (state1_ptr) {
										ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, Rewrite, *state1_ptr));
									}
									ddcs_ref.m_fallback_current_initialization_expr_str = initialization_expr_str;
								} else {
									initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
								}
							}
						} else {
							int q = 5;
						}
					} else {
						int q = 3;
					}
				} else {
					if (qtype.getTypePtr()->isScalarType()) {
						auto PVD = dyn_cast<const ParmVarDecl>(VD);
						if (!PVD) {
							if (!VD->isExternallyDeclarable()) {
								{
									/* Here we're adding a missing initialization value to the variable declaration. */
									auto l_DD = VD;

									std::string initializer_info_str = default_init_value_str(qtype);
									ddcs_ref.m_fallback_current_initialization_expr_str = initializer_info_str;

									if (ConvertToSCPP && (ESuppressModifications::No == suppress_modifications) && state1_ptr) {
										/* Specify that the new initialization string should be
										inserted at the end of the declaration. */
										//ddcs_ref.m_maybe_embedded_initializer_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
										ddcs_ref.m_initializer_SR_or_insert_before_point = decl_source_range.getEnd().getLocWithOffset(+1);
									}
								}
							} else {
								/* todo: emit error that (uninitialized) 'extern' variables
								aren't supported?  */;
							}
						}
					}
				}

				ddcs_ref.m_original_initialization_has_been_noted = true;
			}

			if (ConvertToSCPP && (ESuppressModifications::No == suppress_modifications) && state1_ptr) {
				auto& SR = decl_source_range;
				auto qtype = VD->getType();
				//const auto storage_duration = VD->getStorageDuration();
				const auto var_qualified_name = VD->getQualifiedNameAsString();

				if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
					bool satisfies_checks = satisfies_restrictions_for_static_storage_duration(qtype);
					if (!satisfies_checks) {
						if (clang::StorageDuration::SD_Static == storage_duration) {
							if (!ddcs_ref.m_has_been_replaced_as_a_whole) {
								/* Here we're (unjustifiably) assuming that the program is single threaded 
								and changing variables with static duration to thread_local duration. */
								std::string l_source_text1 = Rewrite.getRewrittenText(SR);
								std::size_t replace_pos = 0;
								std::size_t replace_length = 0;
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

								static const std::string thread_local_specifier_str = "thread_local ";
								if (1 <= replace_length) {
									(*state1_ptr).m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range({ SR.getBegin().getLocWithOffset(replace_pos), SR.getBegin().getLocWithOffset(replace_pos + replace_length - 1) }), thread_local_specifier_str);
									ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = clang::SourceRange(SR.getBegin().getLocWithOffset(replace_pos), SR.getBegin().getLocWithOffset(replace_pos + replace_length - 1));
								} else {
									const auto& insert_before_point = SR.getBegin().getLocWithOffset(replace_pos);
									(*state1_ptr).m_pending_code_modification_actions.add_insert_before_given_location_action(Rewrite, write_once_source_range({ insert_before_point, insert_before_point.getLocWithOffset(-1) }), insert_before_point, thread_local_specifier_str);
									ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = SR.getBegin().getLocWithOffset(replace_pos);
								}

								int q = 5;
							}
						}
					}
				}
			}

		} else if (FD) {
			{
				is_member = true;

				if (!ddcs_ref.m_original_initialization_has_been_noted) {
					if (FD->hasInClassInitializer()) {
						auto pInitExpr = FD->getInClassInitializer();
						if (pInitExpr) {
							auto init_expr_source_range = state1_ptr
								? cm1_adj_nice_source_range(pInitExpr->getSourceRange(), *state1_ptr, Rewrite)
								: cm1_nice_source_range(pInitExpr->getSourceRange(), Rewrite);
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
									if (ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr).empty()) {
										if (state1_ptr) {
											ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, Rewrite, *state1_ptr));
										}
										ddcs_ref.m_fallback_current_initialization_expr_str = initialization_expr_str;
									} else {
										initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
									}
								}
							} else {
								int q = 3;
							}
						} else {
							int q = 3;
						}
					} else {
						if (qtype.getTypePtr()->isScalarType()) {
							const auto* init_EX = FD->getInClassInitializer();
							if (!init_EX) {
								const auto parent_RD = FD->getParent();

								bool is_lambda_capture_field = false;
								assert(parent_RD);
								if (llvm::isa<CXXRecordDecl>(parent_RD)) {
									const auto CXXRD = llvm::cast<CXXRecordDecl>(parent_RD);
									assert(CXXRD);
									is_lambda_capture_field = CXXRD->isLambda();
								}

								bool is_implicit = false;
								if (FD->getSourceRange() == parent_RD->getSourceRange()) {
									/* If the FieldDecl has the same source location as its parent CXXRecordDecl,
									then we're going to assume that the FieldDecl is some implicit declaration
									(that doesn't concern us) (such as an implicit lambda capture). We're doing
									this check for now because we don't know the proper way to determine if this
									declaration is implicit or not. */
									is_implicit = true;
								}

								if ((!is_lambda_capture_field) && (!is_implicit)) {
									if (qtype.getTypePtr()->isPointerType()) {
									} else {
										{
											/* Here we're adding a missing initialization value to the field declaration. */
											auto l_DD = FD;

											std::string initializer_info_str;
											initializer_info_str += default_init_value_str(qtype);
											ddcs_ref.m_fallback_current_initialization_expr_str = initializer_info_str;

											if (!(ddcs_ref.m_has_been_replaced_as_a_whole)) {
												/* Specify that the new initialization string should be
												inserted at the end of the declaration. */
												ddcs_ref.m_initializer_SR_or_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
											}
										}
									}
								}
							}
						}
					}

					ddcs_ref.m_original_initialization_has_been_noted = true;
				}
			}
		}
		ddcs_ref.m_original_initialization_has_been_noted = true;

		std::string replacement_code;
		std::string replacement_type_str;
		std::string replacement_return_type_str;
		std::string replacement_return_type_post_params_suffix_str;
		std::string prefix_str;
		std::string suffix_str;
		std::string post_name_suffix_str;

		EIsFunctionParam is_a_function_parameter_enum = is_a_function_parameter ? EIsFunctionParam::Yes : EIsFunctionParam::No;

		auto res4 = type_indirection_prefix_and_suffix_modifier_and_code_generator(ddcs_ref.m_indirection_state_stack,
				Rewrite, is_a_function_parameter_enum, ddcs_ref.m_maybe_current_storage_duration, suppress_modifications, state1_ptr);

		retval.m_action_species = res4.m_action_species;


		bool no_indirection = (1 > ddcs_ref.m_indirection_state_stack.size());
		/* If the direct type is a function type, then generally we want just the function return type
		without the parameter list, as the parameter list will already be incorporated into the parent
		indirection (suffix). But if there is no indirection, then we can't discard the parameter list. */
		auto direct_qtype_str = no_indirection
			? adjusted_qtype_str(ddcs_ref.current_direct_qtype_str())
			: adjusted_qtype_str(ddcs_ref.current_direct_return_qtype_str());
		auto non_const_direct_qtype_str = no_indirection
			? adjusted_qtype_str(ddcs_ref.non_const_current_direct_qtype_str())
			: adjusted_qtype_str(ddcs_ref.non_const_current_direct_return_qtype_str());

		if (res4.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally) {
			/* The original type seems to involve a template parameter (and so the 
			declaration is presumably in the body of a template). We don't want to
			replace the original (presumably) dependent type with direct_qtype_str 
			which may refer to a specific specialization of the originally expressed 
			type. */
			if ("" != res4.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str) {
				/* The name of a template parameter that encompasses the direct type has been provided. We'll 
				use it in place of the direct type. Note that the given template parameter may actually 
				encompass more than just the direct type, but any given prefixes and/or suffixes are presumably 
				adjusted to compensate. */
				direct_qtype_str = res4.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str;
				non_const_direct_qtype_str = res4.m_tparam_or_nonmodifiable_type_that_encompasses_the_direct_type_str;
			} else {
				/* For some reason, a template parameter that encompasses the direct type was not provided. So we'll 
				use the original expression of the type in the source text, if that's available. */
				auto l_original_source_text = ddcs_ref.direct_type_state_ref().original_source_text();
				if ("" != l_original_source_text) {
					direct_qtype_str = l_original_source_text;
					non_const_direct_qtype_str = l_original_source_text;
				}
			}
		}

		if (!(ddcs_ref.m_original_source_text_has_been_noted)) {
			ddcs_ref.m_original_source_text_str = Rewrite.getRewrittenText(decl_source_range);
			if (FND) {
				assert(type_is_function_type);
				ddcs_ref.m_is_a_function = true;
				for (size_t i = 0; i < FND->getNumParams(); i+=1) {
					ddcs_ref.m_original_function_parameter_decl_cptrs.push_back(FND->getParamDecl(i));
				}
				auto return_type_source_range = state1_ptr
					? cm1_adj_nice_source_range(FND->getReturnTypeSourceRange(), *state1_ptr, Rewrite)
					: cm1_nice_source_range(FND->getReturnTypeSourceRange(), Rewrite);
				if (!(return_type_source_range.isValid())) {
					return retval;
				}
				auto name_SL = state1_ptr
					? cm1_adj_nice_source_location(FND->getLocation(), *state1_ptr, Rewrite)
					: cm1_nice_source_location(FND->getLocation(), Rewrite);
				if ((return_type_source_range.getEnd() < name_SL) || (name_SL < return_type_source_range.getBegin())) {
					ddcs_ref.m_function_return_type_original_source_text_str = Rewrite.getRewrittenText(return_type_source_range);
				} else {
					/* The return type source range seems to encompass the function name. Like maybe,
					for example, if the return type is a pointer to a (native) array? */
					int q = 5;
				}
			}
			ddcs_ref.m_original_source_text_has_been_noted = true;
		}

		bool const_qualifier_stripped_from_direct_qtype_str = false;
		if (res4.m_direct_type_must_be_non_const) {
			if (direct_qtype_str != non_const_direct_qtype_str) {
				const_qualifier_stripped_from_direct_qtype_str = true;
				direct_qtype_str = non_const_direct_qtype_str;
			}
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

			auto seems_to_already_be_addressable = [](std::string_view sv) {
				if ((string_begins_with(sv, "mse::TRegisteredObj<"))
					|| (string_begins_with(sv, "MSE_LH_ADDRESSABLE_TYPE("))
					|| (string_begins_with(sv, "const mse::TRegisteredObj<"))
					|| (string_begins_with(sv, "const MSE_LH_ADDRESSABLE_TYPE("))) {

					return true;
				}
				return false;
			};

			if ((true || !is_char_type) && (!is_FILE_type) && (!is_void_type)) {
				if ((REGOBJ_TEST1_FLAG || ("pointer target" == ddcs_ref.direct_type_state_ref().current_pointer_target_state()))) {
					bool b1 = !seems_to_already_be_addressable(ddcs_ref.current_direct_qtype_str());
					if (b1) {
						if ((0 == ddcs_ref.m_indirection_state_stack.size())
							&& (ddcs_ref.m_ddecl_cptr->getType()->isRecordType())
							&& (string_begins_with(ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr), "{"))) {
						
							/* "Aggregate" types that are converted to "addressable" types will lose their
							"aggregate" status and thus their support for aggregate initialization. So if
							the object was being aggregate initialized, then we'll use the initializer list
							to initialize a temporary object of the base aggregate type, which can in turn
							be used to initialize the "addressable" object. So for example:

							some_struct_t obj1 = { 1, "abc", 3 };

							becomes:

							mse::TRegisteredObj<some_struct_t> obj1 = some_struct_t { 1, "abc", 3 };

							*/

							const clang::Expr* init_EX = nullptr;
							if (VD) {
								init_EX = VD->getInit();
							} else if (FD) {
								init_EX = FD->getInClassInitializer();
							}

							if (init_EX) {
								auto ILE = dyn_cast<const clang::InitListExpr>(init_EX);
								if (ILE) {
									std::string new_init_prefix_str;
									if ("Dual" == ConvertMode) {
										new_init_prefix_str = "MSE_LH_IF_ENABLED("
											+ ddcs_ref.non_const_current_direct_qtype_str() + ") ";
									} else {
										new_init_prefix_str = ddcs_ref.non_const_current_direct_qtype_str() + " ";
									}
									if (state1_ptr) {
										auto& state1 = *state1_ptr;
										ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(init_EX, Rewrite, state1));

										auto& ecs_ref = state1.get_expr_conversion_state_ref<CExprConversionState>(*init_EX, Rewrite);
										const auto l_text_modifier = CWrapExprTextModifier(new_init_prefix_str, "");
										bool seems_to_be_already_applied = ((1 <= ecs_ref.m_expr_text_modifier_stack.size()) && ("wrap" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) 
											&& (l_text_modifier.is_equal_to(*(ecs_ref.m_expr_text_modifier_stack.back()))));
										if (!seems_to_be_already_applied) {
											auto shptr2 = std::make_shared<CWrapExprTextModifier>(new_init_prefix_str, "");
											ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
											ecs_ref.update_current_text();

											/* We shouldn't need to add an "expression_update_replacement_action" here as the updated expression 
											text should be rendered when the parent declaration text gets rendered. */
											//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, init_EX_SR, state1, init_EX);
											//state1.add_pending_straight_text_replacement_expression_update(*init_EX, Rewrite, new_init_expr_str);
										}
									}
									ddcs_ref.m_fallback_current_initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
									initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
								}
							}
						}
					}

					bool b2 = !seems_to_already_be_addressable(direct_qtype_str);
					if (b1 || b2) {
						std::string addr_prefix;
						std::string addr_suffix;

						if ("Dual" == ConvertMode) {
							addr_prefix = "MSE_LH_ADDRESSABLE_TYPE(";
							addr_suffix = ")";
						} else if ("FasterAndStricter" == ConvertMode) {
						} else {
							addr_prefix = "mse::TRegisteredObj<";
							addr_suffix = " >";
						}
						if (!("" == addr_prefix)) {
							direct_qtype_str = addr_prefix + non_const_direct_qtype_str + addr_suffix;
							if (ddcs_ref.direct_type_state_ref().is_const()) {
								direct_qtype_str = "const " + direct_qtype_str;
							}
							ddcs_ref.set_current_direct_non_function_qtype_str(direct_qtype_str);
						}
					}
				} else {
					int q = 5;
				}
			}
		}

		bool discard_initializer_option_flag = (std::string::npos != options_str.find("[discard-initializer]"));
		std::string initializer_append_str;
		if ((!discard_initializer_option_flag) && ("" != initialization_expr_str)) {
			if (("Dual" == ConvertMode) && ("" == ddcs_ref.m_original_initialization_expr_str)) {
				initializer_append_str = " MSE_LH_IF_ENABLED( = " + initialization_expr_str + " )";
			} else {
				initializer_append_str = " = " + initialization_expr_str;
			}
		}

		//if (("" != prefix_str) || ("" != suffix_str)/* || ("" != post_name_suffix_str)*/)
		if (res4.m_changed_from_original || ddcs_ref.direct_qtype_has_been_changed()) {
			changed_from_original = true;
		} else if (ddcs_ref.initializer_has_been_changed(Rewrite, state1_ptr) || (discard_initializer_option_flag)) {
			changed_from_original = true;
		}

		bool is_thread_local = (clang::StorageDuration::SD_Thread == storage_duration);
		if ((!is_thread_local) && VD && (clang::StorageDuration::SD_Static == storage_duration)) {
			if (!satisfies_restrictions_for_static_storage_duration(VD->getType())) {
				/* was originally static storage duration, but needs to be converted to thread_local */
				is_thread_local = true;
				changed_from_original = true;
			}
		}

		bool individual_from_compound_declaration = false;
		if (true && (2 <= IndividualDeclaratorDecls(DD, Rewrite).size())) {
			/* There is more than one declaration in the declaration statement. We split
			* them so that each has their own separate declaration statement. This counts
			* as a change from the original source code. */
			individual_from_compound_declaration = true;
		}

		if (FND) {
			assert(type_is_function_type);
			if (changed_from_original || individual_from_compound_declaration || ddcs_ref.m_function_return_type_original_source_text_str.empty()) {
				std::string effective_current_direct_return_qtype_str = ddcs_ref.current_direct_return_qtype_str();

				if (res4.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally) {
					/* In this case the direct type should have already been set to refer to the (effective direct 
					type of the) return type. */
					effective_current_direct_return_qtype_str = direct_qtype_str;
				}

				replacement_return_type_str += prefix_str + effective_current_direct_return_qtype_str + suffix_str;
				replacement_return_type_post_params_suffix_str = post_name_suffix_str;
			} else {
				replacement_return_type_str = ddcs_ref.m_function_return_type_original_source_text_str;
			}
		}
		{
			if (changed_from_original || individual_from_compound_declaration) {
				if (is_extern) {
					if ("" == ddcs_ref.m_original_initialization_expr_str) {
						replacement_code += "extern ";
					}
				}
				if (is_thread_local) {
					replacement_code += "thread_local ";
				}
				if (VD && (!VD->isFileVarDecl())) {
					if ((clang::StorageDuration::SD_Static == storage_duration) && (!is_thread_local)) {
						replacement_code += "static ";
					}
				} else {
					if (has_static_storage_class) {
						assert(!is_extern);
						replacement_code += "static ";
					}
				}
				if (res4.m_just_a_native_array && (!res4.m_some_addressable_indirection)) {
					if (const_qualifier_stripped_from_direct_qtype_str) {
						replacement_code += "const ";
					}
					if ("Dual" == ConvertMode) {
						replacement_code += "MSE_LH_FIXED_ARRAY_DECLARATION(" + direct_qtype_str;
						replacement_code += ", " + res4.m_native_array_size_text;
						replacement_code += ", " + variable_name + ")";
					} else if ("FasterAndStricter" == ConvertMode) {
						replacement_code += "mse::TXScopeObj<mse::nii_array<" + direct_qtype_str;
						replacement_code += ", " + res4.m_native_array_size_text;
						replacement_code += "> " + variable_name;
					} else {
						replacement_code += "mse::lh::TNativeArrayReplacement<" + direct_qtype_str;
						replacement_code += ", " + res4.m_native_array_size_text;
						replacement_code += "> " + variable_name;
					}

					const clang::Expr* init_EX = nullptr;
					if (VD) {
						init_EX = VD->getInit();
					} else if (FD) {
						init_EX = FD->getInClassInitializer();
					}

					if (init_EX) {
						auto ILE = dyn_cast<const clang::InitListExpr>(init_EX);
						if (ILE) {
							bool add_std_array_intermediary = false;

							size_t num_init_elements_rough_estimate = std::count(initializer_append_str.begin(), initializer_append_str.end(), ',');
							if (64/* arbitrary */ < num_init_elements_rough_estimate) {
								/* The SaferCPlusPlus arrays emulate aggregate initialization (at compile-time). Some
								compilers may have difficulty handling large initalizer lists. So we insert an
								intermediate std::array<>. */
								add_std_array_intermediary = true;
							} else if (1 <= ILE->getNumInits()) {
								auto init_item_E = ILE->getInit(0);
								if (init_item_E) {
									auto nested_ILE = dyn_cast<const clang::InitListExpr>(init_item_E);
									if (nested_ILE) {
										add_std_array_intermediary = true;
									}
								}
							}
							if (add_std_array_intermediary) {

								std::string initializer_prefix = "std::array<";
								initializer_prefix += direct_qtype_str;
								initializer_prefix += ", " + res4.m_native_array_size_text;
								initializer_prefix += " > { ";
								std::string initializer_suffix = " }";

								std::string new_init_prefix_str;
								if ("Dual" == ConvertMode) {
									new_init_prefix_str = "MSE_LH_IF_ENABLED("
										+ initializer_prefix + ") ";
								} else {
									new_init_prefix_str = initializer_prefix;
								}
								std::string new_init_suffix_str;
								if ("Dual" == ConvertMode) {
									new_init_suffix_str = "MSE_LH_IF_ENABLED("
										+ initializer_suffix + ") ";
								} else {
									new_init_suffix_str = initializer_suffix;
								}
								if (state1_ptr) {
									auto& state1 = *state1_ptr;
									ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(init_EX, Rewrite, state1));

									auto& ecs_ref = state1.get_expr_conversion_state_ref<CExprConversionState>(*init_EX, Rewrite);
									const auto l_text_modifier = CWrapExprTextModifier(new_init_prefix_str, new_init_suffix_str);
									bool seems_to_be_already_applied = ((1 <= ecs_ref.m_expr_text_modifier_stack.size()) && ("wrap" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) 
										&& (l_text_modifier.is_equal_to(*(ecs_ref.m_expr_text_modifier_stack.back()))));
									if (!seems_to_be_already_applied) {
										auto shptr2 = std::make_shared<CWrapExprTextModifier>(new_init_prefix_str, new_init_suffix_str);
										ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
										ecs_ref.update_current_text();

										/* We shouldn't need to add an "expression_update_replacement_action" here as the updated expression 
										text should be rendered when the parent declaration text gets rendered. */
										//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, init_EX_SR, state1, init_EX);
										//state1.add_pending_straight_text_replacement_expression_update(*init_EX, Rewrite, new_init_expr_str);
									}
								}
								ddcs_ref.m_fallback_current_initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
								initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
							}
						}
					}
				} else {
					if (std::string::npos != direct_qtype_str.find(" (unnamed ")) {
						/* It appears to be an unnamed type, so replacing the declaration with one that tries
						to explicitly specify the type probably isn't going to work. One strategy might be to 
						give the type a name, but for now I think we're just gonna bail. */
						replacement_code = FND ? ddcs_ref.m_function_return_type_original_source_text_str
							: ddcs_ref.m_original_source_text_str;
						initializer_append_str = "";
					} else {
						bool is_enum_decl = false;
						if ((std::string::npos != ddcs_ref.m_original_source_text_str.find("{"))
							&& (std::string::npos != direct_qtype_str.find("enum"))) {
							/* The variable or field declaration might also be defining an enum type. 
							(todo: check for this properly)
							Our generated replacement wouoldn't (yet) preserve such an enam definition, so
							at least for now we'll just stick with the original. */
							replacement_code = ddcs_ref.m_original_source_text_str;
						} else {
							replacement_code += prefix_str + direct_qtype_str + suffix_str;
							replacement_code += " ";
							replacement_code += variable_name;
							replacement_code += post_name_suffix_str;
						}
					}
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
					auto directTypeSR = state1_ptr
						? cm1_adj_nice_source_range(directTypeLoc.getSourceRange(), *state1_ptr, Rewrite)
						: cm1_nice_source_range(directTypeLoc.getSourceRange(), Rewrite);
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

			if (ConvertToSCPP && (ESuppressModifications::No == suppress_modifications) && state1_ptr) {
				if ((ddcs_ref.direct_qtype_has_been_changed() /*|| ("" != ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_function_type_state.m_params_current_str)*/)
					&& (!(res4.m_seems_to_involve_a_template_param_or_nonmodifiable_type_originally))) {

					bool no_indirection = (1 > ddcs_ref.m_indirection_state_stack.size());
					/* If the direct type is a function type, then generally we want just the function return type
					without the parameter list, as the parameter list will already be incorporated into the parent
					indirection (suffix). But if there is no indirection, then we can't discard the parameter list. */
					auto direct_qtype_str = no_indirection
						? adjusted_qtype_str(ddcs_ref.current_direct_qtype_str())
						: adjusted_qtype_str(ddcs_ref.current_direct_return_qtype_str());

					auto& direct_type_state = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
					if (direct_type_state.m_maybe_source_range_including_any_const_qualifier.has_value()) {
						auto& cq_direct_type_SR = direct_type_state.m_maybe_source_range_including_any_const_qualifier.value();
						//TheRewriter.ReplaceText(cq_direct_type_SR, direct_qtype_str);
						//m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(cq_direct_type_SR);
						(*state1_ptr).m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(cq_direct_type_SR), direct_qtype_str);
					}
				}

				{
					auto initializer_SR_ptr = std::get_if<clang::SourceRange>(&(ddcs_ref.m_initializer_SR_or_insert_before_point));
					auto insert_before_point_ptr = std::get_if<clang::SourceLocation>(&(ddcs_ref.m_initializer_SR_or_insert_before_point));
					if (initializer_SR_ptr) {
						if (true || !(*state1_ptr).m_pending_code_modification_actions.m_already_modified_regions.properly_contains(*initializer_SR_ptr)) {
							if (ddcs_ref.initializer_has_been_changed(Rewrite, state1_ptr)) {
								std::string initializer_str = ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
								//TheRewriter.ReplaceText(*initializer_SR_ptr, initializer_str);
								//m_tu_state.m_pending_code_modification_actions.m_already_modified_regions.insert(*initializer_SR_ptr);
								(*state1_ptr).m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, write_once_source_range(*initializer_SR_ptr), initializer_str);
							}
						} else {
							int q = 5;
						}
					} else if (insert_before_point_ptr) {
						const auto insert_after_point = (*insert_before_point_ptr).getLocWithOffset(-1);
						if (true || !(*state1_ptr).m_pending_code_modification_actions.m_already_modified_regions.contains({ insert_after_point, *insert_before_point_ptr })) {
							std::string initializer_str;
							if ("Dual" == ConvertMode) {
								initializer_str = " MSE_LH_IF_ENABLED( = " + ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr) + " )";
							} else {
								initializer_str = " = " + ddcs_ref.current_initialization_expr_str(Rewrite, state1_ptr);
							}
							//TheRewriter.InsertTextAfterToken(insert_after_point, initializer_str);
							(*state1_ptr).m_pending_code_modification_actions.add_insert_after_token_at_given_location_action(Rewrite, write_once_source_range({ (*insert_before_point_ptr), insert_after_point }), insert_after_point, initializer_str);
						} else {
							int q = 5;
						}
					}
				}
			}
		}

		retval.m_replacement_code = replacement_code;
		retval.m_replacement_type_str = replacement_type_str;
		retval.m_replacement_return_type_str = replacement_return_type_str.empty() ? replacement_type_str : replacement_return_type_str;
		retval.m_replacement_return_type_post_params_suffix_str = replacement_return_type_post_params_suffix_str;
		retval.m_changed_from_original = changed_from_original;
		retval.m_individual_from_compound_declaration = individual_from_compound_declaration;
		return retval;
	}

	inline CDeclarationReplacementCodeItem generate_declaration_replacement_code(const DeclaratorDecl* DD,
			Rewriter &Rewrite, CTUState* state1_ptr, CDDeclConversionStateMap& ddecl_conversion_state_map, std::string options_str/* = ""*/) {
		return declaration_modifier_helper1(DD, Rewrite, state1_ptr, ddecl_conversion_state_map, ESuppressModifications::Yes, options_str);
	}

	/* Function parameters of function pointer declarations seem to have their own associated
	clang::ParmVarDecls. This means that modification operations associated with the
	clang::ParmVarDecl will be applied to the source code specifying any pointer parameter. But
	those modifications may need to overwritten with modifications of the parameters associated
	with (analysis of operations involving) the function pointer. */
	inline bool ContainsFunctionPointerOfConcernQType(clang::QualType qtype) {
		qtype = ignore_parens_qtype(qtype);
		IF_DEBUG(std::string qtype_str = qtype.getAsString());
		IF_DEBUG(std::string typeClassName = qtype->getTypeClassName();)
		if (qtype->isPointerType()) {
			auto pointee_qtype = qtype->getPointeeType();
			if (pointee_qtype->isFunctionProtoType()) {
				if (llvm::isa<const clang::FunctionProtoType>(pointee_qtype.getTypePtr())) {
					auto FT = llvm::cast<const clang::FunctionProtoType>(pointee_qtype.getTypePtr());
					for (auto param_type : FT->getParamTypes()) {
						if ((param_type->isPointerType())) {
							return true;
						}
					}
				} else {
					int q = 3;
					return true;
				}
			} else {
				return ContainsFunctionPointerOfConcernQType(qtype->getPointeeType());
			}
		}
		return false;
	}
	/* Function parameters of function pointer declarations seem to have their own associated
	clang::ParmVarDecls. This means that modification operations associated with the
	clang::ParmVarDecl will be applied to the source code specifying any pointer parameter. But
	those modifications may need to overwritten with modifications of the parameters associated
	with (analysis of operations involving) the function pointer. */
	inline bool ContainsFunctionPointerOfConcernDecl(const DeclaratorDecl& ddecl) {
		return ContainsFunctionPointerOfConcernQType(ddecl.getType());
	}

	/* With most declarations, the object/variable name follows the type. But declarations of native
	arrays and functions (can) have the object name "surrounded" by the type. In these cases, modifying
	the type (declaration text) in place is more complicated. If a declaration involves more than one
	native array and/or function type (for example a native array of functions), then we'll consider
	it too complicated to modify in place, instead opting to replace the entire declaration. */
	inline bool IsUnwieldyQType(clang::QualType qtype, int num_unwieldy_indirections_encounterred = 0) {
		qtype = ignore_parens_qtype(qtype);
		IF_DEBUG(std::string qtype_str = qtype.getAsString());
		IF_DEBUG(std::string typeClassName = qtype->getTypeClassName();)
		if (1 < num_unwieldy_indirections_encounterred) {
			return true;
		} else  {
			if (qtype->isPointerType()) {
				return IsUnwieldyQType(qtype->getPointeeType(), num_unwieldy_indirections_encounterred);
			} else if (qtype->isArrayType()) {
				num_unwieldy_indirections_encounterred += 1;
				if (llvm::isa<const clang::ArrayType>(qtype.getTypePtr())) {
					auto ATP = llvm::cast<const clang::ArrayType>(qtype.getTypePtr());
					return IsUnwieldyQType(ATP->getElementType(), num_unwieldy_indirections_encounterred);
				} else {
					int q = 3;
				}
			} else if (qtype->isFunctionType()) {
				num_unwieldy_indirections_encounterred += 1;
				if (llvm::isa<const clang::FunctionType>(qtype.getTypePtr())) {
					auto FT = llvm::cast<const clang::FunctionType>(qtype.getTypePtr());
					return IsUnwieldyQType(FT->getReturnType(), num_unwieldy_indirections_encounterred);
				} else {
					int q = 3;
				}
			}
		}
		return false;
	}
	/* With most declarations, the object/variable name follows the type. But declarations of native
	arrays and functions (can) have the object name "surrounded" by the type. In these cases, modifying
	the type (declaration text) in place is more complicated. If a declaration involves more than one
	native array and/or function type (for example a native array of functions), then we'll consider
	it too complicated to modify in place, instead opting to replace the entire declaration. */
	inline bool IsUnwieldyDecl(const DeclaratorDecl& ddecl) {
		return IsUnwieldyQType(ddecl.getType());
	}

	COrderedSourceRange get_potentially_rewritable_source_range(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1) {
		const DeclaratorDecl* DD = &ddecl;
		auto raw_SR = DD->getSourceRange();
		auto unordered_SR = cm1_adj_nice_source_range(raw_SR, state1, Rewrite);
		if (!unordered_SR.isValid()) {
			return raw_SR;
		}
		/* Our theory is that whole statements can be safely overwritten multiple times even with
		replacement texts of differing sizes. */
		bool suspected_to_be_rewritable = true;
		auto PVD = dyn_cast<const clang::ParmVarDecl>(DD);
		if (PVD) {
			/* Unless the declaration is a function parameter. */
			suspected_to_be_rewritable = false;
		} else {
			if (raw_SR.getBegin().isMacroID()) {
				auto& SM = Rewrite.getSourceMgr();
				if (SM.isMacroBodyExpansion(raw_SR.getBegin())) {
					/* Or seems to be in the body of a macro definition. */
					suspected_to_be_rewritable = false;
				}
			}
		}

		/* Though I think that we may no longer depend on any source range being rewritable. */
		return suspected_to_be_rewritable ? rewritable_source_range(unordered_SR) : write_once_source_range(unordered_SR);
	}

	static void declaration_modifier(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, std::string options_str = "") {
		const DeclaratorDecl* DD = &ddecl;

		QualType QT = DD->getType();
		const clang::Type* TP = QT.getTypePtr();
		IF_DEBUG(auto qtype_str = QT.getAsString();)

		auto SR = get_potentially_rewritable_source_range(ddecl, Rewrite, state1);
		RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		auto& SM = Rewrite.getSourceMgr();

		IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

		if (filtered_out_by_location<options_t<converter_mode_t> >(SM, SR.getBegin())) {
			return void();
		}

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto suppress_check_flag = false;
		if (state1.m_ast_context_ptr) {
			/* Generally, we wouldn't expect to get here because any element in a "check suppressed" region 
			should have been weeded out in the calling function. But we ran into a case where a non-template
			overload of a template function was defined in a "check suppressed" region, while the associated 
			template function was not. This check happened to be an expedient, if not ideal, way to deal 
			with that particular situation. */
			suppress_check_flag = state1.m_suppress_check_region_set.contains(DD, Rewrite, *(state1.m_ast_context_ptr));
		}
		//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
		if (suppress_check_flag) {
			return;
		}

		std::string variable_name = DD->getNameAsString();

		if (/*("" == variable_name) || */(!TP)) {
			return;
		}

		bool changed_from_original = false;

		assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if (true && (TP->isFunctionType())) {
			const clang::FunctionDecl* FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				auto name_str = FND->getNameAsString();
				auto return_type_source_range = write_once_source_range(cm1_adj_nice_source_range(FND->getReturnTypeSourceRange(), state1, Rewrite));
				if (!(return_type_source_range.isValid())) {
					return;
				}

				IF_DEBUG(std::string l_debug_source_location_str = return_type_source_range.getBegin().printToString(SM);)

				DEBUG_SOURCE_TEXT_STR(l_debug_source_text, return_type_source_range, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != l_debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				if (SR.getBegin() < return_type_source_range.getBegin()) {
					/* FunctionDecl::getReturnTypeSourceRange() seems to not include prefix qualifiers, like
					* "const". */
					return_type_source_range = write_once_source_range(extended_to_include_west_const_if_any(Rewrite, return_type_source_range));
				}

				auto res = generate_declaration_replacement_code(&ddecl, Rewrite, &state1, state1.m_ddecl_conversion_state_map, options_str);
				changed_from_original |= res.m_changed_from_original;

				auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(ddecl, &Rewrite);

				if (ConvertToSCPP && return_type_source_range.isValid() && (1 <= res.m_replacement_code.size())
						&& changed_from_original) {
					if (return_type_source_range.isValid()/* && res.m_replacement_return_type_post_params_suffix_str.empty()*/) {
						IF_DEBUG(std::string code_to_be_replaced = return_type_source_range.isValid() ? Rewrite.getRewrittenText(return_type_source_range) : std::string("[invalid]");)
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, return_type_source_range, res.m_replacement_return_type_str);
					} else if (!FND->isThisDeclarationADefinition()) {
						IF_DEBUG(std::string code_to_be_replaced = Rewrite.getRewrittenText(SR);)
						//auto unordered_SR2 = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, SR, res.m_replacement_code);
					} else {
						IF_DEBUG(std::string text1 = Rewrite.getRewrittenText(SR);)
						auto SR2 = SR;
						auto B = FND->getBody();
						if (B) {
							auto SLE = B->getSourceRange().getBegin().getLocWithOffset(-1);
							SR2 = write_once_source_range({ SR.getBegin(), SLE });
						} else {
							int q = 7;
						}
						IF_DEBUG(std::string code_to_be_replaced = Rewrite.getRewrittenText(SR2);)
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, SR2, res.m_replacement_code);
					}
				} else {
					int q = 7;
				}
			}
		} else {
			std::string replacement_code;

			auto rd_map_iter = state1.m_recdecl_map.find(SR.getBegin());
			if (state1.m_recdecl_map.end() != rd_map_iter) {
				auto RD = (*rd_map_iter).second;

				auto res1 = state1.m_recdecl_conversion_state_map.insert(*RD, Rewrite, state1);
				auto rdcs_map_iter = res1.first;
				auto& rdcs_ref = (*rdcs_map_iter).second;
				////bool update_declaration_flag = res1.second;

				std::string rd_name = rdcs_ref.recdecl_ptr()->getNameAsString();
				if ("" != rd_name) {
					if (rdcs_ref.recdecl_ptr()->isThisDeclarationADefinition()) {
						/* Some declarations combine a (named) struct definition with one or more variable definitions.
						For example, somethng like "struct abc_t { int m_a; } acb1;". In these cases we'll separate the
						the definition and declaration to something like "struct abc_t { int m_a; }; abc_t acb1;". */
						replacement_code += rdcs_ref.m_current_text_str + "; ";
					}
				} else {
					/* We are unable to handle this case at the moment. */
					return;
				}
				int q = 5;
			}

			/* Here we're checking for and noting any unsupported types. */
			struct CUnsupportedElementTypeNameAndReplacement {
				std::string m_element_type_name;
				std::string m_replacement_element_type_name;
				bool operator<(const CUnsupportedElementTypeNameAndReplacement &RHS) const {
					return (m_element_type_name < RHS.m_element_type_name);
				}
			};
			std::set<CUnsupportedElementTypeNameAndReplacement> unsupported_elements_encounterred;

			auto check_for_and_handle_unsupported_element2 = [&Rewrite, &unsupported_elements_encounterred, &SR](const clang::TypeLoc& typeLoc, clang::SourceRange l_SR, CTUState& state1) {
				//IF_DEBUG(auto type_SR = cm1_adj_nice_source_range(typeLoc.getSourceRange(), state1, Rewrite);)
				//IF_DEBUG(std::string old_text = Rewrite.getRewrittenText(type_SR);)

				auto qtype = typeLoc.getType().getUnqualifiedType();
				std::string element_type_name;
				const auto* l_CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();
				if (l_CXXRD) {
					element_type_name = l_CXXRD->getQualifiedNameAsString();

					if (("std::__cxx11::basic_string" == element_type_name) || ("std::basic_string" == element_type_name)) {
						if ("std::string" == qtype.getAsString()) {
							element_type_name = qtype.getAsString();
						}
					}
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

							unsupported_elements_encounterred.insert({element_type_name, replacement_element_type_name});
						}
					}
				}
			};
			auto tsi_ptr = DD->getTypeSourceInfo();
			if (tsi_ptr) {
				check_for_and_handle_unsupported_element2(tsi_ptr->getTypeLoc(), SR, state1);
				apply_to_template_arg_types_if_any(tsi_ptr->getTypeLoc(), check_for_and_handle_unsupported_element2, state1);
			}

			/* There may be multiple declarations in the same declaration statement. Replacing
			* one of them requires replacing all of them together. */
			auto ddecls = IndividualDeclaratorDecls(DD, Rewrite);
			if ((1 <= ddecls.size())/* && (ddecls.back() == DD)*/) {
				for (const auto& ddecl_cref : ddecls) {
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*ddecl_cref, &Rewrite);
					for (const auto& unsupported_element_encounterred : unsupported_elements_encounterred) {
						std::string tmp_str = ddcs_ref.current_direct_qtype_str();
						replace_whole_instances_of_given_string(tmp_str, unsupported_element_encounterred.m_element_type_name, unsupported_element_encounterred.m_replacement_element_type_name);
						ddcs_ref.set_current_direct_non_function_qtype_str(tmp_str);
						//update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
					}
				}

				if ((true || (2 <= ddecls.size()) || UsesPointerTypedef(DD->getType()) || is_macro_instantiation(DD->getSourceRange(), Rewrite)
					|| IsUnwieldyDecl(*DD) || ContainsFunctionPointerOfConcernDecl(*DD))) {

					std::string l_replacement_code;
					/* Instead of modifying the existing declaration(s), here we're completely
					replacing (overwriting) them. */
					static const std::string semicolon_space_str = "; ";
					std::vector<std::string> action_species_list;
					for (auto ddecl_ptr : ddecls) {
						if ((state1.m_ast_context_ptr) && (state1.m_suppress_check_region_set.contains(ddecl_ptr, Rewrite, *(state1.m_ast_context_ptr)))) {
							auto DDSR = cm1_adj_nice_source_range(ddecl_ptr->getSourceRange(), state1, Rewrite);
							if (DDSR.isValid()) {
								l_replacement_code += Rewrite.getRewrittenText(DDSR);
							} else {
								int q = 3;
							}
						} else {
							auto res = generate_declaration_replacement_code(ddecl_ptr, Rewrite, &state1, state1.m_ddecl_conversion_state_map, options_str);
							changed_from_original |= res.m_changed_from_original;

							action_species_list.push_back(res.m_action_species);
							l_replacement_code += res.m_replacement_code;
						}

						l_replacement_code += semicolon_space_str;
					}
					if (l_replacement_code.size() >= 3) {
						l_replacement_code = l_replacement_code.substr(0, l_replacement_code.size() - semicolon_space_str.length());
					}
					replacement_code += l_replacement_code;

					for (const auto& unsupported_element_encounterred : unsupported_elements_encounterred) {
						replace_whole_instances_of_given_string(replacement_code, unsupported_element_encounterred.m_element_type_name, unsupported_element_encounterred.m_replacement_element_type_name);
					}

					/* (Only) the source range of the last individual declaration in the declaration statement
					* should encompass the whole statement. */
					auto last_ddecl = ddecls.back();

					auto last_ddecl_qtype = last_ddecl->getType();
					IF_DEBUG(auto last_ddecl_qtype_str = last_ddecl_qtype.getAsString();)

					auto last_decl_source_range = get_potentially_rewritable_source_range(*last_ddecl, Rewrite, state1);

					IF_DEBUG(std::string last_decl_source_text = last_decl_source_range.isValid() ? Rewrite.getRewrittenText(last_decl_source_range) : std::string("[invalid]");)

					{
						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);
						/* In this case we're overwriting the whole declaration and including any
						initializers, so we want to remove any direction to append the initializer to
						the end of the declaration. */
						//ddcs_ref.m_maybe_embedded_initializer_insert_before_point = {};
						ddcs_ref.m_initializer_SR_or_insert_before_point = {};
						ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = {};
						ddcs_ref.m_has_been_replaced_as_a_whole = true;
					}

					if (ConvertToSCPP && last_decl_source_range.isValid() && (3 <= replacement_code.size())
							&& changed_from_original) {

						DEBUG_SOURCE_LOCATION_STR(debug_last_decl_source_location_str, last_decl_source_range, Rewrite);
						DEBUG_SOURCE_TEXT_STR(debug_last_decl_source_text1, last_decl_source_range, Rewrite);
#ifndef NDEBUG
						if (std::string::npos != debug_last_decl_source_location_str.find(g_target_debug_source_location_str1)) {
							int q = 5;
						}
#endif /*!NDEBUG*/

						if (last_decl_source_range.is_rewritable()) {
							state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, last_decl_source_range, replacement_code);
						} else {
							/* If the source range is not rewritable then this may be the one and only chance to write anything 
							to the source range, so we cannot queue the modification for later execution, we must execute the 
							modification here and now. */
							auto res2 = state1.m_pending_code_modification_actions.ReplaceText(Rewrite, last_decl_source_range, replacement_code);
						}
					} else {
						int q = 7;
					}
				} else {
					auto res = declaration_modifier_helper1(&ddecl, Rewrite, &state1, state1.m_ddecl_conversion_state_map, ESuppressModifications::No, options_str);

					for (const auto& unsupported_element_encounterred : unsupported_elements_encounterred) {
						if (SR.is_rewritable()) {
							/* This modification needs to be queued so that it will be executed after any other
							modifications that might affect the relevant part of the source text. */
							state1.m_pending_code_modification_actions.add_replacement_of_whole_instances_of_given_string_action(Rewrite, SR, unsupported_element_encounterred.m_element_type_name, unsupported_element_encounterred.m_replacement_element_type_name);
						} else {
							/* If the source range is not rewritable then this may be the one and only chance to write anything 
							to the source range, so we cannot queue the modification for later execution, we must execute the 
							modification here and now. */
							std::string replacement_code = Rewrite.getRewrittenText(SR);
							bool changed_flag = replace_whole_instances_of_given_string(replacement_code, unsupported_element_encounterred.m_element_type_name, unsupported_element_encounterred.m_replacement_element_type_name);
							if (changed_flag) {
#ifndef NDEBUG
								if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
									int q = 5;
								}
#endif /*!NDEBUG*/

								auto res2 = state1.m_pending_code_modification_actions.ReplaceText(Rewrite, SR, replacement_code);
								state1.m_pending_code_modification_actions.m_already_modified_regions.insert(SR);
							}
						}
					}
				}
			} else {
				int q = 7;
			}
		}
	}

	enum class apply_to_redeclarations_t : bool { no, yes };

	/* Ensure that the given declarations are the same type (give or take a reference). */
	void homogenize_types(CTUState& state1, Rewriter &Rewrite, const clang::DeclaratorDecl& ddecl_cref1,
		const clang::DeclaratorDecl& ddecl_cref2) {

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range(ddecl_cref1.getSourceRange(), state1, Rewrite);
		//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

		//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		/* homogenize the direct types (i.e. the types with any pointer/reference/array/etc indirections removed) */
		CSameTypeReplacementAction(Rewrite, CDDeclIndirection(ddecl_cref2, CDDeclIndirection::no_indirection)
			, CDDeclIndirection(ddecl_cref1, CDDeclIndirection::no_indirection)).do_replacement(state1);

		auto [lhs_ddcs_ref, lhs_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(ddecl_cref2, &Rewrite);
		auto [rhs_ddcs_ref, rhs_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(ddecl_cref1, &Rewrite);

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
				CSameTypeReplacementAction(Rewrite, CDDeclIndirection(ddecl_cref2, i + lhs_indirection_level_adjustment)
					, CDDeclIndirection(ddecl_cref1, i + rhs_indirection_level_adjustment)).do_replacement(state1);

				CSameTypeArray2ReplacementAction(Rewrite, CDDeclIndirection(*(lhs_ddcs_ref.m_ddecl_cptr), i + lhs_indirection_level_adjustment)
					, CDDeclIndirection(*(rhs_ddcs_ref.m_ddecl_cptr), i + rhs_indirection_level_adjustment)).do_replacement(state1);
			}
		}
	}

	static void update_declaration(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, apply_to_redeclarations_t apply_to_redeclarations = apply_to_redeclarations_t::yes, std::string options_str = "");
	static void update_declaration_if_not_suppressed(const DeclaratorDecl& ddecl, Rewriter &Rewrite, clang::ASTContext& context, CTUState& state1, apply_to_redeclarations_t apply_to_redeclarations = apply_to_redeclarations_t::yes, std::string options_str = "");

	/* Ensure that all the (re)declarations of the same variable are the same type. */
	void homogenize_redeclaration_types(const clang::DeclaratorDecl* ddecl_cptr, CTUState& state1, Rewriter &Rewrite, int ttl = -1) {
		/* While the ttl parameter is used to deal with runaway direct recursion, it's not unrealistic that 
		future code changes might result in (subtle) potential indirect infinite recursion. So we have a 
		separate mechanism to address that possibility. */
		static int sl_context_independent_recursion_depth = 0;
		struct CRAIIDepthTrackingHelper {
			CRAIIDepthTrackingHelper() {
				sl_context_independent_recursion_depth += 1;
			}
			~CRAIIDepthTrackingHelper() {
				sl_context_independent_recursion_depth -= 1;
			}
		};
		CRAIIDepthTrackingHelper raii_depth_tracking_helper1;
		if (100/*arbitrary*/ < sl_context_independent_recursion_depth) {
			static bool note_given = false;
			if (!note_given) {
				llvm::errs() << "\nnote: homogenize_redeclaration_types() recursion cut off due to exceeding the (preset) limit. \n";
				note_given = true;
			}
			return;
		}

		if (!ddecl_cptr) { return; }

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range(ddecl_cptr->getSourceRange(), state1, Rewrite);
		//RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

		//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto PVD = dyn_cast<const clang::ParmVarDecl>(ddecl_cptr);
		if (PVD) {
			auto PVD_qtype = PVD->getType();
			IF_DEBUG(std::string PVD_qtype_str = PVD_qtype.getAsString();)
			auto PVD_is_a_dependent_type = PVD_qtype->isInstantiationDependentType();
			auto DC = PVD->getDeclContext();
			const clang::FunctionDecl* function_decl1 = DC ? dyn_cast<const clang::FunctionDecl>(DC) : nullptr;
			if (function_decl1) {
				const std::string function_name = function_decl1->getNameAsString();

				std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
				for (auto param_PVD : function_decl1->parameters()) {
					param_decls_of_first_function_decl.push_back(param_PVD);
				}

				auto function_decls_range = function_decl1->redecls();
				for (const auto& function_decl : function_decls_range) {
					if (function_decl == function_decl1) {
						continue;
					}
					auto fdecl_source_range = cm1_adj_nice_source_range(function_decl->getSourceRange(), state1, Rewrite);
					if (!(fdecl_source_range.isValid())) {
						continue;
					}
					auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(Rewrite.getSourceMgr());

					auto param_index = PVD->getFunctionScopeIndex();
					auto PVD2 = function_decl->getParamDecl(param_index);
					if (PVD2) {
						auto PVD2_qtype = PVD2->getType();
						IF_DEBUG(std::string PVD2_qtype_str = PVD2_qtype.getAsString();)
						auto PVD2_is_a_dependent_type = PVD2_qtype->isInstantiationDependentType();

						if ((PVD2 != PVD) && (PVD2_is_a_dependent_type == PVD_is_a_dependent_type)) {
							homogenize_types(state1, Rewrite, *PVD2, *PVD);
							update_declaration(*PVD2, Rewrite, state1, apply_to_redeclarations_t::no);

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
												homogenize_types(state1, Rewrite, *init_VD2, *PVD2);
												update_declaration(*init_VD2, Rewrite, state1, apply_to_redeclarations_t::no);

												/* A non-negative ttl parameter specifies a maximum permitted number of
												recursive calls (to ensure no infinite recursion). */
												if (0 != ttl) {
													auto new_ttl = (0 < ttl) ? (ttl - 1) : ttl;
													homogenize_redeclaration_types(init_VD2, state1, Rewrite, new_ttl);
												}
											}
										}
									}
								}
							}
						} else if (!PVD2_is_a_dependent_type == PVD_is_a_dependent_type) {
							int q = 5;
						}
					}
				}
			}
		}

		auto VD = dyn_cast<const clang::VarDecl>(ddecl_cptr);
		if (VD) {
			auto VD_qtype = VD->getType();
			IF_DEBUG(std::string VD_qtype_str = VD_qtype.getAsString();)
			auto VD_is_a_dependent_type = VD_qtype->isInstantiationDependentType();

			for (auto redecl : VD->redecls()) {
				/* this part hasn't been tested yet */
				auto redecl_qtype = redecl->getType();
				IF_DEBUG(std::string redecl_qtype_str = redecl_qtype.getAsString();)
				auto redecl_is_a_dependent_type = redecl_qtype->isInstantiationDependentType();

				if ((redecl != VD) && (redecl_is_a_dependent_type == VD_is_a_dependent_type)) {
					homogenize_types(state1, Rewrite, *redecl, *VD);
					update_declaration(*redecl, Rewrite, state1, apply_to_redeclarations_t::no);
				}
			}

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
								homogenize_types(state1, Rewrite, *init_VD2, *VD);
								update_declaration(*init_VD2, Rewrite, state1, apply_to_redeclarations_t::no);

								if (0 != ttl) {
									auto new_ttl = (0 < ttl) ? (ttl - 1) : ttl;
									homogenize_redeclaration_types(init_VD2, state1, Rewrite, new_ttl);
								}
							}
						}
					}
				}
			}
		} else {
			auto FND = dyn_cast<const clang::FunctionDecl>(ddecl_cptr);
			if (FND) {
				auto FND_qtype = FND->getType();
				IF_DEBUG(std::string FND_qtype_str = FND_qtype.getAsString();)
				auto FND_is_a_dependent_type = FND_qtype->isInstantiationDependentType();

				for (auto redecl : FND->redecls()) {
					/* this part hasn't been tested yet */
					auto redecl_qtype = redecl->getType();
					IF_DEBUG(std::string redecl_qtype_str = redecl_qtype.getAsString();)
					auto redecl_is_a_dependent_type = redecl_qtype->isInstantiationDependentType();

					if ((redecl != FND) && (redecl_is_a_dependent_type == FND_is_a_dependent_type)) {
						homogenize_types(state1, Rewrite, *redecl, *FND);
						update_declaration(*redecl, Rewrite, state1, apply_to_redeclarations_t::no);
					} else if (!(redecl_is_a_dependent_type == FND_is_a_dependent_type)) {
						int q = 5;
					}
				}
			}
		}
	}

	static void update_declaration(const DeclaratorDecl& ddecl, Rewriter &Rewrite, CTUState& state1, apply_to_redeclarations_t apply_to_redeclarations/* = apply_to_redeclarations_t::yes*/, std::string options_str/* = ""*/) {
		const DeclaratorDecl* DD = &ddecl;

		QualType QT = DD->getType();
		const clang::Type* TP = QT.getTypePtr();
		IF_DEBUG(auto qtype_str = QT.getAsString();)

		auto SR = get_potentially_rewritable_source_range(ddecl, Rewrite, state1);
		RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

		auto& SM = Rewrite.getSourceMgr();

		IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)

		if (filtered_out_by_location<options_t<converter_mode_t> >(SM, SR.getBegin())) {
			return void();
		}

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		assert(ddecl.isFunctionOrFunctionTemplate() == QT->isFunctionType());
		if ((TP->isFunctionType()) || (false)) {
			const clang::FunctionDecl* FND = dyn_cast<const clang::FunctionDecl>(DD);
			if (FND) {
				auto return_type_source_range = write_once_source_range(cm1_adj_nice_source_range(FND->getReturnTypeSourceRange(), state1, Rewrite));
				if (!(return_type_source_range.isValid())) {
					return;
				}
				/* This function enqueues a "declaration_modifier()" action. declaration_modifier() does
				not directly modify the source text, but rather, in turn, enqueues other actions that do
				the direct modifications.
				In this case the declaration in question is a function declaration and we expect that
				only the function return type will be modified (rather than the whole (function)
				declaration). But we're still going to associate the declaration_modifier() action with
				the ("rewritable") range of the whole declaration, because the source range of the
				return type is a "write once" source range and it is not appropriate to associate "write
				once" source ranges with actions that don't do the direct modifications as doing so could
				result in the action pre-empting/blocking the action(s) that do the actual direct
				modifications. */
				//SR = return_type_source_range;
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

		if (apply_to_redeclarations_t::yes == apply_to_redeclarations) {
			homogenize_redeclaration_types(&ddecl, state1, Rewrite);
		}

		auto PVD = dyn_cast<const clang::ParmVarDecl>(&ddecl);
		if (PVD) {
			auto DC = PVD->getDeclContext();
			const auto* function_decl1 = DC ? dyn_cast<const clang::FunctionDecl>(DC) : nullptr;
			const auto* funDD = dyn_cast<const clang::DeclaratorDecl>(DC);
			if (funDD) {
				update_declaration(*funDD, Rewrite, state1, apply_to_redeclarations, options_str);
			}
		}
	}
	static void update_declaration_if_not_suppressed(const DeclaratorDecl& ddecl, Rewriter &Rewrite, clang::ASTContext& context, CTUState& state1, apply_to_redeclarations_t apply_to_redeclarations/* = apply_to_redeclarations_t::yes*/, std::string options_str/* = ""*/) {
		if (!state1.m_suppress_check_region_set.contains(&ddecl, Rewrite, context)) {
			update_declaration(ddecl, Rewrite, state1, apply_to_redeclarations, options_str);
		}
	}

	void note_array_determination(Rewriter &Rewrite, CTUState& state1, const CDDeclIndirection& ddecl_indirection) {

		auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(ddecl_indirection.m_ddecl_cptr), &Rewrite);

		if (ddcs_ref.m_indirection_state_stack.size() >= ddecl_indirection.m_indirection_level) {
			auto& indirection_state1 = ddcs_ref.m_indirection_state_stack.at(ddecl_indirection.m_indirection_level);
			if (!(indirection_state1.is_known_to_be_used_as_an_array_iterator())) {
				indirection_state1.set_is_known_to_be_used_as_an_array_iterator(true);
				update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, ddecl_indirection);
				if (indirection_state1.is_known_to_have_malloc_target()) {
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
				}
				if (indirection_state1.is_known_to_have_non_malloc_target()) {
					state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
				}
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
		std::optional<int> maybe_indirection_level_adjustment;
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

			auto& ddcs_indirection_state = ddcs_ref.m_indirection_state_stack.at(i);
			if (("" == stmt_indirection_stack[i])) {
				/* We're using the empty string as a generic state for the "terminal level of indirection"
				* when we don't want to bother specifying a specific state. */
			} else if (!ddcs_indirection_state.is_known_to_be_used_as_an_array_iterator()) {
				if (("ArraySubscriptExpr" == stmt_indirection_stack[i])
						|| ("pointer arithmetic" == stmt_indirection_stack[i])) {
					ddcs_indirection_state.set_is_known_to_be_used_as_an_array_iterator(true);
					retval.update_declaration_flag = true;
					retval.has_just_been_determined_to_be_an_array_flag = true;
					state1_ref.m_conversion_state_change_action_map.execute_matching_actions(state1_ref, CDDeclIndirection(*DD, i));
					if (ddcs_indirection_state.is_known_to_have_malloc_target()) {
						state1_ref.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
					}
					if (ddcs_indirection_state.is_known_to_have_non_malloc_target()) {
						state1_ref.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
					}
				} else if (("malloc target" == stmt_indirection_stack[i])) {
					ddcs_indirection_state.set_is_known_to_have_malloc_target(true);
					retval.update_declaration_flag = true;
					state1_ref.m_conversion_state_change_action_map.execute_matching_actions(state1_ref, CDDeclIndirection(*DD, i));
				} else if (("set to null" == stmt_indirection_stack[i]) ||
						("memset/cpy target" == stmt_indirection_stack[i])) {
				}
			} else {
				if (("malloc target" == stmt_indirection_stack[i]) 
						&& (!ddcs_indirection_state.is_known_to_have_malloc_target())) {
					ddcs_indirection_state.set_is_known_to_have_malloc_target(true);
					retval.update_declaration_flag = true;
					state1_ref.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				} else if (("native array" == stmt_indirection_stack[i])
						&& (!ddcs_indirection_state.is_known_to_have_non_malloc_target())) {
					ddcs_indirection_state.set_is_known_to_have_non_malloc_target(true);
					retval.update_declaration_flag = true;
					state1_ref.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1_ref, CDDeclIndirection(*DD, i));
				} else if (("memset/cpy target" == stmt_indirection_stack[i])) {
				} else if (("set to null" == stmt_indirection_stack[i])) {
				}
			}
		}
		if (1 <= stmt_indirection_stack.size()) {
			retval.indirection_level = (stmt_indirection_stack.size() - 1);
		} else {
			int q = 3;
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
		std::optional<int> maybe_stack_size_adjustment;
		const clang::Expr* expr2 = populateStmtIndirectionStack(stmt_indirection_stack, stmt_cref, maybe_stack_size_adjustment);
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

				bool function_return_value_only = false;
				if (l_DD->getType()->isFunctionType()) {
					auto FND = dyn_cast<const clang::FunctionDecl>(l_DD);
					auto E = dyn_cast<const clang::Expr>(&stmt_cref);
					if (E && FND) {
						auto E_ii = state1_ref.m_ast_context_ptr ? IgnoreParenImpNoopCasts(E, *(state1_ref.m_ast_context_ptr))
							: IgnoreParenImpCasts(E);
						auto CE = dyn_cast<const clang::CallExpr>(E_ii);
						if (!(CE)) {
							auto DRE = dyn_cast<const clang::DeclRefExpr>(E_ii);
							if (DRE) {
								for (auto child : DRE->children()) {
									auto E = dyn_cast<const clang::Expr>(child);
									if (E) {
										auto E_ii = state1_ref.m_ast_context_ptr ? IgnoreParenImpNoopCasts(E, *(state1_ref.m_ast_context_ptr))
											: IgnoreParenImpCasts(E);
										CE = dyn_cast<const clang::CallExpr>(E_ii);
									}
								}
							}
						}
						if (CE) {
							auto const fun_return_qtype = FND->getReturnType();
							auto const rr_fun_return_qtype = remove_reference(fun_return_qtype);
							auto const ce_qtype = CE->getType();
							auto const rr_ce_qtype = remove_reference(ce_qtype);
							if (rr_fun_return_qtype == rr_ce_qtype) {
								/* The given statement was (essentially) a call expression, and presumably l_DD refers to the 
								declaration of the called function. And presumably, because the function was actually called, 
								we're interested in the return value, not the "value" of the function itself. */
								function_return_value_only = true;
							} else {
								int q = 5;
							}
						}
					}
				}
				auto [ddcs_ref, update_declaration_flag] = state1_ref.get_ddecl_conversion_state_ref_and_update_flag(*l_DD, state1_ref.m_Rewrite_ptr, function_return_value_only);

				auto QT = (*l_DD).getType();
				std::string variable_name = (*l_DD).getNameAsString();

				if ((expr2_QT == QT) && (expr2_variable_name == variable_name)) {
					retval = infer_array_type_info_from_stmt_indirection_stack(ddcs_ref, stmt_indirection_stack, state1_ref);
					retval.maybe_indirection_level_adjustment = maybe_stack_size_adjustment;
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

		const Expr* EX = m_EX;
		if (EX) {
			auto EXSR = write_once_source_range(cm1_adj_nice_source_range(EX->getSourceRange(), state1, (*this).m_Rewrite));
			if (EXSR.isValid()) {
				auto excs_iter = state1.m_expr_conversion_state_map.find(EX);
				if (state1.m_expr_conversion_state_map.end() == excs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*EX, m_Rewrite, state1);
					excs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& excs_shptr_ref = (*excs_iter).second;

				if (ConvertToSCPP) {
					std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>((*this).m_replacement_code);
					(*excs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
					(*excs_shptr_ref).update_current_text();

					state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, EXSR, state1, EX);
					//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, EXSR, (*excs_shptr_ref).current_text());
					//(*this).m_Rewrite.ReplaceText(EXSR, (*excs_shptr_ref).current_text());
				}
			}
		}
	}

	/* A CExprTextYieldingReplacementAction is just like a regular CExprTextReplacementAction, except
	that it will not attempt the modification if the expression already has any modifications pending. */
	void CExprTextYieldingReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		const Expr* EX = m_EX;
		if (EX) {
			auto EXSR = write_once_source_range(cm1_adj_nice_source_range(EX->getSourceRange(), state1, (*this).m_Rewrite));
			if (EXSR.isValid()) {
				auto excs_iter = state1.m_expr_conversion_state_map.find(EX);
				if (state1.m_expr_conversion_state_map.end() == excs_iter) {
					std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*EX, m_Rewrite, state1);
					excs_iter = state1.m_expr_conversion_state_map.insert(shptr1);
				}
				auto& excs_shptr_ref = (*excs_iter).second;
				if (0 == (*excs_shptr_ref).m_expr_text_modifier_stack.size()) {
					if (ConvertToSCPP) {
						std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>((*this).m_replacement_code);
						(*excs_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
						(*excs_shptr_ref).update_current_text();

						state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, EXSR, state1, EX);
						//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, EXSR, (*excs_shptr_ref).current_text());
						//(*this).m_Rewrite.ReplaceText(EXSR, (*excs_shptr_ref).current_text());
					}
				} else {
					int q = 5;
				}
			}
		}
	}

	void CAssignmentTargetConstrainsSourceReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		bool target_points_to_mallocked_obj = false;

		auto& trgt_indirection = m_ddecl_indirection;
		auto [trgt_ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(trgt_indirection.m_ddecl_cptr), &m_Rewrite);
		auto trgt_indirection_level = trgt_indirection.m_indirection_level;

		if (trgt_ddcs_ref.m_indirection_state_stack.size() > trgt_indirection_level) {
			target_points_to_mallocked_obj = trgt_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level).is_known_to_have_malloc_target();
		} else {
			int q = 7;
		}

		auto& src_indirection = m_ddecl_indirection2;
		auto [src_ddcs_ref, src_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(src_indirection.m_ddecl_cptr), &m_Rewrite);
		auto src_indirection_level = src_indirection.m_indirection_level;

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range(trgt_indirection.m_ddecl_cptr->getSourceRange(), state1, Rewrite);
		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
		auto src_SR = cm1_adj_nice_source_range(src_indirection.m_ddecl_cptr->getSourceRange(), state1, Rewrite);
		DEBUG_SOURCE_LOCATION_STR(debug_src_source_location_str, src_SR, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_src_source_text, src_SR, Rewrite);
		if (std::string::npos != debug_src_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto is_void_star = [](clang::QualType const& qtype) {
			std::string qtype_str = qtype.getAsString();
			if (("void *" == qtype_str) || ("const void *" == qtype_str)) {
				return true;
			}
			return false;
		};
		if (is_void_star(trgt_indirection.m_ddecl_cptr->getType()) || is_void_star(src_indirection.m_ddecl_cptr->getType())) {
			/* `void *` elements do not provide the reliable information needed to infer constraints on other 
			elements that interact with them. */
			return;
		}

		{
			auto& lhs_ddcs_ref = trgt_ddcs_ref;
			auto& rhs_ddcs_ref = src_ddcs_ref;
			auto& rhs_update_declaration_flag = src_update_declaration_flag;

			auto& lhs_is_ineligible_for_xscope_status = (CDDeclIndirection::no_indirection == trgt_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_is_ineligible_for_xscope_status : lhs_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level).m_is_ineligible_for_xscope_status;
			auto& rhs_is_ineligible_for_xscope_status = (CDDeclIndirection::no_indirection == src_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_is_ineligible_for_xscope_status : rhs_ddcs_ref.m_indirection_state_stack.at(src_indirection_level).m_is_ineligible_for_xscope_status;

			if (lhs_is_ineligible_for_xscope_status) {
				if (!rhs_is_ineligible_for_xscope_status) {
#ifndef NDEBUG
					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					rhs_is_ineligible_for_xscope_status = lhs_is_ineligible_for_xscope_status;
					rhs_update_declaration_flag |= true;
					state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, src_indirection);
				}
			}

			auto& lhs_function_state_ref = (CDDeclIndirection::no_indirection == trgt_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_function_type_state : lhs_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level).m_function_type_state;
			auto& rhs_function_state_ref = (CDDeclIndirection::no_indirection == src_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_function_type_state : rhs_ddcs_ref.m_indirection_state_stack.at(src_indirection_level).m_function_type_state;
			if (lhs_function_state_ref.has_been_changed()) {
				bool assign_function_state = true;
				if (CDDeclIndirection::no_indirection == src_indirection_level) {
					if (!rhs_ddcs_ref.direct_type_state_ref().seems_to_be_a_function_type()) {
						assign_function_state = false;
					}
				}
				if (assign_function_state) {
					rhs_function_state_ref = lhs_function_state_ref;
					rhs_update_declaration_flag |= true;
				}
			}
		}

		if (src_ddcs_ref.m_indirection_state_stack.size() > src_indirection_level) {
			const auto& trgt_species = trgt_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level).current_species();
			const auto& src_species = src_ddcs_ref.m_indirection_state_stack.at(src_indirection_level).current_species();

			auto& src_indirection_state = src_ddcs_ref.m_indirection_state_stack.at(src_indirection_level);
			auto& trgt_indirection_state = trgt_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level);

			if (trgt_indirection_state.is_known_to_be_used_as_an_array_iterator() && (!src_indirection_state.is_known_to_be_used_as_an_array_iterator())) {
				src_indirection_state.set_is_known_to_be_used_as_an_array_iterator(true);
				src_update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, src_indirection);
				if (src_indirection_state.is_known_to_have_malloc_target()) {
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, src_indirection);
				}
				if (src_indirection_state.is_known_to_have_non_malloc_target()) {
					state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, src_indirection);
				}
			}
			if (true && (trgt_indirection_state.is_known_to_have_malloc_target() && (!trgt_indirection_state.is_known_to_have_non_malloc_target())) 
					&& ((!src_indirection_state.is_known_to_have_malloc_target()) && (!trgt_indirection_state.is_known_to_have_non_malloc_target()))
					&& ("native array" != src_indirection_state.original_species() )) {
				/* So in this situation the target is_known_to_have_malloc_target but not known_to_have_non_malloc_target target 
				and the source is not know to have either. The question is, do we assume that the source provides 
				a malloc target? */

				src_indirection_state.set_is_known_to_have_malloc_target(true);
				src_update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, src_indirection);
				if (src_indirection_state.is_known_to_be_used_as_an_array_iterator()) {
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, src_indirection);
				}
			}
		} else {
			int q = 7;
		}

		if (src_update_declaration_flag) {
			update_declaration(*(src_ddcs_ref.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CAssignmentSourceConstrainsTargetReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		auto& trgt_indirection = m_ddecl_indirection2;
		auto [trgt_ddcs_ref, trgt_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(trgt_indirection.m_ddecl_cptr), &m_Rewrite);
		auto trgt_indirection_level = trgt_indirection.m_indirection_level;

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range(trgt_indirection.m_ddecl_cptr->getSourceRange(), state1, Rewrite);

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto& src_indirection = m_ddecl_indirection;
		auto [src_ddcs_ref, src_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(src_indirection.m_ddecl_cptr), &m_Rewrite);
		auto src_indirection_level = src_indirection.m_indirection_level;

		auto is_void_star = [](clang::QualType const& qtype) {
			std::string qtype_str = qtype.getAsString();
			if (("void *" == qtype_str) || ("const void *" == qtype_str)) {
				return true;
			}
			return false;
		};
		if (is_void_star(trgt_indirection.m_ddecl_cptr->getType()) || is_void_star(src_indirection.m_ddecl_cptr->getType())) {
			/* `void *` elements do not provide the reliable information needed to infer constraints on other 
			elements that interact with them. */
			return;
		}

		{
			auto& lhs_ddcs_ref = trgt_ddcs_ref;
			auto& rhs_ddcs_ref = src_ddcs_ref;
			auto& lhs_update_declaration_flag = trgt_update_declaration_flag;

			auto& lhs_function_state_ref = (CDDeclIndirection::no_indirection == trgt_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_function_type_state : lhs_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level).m_function_type_state;
			auto& rhs_function_state_ref = (CDDeclIndirection::no_indirection == src_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_function_type_state : rhs_ddcs_ref.m_indirection_state_stack.at(src_indirection_level).m_function_type_state;
			if (rhs_function_state_ref.has_been_changed()) {
				bool assign_function_state = true;
				if (CDDeclIndirection::no_indirection == trgt_indirection_level) {
					if (!lhs_ddcs_ref.direct_type_state_ref().seems_to_be_a_function_type()) {
						assign_function_state = false;
					}
				}
				if (assign_function_state) {
					lhs_function_state_ref = rhs_function_state_ref;
					lhs_update_declaration_flag |= true;
				}
			}
		}

		if (trgt_ddcs_ref.m_indirection_state_stack.size() > trgt_indirection_level) {
			auto& src_indirection_state = src_ddcs_ref.m_indirection_state_stack.at(src_indirection_level);
			auto& trgt_indirection_state = trgt_ddcs_ref.m_indirection_state_stack.at(trgt_indirection_level);

			if (src_indirection_state.is_known_to_be_used_as_an_array_iterator() && (!trgt_indirection_state.is_known_to_be_used_as_an_array_iterator())) {
				trgt_indirection_state.set_is_known_to_be_used_as_an_array_iterator(true);
				trgt_update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, trgt_indirection);
				if (src_indirection_state.is_known_to_have_malloc_target() || trgt_indirection_state.is_known_to_have_malloc_target()) {
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, trgt_indirection);
				}
				if (src_indirection_state.is_known_to_have_non_malloc_target() || trgt_indirection_state.is_known_to_have_non_malloc_target()) {
					state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, trgt_indirection);
				}
			}
			if (src_indirection_state.is_known_to_have_malloc_target() && (!trgt_indirection_state.is_known_to_have_malloc_target())) {
				trgt_indirection_state.set_is_known_to_have_malloc_target(true);
				trgt_update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, trgt_indirection);
				if (trgt_indirection_state.is_known_to_be_used_as_an_array_iterator()) {
					state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, trgt_indirection);
				}
			}
			if (src_indirection_state.is_known_to_have_non_malloc_target() && (!trgt_indirection_state.is_known_to_have_non_malloc_target())) {
				trgt_indirection_state.set_is_known_to_have_non_malloc_target(true);
				trgt_indirection_state.m_array_size_expr = src_indirection_state.m_array_size_expr;
				trgt_indirection_state.m_array_size_expr_read_from_source_text = src_indirection_state.m_array_size_expr_read_from_source_text;

				trgt_update_declaration_flag |= true;
				state1.m_conversion_state_change_action_map.execute_matching_actions(state1, trgt_indirection);
				if (trgt_indirection_state.is_known_to_be_used_as_an_array_iterator()) {
					state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, trgt_indirection);
				}
			}
		} else {
			int q = 7;
		}

		if (trgt_update_declaration_flag) {
			update_declaration(*(trgt_ddcs_ref.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CSameTypeReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		auto& lhs_indirection = m_ddecl_indirection2;
		auto [lhs_ddcs_ref, lhs_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(lhs_indirection.m_ddecl_cptr), &m_Rewrite);

		auto& rhs_indirection = m_ddecl_indirection;
		auto [rhs_ddcs_ref, rhs_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(rhs_indirection.m_ddecl_cptr), &m_Rewrite);

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range(rhs_indirection.m_ddecl_cptr->getSourceRange(), state1, Rewrite);

		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		{
			std::string& lhs_pointer_target_state = (CDDeclIndirection::no_indirection == lhs_indirection.m_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_current_pointer_target_state : lhs_ddcs_ref.m_indirection_state_stack.at(lhs_indirection.m_indirection_level).m_current_pointer_target_state;
			std::string& rhs_pointer_target_state = (CDDeclIndirection::no_indirection == rhs_indirection.m_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_current_pointer_target_state : rhs_ddcs_ref.m_indirection_state_stack.at(rhs_indirection.m_indirection_level).m_current_pointer_target_state;

			if ("pointer target" == lhs_pointer_target_state) {
				if ("" == rhs_pointer_target_state) {
					rhs_pointer_target_state = lhs_pointer_target_state;
					rhs_update_declaration_flag |= true;
					state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
				}
			} else if ("" == lhs_pointer_target_state) {
				if ("pointer target" == rhs_pointer_target_state) {
					lhs_pointer_target_state = rhs_pointer_target_state;
					lhs_update_declaration_flag |= true;
					state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
				}
			}

			auto& lhs_is_ineligible_for_xscope_status = (CDDeclIndirection::no_indirection == lhs_indirection.m_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_is_ineligible_for_xscope_status : lhs_ddcs_ref.m_indirection_state_stack.at(lhs_indirection.m_indirection_level).m_is_ineligible_for_xscope_status;
			auto& rhs_is_ineligible_for_xscope_status = (CDDeclIndirection::no_indirection == rhs_indirection.m_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_is_ineligible_for_xscope_status : rhs_ddcs_ref.m_indirection_state_stack.at(rhs_indirection.m_indirection_level).m_is_ineligible_for_xscope_status;

			if (lhs_is_ineligible_for_xscope_status != rhs_is_ineligible_for_xscope_status) {
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/
			}

			if (lhs_is_ineligible_for_xscope_status) {
				if (!rhs_is_ineligible_for_xscope_status) {
					rhs_is_ineligible_for_xscope_status = lhs_is_ineligible_for_xscope_status;
					rhs_update_declaration_flag |= true;
					state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
				}
			} else {
				assert(!lhs_is_ineligible_for_xscope_status);
				if (rhs_is_ineligible_for_xscope_status) {
					lhs_is_ineligible_for_xscope_status = rhs_is_ineligible_for_xscope_status;
					lhs_update_declaration_flag |= true;
					state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
				}
			}

			auto is_void_star = [](clang::QualType const& qtype) {
				std::string qtype_str = qtype.getAsString();
				if (("void *" == qtype_str) || ("const void *" == qtype_str)) {
					return true;
				}
				return false;
			};
			if (is_void_star(lhs_indirection.m_ddecl_cptr->getType()) || is_void_star(rhs_indirection.m_ddecl_cptr->getType())) {
				/* `void *` elements do not provide the reliable information needed to infer constraints on other 
				elements that interact with them. */
				return;
			}

			auto& lhs_function_state_ref = (CDDeclIndirection::no_indirection == lhs_indirection.m_indirection_level) ? lhs_ddcs_ref.direct_type_state_ref().m_function_type_state : lhs_ddcs_ref.m_indirection_state_stack.at(lhs_indirection.m_indirection_level).m_function_type_state;
			auto& rhs_function_state_ref = (CDDeclIndirection::no_indirection == rhs_indirection.m_indirection_level) ? rhs_ddcs_ref.direct_type_state_ref().m_function_type_state : rhs_ddcs_ref.m_indirection_state_stack.at(rhs_indirection.m_indirection_level).m_function_type_state;
			if (rhs_function_state_ref.has_been_changed()) {
				if (!lhs_function_state_ref.has_been_changed()) {
					bool assign_function_state = true;
					if (CDDeclIndirection::no_indirection == lhs_indirection.m_indirection_level) {
						if (!lhs_ddcs_ref.direct_type_state_ref().seems_to_be_a_function_type()) {
							assign_function_state = false;
						}
					}
					if (assign_function_state) {
						lhs_function_state_ref = rhs_function_state_ref;
						lhs_update_declaration_flag |= true;
					}
				} else {
					int q = 7;
				}
			} else if (lhs_function_state_ref.has_been_changed()) {
				bool assign_function_state = true;
				if (CDDeclIndirection::no_indirection == rhs_indirection.m_indirection_level) {
					if (!rhs_ddcs_ref.direct_type_state_ref().seems_to_be_a_function_type()) {
						assign_function_state = false;
					}
				}
				if (assign_function_state) {
					rhs_function_state_ref = lhs_function_state_ref;
					rhs_update_declaration_flag |= true;
				}
			}
		}

		if ((lhs_ddcs_ref.m_indirection_state_stack.size() > lhs_indirection.m_indirection_level) &&
				(rhs_ddcs_ref.m_indirection_state_stack.size() > rhs_indirection.m_indirection_level)){

			auto& rhs_indirection_state = rhs_ddcs_ref.m_indirection_state_stack.at(rhs_indirection.m_indirection_level);
			auto& lhs_indirection_state = lhs_ddcs_ref.m_indirection_state_stack.at(lhs_indirection.m_indirection_level);

			//if (!(rhs_indirection_state.m_indirection_properties1 == lhs_indirection_state.m_indirection_properties1))
			if (rhs_indirection_state.current_species() != lhs_indirection_state.current_species()) 
			{
				CAssignmentSourceConstrainsTargetReplacementAction(m_Rewrite, m_ddecl_indirection, m_ddecl_indirection2).do_replacement(state1);
				CAssignmentSourceConstrainsTargetReplacementAction(m_Rewrite, m_ddecl_indirection2, m_ddecl_indirection).do_replacement(state1);
			}

			auto lhs_indirection_level = lhs_indirection.m_indirection_level;
			auto rhs_indirection_level = rhs_indirection.m_indirection_level;
			auto& lhs_indirection_state_ref = lhs_ddcs_ref.m_indirection_state_stack.at(lhs_indirection_level);
			auto& rhs_indirection_state_ref = rhs_ddcs_ref.m_indirection_state_stack.at(rhs_indirection_level);

			if (false) {
				auto& lhs_current_cref = lhs_ddcs_ref.indirection_current(lhs_indirection_level);
				auto& rhs_current_cref = rhs_ddcs_ref.indirection_current(rhs_indirection_level);

				if (rhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator() && (!lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator())) {
					lhs_indirection_state_ref.set_is_known_to_be_used_as_an_array_iterator(true);
					lhs_update_declaration_flag |= true;
					state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
					if (rhs_indirection_state_ref.is_known_to_have_malloc_target() || lhs_indirection_state_ref.is_known_to_have_malloc_target()) {
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					}
					if (rhs_indirection_state_ref.is_known_to_have_non_malloc_target() || lhs_indirection_state_ref.is_known_to_have_non_malloc_target()) {
						state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					}
				}
				if (rhs_indirection_state_ref.is_known_to_have_malloc_target() && (!lhs_indirection_state_ref.is_known_to_have_malloc_target())) {
					lhs_indirection_state_ref.set_is_known_to_have_malloc_target(true);
					lhs_update_declaration_flag |= true;
					if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					}
				}
				if (rhs_indirection_state_ref.is_known_to_have_non_malloc_target() && (!lhs_indirection_state_ref.is_known_to_have_non_malloc_target())) {
					lhs_indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
					lhs_update_declaration_flag |= true;
					if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
						state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					}
				}


				if ("native pointer" == lhs_current_cref) {
					if ("native pointer" == rhs_current_cref) {
					} else if (rhs_indirection_state_ref.is_a_pointer_that_has_not_been_determined_to_be_an_array()) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
					} else if (("inferred array" == rhs_current_cref) || ("variously native and dynamic array" == rhs_current_cref)) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
					} else if ("dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					} else if ("native array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
					}
				} else if ("malloc target" == lhs_current_cref) {
					if ("native pointer" == rhs_current_cref) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
					} else if ("malloc target" == rhs_current_cref) {
					} else if ("non-malloc target" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously malloc and non-malloc target");
						lhs_update_declaration_flag |= true;
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously malloc and non-malloc target");
						rhs_update_declaration_flag |= true;
					} else if ("variously malloc and non-malloc target" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously malloc and non-malloc target");
						lhs_update_declaration_flag |= true;
					} else if ("inferred array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "dynamic array");
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "dynamic array");
						rhs_update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
					} else if ("dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					} else if ("native array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
					} else if ("variously native and dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
					}
				} else if ("variously malloc and non-malloc target" == lhs_current_cref) {
					if ("variously malloc and non-malloc target" == rhs_current_cref) {
					} else if (rhs_indirection_state_ref.is_a_pointer_that_has_not_been_determined_to_be_an_array()) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
					} else if (rhs_indirection_state_ref.has_been_determined_to_point_to_an_array()) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
					}
				} else if ("inferred array" == lhs_current_cref) {
					if (("native pointer" == rhs_current_cref) || ("non-malloc target" == rhs_current_cref)) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
					} else if ("malloc target" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "dynamic array");
						lhs_update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "dynamic array");
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
					} else if ("variously malloc and non-malloc target" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
					} else if ("inferred array" == rhs_current_cref) {
					} else if ("dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
					} else if (("native array" == rhs_current_cref) || ("variously native and dynamic array" == rhs_current_cref)) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, rhs_current_cref);
						lhs_update_declaration_flag |= true;
					}
				} else if ("dynamic array" == lhs_current_cref) {
					if ("native pointer" == rhs_current_cref) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
					} else if ("malloc target" == rhs_current_cref) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
					} else if (("non-malloc target" == rhs_current_cref) || ("variously malloc and non-malloc target" == rhs_current_cref)) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
					} else if ("inferred array" == rhs_current_cref) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
					} else if ("dynamic array" == rhs_current_cref) {
					} else if ("native array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
					} else if ("variously native and dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
					}
				} else if ("native array" == lhs_current_cref) {
					if (("native pointer" == rhs_current_cref) || ("non-malloc target" == rhs_current_cref)) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
					} else if (("malloc target" == rhs_current_cref) || ("variously malloc and non-malloc target" == rhs_current_cref)) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
						state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
					} else if ("inferred array" == rhs_current_cref) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
					} else if ("dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;

						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
					} else if ("native array" == rhs_current_cref) {
					} else if ("variously native and dynamic array" == rhs_current_cref) {
						lhs_ddcs_ref.set_indirection_current(lhs_indirection_level, "variously native and dynamic array");
						lhs_update_declaration_flag |= true;
					}
				} else if ("variously native and dynamic array" == lhs_current_cref) {
					if (rhs_indirection_state_ref.is_a_pointer_that_has_not_been_determined_to_be_an_array()) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, lhs_current_cref);
						rhs_update_declaration_flag |= true;
					} else if ("variously native and dynamic array" == rhs_current_cref) {
					} else if (rhs_indirection_state_ref.has_been_determined_to_point_to_an_array()) {
						rhs_ddcs_ref.set_indirection_current(rhs_indirection_level, "variously native and dynamic array");
						rhs_update_declaration_flag |= true;
					}
				}
			}

			if ("" == lhs_indirection_state_ref.m_function_type_state.m_params_current_str) {
				if ("" != rhs_indirection_state_ref.m_function_type_state.m_params_current_str) {
					lhs_indirection_state_ref.m_function_type_state.m_params_current_str = rhs_indirection_state_ref.m_function_type_state.m_params_current_str;
				}
			} else if ("" == rhs_indirection_state_ref.m_function_type_state.m_params_current_str) {
				rhs_indirection_state_ref.m_function_type_state.m_params_current_str = lhs_indirection_state_ref.m_function_type_state.m_params_current_str;
			}

			/* There is presumably a CSameTypeReplacementAction to go along with this
			CSameTypeArray2ReplacementAction. The "pointer target state", "xscope eligibility status", etc.
			should be handled by that CSameTypeReplacementAction, so they shouldn't need to be
			handled here. */
		} else {
			int q = 7;
		}

		if (lhs_update_declaration_flag) {
			update_declaration(*(lhs_indirection.m_ddecl_cptr), Rewrite, state1);
		}
		if (rhs_update_declaration_flag) {
			update_declaration(*(rhs_indirection.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CMallocArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const BinaryOperator* BO = m_BO;
		const DeclaratorDecl* DD = m_DD;

		if ((BO != nullptr) && (DD != nullptr))
		{
			auto BOSR = write_once_source_range(cm1_adj_nice_source_range(BO->getSourceRange(), state1, Rewrite));

			if ((*this).ddecl_indirection_cref().m_ddecl_cptr) {
				auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*((*this).ddecl_indirection_cref().m_ddecl_cptr), &m_Rewrite);

				auto& indirection_state = ddcs_ref.m_indirection_state_stack.at((*this).ddecl_indirection_cref().m_indirection_level);
				if (!indirection_state.is_known_to_have_malloc_target()) {
					int q = 3;
					indirection_state.set_is_known_to_have_malloc_target(true);
					state1.m_conversion_state_change_action_map.execute_matching_actions(state1, (*this).ddecl_indirection_cref());
				}

				if (ddcs_ref.m_indirection_state_stack.size() >= (*this).ddecl_indirection_cref().m_indirection_level) {
					if ("inferred array" == ddcs_ref.indirection_current((*this).ddecl_indirection_cref().m_indirection_level)) {
						ddcs_ref.set_indirection_current((*this).ddecl_indirection_cref().m_indirection_level, "dynamic array");
						update_declaration_flag |= true;
						state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, (*this).ddecl_indirection_cref());
					} else if ("native array" == ddcs_ref.indirection_current((*this).ddecl_indirection_cref().m_indirection_level)) {
						ddcs_ref.set_indirection_current((*this).ddecl_indirection_cref().m_indirection_level, "variously native and dynamic array");
						update_declaration_flag |= true;
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

				state1.add_pending_straight_text_replacement_expression_update(Rewrite, BOSR, BO, (*this).m_bo_replacement_code);
				int q = 3;
			} else {
				int q = 7;
			}
		}
	}

	const clang::Expr* get_init_expr_if_any(const DeclaratorDecl* DD) {
		clang::Expr const* pInitExpr = nullptr;
		if (!DD) {
			return pInitExpr;
		}
		auto VD = dyn_cast<const clang::VarDecl>(DD);
		auto FD = dyn_cast<const clang::FieldDecl>(DD);
		if (VD || FD) {
			if (VD) {
				if (VD->hasInit()) {
					pInitExpr = VD->getInit();
				}
			} else {
				if (FD->hasInClassInitializer()) {
					pInitExpr = FD->getInClassInitializer();
				}
			}
		}
		return pInitExpr;
	}

	void CInitializerArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		//const DeclStmt* DS = m_DS;
		const DeclaratorDecl* DD = m_DD;

		if (/*(DS != nullptr) && */(DD != nullptr))
		{
			auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);

			DEBUG_SOURCE_LOCATION_STR(debug_last_decl_source_location_str, decl_source_range, Rewrite);
			DEBUG_SOURCE_TEXT_STR(debug_last_decl_source_text1, decl_source_range, Rewrite);
#ifndef NDEBUG
			if (std::string::npos != debug_last_decl_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &m_Rewrite);

			std::string current_direct_qtype_str = ddcs_ref.current_direct_qtype_str();
			std::string initializer_info_str = m_current_initialization_expr_str;
			static const std::string void_str = "void";
			auto void_pos = initializer_info_str.find(void_str);
			if (std::string::npos != void_pos) {
				initializer_info_str.replace(void_pos, void_str.length(), current_direct_qtype_str);
			}

			clang::Expr const* pInitExpr = get_init_expr_if_any(DD);
			if (pInitExpr) {
				ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, Rewrite, state1));

				auto& ecs = state1.get_expr_conversion_state_ref<CExprConversionState>(*pInitExpr, Rewrite);
				std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(initializer_info_str);
				ecs.m_expr_text_modifier_stack.push_back(shptr1);
				ecs.update_current_text();

				/* We shouldn't need to add an "expression_update_replacement_action" here as the updated expression 
				text should be rendered when the parent declaration text gets rendered. */
				//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, SR, state1, CE);
			}
			ddcs_ref.m_fallback_current_initialization_expr_str = initializer_info_str;

			if (ConvertToSCPP && decl_source_range.isValid()) {
				update_declaration(*DD, Rewrite, state1);

				int q = 3;
			} else {
				int q = 7;
			}
		}
	}

	void CFreeDynamicArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const CallExpr* CE = m_CE;
		const DeclaratorDecl* DD = m_DD;

		if ((CE != nullptr) && (DD != nullptr))
		{
			auto CESR = write_once_source_range(cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite));
			auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
			if (!decl_source_range.isValid()) {
				return;
			}
			DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
			DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

			if (ConvertToSCPP && decl_source_range.isValid() && (CESR.isValid())) {
				update_declaration(*DD, Rewrite, state1);

				state1.add_pending_straight_text_replacement_expression_update(Rewrite, CESR, CE, (*this).m_ce_replacement_code);
			} else {
				int q = 7;
			}
		}
	}

	void CAssignmentTargetConstrainsSourceDynamicArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(m_ddecl_indirection2.m_ddecl_cptr), &m_Rewrite);

		if (ddcs_ref.m_indirection_state_stack.size() >= m_ddecl_indirection2.m_indirection_level) {
			if (true) {
				ddcs_ref.set_indirection_current(m_ddecl_indirection2.m_indirection_level, "dynamic array");
				update_declaration_flag |= true;
				//state1.m_conversion_state_change_action_map.execute_matching_actions(state1, m_ddecl_indirection2);
				state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, m_ddecl_indirection2);
			}
		} else {
			int q = 7;
		}

		if (update_declaration_flag) {
			update_declaration(*(m_ddecl_indirection2.m_ddecl_cptr), Rewrite, state1);
		}
	}

	void CAssignmentTargetConstrainsSourceArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		CAssignmentTargetConstrainsSourceReplacementAction(Rewrite, m_ddecl_indirection, m_ddecl_indirection2).do_replacement(state1);
	}

	void CAssignmentSourceConstrainsTargetArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

#ifndef NDEBUG
		if (true && m_ddecl_indirection.m_ddecl_cptr) {
			std::string variable_name = (m_ddecl_indirection.m_ddecl_cptr)->getNameAsString();
			if ("env_wgetrc" == variable_name) {
				std::string qtype_str = (m_ddecl_indirection.m_ddecl_cptr)->getType().getAsString();
				if ("const unsigned char *" == qtype_str) {
					int q = 5;
				}
			}
		}
#endif /*!NDEBUG*/

		CAssignmentSourceConstrainsTargetReplacementAction(Rewrite, m_ddecl_indirection, m_ddecl_indirection2).do_replacement(state1);
	}

	void CSameTypeArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		CSameTypeReplacementAction(Rewrite, m_ddecl_indirection, m_ddecl_indirection2).do_replacement(state1);
	}

	void CAddressofArraySubscriptExprReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const ArraySubscriptExpr* ASE = m_arraysubscriptexpr_cptr;
		if (UO && ASE) {
			if (ConvertToSCPP) {

				state1.add_pending_expression_update<CAddressofArraySubscriptExprConversionState>(*UO, Rewrite, *ASE);
				//state1.add_pending_straight_text_replacement_expression_update(Rewrite, UOSR, UO, UO_replacement_text);
				//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, UOSR, state1, UO);
				//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).current_text());
				//(*this).m_Rewrite.ReplaceText(UOSR, uo_ecs_ref.current_text());
			}
		}
	}

	void CAddressofSubscriptOperatorCallExprReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		const clang::UnaryOperator* UO = m_addrofexpr_cptr;
		const clang::CXXOperatorCallExpr* ASE = m_arraysubscriptexpr_cptr;
		if (UO && ASE) {
			auto UOSR = write_once_source_range(cm1_adj_nice_source_range(UO->getSourceRange(), state1, (*this).m_Rewrite));
			auto ase_SR = cm1_adj_nice_source_range(ASE->getSourceRange(), state1, (*this).m_Rewrite);
			if ((UOSR.isValid()) && (ase_SR.isValid())) {
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
				if (index_expr_cptr && array_expr_cptr) {
					auto& index_ecs_ref = state1.get_expr_conversion_state_ref(*index_expr_cptr, Rewrite);
					auto& array_ecs_ref = state1.get_expr_conversion_state_ref(*array_expr_cptr, Rewrite);

					std::vector<const clang::Expr *> arg_expr_cptrs;
					arg_expr_cptrs.push_back(array_expr_cptr);
					arg_expr_cptrs.push_back(index_expr_cptr);

					std::string new_function_name;
					if ("Dual" == ConvertMode) {
						new_function_name = "MSE_LH_ADDRESS_OF_ARRAY_ELEMENT";
					} else if ("FasterAndStricter" == ConvertMode) {
						new_function_name = "mse::lh::address_of_array_element_replacement(";
					} else {
						new_function_name = "mse::lh::address_of_array_element_replacement(";
					}
					if (ConvertToSCPP) {

						state1.add_pending_expression_update<CCallExprConversionState>(*UO, Rewrite, arg_expr_cptrs, new_function_name);
						//state1.add_pending_straight_text_replacement_expression_update(Rewrite, UOSR, UO, UO_replacement_text);
						//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, UOSR, state1, UO);
						//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, UOSR, (*uocs_shptr_ref).current_text());
						//(*this).m_Rewrite.ReplaceText(UOSR, uo_ecs_ref.current_text());
					}
				}
			}
		}
	}

	void CExprTextDDIReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		const Expr* EX = m_EX;
		CExprTextReplacementAction(Rewrite, EX, (*this).m_replacement_code).do_replacement(state1);
	}

	void CUpdateIndirectFunctionTypeParamsArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		const clang::CallExpr* CE = m_CE;
		if (CE) {
			auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*((*this).m_indirect_function_DD), &m_Rewrite);

			auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

			//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			assert(ddcs_ref.direct_type_state_ref().current_qtype_if_any().has_value());
			if (llvm::isa<const clang::FunctionType>(ddcs_ref.direct_type_state_ref().current_qtype_if_any().value())) {
				auto FNT = llvm::cast<const clang::FunctionType>(ddcs_ref.direct_type_state_ref().current_qtype_if_any().value());
				std::string new_return_type_code = adjusted_qtype_str(FNT->getReturnType().getAsString(), FNT->getReturnType());
				std::string new_params_code = "(";

				for (size_t i = 0; (i < CE->getNumArgs()); i += 1) {
					if (1 <= i) {
						new_params_code += ", ";
					}

					auto arg = CE->getArg(i);
					auto rhs_res2 = infer_array_type_info_from_stmt(*arg, "", state1);
					bool rhs_is_an_indirect_type = is_an_indirect_type(arg->getType());
					if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
						update_declaration(*(rhs_res2.ddecl_cptr), Rewrite, state1);
					}

					if (rhs_res2.ddecl_cptr && rhs_res2.ddecl_conversion_state_ptr) {
						auto res3 = generate_type_indirection_prefix_and_suffix((*rhs_res2.ddecl_conversion_state_ptr).m_indirection_state_stack, Rewrite, 
							EIsFunctionParam::Yes, {}/*maybe_storage_duration*/, &state1);

						new_params_code += res3.m_complete_type_str;
					} else {
						new_params_code += adjusted_qtype_str(arg->getType().getAsString(), arg->getType());
						int q = 7;
					}
				}
				new_params_code += ")";

#ifndef NDEBUG
				if (false && ("(png_structrp, MSE_LH_ARRAY_ITERATOR_TYPE(const char) )" == new_params_code)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				ddcs_ref.set_current_direct_function_qtype_str(new_return_type_code, new_params_code);

				update_declaration(*((*this).m_indirect_function_DD), (*this).m_Rewrite, state1);
			} else {
				int q = 7;
			}
		} else {
			int q = 7;
		}
	}

	void CUpdateDeclIndirectionArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;
		auto DD = (*this).m_ddecl_indirection2.m_ddecl_cptr;

		if (DD) {
			auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &m_Rewrite);

			auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

			//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			update_declaration(*DD, (*this).m_Rewrite, state1);
		}
	}

	inline clang::SourceRange spelling_source_range(const clang::SourceRange& sr, Rewriter &Rewrite) {
		if (true || sr.getBegin().isMacroID()) {
			if (Rewrite.getSourceMgr().isMacroBodyExpansion(sr.getBegin())) {
				auto spelling_SR = clang::SourceRange{ Rewrite.getSourceMgr().getSpellingLoc(sr.getBegin()), Rewrite.getSourceMgr().getSpellingLoc(sr.getEnd()) };
				if (spelling_SR.isValid()) {
					auto spelling_text = Rewrite.getRewrittenText(spelling_SR);
					if (!spelling_text.empty()) {
						return spelling_SR;
					}
				}
			}
		}
		auto source_text = Rewrite.getRewrittenText(sr);
		if (source_text.empty()) {
			auto file_SR = clang::SourceRange{ Rewrite.getSourceMgr().getFileLoc(sr.getBegin()), Rewrite.getSourceMgr().getFileLoc(sr.getEnd()) };
			if (file_SR.isValid()) {
				return file_SR;
			}
		}
		return sr;
	}

	inline auto given_or_descendant_DeclRefExpr(const clang::Expr* E, clang::ASTContext& context) {
		auto DRE = dyn_cast<const clang::DeclRefExpr>(E);
		auto ME = dyn_cast<const clang::MemberExpr>(E);
		if (!DRE) {
			if (ME) {
				DRE = Tget_descendant_of_type<const clang::DeclRefExpr>(ME, context);
			} else {
				ME = Tget_descendant_of_type<const clang::MemberExpr>(E, context);
				if (ME) {
					DRE = Tget_descendant_of_type<const clang::DeclRefExpr>(ME, context);
				} else {
					DRE = Tget_descendant_of_type<const clang::DeclRefExpr>(E, context);
				}
			}
		}
		return DRE;
	}

	inline auto ddecl_of_expression_if_available(clang::Expr const *E, clang::ASTContext& context) {
		std::optional<clang::DeclaratorDecl const *> retval;
		if (!E) { return retval; }

		auto E_ii = IgnoreParenImpNoopCasts(E, context);
		auto DRE = given_or_descendant_DeclRefExpr(E_ii, context);
		if (DRE) {
			auto DD = llvm::dyn_cast<clang::DeclaratorDecl const>(DRE->getDecl());
			if (DD) {
				retval = DD;
			} else { assert(false); }
		}
		return retval;
	}

	class CCStyleCastReplacementCodeItem {
	public:
		std::string m_new_cast_prefix;
		std::string m_new_cast_suffix;
		std::string m_whole_cast_expression_replacement_text;
	};
	inline CCStyleCastReplacementCodeItem generate_c_style_cast_replacement_code(Rewriter &Rewrite, CTUState& state1, const clang::CStyleCastExpr* CSCE, std::optional<std::string> maybe_replacement_qtype_str = {}) {
		CCStyleCastReplacementCodeItem retval;
		if (CSCE) {
			auto CSCE_qtype = CSCE->getType();
			IF_DEBUG(std::string CSCE_qtype_str = CSCE_qtype.getAsString();)
			auto whole_cast_expression_SR = write_once_source_range(spelling_source_range(CSCE->getSourceRange(), Rewrite));
			auto cast_operation_SR = write_once_source_range(spelling_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, Rewrite));
			auto cast_preconversion_expression_SR = write_once_source_range(spelling_source_range(CSCE->getSubExprAsWritten()->getSourceRange(), Rewrite));

			if (whole_cast_expression_SR.isValid()) {
				IF_DEBUG(auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);)
				IF_DEBUG(auto whole_cast_expression_text = Rewrite.getRewrittenText(whole_cast_expression_SR);)
				auto cast_preconversion_expression_text = Rewrite.getRewrittenText(cast_preconversion_expression_SR);

				if ((!(maybe_replacement_qtype_str.has_value())) && state1.m_ast_context_ptr) {
					auto& context_ref = *(state1.m_ast_context_ptr);
					/* At this point we haven't been given any (direct) information about what the cast type should be 
					converted to. So we going to look to see if this cast operation is embedded in some context from 
					which we know how to extract information about the what type it should be converted to. */

					/* First we look for a containing call expression. If our cast expression is part of a call expression 
					argument then we probably don't want to consider any context outside of the call expression. */
					const auto containing_CE = Tget_containing_element_of_type<clang::CallExpr>(CSCE, context_ref);
					auto is_outside_of_containing_CE = [containing_CE, &context_ref](auto* node_ptr1) {
							if (!containing_CE) {
								return false;
							}
							const auto containing_CE2 = Tget_containing_element_of_type<clang::CallExpr>(node_ptr1, context_ref);
							if (containing_CE2 == containing_CE) {
								return false;
							}
							return true;
						};

					/* First we're going to try to determine if the cast expression is part of a constraining binary 
					operation. */
					auto containing_BO = Tget_containing_element_of_type<clang::BinaryOperator>(CSCE, context_ref);
					while (containing_BO) {
						const auto opcode_str = std::string(containing_BO->getOpcodeStr());
						if ((clang::BinaryOperator::Opcode::BO_Assign == containing_BO->getOpcode()) 
							|| ("==" == opcode_str) || ("!=" == opcode_str) || ("<" == opcode_str) || (">" == opcode_str) || ("<=" == opcode_str) || (">=" == opcode_str)) {

							break;
						}
						containing_BO = Tget_containing_element_of_type<clang::BinaryOperator>(containing_BO, context_ref);
					}
					if (containing_BO && !is_outside_of_containing_CE(containing_BO)) {
						auto precasted_expr_res = infer_array_type_info_from_stmt(*(CSCE->getSubExpr()), "", state1);
						clang::DeclaratorDecl const* precasted_DD = precasted_expr_res.ddecl_cptr;

						/* Ok, so now we're looking for a DeclRefExpr that is part of the binary operation, but ouside of our 
						cast expression, whose associated CDDeclConversionState from which we can extract information about
						what type the cast type should bre converted to. */
						auto res1 = infer_array_type_info_from_stmt(*(containing_BO->getLHS()), "", state1);
						if ((!(res1.ddecl_cptr)) || (res1.ddecl_cptr == precasted_DD)) {
							res1 = infer_array_type_info_from_stmt(*(containing_BO->getRHS()), "", state1);
						}
						if (res1.ddecl_cptr && (res1.ddecl_cptr != precasted_DD)) {
							CIndirectionStateStack CSCE_qtype_indirection_state_stack;
							auto CSCE_direct_rhs_qtype = populateQTypeIndirectionStack(CSCE_qtype_indirection_state_stack, CSCE_qtype);
							CIndirectionStateStack related_ddecl_qtype_indirection_state_stack;
							auto related_ddecl_direct_rhs_qtype = populateQTypeIndirectionStack(related_ddecl_qtype_indirection_state_stack, res1.ddecl_cptr->getType());
							bool related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = true;
							if ((CSCE_qtype_indirection_state_stack.size() + res1.indirection_level) == related_ddecl_qtype_indirection_state_stack.size()) {
								related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = false;
							}
							if (related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								std::string CSCE_qtype_str = CSCE_qtype.getAsString();
								if (("void *" == CSCE_qtype_str) || ("const void *" == CSCE_qtype_str)) {
									/* If the cast expression is a `void *`, it could be legitimate to cast to a pointer with more levels 
									of indirection than the one apparent one thet `void *` does. */
									related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = false;
								}
							}
							if (!related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								std::string related_ddecl_qtype_str = res1.ddecl_cptr->getType().getAsString();
								if (("void *" == related_ddecl_qtype_str) || ("const void *" == related_ddecl_qtype_str)) {
									/* While the fact that the related ddecl is a `void *` prevents us from concluding that its degree of 
									indirection is mismatched with that of our cast expression, it also means that meaningful information 
									about the original type of the value it holds is not available. */
									related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = true;
								}
							}

							if ((0 == res1.indirection_level) && !related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								/* For now we only support the case where there are no (net) dereferencing or address_of operations. */

								auto res4 = generate_declaration_replacement_code(res1.ddecl_cptr, Rewrite, &state1, state1.m_ddecl_conversion_state_map);
								if ("" != res4.m_replacement_return_type_str) {
									maybe_replacement_qtype_str = res4.m_replacement_return_type_str;
								} else {
									int q = 5;
								}
							} else {
								/* todo: */
								int q = 5;
							}
						}
					}
					if (!(maybe_replacement_qtype_str.has_value())) {
						/* We're still haven't found a suitable associated DeclRefExpr from which to obtain the neede type 
						conversion information. Next we'll try to determine if the cast expression is part of (an 
						initialization expression of) a (variable) declaration. */
						auto containing_VD = Tget_containing_element_of_type<clang::VarDecl>(CSCE, context_ref);
						if (containing_VD && !is_outside_of_containing_CE(containing_VD)) {
							CIndirectionStateStack CSCE_qtype_indirection_state_stack;
							auto CSCE_direct_rhs_qtype = populateQTypeIndirectionStack(CSCE_qtype_indirection_state_stack, CSCE_qtype);
							CIndirectionStateStack related_ddecl_qtype_indirection_state_stack;
							auto related_ddecl_direct_rhs_qtype = populateQTypeIndirectionStack(related_ddecl_qtype_indirection_state_stack, containing_VD->getType());
							bool related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = true;
							if (CSCE_qtype_indirection_state_stack.size() == related_ddecl_qtype_indirection_state_stack.size()) {
								related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = false;
							}
							if (related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								std::string CSCE_qtype_str = CSCE_qtype.getAsString();
								if (("void *" == CSCE_qtype_str) || ("const void *" == CSCE_qtype_str)) {
									related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = false;
								}
							}
							if (!related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								std::string related_ddecl_qtype_str = containing_VD->getType().getAsString();
								if (("void *" == related_ddecl_qtype_str) || ("const void *" == related_ddecl_qtype_str)) {
									/* While the fact that the related ddecl is a `void *` prevents us from concluding that its degree of 
									indirection is mismatched with that of our cast expression, it also means that meaningful information 
									about the original type of the value it holds is not available. */
									related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info = true;
								}
							}

							if (!related_ddecl_type_does_not_seem_to_match_or_has_no_useful_info) {
								auto res4 = generate_declaration_replacement_code(containing_VD, Rewrite, &state1, state1.m_ddecl_conversion_state_map);
								if ("" != res4.m_replacement_return_type_str) {
									maybe_replacement_qtype_str = res4.m_replacement_return_type_str;

									auto VD = dyn_cast<const clang::VarDecl>(containing_VD);
									if (VD && VD->hasInit()) {
										const auto init_E = VD->getInit();
										assert(init_E);
										/* So this cast expression has seems to have an ancestor variable declaration that has an initialization 
										expression. So that cast expression that we are generating the replacement code for is likely a 
										sub-expression if that initialization expression. When the associated CExprConversionState is created 
										for the cast expression, it will search its ancestor expressions for one that has its own associated 
										CExprConversionState, and if it finds one, will establish a relationship between them such that any 
										changes to the descendant expression will be reflected in the rendering of the ancestor expression. So 
										we want to make sure that that initalization expression indeed does have an associated 
										CExprConversionState, so the relationship will indeed be established. Otherwise the initialization 
										expression might be rendered in a way that overwrites and does not reflect any changes to the cast 
										expression. The following line ensures that the initialization expression has an associated 
										CExprConversionState. (This may be redundant now.) */
										state1.get_expr_conversion_state_ref(*init_E, Rewrite);
									}
								} else {
									int q = 5;
								}
							} else {
								int q = 5;
							}
						} else {
							int q = 5;
						}
					}
				}
				if (!(maybe_replacement_qtype_str.has_value())) {
					auto containing_D = Tget_containing_element_of_type<clang::Decl>(CSCE, *(state1.m_ast_context_ptr));
					std::optional<clang::TypeLoc> maybe_typeLoc;
					auto* tsi_ptr = CSCE->getTypeInfoAsWritten();
					if (tsi_ptr) {
						maybe_typeLoc = typeLoc_if_available(*tsi_ptr);
					}

					maybe_replacement_qtype_str = generate_qtype_replacement_code(CSCE->getType(), Rewrite, &state1, EIsFunctionParam::No, {}/*maybe_storage_duration*/, containing_D, maybe_typeLoc);
				}
				auto replacement_qtype_str = maybe_replacement_qtype_str.value();
				/* remove trailing whitespace */
				while (!replacement_qtype_str.empty()) {
					if (isspace(replacement_qtype_str.back())) {
						replacement_qtype_str.pop_back();
					} else {
						break;
					}
				}

				std::string new_cast_prefix;
				std::string new_cast_suffix;
				bool preconversion_expression_is_void_star = false;
				bool converted_expression_is_void_star = false;
				if (CSCE->getType()->isPointerType() && CSCE->getSubExpr()->getType()->isPointerType()) {
					const std::string og_converted_pointee_qtype_str = CSCE->getType()->getPointeeType().getAsString();
					const std::string og_preconversion_pointee_qtype_str = CSCE->getSubExpr()->getType()->getPointeeType().getAsString();
					preconversion_expression_is_void_star = ("void" == og_preconversion_pointee_qtype_str) || ("const void" == og_preconversion_pointee_qtype_str);
					converted_expression_is_void_star = ("void" == og_converted_pointee_qtype_str) || ("const void" == og_converted_pointee_qtype_str);
				}
				if (new_cast_prefix.empty()) {
					if (preconversion_expression_is_void_star || converted_expression_is_void_star) {
						if ("Dual" == ConvertMode) {
							new_cast_prefix = "MSE_LH_CAST("
								+ replacement_qtype_str + ", ";
						} else {
							new_cast_prefix = "("
								+ replacement_qtype_str + ")(";
						}
					} else {
						if ("Dual" == ConvertMode) {
							new_cast_prefix = "MSE_LH_UNSAFE_CAST("
								+ replacement_qtype_str + ", ";
						} else {
							new_cast_prefix = "mse::us::lh::unsafe_cast<"
								+ replacement_qtype_str + ">(";
						}
					}
					new_cast_suffix = ")";
				}
				retval.m_new_cast_prefix = new_cast_prefix;
				retval.m_new_cast_suffix = new_cast_suffix;
				retval.m_whole_cast_expression_replacement_text = new_cast_prefix + cast_preconversion_expression_text + new_cast_suffix;
			}
		}

		return retval;
	}

	void CTargetConstrainsCStyleCastExprArray2ReplacementAction::do_replacement(CTUState& state1) const {
		Rewriter &Rewrite = m_Rewrite;

		assert((*this).m_ddecl_indirection.m_ddecl_cptr);
		assert((*this).m_c_style_cast_expr_cptr);

#ifndef NDEBUG
		auto SR = cm1_adj_nice_source_range((*this).m_ddecl_indirection.m_ddecl_cptr->getSourceRange(), state1, Rewrite);
		DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
		DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
		if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto res1 = generate_declaration_replacement_code((*this).m_ddecl_indirection.m_ddecl_cptr, Rewrite, &state1, state1.m_ddecl_conversion_state_map);

		if (ConvertToSCPP && (true || res1.m_changed_from_original)) {
			auto whole_cast_expression_SR = write_once_source_range({ m_c_style_cast_expr_cptr->getBeginLoc(), m_c_style_cast_expr_cptr->getEndLoc() });
			if (whole_cast_expression_SR.isValid()) {
				IF_DEBUG(auto whole_cast_expression_text = Rewrite.getRewrittenText(whole_cast_expression_SR);)
				auto replacement_qtype_str = res1.m_replacement_type_str;

				auto res = generate_c_style_cast_replacement_code(Rewrite, state1, m_c_style_cast_expr_cptr, replacement_qtype_str);

				CExprTextReplacementAction(Rewrite, m_c_style_cast_expr_cptr, res.m_whole_cast_expression_replacement_text).do_replacement(state1);
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
		if ((COND != nullptr) && (LHS != nullptr) && (RHS != nullptr)) {
			auto& Rewrite = m_Rewrite;
			IF_DEBUG(std::string LHS_qtype_str = LHS->getType().getAsString();)
			IF_DEBUG(std::string RHS_qtype_str = RHS->getType().getAsString();)

#ifndef NDEBUG
			auto SR = cm1_adj_nice_source_range(COND->getSourceRange(), state1, Rewrite);
			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			auto COSR = write_once_source_range(cm1_adj_nice_source_range(CO->getSourceRange(), state1, (*this).m_Rewrite));
			auto cond_SR = write_once_source_range(cm1_adj_nice_source_range(COND->getSourceRange(), state1, (*this).m_Rewrite));
			auto lhs_SR = write_once_source_range(cm1_adj_nice_source_range(LHS->getSourceRange(), state1, (*this).m_Rewrite));
			auto rhs_SR = write_once_source_range(cm1_adj_nice_source_range(RHS->getSourceRange(), state1, (*this).m_Rewrite));
			if ((COSR.isValid()) && (cond_SR.isValid()) && (lhs_SR.isValid()) && (rhs_SR.isValid())) {
				auto lhs_inference_info = infer_array_type_info_from_stmt(*LHS, "", state1, lhs_DD);
				bool lhs_is_known_to_be_an_array = false;
				bool lhs_is_dynamic_array = false;
				bool lhs_is_native_array = false;
				bool lhs_is_variously_native_and_dynamic_array = false;
				bool lhs_update_flag = false;
				bool lhs_is_known_to_be_a_pointer_target = false;
				std::optional<CFunctionTypeState> lhs_maybe_function_state;
				CDDeclConversionState* lhs_ddcs_ptr = nullptr;
				if (lhs_DD != nullptr) {
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &m_Rewrite);
					lhs_ddcs_ptr = &ddcs_ref;
					/* At the moment we only support the case where the value option expressions are
					* just declared variables. */

					/* If a declaration for this conditional operator option is available, then
					we'll mark that declaration as ineligible for "xscope status". The idea
					being that conditional operator options are likely the (obfuscated) source of an
					assignment operation (including being passed as a function argument). */
					ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
					state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*lhs_DD, CDDeclIndirection::no_indirection));
					size_t indirection_level = 0;
					for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
						indirection_state.set_xscope_eligibility(false);
						state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*lhs_DD, indirection_level));
						++indirection_level;
					}
					lhs_update_flag |= true;

					if (lhs_inference_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
						auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(lhs_inference_info.indirection_level);
						lhs_maybe_function_state = indirection_state_ref.m_function_type_state;
						if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
							lhs_is_known_to_be_an_array = true;
							if (indirection_state_ref.is_known_to_have_malloc_target() && indirection_state_ref.is_known_to_have_non_malloc_target()) {
								lhs_is_variously_native_and_dynamic_array = true;
							} else if (indirection_state_ref.is_known_to_have_malloc_target()) {
								lhs_is_dynamic_array = true;
							} else if (indirection_state_ref.is_known_to_have_non_malloc_target()) {
								lhs_is_native_array = true;
							} 
						}
					} else {
						auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
						lhs_maybe_function_state = indirection_state_ref.m_function_type_state;
						if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
							lhs_is_known_to_be_a_pointer_target = true;
						}
					}
					if (lhs_update_flag) {
						update_declaration(*lhs_DD, (*this).m_Rewrite, state1);
					}
				}

				auto rhs_inference_info = infer_array_type_info_from_stmt(*RHS, "", state1, rhs_DD);
				bool rhs_is_known_to_be_an_array = false;
				bool rhs_is_dynamic_array = false;
				bool rhs_is_native_array = false;
				bool rhs_is_variously_native_and_dynamic_array = false;
				bool rhs_update_flag = false;
				bool rhs_is_known_to_be_a_pointer_target = false;
				std::optional<CFunctionTypeState> rhs_maybe_function_state;
				if (rhs_DD != nullptr) {
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_DD, &m_Rewrite);

					/* If a declaration for this conditional operator option is available, then
					we'll mark that declaration as ineligible for "xscope status". The idea
					being that conditional operator options are likely the (obfuscated) source of an
					assignment operation (including being passed as a function argument). */
					ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
					state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*rhs_DD, CDDeclIndirection::no_indirection));
					size_t indirection_level = 0;
					for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
						indirection_state.set_xscope_eligibility(false);
						state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*rhs_DD, indirection_level));
						++indirection_level;
					}
					rhs_update_flag |= true;

					if (rhs_inference_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
						auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(rhs_inference_info.indirection_level);
						rhs_maybe_function_state = indirection_state_ref.m_function_type_state;
						if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
							rhs_is_known_to_be_an_array = true;
							if (indirection_state_ref.is_known_to_have_malloc_target() && indirection_state_ref.is_known_to_have_non_malloc_target()) {
								rhs_is_variously_native_and_dynamic_array = true;
							} else if (indirection_state_ref.is_known_to_have_malloc_target()) {
								rhs_is_dynamic_array = true;
							} else if (indirection_state_ref.is_known_to_have_non_malloc_target()) {
								rhs_is_native_array = true;
							} 
						}
						if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
							rhs_is_known_to_be_a_pointer_target = true;
						}

						/* Whether or not the lhs argument(/alternative/option/branch) is known to point to malloc()ed 
						and/or non-malloc()ed targets wouldn't necessarily constrain the rhs argument's type. But if 
						the lhs argument is known to be used as an array/buffer iterator, then presumably the rhs 
						argument should probably also be (constrained to be) an iterator type. And vice versa. */
						if (lhs_is_known_to_be_an_array && !rhs_is_known_to_be_an_array) {
							auto& rhs_ddcs_ref = ddcs_ref;
							if (rhs_inference_info.indirection_level < rhs_ddcs_ref.m_indirection_state_stack.size()) {
								auto rhs_indirection = CDDeclIndirection{ *rhs_DD, rhs_inference_info.indirection_level };
								auto& rhs_indirection_state_ref = rhs_ddcs_ref.m_indirection_state_stack.at(rhs_inference_info.indirection_level);
								rhs_indirection_state_ref.set_is_known_to_be_used_as_an_array_iterator(true);
								rhs_update_flag |= true;
								state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
								if (rhs_indirection_state_ref.is_known_to_have_malloc_target()) {
									state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
								}
								if (rhs_indirection_state_ref.is_known_to_have_non_malloc_target()) {
									state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
								}
								if (rhs_update_flag) {
									update_declaration(*rhs_DD, (*this).m_Rewrite, state1);
								}
							} else {
								int q = 5;
							}
						}

						if (lhs_DD && lhs_ddcs_ptr) {
							auto& lhs_ddcs_ref = *lhs_ddcs_ptr;
							if (rhs_is_known_to_be_an_array && !lhs_is_known_to_be_an_array) {
								if (lhs_inference_info.indirection_level < lhs_ddcs_ref.m_indirection_state_stack.size()) {
									auto lhs_indirection = CDDeclIndirection{ *lhs_DD, lhs_inference_info.indirection_level };
									auto& lhs_indirection_state_ref = lhs_ddcs_ref.m_indirection_state_stack.at(lhs_inference_info.indirection_level);
									lhs_indirection_state_ref.set_is_known_to_be_used_as_an_array_iterator(true);
									lhs_update_flag |= true;
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
									if (lhs_indirection_state_ref.is_known_to_have_malloc_target()) {
										state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
									}
									if (lhs_indirection_state_ref.is_known_to_have_non_malloc_target()) {
										state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
									}
									if (lhs_update_flag) {
										update_declaration(*lhs_DD, (*this).m_Rewrite, state1);
									}
								} else {
									int q = 5;
								}
							}
						}
					} else {
						auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
						rhs_maybe_function_state = indirection_state_ref.m_function_type_state;
						if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
							rhs_is_known_to_be_a_pointer_target = true;
						}
					}
					if (rhs_update_flag) {
						update_declaration(*rhs_DD, (*this).m_Rewrite, state1);
					}
				}

				bool array_needed_to_be_wrapped = false;
				if ((lhs_is_known_to_be_an_array || rhs_is_known_to_be_an_array) && LHS->getType()->isPointerType()) {
					bool lhs_needs_to_be_wrapped = false;
					if ((lhs_is_dynamic_array && (!rhs_is_dynamic_array)) || (lhs_is_native_array && (!rhs_is_native_array))
						|| ((!lhs_is_known_to_be_an_array) && rhs_is_known_to_be_an_array)) {
						lhs_needs_to_be_wrapped = true;
						array_needed_to_be_wrapped = true;
					}
					bool rhs_needs_to_be_wrapped = false;
					if ((rhs_is_dynamic_array && (!lhs_is_dynamic_array)) || (rhs_is_native_array && (!lhs_is_native_array))
						|| ((!rhs_is_known_to_be_an_array) && lhs_is_known_to_be_an_array)) {
						rhs_needs_to_be_wrapped = true;
						array_needed_to_be_wrapped = true;
					}
				}

				auto& Rewrite = m_Rewrite;
				auto var_DD = m_var_DD;
				auto var_indirection_level = m_var_indirection_level;

				auto lambda = [&Rewrite, &state1, CO, LHS, RHS, lhs_DD, rhs_DD, var_DD, var_indirection_level]() {
					if (!(CO && LHS && RHS)) { assert(false); return; }
#ifndef NDEBUG
					auto SR = cm1_adj_nice_source_range(CO->getSourceRange(), state1, Rewrite);
					DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
					DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					auto lhs_inference_info = infer_array_type_info_from_stmt(*LHS, "", state1, lhs_DD);
					bool lhs_is_known_to_be_an_array = false;
					bool lhs_is_dynamic_array = false;
					bool lhs_is_native_array = false;
					bool lhs_is_variously_native_and_dynamic_array = false;
					bool lhs_update_flag = false;
					bool lhs_is_known_to_be_a_pointer_target = false;
					bool lhs_is_non_modifiable = false;
					std::optional<CFunctionTypeState> lhs_maybe_function_state;
					CDDeclConversionState* lhs_ddcs_ptr = nullptr;
					if (lhs_DD != nullptr) {
						if (state1.m_ast_context_ptr) {
							lhs_is_non_modifiable = is_non_modifiable(*lhs_DD, *(state1.m_ast_context_ptr), Rewrite, state1);
						}

						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &Rewrite);
						lhs_ddcs_ptr = &ddcs_ref;

						if (lhs_inference_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(lhs_inference_info.indirection_level);
							lhs_maybe_function_state = indirection_state_ref.m_function_type_state;

							if (lhs_is_non_modifiable && LHS->getType()->isPointerType()) {
								/* We consider non-modifiable raw pointer expressions to be a pointer type that could be pointing to either 
								a malloc()ed target or a non-malloc()ed target.*/
								auto lhs_indirection = CDDeclIndirection{ *lhs_DD, lhs_inference_info.indirection_level };
								if (!indirection_state_ref.is_known_to_have_malloc_target()) {
									indirection_state_ref.set_is_known_to_have_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
									if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
									}
								}
								if (!indirection_state_ref.is_known_to_have_non_malloc_target()) {
									indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_indirection);
									if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_indirection);
									}
								}
							}

							if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								lhs_is_known_to_be_an_array = true;
								if (indirection_state_ref.is_known_to_have_malloc_target() && indirection_state_ref.is_known_to_have_non_malloc_target()) {
									lhs_is_variously_native_and_dynamic_array = true;
								} else if (indirection_state_ref.is_known_to_have_malloc_target()) {
									lhs_is_dynamic_array = true;
								} else if (indirection_state_ref.is_known_to_have_non_malloc_target()) {
									lhs_is_native_array = true;
								} 
							}
						} else {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
							lhs_maybe_function_state = indirection_state_ref.m_function_type_state;
							if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
								lhs_is_known_to_be_a_pointer_target = true;
							}
						}
						if (lhs_update_flag) {
							update_declaration(*lhs_DD, Rewrite, state1);
						}
					} else {
						const auto LHS_ii = state1.m_ast_context_ptr ? IgnoreParenImpNoopCasts(LHS, *(state1.m_ast_context_ptr)) : IgnoreParenImpCasts(LHS);
						auto SL = dyn_cast<const clang::StringLiteral>(LHS_ii);
						if (SL) {
							lhs_is_known_to_be_an_array = true;
							//lhs_is_native_array = true;
							lhs_is_variously_native_and_dynamic_array = true;
							lhs_is_non_modifiable = true;
						}
					}

					auto rhs_inference_info = infer_array_type_info_from_stmt(*RHS, "", state1, rhs_DD);
					bool rhs_is_known_to_be_an_array = false;
					bool rhs_is_dynamic_array = false;
					bool rhs_is_native_array = false;
					bool rhs_is_variously_native_and_dynamic_array = false;
					bool rhs_update_flag = false;
					bool rhs_is_known_to_be_a_pointer_target = false;
					bool rhs_is_non_modifiable = false;
					std::optional<CFunctionTypeState> rhs_maybe_function_state;
					if (rhs_DD != nullptr) {
						if (state1.m_ast_context_ptr) {
							rhs_is_non_modifiable = is_non_modifiable(*rhs_DD, *(state1.m_ast_context_ptr), Rewrite, state1);
						}

						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_DD, &Rewrite);

						if (rhs_inference_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(rhs_inference_info.indirection_level);
							rhs_maybe_function_state = indirection_state_ref.m_function_type_state;

							if (rhs_is_non_modifiable && RHS->getType()->isPointerType()) {
								/* We consider non-modifiable raw pointer expressions to be a pointer type that could be pointing to either 
								a malloc()ed target or a non-malloc()ed target.*/
								auto rhs_indirection = CDDeclIndirection{ *rhs_DD, rhs_inference_info.indirection_level };
								if (!indirection_state_ref.is_known_to_have_malloc_target()) {
									indirection_state_ref.set_is_known_to_have_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
									if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
									}
								}
								if (!indirection_state_ref.is_known_to_have_non_malloc_target()) {
									indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, rhs_indirection);
									if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, rhs_indirection);
									}
								}
							}

							if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								rhs_is_known_to_be_an_array = true;
								if (indirection_state_ref.is_known_to_have_malloc_target() && indirection_state_ref.is_known_to_have_non_malloc_target()) {
									rhs_is_variously_native_and_dynamic_array = true;
								} else if (indirection_state_ref.is_known_to_have_malloc_target()) {
									rhs_is_dynamic_array = true;
								} else if (indirection_state_ref.is_known_to_have_non_malloc_target()) {
									rhs_is_native_array = true;
								} 
							}
							if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
								rhs_is_known_to_be_a_pointer_target = true;
							}
						} else {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state;
							rhs_maybe_function_state = indirection_state_ref.m_function_type_state;
							if (indirection_state_ref.is_known_to_be_a_pointer_target()) {
								rhs_is_known_to_be_a_pointer_target = true;
							}
						}
						if (rhs_update_flag) {
							update_declaration(*rhs_DD, Rewrite, state1);
						}
					} else {
						const auto RHS_ii = state1.m_ast_context_ptr ? IgnoreParenImpNoopCasts(RHS, *(state1.m_ast_context_ptr)) : IgnoreParenImpCasts(RHS);
						auto SL = dyn_cast<const clang::StringLiteral>(RHS_ii);
						if (SL) {
							rhs_is_known_to_be_an_array = true;
							//rhs_is_native_array = true;
							rhs_is_variously_native_and_dynamic_array = true;
							rhs_is_non_modifiable = true;
						}
					}

					auto& ecs_ref = state1.get_expr_conversion_state_ref<CConditionalOperatorExprConversionState>(*CO, Rewrite);
					/* Even though ecs_ref should be referencing an object of type CConditionalOperatorExprConversionState, 
					ecs_ref's type is actually a reference to its base class. So we'll up(/down?) cast it to (a reference to) 
					the object's actual type. As rtti is disabled, we can't use dynamic_cast<>(). */
					assert("conditional operator" == ecs_ref.species());
					auto& cocs_ref = static_cast<CConditionalOperatorExprConversionState&>(ecs_ref);

					cocs_ref.m_arg_prefix_str = "";
					cocs_ref.m_arg_suffix_str = "";

					bool array_needed_to_be_wrapped = false;

					bool is_a_function_or_function_pointer_value_type = false;
					if (var_DD) {
						is_a_function_or_function_pointer_value_type |= var_DD->getType()->isFunctionPointerType();
					}
					if (LHS && !is_a_function_or_function_pointer_value_type) {
						is_a_function_or_function_pointer_value_type |= (LHS->getType()->isFunctionType() || LHS->getType()->isFunctionPointerType());
					}
					if (RHS && !is_a_function_or_function_pointer_value_type) {
						is_a_function_or_function_pointer_value_type |= (RHS->getType()->isFunctionType() || RHS->getType()->isFunctionPointerType());
					}
					if (is_a_function_or_function_pointer_value_type) {
						auto& changed_function_state = (lhs_maybe_function_state.has_value() && lhs_maybe_function_state.value().has_been_changed()) 
							? lhs_maybe_function_state.value() : rhs_maybe_function_state.value();
						if (lhs_DD && rhs_DD) {
							if (state1.m_ast_context_ptr) {
								struct CLoneModifiableArgInfo {
									decltype(lhs_DD) DD = nullptr;
									decltype(lhs_inference_info.indirection_level) indirection_level = 0;
									decltype(LHS) EX = nullptr;
									decltype(LHS) non_modifiable_EX = nullptr;
								};
								std::optional<CLoneModifiableArgInfo> maybe_lone_modifiable_arg_info;

								/* In this case, where we're dealing with the functions themselves rather than invocations of the 
								functions, we need to use a slightly different evaluation method of whether the lhs and rhs are 
								"non-modifiable". Certain known unsafe functions that would be substituted with safe replacements 
								when used in a call expression, will not be replaced when used as a (function or function pointer) 
								value. */
								auto lhs_is_non_modifiable = is_non_modifiable<options_t<do_not_exclude_functions_with_conversions_t> >(*lhs_DD, *(state1.m_ast_context_ptr), Rewrite, state1);
								auto rhs_is_non_modifiable = is_non_modifiable<options_t<do_not_exclude_functions_with_conversions_t> >(*rhs_DD, *(state1.m_ast_context_ptr), Rewrite, state1);
								if (rhs_is_non_modifiable && (!lhs_is_non_modifiable)) {
									maybe_lone_modifiable_arg_info = { lhs_DD, lhs_inference_info.indirection_level, LHS, RHS };
								} else if (lhs_is_non_modifiable && (!rhs_is_non_modifiable)) {
									maybe_lone_modifiable_arg_info = { rhs_DD, rhs_inference_info.indirection_level, RHS, LHS };
								}
								if (maybe_lone_modifiable_arg_info.has_value()) {
									auto& lone_modifiable_arg_info = maybe_lone_modifiable_arg_info.value();
									/* Ok, one of the conditional operator (function) arguments(/alternatives/options/branches) seems to be 
									non-modifiable while the other one doesn't seem to be. So we're going to replace the non-modifiable one 
									with a wrapper function that has an interface that's hopefully compatible with the other (changed) 
									(function) argument. */
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(lone_modifiable_arg_info.DD), &Rewrite);
									auto indirection_state_stack_of_expression = ddcs_ref.m_indirection_state_stack;
									indirection_state_stack_of_expression.clear();
									if (lone_modifiable_arg_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
										for (size_t i = size_t(lone_modifiable_arg_info.indirection_level); ddcs_ref.m_indirection_state_stack.size() > i; ++i) {
											indirection_state_stack_of_expression.push_back(ddcs_ref.m_indirection_state_stack.at(i));
										}
									}

									auto direct_qtype_str = indirection_state_stack_of_expression.m_direct_type_state.current_return_qtype_str();

									auto& function_type_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_function_type_state;
									if (function_type_state_ref.m_function_decl_ptr) {
										clang::FunctionProtoTypeLoc functionProtoTypeLoc;
										/* I don't think functionProtoTypeLoc will actually be used in this case (so it should be ok if it 
										just has the default value), but current_params_string() does take it as a parameter so we'll 
										assign it a proper value if available. */
										if (function_type_state_ref.m_maybe_functionProtoTypeLoc.has_value()) {
											IF_DEBUG(auto typeLocClass = definition_TypeLoc(function_type_state_ref.m_maybe_functionProtoTypeLoc.value()).getTypeLocClass();)
											functionProtoTypeLoc = definition_TypeLoc(function_type_state_ref.m_maybe_functionProtoTypeLoc.value()).getAsAdjusted<clang::FunctionProtoTypeLoc>();
										}

										auto direct_qtype_str2 = ddcs_ref.current_direct_return_qtype_str();
										direct_qtype_str2 += current_params_string(Rewrite, function_type_state_ref
											, functionProtoTypeLoc, true/*suppress_modifications*/, &state1);

										direct_qtype_str = direct_qtype_str2;
									}

									if (0 == indirection_state_stack_of_expression.size()) {
										/* The expressions seems to not be indirect. The type of the expression is the "direct" type. */
										if (lone_modifiable_arg_info.EX->getType().isConstQualified()) {
											if (!(ddcs_ref.direct_type_state_ref().is_const())) {
												/* Ok, so the "direct" type derived from the DDecl seems to be non-const, while the 
												"direct" type of the actual type of the conditional operator argument(/alternative/option/branch) 
												expressions is const. So we'll just add a const qualifier to the "direct" type. */
												direct_qtype_str = "const " + direct_qtype_str;
											}
										}
									}

									std::string arg_prefix_str;
									std::string function_pointer_type_str;
									std::string make_fn_wrapper_prefix_str;
									if ("Dual" == ConvertMode) {
										auto direct_return_qtype_str = ddcs_ref.current_direct_return_qtype_str();
										function_pointer_type_str = std::string("(MSE_LH_FUNCTION_POINTER_TYPE_PREFIX") + direct_return_qtype_str 
											+ "MSE_LH_FUNCTION_POINTER_TYPE_SUFFIX(" + changed_function_state.m_params_current_str  + "))";
										arg_prefix_str = "(" + function_pointer_type_str + ")(";
										make_fn_wrapper_prefix_str = "MSE_LH_UNSAFE_MAKE_FN_WRAPPER(";
									} else {
										function_pointer_type_str = std::string("mse::lh::TNativeFunctionPointerReplacement<") + direct_qtype_str + ">";
										arg_prefix_str = function_pointer_type_str + "(";
										make_fn_wrapper_prefix_str = "mse::us::lh::unsafe_make_fn_wrapper(";
									}

									std::string arg_suffix_str;
									std::string make_fn_wrapper_suffix_str;
									if ("Dual" == ConvertMode) {
										arg_suffix_str = ")";
										make_fn_wrapper_suffix_str = std::string(", ") + function_pointer_type_str + "())";
									} else {
										arg_suffix_str = ")";
										make_fn_wrapper_suffix_str = std::string(", ") + function_pointer_type_str + "())";
									}

									auto& ecs_ref = state1.get_expr_conversion_state_ref(*(lone_modifiable_arg_info.non_modifiable_EX), Rewrite);
									const auto l_text_modifier = CWrapExprTextModifier(make_fn_wrapper_prefix_str, make_fn_wrapper_suffix_str);
									bool seems_to_be_already_applied = ((1 <= ecs_ref.m_expr_text_modifier_stack.size()) && ("wrap" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) 
										&& (l_text_modifier.is_equal_to(*(ecs_ref.m_expr_text_modifier_stack.back()))));
									if (!seems_to_be_already_applied) {
										auto shptr2 = std::make_shared<CWrapExprTextModifier>(make_fn_wrapper_prefix_str, make_fn_wrapper_suffix_str);
										ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
										ecs_ref.update_current_text();
									}

									cocs_ref.m_arg_prefix_str += arg_prefix_str;
									cocs_ref.m_arg_suffix_str = cocs_ref.m_arg_suffix_str + ")";
									cocs_ref.m_lhs_needs_to_be_cast = true;
									cocs_ref.m_rhs_needs_to_be_cast = true;

								} else if (lhs_is_non_modifiable && rhs_is_non_modifiable) {
									/* Both arguments(/alternatives/options/branches) of the conditional operator are non-modifiable 
									so they presumably wouldn't need any casting or wrapping to conform with each other, but it still 
									might be needed to for compatibility with the assignment target of the conditional expression (if 
									one is provided). */
									bool var_is_non_modifiable = false;
									if (var_DD) {
										var_is_non_modifiable = is_non_modifiable(*var_DD, *(state1.m_ast_context_ptr), Rewrite, state1);

										if (!var_is_non_modifiable) {
											auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*var_DD, &Rewrite);

											if (var_indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
												auto indirection_state_stack_of_asignee = ddcs_ref.m_indirection_state_stack;
												/* This clears the base vector, but leaves the other members intact. */
												indirection_state_stack_of_asignee.clear();
												for (size_t i = size_t(var_indirection_level); ddcs_ref.m_indirection_state_stack.size() > i; ++i) {
													indirection_state_stack_of_asignee.push_back(ddcs_ref.m_indirection_state_stack.at(i));
												}

												auto res3 = generate_type_indirection_prefix_and_suffix(indirection_state_stack_of_asignee, Rewrite, EIsFunctionParam::No, {}/*maybe_storage_duration*/, &state1);

												auto new_asignee_qtype_str = res3.m_complete_type_str;

												if (0 == indirection_state_stack_of_asignee.size()) {
													/* The type of the LHS expression does not seem to be indirect. So the LHS type is the 
													"direct" type. */
													if (LHS->getType().isConstQualified()) {
														if (!(ddcs_ref.direct_type_state_ref().is_const())) {
															/* Ok, so the "direct" type derived from the DDecl seems to be non-const, while the 
															LHS expressions is const. So we'll just add a const qualifier to the "direct" type. */
															new_asignee_qtype_str = "const " + new_asignee_qtype_str;
														}
													}
												}

												std::string arg_prefix_str;
												std::string& function_pointer_type_str = new_asignee_qtype_str;
												std::string make_fn_wrapper_prefix_str;
												if ("Dual" == ConvertMode) {
													make_fn_wrapper_prefix_str = "MSE_LH_UNSAFE_MAKE_FN_WRAPPER(";
												} else {
													make_fn_wrapper_prefix_str = "mse::us::lh::unsafe_make_fn_wrapper(";
												}

												std::string arg_suffix_str;
												std::string make_fn_wrapper_suffix_str;
												if ("Dual" == ConvertMode) {
													make_fn_wrapper_suffix_str = std::string(", ") + function_pointer_type_str + "())";
												} else {
													make_fn_wrapper_suffix_str = std::string(", ") + function_pointer_type_str + "())";
												}

												auto apply_to_expr_conversion_state = [&](clang::Expr const * E) {
													auto& ecs_ref = state1.get_expr_conversion_state_ref(*E, Rewrite);
													const auto l_text_modifier = CWrapExprTextModifier(make_fn_wrapper_prefix_str, make_fn_wrapper_suffix_str);
													bool seems_to_be_already_applied = ((1 <= ecs_ref.m_expr_text_modifier_stack.size()) && ("wrap" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) 
														&& (l_text_modifier.is_equal_to(*(ecs_ref.m_expr_text_modifier_stack.back()))));
													if (!seems_to_be_already_applied) {
														auto shptr2 = std::make_shared<CWrapExprTextModifier>(make_fn_wrapper_prefix_str, make_fn_wrapper_suffix_str);
														ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
														ecs_ref.update_current_text();
													}
												};
												apply_to_expr_conversion_state(LHS);
												apply_to_expr_conversion_state(RHS);
											}
										}
									}
								}
							}
						}
					} else {
						if ((lhs_is_known_to_be_an_array || rhs_is_known_to_be_an_array) && LHS->getType()->isPointerType()) {
							bool lhs_needs_to_be_wrapped = false;
							if ((lhs_is_dynamic_array && (!rhs_is_dynamic_array)) || (lhs_is_native_array && (!rhs_is_native_array))
								|| ((!lhs_is_known_to_be_an_array) && rhs_is_known_to_be_an_array)
								|| ((!lhs_is_non_modifiable) && rhs_is_non_modifiable)) {

								lhs_needs_to_be_wrapped = true;
								array_needed_to_be_wrapped = true;
							}
							bool rhs_needs_to_be_wrapped = false;
							if ((rhs_is_dynamic_array && (!lhs_is_dynamic_array)) || (rhs_is_native_array && (!lhs_is_native_array))
								|| ((!rhs_is_known_to_be_an_array) && lhs_is_known_to_be_an_array)
								|| ((!rhs_is_non_modifiable) && lhs_is_non_modifiable)) {

								rhs_needs_to_be_wrapped = true;
								array_needed_to_be_wrapped = true;
							}

							if (lhs_needs_to_be_wrapped || rhs_needs_to_be_wrapped) {
								std::string arg_pointee_qtype_str;

								CDDeclConversionState* ddcs_ptr = nullptr;
								CArrayInferenceInfo& inference_info = lhs_DD ? lhs_inference_info : rhs_inference_info;
								if (lhs_DD != nullptr) {
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &Rewrite);
									ddcs_ptr = &ddcs_ref;
									inference_info = lhs_inference_info;
								} else if (rhs_DD != nullptr) {
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_DD, &Rewrite);
									ddcs_ptr = &ddcs_ref;
									inference_info = rhs_inference_info;
								}
								if (ddcs_ptr) {
									auto& ddcs_ref = *ddcs_ptr;
									if ((inference_info.indirection_level + 1) <= ddcs_ref.m_indirection_state_stack.size()) {
										auto indirection_state_stack_of_pointee = ddcs_ref.m_indirection_state_stack;
										/* This clears the base vector, but leaves the other members intact. */
										indirection_state_stack_of_pointee.clear();
										for (size_t i = size_t(inference_info.indirection_level + 1); ddcs_ref.m_indirection_state_stack.size() > i; ++i) {
											indirection_state_stack_of_pointee.push_back(ddcs_ref.m_indirection_state_stack.at(i));
										}

										auto res3 = generate_type_indirection_prefix_and_suffix(indirection_state_stack_of_pointee, Rewrite, EIsFunctionParam::No, {}/*maybe_storage_duration*/, &state1);

										arg_pointee_qtype_str = res3.m_complete_type_str;

										if (0 == indirection_state_stack_of_pointee.size()) {
											/* The type of the conditional operator argument(/alternative/option/branch) expressions seems to have
											(only) one level of indirection. (I.e. It may be a pointer, but not a pointer to a pointer.) So the
											"pointee" type is the "direct" type. */
											if (LHS->getType()->getPointeeType().isConstQualified()) {
												if (!(ddcs_ref.direct_type_state_ref().is_const())) {
													/* Ok, so the pointee/"direct" type derived from the DDecl seems to be non-const, while the 
													pointee/"direct" type of the actual type of the conditional operator argument(/alternative/option/branch) 
													expressions is const. So we'll just add a const qualifier to the "direct" type. */
													arg_pointee_qtype_str = "const " + arg_pointee_qtype_str;
												}
											}
										}
									} else {
										/* unexpected */
										int q = 3;
									}
								}
								if ("" == arg_pointee_qtype_str) {
									arg_pointee_qtype_str = LHS->getType()->getPointeeType().getAsString();
								}

								auto xscope_eligibility = EXScopeEligibility::No;
								std::string arg_prefix_str;
								if ("Dual" == ConvertMode) {
									arg_prefix_str = "MSE_LH_CAST(";
									if (false && (EXScopeEligibility::Yes == xscope_eligibility)) {
										arg_prefix_str += "MSE_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE(" + arg_pointee_qtype_str + ")";
									} else {
										arg_prefix_str += "MSE_LH_ARRAY_ITERATOR_TYPE(" + arg_pointee_qtype_str + ")";
									}
									arg_prefix_str += ", ";
								} else {
									if (false && EXScopeEligibility::Yes == xscope_eligibility) {
										arg_prefix_str += "mse::lh::TXScopeLHNullableAnyRandomAccessIterator<" + arg_pointee_qtype_str + " >";
									} else {
										arg_prefix_str += "mse::lh::TLHNullableAnyRandomAccessIterator<" + arg_pointee_qtype_str + " >";
									}
									arg_prefix_str += "(";
								}
								cocs_ref.m_arg_prefix_str += arg_prefix_str;
								cocs_ref.m_arg_suffix_str = cocs_ref.m_arg_suffix_str + ")";

								if (lhs_needs_to_be_wrapped) {
									cocs_ref.m_lhs_needs_to_be_cast = true;
								}
								if (rhs_needs_to_be_wrapped) {
									cocs_ref.m_rhs_needs_to_be_cast = true;
								}
							}
						}
					}

					if ((lhs_is_known_to_be_a_pointer_target || rhs_is_known_to_be_a_pointer_target)
						&& (!(lhs_is_known_to_be_a_pointer_target && rhs_is_known_to_be_a_pointer_target))) {

						bool vetoed = false;
						auto pointer_target_E = lhs_is_known_to_be_a_pointer_target ? LHS : RHS;
						auto pointer_target_E_ii = IgnoreParenImpCasts(pointer_target_E);
						assert(pointer_target_E_ii);
						auto CE = dyn_cast<const clang::CallExpr>(pointer_target_E_ii);
						if (CE) {
							vetoed = true;
						} else {
							auto ECE = dyn_cast<const clang::ExplicitCastExpr>(pointer_target_E_ii);
							if (ECE) {
								vetoed = true;
							}
						}
						if (!vetoed) {
							std::string adj_arg_qtype_str;

							if (LHS->getType()->isPointerType()) {
								CDDeclConversionState* ddcs_ptr = nullptr;
								if (lhs_is_known_to_be_a_pointer_target && (lhs_DD != nullptr)) {
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &Rewrite);
									ddcs_ptr = &ddcs_ref;
								} else if (rhs_DD != nullptr) {
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_DD, &Rewrite);
									ddcs_ptr = &ddcs_ref;
								}
								if (ddcs_ptr) {
									auto& ddcs_ref = *ddcs_ptr;
									auto res3 = generate_type_indirection_prefix_and_suffix(ddcs_ref.m_indirection_state_stack, Rewrite, EIsFunctionParam::No, {}/*maybe_storage_duration*/, &state1);
									adj_arg_qtype_str = res3.m_complete_type_str;
								}
							}
							if ("" == adj_arg_qtype_str) {
								adj_arg_qtype_str = LHS->getType().getAsString();
							}
							std::string arg_prefix_str;
							if ("Dual" == ConvertMode) {
								arg_prefix_str = "MSE_LH_CAST(" + adj_arg_qtype_str + ", ";
							} else {
								arg_prefix_str = "(" + adj_arg_qtype_str + ")(";
							}
							cocs_ref.m_arg_prefix_str = cocs_ref.m_arg_prefix_str + arg_prefix_str;
							cocs_ref.m_arg_suffix_str += ")";

							if (lhs_is_known_to_be_a_pointer_target) {
								cocs_ref.m_lhs_needs_to_be_cast = true;
							}
							if (rhs_is_known_to_be_a_pointer_target) {
								cocs_ref.m_rhs_needs_to_be_cast = true;
							}
						}
					}

					if ((!is_a_function_or_function_pointer_value_type) && (LHS->getType()->isPointerType())) {
						struct CLoneModifiableArgInfo {
							decltype(lhs_DD) DD = nullptr;
							decltype(lhs_inference_info.indirection_level) indirection_level = 0;
							decltype(LHS) EX = nullptr;
							decltype(LHS) non_modifiable_EX = nullptr;
						};
						std::optional<CLoneModifiableArgInfo> maybe_lone_modifiable_arg_info;
						if (rhs_is_non_modifiable && (!lhs_is_non_modifiable)) {
							if (lhs_DD) {
								maybe_lone_modifiable_arg_info = { lhs_DD, lhs_inference_info.indirection_level, LHS, RHS };
							}
						} else if (lhs_is_non_modifiable && (!rhs_is_non_modifiable)) {
							if (rhs_DD) {
								maybe_lone_modifiable_arg_info = { rhs_DD, rhs_inference_info.indirection_level, RHS, LHS };
							}
						}


						auto is_char_star_or_const_char_star = [](CDDeclConversionState& ddcs_ref, size_t indirection_level = 0) {
							if (ddcs_ref.m_indirection_state_stack.size() == 1 + indirection_level) {
								auto direct_qtype_str = ddcs_ref.m_indirection_state_stack.m_direct_type_state.current_qtype_str();
								return (("char" == direct_qtype_str) || ("const char" == direct_qtype_str));
							}
							return false;
						};

						if (maybe_lone_modifiable_arg_info.has_value()) {
							auto& lone_modifiable_arg_info = maybe_lone_modifiable_arg_info.value();
							/* Ok, one of the conditional operator arguments(/alternatives/options/branches) seems to be non-modifiable 
							while the other one doesn't seem to be. So we're going to wrap the non-modifiable one in a function that 
							unsafely constructs a (safe) interface for it that's hopefully compatible with the other (changed) argument. */
							auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(lone_modifiable_arg_info.DD), &Rewrite);
							if (lone_modifiable_arg_info.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
								auto& ecs_ref = state1.get_expr_conversion_state_ref(*(lone_modifiable_arg_info.non_modifiable_EX), Rewrite);

								auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(lone_modifiable_arg_info.indirection_level);
								bool is_iterator = indirection_state_ref.is_known_to_be_used_as_an_array_iterator() || is_char_star_or_const_char_star(ddcs_ref, lone_modifiable_arg_info.indirection_level);
								std::shared_ptr<CExprTextModifier> l_text_modifier_shptr = is_iterator 
									? std::shared_ptr<CExprTextModifier>(std::make_shared<CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier>()) 
									: std::shared_ptr<CExprTextModifier>(std::make_shared<CUnsafeMakeLHNullableAnyPointerFromExprTextModifier>());

								if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
									/* The expression pointer is now known to be used as an iterator, so if there is an already existing 
									modifier that assumed it was not an iterator, we'll remove it. */
									for (size_t i = 0; ecs_ref.m_expr_text_modifier_stack.size() > i; i += 1) {
										size_t j = ecs_ref.m_expr_text_modifier_stack.size() - 1 - i;
										auto& text_modifier_ref = ecs_ref.m_expr_text_modifier_stack.at(j);
										if ("unsafe make lh_nullable_any_pointer from" == text_modifier_ref->species_str()) {
											ecs_ref.m_expr_text_modifier_stack.erase(ecs_ref.m_expr_text_modifier_stack.begin() + j);
											break;
										};
									}
								}

								bool seems_to_be_already_applied = false;
								const auto species_str = l_text_modifier_shptr->species_str();
								for (auto& text_modifier_shptr_ref : ecs_ref.m_expr_text_modifier_stack) {
									if (species_str == text_modifier_shptr_ref->species_str()) {
										seems_to_be_already_applied = true;
										break;
									}
								}
								if (!seems_to_be_already_applied) {
									ecs_ref.m_expr_text_modifier_stack.push_back(l_text_modifier_shptr);
									ecs_ref.update_current_text();
								}
							} else {
								/* unexpected */
								int q = 3;
							}
						} else if (lhs_is_non_modifiable && rhs_is_non_modifiable && state1.m_ast_context_ptr) {
							/* Both arguments(/alternatives/options/branches) of the conditional operator are non-modifiable 
							so they presumably wouldn't need any casting or wrapping to conform with each other, but it still 
							might be needed to for compatibility with the assignment target of the conditional expression (if 
							one is provided). */
							bool var_is_non_modifiable = false;
							if (var_DD) {
								var_is_non_modifiable = is_non_modifiable(*var_DD, *(state1.m_ast_context_ptr), Rewrite, state1);

								if (!var_is_non_modifiable) {
									auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*var_DD, &Rewrite);

									auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(var_indirection_level);
									bool is_iterator = indirection_state_ref.is_known_to_be_used_as_an_array_iterator() || is_char_star_or_const_char_star(ddcs_ref, var_indirection_level);
									std::shared_ptr<CExprTextModifier> l_text_modifier_shptr = is_iterator 
										? std::shared_ptr<CExprTextModifier>(std::make_shared<CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier>()) 
										: std::shared_ptr<CExprTextModifier>(std::make_shared<CUnsafeMakeLHNullableAnyPointerFromExprTextModifier>());

									auto apply_to_expr_conversion_state2 = [&](clang::Expr const * E) {
										auto& ecs_ref = state1.get_expr_conversion_state_ref(*IgnoreParenImpCasts(E), Rewrite);

										if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
											/* The expression pointer is now known to be used as an iterator, so if there is an already existing 
											modifier that assumed it was not an iterator, we'll remove it. */
											for (size_t i = 0; ecs_ref.m_expr_text_modifier_stack.size() > i; i += 1) {
												size_t j = ecs_ref.m_expr_text_modifier_stack.size() - 1 - i;
												auto& text_modifier_ref = ecs_ref.m_expr_text_modifier_stack.at(j);
												if ("unsafe make lh_nullable_any_pointer from" == text_modifier_ref->species_str()) {
													ecs_ref.m_expr_text_modifier_stack.erase(ecs_ref.m_expr_text_modifier_stack.begin() + j);
													break;
												};
											}
										}

										bool seems_to_be_already_applied = false;
										const auto species_str = l_text_modifier_shptr->species_str();
										for (auto& text_modifier_shptr_ref : ecs_ref.m_expr_text_modifier_stack) {
											if (species_str == text_modifier_shptr_ref->species_str()) {
												seems_to_be_already_applied = true;
												break;
											}
										}
										if (!seems_to_be_already_applied) {
											ecs_ref.m_expr_text_modifier_stack.push_back(l_text_modifier_shptr);
											ecs_ref.update_current_text();
										}
									};
									apply_to_expr_conversion_state2(LHS);
									apply_to_expr_conversion_state2(RHS);
								}
							}
						}
					}

					if (ConvertToSCPP) {
						//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, COSR, state1, CO);

						/* This lambda is expected to be executed as a deferred action, so we cannot furhter defer modification 
						of the source text, as we presumably won't get another chance to do so. */
						const auto raw_COSR = CO->getSourceRange();
						bool macro_flag = raw_COSR.getBegin().isMacroID() && raw_COSR.getEnd().isMacroID();
						const auto COSRPlus = cm1_adjusted_source_range(CO->getSourceRange(), state1, Rewrite);
						bool macro_flag2 = COSRPlus.getBegin().isMacroID() && COSRPlus.getEnd().isMacroID();
						const auto COSR = write_once_source_range(COSRPlus);
						if (COSR.isValid()) {
							std::optional<CExprTextInfoContext> maybe_context;
							if (macro_flag) {
								maybe_context = CExprTextInfoContext{ COSRPlus };
							}
							auto& current_text_ref = cocs_ref.current_text(maybe_context);
							if (current_text_ref != cocs_ref.m_original_source_text_str) {
								state1.m_pending_code_modification_actions.ReplaceText(Rewrite, COSR, current_text_ref);
							}
						}
					}
				};
				//lambda();
				bool apply_modification = true;
				assert(COSR.isValid());
				state1.m_pending_code_modification_actions.add_replacement_action(COSR, lambda);

				/* We've queued the modification action for deferred execution, but we don't want to delay the
				establishment of the expression conversion state because, among other reasons, it reads from 
				and stores the original source text and we want that done before the source text gets 
				potentially modified. */
				auto& ecs_ref = state1.get_expr_conversion_state_ref<CConditionalOperatorExprConversionState>(*CO, Rewrite);

				CDDeclConversionState* var_ddcs_ptr = nullptr;
				if (m_var_DD != nullptr) {
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*m_var_DD, &m_Rewrite);
					var_ddcs_ptr = &ddcs_ref;
				}

				if (m_var_DD && var_ddcs_ptr) {
					auto& ddcs_ref = *var_ddcs_ptr;
					if (m_var_indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
						auto var_indirection = CDDeclIndirection{ *m_var_DD, m_var_indirection_level };
						auto& var_indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(m_var_indirection_level);

						bool lhs_has_DD_and_is_not_known_to_be_an_array = (lhs_DD && !lhs_is_known_to_be_an_array);
						bool rhs_has_DD_and_is_not_known_to_be_an_array = (rhs_DD && !rhs_is_known_to_be_an_array);
						bool both_sides_have_DD_and_are_not_known_to_be_an_array = lhs_has_DD_and_is_not_known_to_be_an_array && rhs_has_DD_and_is_not_known_to_be_an_array;
						/* If either side is missing a DD, then we can't know the nature of its pointee, so we'll be "conservative" 
						and assume that we have to support both malloc()ed and non-malloc()ed pointees. */
						if ((!both_sides_have_DD_and_are_not_known_to_be_an_array) && (!var_indirection_state_ref.is_known_to_have_malloc_target())) {
							var_indirection_state_ref.set_is_known_to_have_malloc_target(true);
							state1.m_conversion_state_change_action_map.execute_matching_actions(state1, var_indirection);
							if (var_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, var_indirection);
							}
						}
						if ((!both_sides_have_DD_and_are_not_known_to_be_an_array) && (!var_indirection_state_ref.is_known_to_have_non_malloc_target())) {
							var_indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
							state1.m_conversion_state_change_action_map.execute_matching_actions(state1, var_indirection);
							if (var_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, var_indirection);
							}
						}

						if ((lhs_is_known_to_be_an_array || rhs_is_known_to_be_an_array)) {
							if (!var_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								var_indirection_state_ref.set_is_known_to_be_used_as_an_array_iterator(true);
								state1.m_conversion_state_change_action_map.execute_matching_actions(state1, var_indirection);
								if (var_indirection_state_ref.is_known_to_have_malloc_target()) {
									state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, var_indirection);
								}
								if (var_indirection_state_ref.is_known_to_have_non_malloc_target()) {
									state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, var_indirection);
								}
							}
						}

						/* Ok, we just handled the constraints the conditional operator (source expression) imposes on the 
						assignee (target) object. Now we need to handle the constraints that the assignee (target) object 
						imposes on each argument(/alternative/option/branch) of the conditional operator. */
						if (lhs_DD && lhs_inference_info.ddecl_conversion_state_ptr && (lhs_inference_info.indirection_level < lhs_inference_info.ddecl_conversion_state_ptr->m_indirection_state_stack.size())) {
							CAssignmentTargetConstrainsSourceReplacementAction(Rewrite, var_indirection, CDDeclIndirection{ *lhs_DD, lhs_inference_info.indirection_level }).do_replacement(state1);

							auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &m_Rewrite);
							if (update_declaration_flag) {
								update_declaration(*lhs_DD, (*this).m_Rewrite, state1);
							}
						}
						if (rhs_DD && rhs_inference_info.ddecl_conversion_state_ptr && (rhs_inference_info.indirection_level < rhs_inference_info.ddecl_conversion_state_ptr->m_indirection_state_stack.size())) {
							CAssignmentTargetConstrainsSourceReplacementAction(Rewrite, var_indirection, CDDeclIndirection{ *rhs_DD, rhs_inference_info.indirection_level }).do_replacement(state1);

							auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_DD, &m_Rewrite);
							if (update_declaration_flag) {
								update_declaration(*rhs_DD, (*this).m_Rewrite, state1);
							}
						}
					}

					clang::Expr const* pInitExpr = get_init_expr_if_any(m_var_DD);
					const auto pInitExpr_ii = state1.m_ast_context_ptr ? IgnoreParenImpNoopCasts(pInitExpr, *(state1.m_ast_context_ptr)) : IgnoreParenImpCasts(pInitExpr);
					if (pInitExpr_ii == CO) {
						/* m_var_DD's initalization expression is this conditional operator. */
						if (pInitExpr && (!ddcs_ref.m_maybe_initialization_expr_text_info.has_value())) {
							ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, m_Rewrite, state1));
						}

						auto& ecs_ref = state1.get_expr_conversion_state_ref<CConditionalOperatorExprConversionState>(*CO, m_Rewrite);
						/* Even though ecs_ref should be referencing an object of type CConditionalOperatorExprConversionState, 
						ecs_ref's type is actually a reference to its base class. So we'll up(/down?) cast it to (a reference to) 
						the object's actual type. As rtti is disabled, we can't use dynamic_cast<>(). */
						assert("conditional operator" == ecs_ref.species());
						auto& cocs_ref = static_cast<CConditionalOperatorExprConversionState&>(ecs_ref);

						ddcs_ref.m_fallback_current_initialization_expr_str = cocs_ref.current_text();
					} else {
						int q = 5;
					}

					update_declaration(*m_var_DD, m_Rewrite, state1);
					if (array_needed_to_be_wrapped) {
						if (m_var_indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
							ddcs_ref.set_indirection_current(m_var_indirection_level, "variously native and dynamic array");

							update_declaration(*m_var_DD, m_Rewrite, state1);
						} else {
							/* unexpected? */
							int q = 3;
						}
					}
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
				auto SR = cm1_adj_nice_source_range(RD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(RD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
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
						auto res1 = (*this).m_state1.m_recdecl_conversion_state_map.insert(*RD, Rewrite, m_state1);
						auto rdcs_map_iter = res1.first;
						if ((*this).m_state1.m_recdecl_conversion_state_map.end() == rdcs_map_iter) {
							return;
						}
						auto& rdcs_ref = (*rdcs_map_iter).second;
						////bool update_declaration_flag = res1.second;
					}
					{
						auto res1 = (*this).m_state1.m_recdecl_map.insert(*RD, Rewrite, m_state1);
						auto rdcs_map_iter = res1.first;
						if ((*this).m_state1.m_recdecl_map.end() == rdcs_map_iter) {
							return;
						}
						auto& rdcs_ref = (*rdcs_map_iter).second;
						////bool update_declaration_flag = res1.second;
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
		bool m_seems_to_be_some_kind_of_free = false;
		std::string m_num_bytes_arg_source_text;
		std::string m_realloc_or_free_pointer_arg_source_text;
		std::string m_realloc_or_free_pointer_arg_adjusted_source_text;
		clang::ValueDecl const * m_realloc_or_free_pointer_arg_DD = nullptr;
		std::string m_function_name;
		size_t m_num_params = 0;
	};
	struct CAllocFunctionCacheItemInfo : public CAllocFunctionInfo {
		typedef CAllocFunctionInfo base_class;
		using base_class::base_class;
		CAllocFunctionCacheItemInfo(const CAllocFunctionCacheItemInfo&) = default;
		CAllocFunctionCacheItemInfo(CAllocFunctionCacheItemInfo&&) = default;
		CAllocFunctionCacheItemInfo() = default;
		CAllocFunctionCacheItemInfo(base_class alloc_function_info, clang::FunctionDecl const * FND) 
			: base_class(std::move(alloc_function_info)), m_FND(FND)  {}

		CAllocFunctionCacheItemInfo& operator=(const CAllocFunctionCacheItemInfo&) = default;

		clang::FunctionDecl const * m_FND = nullptr;
	};

	static std::vector<CAllocFunctionCacheItemInfo>& s_encountered_alloc_infos_ref() {
		thread_local std::vector<CAllocFunctionCacheItemInfo> tl_list;
		return tl_list;
	}

	CAllocFunctionInfo analyze_malloc_resemblance(const clang::FunctionDecl& function_decl_ref, CTUState& state1, Rewriter &Rewrite) {
		CAllocFunctionInfo retval;

		auto function_decl = &function_decl_ref;
		auto num_params = function_decl->getNumParams();
		if (function_decl && (1 <= num_params)) {
			std::string return_type_str = definition_qtype(function_decl->getReturnType()).getAsString();
			bool return_type_is_void_star = ("void *" == return_type_str);
			const std::string function_name = function_decl->getNameAsString();

			/* Preliminary observations seem to confirm the net benefits of this one-element cache. But 
			further testing would be warranted. */
			thread_local CAllocFunctionCacheItemInfo last_item;
			if (function_decl == last_item.m_FND) {
				return last_item;
			} else {
				int q = 5;
			}

			retval.m_function_name = function_name;
			retval.m_num_params = num_params;
			static const std::string alloc_str = "alloc";
			static const std::string realloc_str = "realloc";
			const auto lc_function_name = tolowerstr(function_name);

			bool ends_with_alloc = ((lc_function_name.size() >= alloc_str.size())
					&& (0 == lc_function_name.compare(lc_function_name.size() - alloc_str.size(), alloc_str.size(), alloc_str)));
			bool ends_with_realloc = (ends_with_alloc && (lc_function_name.size() >= realloc_str.size())
					&& (0 == lc_function_name.compare(lc_function_name.size() - realloc_str.size(), realloc_str.size(), realloc_str)));

			bool contains_alloc = (std::string::npos != lc_function_name.find(alloc_str));
			bool contains_realloc = (std::string::npos != lc_function_name.find(realloc_str));

			bool not_yet_ruled_out1 = (contains_alloc && (1 <= num_params)) || (contains_realloc && (2 <= num_params));
			//not_yet_ruled_out1 = (not_yet_ruled_out1 && return_type_is_void_star);
			if (not_yet_ruled_out1 && !return_type_is_void_star) {
				not_yet_ruled_out1 = false;
				if (function_decl->getReturnType()->isPointerType()) {
					/* We've encountered "realloc" function templates that return the same pointer type they were passed. */
					auto return_type_source_range = cm1_adj_nice_source_range(function_decl->getReturnTypeSourceRange(), state1, Rewrite);
					if (!(return_type_source_range.isValid())) {
						not_yet_ruled_out1 = true;
					} else {
						std::string return_type_source_text_str = Rewrite.getRewrittenText(return_type_source_range);
						auto maybe_tparam_usage_info = seems_to_contain_an_instantiation_of_a_template_parameter(*function_decl, return_type_source_text_str, Rewrite, &state1);
						if (true || maybe_tparam_usage_info.has_value()) {
							/* We're requiring that the return type text contains a template parameter. This might be too strict? */
							not_yet_ruled_out1 = true;
						} else {
							int q = 5;
						}
					}
				}
			}
			if (not_yet_ruled_out1) {
				for (auto const& info : s_encountered_alloc_infos_ref()) {
					if (function_decl == info.m_FND) {
						last_item = info;
						return info;
					}
				}

				std::string realloc_pointer_param_source_text;
				clang::ValueDecl const * realloc_pointer_param_DD = nullptr;
				std::string num_bytes_param_source_text;
				for (auto param : function_decl->parameters()) {
					auto param_qtype = param->getType();
					IF_DEBUG(std::string param_qtype_str = param_qtype.getAsString();)
					auto raw_param_source_range = param->getSourceRange();
					auto param_source_range = cm1_adjusted_source_range(param->getSourceRange(), state1, Rewrite);
					if (!param_source_range.isValid()) {
						/*assert(false); */continue;
					}
					if (param->getSourceRange().getBegin().isMacroID()) {
						int q = 5;
						//continue;
					}
					std::string l_param_source_text = Rewrite.getRewrittenText(param_source_range);
					clang::ValueDecl const * DD = nullptr;
					if (contains_realloc && param->getType()->isPointerType()) {
						realloc_pointer_param_source_text = l_param_source_text;
					} else if (param_qtype->isIntegerType()) {
						num_bytes_param_source_text = l_param_source_text;
						//auto num_bytes_param_source_text_sans_ws = with_whitespace_removed(num_bytes_param_source_text);
						break;
					}
				}

				if (!num_bytes_param_source_text.empty()) {
					retval.m_seems_to_be_some_kind_of_malloc_or_realloc = true;
					retval.m_num_bytes_arg_source_text = num_bytes_param_source_text;
					if (contains_realloc && (2 <= num_params)) {
						retval.m_seems_to_be_some_kind_of_realloc = true;
						retval.m_realloc_or_free_pointer_arg_source_text = realloc_pointer_param_source_text;
						retval.m_realloc_or_free_pointer_arg_adjusted_source_text = realloc_pointer_param_source_text;
						retval.m_realloc_or_free_pointer_arg_DD = realloc_pointer_param_DD;
					}
					s_encountered_alloc_infos_ref().push_back(CAllocFunctionCacheItemInfo{ retval, function_decl});
				}
			}

			if (!(retval.m_seems_to_be_some_kind_of_malloc_or_realloc)) {
				bool return_type_is_void = ("void" == return_type_str);

				static const std::string free_str = "free";

				bool ends_with_free = ((lc_function_name.size() >= free_str.size())
						&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));

				bool contains_free = (std::string::npos != lc_function_name.find(free_str));

				bool not_yet_ruled_out1 = (contains_free && (1 <= num_params));
				not_yet_ruled_out1 = (not_yet_ruled_out1 && return_type_is_void);
				if (not_yet_ruled_out1) {
					for (auto const& info : s_encountered_alloc_infos_ref()) {
						if (function_decl == info.m_FND) {
							last_item = info;
							return info;
						}
					}

					std::string free_pointer_param_source_text;
					clang::ValueDecl const * free_pointer_param_DD = nullptr;
					for (auto param : function_decl->parameters()) {
						auto param_qtype = param->getType();
						IF_DEBUG(std::string param_qtype_str = param_qtype.getAsString();)
						auto raw_param_source_range = param->getSourceRange();
						auto param_source_range = cm1_adjusted_source_range(param->getSourceRange(), state1, Rewrite);
						if (!param_source_range.isValid()) {
							/*assert(false); */continue;
						}
						if (param->getSourceRange().getBegin().isMacroID()) {
							int q = 5;
							//continue;
						}
						std::string l_param_source_text = Rewrite.getRewrittenText(param_source_range);
						clang::ValueDecl const * DD = nullptr;
						if (contains_free && param->getType()->isPointerType()) {
							free_pointer_param_source_text = l_param_source_text;
							break;
						}
					}

					if (!free_pointer_param_source_text.empty()) {
						/* We don't want to mess with a "free" function unless we've encountered a corrsponding "alloc" 
						function. But how do we know if a previously encountered "alloc" function corresponds to this 
						"free" function? I don't know if there's a good answer to that, but for now we're going to 
						require that the names at least start with the same letter. */
						bool prefix_match = ("free" == function_name);
						if (!prefix_match) {
							const auto first_letter = function_name.at(0);
							for (auto const& info : s_encountered_alloc_infos_ref()) {
								if (first_letter == info.m_function_name.at(0)) {
									prefix_match = true;
									break;
								}
							}
						}
						if (prefix_match) {
							retval.m_seems_to_be_some_kind_of_free = true;

							retval.m_realloc_or_free_pointer_arg_source_text = free_pointer_param_source_text;
							retval.m_realloc_or_free_pointer_arg_adjusted_source_text = free_pointer_param_source_text;
							retval.m_realloc_or_free_pointer_arg_DD = free_pointer_param_DD;
						} else {
							int q = 5;
						}
					}
				}
			}
			last_item = CAllocFunctionCacheItemInfo{ retval, function_decl };
		}
		return retval;
	}
	CAllocFunctionInfo analyze_malloc_resemblance(const clang::CallExpr& call_expr, CTUState& state1, Rewriter &Rewrite) {
		CAllocFunctionInfo retval;

		auto CE = &call_expr;
		auto function_decl = CE->getDirectCallee();
		auto num_args = CE->getNumArgs();
		if (function_decl && (1 <= num_args)) {
			auto res2 = analyze_malloc_resemblance(*function_decl, state1, Rewrite);
			if (res2.m_seems_to_be_some_kind_of_malloc_or_realloc) {
				std::string realloc_pointer_arg_source_text;
				std::string realloc_pointer_arg_adjusted_source_text;
				clang::ValueDecl const * realloc_pointer_arg_DD = nullptr;
				clang::MemberExpr const * ME = nullptr;
				std::string num_bytes_arg_source_text;
				for (auto arg : CE->arguments()) {
					auto arg_qtype = arg->getType();
					IF_DEBUG(std::string arg_qtype_str = arg_qtype.getAsString();)
					auto arg_source_range = cm1_adjusted_source_range(arg->getSourceRange(), state1, Rewrite);
					if (!arg_source_range.isValid()) {
						/*assert(false); */continue;
					}
					std::string l_arg_source_text = Rewrite.getRewrittenText(arg_source_range);
					if (arg->getSourceRange().getBegin().isMacroID() && ("" != arg_source_range.m_adjusted_source_text_as_if_expanded)) {
						/* At this point, l_arg_source_text should have the actual text of the expression from the source 
						code. But if the text is in the body of a macro function, then we would actually want any macro 
						parameters in the text to be replaced by their corresponding macro argument. 
						arg_source_range.m_adjusted_source_text_as_if_expanded should contain the text we want. */
						l_arg_source_text = arg_source_range.m_adjusted_source_text_as_if_expanded;
					}
					clang::ValueDecl const * DD = nullptr;
					if (res2.m_seems_to_be_some_kind_of_realloc && arg->getType()->isPointerType() && (!realloc_pointer_arg_DD)) {
						realloc_pointer_arg_source_text = l_arg_source_text;
						realloc_pointer_arg_adjusted_source_text = l_arg_source_text;

						auto arg_ii = IgnoreParenImpCasts(arg);
						auto CSCE = dyn_cast<const clang::CStyleCastExpr>(arg_ii);
						if (CSCE) {
							auto csce_QT = definition_qtype(CSCE->getType());
							IF_DEBUG(std::string csce_QT_str = csce_QT.getAsString();)
							//MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
							auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
							assert(precasted_expr_ptr);
							auto precasted_expr_QT = precasted_expr_ptr->getType();
							IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
							arg_ii = precasted_expr_ptr;

							auto adj_arg_qtype = arg_ii->getType();
							IF_DEBUG(std::string adj_arg_qtype_str = adj_arg_qtype.getAsString();)
							auto adj_arg_source_range = cm1_adjusted_source_range(arg_ii->getSourceRange(), state1, Rewrite);
							if (!adj_arg_source_range.isValid()) {
								/*assert(false); */continue;
							}
							if (arg_ii->getSourceRange().getBegin().isMacroID()) {
								int q = 5;
								//continue;
							}
							std::string l_adj_arg_source_text = Rewrite.getRewrittenText(adj_arg_source_range);
							if (!(l_adj_arg_source_text.empty())) {
								realloc_pointer_arg_adjusted_source_text = l_adj_arg_source_text;
							} else {
								int q = 5;
							}
						}
						auto DRE = dyn_cast<const clang::DeclRefExpr>(arg_ii);
						auto ME = dyn_cast<const clang::MemberExpr>(arg_ii);
						if (DRE) {
							realloc_pointer_arg_DD = DRE->getDecl();
						} else if (ME) {
							realloc_pointer_arg_DD = ME->getMemberDecl();
						}
					} else if (arg_qtype->isIntegerType() && (num_bytes_arg_source_text.empty())) {
						num_bytes_arg_source_text = l_arg_source_text;
						//auto num_bytes_arg_source_text_sans_ws = with_whitespace_removed(num_bytes_arg_source_text);
						break;
					}
				}

				if (!num_bytes_arg_source_text.empty()) {
					bool asterisk_found = false;
					auto sizeof_start_index = num_bytes_arg_source_text.find("sizeof(");
					if (std::string::npos != sizeof_start_index) {
						auto sizeof_end_index = num_bytes_arg_source_text.find(")", sizeof_start_index);
						if (std::string::npos != sizeof_end_index) {
							assert(sizeof_end_index > sizeof_start_index);
							std::string before_str = num_bytes_arg_source_text.substr(0, sizeof_start_index);
							std::string after_str;
							if (sizeof_end_index + 1 < num_bytes_arg_source_text.size()) {
								after_str = num_bytes_arg_source_text.substr(sizeof_end_index + 1);
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
						retval.m_seems_to_be_some_kind_of_malloc_or_realloc = true;
						retval.m_num_bytes_arg_source_text = num_bytes_arg_source_text;
						if ((!(realloc_pointer_arg_source_text.empty())) && (2 <= num_args)) {
							retval.m_seems_to_be_some_kind_of_realloc = true;
							retval.m_realloc_or_free_pointer_arg_source_text = realloc_pointer_arg_source_text;
							retval.m_realloc_or_free_pointer_arg_adjusted_source_text = realloc_pointer_arg_adjusted_source_text;
							retval.m_realloc_or_free_pointer_arg_DD = realloc_pointer_arg_DD;
						}
					}
				} else {
					int q = 3;
				}
			} else if (res2.m_seems_to_be_some_kind_of_free) {
				std::string free_pointer_arg_source_text;
				std::string free_pointer_arg_adjusted_source_text;
				clang::ValueDecl const * free_pointer_arg_DD = nullptr;
				clang::MemberExpr const * ME = nullptr;
				std::string num_bytes_arg_source_text;
				for (auto arg : CE->arguments()) {
					auto arg_qtype = arg->getType();
					IF_DEBUG(std::string arg_qtype_str = arg_qtype.getAsString();)
					auto arg_source_range = cm1_adjusted_source_range(arg->getSourceRange(), state1, Rewrite);
					if (!arg_source_range.isValid()) {
						/*assert(false); */continue;
					}
					if (arg->getSourceRange().getBegin().isMacroID()) {
						int q = 5;
						//continue;
					}
					std::string l_arg_source_text = Rewrite.getRewrittenText(arg_source_range);
					clang::ValueDecl const * DD = nullptr;
					if (arg->getType()->isPointerType() && (!free_pointer_arg_DD)) {
						free_pointer_arg_source_text = l_arg_source_text;
						free_pointer_arg_adjusted_source_text = l_arg_source_text;

						auto arg_ii = IgnoreParenImpCasts(arg);
						auto CSCE = dyn_cast<const clang::CStyleCastExpr>(arg_ii);
						if (CSCE) {
							auto csce_QT = definition_qtype(CSCE->getType());
							IF_DEBUG(std::string csce_QT_str = csce_QT.getAsString();)
							//MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
							auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
							assert(precasted_expr_ptr);
							auto precasted_expr_QT = precasted_expr_ptr->getType();
							IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
							arg_ii = precasted_expr_ptr;

							auto adj_arg_qtype = arg_ii->getType();
							IF_DEBUG(std::string adj_arg_qtype_str = adj_arg_qtype.getAsString();)
							auto adj_arg_source_range = cm1_adjusted_source_range(arg_ii->getSourceRange(), state1, Rewrite);
							if (!adj_arg_source_range.isValid()) {
								/*assert(false); */continue;
							}
							if (arg_ii->getSourceRange().getBegin().isMacroID()) {
								int q = 5;
								//continue;
							}
							std::string l_adj_arg_source_text = Rewrite.getRewrittenText(adj_arg_source_range);
							if (!(l_adj_arg_source_text.empty())) {
								free_pointer_arg_adjusted_source_text = l_adj_arg_source_text;
							} else {
								int q = 5;
							}
						}
						auto DRE = dyn_cast<const clang::DeclRefExpr>(arg_ii);
						auto ME = dyn_cast<const clang::MemberExpr>(arg_ii);
						if (DRE) {
							free_pointer_arg_DD = DRE->getDecl();
						} else if (ME) {
							free_pointer_arg_DD = ME->getMemberDecl();
						}
						break;
					}
				}

				if (!free_pointer_arg_source_text.empty()) {
					retval.m_seems_to_be_some_kind_of_free = true;

					retval.m_realloc_or_free_pointer_arg_source_text = free_pointer_arg_source_text;
					retval.m_realloc_or_free_pointer_arg_adjusted_source_text = free_pointer_arg_adjusted_source_text;
					retval.m_realloc_or_free_pointer_arg_DD = free_pointer_arg_DD;
				} else {
					int q = 3;
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
				auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				if (is_non_modifiable(*DD, *(MR.Context), Rewrite, m_state1)) {
					int q = 5;
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
					auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);
					auto qtype = DD->getType();

					if (AddressableVars) {
						IF_DEBUG(std::string qtype_str = DD->getType().getAsString();)
						if (qtype->isEnumeralType() || qtype->isPointerType() || qtype->isArrayType()) {
						} else {
							if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
								ddcs_ref.m_indirection_state_stack.at(0).set_original_pointer_target_state("native pointer target");
								ddcs_ref.m_indirection_state_stack.at(0).set_current_pointer_target_state("pointer target");
							} else {
								ddcs_ref.direct_type_state_ref().set_original_pointer_target_state("native pointer target");
								ddcs_ref.direct_type_state_ref().set_current_pointer_target_state("pointer target");
							}
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
							update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
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
									auto alloc_function_info1 = analyze_malloc_resemblance(*CE, m_state1, Rewrite);
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
									auto casted_expr_SR = cm1_adj_nice_source_range(casted_expr_ptr->getSourceRange(), m_state1, Rewrite);
									auto CCESR = cm1_adj_nice_source_range(CCE->getSourceRange(), m_state1, Rewrite);
									auto cast_operation_SR = cm1_adj_nice_source_range({ CCE->getLParenLoc(), CCE->getRParenLoc() }, m_state1, Rewrite);

									if (cast_operation_SR.isValid()
											&& (("void" == rhs_ddecl_current_direct_qtype_str) || ("const void" == rhs_ddecl_current_direct_qtype_str))) {
										if (ConvertToSCPP) {
											(*rhs_res2.ddecl_conversion_state_ptr).set_current_direct_qtype(direct_rhs_qtype);

											auto cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
											/* We're going to "blank out"/erase the original source text of the C-style cast operation
											(including the parenthesis) (but not the expression that was being casted). */
											std::string blank_text = cast_operation_text;
											for (auto& ch : blank_text) {
												ch = ' ';
											}
											m_state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, blank_text);

											static const std::string void_str = "void";
											auto void_pos = (*rhs_res2.ddecl_conversion_state_ptr).current_initialization_expr_str(Rewrite, &m_state1).find(void_str);
											if (std::string::npos != void_pos) {
												clang::Expr const* pInitExpr = get_init_expr_if_any(DD);
												if (pInitExpr) {
													if (!(ddcs_ref.m_maybe_initialization_expr_text_info.has_value())) {
														ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, Rewrite, m_state1));
													}
													auto& ecs_ref = m_state1.get_expr_conversion_state_ref(*pInitExpr, Rewrite);
													auto lambda = [void_str = void_str, void_pos, direct_rhs_qtype_str](const std::string& input_text, const clang::Expr* expr_ptr = nullptr) -> std::string {
														auto retval = input_text;
														retval.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
														return retval;
													};
													auto shptr2 = std::make_shared<CGivenFunctionExprTextModifier>(lambda);
													ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
													ecs_ref.update_current_text();
												}
												(*rhs_res2.ddecl_conversion_state_ptr).m_fallback_current_initialization_expr_str.replace(void_pos, void_str.length(), direct_rhs_qtype_str);
											}

											update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
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
							auto alloc_function_info1 = analyze_malloc_resemblance(*CE, m_state1, Rewrite);
							if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
								/* This seems to be some kind of malloc/realloc function. These case should not be
								* handled here. They are handled elsewhere. */
								return;
							}
						}

						auto VD = dyn_cast<const clang::VarDecl>(DD);
						if (VD && (qtype->isArrayType()) && (1 <= ddcs_ref.m_indirection_state_stack.size())) {
							auto& outermost_indirection_state = ddcs_ref.m_indirection_state_stack.at(0);
							if (VD->hasInit() && ("" == outermost_indirection_state.m_array_size_expr) && ("inferred array" == ddcs_ref.m_indirection_state_stack.at(0).current_species())) {
								/* So this appears to be a native array declaration without an explicit array size argument, but with
								an initialization expression from which the array size could presumably be inferred. */
								/* So we're not going to attempt to infer the number of elements (which would be necessary in order to
								convert it to an `mse::lh::TNativeArrayReplacement<>`). We'll just convert it as if it was a dynamic 
								array allocated on the heap, which should result in conversion to an `mse::lh::TStrongVectorIterator<>`, 
								which supports initialization by initializer list. */

								ddcs_ref.set_indirection_current(0, "dynamic array");
								//retval.update_declaration_flag = true;
								m_state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*DD, 0));
								update_declaration(*DD, Rewrite, m_state1);
							}
						}
						if (VD) {
							if (VD->hasInit()) {
								auto pInitExpr = VD->getInit();
								if (pInitExpr) {
									auto init_expr_source_range = cm1_adj_nice_source_range(pInitExpr->getSourceRange(), m_state1, Rewrite);

									/* It seems that getInit() will return the initalization expression even if it
									was expressed in another redeclaration of the variable rather than this declaration.
									We're using source ranges to determine whether the initializtion expression is
									part of this declaration because it's not immediately clear how else to do it. */
									auto init_expr_located_in_this_decl = first_is_a_subset_of_second(init_expr_source_range, SR);

									if (init_expr_source_range.isValid() && init_expr_located_in_this_decl) {
										auto initialization_expr_str = Rewrite.getRewrittenText(init_expr_source_range);
										if (variable_name == initialization_expr_str) {
											/* We encountered a weird bug where the initialization expression sometimes
											* was indicated as being present and the source range set to the variable name
											* when actually no initialization expression was present in the original source. */
											initialization_expr_str = "";
										} else {
											ddcs_ref.m_initializer_SR_or_insert_before_point = init_expr_source_range;
											ddcs_ref.m_original_initialization_expr_str = initialization_expr_str;
											if (true || ddcs_ref.current_initialization_expr_str(Rewrite, &m_state1).empty()) {
												/* This line ensures that the initialization expression has an associated CExprConversionState. This 
												will allow any (modified) subexpressions to establish an ancestor-descendant relationship and 
												facilitate the incorporation of any subexpression modifications into the rendering of the 
												initialization expression. */
												m_state1.get_expr_conversion_state_ref(*pInitExpr, Rewrite);

												ddcs_ref.m_maybe_initialization_expr_text_info.emplace(CExprTextInfo(pInitExpr, Rewrite, m_state1));
												ddcs_ref.m_fallback_current_initialization_expr_str = initialization_expr_str;
											} else {
												initialization_expr_str = ddcs_ref.current_initialization_expr_str(Rewrite, &m_state1);
											}
											ddcs_ref.m_original_initialization_has_been_noted = true;
										}
									} else {
										int q = 5;
									}
								} else {
									int q = 3;
								}
							}
						}

						if (ConvertToSCPP && (rhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
							for (size_t i = 0; ((0 + i) < ddcs_ref.m_indirection_state_stack.size())
														&& (rhs_res2.indirection_level + i < (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size()); i += 1) {
								{
									/* Here we're establishing and "enforcing" the constraint that the rhs value must
									* be of an (array) type that can be assigned to the lhs. */
									std::shared_ptr<CArray2ReplacementAction> cr_shptr;
									if (1 > (0 + i)) {
										cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0 + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
									} else {
										/* Levels of indirection beyond the first one must be of the same type,
										* not just of "compatible" types. */
										cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0 + i), CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i));
									}
									m_state1.m_conversion_state_change_action_map.insert(cr_shptr);

									if (ddcs_ref.has_been_determined_to_point_to_an_array(0 + i)) {
										(*cr_shptr).do_replacement(m_state1);
										if (!ddcs_ref.has_been_determined_to_point_to_a_dynamic_array(0 + i)) {
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
										if (!ddcs_ref.has_been_determined_to_point_to_a_native_array(0 + i)) {
											m_state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);

										if (ddcs_ref.has_been_determined_to_be_ineligible_for_xscope_status(0 + i)) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
										}
									}
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									std::shared_ptr<CArray2ReplacementAction> cr_shptr;
									if (1 > (0 + i)) {
										cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*DD, 0 + i));
									} else {
										/* Levels of indirection beyond the first one must be of the same type,
										* not just of "compatible" types. */
										cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_res2.indirection_level + i), CDDeclIndirection(*DD, 0 + i));
									}
									m_state1.m_conversion_state_change_action_map.insert(cr_shptr);

									if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(rhs_res2.indirection_level + i)) {
										(*cr_shptr).do_replacement(m_state1);
										if (!(*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_dynamic_array(rhs_res2.indirection_level + i)) {
											m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
										if (!(*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_native_array(rhs_res2.indirection_level + i)) {
											m_state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
										}
									} else {
										m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							}
						}
					}

#ifndef NDEBUG
					auto PVD = llvm::dyn_cast<const clang::ParmVarDecl>(DD);
					if (PVD) {
						auto DC = PVD->getDeclContext();
						auto decl_kind = DC->getDeclKind();
						auto decl_kind_name = DC->getDeclKindName();
						if (clang::Decl::TranslationUnit == decl_kind) {
							/* A parameter variable declaration whose declaration context is not a
							clang::Decl::Function. Seems to correspond to the function parameters of
							function pointer declarations. Though it's not immediately apparent why
							a function parameter of a function pointer declaration would have its own
							declaration in the AST, and a dump of the AST (via clang-check  -ast-dump)
							does not seem to indicate the presence of these such declarations, and
							indeed there seems not to be any corresponding declaration of the function
							to which this parameter declaration would belong. So the source code
							modifications associated with this declaration may be redundant with /
							overwritten by source code modifications associated with the corresponding
							function pointer (declaration). */
							int q = 5;
						}
					}
#endif /*!NDEBUG*/

					update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), m_state1);
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
		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const Expr* E , const DeclRefExpr* DRE, const MemberExpr* ME = nullptr) {

			if ((DRE != nullptr) && (E != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(DRE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(DRE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
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
					auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
					if (!decl_source_range.isValid()) {
						return;
					}
					DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
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

					auto res2 = infer_array_type_info_from_stmt(*E, "pointer arithmetic", state1, DD);

					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);
					if (ddcs_ref.m_indirection_state_stack.size() > res2.indirection_level) {

						/* If a declaration for the pointer arithmetic expression is available, then
						we'll mark that declaration as ineligible for "xscope status". The idea
						being that conditional operator options are likely the (obfuscated) source of an
						assignment operation (including being passed as a function argument). */
						ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
						state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*DD, CDDeclIndirection::no_indirection));
						for (auto indirection_level = res2.indirection_level ; ddcs_ref.m_indirection_state_stack.size() > indirection_level; ++indirection_level) {
							ddcs_ref.m_indirection_state_stack.at(indirection_level).set_xscope_eligibility(false);
							state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*DD, indirection_level));
						}
						res2.update_declaration_flag |= true;
					}

					if (res2.update_declaration_flag) {
						update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcssspointerarithmetic");
			const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcssspointerarithmetic2");
			const Expr* E = MR.Nodes.getNodeAs<clang::Expr>("mcssspointerarithmetic3");

			s_handler1(MR, Rewrite, m_state1, E, DRE, ME);
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/**********************************************************************************************************************/

	/* This matcher replaces various legacy versions of nullptr (i.e. NULL, 0) with nullptr. */
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
				const Expr* E2 = MR.Nodes.getNodeAs<clang::Expr>("b");
				if (E2) {
					auto qtype_str = E2->getType().getAsString();
					if ("void *" == qtype_str) {
						/* The expression seems to be (something like) "(void*)0". We'll replace the
						whole expression (not just the "0" part). */
						E = E2;
					}
				}
				auto SR = write_once_source_range(cm1_adj_nice_source_range(E->getSourceRange(), m_state1, Rewrite));
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(E, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				if (false && ConvertToSCPP) {
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

				auto SR = cm1_adj_nice_source_range(DRE->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DRE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				DEBUG_SOURCE_LOCATION_STR(expr_debug_source_location_str, E->getSourceRange(), Rewrite);
				DEBUG_SOURCE_TEXT_STR(expr_debug_source_text, E->getSourceRange(), Rewrite);
				const clang::Expr* subE = nullptr;
				if (UO) {
					DEBUG_SOURCE_LOCATION_STR(uo_debug_source_location_str, UO->getSourceRange(), Rewrite);
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
						DEBUG_SOURCE_LOCATION_STR(decl_uo_debug_source_location_str, D_UO->getSourceRange(), Rewrite);
						DEBUG_SOURCE_TEXT_STR(decl_uo_debug_source_text, D_UO->getSourceRange(), Rewrite);
						IF_DEBUG(auto b1 = D_UO->isImplicit();)
						int(5);
						/* Todo: Ensure that the ('&') operator hasn't been explicitly overloaded. */
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
					auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
					if (!decl_source_range.isValid()) {
						return;
					}
					DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
					DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_source_range, Rewrite);

#ifndef NDEBUG
					if (std::string::npos != decl_debug_source_location_str.find(g_target_debug_source_location_str1)) {
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

					auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

					size_t target_indirection_index = CDDeclIndirection::no_indirection;
					if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
						if ((2 <= ddcs_ref.m_indirection_state_stack.size())
							|| ("native reference" != ddcs_ref.m_indirection_state_stack.at(0).current_species())) {

							target_indirection_index = 0;
							while ((ddcs_ref.m_indirection_state_stack.size() > (target_indirection_index + 1))
								&& ("native reference" == ddcs_ref.m_indirection_state_stack.at(target_indirection_index).current_species())) {
								/* Since taking the address of a native reference actually takes the address of the
								reference's target, we adjust the indirection_index accordingly. */
								target_indirection_index += 1;
							}
						} else {
							int q = 5;
						}
					}
					if ((CDDeclIndirection::no_indirection != target_indirection_index) && (target_indirection_index < ddcs_ref.m_indirection_state_stack.size())) {
						ddcs_ref.m_indirection_state_stack.at(target_indirection_index).set_original_pointer_target_state("native pointer target");
						ddcs_ref.m_indirection_state_stack.at(target_indirection_index).set_current_pointer_target_state("pointer target");
					} else {
						ddcs_ref.direct_type_state_ref().set_original_pointer_target_state("native pointer target");
						ddcs_ref.direct_type_state_ref().set_current_pointer_target_state("pointer target");
					}

					m_state1.m_pointer_target_contingent_replacement_map.do_and_dispose_matching_replacements(m_state1, CDDeclIndirection(*ddcs_ref.m_ddecl_cptr, target_indirection_index));

					update_declaration_if_not_suppressed(*ddcs_ref.m_ddecl_cptr, Rewrite, *(MR.Context), m_state1);

					//homogenize_redeclaration_types(ddcs_ref.m_ddecl_cptr, m_state1, Rewrite);
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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const Expr* LHS, const CallExpr* CE, const BinaryOperator* BO = nullptr, const DeclRefExpr* DRE = nullptr, const MemberExpr* ME = nullptr) {
			const bool is_binary_assignment_operation = ((BO != nullptr) && (BO->isAssignmentOp()));
			/* If BO is provided, then we expect that it will correspond to an assignment 
			operator. In which case we'd expect LHS to be null as the "left hand side" 
			expression can be obtained from BO. */
			if (is_binary_assignment_operation) {
				if (LHS) {
					/* unexpected? */
					int q = 3;
				}
				LHS = BO->getLHS();
			} else if (BO) {
				/* unexpected? */
				int q = 3;
			}

			if ((LHS != nullptr) && (CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = cm1_adj_nice_source_range(is_binary_assignment_operation ? BO->getSourceRange() : CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */
					std::string num_elements_text/* = before_str + after_str*/;
					std::string element_type_str;
					auto lhs_QT = LHS->getType();

					const clang::Type* lhs_TP = lhs_QT.getTypePtr();
					auto lhs_type_str = lhs_QT.getAsString();

					std::string lhs_element_type_str;
					std::string adjusted_num_bytes_str = alloc_function_info1.m_num_bytes_arg_source_text;
					if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
						auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
						assert(nullptr != ATP);
						auto element_type = ATP->getElementType();
						auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &state1);
						if (true || (("char" != type_str) && ("const char" != type_str))) {
							lhs_element_type_str = type_str;
						}
						adjusted_num_bytes_str = "(" + alloc_function_info1.m_num_bytes_arg_source_text + ")";
						if ((element_type.getAsString() != lhs_element_type_str) && ("void" != lhs_element_type_str)) {
							adjusted_num_bytes_str += " / sizeof(" + element_type.getAsString() + ") * sizeof(" + lhs_element_type_str + ")";
						}
					} else if (lhs_TP->isPointerType()) {
						auto target_type = lhs_TP->getPointeeType();
						auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &state1);
						if (true || (("char" != type_str) && ("const char" != type_str))) {
							lhs_element_type_str = type_str;
						}
						adjusted_num_bytes_str = "(" + alloc_function_info1.m_num_bytes_arg_source_text + ")";
						if (("void" == lhs_element_type_str) | ("const void" == lhs_element_type_str)) {
							/* The assignee of the *alloc() function seems to be a void pointer. Without being able to deduce 
							the intended type of the allocated memory, there's nothing we can really do to make it (type) safe. */
							return;
						} else if (target_type.getAsString() != lhs_element_type_str) {
							adjusted_num_bytes_str += " / sizeof(" + target_type.getAsString() + ") * sizeof(" + lhs_element_type_str + ")";
						}
					}
					if ("" != lhs_element_type_str) {
						bool is_char_star = (("char" == lhs_element_type_str) || ("const char" == lhs_element_type_str));

						auto lhs_source_range = cm1_adj_nice_source_range(LHS->getSourceRange(), state1, Rewrite);
						auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);

						auto res2 = infer_array_type_info_from_stmt(*LHS, "malloc target", state1);

						const clang::DeclaratorDecl* DD = res2.ddecl_cptr;

						auto maybe_replacement_SR = std::optional<clang::SourceRange>{};
						if (DD && is_binary_assignment_operation) {
							/* In the case of a binary assignment operation, we will replace the text of whole 
							operation, not just the malloc call. */
							std::string variable_name;
							std::string bo_replacement_code;

							auto decl_SR = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
							if (!decl_SR.isValid()) {
								return;
							}
							DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_SR, Rewrite);
							DEBUG_SOURCE_TEXT_STR(decl_debug_source_text, decl_SR, Rewrite);

							auto QT = DD->getType();
							variable_name = DD->getNameAsString();

							auto qualified_name = DD->getQualifiedNameAsString();
							static const std::string mse_namespace_str1 = "mse::";
							static const std::string mse_namespace_str2 = "::mse::";
							if ((0 == qualified_name.compare(0, mse_namespace_str1.size(), mse_namespace_str1))
									|| (0 == qualified_name.compare(0, mse_namespace_str2.size(), mse_namespace_str2))) {
								return;
							}

							auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

							ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
							for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
								indirection_state.set_xscope_eligibility(false);
							}
							update_declaration_flag |= true;

							if (update_declaration_flag) {
								update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
							}

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								if ("Dual" == ConvertMode) {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = MSE_LH_REALLOC(";
									bo_replacement_code += lhs_element_type_str + ", ";
									bo_replacement_code += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = mse::lh::reallocate(";
									bo_replacement_code += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								} else {
									bo_replacement_code = lhs_source_text;
									bo_replacement_code += " = mse::lh::reallocate(";
									bo_replacement_code += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								}
							} else {
								if ("Dual" == ConvertMode) {
									bo_replacement_code = "MSE_LH_ALLOC(";
									bo_replacement_code += lhs_element_type_str + ", ";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									bo_replacement_code = "mse::lh::allocate(";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								} else {
									bo_replacement_code = "mse::lh::allocate(";
									bo_replacement_code += lhs_source_text + ", ";
									bo_replacement_code += adjusted_num_bytes_str + ")";
								}
							}

							std::string decl_source_location_str;
							std::string decl_source_text;
							if (decl_SR.isValid()) {
								DEBUG_SOURCE_LOCATION_STR(decl_source_location_str, decl_SR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(decl_source_text, decl_SR, Rewrite);
							} else {
								return;
							}

							if (ConvertToSCPP && decl_SR.isValid() && (SR.isValid())
									&& (nullptr != res2.ddecl_conversion_state_ptr)) {
								auto cr_shptr = std::make_shared<CMallocArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), BO, bo_replacement_code);

								if (true || ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level) || is_char_star)) {
									(*cr_shptr).do_replacement(state1);
								} else {
									//state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
								state1.m_conversion_state_change_action_map.insert(cr_shptr);
							} else {
								int q = 7;
							}
						} else {
							std::string replacement_code_str;

							auto CE_source_range = write_once_source_range(cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite));
							if (!CE_source_range.isValid()) {
								return;
							}
							DEBUG_SOURCE_LOCATION_STR(CE_debug_source_location_str, CE_source_range, Rewrite);
							DEBUG_SOURCE_TEXT_STR(CE_debug_source_text, CE_source_range, Rewrite);
							auto replacement_SR = CE_source_range;

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								if ("Dual" == ConvertMode) {
									replacement_code_str += "MSE_LH_REALLOC(";
									replacement_code_str += lhs_element_type_str + ", ";
									replacement_code_str += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									replacement_code_str += adjusted_num_bytes_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									replacement_code_str += "mse::lh::reallocate(";
									replacement_code_str += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									replacement_code_str += adjusted_num_bytes_str + ")";
								} else {
									replacement_code_str += "mse::lh::reallocate(";
									replacement_code_str += alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text + ", ";
									replacement_code_str += adjusted_num_bytes_str + ")";
								}
							} else {
								if ("Dual" == ConvertMode) {
									replacement_code_str = "MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(";
									replacement_code_str += lhs_element_type_str + "), ";
									replacement_code_str += adjusted_num_bytes_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									replacement_code_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<";
									replacement_code_str += lhs_element_type_str + "> >(";
									replacement_code_str += adjusted_num_bytes_str + ")";
								} else {
									replacement_code_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<";
									replacement_code_str += lhs_element_type_str + "> >(";
									replacement_code_str += adjusted_num_bytes_str + ")";
								}
							}

							std::string CE_source_location_str;
							std::string CE_source_text;
							if (replacement_SR.isValid()) {
								DEBUG_SOURCE_LOCATION_STR(CE_source_location_str, replacement_SR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(CE_source_text, replacement_SR, Rewrite);
							} else {
								return;
							}

							if (ConvertToSCPP && replacement_SR.isValid()) {
								if (true) {

									state1.add_pending_straight_text_replacement_expression_update(Rewrite, replacement_SR, CE, replacement_code_str);
									state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, replacement_SR, state1, CE);
									//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, replacement_SR, (*ce_shptr_ref).current_text());
								} else {
									if (CE->getSourceRange().getBegin().isMacroID()) {
										IF_DEBUG(std::string og_whole_expression_str = Rewrite.getRewrittenText(replacement_SR);)
										CExprTextYieldingReplacementAction(Rewrite, MR, CE, replacement_code_str).do_replacement(state1);
									} else {
										state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, replacement_SR, replacement_code_str);
									}
								}
							} else {
								int q = 7;
							}
						}
					}
				}
			} else {
				int q = 5;
			}
		}
		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const BinaryOperator* BO, const CallExpr* CE, const DeclRefExpr* DRE = nullptr, const MemberExpr* ME = nullptr) {

			s_handler1(MR, Rewrite, state1, nullptr/* LHS */, CE, BO, DRE, ME);
		}

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
				auto SR = cm1_adj_nice_source_range(BO->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				s_handler1(MR, Rewrite, m_state1, BO, CE, DRE, ME);
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
			//const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssnullinitializer1");
			const Expr* RHS = MR.Nodes.getNodeAs<clang::Expr>("mcsssnullinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssnullinitializer3");

			if (/*(DS != nullptr) && */(RHS != nullptr) && (DD != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				const auto qtype = DD->getType();
				if (qtype.getTypePtr()->isPointerType()) {
					Expr::NullPointerConstantKind kind = RHS->IgnoreParenCasts()->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
					if ((clang::Expr::NPCK_NotNull != kind) && (clang::Expr::NPCK_CXX11_nullptr != kind)) {
						bool is_extern_var = false;
						auto VD = dyn_cast<const clang::VarDecl>(DD);
						if (VD && VD->hasExternalStorage()) {
							is_extern_var = true;
						}
						if (!is_extern_var) {
							auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

							if (!ddcs_ref.m_original_initialization_has_been_noted) {
								int q = 5;
							}

							std::string initialization_expr_str = default_init_value_str(qtype, ESuppressComment::Yes);
							ddcs_ref.m_fallback_current_initialization_expr_str = initialization_expr_str;
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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const CallExpr* CE, const DeclRefExpr* DRE)
		{
			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (1 == num_args)) {
					auto alloc_function_info1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
					if (alloc_function_info1.m_seems_to_be_some_kind_of_free) {
						auto arg_iter = CE->arg_begin();
						assert((*arg_iter)->getType().getTypePtrOrNull());
						auto arg_source_range = cm1_adj_nice_source_range((*arg_iter)->getSourceRange(), state1, Rewrite);
						std::string arg_source_text;
						if (arg_source_range.isValid()) {
							arg_source_text = Rewrite.getRewrittenText(arg_source_range);
							//auto arg_source_text_sans_ws = with_whitespace_removed(arg_source_text);

							auto arg_E = (*(arg_iter));
							auto arg_E_ii = IgnoreParenImpCasts(*(arg_iter));
							auto arg_E_ic = (*(arg_iter))->IgnoreParenCasts();
							auto arg_res2 = infer_array_type_info_from_stmt(*arg_E_ic, "malloc target", state1);
							bool arg_is_an_indirect_type = is_an_indirect_type(arg_E_ic->getType());

							if (arg_res2.ddecl_cptr) {
								if (arg_res2.update_declaration_flag) {
									update_declaration_if_not_suppressed(*(arg_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
								}

								auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(arg_res2.ddecl_cptr), &Rewrite);

								ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
								for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
									indirection_state.set_xscope_eligibility(false);
								}
								update_declaration_flag |= true;

								if (update_declaration_flag) {
									update_declaration_if_not_suppressed(*(arg_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
								}

							}

							auto arg_QT = arg_E_ic->getType();
							const clang::Type* arg_TP = arg_QT.getTypePtr();
							auto arg_type_str = arg_QT.getAsString();
							if (arg_QT->isPointerType()) {
								auto callee_SR = write_once_source_range(cm1_adj_nice_source_range(CE->getCallee()->getSourceRange(), state1, Rewrite));
								auto callee_raw_SR = CE->getCallee()->getSourceRange();
								if (callee_SR.isValid()) {
									IF_DEBUG(auto callee_text = Rewrite.getRewrittenText(callee_SR);)
									IF_DEBUG(auto callee_text1 = Rewrite.getRewrittenText(callee_raw_SR);)

									std::string callee_name_replacement_text;
									if ("Dual" == ConvertMode) {
										callee_name_replacement_text = "MSE_LH_FREE";
									} else if ("FasterAndStricter" == ConvertMode) {
										callee_name_replacement_text = "mse::lh::free";
									} else {
										callee_name_replacement_text = "mse::lh::free";
									}

									if (ConvertToSCPP) {
										bool use_the_more_surgical_replacement_method = false;
										auto rawSR = CE->getSourceRange();
										if (rawSR.isValid() && rawSR.getBegin().isMacroID()) {
											auto adj_SR = cm1_adjusted_source_range(rawSR, state1, Rewrite);
											if (!(adj_SR.m_macro_expansion_range_substituted_with_macro_invocation_range)) {
												/* The `free()` call expression may be part of (but not the whole of) the definition body of a macro. The macro may be a macro 
												function and the argument of the `free()` call expression may be passed as an argument to the macro function. In such cases, 
												the source range of the argument will be reported, by default, as the one at the site of instantiation of the function macro, 
												while in order to replace the entire `free()` call expression, the source range of the argument in the definition body of the 
												macro would need to be used instead. For that we'd need to create and use a custom version of CCallExprConversionState that 
												uses the desired argument source range. Because it's easier, for now we're just going to avoid overwriting the argument (in 
												the definition body) and just overwrite the function name. */
												use_the_more_surgical_replacement_method = true;
											}
										}
										if (!use_the_more_surgical_replacement_method) {
											std::vector<const clang::Expr *> arg_expr_cptrs;

											auto CSCE = dyn_cast<const clang::CStyleCastExpr>(arg_E_ii);
											if (CSCE) {
												auto precasted_expr_as_written_ptr = CSCE->getSubExprAsWritten();
												auto precasted_expr_ptr = CSCE->getSubExpr();
												assert(precasted_expr_ptr);
												if (precasted_expr_as_written_ptr != precasted_expr_ptr) {
													int q = 5;
												} else {
													int q = 5;
												}
												auto precasted_expr_QT = precasted_expr_ptr->getType();
												IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
												arg_E_ic = precasted_expr_ptr->IgnoreParenCasts();
											}
											arg_expr_cptrs.push_back(arg_E_ic);

											auto& ecs = state1.get_expr_conversion_state_ref<CCallExprConversionState>(*CE, Rewrite, arg_expr_cptrs, callee_name_replacement_text);
											ecs.update_current_text();

											state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, SR, state1, CE);

										} else {
											if ((!callee_SR.isValid()) || filtered_out_by_location<options_t<converter_mode_t> >(MR, callee_SR.getBegin())) {
												int q = 5;
												return void();
											}
											state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, callee_SR, callee_name_replacement_text);

											auto CSCE = dyn_cast<const clang::CStyleCastExpr>(arg_E_ii);
											if (CSCE) {
												auto cast_operation_SR = cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite);
												std::string cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
												/* We're going to "blank out"/erase the original source text of the C-style cast operation
												(including the parenthesis) (but not the expression that was being casted). */
												std::string blank_text = cast_operation_text;
												for (auto& ch : blank_text) {
													ch = ' ';
												}
												state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, blank_text);
											}
										}
									}
								} else {
									int q = 5;
								}
							}

							if (false) {
								std::string arg_element_type_str;
								if (arg_TP->isArrayType()) {
									if (llvm::isa<const clang::ArrayType>(arg_TP)) {
										auto ATP = llvm::cast<const clang::ArrayType>(arg_TP);
										assert(nullptr != ATP);
										auto element_type = ATP->getElementType();
										auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &state1);
										if (true || (("char" != type_str) && ("const char" != type_str))) {
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
									auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &state1);
									if (true || (("char" != type_str) && ("const char" != type_str)) || (!llvm::isa<const clang::PointerType>(arg_TP))) {
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
											ce_replacement_code = "mse::lh::free(" + arg_source_text + ")";
										} else {
											ce_replacement_code = "mse::lh::free(" + arg_source_text + ")";
										}

										auto cr_shptr = std::make_shared<CFreeDynamicArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(arg_res2.ddecl_cptr), arg_res2.indirection_level), CE, ce_replacement_code);

										if (true || ((*(arg_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(arg_res2.indirection_level))) {
											(*cr_shptr).do_replacement(state1);
										} else {
											//state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										}
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
								}
							}
						} else {
							int q = 5;
						}
						int q = 5;
					}

				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssfree1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssfree2");

			if ((CE != nullptr) && (DRE != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (1 == num_args)) {
					{
						const std::string function_name = function_decl->getNameAsString();
						static const std::string free_str = "free";
						const auto lc_function_name = tolowerstr(function_name);
						bool ends_with_free = ((lc_function_name.size() >= free_str.size())
								&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));
						if (ends_with_free) {
							auto arg_iter = CE->arg_begin();
							assert((*arg_iter)->getType().getTypePtrOrNull());
							auto arg_source_range = cm1_adj_nice_source_range((*arg_iter)->getSourceRange(), m_state1, Rewrite);
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)

								auto ARG = (*(arg_iter))->IgnoreParenCasts();
								auto arg_res2 = infer_array_type_info_from_stmt(*ARG, "malloc target", (*this).m_state1);
								bool arg_is_an_indirect_type = is_an_indirect_type(ARG->getType());

								if (arg_res2.update_declaration_flag) {
									update_declaration_if_not_suppressed(*(arg_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
								}

								s_handler1(MR, (*this).Rewrite, (*this).m_state1, CE, DRE);

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
				auto SR = cm1_adj_nice_source_range(BO->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
				if (false && (clang::Expr::NPCK_NotNull != kind)) {
					auto lhs_source_range = cm1_adj_nice_source_range(LHS->getSourceRange(), m_state1, Rewrite);
					std::string lhs_source_text;
					if (lhs_source_range.isValid()) {
						lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
						//auto lhs_source_text_sans_ws = with_whitespace_removed(lhs_source_text);

						auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "set to null", (*this).m_state1);
						bool lhs_is_an_indirect_type = is_an_indirect_type(LHS->getType());

						if (lhs_res2.update_declaration_flag) {
							update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
						}

						auto lhs_QT = LHS->getType();
						const clang::Type* lhs_TP = lhs_QT.getTypePtr();
						auto lhs_type_str = lhs_QT.getAsString();

						std::string lhs_element_type_str;
						if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &m_state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								lhs_element_type_str = type_str;
							}
						} else if (lhs_TP->isPointerType()) {
							auto target_type = lhs_TP->getPointeeType();
							auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &m_state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								lhs_element_type_str = type_str;
							}
						}

						if ("" != lhs_element_type_str) {
							if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
								auto lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
								std::string bo_replacement_code = "( (" + lhs_source_text + ") = typename std::remove_reference<decltype(" + lhs_source_text + ")>::type() )";

								auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*(lhs_res2.ddecl_cptr), lhs_res2.indirection_level), BO, bo_replacement_code);

								if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(lhs_res2.indirection_level)) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
								m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
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
				auto SR = cm1_adj_nice_source_range(BO->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				Expr::NullPointerConstantKind kind = RHS->isNullPointerConstant(*(MR.Context), Expr::NullPointerConstantValueDependence());
				if (clang::Expr::NPCK_NotNull != kind) {
					auto lhs_source_range = cm1_adj_nice_source_range(LHS->getSourceRange(), m_state1, Rewrite);
					std::string lhs_source_text;
					if (lhs_source_range.isValid()) {
						lhs_source_text = Rewrite.getRewrittenText(lhs_source_range);
						//auto lhs_source_text_sans_ws = with_whitespace_removed(lhs_source_text);

						auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "compare with null", (*this).m_state1);
						bool lhs_is_an_indirect_type = is_an_indirect_type(LHS->getType());

						if (lhs_res2.update_declaration_flag) {
							update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
						}

						auto lhs_QT = LHS->getType();
						const clang::Type* lhs_TP = lhs_QT.getTypePtr();
						auto lhs_type_str = lhs_QT.getAsString();

						std::string lhs_element_type_str;
						if (llvm::isa<const clang::ArrayType>(lhs_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(lhs_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &m_state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								lhs_element_type_str = type_str;
							}
						} else if (lhs_TP->isPointerType()) {
							auto target_type = lhs_TP->getPointeeType();
							auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &m_state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								lhs_element_type_str = type_str;
							}
						}

						if ("" != lhs_element_type_str) {
							if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type) {
								std::string bo_replacement_code;

								auto opcode_str = std::string(BO->getOpcodeStr());
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

								if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(lhs_res2.indirection_level)) {
									(*cr_shptr).do_replacement(m_state1);
								} else {
									//m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
								m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
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

	struct CFConversionInfo {
		std::string OriginalFunctionQName;
		std::string NewFunctionQName;
		std::string NewDualModeFunctionQName;
		std::optional<size_t> m_maybe_num_parameters;
		std::vector<std::pair<std::string, std::string> > m_string_options;
	};
	static std::vector<CFConversionInfo> const& s_function_conversion_infos() {
		static std::vector<CFConversionInfo> sl_function_conversion_infos = {
			{ "memset", "mse::lh::memset", "MSE_LH_MEMSET", {3} }
			, { "memcpy", "mse::lh::memcpy", "MSE_LH_MEMCPY", {3} }
			, { "memcmp", "mse::lh::memcmp", "MSE_LH_MEMCMP", {3} }
			, { "memchr", "mse::lh::memchr", "MSE_LH_MEMCHR", {3} }
			, { "strcpy", "mse::lh::strcpy", "MSE_LH_STRCPY", {2} }
			, { "strcmp", "mse::lh::strcmp", "MSE_LH_STRCMP", {2} }
			, { "strncmp", "mse::lh::strncmp", "MSE_LH_STRNCMP", {3} }
			, { "strchr", "mse::lh::strchr", "MSE_LH_STRCMP", {2} }
			, { "strlen", "mse::lh::strlen", "MSE_LH_STRLEN", {1} }
			, { "strnlen_s", "mse::lh::strnlen_s", "MSE_LH_STRNLEN_S", {2} }
			, { "strtol", "mse::lh::strtol", "MSE_LH_STRTOL", {3} }
			, { "strtoll", "mse::lh::strtoll", "MSE_LH_STRTOLL", {3} }
			, { "strtoul", "mse::lh::strtoul", "MSE_LH_STRTOUL", {3} }
			, { "strtoull", "mse::lh::strtoull", "MSE_LH_STRTOULL", {3} }
			, { "strtoimax", "mse::lh::strtoimax", "MSE_LH_STRTOIMAX", {3} }
			, { "strtoumax", "mse::lh::strtoumax", "MSE_LH_STRTOUMAX", {3} }
			, { "strtof", "mse::lh::strtof", "MSE_LH_STRTOF", {2} }
			, { "strtod", "mse::lh::strtod", "MSE_LH_STRTOD", {2} }
			, { "strtold", "mse::lh::strtold", "MSE_LH_STRTOLD", {2} }
			, { "strtok", "mse::lh::strtok", "MSE_LH_STRTOK", {2} }
			, { "strtok_r", "mse::lh::strtok_r", "MSE_LH_STRTOK_R", {3} }
			, { "fread", "mse::lh::fread", "MSE_LH_FREAD", {4} }
			, { "fwrite", "mse::lh::fwrite", "MSE_LH_FWRITE", {4} }
			, { "getline", "mse::lh::getline", "MSE_LH_GETLINE", {3} }
			, { "iconv", "MSE_LH_ICONV", "MSE_LH_ICONV", {5} }
			, { "strndup", "mse::lh::strndup", "MSE_LH_STRNDUP", {2} }
			, { "strdup", "mse::lh::strdup", "MSE_LH_STRDUP", {1} }
			, { "xstrdup", "mse::lh::strdup", "MSE_LH_STRDUP", {1} }

			, { "std::memset", "mse::lh::memset", "MSE_LH_MEMSET", {3} }
			, { "std::memcpy", "mse::lh::memcpy", "MSE_LH_MEMCPY", {3} }
			, { "std::memcmp", "mse::lh::memcmp", "MSE_LH_MEMCMP", {3} }
			, { "std::memchr", "mse::lh::memchr", "MSE_LH_MEMCHR", {3} }
			, { "std::strcpy", "mse::lh::strcpy", "MSE_LH_STRCPY", {2} }
			, { "std::strcmp", "mse::lh::strcmp", "MSE_LH_STRCMP", {2} }
			, { "std::strncmp", "mse::lh::strncmp", "MSE_LH_STRNCMP", {3} }
			, { "std::strchr", "mse::lh::strchr", "MSE_LH_STRCMP", {2} }
			, { "std::strlen", "mse::lh::strlen", "MSE_LH_STRLEN", {1} }
			, { "std::strnlen_s", "mse::lh::strnlen_s", "MSE_LH_STRNLEN_S", {2} }
			, { "std::strtol", "mse::lh::strtol", "MSE_LH_STRTOL", {3} }
			, { "std::strtoll", "mse::lh::strtoll", "MSE_LH_STRTOLL", {3} }
			, { "std::strtoul", "mse::lh::strtoul", "MSE_LH_STRTOUL", {3} }
			, { "std::strtoull", "mse::lh::strtoull", "MSE_LH_STRTOULL", {3} }
			, { "std::strtoimax", "mse::lh::strtoimax", "MSE_LH_STRTOIMAX", {3} }
			, { "std::strtoumax", "mse::lh::strtoumax", "MSE_LH_STRTOUMAX", {3} }
			, { "std::strtof", "mse::lh::strtof", "MSE_LH_STRTOF", {2} }
			, { "std::strtod", "mse::lh::strtod", "MSE_LH_STRTOD", {2} }
			, { "std::strtold", "mse::lh::strtold", "MSE_LH_STRTOLD", {2} }
			, { "std::strtok", "mse::lh::strtok", "MSE_LH_STRTOK", {2} }
			, { "std::fread", "mse::lh::fread", "MSE_LH_FREAD", {4} }
			, { "std::fwrite", "mse::lh::fwrite", "MSE_LH_FWRITE", {4} }
		};
		return sl_function_conversion_infos;
	}
	static std::optional<size_t> s_function_conversion_index_if_any(std::string_view target_fqname) {
		size_t count = 0;
		for (auto& function_conversion_info : s_function_conversion_infos()) {
			if (target_fqname == function_conversion_info.OriginalFunctionQName) {
				return count;
			}
			count += 1;
		}
		return {};
	}

	class MCSSSFunctionCall1 : public MatchFinder::MatchCallback
	{
	public:
		MCSSSFunctionCall1 (Rewriter &Rewrite, CTUState& state1) : Rewrite(Rewrite), m_state1(state1) {}

		static void modifier(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const CallExpr* CE = nullptr, const DeclRefExpr* DRE = nullptr)
		{
			if (!CE) {
				CE = MR.Nodes.getNodeAs<clang::CallExpr>("functioncall1");
			}
			if (!DRE) {
				DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("functioncall2");
			}

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl) {

					/* Just trying to ensure that the expression conversion state for this call is established prior to 
					the construction of child argument expression conversion states. */
					auto& ce_ecs_ref = state1.get_expr_conversion_state_ref(*CE, Rewrite);

					const std::string function_name = function_decl->getNameAsString();
					const std::string function_qname = function_decl->getQualifiedNameAsString();
					auto maybe_function_index = s_function_conversion_index_if_any(function_qname);
					if (maybe_function_index.has_value()) {
						auto& fc_info = s_function_conversion_infos().at(maybe_function_index.value());
						if (fc_info.m_maybe_num_parameters.has_value()) {
							if (num_args != fc_info.m_maybe_num_parameters.value()) {
								return;
							}

							auto num_args = CE->getNumArgs();
							if (2 <= num_args) {
								bool first_two_args_are_void_star = false;
								auto* const arg1_E = CE->getArg(0);
								if (arg1_E) {
									auto* const arg1_ii_E = IgnoreParenImpNoopCasts(arg1_E, *(MR.Context));
									const auto arg1_ii_qtype = arg1_ii_E->getType();
									const auto arg1_ii_qtype_str = arg1_ii_qtype.getAsString();
									if (("void *" == arg1_ii_qtype_str) || ("const void *" == arg1_ii_qtype_str)) {
										auto* const arg2_E = CE->getArg(1);
										if (arg2_E) {
											auto* const arg2_ii_E = IgnoreParenImpNoopCasts(arg2_E, *(MR.Context));
											const auto arg2_ii_qtype = arg2_ii_E->getType();
											const auto arg2_ii_qtype_str = arg2_ii_qtype.getAsString();
											if (("void *" == arg2_ii_qtype_str) || ("const void *" == arg2_ii_qtype_str)) {
												/* So library functions like memcmp() and memcpy() are generally expected to be replaced with safe 
												implementations. But the safe implementations don't support the case when the pointer arguments 
												passed are both `void *`. In such case, we don't expect the (unsafe) function to be replaced. */
												return;
											}
										} else {
											int q = 3;
										}
									}
								} else {
									int q = 3;
								}
							}
						}

						bool fall_back_flag = false;
						std::vector<const clang::Expr *> arg_expr_cptrs;
						const auto num_args = CE->getNumArgs();
						for (size_t arg_index = 0; num_args > arg_index; arg_index += 1) {
							auto arg_EX = CE->getArg(arg_index);
							if (!arg_EX) {
								fall_back_flag = true;
								break;
							}
							auto arg_EX_qtype = arg_EX->getType();
							IF_DEBUG(std::string arg_EX_qtype_str = arg_EX_qtype.getAsString();)
							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_EX_ii = IgnoreParenImpCasts(arg_EX);
							auto arg_EX_ii_qtype = arg_EX_ii->getType();
							IF_DEBUG(std::string arg_EX_ii_qtype_str = arg_EX_ii_qtype.getAsString();)
							assert(arg_EX_ii->getType().getTypePtrOrNull());
							arg_expr_cptrs.push_back(arg_EX_ii);
						}

						std::string callee_name_replacement_text;
						if ("Dual" == ConvertMode) {
							callee_name_replacement_text = fc_info.NewDualModeFunctionQName;
						} else if ("FasterAndStricter" == ConvertMode) {
							callee_name_replacement_text = fc_info.NewFunctionQName;
						} else {
							callee_name_replacement_text = fc_info.NewFunctionQName;
						}

						if (!fall_back_flag) {
							auto& ecs = state1.get_expr_conversion_state_ref<CCallExprConversionState>(*CE, Rewrite, arg_expr_cptrs, callee_name_replacement_text);
							ecs.update_current_text();

							state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, SR, state1, CE);
						} else {
							auto callee_SR = write_once_source_range(cm1_adj_nice_source_range(CE->getCallee()->getSourceRange(), state1, Rewrite));
							auto callee_raw_SR = CE->getCallee()->getSourceRange();
							if (callee_raw_SR.getBegin().isMacroID()) {
								auto& SM = Rewrite.getSourceMgr();
								auto callee_spelling_SR = write_once_source_range(clang::SourceRange{ SM.getSpellingLoc(callee_raw_SR.getBegin()), Rewrite.getSourceMgr().getSpellingLoc(callee_raw_SR.getEnd()) });
								std::string callee_spelling_text = Rewrite.getRewrittenText(callee_spelling_SR);
								if (fc_info.OriginalFunctionQName == callee_spelling_text) {
									callee_SR = callee_spelling_SR;
								} else {
									/* The function call seems to be part of a macro we aren't (currently) able to handle. */
									return;
								}
							}
							if (callee_SR.isValid()) {
								IF_DEBUG(auto callee_text = Rewrite.getRewrittenText(callee_SR);)
								IF_DEBUG(auto callee_text1 = Rewrite.getRewrittenText(callee_raw_SR);)

								if (ConvertToSCPP) {
									state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, callee_SR, callee_name_replacement_text);
								}
							} else {
								int q = 5;
							}
						}
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("functioncall1");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("functioncall2");

			if ((CE != nullptr)/* && (DRE != nullptr)*/)
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				const auto function_decl = CE->getDirectCallee();
				const auto num_args = CE->getNumArgs();
				if (function_decl) {
					const std::string function_name = function_decl->getNameAsString();
					const std::string function_qname = function_decl->getQualifiedNameAsString();
					auto maybe_function_index = s_function_conversion_index_if_any(function_qname);
					if (maybe_function_index.has_value()) {
						auto& fc_info = s_function_conversion_infos().at(maybe_function_index.value());
						if (fc_info.m_maybe_num_parameters.has_value()) {
							if (num_args != fc_info.m_maybe_num_parameters.value()) {
								return;
							}
						}

						if (ConvertToSCPP && SR.isValid()) {

							auto lambda = [MR, CE, DRE, *this](){ modifier(MR, (*this).Rewrite, (*this).m_state1, CE, DRE); };
							/* This modification needs to be queued so that it will be executed after any other
							modifications that might affect the relevant part of the source text.
							(Update: This is probably no longer necessary since we no longer modify the arguments.) */
							(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);

							/* We've queued the modification action for deferred execution, but we don't want to delay the
							establishment of the expression conversion state because, among other reasons, it reads from 
							and stores the original source text and we want that done before the source text gets 
							potentially modified. */
							auto& ecs_ref = (*this).m_state1.get_expr_conversion_state_ref(*CE, Rewrite);
						} else {
							int q = 7;
						}
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	/* This class addresses conditional expressions and initialized declarations in the form "type var = cond ? lhs : rhs;". */
	class MCSSSConditionalExpr : public MatchFinder::MatchCallback
	{
	public:
		MCSSSConditionalExpr (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::ConditionalOperator* CO, std::optional<const DeclaratorDecl*> maybe_DD, size_t DD_indirection_level = 0) 
		{
			const Expr* LHS = nullptr;
			const Expr* RHS = nullptr;
			if (CO) {
				LHS = CO->getLHS();
				RHS = CO->getRHS();
			}

			if (/*(DS != nullptr) && */(LHS != nullptr) && (RHS != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(CO->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CO, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}
				auto lhs_qtype = LHS->getType();
				auto rhs_qtype = RHS->getType();
				IF_DEBUG(std::string lhs_qtype_str = lhs_qtype.getAsString();)
				IF_DEBUG(std::string rhs_qtype_str = rhs_qtype.getAsString();)

				auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", state1);
				auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", state1);
				bool lhs_qualifies = false;
				bool rhs_qualifies = false;

				if (lhs_res2.ddecl_cptr && lhs_res2.update_declaration_flag) {
					update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
				}
				if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
					update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
				}

				const DeclaratorDecl* possibly_null_DD = maybe_DD.has_value() ? maybe_DD.value() : nullptr;

				if (maybe_DD.has_value() && LHS->getType()->isPointerType() && RHS->getType()->isPointerType()) {
					assert(nullptr != maybe_DD.value());
					auto DD = maybe_DD.value();
					auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
					if (!decl_source_range.isValid()) {
						return;
					}
					DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
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
					bool var_has_been_determined_to_point_to_an_array = false;

					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);
					if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
						var_current_state_str = ddcs_ref.indirection_current(0);
						var_has_been_determined_to_point_to_an_array = ddcs_ref.has_been_determined_to_point_to_an_array(0);
					} else {
						int q = 7;
					}

					{
						auto& res2 = lhs_res2;
						if (res2.ddecl_cptr && res2.declaration_expr_cptr) {
							std::string variable_name = res2.ddecl_cptr->getNameAsString();
							auto QT = res2.ddecl_cptr->getType();
							IF_DEBUG(std::string QT_str = QT.getAsString();)
							auto LHS_QT = LHS->getType();
							IF_DEBUG(std::string LHS_QT_str = LHS_QT.getAsString();)
							bool are_essentially_the_same_types = (QT == LHS_QT);
							if (!are_essentially_the_same_types) {
								if (QT->isArrayType() && LHS_QT->isPointerType()) {
									auto AT = dyn_cast<const clang::ArrayType>(QT);
									if (AT) {
										IF_DEBUG(std::string AT_element_str = AT->getElementType().getAsString();)
										IF_DEBUG(std::string LHS_QT_pointee_str = LHS_QT->getPointeeType().getAsString();)
										if (AT->getElementType() == LHS_QT->getPointeeType()) {
											are_essentially_the_same_types = true;
										}
									}
								}
							}
							/* Currently we only support the case where the value expressions are direct
							* references to declared variables. */
							if (are_essentially_the_same_types/* && (1 == res2.indirection_level)*/) {
								lhs_qualifies = true;
								if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
									{
										/* Here we're establishing and "enforcing" the constraint that the lhs value must
										* be of an (array) type that can be assigned to the target variable. */
										auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

										if (var_has_been_determined_to_point_to_an_array) {
											(*cr_shptr).do_replacement(state1);
										} else {
											if (ddcs_ref.has_been_determined_to_be_ineligible_for_xscope_status(0)) {
												(*cr_shptr).do_replacement(state1);
											} else {
												state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
											}
										}
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
									{
										/* Here we're establishing the constraint in the opposite direction as well. */
										auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

										if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(state1);
										}
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
								}
							}
						}
					}

					{
						auto& res2 = rhs_res2;
						if (res2.declaration_expr_cptr) {
							if (res2.ddecl_cptr) {
								std::string variable_name = res2.ddecl_cptr->getNameAsString();
								auto QT = res2.ddecl_cptr->getType();
								IF_DEBUG(std::string QT_str = QT.getAsString();)
								auto RHS_QT = RHS->getType();
								IF_DEBUG(std::string RHS_QT_str = RHS_QT.getAsString();)
								bool are_essentially_the_same_types = (QT == RHS_QT);
								if (!are_essentially_the_same_types) {
									if (QT->isArrayType() && RHS_QT->isPointerType()) {
										auto AT = dyn_cast<const clang::ArrayType>(QT);
										if (AT) {
											IF_DEBUG(std::string AT_element_str = AT->getElementType().getAsString();)
											IF_DEBUG(std::string RHS_QT_pointee_str = RHS_QT->getPointeeType().getAsString();)
											if (AT->getElementType() == RHS_QT->getPointeeType()) {
												are_essentially_the_same_types = true;
											}
										}
									}
								}
								/* Currently we only support the case where the value expressions are direct
								* references to declared variables. */
								if (are_essentially_the_same_types) {
									rhs_qualifies = true;
									if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
										{
											/* Here we're establishing and "enforcing" the constraint that the rhs value must
											* be of an (array) type that can be assigned to the target variable. */
											auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

											if (var_has_been_determined_to_point_to_an_array) {
												(*cr_shptr).do_replacement(state1);
											} else {
												if (ddcs_ref.has_been_determined_to_be_ineligible_for_xscope_status(0)) {
													(*cr_shptr).do_replacement(state1);
												} else {
													state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
												}
											}
											state1.m_conversion_state_change_action_map.insert(cr_shptr);
										}
										{
											/* Here we're establishing the constraint in the opposite direction as well. */
											auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

											if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
												(*cr_shptr).do_replacement(state1);
											}
											state1.m_conversion_state_change_action_map.insert(cr_shptr);
										}
									}
								}
							}
						}
					}

					auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, possibly_null_DD, DD_indirection_level);
					(*cr_shptr).do_replacement(state1);
					state1.m_conversion_state_change_action_map.insert(cr_shptr);
				}

				if (ConvertToSCPP) {
					/* Here we're establishing and "enforcing" the constraint that the lhs and rhs
					* values of the conditional operator must be the same type. */
					if (lhs_res2.ddecl_cptr) {
						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(lhs_res2.ddecl_cptr), &Rewrite);

						auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*lhs_res2.ddecl_cptr, lhs_res2.indirection_level), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, possibly_null_DD, DD_indirection_level);

						(*cr_shptr).do_replacement(state1);
						state1.m_conversion_state_change_action_map.insert(cr_shptr);
						if (lhs_res2.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(lhs_res2.indirection_level);

							/* Is this redundant now? */
							if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								if (!indirection_state_ref.is_known_to_have_malloc_target()) {
									state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
								if (!indirection_state_ref.is_known_to_have_non_malloc_target()) {
									state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
								}
							} else {
								state1.m_array2_contingent_replacement_map.insert(cr_shptr);
							}
						}

						if (ddcs_ref.has_been_determined_to_be_a_pointer_target()) {
							(*cr_shptr).do_replacement(state1);
						} else {
							state1.m_pointer_target_contingent_replacement_map.insert(cr_shptr);
						}
					} else {
						int q = 5;
					}
					if (rhs_res2.ddecl_cptr) {
						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(rhs_res2.ddecl_cptr), &Rewrite);

						auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*rhs_res2.ddecl_cptr, rhs_res2.indirection_level), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, possibly_null_DD, DD_indirection_level);

						(*cr_shptr).do_replacement(state1);
						state1.m_conversion_state_change_action_map.insert(cr_shptr);
						if (rhs_res2.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
							auto& indirection_state_ref = ddcs_ref.m_indirection_state_stack.at(rhs_res2.indirection_level);

							/* Is this redundant now? */
							if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								if (!indirection_state_ref.is_known_to_have_malloc_target()) {
									state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
								}
								if (!indirection_state_ref.is_known_to_have_non_malloc_target()) {
									state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
								}
							} else {
								state1.m_array2_contingent_replacement_map.insert(cr_shptr);
							}
						}

						if (ddcs_ref.has_been_determined_to_be_a_pointer_target()) {
							(*cr_shptr).do_replacement(state1);
						} else {
							state1.m_pointer_target_contingent_replacement_map.insert(cr_shptr);
						}
					} else {
						int q = 5;
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			//const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssconditionalinitializer1");
			const clang::ConditionalOperator* CO = MR.Nodes.getNodeAs<clang::ConditionalOperator>("mcsssconditionalinitializer2");
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssconditionalinitializer3");

			std::optional<const DeclaratorDecl*> maybe_DD;
			if (DD) {
				maybe_DD = DD;
			} else {
				int q = 5;
			}

			s_handler1(MR, Rewrite, m_state1, CO, maybe_DD);
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
			//const DeclStmt* DS = MR.Nodes.getNodeAs<clang::DeclStmt>("mcsssconditionalinitializer1");
			const clang::ConditionalOperator* CO = MR.Nodes.getNodeAs<clang::ConditionalOperator>("mcsssconditionalinitializer2");
			const Expr* LHS = nullptr;
			const Expr* RHS = nullptr;
			if (CO) {
				LHS = CO->getLHS();
				RHS = CO->getRHS();
			}
			const DeclaratorDecl* DD = MR.Nodes.getNodeAs<clang::DeclaratorDecl>("mcsssconditionalinitializer3");

			if (/*(DS != nullptr) && */(LHS != nullptr) && (RHS != nullptr) && (DD != nullptr)
				&& LHS->getType()->isPointerType() && RHS->getType()->isPointerType())
			{
				auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
				if (!decl_source_range.isValid()) {
					return;
				}
				DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
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
				bool var_has_been_determined_to_point_to_an_array = false;

				auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);
				if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
					var_current_state_str = ddcs_ref.indirection_current(0);
					var_has_been_determined_to_point_to_an_array = ddcs_ref.has_been_determined_to_point_to_an_array(0);
				} else {
					int q = 7;
				}

				auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", (*this).m_state1);
				auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", (*this).m_state1);
				bool lhs_qualifies = false;
				bool rhs_qualifies = false;

				if (lhs_res2.ddecl_cptr && lhs_res2.update_declaration_flag) {
					update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
				}
				if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
					update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), m_state1);
				}

				{
					auto& res2 = lhs_res2;
					if (res2.ddecl_cptr && res2.declaration_expr_cptr) {
						std::string variable_name = res2.ddecl_cptr->getNameAsString();
						auto QT = res2.ddecl_cptr->getType();
						IF_DEBUG(std::string QT_str = QT.getAsString();)
						auto LHS_QT = LHS->getType();
						IF_DEBUG(std::string LHS_QT_str = LHS_QT.getAsString();)
						bool are_essentially_the_same_types = (QT == LHS_QT);
						if (!are_essentially_the_same_types) {
							if (QT->isArrayType() && LHS_QT->isPointerType()) {
								auto AT = dyn_cast<const clang::ArrayType>(QT);
								if (AT) {
									IF_DEBUG(std::string AT_element_str = AT->getElementType().getAsString();)
									IF_DEBUG(std::string LHS_QT_pointee_str = LHS_QT->getPointeeType().getAsString();)
									if (AT->getElementType() == LHS_QT->getPointeeType()) {
										are_essentially_the_same_types = true;
									}
								}
							}
						}
						/* Currently we only support the case where the value expressions are direct
						* references to declared variables. */
						if (are_essentially_the_same_types/* && (1 == res2.indirection_level)*/) {
							lhs_qualifies = true;
							if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
								{
									/* Here we're establishing and "enforcing" the constraint that the lhs value must
									* be of an (array) type that can be assigned to the target variable. */
									auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

									if (var_has_been_determined_to_point_to_an_array) {
										(*cr_shptr).do_replacement(m_state1);
									} else {
										if (ddcs_ref.has_been_determined_to_be_ineligible_for_xscope_status(0)) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											m_state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
										}
									}
									m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
								}
								{
									/* Here we're establishing the constraint in the opposite direction as well. */
									auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

									if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
										(*cr_shptr).do_replacement(m_state1);
									}
									m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
								}
							}
						}
					}
				}

				{
					auto& res2 = rhs_res2;
					if (res2.declaration_expr_cptr) {
						if (res2.ddecl_cptr) {
							std::string variable_name = res2.ddecl_cptr->getNameAsString();
							auto QT = res2.ddecl_cptr->getType();
							IF_DEBUG(std::string QT_str = QT.getAsString();)
							auto RHS_QT = RHS->getType();
							IF_DEBUG(std::string RHS_QT_str = RHS_QT.getAsString();)
							bool are_essentially_the_same_types = (QT == RHS_QT);
							if (!are_essentially_the_same_types) {
								if (QT->isArrayType() && RHS_QT->isPointerType()) {
									auto AT = dyn_cast<const clang::ArrayType>(QT);
									if (AT) {
										IF_DEBUG(std::string AT_element_str = AT->getElementType().getAsString();)
										IF_DEBUG(std::string RHS_QT_pointee_str = RHS_QT->getPointeeType().getAsString();)
										if (AT->getElementType() == RHS_QT->getPointeeType()) {
											are_essentially_the_same_types = true;
										}
									}
								}
							}
							/* Currently we only support the case where the value expressions are direct
							* references to declared variables. */
							if (are_essentially_the_same_types) {
								rhs_qualifies = true;
								if (ConvertToSCPP && (nullptr != res2.ddecl_conversion_state_ptr)) {
									{
										/* Here we're establishing and "enforcing" the constraint that the rhs value must
										* be of an (array) type that can be assigned to the target variable. */
										auto cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0), CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level));

										if (var_has_been_determined_to_point_to_an_array) {
											(*cr_shptr).do_replacement(m_state1);
										} else {
											if (ddcs_ref.has_been_determined_to_be_ineligible_for_xscope_status(0)) {
												(*cr_shptr).do_replacement(m_state1);
											} else {
												m_state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
											}
										}
										m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
									{
										/* Here we're establishing the constraint in the opposite direction as well. */
										auto cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(res2.ddecl_cptr) , res2.indirection_level), CDDeclIndirection(*DD, 0));

										if ((*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
											(*cr_shptr).do_replacement(m_state1);
										}
										m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
								}
							}
						}
					}
				}

				std::string lhs_current_state_str;
				std::string rhs_current_state_str;
				if (lhs_qualifies || rhs_qualifies) {
					if (lhs_qualifies && (lhs_res2.ddecl_cptr)) {
						auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*(lhs_res2.ddecl_cptr), &Rewrite);
						if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
							lhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current_species();
						} else {
							int q = 7;
						}
					}
					if (rhs_qualifies && (rhs_res2.ddecl_cptr)) {
						auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*(rhs_res2.ddecl_cptr), &Rewrite);
						if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
							rhs_current_state_str = ddcs_ref.m_indirection_state_stack.at(0).current_species();
						} else {
							int q = 7;
						}
					}
				}

				if (ConvertToSCPP) {
					/* Here we're establishing and "enforcing" the constraint that the lhs and rhs
					* values of the conditional operator must be the same type. */
					if (lhs_res2.ddecl_cptr) {
						auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*lhs_res2.ddecl_cptr, 0), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, DD);

						if ("dynamic array" == lhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else if ("native array" == lhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else if ("variously native and dynamic array" == lhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else {
							m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							if ("inferred array" == lhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							}
							m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
						}
					} else {
						int q = 5;
					}
					if (rhs_res2.ddecl_cptr) {
						auto cr_shptr = std::make_shared<CConditionalOperatorReconciliation2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*rhs_res2.ddecl_cptr, 0), CO, lhs_res2.ddecl_cptr, rhs_res2.ddecl_cptr, DD);

						if ("dynamic array" == rhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else if ("native array" == rhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else if ("variously native and dynamic array" == lhs_current_state_str) {
							(*cr_shptr).do_replacement(m_state1);
						} else {
							m_state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							if ("inferred array" == rhs_current_state_str) {
								(*cr_shptr).do_replacement(m_state1);
							}
							m_state1.m_conversion_state_change_action_map.insert(cr_shptr);
						}
					} else {
						int q = 5;
					}
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	inline bool set_xscope_elegibility_of_outermost_indirection_if_any(bool xscope_eligibility, clang::DeclaratorDecl const * DD, CTUState& state1) {
		if (!DD) { return false; }

		auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD);
		if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
			ddcs_ref.m_indirection_state_stack.at(0).set_xscope_eligibility(xscope_eligibility);
			return true;
		}
		return false;
	}

	inline bool contains_explicit_pointer_cast_subexpression(const clang::Expr& EX1_cref) {
		auto expl_cast_exprs = Tget_contained_elements_of_type<clang::ExplicitCastExpr>(EX1_cref);
		for (auto& ECE : expl_cast_exprs) {
			assert(ECE);
			if (&EX1_cref == ECE) {
				int q = 5;
			}
			auto ece_QT = ECE->getType();
			IF_DEBUG(std::string ece_QT_str = ece_QT.getAsString();)
			if (ece_QT->isPointerType()) {
				return true;
			};
		}
		return false;
	}

	template<typename TOptions/* = options_t<> */>
	bool is_non_modifiable(clang::Decl const& decl, clang::ASTContext& Ctx, clang::Rewriter &Rewrite, CTUState& state1, clang::Expr const* E /*= nullptr*/) {
		bool non_modifiable_flag = false;
		do {
			auto suppress_check_flag = state1.m_suppress_check_region_set.contains(&decl, Rewrite, Ctx);
			if (suppress_check_flag) {
				non_modifiable_flag = true;
				break;
			}
			if (filtered_out_by_location<options_t<converter_mode_t> >(Ctx, decl.getSourceRange().getBegin())) {
				if (filtered_out_by_location<options_t<converter_mode_t> >(Ctx, decl.getSourceRange().getEnd())) {
					auto FND = dyn_cast<const clang::FunctionDecl>(&decl);
					if (FND) {
						IF_DEBUG(const auto qfname_str = FND->getQualifiedNameAsString();)

						/* So technically, the fact that the Begin and End of the item's range are in a "filtered out" location 
						out doesn't *necessarily* mean that the whole item is in a "filtered out" location, though we'll 
						generally presume it to be. But in this case the item is a function declaration, so we'll just check if 
						all the parameter declarations are also "filtered out" by location. */
						bool an_arg_is_modifiable = false;
						const auto num_params = FND->getNumParams();
						for (size_t i = 0; num_params > i; i += 1) {
							auto PVD = FND->getParamDecl(i);
							if (PVD && !is_non_modifiable(*PVD, Ctx, Rewrite, state1)) {
								an_arg_is_modifiable = true;
								break;
							}
						}
						if (!an_arg_is_modifiable) {
							non_modifiable_flag = true;
							break;
						} else {
							int q = 5;
						}
					} else {
						non_modifiable_flag = true;
						break;
					}
				} else {
					int q = 5;
				}
			}
			auto SR = decl.getSourceRange();
			if (!(SR.isValid())) {
				non_modifiable_flag = true;
				break;
			}
			auto DD = dyn_cast<const clang::DeclaratorDecl>(&decl);
			if (DD) {
				auto tsi = DD->getTypeSourceInfo();
				if (tsi) {
					auto type_SL = tsi->getTypeLoc().getBeginLoc();

					IF_DEBUG(std::string tsi_qtype_str = tsi->getType().getAsString();)
					auto type_SR = clang::SourceRange(type_SL, type_SL);
					DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, type_SR, Rewrite);

					if (filtered_out_by_location<options_t<converter_mode_t>>(Ctx, type_SL)) {
						/* Even if the declaration itself isn't filtered_out_by_location, if the definition of its type is 
						filtered_out_by_location, then we should probably leave the type specifier alone. For example, the 
						type could be specified by a system macro whose definition might be platform dependent. */
						non_modifiable_flag = true;
						break;
					} else if (true) {
						const auto cannonical_qtype = get_cannonical_type(tsi->getType());
						IF_DEBUG(std::string cannonical_qtype_str = cannonical_qtype.getAsString();)

						auto definition_type_SL = definition_TypeLoc(tsi->getTypeLoc()).getBeginLoc();

						auto definition_type_SR = clang::SourceRange(definition_type_SL, definition_type_SL);
						DEBUG_SOURCE_LOCATION_STR(definition_debug_source_location_str, definition_type_SR, Rewrite);

						if (filtered_out_by_location<options_t<converter_mode_t>>(Ctx, definition_type_SL)) {
							/* The problem with the strategy of designating declarations using potentially platform-specific 
							types as "non-modifiable" is that it technically may include arithmetic types like `int64_t`, which, 
							although (typically) a platform-specific typedef, has no properties, relevant to us, that are 
							non-portable. */
							if (!tsi->getType()->isArithmeticType()) {
								non_modifiable_flag = true;
								break;
							}
						}
					}
				}

				std::string qtype_str = DD->getType().getAsString();
				if ("FILE *" == qtype_str) {
					return true;
				}

				if (DD->getType()->isArrayType()) {
					if (llvm::isa<const clang::ArrayType>(DD->getType().getTypePtr())) {
						auto ATP = llvm::cast<const clang::ArrayType>(DD->getType().getTypePtr());
						auto element_qtype = ATP->getElementType();
						std::string element_qtype_str = element_qtype.getAsString();

						static const std::string anonymous_struct_prefix = "struct (anonymous struct";
						static const std::string unnamed_struct_prefix = "struct (unnamed struct";
						if ((std::string::npos != element_qtype_str.find(anonymous_struct_prefix)) || (std::string::npos != element_qtype_str.find(unnamed_struct_prefix))) {
							/* The declaration seeems to be a (native) array of an unnamed struct type. But `lh::TNativeArrayReplacement<>` 
							(like `std::array<>`) doesn't support unnamed struct types. Ultimately the solution would presumably be to just 
							give the struct a name in a separate declaration, but for now we're just going to treat it as if it were 
							unmodifiable. */
							return true;
						}

					}
				}
				if (DD->getType()->isFunctionPointerType()) {
					auto found_it = state1.m_ddecl_conversion_state_map.find(DD);
					if (state1.m_ddecl_conversion_state_map.end() != found_it) {
						auto& ddcs_ref = found_it->second;
						if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
							clang::FunctionDecl const * function_decl_ptr = nullptr;
							if (2 <= ddcs_ref.m_indirection_state_stack.size()) {
								function_decl_ptr = ddcs_ref.m_indirection_state_stack.at(1).m_function_type_state.m_function_decl_ptr;
							} else {
								function_decl_ptr = ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_function_type_state.m_function_decl_ptr;
							}
							if (function_decl_ptr) {
								if (is_non_modifiable(*function_decl_ptr, Ctx, Rewrite, state1, E)) {
									/* Currently we, don't maintain conversion states for the parameters of the target of a function 
									pointer. If the the function pointer is know to have been assigned a function value with an associated 
									DeclaratorDecl, then currently we use just presume that the the converted types of the function 
									pointer target are the same as those of the assigned function value (whose parameters will have 
									associated conversion states where necessary). So in order to maintain consistency, if the assigned 
									function value is deemed non-modifiable, then we need to report the function pointer (target) as 
									non-modifiable as well. */
									return true;
								}
							}
						} else {
							int q = 3;
						}
						//m_function_type_state
						//m_function_decl_ptr
					}
					/*** lef off here ***/
				}
			}
		} while (false);

		if (non_modifiable_flag) {
			auto FND = dyn_cast<const clang::FunctionDecl>(&decl);
			if (!FND) {
				auto PVD = dyn_cast<const clang::ParmVarDecl>(&decl);
				if (PVD && state1.m_ast_context_ptr) {
					auto DC = PVD->getParentFunctionOrMethod();
					if (DC) {
						FND = dyn_cast<const clang::FunctionDecl>(DC);
						//FND = Tget_immediately_containing_element_of_type<clang::FunctionDecl>(&decl, *(state1.m_ast_context_ptr));
					} else {
						int q = 5;
					}
				}
			}
			if (FND) {
				auto num_params = FND->getNumParams();
				const std::string function_qname = FND->getQualifiedNameAsString();
				if (std::string::npos != function_qname.find("main")) {
					if ("main" == function_qname) {
						int q = 5;
					}
				}

				if constexpr (!std::is_base_of_v<do_not_exclude_functions_with_conversions_t, TOptions>) {
					auto maybe_function_conversion_index = s_function_conversion_index_if_any(function_qname);
					if (maybe_function_conversion_index.has_value()) {
						/* While the function would've otherwise been "non-modifiable", it may be one of the recognized 
						functions slated for conversion. */
						auto index = maybe_function_conversion_index.value();
						auto& function_conversion_infos = s_function_conversion_infos();
						if (function_conversion_infos.at(index).m_maybe_num_parameters.has_value()) {
							const auto fc_num_params = function_conversion_infos.at(index).m_maybe_num_parameters.value();
							if (fc_num_params == num_params) {
								const auto CE = E ? dyn_cast<const clang::CallExpr>(E) : nullptr;
								if (!CE) {
									return false;
								} else {
									auto num_args = CE->getNumArgs();
									if (2 <= num_args) {
										bool first_two_args_are_void_star = false;
										auto* const arg1_E = CE->getArg(0);
										if (arg1_E) {
											auto* const arg1_ii_E = IgnoreParenImpNoopCasts(arg1_E, Ctx);
											const auto arg1_ii_qtype = arg1_ii_E->getType();
											const auto arg1_ii_qtype_str = arg1_ii_qtype.getAsString();
											if (("void *" == arg1_ii_qtype_str) || ("const void *" == arg1_ii_qtype_str)) {
												auto* const arg2_E = CE->getArg(1);
												if (arg2_E) {
													auto* const arg2_ii_E = IgnoreParenImpNoopCasts(arg2_E, Ctx);
													const auto arg2_ii_qtype = arg2_ii_E->getType();
													const auto arg2_ii_qtype_str = arg2_ii_qtype.getAsString();
													if (("void *" == arg2_ii_qtype_str) || ("const void *" == arg2_ii_qtype_str)) {
														/* So library functions like memcmp() and memcpy() are generally expected to be replaced with safe 
														implementations. But the safe implementations don't support the case when the pointer arguments 
														passed are both `void *`. In such case, we don't expect the (unsafe) function to be replaced. */
														return true;
													}
												} else {
													int q = 3;
												}
											}
										} else {
											int q = 3;
										}
									}
									return false;
								}
							}
						} else {
							return false;
						}
					}
				}
				auto res1 = analyze_malloc_resemblance(*FND, state1, Rewrite);
				if (res1.m_seems_to_be_some_kind_of_malloc_or_realloc || res1.m_seems_to_be_some_kind_of_free) {
					/* malloc() and free() calls will presumably be converted. */
					return false;
				}
			}
		}
		return non_modifiable_flag;
	}
	bool is_non_modifiable(clang::Decl const& decl, const clang::ast_matchers::MatchFinder::MatchResult &MR, clang::Rewriter &Rewrite, CTUState& state1, clang::Expr const* E = nullptr) {
		return is_non_modifiable(decl, *(MR.Context), Rewrite, state1, E);
	}

	inline static void handle_c_style_cast_without_context(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
		, const clang::CStyleCastExpr* CSCE) {

		if (CSCE) {
			auto csce_QT = definition_qtype(CSCE->getType());
			IF_DEBUG(std::string csce_QT_str = csce_QT.getAsString();)
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
			auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
			assert(precasted_expr_ptr);
			auto precasted_expr_QT = precasted_expr_ptr->getType();
			IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
			auto precasted_expr_SR = cm1_adj_nice_source_range(precasted_expr_ptr->getSourceRange(), state1, Rewrite);
			auto CSCESR = write_once_source_range(cm1_adj_nice_source_range(CSCE->getSourceRange(), state1, Rewrite));
			auto cast_operation_SR = cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite);
			auto SR = CSCESR;
			bool precasted_expr_is_function_type = precasted_expr_QT->isFunctionType();

			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, CSCESR, Rewrite);
			RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
			DEBUG_SOURCE_TEXT_STR(debug_source_text1, CSCESR, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			bool there_seems_to_be_a_contending_pending_code_modification_action = false;
			auto found_it = state1.m_pending_code_modification_actions.find(CSCESR);
			if (state1.m_pending_code_modification_actions.end() != found_it) {
				/* There seems to already be a pending action for this cast expression. It may have been queued 
				by the MCSSSAssignment handler, which should be able to do a better job than we could here by 
				virtue of having ready access to the context in which the cast expression is situated. But since 
				expressions can generally only be modfied once, executing a modification (or scheduling one for 
				deferred execution) may prevent the other pending modification action(s) from taking effect. So 
				we'll note the presence of the pending action so that we can refrain from executing (or 
				scheduling the execution of) any modifications in a way that might interfere with the already 
				queued action. */
				there_seems_to_be_a_contending_pending_code_modification_action = true;
			}

			if ((csce_QT->isPointerType() || csce_QT->isArrayType()) && (!precasted_expr_is_function_type)
				/*&& (precasted_expr_QT->isPointerType() || precasted_expr_QT->isArrayType())
				&& cast_operation_SR.isValid()*/) {

				auto CE = NonParenImpNoopCastParentOfType<clang::CallExpr>(CSCE, *(MR.Context));
				if (CE) {
					auto function_decl1 = CE->getDirectCallee();
					auto num_args = CE->getNumArgs();
					if (function_decl1) {
						const std::string function_name = function_decl1->getNameAsString();

						auto res1 = analyze_malloc_resemblance(*function_decl1, state1, Rewrite);
						if (res1.m_seems_to_be_some_kind_of_malloc_or_realloc || res1.m_seems_to_be_some_kind_of_free) {
							/* This case should be handled as part of the replacement of the parent *alloc() or *free() function. */
							return;
						}
					}
				}

				auto csce_pointee_QT = llvm::isa<clang::ArrayType>(csce_QT) ? llvm::cast<clang::ArrayType>(csce_QT)->getElementType() : csce_QT->getPointeeType();
				IF_DEBUG(std::string csce_pointee_QT_str = csce_pointee_QT.getAsString();)
				auto non_const_csce_pointee_QT = csce_pointee_QT; non_const_csce_pointee_QT.removeLocalConst();
				auto precasted_expr_pointee_QT = llvm::isa<clang::ArrayType>(precasted_expr_QT) ? llvm::cast<clang::ArrayType>(precasted_expr_QT)->getElementType() : precasted_expr_QT->getPointeeType();
				IF_DEBUG(std::string precasted_expr_pointee_QT_str = precasted_expr_pointee_QT.getAsString();)
				if (ConvertToSCPP) {
					std::string og_cast_operation_str = Rewrite.getRewrittenText(cast_operation_SR);
					std::string expression_replacement_code;
					if (csce_QT->isPointerType() /*"void *" == csce_QT.getAsString()*/) {
						auto IL = dyn_cast<const clang::IntegerLiteral>(precasted_expr_ptr);
						if (IL) {
							if (0 == IL->getValue().getLimitedValue()) {
								/* The expression seems to be (something like) "(void*)0" which is equivalent to NULL. */
								std::string null_value_str;
								if ("Dual" == ConvertMode) {
									null_value_str = "MSE_LH_NULL_POINTER";
								} else {
									null_value_str = "nullptr";
								}

								auto& ecs_ref = state1.get_expr_conversion_state_ref(*CSCE, Rewrite);

								std::shared_ptr<CExprTextModifier> shptr1 = std::make_shared<CStraightReplacementExprTextModifier>(null_value_str);
								if (1 <= ecs_ref.m_expr_text_modifier_stack.size()) {
									if ("straight replacement" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) {
										/* already applied? */
										//return;
									}
								}
								ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
								ecs_ref.update_current_text();

								if (CSCESR.isValid() && (!there_seems_to_be_a_contending_pending_code_modification_action)) {
									state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);
								}
								return;
							} else {
								/* Uhh, this seems to be a cast from an integer literal other than zero to a pointer. */
								std::string og_precasted_expr_str = Rewrite.getRewrittenText(precasted_expr_SR);
								if (("" != og_precasted_expr_str) && (3 <= og_cast_operation_str.size()) && ('(' == og_cast_operation_str.front()) && (')' == og_cast_operation_str.back())) {
									auto og_cast_operation_wo_parens_str = og_cast_operation_str.substr(1, int(og_cast_operation_str.size()) - 2);
									std::string new_cast_prefix;
									if ("Dual" == ConvertMode) {
										new_cast_prefix = "MSE_LH_UNSAFE_CAST("
											+ og_cast_operation_wo_parens_str + ", ";
									} else {
										new_cast_prefix = "mse::us::lh::unsafe_cast<"
											+ og_cast_operation_wo_parens_str + ">(";
									}
									static const std::string new_cast_suffix = ")";

									auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, new_cast_prefix, new_cast_suffix);
									ecs_ref.update_current_text();

									if (CSCESR.isValid() && (!there_seems_to_be_a_contending_pending_code_modification_action)) {
										state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);
									}
								} else {
									int q = 5;
								}
								return;
							}
						}
						if (false) {
							std::string new_cast_prefix;
							if ("Dual" == ConvertMode) {
								new_cast_prefix = "MSE_LH_CAST(" + std::string("MSE_LH_VOID_STAR ") + ", ";
							} else {
								new_cast_prefix = std::string("MSE_LH_VOID_STAR ") + "(";
							}
							std::string cast_expr_str = Rewrite.getRewrittenText(precasted_expr_SR);

							expression_replacement_code = new_cast_prefix + cast_expr_str + ")";
							CExprTextReplacementAction(Rewrite, MR, CSCE, expression_replacement_code).do_replacement(state1);
							return;
						}
					}

					auto maybe_DD = ddecl_of_expression_if_available(precasted_expr_ptr, *(MR.Context));
					if (maybe_DD.has_value()) {
						/* If a declaration for the "pre-casted" expression is available, then
						we'll mark that declaration as ineligible for "xscope status". The idea
						being that casted expressions are likely the (obfuscated) source of an
						assignment operation (including being passed as a function argument). */
						set_xscope_elegibility_of_outermost_indirection_if_any(false, maybe_DD.value(), state1);
					}

					auto b1 = contains_explicit_pointer_cast_subexpression(*precasted_expr_ptr);

					if (true || (!b1) || ("char" == non_const_csce_pointee_QT.getAsString())) {

						auto lambda = [&Rewrite, &state1, CSCE, precasted_expr_ptr, CSCESR, precasted_expr_SR]() {
							auto SR = CSCESR;
							RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
							DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, CSCESR, Rewrite);
							DEBUG_SOURCE_TEXT_STR(debug_source_text1, CSCESR, Rewrite);
#ifndef NDEBUG
							if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
								int q = 5;
							}
#endif /*!NDEBUG*/

							auto res = generate_c_style_cast_replacement_code(Rewrite, state1, CSCE);

							auto adjusted_new_cast_prefix = res.m_new_cast_prefix;
							if (true) {
								auto preconversion_expression_start_offset = Rewrite.getSourceMgr().getFileOffset(precasted_expr_SR.getBegin()) - Rewrite.getSourceMgr().getFileOffset(CSCESR.getBegin());
								const std::string og_whole_cast_expr_text = Rewrite.getRewrittenText(CSCESR);
								if ((og_whole_cast_expr_text.length() > preconversion_expression_start_offset) && (0 <= preconversion_expression_start_offset)) {
									if (res.m_new_cast_prefix.length() < preconversion_expression_start_offset) {
										/* Just adding some padding to the prefix so that "precasted" expression ends up at its original 
										(relative) location. (Just in case some of our other hacks depend on it.) */
										const auto length_diff1 = preconversion_expression_start_offset - res.m_new_cast_prefix.length();
										std::string padding;
										for (size_t i = 0; length_diff1 > i; i += 1) {
											padding += ' ';
										}
										adjusted_new_cast_prefix = padding + adjusted_new_cast_prefix;
									}
								}

								auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, adjusted_new_cast_prefix, res.m_new_cast_suffix);
								ecs_ref.update_current_text();

								if (CSCESR.isValid()) {
									//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);

									auto& current_text_ref = ecs_ref.current_text();
									if (current_text_ref != ecs_ref.m_original_source_text_str) {
										state1.m_pending_code_modification_actions.ReplaceText(Rewrite, CSCESR, current_text_ref);
									}
								}
							}
						};
						//lambda();
						if (CSCESR.isValid() && (!there_seems_to_be_a_contending_pending_code_modification_action)) {
							state1.m_pending_code_modification_actions.add_replacement_action(CSCESR, lambda);

							/* We've queued the modification action for deferred execution, but we don't want to delay the
							establishment of the expression conversion state because, among other reasons, it reads from 
							and stores the original source text and we want that done before the source text gets 
							potentially modified. */
							auto& ecs_ref = state1.get_expr_conversion_state_ref(*CSCE, Rewrite);
							/* Note, the expression conversion state of subexpressions should be constructed after the establishment 
							of any ancestor expressions as they will automatically establish relationships with any pre-existing 
							ancestors (but not descendants). */
							auto& precasted_ecs_ref = state1.get_expr_conversion_state_ref(*precasted_expr_ptr, Rewrite);
						}
					} else {
						std::string cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
						/* We're going to "blank out"/erase the original source text of the C-style cast operation
						(including the parenthesis) (but not the expression that was being casted). */
						std::string blank_text = cast_operation_text;
						for (auto& ch : blank_text) {
							ch = ' ';
						}
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, blank_text);
					}
				}
			} else if (precasted_expr_QT->isPointerType() || precasted_expr_QT->isArrayType()) {
				/* This seems to be a cast from a pointer to something other that a pointer. */
				std::string csce_QT_str = csce_QT.getAsString();
				/* We have encountered casts to void in the wild. */
				if ("void" != csce_QT_str) {
					IF_DEBUG(bool b1 = csce_QT->isIntegerType();)
					if (ConvertToSCPP) {
						std::string og_cast_operation_str = Rewrite.getRewrittenText(cast_operation_SR);
						std::string expression_replacement_code;

						std::string og_precasted_expr_str = Rewrite.getRewrittenText(precasted_expr_SR);
						if (("" != og_precasted_expr_str) && (3 <= og_cast_operation_str.size()) && ('(' == og_cast_operation_str.front()) && (')' == og_cast_operation_str.back())) {
							auto og_cast_operation_wo_parens_str = og_cast_operation_str.substr(1, int(og_cast_operation_str.size()) - 2);
							std::string new_cast_prefix;
							if ("Dual" == ConvertMode) {
								new_cast_prefix = "MSE_LH_UNSAFE_CAST("
									+ og_cast_operation_wo_parens_str + ", ";
							} else {
								new_cast_prefix = "mse::us::lh::unsafe_cast<"
									+ og_cast_operation_wo_parens_str + ">(";
							}
							static const std::string new_cast_suffix = ")";

							auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, new_cast_prefix, new_cast_suffix);
							ecs_ref.update_current_text();

							if (CSCESR.isValid() && (!there_seems_to_be_a_contending_pending_code_modification_action)) {
								state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);
							}
						} else {
							int q = 5;
						}
					}
				} else {
					int q = 5;
				}
			}
		}
	}

	inline static void handle_cxx_static_cast_without_context(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
		, const clang::CXXStaticCastExpr* CXXSCE) {

		if (CXXSCE) {
			auto csce_QT = definition_qtype(CXXSCE->getType());
			IF_DEBUG(std::string csce_QT_str = csce_QT.getAsString();)
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
			auto precasted_expr_ptr = CXXSCE->getSubExprAsWritten();
			assert(precasted_expr_ptr);
			auto precasted_expr_QT = precasted_expr_ptr->getType();
			IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
			auto precasted_expr_SR = cm1_adj_nice_source_range(precasted_expr_ptr->getSourceRange(), state1, Rewrite);
			auto CXXSCESR = write_once_source_range(cm1_adj_nice_source_range(CXXSCE->getSourceRange(), state1, Rewrite));
			auto angle_brackets_SR = write_once_source_range(cm1_adj_nice_source_range(CXXSCE->getAngleBrackets(), state1, Rewrite));
			auto cast_operation_SR = cm1_adj_nice_source_range({ CXXSCESR.getBegin(), angle_brackets_SR.getEnd() }, state1, Rewrite);
			auto SR = CXXSCESR;

			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, CXXSCESR, Rewrite);
			RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
			DEBUG_SOURCE_TEXT_STR(debug_source_text1, CXXSCESR, Rewrite);

			if ((csce_QT->isPointerType() || csce_QT->isArrayType())
				&& (precasted_expr_QT->isPointerType() || precasted_expr_QT->isArrayType())
				&& cast_operation_SR.isValid()) {

				auto csce_pointee_QT = llvm::isa<clang::ArrayType>(csce_QT) ? llvm::cast<clang::ArrayType>(csce_QT)->getElementType() : csce_QT->getPointeeType();
				IF_DEBUG(std::string csce_pointee_QT_str = csce_pointee_QT.getAsString();)
				auto non_const_csce_pointee_QT = csce_pointee_QT; non_const_csce_pointee_QT.removeLocalConst();
				auto precasted_expr_pointee_QT = llvm::isa<clang::ArrayType>(precasted_expr_QT) ? llvm::cast<clang::ArrayType>(precasted_expr_QT)->getElementType() : precasted_expr_QT->getPointeeType();
				IF_DEBUG(std::string precasted_expr_pointee_QT_str = precasted_expr_pointee_QT.getAsString();)
				if (ConvertToSCPP) {
					auto maybe_DD = ddecl_of_expression_if_available(precasted_expr_ptr, *(MR.Context));
					if (maybe_DD.has_value()) {
						/* If a declaration for the "pre-casted" expression is available, then
						we'll mark that declaration as ineligible for "xscope status". The idea
						being that casted expressions are likely the (obfuscated) source of an
						assignment operation (including being passed as a function argument). */
						set_xscope_elegibility_of_outermost_indirection_if_any(false, maybe_DD.value(), state1);
					}

					if (true || CXXSCE->getSourceRange().getBegin().isMacroID()) {
						IF_DEBUG(std::string og_whole_expression_str = Rewrite.getRewrittenText(CXXSCESR);)

						auto containing_D = Tget_containing_element_of_type<clang::Decl>(CXXSCE, *(state1.m_ast_context_ptr));
						std::optional<clang::TypeLoc> maybe_typeLoc;
						auto* tsi_ptr = CXXSCE->getTypeInfoAsWritten();
						if (tsi_ptr) {
							maybe_typeLoc = typeLoc_if_available(*tsi_ptr);
						}

						auto replacement_qtype_str = generate_qtype_replacement_code(CXXSCE->getType(), Rewrite, &state1, EIsFunctionParam::No, {}/*maybe_storage_duration*/, containing_D, maybe_typeLoc);

						auto adjusted_new_cast_prefix = std::string(CXXSCE->getCastName()) + "<" + replacement_qtype_str + ">";

						auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CXXSCE, Rewrite, *precasted_expr_ptr, adjusted_new_cast_prefix, ")");
						ecs_ref.update_current_text();

						if (CXXSCESR.isValid()) {
							state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CXXSCESR, state1, CXXSCE);
						}

						/*
						std::string og_precasted_expr_str = Rewrite.getRewrittenText(precasted_expr_SR);
						std::string new_whole_expression_str = std::string(CXXSCE->getCastName()) + "<" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ">("
							+ og_precasted_expr_str + ")";
						CExprTextYieldingReplacementAction(Rewrite, MR, CXXSCE, new_whole_expression_str).do_replacement(state1);
						*/
					} else {
						IF_DEBUG(std::string og_angle_brackets_str = Rewrite.getRewrittenText(angle_brackets_SR);)
						std::string new_angle_brackets_str = "<" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ">";
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, angle_brackets_SR, new_angle_brackets_str);
					}
				}
			}
		}
	}

	inline static void handle_cxx_const_cast_without_context(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
		, const clang::CXXConstCastExpr* CXXSCE) {

		if (CXXSCE) {
			auto csce_QT = definition_qtype(CXXSCE->getType());
			IF_DEBUG(std::string csce_QT_str = csce_QT.getAsString();)
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
			auto precasted_expr_ptr = CXXSCE->getSubExprAsWritten();
			assert(precasted_expr_ptr);
			auto precasted_expr_QT = precasted_expr_ptr->getType();
			IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
			auto precasted_expr_SR = cm1_adj_nice_source_range(precasted_expr_ptr->getSourceRange(), state1, Rewrite);
			auto CXXSCESR = write_once_source_range(cm1_adj_nice_source_range(CXXSCE->getSourceRange(), state1, Rewrite));
			auto angle_brackets_SR = write_once_source_range(cm1_adj_nice_source_range(CXXSCE->getAngleBrackets(), state1, Rewrite));
			auto cast_operation_SR = cm1_adj_nice_source_range({ CXXSCESR.getBegin(), angle_brackets_SR.getEnd() }, state1, Rewrite);
			auto SR = CXXSCESR;

			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, CXXSCESR, Rewrite);
			RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
			DEBUG_SOURCE_TEXT_STR(debug_source_text1, CXXSCESR, Rewrite);

			if ((csce_QT->isPointerType() || csce_QT->isArrayType())
				&& (precasted_expr_QT->isPointerType() || precasted_expr_QT->isArrayType())
				&& cast_operation_SR.isValid()) {

				auto csce_pointee_QT = llvm::isa<clang::ArrayType>(csce_QT) ? llvm::cast<clang::ArrayType>(csce_QT)->getElementType() : csce_QT->getPointeeType();
				IF_DEBUG(std::string csce_pointee_QT_str = csce_pointee_QT.getAsString();)
				auto non_const_csce_pointee_QT = csce_pointee_QT; non_const_csce_pointee_QT.removeLocalConst();
				auto precasted_expr_pointee_QT = llvm::isa<clang::ArrayType>(precasted_expr_QT) ? llvm::cast<clang::ArrayType>(precasted_expr_QT)->getElementType() : precasted_expr_QT->getPointeeType();
				IF_DEBUG(std::string precasted_expr_pointee_QT_str = precasted_expr_pointee_QT.getAsString();)
				if (ConvertToSCPP) {
					auto maybe_DD = ddecl_of_expression_if_available(precasted_expr_ptr, *(MR.Context));
					if (maybe_DD.has_value()) {
						/* If a declaration for the "pre-casted" expression is available, then
						we'll mark that declaration as ineligible for "xscope status". The idea
						being that casted expressions are likely the (obfuscated) source of an
						assignment operation (including being passed as a function argument). */
						set_xscope_elegibility_of_outermost_indirection_if_any(false, maybe_DD.value(), state1);
					}

					auto b1 = contains_explicit_pointer_cast_subexpression(*precasted_expr_ptr);

					if ((!b1) || CXXSCE->getSourceRange().getBegin().isMacroID()) {
						IF_DEBUG(std::string og_whole_expression_str = Rewrite.getRewrittenText(CXXSCESR);)

						auto containing_D = Tget_containing_element_of_type<clang::Decl>(CXXSCE, *(state1.m_ast_context_ptr));
						std::optional<clang::TypeLoc> maybe_typeLoc;
						auto* tsi_ptr = CXXSCE->getTypeInfoAsWritten();
						if (tsi_ptr) {
							maybe_typeLoc = typeLoc_if_available(*tsi_ptr);
						}

						auto replacement_qtype_str = generate_qtype_replacement_code(CXXSCE->getType(), Rewrite, &state1, EIsFunctionParam::No, {}/*maybe_storage_duration*/, containing_D, maybe_typeLoc);

						auto adjusted_new_cast_prefix = std::string(CXXSCE->getCastName()) + "<" + replacement_qtype_str + ">";

						auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CXXSCE, Rewrite, *precasted_expr_ptr, adjusted_new_cast_prefix, ")");
						ecs_ref.update_current_text();

						if (CXXSCESR.isValid()) {
							state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CXXSCESR, state1, CXXSCE);
						}

						/*
						std::string og_precasted_expr_str = Rewrite.getRewrittenText(precasted_expr_SR);
						std::string new_whole_expression_str = std::string(CXXSCE->getCastName()) + "<" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ">("
							+ og_precasted_expr_str + ")";

						if ("Dual" == ConvertMode) {
							new_whole_expression_str = "MSE_LH_UNSAFE_CAST(" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ", "
							+ og_precasted_expr_str + ")";
						} else {
							new_whole_expression_str = "mse::us::lh::unsafe_cast<" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ">("
							+ og_precasted_expr_str + ")";
						}

						CExprTextYieldingReplacementAction(Rewrite, MR, CXXSCE, new_whole_expression_str).do_replacement(state1);
						*/
					} else {
						IF_DEBUG(std::string og_angle_brackets_str = Rewrite.getRewrittenText(angle_brackets_SR);)
						std::string new_angle_brackets_str = "<" + generate_qtype_replacement_code(csce_QT, Rewrite, &state1) + ">";
						state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, angle_brackets_SR, new_angle_brackets_str);
					}
				}
			}
		}
	}
	static void decl_util_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const clang::Decl* D);

	bool type_definition_is_non_modifiable(clang::Type const& type_cref, clang::ASTContext& Ctx, clang::Rewriter &Rewrite, CTUState& state1) {
		auto Type = &type_cref;
		auto TD = type_cref.getAsTagDecl();
		if (TD) {
			return is_non_modifiable(*TD, Ctx, Rewrite, state1);
		}
		const TypedefType * TDT = type_cref.getAs<TypedefType>();
		if (TDT) {
			TypedefNameDecl *TDND = TDT->getDecl();
			if (TDND) {
				return is_non_modifiable(*TDND, Ctx, Rewrite, state1);
			}
		}
		return false;
	}
	bool type_definition_is_non_modifiable(clang::QualType const& qtype, clang::ASTContext& Ctx, clang::Rewriter &Rewrite, CTUState& state1) {
		auto type_ptr = qtype.getTypePtr();
		if (!type_ptr) {
			return false;
		}
		return type_definition_is_non_modifiable(*type_ptr, Ctx, Rewrite, state1);
	}
	bool type_definition_is_non_modifiable(clang::QualType const& qtype, const clang::ast_matchers::MatchFinder::MatchResult &MR, clang::Rewriter &Rewrite, CTUState& state1) {
		return type_definition_is_non_modifiable(qtype, *(MR.Context), Rewrite, state1);
	}

	class MCSSSAssignment : public MatchFinder::MatchCallback
	{
	public:
		MCSSSAssignment (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}


		static void s_c_style_cast_of_rhs(Rewriter &Rewrite, CTUState& state1, clang::CStyleCastExpr const& CSCE_ref
			, clang::QualType const& lhs_qtype, CArrayInferenceInfo const& lhs_res2, bool LHS_decl_is_non_modifiable, bool RHS_decl_is_non_modifiable) {

			auto* CSCE = &CSCE_ref;

			auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
			assert(precasted_expr_ptr);
			auto CSCESR = write_once_source_range(cm1_adj_nice_source_range(CSCE->getSourceRange(), state1, Rewrite));

			auto SR = CSCESR;
			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			if (!(lhs_res2.ddecl_conversion_state_ptr)) {
				int q = 3;
				return;
			}
			std::string new_LHS_qtype_str;
			auto& ddcs_ref = *(lhs_res2.ddecl_conversion_state_ptr);
			if (lhs_res2.indirection_level < ddcs_ref.m_indirection_state_stack.size()) {
				auto indirection_state_stack_of_LHS = ddcs_ref.m_indirection_state_stack;
				/* This clears the base vector, but leaves the other members intact. */
				indirection_state_stack_of_LHS.clear();
				for (size_t i = size_t(lhs_res2.indirection_level); ddcs_ref.m_indirection_state_stack.size() > i; ++i) {
					indirection_state_stack_of_LHS.push_back(ddcs_ref.m_indirection_state_stack.at(i));
				}

				auto res3 = generate_type_indirection_prefix_and_suffix(indirection_state_stack_of_LHS, Rewrite, EIsFunctionParam::No, {}/*maybe_storage_duration*/, &state1);
				new_LHS_qtype_str = res3.m_complete_type_str;

				if (0 == indirection_state_stack_of_LHS.size()) {
					/* The type of the LHS expression does not seem to be indirect. So the LHS type is the 
					"direct" type. */
					if (lhs_qtype.isConstQualified()) {
						if (!(ddcs_ref.direct_type_state_ref().is_const())) {
							/* Ok, so the "direct" type derived from the DDecl seems to be non-const, while the 
							LHS expressions is const. So we'll just add a const qualifier to the "direct" type. */
							new_LHS_qtype_str = "const " + new_LHS_qtype_str;
						}
					}
				}

				auto& replacement_qtype_str = new_LHS_qtype_str;

				if (ddcs_ref.m_maybe_is_non_modifiable.has_value() && lhs_qtype->isPointerType()) {
					auto lhs_is_non_modifiable = ddcs_ref.m_maybe_is_non_modifiable.value();
					if (lhs_is_non_modifiable && precasted_expr_ptr) {
						auto precasted_res2 = infer_array_type_info_from_stmt(*precasted_expr_ptr, "", state1);

						bool precasted_expr_ptr_decl_is_non_modifiable = false;
						if (precasted_res2.ddecl_conversion_state_ptr && precasted_res2.ddecl_conversion_state_ptr->m_maybe_is_non_modifiable.has_value()) {
							precasted_expr_ptr_decl_is_non_modifiable = precasted_res2.ddecl_conversion_state_ptr->m_maybe_is_non_modifiable.value();
						} else {
							if (precasted_res2.ddecl_cptr && state1.m_ast_context_ptr) {
								auto& Ctx = *(state1.m_ast_context_ptr);
								auto precasted_expr_ptr_decl_is_non_modifiable = is_non_modifiable(*(precasted_res2.ddecl_cptr), Ctx, Rewrite, state1, precasted_expr_ptr);
								if (precasted_res2.ddecl_conversion_state_ptr) {
									precasted_res2.ddecl_conversion_state_ptr->m_maybe_is_non_modifiable = precasted_expr_ptr_decl_is_non_modifiable;
								}
							}
						}
						const auto precasted_qtype = precasted_expr_ptr->getType();
						IF_DEBUG(std::string precasted_qtype_str = precasted_qtype.getAsString();)

						if ((!precasted_expr_ptr_decl_is_non_modifiable) && (precasted_qtype->isPointerType() || (precasted_qtype->isArrayType()))) {
							bool is_pointer_to_pointer = false;
							auto pointee_type_ptr = precasted_qtype->getPointeeOrArrayElementType();
							if (pointee_type_ptr) {
								auto pointee_qtype = clang::QualType(pointee_type_ptr , 0/*I'm just assuming zero specifies no qualifiers*/);
								IF_DEBUG(std::string pointee_qtype_str = pointee_qtype.getAsString();)
								if (pointee_type_ptr->isPointerType() || pointee_type_ptr->isArrayType()) {
									is_pointer_to_pointer = true;
								}
							} else { assert(false); }

							auto& precasted_expr_ptr_ecs_ref = state1.get_expr_conversion_state_ref(*precasted_expr_ptr, Rewrite);

							std::shared_ptr<CExprTextModifier> shptr1;

							if (is_pointer_to_pointer) {
								{
									shptr1 = std::make_shared<CUnsafeMakeTemporaryArrayOfRawPointersFromExprTextModifier>();
									for  (auto& expr_text_modifier_shptr_ref : precasted_expr_ptr_ecs_ref.m_expr_text_modifier_stack) {
										if ("unsafe make temporary array of raw pointers from" == expr_text_modifier_shptr_ref->species_str()) {
											/* already applied */
											shptr1 = nullptr;
											break;
										}
									}
								}
							} else {
								{
									shptr1 = std::make_shared<CUnsafeMakeRawPointerFromExprTextModifier>();
									for  (auto& expr_text_modifier_shptr_ref : precasted_expr_ptr_ecs_ref.m_expr_text_modifier_stack) {
										if ("unsafe make raw pointer from" == expr_text_modifier_shptr_ref->species_str()) {
											/* already applied */
											shptr1 = nullptr;
											break;
										}
									}
								}
							}
							if (shptr1) {
								precasted_expr_ptr_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
								precasted_expr_ptr_ecs_ref.update_current_text();

								//state1.add_pending_expression_update(*precasted_expr_ptr, Rewrite);
							}
						}
					}
				}

				std::string new_cast_prefix;
				std::string new_cast_suffix;
				bool preconversion_expression_is_void_star = false;
				bool converted_expression_is_void_star = false;
				bool apparent_const_correctness_violation = false;
				bool seems_to_be_a_benign_cast = (CSCE->getType() == CSCE->getSubExpr()->getType());
				if (CSCE->getType()->isPointerType() && CSCE->getSubExpr()->getType()->isPointerType()) {
					const auto og_converted_pointee_qtype = CSCE->getType()->getPointeeType();
					const auto og_preconversion_pointee_qtype = CSCE->getSubExpr()->getType()->getPointeeType();
					const std::string og_converted_pointee_qtype_str = og_converted_pointee_qtype.getAsString();
					const std::string og_preconversion_pointee_qtype_str = og_preconversion_pointee_qtype.getAsString();
					if ((!og_converted_pointee_qtype.isConstQualified()) && og_preconversion_pointee_qtype.isConstQualified()) {
						apparent_const_correctness_violation = true;
					} else if (og_converted_pointee_qtype == og_preconversion_pointee_qtype.withConst()) {
						seems_to_be_a_benign_cast |= true;
					}

					bool preconversion_expression_is_const_void_star = ("const void" == og_preconversion_pointee_qtype_str);
					bool preconversion_expression_is_nonconst_void_star = ("void" == og_preconversion_pointee_qtype_str);
					preconversion_expression_is_void_star = preconversion_expression_is_const_void_star || preconversion_expression_is_nonconst_void_star;
					bool converted_expression_is_const_void_star = ("const void" == og_converted_pointee_qtype_str);
					bool converted_expression_is_nonconst_void_star = ("void" == og_converted_pointee_qtype_str);
					converted_expression_is_void_star = converted_expression_is_const_void_star || converted_expression_is_nonconst_void_star;
				}
				if (new_cast_prefix.empty()) {
					if (((preconversion_expression_is_void_star || converted_expression_is_void_star) && (!apparent_const_correctness_violation))
						|| seems_to_be_a_benign_cast) {

						if ("Dual" == ConvertMode) {
							new_cast_prefix = "MSE_LH_CAST("
								+ replacement_qtype_str + ", ";
						} else {
							new_cast_prefix = "("
								+ replacement_qtype_str + ")(";
						}
					} else {
						if ("Dual" == ConvertMode) {
							new_cast_prefix = "MSE_LH_UNSAFE_CAST("
								+ replacement_qtype_str + ", ";
						} else {
							new_cast_prefix = "mse::us::lh::unsafe_cast<"
								+ replacement_qtype_str + ">(";
						}
					}
					new_cast_suffix = ")";
				}

				auto& ecs_ref1 = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, new_cast_prefix, new_cast_suffix);
				auto expr_text_modifier_stack1 = ecs_ref1.m_expr_text_modifier_stack;
				/* Here we make sure that any existing "conversion state" associated with this cast expression 
				is overwritten by our new one. */
				auto& ecs_ref = state1.set_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, new_cast_prefix, new_cast_suffix);
				/* But we'll restore any previously present "text modifiers", as they may be associated with modifications 
				that are (technically) distinct from the cast operation. (For example, a text modifier might wrap the 
				expression in a make_raw_pointer_from() call.) */
				ecs_ref.m_expr_text_modifier_stack = expr_text_modifier_stack1;
				ecs_ref.update_current_text();

				if (CSCESR.isValid()) {
					//state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);

					auto& current_text_ref = ecs_ref.current_text();
					if (current_text_ref != ecs_ref.m_original_source_text_str) {
						state1.m_pending_code_modification_actions.ReplaceText(Rewrite, CSCESR, current_text_ref);
					}
				}
				//finished_cast_handling_flag = true;
			} else {
				/* unexpected */
				int q = 3;
			}
		}

		enum class EIsAnInitialization { Yes, No };

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::Expr* LHS, const clang::Expr* RHS, const clang::ValueDecl* VLD = nullptr
			, EIsAnInitialization is_an_initialization = EIsAnInitialization::No, std::optional<int> maybe_lhs_indirection_level_adjustment = {}) {

			if (!RHS) {
				return;
			}

			auto SR = cm1_adj_nice_source_range(RHS->getSourceRange(), state1, Rewrite);
			RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

			DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

			auto& SM = Rewrite.getSourceMgr();
			auto b10 = SM.isMacroArgExpansion(RHS->getSourceRange().getBegin());
			auto b10b = SM.isMacroArgExpansion(RHS->getSourceRange().getEnd());
			if (b10 && b10b) {
				/* The RHS expression seems like it might be an argument to a function macro. If the macro is some 
				kind of "system" macro that can't be changed, then the RHS expression may have to be adjusted to 
				accommodate that. */
				int q = 5;
			} else {
				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
			}

			bool vld_is_filtered_out = false;
			if (VLD) {
				auto VLD_SR = cm1_adj_nice_source_range(VLD->getSourceRange(), state1, Rewrite);
				if ((!VLD_SR.isValid()) || filtered_out_by_location<options_t<converter_mode_t> >(MR, VLD_SR.getBegin())) {
					vld_is_filtered_out = true;
				}
			}
			bool rhs_is_filtered_out = false;
			if ((!SR.isValid()) || filtered_out_by_location<options_t<converter_mode_t> >(MR, SR.getBegin())) {
				if (vld_is_filtered_out) {
					//return void();
				}
				rhs_is_filtered_out = true;
			}

			DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
			if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			if (VLD) {
				auto SR = cm1_adj_nice_source_range(VLD->getSourceRange(), state1, Rewrite);
				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);
				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(VLD->getType());
			}

			CArrayInferenceInfo lhs_res2;
			if (LHS) {
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(LHS->getType());
				lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", state1);
			} else {
				const DeclaratorDecl* lhs_DD = llvm::cast<const clang::DeclaratorDecl>(VLD);
				if (lhs_DD) {
					auto found_it = state1.m_ddecl_conversion_state_map.find(lhs_DD);
					if (state1.m_ddecl_conversion_state_map.end() == found_it) {
						decl_util_handler1(MR, Rewrite, state1, lhs_DD);
					}
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*lhs_DD, &Rewrite);

					lhs_res2.ddecl_cptr = lhs_DD;
					lhs_res2.ddecl_conversion_state_ptr = &ddcs_ref;
					lhs_res2.indirection_level = 0;
					lhs_res2.update_declaration_flag = update_declaration_flag;
				} else {
					assert(false);
					return;
				}
			}
			if (maybe_lhs_indirection_level_adjustment.has_value()) {
				auto& lhs_indirection_level_adjustment_param = maybe_lhs_indirection_level_adjustment.value();
				/* The maybe_lhs_indirection_level_adjustment is intended to enable synthesized assignment operations 
				where the intended lhs declaration (or expression) is not available, but is an indirection of one that 
				is available. In such case, one can pass the available declaration (or expression) and also an 
				lhs_indirection_level_adjustment value indicating which indirecction level of the provided declaration 
				(or expression) is intended to as the lhs.  */
				if (lhs_res2.ddecl_conversion_state_ptr) {
					if (lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size() >= (lhs_res2.indirection_level + lhs_indirection_level_adjustment_param)) {
						lhs_res2.indirection_level += lhs_indirection_level_adjustment_param;
					} else {
						int q = 3;
						return;
					}
				}
			}
			std::optional<clang::QualType> maybe_adjusted_lhs_qtype;
			if (lhs_res2.ddecl_conversion_state_ptr) {
				if (lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size() > (lhs_res2.indirection_level)) {
					auto& indirection_state_ref = lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.at(lhs_res2.indirection_level);
					maybe_adjusted_lhs_qtype = indirection_state_ref.m_maybe_original_qtype;
				} else if (lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size() == (lhs_res2.indirection_level)) {
					maybe_adjusted_lhs_qtype = lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.m_direct_type_state.maybe_original_qtype();
				} else {
					int q = 3;
				}
			}
			std::string adjusted_lhs_qtype_str;
			if (maybe_adjusted_lhs_qtype.has_value()) {
				adjusted_lhs_qtype_str = maybe_adjusted_lhs_qtype.value().getAsString();
			}

			auto DD = lhs_res2.ddecl_cptr;
			MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(RHS->getType());

			auto RHS_ii = IgnoreParenImpNoopCasts(RHS, *(MR.Context));

			std::optional<clang::QualType> maybe_VLD_effective_qtype;
			if (VLD) {
				auto FND = dyn_cast<const clang::FunctionDecl>(VLD);
				if (FND) {
					maybe_VLD_effective_qtype = FND->getReturnType();
				} else {
					maybe_VLD_effective_qtype = VLD->getType();
				}
			}

			auto lhs_qtype = LHS ? LHS->getType() : maybe_VLD_effective_qtype.value();

			IF_DEBUG(std::string LHS_qtype_str = LHS ? LHS->getType().getAsString() : (maybe_VLD_effective_qtype.has_value() ? maybe_VLD_effective_qtype.value().getAsString() : "");)
			IF_DEBUG(std::string RHS_qtype_str = RHS->getType().getAsString();)
			IF_DEBUG(std::string RHS_ii_qtype_str = RHS_ii->getType().getAsString();)

			RETURN_IF_DEPENDENT_TYPE_CONV1(RHS->getType());

			//auto lhs_res2 = infer_array_type_info_from_stmt(*LHS, "", state1);
			auto rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", state1);
			bool lhs_is_an_indirect_type = LHS ? is_an_indirect_type(LHS->getType()) : (VLD ? is_an_indirect_type(VLD->getType()) : false);
			bool rhs_is_an_indirect_type = is_an_indirect_type(RHS->getType());
			if (lhs_is_an_indirect_type != rhs_is_an_indirect_type) {
				/* This might happen if one side is a reference type and the other side isn't. */
				int q = 5;
			}

			bool LHS_decl_is_non_modifiable = false;
			bool RHS_decl_is_non_modifiable = false;
			if (lhs_res2.ddecl_cptr) {
				LHS_decl_is_non_modifiable = is_non_modifiable(*(lhs_res2.ddecl_cptr), MR, Rewrite, state1, LHS);
				if (lhs_res2.ddecl_conversion_state_ptr) {
					lhs_res2.ddecl_conversion_state_ptr->m_maybe_is_non_modifiable = LHS_decl_is_non_modifiable;
				}
			}
			if (rhs_res2.ddecl_cptr) {
				RHS_decl_is_non_modifiable = is_non_modifiable(*(rhs_res2.ddecl_cptr), MR, Rewrite, state1, RHS);
				if (rhs_res2.ddecl_conversion_state_ptr) {
					rhs_res2.ddecl_conversion_state_ptr->m_maybe_is_non_modifiable = RHS_decl_is_non_modifiable;
				}
			}

			auto rhsii_EX = RHS->IgnoreParenImpCasts();
			auto rhsii_stmt_class = rhsii_EX->getStmtClass();
			//clang::Stmt::StmtClass::CXXStaticCastExprClass;
			if (rhs_is_an_indirect_type && (rhsii_EX->getStmtClass() == clang::Stmt::StmtClass::CStyleCastExprClass)) {
				auto CSCE = dyn_cast<const clang::CStyleCastExpr>(rhsii_EX);
				const clang::CXXStaticCastExpr* CXXSCE = dyn_cast<const clang::CXXStaticCastExpr>(rhsii_EX);
				assert(CSCE || CXXSCE);
				auto cast_qtype = definition_qtype(CSCE ? CSCE->getTypeAsWritten() : CXXSCE->getTypeAsWritten());
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(cast_qtype);
				bool finished_cast_handling_flag = false;

				if (CSCE) {
					auto csce_QT = definition_qtype(CSCE->getType());
					MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
					auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
					assert(precasted_expr_ptr);
					const auto precasted_expr_qtype = precasted_expr_ptr->getType();
					auto precasted_expr_SR = cm1_adj_nice_source_range(precasted_expr_ptr->getSourceRange(), state1, Rewrite);
					auto CSCESR = write_once_source_range(cm1_adj_nice_source_range(CSCE->getSourceRange(), state1, Rewrite));
					auto cast_operation_SR = write_once_source_range(cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite));

					if (precasted_expr_qtype->isPointerType()) {
						const std::string precasted_expr_qtype_str = precasted_expr_qtype.getAsString();
						if (("const void *" == precasted_expr_qtype_str) || ("void *" == precasted_expr_qtype_str)) {
							/* At this point, `void *` (or rather, its replacement) doesn't preserve "xscope" eligibility information, 
							so we'll mark any object that is assigned a value from a `void *` as ineligible for "xscope" status. */
							if (lhs_res2.ddecl_conversion_state_ptr) {
								auto& ddcs = *(lhs_res2.ddecl_conversion_state_ptr);
								if (ddcs.m_indirection_state_stack.size() > lhs_res2.indirection_level) {
									auto& indirection_state_ref = ddcs.m_indirection_state_stack.at(lhs_res2.indirection_level);
									indirection_state_ref.set_xscope_eligibility(false);
								}
							}
						}
					}
					do {
						if ((csce_QT->isPointerType()) && precasted_expr_ptr->getType()->isPointerType() && cast_operation_SR.isValid()) {
							std::string csce_QT_str = csce_QT.getAsString();
							if (false && ("void *" == csce_QT_str)) {
								/* This case is handled by the MCSSSExprUtil handler. */
								finished_cast_handling_flag = true;
								break;
							}
						}

						auto precasted_CE = llvm::dyn_cast<const clang::CallExpr>(precasted_expr_ptr->IgnoreParenCasts());
						if (precasted_CE) {
							auto alloc_function_info1 = analyze_malloc_resemblance(*precasted_CE, state1, Rewrite);
							if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
								/* This case is handled elsewhere. */
								finished_cast_handling_flag = true;
								break;
							}
						}

						if (csce_QT->isPointerType() /*"void *" == csce_QT.getAsString()*/) {
							auto IL = dyn_cast<const clang::IntegerLiteral>(precasted_expr_ptr);
							if (IL) {
								/* This case is handled in the handle_c_style_cast_without_context() handler. */
								finished_cast_handling_flag = true;
								break;
							}
						}
					} while (false);

					if ((!finished_cast_handling_flag) && lhs_res2.ddecl_conversion_state_ptr && ConvertToSCPP) {

						auto lambda = [&Rewrite, &state1, lhs_qtype, lhs_res2, CSCE, LHS_decl_is_non_modifiable, RHS_decl_is_non_modifiable]() {
							if (CSCE) {
								MCSSSAssignment::s_c_style_cast_of_rhs((Rewrite), state1, *CSCE, lhs_qtype, lhs_res2, LHS_decl_is_non_modifiable, RHS_decl_is_non_modifiable);
							}
						};

						state1.m_pending_code_modification_actions.add_replacement_action(CSCESR, lambda);
						finished_cast_handling_flag = true;

						/* We've queued the modification action for deferred execution, but we don't want to delay the
						establishment of the expression conversion state because, among other reasons, it reads from 
						and stores the original source text and we want that done before the source text gets 
						potentially modified. */
						auto& ecs_ref = state1.get_expr_conversion_state_ref(*CSCE, Rewrite);
						/* Note, the expression conversion state of subexpressions should be constructed after the establishment 
						of any ancestor expressions as they will automatically establish relationships with any pre-existing 
						ancestors (but not descendants). */
						auto& precasted_ecs_ref = state1.get_expr_conversion_state_ref(*precasted_expr_ptr, Rewrite);
					}
				}
				if (!finished_cast_handling_flag) {
					if (DD && (cast_qtype->isPointerType() || cast_qtype->isReferenceType())) {
						bool b1 = (DD->getType()->isPointerType() || (DD->getType()->isReferenceType()));
						clang::FunctionDecl const * FND = nullptr;
						if (!b1) {
							FND = dyn_cast<const clang::FunctionDecl>(DD);
							if (FND && (FND->getReturnType()->isPointerType() || FND->getReturnType()->isReferenceType())) {
								b1 = true;
							}
						}
						if (b1) {
							auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

							auto cast_pointee_qtype = definition_qtype(cast_qtype->getPointeeType());
							auto DD_pointee_qtype = FND ? definition_qtype(FND->getReturnType()->getPointeeType()) : definition_qtype(DD->getType()->getPointeeType());

							auto const_adjusted_cast_pointee_qtype = cast_pointee_qtype;
							auto const_cast_pointee_qtype = const_adjusted_cast_pointee_qtype;
							const_cast_pointee_qtype.addConst();
							if (const_cast_pointee_qtype == DD_pointee_qtype) {
								const_adjusted_cast_pointee_qtype = const_cast_pointee_qtype;
							}
							IF_DEBUG(std::string const_adjusted_cast_pointee_qtype_str = const_adjusted_cast_pointee_qtype.getAsString();)
							IF_DEBUG(std::string const_cast_pointee_qtype_str = const_cast_pointee_qtype.getAsString();)
							IF_DEBUG(std::string DD_qtype_str = definition_qtype(DD->getType()).getAsString();)

							if (ConvertToSCPP) {
								if (DD_pointee_qtype == const_adjusted_cast_pointee_qtype) {
									for (size_t i = 0; i < ddcs_ref.m_indirection_state_stack.size(); ++i) {
										std::shared_ptr<CArray2ReplacementAction> cr_shptr = std::make_shared<CTargetConstrainsCStyleCastExprArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*DD, i), *CSCE);

										if (ddcs_ref.has_been_determined_to_point_to_an_array(i)) {
											(*cr_shptr).do_replacement(state1);
										}
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
									}
								} else {
									CSCE ? handle_c_style_cast_without_context(MR, Rewrite, state1, CSCE)
										: handle_cxx_static_cast_without_context(MR, Rewrite, state1, CXXSCE);
								}
							}
						}
					}
				}
			}

			if (lhs_res2.ddecl_cptr) {
				auto LHSDD_SR = cm1_adj_nice_source_range(lhs_res2.ddecl_cptr->getSourceRange(), state1, Rewrite);
				if ((LHS_decl_is_non_modifiable /*&& (!RHS_decl_is_non_modifiable)*/) && (LHS || VLD) && RHS) {
					/* LHS will, for whatever reason, not be converted to a safe pointer. But presumably the RHS will 
					(or at least could) be. So we may need to add an unsafe cast from the RHS safe pointer to the LHS
					raw pointer. */
					auto LHS_qtype = LHS ? LHS->getType() : maybe_VLD_effective_qtype.value();
					auto RHS_qtype = RHS->getType();
					
					RETURN_IF_DEPENDENT_TYPE_CONV1(LHS_qtype);

					assert(RHS->getType().getTypePtrOrNull());
					auto rhs_source_range = write_once_source_range(cm1_adj_nice_source_range(RHS->getSourceRange(), state1, Rewrite));
					std::string rhs_source_text;
					if (rhs_source_range.isValid()) {
						IF_DEBUG(rhs_source_text = Rewrite.getRewrittenText(rhs_source_range);)
					}

					auto DRE = given_or_descendant_DeclRefExpr(RHS_ii, *(MR.Context));

					if ((nullptr != DRE) && rhs_source_range.isValid()
						&& LHS_qtype->isPointerType()) {

						if (!LHS_qtype->isFunctionPointerType()) {
							assert(nullptr != RHS);
							auto& rhs_ecs_ref = state1.get_expr_conversion_state_ref(*RHS, Rewrite);

							if (ConvertToSCPP) {

								std::string rhs_function_qname_if_any;
								auto rhs_CE = dyn_cast<const clang::CallExpr>(RHS_ii);
								if (rhs_CE) {
									auto rhs_function_decl1 = rhs_CE->getDirectCallee();
									if (rhs_function_decl1) {
										rhs_function_qname_if_any = rhs_function_decl1->getQualifiedNameAsString();
									}
								}

								std::shared_ptr<CExprTextModifier> shptr1;
								auto RHS_qtype_str = RHS_qtype.getAsString();
								if (!string_begins_with(rhs_function_qname_if_any, "mse::")) {
									shptr1 = std::make_shared<CUnsafeMakeRawPointerFromExprTextModifier>();
									if (1 <= rhs_ecs_ref.m_expr_text_modifier_stack.size()) {
										if ("unsafe make raw pointer from" == rhs_ecs_ref.m_expr_text_modifier_stack.back()->species_str()) {
											/* already applied */
											return;
										}
									}
								}
								if (shptr1) {
									rhs_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
									rhs_ecs_ref.update_current_text();

									state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, rhs_source_range, state1, RHS);
									//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, rhs_source_range, (*rhs_shptr_ref).current_text());
									//(*this).Rewrite.ReplaceText(rhs_source_range, (*rhs_shptr_ref).current_text());
									return;
								}
							} else {
								int q = 5;
							}
						} else {
							/* It seems the lhs is of function pointer type. */
							std::string lhs_text;
							if (VLD) {
								auto PVD = dyn_cast<const clang::ParmVarDecl>(VLD);
								if (PVD) {
									/* The lhs, being a parameter declaration, doesn't really contain an accessible expression or value 
									from which the (function pointer) type can be deduced. So we're going to (try to) manufacture one. */
									lhs_text = LHS_qtype_str + "{}";
								} else {
									lhs_text = VLD->getNameAsString();
								}
							} else if (LHS) {
								auto lhs_SR = cm1_adj_nice_source_range(LHS->getSourceRange(), state1, Rewrite);
								lhs_text = Rewrite.getRewrittenText(lhs_SR);
								if ("" == lhs_text) {
									auto lhs_SRPlus = cm1_adjusted_source_range(LHS->getSourceRange(), state1, Rewrite);
									//lhs_text = lhs_SRPlus.m_adjusted_source_text_as_if_expanded;;
								}
							} else { assert(false); }

							if ("" != lhs_text) {
								std::string make_raw_fn_wrapper_prefix_str;
								if ("Dual" == ConvertMode) {
									make_raw_fn_wrapper_prefix_str = "MSE_LH_UNSAFE_MAKE_RAW_FN_WRAPPER_SHORT1(";
								} else {
									make_raw_fn_wrapper_prefix_str = "mse::us::lh::make_raw_fn_wrapper(";
								}

								std::string make_raw_fn_wrapper_suffix_str;
								if ("Dual" == ConvertMode) {
									make_raw_fn_wrapper_suffix_str = std::string(", (") + lhs_text + "))";
								} else {
									make_raw_fn_wrapper_suffix_str = std::string(", (") + lhs_text + "))";
								}

								if (RHS_ii->getType()->isFunctionType()) {
									/* The rhs seems to be a function rather than a function pointer. */
									make_raw_fn_wrapper_prefix_str += "&(";
									make_raw_fn_wrapper_suffix_str = ")" + make_raw_fn_wrapper_suffix_str;
								}

								auto& ecs_ref = state1.get_expr_conversion_state_ref(*RHS_ii, Rewrite);
								const auto l_text_modifier = CWrapExprTextModifier(make_raw_fn_wrapper_prefix_str, make_raw_fn_wrapper_suffix_str);
								bool seems_to_be_already_applied = ((1 <= ecs_ref.m_expr_text_modifier_stack.size()) && ("wrap" == ecs_ref.m_expr_text_modifier_stack.back()->species_str()) 
									&& (l_text_modifier.is_equal_to(*(ecs_ref.m_expr_text_modifier_stack.back()))));
								if (!seems_to_be_already_applied) {
									auto shptr2 = std::make_shared<CWrapExprTextModifier>(make_raw_fn_wrapper_prefix_str, make_raw_fn_wrapper_suffix_str);
									ecs_ref.m_expr_text_modifier_stack.push_back(shptr2);
									ecs_ref.update_current_text();

									auto rhs_ii_SR = cm1_adj_nice_source_range(RHS_ii->getSourceRange(), state1, Rewrite);
									if (rhs_ii_SR.isValid()) {
										state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, rhs_ii_SR, state1, RHS_ii);
									}
								}

								int q = 5;
							} else {
								int q = 3;
							}
						}
					} else {
						int q = 5;
					}
					int q = 5;
		
					return;
				}
			}

			int lhs_indirection_level_adjustment = 0;
			auto rhs_res3 = leading_addressof_operator_info_from_stmt(*RHS);
			if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
				assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

				RHS = rhs_res3.without_leading_addressof_operator_expr_cptr;
				rhs_res2 = infer_array_type_info_from_stmt(*RHS, "", state1);

				auto without_leading_addressof_operator_expr_cptr_ii = IgnoreParenImpNoopCasts(rhs_res3.without_leading_addressof_operator_expr_cptr, *(MR.Context));
				if (false && (clang::Stmt::StmtClass::ArraySubscriptExprClass == (*(without_leading_addressof_operator_expr_cptr_ii)).getStmtClass())) {
					assert(llvm::isa<const clang::ArraySubscriptExpr>(without_leading_addressof_operator_expr_cptr_ii));
					if (1 <= rhs_res2.indirection_level) {
						rhs_res2.indirection_level -= 1;
					} else {
						int q = 3;
					}
				} else {
					lhs_indirection_level_adjustment += 1;
				}
			}

			if (llvm::isa<const clang::CallExpr>(RHS->IgnoreParenCasts()) && rhs_is_an_indirect_type) {
				auto CE = llvm::cast<const clang::CallExpr>(RHS->IgnoreParenCasts());
				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* This seems to be some kind of malloc/realloc function. These case should not be
					* handled here. They are handled elsewhere. */
					return;
				}
				if (lhs_res2.ddecl_cptr && lhs_res2.ddecl_conversion_state_ptr) {
					auto& lhs_ddecl_ref = *(lhs_res2.ddecl_cptr);
					auto& lhs_ddcs_ref = *(lhs_res2.ddecl_conversion_state_ptr);
					if (lhs_ddcs_ref.m_indirection_state_stack.size() > lhs_res2.indirection_level) {
						auto& indirection_state_ref = lhs_ddcs_ref.m_indirection_state_stack.at(lhs_res2.indirection_level);
						auto lhs_ddecl_indirection = CDDeclIndirection(lhs_ddecl_ref, lhs_res2.indirection_level);

						auto function_decl = CE->getDirectCallee();
						auto num_args = CE->getNumArgs();
						if (function_decl) {
							const std::string function_name = function_decl->getNameAsString();
							const std::string function_qname = function_decl->getQualifiedNameAsString();
							auto maybe_function_index = s_function_conversion_index_if_any(function_qname);
							if (maybe_function_index.has_value()) {
								auto& fc_info = s_function_conversion_infos().at(maybe_function_index.value());
								if (fc_info.m_maybe_num_parameters.has_value()) {
									if (num_args == fc_info.m_maybe_num_parameters.value()) {

										/* We're not necessarily in a position to deduce much about a pointer returned from a presumably 
										opaque (standard library or system) function that is slated to be converted to a safe counterpart 
										implementation. So we'll treat it as if it could be potentially point to malloc() or non-malloc() 
										targets. */
										if (!(indirection_state_ref.is_known_to_have_non_malloc_target())) {
											indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
											state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
										}
										if (!(indirection_state_ref.is_known_to_have_malloc_target())) {
											indirection_state_ref.set_is_known_to_have_malloc_target(true);
											state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
										}
									}
								}

							}
						} else {
							/* This might be the invocation of a function call via function pointer. */

							if (rhs_res2.ddecl_cptr && rhs_res2.ddecl_conversion_state_ptr) {
								auto& rhs_ddecl_ref = *(rhs_res2.ddecl_cptr);
								auto& rhs_ddcs_ref = *(rhs_res2.ddecl_conversion_state_ptr);
								if (rhs_ddcs_ref.m_indirection_state_stack.size() > rhs_res2.indirection_level) {
									auto& indirection_state_ref = rhs_ddcs_ref.m_indirection_state_stack.at(rhs_res2.indirection_level);
									auto rhs_ddecl_indirection = CDDeclIndirection(rhs_ddecl_ref, rhs_res2.indirection_level);

									bool b1 = indirection_state_ref.current_is_function_type();

									if (rhs_ddcs_ref.m_indirection_state_stack.size() > 1 + rhs_res2.indirection_level) {
										auto& pointee_indirection_state_ref = rhs_ddcs_ref.m_indirection_state_stack.at(1 + rhs_res2.indirection_level);
										auto rhs_pointee_ddecl_indirection = CDDeclIndirection(rhs_ddecl_ref, 1 + rhs_res2.indirection_level);

										bool b2 = pointee_indirection_state_ref.current_is_function_type();
										if (b2) {
											/* You can invoke a function call from a function pointer, `fnptr`, by dereferencing the function 
											pointer like so: `(*fnptr)()`. But you can also equivalently omit the dereference operation: `fnptr()`, 
											right? That appears to be what's going on here. So we're going to adjust the rhs indirection level so 
											that the rhs expression will be treated as if the function pointer were dereferenced. */
											rhs_res2.indirection_level += 1;
										}
										int q = 5;
									} else {
										auto& direct_type_state_ref = rhs_ddcs_ref.m_indirection_state_stack.m_direct_type_state;
										bool b3 = direct_type_state_ref.seems_to_be_a_function_type();
										if (b3) {
											/* You can invoke a function call from a function pointer, `fnptr`, by dereferencing the function 
											pointer like so: `(*fnptr)()`. But you can also equivalently omit the dereference operation: `fnptr()`, 
											right? That appears to be what's going on here. So we're going to adjust the rhs indirection level so 
											that the rhs expression will be treated as if the function pointer were dereferenced. */
											rhs_res2.indirection_level += 1;
										}
										int q = 5;
									}
								}
							}
						}
					} else {
						int q = 3;
					}
				}
			} else if (llvm::isa<const clang::ConditionalOperator>(RHS->IgnoreParenCasts()) && rhs_is_an_indirect_type) {
				auto CO = llvm::cast<const clang::ConditionalOperator>(RHS->IgnoreParenCasts());
				std::optional<const DeclaratorDecl*> maybe_DD;
				if (lhs_res2.ddecl_cptr) {
					maybe_DD = lhs_res2.ddecl_cptr;
				}
				MCSSSConditionalExpr::s_handler1(MR, Rewrite, state1, CO, maybe_DD, lhs_res2.indirection_level);
			}

			if (lhs_res2.ddecl_conversion_state_ptr && RHS_ii->getType()->isFunctionType()) {
				auto& lhs_ddecl_ref = *(lhs_res2.ddecl_cptr);
				auto& lhs_ddcs_ref = *(lhs_res2.ddecl_conversion_state_ptr);

				if (lhs_ddcs_ref.m_indirection_state_stack.size() > 1 + lhs_res2.indirection_level) {
					auto& pointee_indirection_state_ref = lhs_ddcs_ref.m_indirection_state_stack.at(1 + lhs_res2.indirection_level);
					auto lhs_pointee_ddecl_indirection = CDDeclIndirection(lhs_ddecl_ref, 1 + lhs_res2.indirection_level);

					bool b2 = pointee_indirection_state_ref.current_is_function_type();
					if (b2) {
						/* You can assign a function value to a function pointer, `fnptr`, by taking the address of the 
						function like so: `fnptr = &strlen;`. But you can also equivalently omit the address taking operation: 
						`fnptr = strlen;`, right? That appears to be what's going on here. So we're going to adjust the lhs 
						indirection level so that it will be treated as if the address were taken. */
						lhs_indirection_level_adjustment += 1;
					}
					int q = 5;
				} else {
					auto& direct_type_state_ref = lhs_ddcs_ref.m_indirection_state_stack.m_direct_type_state;
					bool b3 = direct_type_state_ref.seems_to_be_a_function_type();
					if (b3) {
						/* You can assign a function value to a function pointer, `fnptr`, by taking the address of the 
						function like so: `fnptr = &strlen;`. But you can also equivalently omit the address taking operation: 
						`fnptr = strlen;`, right? That appears to be what's going on here. So we're going to adjust the lhs 
						indirection level so that it will be treated as if the address were taken. */
						lhs_indirection_level_adjustment += 1;
					}
					int q = 5;
				}
			}

			if (rhs_is_an_indirect_type && rhs_res2.ddecl_cptr) {
				auto LHS_qtype = LHS ? LHS->getType() : VLD->getType();
				auto RHS_qtype = RHS->getType();
				assert(RHS->getType().getTypePtrOrNull());

				/* If the type definition is non-modifiable then we hesitate to convert items declared as that type. 
				For example, you could imagine a library defining a "handle" type as a pointer (at least on cetain 
				platforms). We probably wouldn't want to convert items declared as that handle type to safe pointers. */
				auto RHS_type_definition_is_non_modifiable = type_definition_is_non_modifiable(RHS_qtype, MR, Rewrite, state1);

				if ((RHS_decl_is_non_modifiable && (!LHS_decl_is_non_modifiable)) && (LHS || VLD) && RHS && (!RHS_type_definition_is_non_modifiable)) {
					/* RHS will, for whatever reason, not be converted to a safe pointer. But presumably the LHS will 
					(or at least could) be. So we may need to add an unsafe cast from the RHS raw pointer to the LHS 
					safe pointer. */
					auto rhs_source_range = write_once_source_range(cm1_adj_nice_source_range(RHS->getSourceRange(), state1, Rewrite));
					std::string rhs_source_text;
					if (rhs_source_range.isValid()) {
						IF_DEBUG(rhs_source_text = Rewrite.getRewrittenText(rhs_source_range);)
					}

					auto RHS_ii = IgnoreParenImpNoopCasts(RHS, *(MR.Context));

					auto DRE = given_or_descendant_DeclRefExpr(RHS_ii, *(MR.Context));

					if ((nullptr != DRE) && rhs_source_range.isValid()
						&& LHS_qtype->isPointerType() && (!LHS_qtype->isFunctionPointerType())) {

						assert(nullptr != RHS_ii);
						auto& rhs_ecs_ref = state1.get_expr_conversion_state_ref(*RHS_ii, Rewrite);

						if (ConvertToSCPP) {

							std::string rhs_function_qname_if_any;
							auto rhs_CE = dyn_cast<const clang::CallExpr>(RHS_ii);
							if (rhs_CE) {
								auto rhs_function_decl1 = rhs_CE->getDirectCallee();
								if (rhs_function_decl1) {
									rhs_function_qname_if_any = rhs_function_decl1->getQualifiedNameAsString();
								}
							}

							auto RHS_ii_qtype_str = RHS_ii->getType().getAsString();
							if ((("void *" == RHS_ii_qtype_str) || ("const void *" == RHS_ii_qtype_str)) 
								&& !(("void *" == RHS_qtype_str) || ("const void *" == RHS_qtype_str))) {
								/* RHS expression seems to consist of an ("non-modifiable") void pointer being converted to something 
								else. (Presumably some non-void pointer.) Being non-modifiable, presumably we're going to wrap it in 
								a function call that (unsafely) creates a "safe" iterator interface wrapping for the raw pointer. But 
								in the case of void pointers, we'd want to cast it to the non-void pointer type first so that the 
								iterator interface wrapping will be created with the proper type. */

								std::shared_ptr<CExprTextModifier> shptr1;
								if (!string_begins_with(rhs_function_qname_if_any, "mse::")) {
									bool seems_to_be_already_applied = false;
									for (auto& expr_text_modifier_shptr : rhs_ecs_ref.m_expr_text_modifier_stack) {
										if ("cast" == expr_text_modifier_shptr->species_str()) {
											seems_to_be_already_applied = true;
											break;
										}
									}
									if (!seems_to_be_already_applied) {
										shptr1 = std::make_shared<CCastExprTextModifier>(RHS_qtype_str);
									}
								}
								if (shptr1) {
									rhs_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
									rhs_ecs_ref.update_current_text();
								}
							}
							if (("FILE *" != RHS_qtype_str)) {
								std::shared_ptr<CExprTextModifier> shptr1;
								if (!string_begins_with(rhs_function_qname_if_any, "mse::")) {
									bool seems_to_be_already_applied = false;
									for (auto& expr_text_modifier_shptr : rhs_ecs_ref.m_expr_text_modifier_stack) {
										if ("unsafe make lh_nullable_any_random_access_iterator from" == expr_text_modifier_shptr->species_str()) {
											seems_to_be_already_applied = true;
											break;
										}
									}
									if (!seems_to_be_already_applied) {
										shptr1 = std::make_shared<CUnsafeMakeLHNullableAnyRandomAccessIteratorFromExprTextModifier>();
									}
								}
								if (shptr1) {
									rhs_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
									rhs_ecs_ref.update_current_text();

									state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, rhs_source_range, state1, RHS);
									//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, rhs_source_range, (*rhs_shptr_ref).current_text());
									//(*this).Rewrite.ReplaceText(rhs_source_range, (*rhs_shptr_ref).current_text());
									//return;
								}
							}
						} else {
							int q = 5;
						}
					} else {
						int q = 5;
					}
					int q = 5;
		
					//return;
				}
			}

			auto ILE = dyn_cast<const clang::InitListExpr>(RHS_ii);
			if (ILE && lhs_res2.ddecl_conversion_state_ptr) {
				if (lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size() >= (lhs_res2.indirection_level + lhs_indirection_level_adjustment)) {
					auto adjusted_lhs_qtype = [&]() {
						if (maybe_adjusted_lhs_qtype.has_value()) {
							return maybe_adjusted_lhs_qtype.value();
						}
						return lhs_qtype;
					};
					bool is_array_type = false;
					const auto adj_lhs_qtype = adjusted_lhs_qtype();
					if (adj_lhs_qtype->isArrayType()) {
						auto element_TypePtr = adj_lhs_qtype->getPointeeOrArrayElementType();
						const auto num_int_items = ILE->getNumInits();
						for (size_t i = 0; num_int_items > i; i += 1) {
							auto init_item_E = ILE->getInit(i);
							if (init_item_E) {
								/* For each item in the initializer list, we'll process a synthesized assignment operation. We need 
								to pass 1 as the argument for the maybe_lhs_indirection_level_adjustment parameter to indicate that 
								the provided RHS expression should not be assigned to the provided (native) array declaration, but 
								rather some element of the array. */
								auto l_maybe_lhs_indirection_level_adjustment = maybe_lhs_indirection_level_adjustment;
								if (l_maybe_lhs_indirection_level_adjustment.has_value()) {
									l_maybe_lhs_indirection_level_adjustment.value() += 1;
								} else {
									l_maybe_lhs_indirection_level_adjustment = 1;
								}
								/* recursion */
								MCSSSAssignment::s_handler1(MR, Rewrite, state1, LHS/*LHS*//*presumably null in this case*/, init_item_E/*RHS*/
									, VLD/*VLD*/, is_an_initialization, l_maybe_lhs_indirection_level_adjustment);
							} else {
								int q = 3;
							}
						}
					} else {
						const auto RD = adj_lhs_qtype->getAsRecordDecl();
						if (RD) {
							auto field_range = RD->fields();
							int num_fields = 0;
							for (auto iter = field_range.begin(); field_range.end() != iter; ++iter) {
								num_fields += 1;
							}
							const auto num_int_items = ILE->getNumInits();
							if (num_int_items <= num_fields) {
								size_t init_item_index = 0;
								for (auto iter = field_range.begin(); (field_range.end() != iter) && (num_int_items > init_item_index); ++iter) {
									auto init_item_E = ILE->getInit(init_item_index);
									auto FD = *iter;
									if (FD && init_item_E) {
										/* recursion */
										MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, init_item_E/*RHS*/, FD/*VLD*/, is_an_initialization);
									} else {
										int q = 3;
									}

									init_item_index += 1;
								}
							} else {
								int q = 7;
							}
						}
					}
				}

				/*** left off here ***/
				return;
			}

			if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && (rhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type && lhs_res2.ddecl_cptr) {
				auto& lhs_ddecl_ref = *(lhs_res2.ddecl_cptr);

				for (size_t i = 0; lhs_indirection_level_adjustment > i; i += 1) {
					/* These are the lhs indirection levels (actually, there should be at max one) that
					have no corresponding indirection level in the rhs variable because a `&`
					("address of") operator caused the correspondence to be shifted (by one level). */
					if (EIsAnInitialization::No == is_an_initialization) {
						auto& ddcs_ref = *(lhs_res2.ddecl_conversion_state_ptr);
						ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
						state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(lhs_ddecl_ref, CDDeclIndirection::no_indirection));
						size_t indirection_level = 0;
						for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
							indirection_state.set_xscope_eligibility(false);
							state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(lhs_ddecl_ref, indirection_level));
							++indirection_level;
						}
						lhs_res2.update_declaration_flag |= true;
					}
				}
				for (size_t i = 0; (i + lhs_res2.indirection_level + lhs_indirection_level_adjustment < (*(lhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size())
											&& (i + rhs_res2.indirection_level < (*(rhs_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack.size()); i += 1) {

					auto adjusted_lhs_indirection_level = i + lhs_res2.indirection_level + lhs_indirection_level_adjustment;
					auto rhs_indirection_level = i + rhs_res2.indirection_level;

					auto& lhs_indirection_state_ref = lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.at(adjusted_lhs_indirection_level);
					auto& rhs_indirection_state_ref = rhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.at(rhs_indirection_level);

					auto lhs_ddecl_indirection = CDDeclIndirection(lhs_ddecl_ref, adjusted_lhs_indirection_level);

					{
						auto& lhs_is_ineligible_for_xscope_status = lhs_indirection_state_ref.m_is_ineligible_for_xscope_status;
						auto& rhs_is_ineligible_for_xscope_status = rhs_indirection_state_ref.m_is_ineligible_for_xscope_status;

						if ((0 == i) && (EIsAnInitialization::No == is_an_initialization) && (false == lhs_is_ineligible_for_xscope_status)) {
							/* Currently, (non-initialization) assignment operations make the
							target (and therefore the source too) ineligible for xscope status. */
							lhs_is_ineligible_for_xscope_status = rhs_is_ineligible_for_xscope_status;
							lhs_res2.update_declaration_flag |= true;
							state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_ddecl_indirection);
						}

						if (lhs_is_ineligible_for_xscope_status) {
							if (!rhs_is_ineligible_for_xscope_status) {
								rhs_is_ineligible_for_xscope_status = lhs_is_ineligible_for_xscope_status;
								rhs_res2.update_declaration_flag |= true;
								state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_indirection_level));
							}
						}
					}
					{
						/* Here we're establishing and "enforcing" the constraint that the rhs value must
						* be of an (array) type that can be assigned to the lhs. */
						std::shared_ptr<CArray2ReplacementAction> cr_shptr;
						if (1 > adjusted_lhs_indirection_level) {
							cr_shptr = std::make_shared<CAssignmentTargetConstrainsSourceArray2ReplacementAction>(Rewrite, MR, lhs_ddecl_indirection, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_indirection_level));
						} else {
							/* Levels of indirection beyond the first one must be of the same type,
							* not just of "compatible" types. */
							cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR, lhs_ddecl_indirection, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_indirection_level));
						}

						(*cr_shptr).do_replacement(state1);
						state1.m_conversion_state_change_action_map.insert(cr_shptr);
						if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(adjusted_lhs_indirection_level)) {
							if (!(*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_dynamic_array(adjusted_lhs_indirection_level)) {
								state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							}
							if (!(*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_native_array(adjusted_lhs_indirection_level)) {
								state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
							}
						} else {
							state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);

							if ((*(lhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_be_ineligible_for_xscope_status(adjusted_lhs_indirection_level)) {
								(*cr_shptr).do_replacement(state1);
							} else {
								state1.m_xscope_ineligibility_contingent_replacement_map.insert(cr_shptr);
							}
						}
					}
					{
						/* Here we're establishing the constraint in the opposite direction as well. */

						if (rhs_res2.ddecl_cptr) {
							bool RHS_decl_is_non_modifiable = is_non_modifiable(*(rhs_res2.ddecl_cptr), MR, Rewrite, state1, RHS);
							if (RHS_decl_is_non_modifiable && (LHS || VLD) && RHS) {
								/* RHS will, for whatever reason, not be converted to a safe iterator. But presumably the LHS wiil 
								(or at least could) be. So we may need to ensure that LHS gets converted to a type that can handle 
								being assigned a raw pointer (iterator). */
								if (!lhs_indirection_state_ref.is_known_to_have_malloc_target()) {
									lhs_indirection_state_ref.set_is_known_to_have_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
									if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_ddecl_indirection);
									}
								}
								if (!lhs_indirection_state_ref.is_known_to_have_non_malloc_target()) {
									lhs_indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
									state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
									if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
										state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_ddecl_indirection);
									}
								}
								update_declaration_if_not_suppressed(lhs_ddecl_ref, Rewrite, *(MR.Context), state1);
							}
						}

						std::shared_ptr<CArray2ReplacementAction> cr_shptr;
						if (1 > adjusted_lhs_indirection_level) {
							cr_shptr = std::make_shared<CAssignmentSourceConstrainsTargetArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_indirection_level), lhs_ddecl_indirection);
						} else {
							/* Levels of indirection beyond the first one must be of the same type,
							* not just of "compatible" types. */
							cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*(rhs_res2.ddecl_cptr), rhs_indirection_level), lhs_ddecl_indirection);
						}

						(*cr_shptr).do_replacement(state1);
						state1.m_conversion_state_change_action_map.insert(cr_shptr);
						if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(rhs_indirection_level)) {
							if (!(*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_dynamic_array(rhs_indirection_level)) {
								state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							}
							if (!(*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_native_array(rhs_indirection_level)) {
								state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
							}
						} else {
							state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
							state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
						}
					}
				}
			} else if (true && (!LHS_decl_is_non_modifiable) && (!(rhs_res2.ddecl_cptr))) {
				auto rhs_STL = dyn_cast<const clang::StringLiteral>(RHS_ii);
				if (rhs_STL) {
					if (ConvertToSCPP && (lhs_res2.ddecl_conversion_state_ptr) && lhs_is_an_indirect_type && lhs_res2.ddecl_cptr && (LHS || VLD)) {
						auto& lhs_ddecl_ref = *(lhs_res2.ddecl_cptr);
						auto& lhs_indirection_state_ref = lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.at(lhs_res2.indirection_level);
						auto lhs_ddecl_indirection = CDDeclIndirection(lhs_ddecl_ref, lhs_res2.indirection_level);

						auto LHS_qtype = LHS ? LHS->getType() : VLD->getType();
						auto LHS_qtype_str = LHS_qtype.getAsString();
						bool is_initialization_of_native_array = (bool(VLD) && VLD->getType()->isArrayType());
						if (!(("void *" == LHS_qtype_str) || ("const void *" == LHS_qtype_str) || is_initialization_of_native_array)) {
							/* Here we're establishing and "enforcing" the constraint that the lhs value must
							* be of an (array) type that can be assigned the rhs string literal. */
							if (!lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
								lhs_indirection_state_ref.set_is_known_to_be_used_as_an_array_iterator(true);
								state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
								state1.m_array2_contingent_replacement_map.execute_matching_actions(state1, lhs_ddecl_indirection);
							}
							if (!lhs_indirection_state_ref.is_known_to_have_malloc_target()) {
								lhs_indirection_state_ref.set_is_known_to_have_malloc_target(true);
								state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
								if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
									state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_ddecl_indirection);
								}
							}
							if (!lhs_indirection_state_ref.is_known_to_have_non_malloc_target()) {
								lhs_indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
								state1.m_conversion_state_change_action_map.execute_matching_actions(state1, lhs_ddecl_indirection);
								if (lhs_indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
									state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, lhs_ddecl_indirection);
								}
							}
							update_declaration_if_not_suppressed(lhs_ddecl_ref, Rewrite, *(MR.Context), state1);
						}

					}
				}
			}

			IF_DEBUG(auto b1 = rhs_res2.ddecl_cptr ? llvm::isa<const clang::FunctionDecl>(rhs_res2.ddecl_cptr) : false;)
			IF_DEBUG(std::string rhs_qtype_str = RHS->getType().getAsString();)
			IF_DEBUG(std::string rhs_ddecl_qtype_str = rhs_res2.ddecl_cptr ? rhs_res2.ddecl_cptr->getType().getAsString() : std::string("");)

			if (lhs_res2.ddecl_cptr && rhs_res2.ddecl_cptr && lhs_res2.ddecl_conversion_state_ptr && rhs_res2.ddecl_conversion_state_ptr) {
				if (((0 == rhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size()) || (0 == rhs_res2.indirection_level))
					&& llvm::isa<const clang::FunctionDecl>(rhs_res2.ddecl_cptr)) {

					auto rhs_FND = llvm::cast<const clang::FunctionDecl>(rhs_res2.ddecl_cptr);

					/* Note the adjusted_lhs_indirection_level should already be set so that it corresponds to the indirection 
					level that is a function type (i.e. the pointee of the function pointer), not a function pointer type.  */
					auto adjusted_lhs_indirection_level = lhs_res2.indirection_level + lhs_indirection_level_adjustment;

					if ((CDDeclIndirection::no_indirection == lhs_res2.indirection_level) || (0 == lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size())) {
						int q = 5;
					} else if (adjusted_lhs_indirection_level >= lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size()) {
						assert(adjusted_lhs_indirection_level == lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.size());
						auto& lhs_direct_type_state_ref = lhs_res2.ddecl_conversion_state_ptr->direct_type_state_ref();
						auto lhs_pointee_maybe_qtype = lhs_direct_type_state_ref.current_qtype_if_any();
						if (lhs_pointee_maybe_qtype.has_value()) {
							auto lhs_pointee_qtype = lhs_pointee_maybe_qtype.value();
							if (lhs_pointee_qtype->isFunctionType()) {
								lhs_direct_type_state_ref.m_function_type_state.m_function_decl_ptr = rhs_FND;
								update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);

								for (auto param_PVD : rhs_FND->parameters()) {
									auto [PVD_ddcs_ref, PVD_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*param_PVD, &Rewrite);
									for (size_t j = 0; j < PVD_ddcs_ref.m_indirection_state_stack.size(); j += 1) {
										auto cr_shptr = std::make_shared<CUpdateDeclIndirectionArray2ReplacementAction>(Rewrite, MR,
											CDDeclIndirection(*param_PVD, j),
											CDDeclIndirection(*(lhs_res2.ddecl_conversion_state_ptr->m_ddecl_cptr), CDDeclIndirection::no_indirection));
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
										state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
										state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
									}
								}
							} else {
								int q = 5;
							}
						} else {
							int q = 3;
						}
					} else {
						auto& lhs_pointee_indirection_state_ref = lhs_res2.ddecl_conversion_state_ptr->m_indirection_state_stack.at(adjusted_lhs_indirection_level);
						if (lhs_pointee_indirection_state_ref.m_current_is_function_type) {
							if (lhs_pointee_indirection_state_ref.m_function_type_state.m_function_decl_ptr 
								&& (lhs_pointee_indirection_state_ref.m_function_type_state.m_function_decl_ptr != rhs_FND)) {
								
								/* It seems that the function pointer is being reassigned to a different value. We need to ensure 
								that (all the parameters and return value of) the old function and the new function are constrained 
								to be the same type. */;
								const auto old_FND = lhs_pointee_indirection_state_ref.m_function_type_state.m_function_decl_ptr;
								const auto old_num_params = old_FND->getNumParams();
								const auto rhs_num_params = rhs_FND->getNumParams();
								if (old_num_params == rhs_num_params) {
									for (size_t i = 0; rhs_num_params > i; i += 1) {
										const auto old_param_PVD = old_FND->getParamDecl(i);
										const auto param_PVD = rhs_FND->getParamDecl(i);

										auto [old_PVD_ddcs_ref, old_PVD_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*old_param_PVD, &Rewrite);
										auto [PVD_ddcs_ref, PVD_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*param_PVD, &Rewrite);
										if (PVD_ddcs_ref.m_indirection_state_stack.size() == old_PVD_ddcs_ref.m_indirection_state_stack.size()) {
											for (size_t j = 0; j < PVD_ddcs_ref.m_indirection_state_stack.size(); j += 1) {
												auto cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
													CDDeclIndirection(*param_PVD, j), CDDeclIndirection(*old_param_PVD, j));
												state1.m_conversion_state_change_action_map.insert(cr_shptr);
												state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
												state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);

												auto cr_shptr2 = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
													CDDeclIndirection(*old_param_PVD, j), CDDeclIndirection(*param_PVD, j));
												state1.m_conversion_state_change_action_map.insert(cr_shptr2);
												state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
												state1.m_native_array2_contingent_replacement_map.insert(cr_shptr2);
											}
										} else {
											int q = 3;
										}
									}

									auto [old_FND_ddcs_ref, old_FND_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*old_FND, &Rewrite);
									auto [rhs_FND_ddcs_ref, rhs_FND_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*rhs_FND, &Rewrite);
									if (rhs_FND_ddcs_ref.m_indirection_state_stack.size() == old_FND_ddcs_ref.m_indirection_state_stack.size()) {
										for (size_t j = 0; j < rhs_FND_ddcs_ref.m_indirection_state_stack.size(); j += 1) {
											auto cr_shptr = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*rhs_FND, j), CDDeclIndirection(*old_FND, j));
											state1.m_conversion_state_change_action_map.insert(cr_shptr);
											state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);

											auto cr_shptr2 = std::make_shared<CSameTypeArray2ReplacementAction>(Rewrite, MR,
												CDDeclIndirection(*old_FND, j), CDDeclIndirection(*rhs_FND, j));
											state1.m_conversion_state_change_action_map.insert(cr_shptr2);
											state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr2);
											state1.m_native_array2_contingent_replacement_map.insert(cr_shptr2);
										}
									} else {
										int q = 3;
									}

								} else {
									int q = 3;
								}

								int q = 5;
							}
							lhs_pointee_indirection_state_ref.m_function_type_state.m_function_decl_ptr = rhs_FND;
							update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);

							for (auto param_PVD : rhs_FND->parameters()) {
								auto [PVD_ddcs_ref, PVD_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*param_PVD, &Rewrite);
								for (size_t j = 0; j < PVD_ddcs_ref.m_indirection_state_stack.size(); j += 1) {
									auto cr_shptr = std::make_shared<CUpdateDeclIndirectionArray2ReplacementAction>(Rewrite, MR,
										CDDeclIndirection(*param_PVD, j),
										CDDeclIndirection(*(lhs_res2.ddecl_conversion_state_ptr->m_ddecl_cptr), adjusted_lhs_indirection_level));
									state1.m_conversion_state_change_action_map.insert(cr_shptr);
									state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
									state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
								}
							}
						} else {
							int q = 5;
						}
					}
				}
				if (true || (CDDeclIndirection::no_indirection == lhs_res2.indirection_level)) {
					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(lhs_res2.ddecl_cptr), &Rewrite);

					auto adjusted_lhs_indirection_level = lhs_res2.indirection_level + lhs_indirection_level_adjustment;
					auto rhs_indirection_level = rhs_res2.indirection_level;
					if (adjusted_lhs_indirection_level == rhs_indirection_level) {
						auto [rhs_ddcs_ref, rhs_update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*(rhs_res2.ddecl_cptr), &Rewrite);

						auto& lhs_function_state_ref = ddcs_ref.m_indirection_state_stack.m_direct_type_state.m_function_type_state;
						auto& rhs_function_state_ref = rhs_ddcs_ref.direct_type_state_ref().m_function_type_state;
						if (rhs_function_state_ref.has_been_changed()) {
							if (!lhs_function_state_ref.has_been_changed()) {
								lhs_function_state_ref = rhs_function_state_ref;
							} else {
								int q = 7;
							}
						} else if (lhs_function_state_ref.has_been_changed()) {
							rhs_function_state_ref = lhs_function_state_ref;
						}
					}
				}
			}

			if (lhs_res2.ddecl_cptr && lhs_res2.update_declaration_flag) {
				update_declaration_if_not_suppressed(*(lhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
			}
			if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
				update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
			}
		}

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
			const clang::ValueDecl* VLD = MR.Nodes.getNodeAs<clang::VarDecl>("mcsssassignment7");
			const clang::DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssassignment2");
			const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssassignment4");

			if ((RHS != nullptr) && ((LHS != nullptr) || (VLD != nullptr)))
			{
				auto SR = BO ? cm1_adj_nice_source_range(BO->getSourceRange(), m_state1, Rewrite) : 
							VLD ? cm1_adj_nice_source_range(VLD->getSourceRange(), m_state1, Rewrite) :
							cm1_adj_nice_source_range(LHS->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = BO ? m_state1.m_suppress_check_region_set.contains(BO, Rewrite, *(MR.Context)) :
							VLD ? m_state1.m_suppress_check_region_set.contains(VLD, Rewrite, *(MR.Context)) :
							m_state1.m_suppress_check_region_set.contains(LHS, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}
 
				s_handler1(MR, Rewrite, m_state1, LHS, RHS, VLD);

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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const CallExpr* CE)
		{
			if (CE != nullptr)
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				auto get_arg_EX_ii = [&](const size_t arg_index) {
						clang::Expr const* retval = nullptr;
						const auto num_args = CE->getNumArgs();
						if (num_args > arg_index) {
							auto arg_EX = CE->getArg(arg_index);
							auto arg_EX_qtype = arg_EX->getType();
							IF_DEBUG(std::string arg_EX_qtype_str = arg_EX_qtype.getAsString();)
							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_EX_ii = IgnoreParenImpNoopCasts(arg_EX, *(MR.Context));
							auto arg_EX_ii_qtype = arg_EX_ii->getType();
							IF_DEBUG(std::string arg_EX_ii_qtype_str = arg_EX_ii_qtype.getAsString();)
							assert(arg_EX_ii->getType().getTypePtrOrNull());
							retval = arg_EX_ii;
						}
						return retval;
					};

				//RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
				/* In this case we only filter out the element if the call expression and all of its arguments would 
				be individually filtered out. */
				if ((!SR.isValid()) || filtered_out_by_location<options_t<converter_mode_t> >(MR, SR.getBegin())) {
					bool an_arg_is_not_filtered_out = false;
					auto function_decl1 = CE->getDirectCallee();
					const auto num_args = CE->getNumArgs();
					if (function_decl1) {
						const std::string function_qname = function_decl1->getQualifiedNameAsString();
						const auto num_args = CE->getNumArgs();
						size_t arg_index = 0;
						for (; (num_args > arg_index); arg_index += 1) {
							auto arg_EX_ii = get_arg_EX_ii(arg_index);
							auto arg_EX_ii_SR = cm1_adj_nice_source_range(arg_EX_ii->getSourceRange(), state1, Rewrite);
							if (arg_EX_ii_SR.isValid()) {
								if (!filtered_out_by_location<options_t<converter_mode_t> >(MR, arg_EX_ii_SR.getBegin())) {
									an_arg_is_not_filtered_out = true;
									break;
								}
							}
						}
					}
					if (!an_arg_is_not_filtered_out) {
						return;
					}
				}

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl1 = CE->getDirectCallee();
				const auto num_args = CE->getNumArgs();

				if (!function_decl1) {
					/* The call expression did not report having an associated function declaration. This may be because 
					the call expression is the invocation of a function pointer. If so, we'll check to see if we've 
					associated a function declaration with the function pointer (as the result of some assignment expression 
					or whatever). If so, we'll use that function declaration. */
					auto ce_res2 = infer_array_type_info_from_stmt(*CE, "", state1);

					if (ce_res2.ddecl_cptr && ce_res2.ddecl_conversion_state_ptr) {
						auto& indirection_state_stack_ref = (*(ce_res2.ddecl_conversion_state_ptr)).m_indirection_state_stack;
						if (indirection_state_stack_ref.size() > ce_res2.indirection_level) {
							auto pointee_indirection_level = 1 + ce_res2.indirection_level;
							if (indirection_state_stack_ref.size() > pointee_indirection_level) {
								auto& indirection_state_ref = indirection_state_stack_ref.at(pointee_indirection_level);
								if (indirection_state_ref.m_function_type_state.m_function_decl_ptr) {
									function_decl1 = indirection_state_ref.m_function_type_state.m_function_decl_ptr;
								}
							} else {
								auto& direct_type_state_ref = indirection_state_stack_ref.m_direct_type_state;
								if (direct_type_state_ref.m_function_type_state.m_function_decl_ptr) {
									function_decl1 = direct_type_state_ref.m_function_type_state.m_function_decl_ptr;
								}
							}
						}
					}
				}

				if (function_decl1) {
					const std::string function_name = function_decl1->getNameAsString();
					const std::string function_qname = function_decl1->getQualifiedNameAsString();

					auto function_decl1_SR = cm1_adj_nice_source_range(function_decl1->getSourceRange(), state1, Rewrite);
					bool FD_is_non_modifiable = is_non_modifiable(*function_decl1, MR, Rewrite, state1, CE);
					bool function_is_variadic = function_decl1->isVariadic();

					do {
						auto res1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
						if (res1.m_seems_to_be_some_kind_of_malloc_or_realloc || res1.m_seems_to_be_some_kind_of_free) {
							/* malloc()/realloc() calls, including the passed arguments, are expected to be entirely replaced 
							in another part of the code, so presumably there's no point in trying to address them here. */
							return;
						}

						if (FD_is_non_modifiable) {
							bool begins_with__builtin_ = string_begins_with(function_name, "__builtin_");
							if (begins_with__builtin_) {
								return;
							}

							if ("fprintf" == function_name) {
								const auto arg_EX_ii = get_arg_EX_ii(1);
								if (arg_EX_ii) {
									auto SL = dyn_cast<const clang::StringLiteral>(arg_EX_ii);
									if (SL) {
										//return;
									}
								}
							}

#ifndef NDEBUG
							if ("_setjmp" == function_name) {
								int q = 5;
							}
#endif /*!NDEBUG*/
						}
					} while (false);

					/* We may end up establishing expression conversion states for the arguments. Here we're just making 
					the expression conversion state of the parent call expression is established first, which is 
					necessary to enable the child argument expression conversion states to automatically find and 
					establish a relationship with the parent. */
					auto& ce_ecs_ref = state1.get_expr_conversion_state_ref(*CE, Rewrite);

					if (FD_is_non_modifiable || function_is_variadic) {
						const std::string function_qname = function_decl1->getQualifiedNameAsString();
						const auto num_args = CE->getNumArgs();
						const auto num_params = function_decl1->getNumParams();
						size_t arg_index = 0;
						for (; (num_args > arg_index) && (num_params > arg_index); arg_index += 1) {
							auto param_VD = function_decl1->getParamDecl(arg_index);
							auto arg_EX = CE->getArg(arg_index);

							auto param_VD_qtype = param_VD->getType();
							auto arg_EX_qtype = arg_EX->getType();
							IF_DEBUG(std::string param_VD_qtype_str = param_VD_qtype.getAsString();)
							IF_DEBUG(std::string arg_EX_qtype_str = arg_EX_qtype.getAsString();)
							assert(arg_EX->getType().getTypePtrOrNull());

							//auto arg_EX_ii = IgnoreParenImpNoopCasts(arg_EX, *(MR.Context));
							auto arg_EX_ii = IgnoreParenImpCasts(arg_EX);
							auto arg_EX_ii_qtype = arg_EX_ii->getType();
							IF_DEBUG(std::string arg_EX_ii_qtype_str = arg_EX_ii_qtype.getAsString();)
							assert(arg_EX_ii->getType().getTypePtrOrNull());

							auto arg_EX_ii_SR = write_once_source_range(cm1_adj_nice_source_range(arg_EX_ii->getSourceRange(), state1, Rewrite));

							DEBUG_SOURCE_LOCATION_STR(arg_EX_ii_debug_source_location_str, arg_EX_ii_SR, Rewrite);
							DEBUG_SOURCE_TEXT_STR(arg_EX_ii_debug_source_text, arg_EX_ii_SR, Rewrite);
#ifndef NDEBUG
							if (std::string::npos != arg_EX_ii_debug_source_location_str.find(g_target_debug_source_location_str1)) {
								int q = 5;
							}
#endif /*!NDEBUG*/

							if (is_nullptr_literal(arg_EX_ii, *(MR.Context))) {
								continue;
							}
							auto SL = dyn_cast<const clang::StringLiteral>(arg_EX_ii);
							if (SL) {
								continue;
							}
							auto is_unmodifiable_raw_pointer_type = [&MR, &Rewrite, &state1](clang::Expr const& arg_EX_ii_cref, std::optional<clang::SourceRange> maybe_arg_EX_ii_SR = {}) {
									auto arg_EX_ii_qtype = arg_EX_ii_cref.getType();
									if (arg_EX_ii_qtype->isPointerType()) {
										auto arg_CE = dyn_cast<const clang::CallExpr>(&arg_EX_ii_cref);
										if (arg_CE) {
											auto arg_function_decl1 = arg_CE->getDirectCallee();
											auto arg_num_args = arg_CE->getNumArgs();
											if (arg_function_decl1) {
												IF_DEBUG(std::string debug_arg_function_name = arg_function_decl1->getNameAsString();)
												bool arg_FD_is_non_modifiable = is_non_modifiable(*arg_function_decl1, MR, Rewrite, state1, arg_CE);
												if (arg_FD_is_non_modifiable) {
													/* This argument corresponding to a pointer parameter that cannot be converted to a safe pointer, 
													seems to be the direct return value of a function that also cannot be converted to return a safe 
													pointer. */
													return true;
												}
											}
										}
										clang::SourceRange l_arg_EX_ii_SR;
										if (maybe_arg_EX_ii_SR.has_value()) {
											l_arg_EX_ii_SR = maybe_arg_EX_ii_SR.value();
										} else {
											l_arg_EX_ii_SR = cm1_adj_nice_source_range(arg_EX_ii_cref.getSourceRange(), state1, Rewrite);
										}
										if (filtered_out_by_location<options_t<converter_mode_t> >(Rewrite.getSourceMgr(), l_arg_EX_ii_SR.getBegin())) {
											return true;
										}
									}
									return false;
								};

							if (is_unmodifiable_raw_pointer_type(*arg_EX_ii, arg_EX_ii_SR)) {
								/* This argument corresponding to a pointer parameter that cannot be converted to a safe pointer, 
								seems to be the direct return value of a function that also cannot be converted to return a safe 
								pointer. So we'll just leave this passing of a raw pointer as is. */
								continue;
							}
							auto CO = dyn_cast<const clang::ConditionalOperator>(arg_EX_ii);
							if (CO) {
								auto lhs_ptr = CO->getLHS();
								auto rhs_ptr = CO->getRHS();
								if (lhs_ptr && rhs_ptr) {
									auto lhs_ii = IgnoreParenImpNoopCasts(lhs_ptr, *(MR.Context));
									auto rhs_ii = IgnoreParenImpNoopCasts(rhs_ptr, *(MR.Context));
									if (is_unmodifiable_raw_pointer_type(*lhs_ii) && is_unmodifiable_raw_pointer_type(*rhs_ii)) {
										continue;
									}
								}
							}

							if ((nullptr != param_VD) && arg_EX_ii_SR.isValid()
								&& param_VD->getType()->isPointerType() && (!param_VD->getType()->isFunctionPointerType())) {

								assert(nullptr != arg_EX_ii);
								bool is_pointer_to_pointer = false;

								auto param_VD_pointee_qtype = param_VD->getType()->getPointeeType();
								if (param_VD_pointee_qtype->isPointerType() || param_VD_pointee_qtype->isArrayType()) {
									/* The (unmodifiable) parameter seems to be a pointer to a pointer. We can't simply cast from a 
									(safe) smart pointer targeting another (safe) smart pointer to a raw pointer targeting another 
									raw pointer. We instead use a more specialized "cast" which will construct a temporary array of
									raw pointers. */

									if ((arg_EX_ii_qtype->isPointerType()) || (arg_EX_ii_qtype->isArrayType())) {
										auto pointee_type_ptr = arg_EX_ii_qtype->getPointeeOrArrayElementType();
										if (pointee_type_ptr) {
											auto pointee_qtype = clang::QualType(pointee_type_ptr , 0/*I'm just assuming zero specifies no qualifiers*/);
											IF_DEBUG(std::string pointee_qtype_str = pointee_qtype.getAsString();)
											if (pointee_type_ptr->isPointerType() || pointee_type_ptr->isArrayType()) {
												is_pointer_to_pointer = true;
											}
										} else { assert(false); }
									}
								}

								if (ConvertToSCPP) {
									std::string arg_function_qname_if_any;
									auto arg_CE = dyn_cast<const clang::CallExpr>(arg_EX_ii);
									if (arg_CE) {
										auto arg_function_decl1 = arg_CE->getDirectCallee();
										if (arg_function_decl1) {
											arg_function_qname_if_any = arg_function_decl1->getQualifiedNameAsString();
										}
									}

									auto& arg_EX_ii_ecs_ref = state1.get_expr_conversion_state_ref(*arg_EX_ii, Rewrite);

									std::shared_ptr<CExprTextModifier> shptr1;
									auto arg_EX_ii_qtype_str = arg_EX_ii_qtype.getAsString();
									if (true || (("void *" != arg_EX_ii_qtype_str) && ("const void *" != arg_EX_ii_qtype_str))) {

										if (is_pointer_to_pointer) {
											if ((!string_begins_with(function_qname, "mse::")) && (!string_begins_with(arg_function_qname_if_any, "mse::"))) {
												shptr1 = std::make_shared<CUnsafeMakeTemporaryArrayOfRawPointersFromExprTextModifier>();
												for  (auto& expr_text_modifier_shptr_ref : arg_EX_ii_ecs_ref.m_expr_text_modifier_stack) {
													if ("unsafe make temporary array of raw pointers from" == expr_text_modifier_shptr_ref->species_str()) {
														/* already applied */
														shptr1 = nullptr;
														break;
													}
												}
											} else {
												int q = 5;
											}
										} else {
											if ((!string_begins_with(function_qname, "mse::")) && (!string_begins_with(arg_function_qname_if_any, "mse::"))) {
												shptr1 = std::make_shared<CUnsafeMakeRawPointerFromExprTextModifier>();
												for  (auto& expr_text_modifier_shptr_ref : arg_EX_ii_ecs_ref.m_expr_text_modifier_stack) {
													if ("unsafe make raw pointer from" == expr_text_modifier_shptr_ref->species_str()) {
														/* already applied */
														shptr1 = nullptr;
														break;
													}
												}
											} else {
												int q = 5;
											}
										}
									} else {
										/* mse::us::lh::make_raw_pointer_from() now supoorts making a `void*` from an 
										mse::void_star_replacement, so this branch shouldn't be needed anymore. Right? */
										auto param_VD_qtype_str = param_VD_qtype.getAsString();
										if (("void *" != param_VD_qtype_str) && ("const void *" != param_VD_qtype_str)) {
											shptr1 = std::make_shared<CUnsafeCastExprTextModifier>(param_VD_qtype);
											for  (auto& expr_text_modifier_shptr_ref : arg_EX_ii_ecs_ref.m_expr_text_modifier_stack) {
												if ("unsafe cast" == expr_text_modifier_shptr_ref->species_str()) {
													/* already applied */
													shptr1 = nullptr;
													break;
												}
											}
										}
									}
									if (shptr1) {
										arg_EX_ii_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
										arg_EX_ii_ecs_ref.update_current_text();

										state1.add_pending_expression_update(*arg_EX_ii, Rewrite);
									}
								}
							} else {
								int q = 5;
							}
							MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, arg_EX/*RHS*/, param_VD/*VLD*/, MCSSSAssignment::EIsAnInitialization::Yes);
							int q = 5;
						}

						if (/*(FD_is_non_modifiable && string_ends_with(function_name, "printf")) || */
							function_is_variadic) {

							/* Apparently you can't pass non-trivial types to variadic functions (like printf())? The variadic 
							parameters still need to be processed. Basically, ensuring that any (safe) pointer arguments are 
							(unsafely) converted to raw pointers before being passed to the function. Presumably, pointers 
							passed to printf() would generally be "(const) char *"s. */
							for (; (num_args > arg_index); arg_index += 1) {
								auto arg_EX = CE->getArg(arg_index);

								auto arg_EX_qtype = arg_EX->getType();
								IF_DEBUG(std::string arg_EX_qtype_str = arg_EX_qtype.getAsString();)

								assert(arg_EX->getType().getTypePtrOrNull());
								auto arg_source_range = write_once_source_range(cm1_adj_nice_source_range(arg_EX->getSourceRange(), state1, Rewrite));
								std::string arg_source_text;
								if (arg_source_range.isValid()) {
									IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)
								}

								auto arg_EX_ii = IgnoreParenImpNoopCasts(arg_EX, *(MR.Context));

								auto arg_EX_ii_SR = write_once_source_range(cm1_adj_nice_source_range(arg_EX_ii->getSourceRange(), state1, Rewrite));

								DEBUG_SOURCE_LOCATION_STR(arg_EX_ii_debug_source_location_str, arg_EX_ii_SR, Rewrite);
								DEBUG_SOURCE_TEXT_STR(arg_EX_ii_debug_source_text, arg_EX_ii_SR, Rewrite);
#ifndef NDEBUG
								if (std::string::npos != arg_EX_ii_debug_source_location_str.find(g_target_debug_source_location_str1)) {
									int q = 5;
								}
#endif /*!NDEBUG*/

								std::string arg_function_qname_if_any;
								auto arg_CE = dyn_cast<const clang::CallExpr>(arg_EX_ii);
								if (arg_CE) {
									auto arg_function_decl1 = arg_CE->getDirectCallee();
									if (arg_function_decl1) {
										arg_function_qname_if_any = arg_function_decl1->getQualifiedNameAsString();
									}
								}

								auto DRE = given_or_descendant_DeclRefExpr(arg_EX_ii, *(MR.Context));

								if ((nullptr != DRE) && arg_source_range.isValid() && !(arg_EX->getType()->isFunctionPointerType())) {
									if (arg_EX->getType()->isPointerType()) {
										assert(nullptr != arg_EX);

										auto CO = dyn_cast<const clang::ConditionalOperator>(arg_EX_ii);
										if (CO) {
											MCSSSConditionalExpr::s_handler1(MR, Rewrite, state1, CO, {});
										}

										auto& arg_ecs_ref = state1.get_expr_conversion_state_ref(*arg_EX, Rewrite);

										if (ConvertToSCPP) {
											std::shared_ptr<CExprTextModifier> shptr1;
											auto arg_EX_qtype_str = arg_EX_qtype.getAsString();
											if (("void *" != arg_EX_qtype_str) && ("const void *" != arg_EX_qtype_str)) {
												if ((!string_begins_with(function_qname, "mse::")) && (!string_begins_with(arg_function_qname_if_any, "mse::"))) {
													shptr1 = std::make_shared<CUnsafeMakeRawPointerFromExprTextModifier>();
													for  (auto& expr_text_modifier_shptr_ref : arg_ecs_ref.m_expr_text_modifier_stack) {
														if ("unsafe make raw pointer from" == expr_text_modifier_shptr_ref->species_str()) {
															/* already applied */
															shptr1 = nullptr;
															break;
														}
													}
												} else {
													int q = 5;
												}
											}
											if (shptr1) {
												arg_ecs_ref.m_expr_text_modifier_stack.push_back(shptr1);
												arg_ecs_ref.update_current_text();

												state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, arg_source_range, state1, arg_EX);
												//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, arg_source_range, (*arg_shptr_ref).current_text());
												//(*this).Rewrite.ReplaceText(arg_source_range, (*arg_shptr_ref).current_text());
												//return;
											}
										}
									} else {
										auto DD = clang::dyn_cast<clang::DeclaratorDecl>(DRE->getDecl());
										if (DD) {
											/* It seems that (at least some implementations of) the *printf() functions don't
											like C++ objects with non-trivial copy constructors as (variadic) arguments, even
											if said objects are implicitly convertible to scalar integer types. Since we convert
											objects that are pointer targets (aka "addressable" ojects) to C++ objects with
											non-trivial copy constructors, we need to cast them back to their original scalar
											type before passing them to a *printf() function. */

											auto CSCE = clang::dyn_cast<clang::CStyleCastExpr>(IgnoreParenImpCasts(arg_EX));
											if (CSCE) {
												/* There seems to already be a cast operation. At the moment we seem to be leaving non-pointer 
												casts alone. So presumably adding another cast would be redundant. */;
											} else {
												auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

												auto arg_iter = state1.m_expr_conversion_state_map.find(arg_EX);
												if (state1.m_expr_conversion_state_map.end() == arg_iter) {
													std::shared_ptr<CExprConversionState> shptr1 = make_expr_conversion_state_shared_ptr<CExprConversionState>(*arg_EX, Rewrite, state1);
													arg_iter = state1.m_expr_conversion_state_map.insert(shptr1);
												}
												auto& arg_shptr_ref = (*arg_iter).second;

												auto arg_source_text = Rewrite.getRewrittenText(arg_source_range);

												std::string arg_EX_qtype_str = arg_EX_qtype.getAsString();
												std::string prefix;
												std::string suffix = ")";
												if ("Dual" == ConvertMode) {
													prefix = "MSE_LH_CAST(" + arg_EX_qtype_str + ", ";
												} else {
													auto arg_EX_qtype_str_wwsr = with_whitespace_removed(arg_EX_qtype_str);
													bool has_space = (arg_EX_qtype_str_wwsr != arg_EX_qtype_str);
													if (has_space) {
														prefix = "(";
													}
													prefix += arg_EX_qtype_str;
													if (has_space) {
														prefix += ")";
													}
													prefix += "(";
												}

												auto shptr1 = std::make_shared<CWrapExprTextModifier>(prefix, suffix);
												if (1 <= (*arg_shptr_ref).m_expr_text_modifier_stack.size()) {
													if ("wrap" == (*arg_shptr_ref).m_expr_text_modifier_stack.back()->species_str()) {
														/* already applied */
														return;
													}
												}
												(*arg_shptr_ref).m_expr_text_modifier_stack.push_back(shptr1);
												(*arg_shptr_ref).update_current_text();

												if (ConvertToSCPP) {
													std::shared_ptr<CDDeclIndirectionReplacementAction> cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, CDDeclIndirection::no_indirection), arg_EX, (*arg_shptr_ref).current_text());

													if (ddcs_ref.has_been_determined_to_be_a_pointer_target()) {
														(*cr_shptr).do_replacement(state1);
													} else {
														state1.m_pointer_target_contingent_replacement_map.insert(cr_shptr);
													}
												}
											}

											int q = 5;
										} else {
											int q = 3;
										}
									}
								}
								int q = 5;
							}
						}

						return;
					}

					std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
					for (auto param_VD : function_decl1->parameters()) {
						param_decls_of_first_function_decl.push_back(param_VD);
					}

					/* The constraint(s) need to be applied to all (re)declarations of the function. */
					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						if (function_decl == function_decl1) {
							//continue;
						}
						auto fdecl_source_range = cm1_adj_nice_source_range(function_decl->getSourceRange(), state1, Rewrite);
						auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(*MR.SourceManager);

						for (size_t arg_index = 0; (CE->getNumArgs() > arg_index) && (function_decl->getNumParams() > arg_index); arg_index += 1) {
							auto param_VD = function_decl->getParamDecl(arg_index);
							auto arg_EX = CE->getArg(arg_index);

							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_source_range = write_once_source_range(cm1_adj_nice_source_range(arg_EX->getSourceRange(), state1, Rewrite));
							std::string arg_source_text;
							if (arg_source_range.isValid()) {
								IF_DEBUG(arg_source_text = Rewrite.getRewrittenText(arg_source_range);)
							}

							if ((nullptr != param_VD) && (nullptr != arg_EX) && arg_source_range.isValid()) {
								bool arg_is_an_indirect_type = is_an_indirect_type(arg_EX->getType());
								if (arg_is_an_indirect_type) {
									static const std::array strtox_function_names = { "strtol", "strtoul", "strtoll", "strtoull", "strtoimax", "strtoumax", "strtof", "strtod", "strtold" };
									static const std::string strtox_common_prefix = "strto";
									if (string_begins_with(function_qname, strtox_common_prefix)) {
										bool is_strtox = false;
										for (auto const& strtox_function_name : strtox_function_names) {
											if (strtox_function_name == function_qname) {
												is_strtox = true;
												break;
											}
										}
										if (is_strtox) {
											auto argii_EX = arg_EX->IgnoreParenImpCasts();
											auto CSCE = dyn_cast<const clang::CStyleCastExpr>(argii_EX);
											if (CSCE) {
												auto csce_QT = definition_qtype(CSCE->getType());
												std::string csce_QT_str = csce_QT.getAsString();
												MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(csce_QT);
												auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
												assert(precasted_expr_ptr);
												auto precasted_expr_QT = precasted_expr_ptr->getType();
												IF_DEBUG(std::string precasted_expr_QT_str = precasted_expr_QT.getAsString();)
												auto precasted_expr_SR = cm1_adj_nice_source_range(precasted_expr_ptr->getSourceRange(), state1, Rewrite);
												auto CSCESR = write_once_source_range(cm1_adj_nice_source_range(CSCE->getSourceRange(), state1, Rewrite));
												auto cast_operation_SR = cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite);
												auto SR = CSCESR;
												bool precasted_expr_is_function_type = precasted_expr_QT->isFunctionType();

												RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;
												DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, CSCESR, Rewrite);
												RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;
												DEBUG_SOURCE_TEXT_STR(debug_source_text1, CSCESR, Rewrite);

									#ifndef NDEBUG
												if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
													int q = 5;
												}
									#endif /*!NDEBUG*/

												if ("char **" == csce_QT_str) {
													if (ConvertToSCPP) {
														std::string new_cast_prefix;
														if ("Dual" == ConvertMode) {
															new_cast_prefix = "MSE_LH_CHAR_STAR_STAR_CAST_FOR_STRTOX(";
														} else if ("FasterAndStricter" == ConvertMode) {
															new_cast_prefix = "(";
														} else {
															new_cast_prefix = "(";
														}
														std::string new_cast_suffix = ")";
													
														auto& ecs_ref = state1.get_expr_conversion_state_ref<CCastExprConversionState>(*CSCE, Rewrite, *precasted_expr_ptr, new_cast_prefix, new_cast_suffix);
														ecs_ref.update_current_text();

														if (CSCESR.isValid()) {
															state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCESR, state1, CSCE);
														}
														return;
													}
												}
											}
										}
									}
								}
								MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, arg_EX/*RHS*/, param_VD/*VLD*/, MCSSSAssignment::EIsAnInitialization::Yes);
							} else {
								int q = 5;
							}
							int q = 5;
						}
					}
				} else {
					auto D = CE->getCalleeDecl();
					auto DD = D ? dyn_cast<const DeclaratorDecl>(D) : nullptr;
					auto EX = CE->getCallee();

					if (DD && EX) {
						auto DDSR = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
						std::string ddecl_source_text;
						if (DDSR.isValid()) {
							IF_DEBUG(ddecl_source_text = Rewrite.getRewrittenText(DDSR);)
						} else {
							return;
						}

						auto EXSR = write_once_source_range(cm1_adj_nice_source_range(EX->getSourceRange(), state1, Rewrite));
						std::string expr_source_text;
						if (EXSR.isValid()) {
							IF_DEBUG(expr_source_text = Rewrite.getRewrittenText(EXSR);)
						} else {
							return;
						}

						if (ConvertToSCPP) {
							auto args = CE->arguments();
							for (auto arg : args) {
								auto rhs_res2 = infer_array_type_info_from_stmt(*arg, "", state1);
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

										if ((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(rhs_res2.indirection_level + i)) {
											(*cr_shptr).do_replacement(state1);
											if (!((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_dynamic_array(rhs_res2.indirection_level + i))) {
												state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											}
											if (!((*(rhs_res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_a_native_array(rhs_res2.indirection_level + i))) {
												state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
											}
										} else {
											state1.m_dynamic_array2_contingent_replacement_map.insert(cr_shptr);
											state1.m_native_array2_contingent_replacement_map.insert(cr_shptr);
										}
										state1.m_conversion_state_change_action_map.insert(cr_shptr);
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssparameterpassing1");
			//const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssparameterpassing2");
			//const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssparameterpassing3");
			//const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssparameterpassing4");

			if (CE != nullptr)
			{
				s_handler1(MR, Rewrite, m_state1, CE);
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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const CallExpr* CE)
		{
			if (CE != nullptr)
			{
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl1 = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl1) {
					const std::string function_name = function_decl1->getNameAsString();

					/* We may end up establishing expression conversion states for the arguments. Here we're just making 
					the expression conversion state of the parent call expression is established first, which is 
					necessary to enable the child argument expression conversion states to automatically find and 
					establish a relationship with the parent. */
					auto& ce_ecs_ref = state1.get_expr_conversion_state_ref(*CE, Rewrite);

					std::vector<const clang::ParmVarDecl*> param_decls_of_first_function_decl;
					for (auto param_VD : function_decl1->parameters()) {
						param_decls_of_first_function_decl.push_back(param_VD);
					}

					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						if (function_decl == function_decl1) {
							//continue;
						}
						auto fdecl_source_range = cm1_adj_nice_source_range(function_decl->getSourceRange(), state1, Rewrite);
						auto fdecl_source_location_str = fdecl_source_range.getBegin().printToString(*MR.SourceManager);
#ifndef NDEBUG
						if (std::string::npos != fdecl_source_location_str.find("lodepng.cpp")) {
							int q = 5;
						} else if (std::string::npos != fdecl_source_location_str.find("lodepng_util.cpp")) {
							int q = 5;
						}
#endif /*!NDEBUG*/

						for (size_t arg_index = 0; (CE->getNumArgs() > arg_index) && (function_decl->getNumParams() > arg_index); arg_index += 1) {
							auto param_VD = function_decl->getParamDecl(arg_index);
							auto arg_EX = CE->getArg(arg_index);

							assert(arg_EX->getType().getTypePtrOrNull());
							auto arg_source_range = write_once_source_range(cm1_adj_nice_source_range(arg_EX->getSourceRange(), state1, Rewrite));
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

														auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*param_VD, &Rewrite);

														if (ddcs_ref.has_been_determined_to_be_a_pointer_target()) {
															(*cr_shptr).do_replacement(state1);
														} else {
															state1.m_pointer_target_contingent_replacement_map.insert(cr_shptr);
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

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const CallExpr* CE = MR.Nodes.getNodeAs<clang::CallExpr>("mcsssparameterpassing1");
			//const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssparameterpassing2");
			//const MemberExpr* ME = MR.Nodes.getNodeAs<clang::MemberExpr>("mcsssparameterpassing3");
			//const clang::CStyleCastExpr* CCE = MR.Nodes.getNodeAs<clang::CStyleCastExpr>("mcsssparameterpassing4");

			if (CE != nullptr)
			{
				s_handler1(MR, Rewrite, m_state1, CE);
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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1,
			const CallExpr* CE, const DeclaratorDecl* DD) {

			if (/*(DS != nullptr) && */(CE != nullptr) && (DD != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */
					std::string num_elements_text/* = before_str + after_str*/;

					if (nullptr != DD) {
						auto decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
						if (!decl_source_range.isValid()) {
							return;
						}
						DEBUG_SOURCE_LOCATION_STR(decl_debug_source_location_str, decl_source_range, Rewrite);
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

						auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

						ddcs_ref.m_indirection_state_stack.at(0).set_is_known_to_have_malloc_target(true);
						bool lhs_has_been_determined_to_point_to_an_array = ddcs_ref.m_indirection_state_stack.at(0).is_known_to_be_used_as_an_array_iterator();

						const clang::Type* TP = QT.getTypePtr();
						auto lhs_type_str = QT.getAsString();

						std::string element_type_str;
						std::string adjusted_num_bytes_str = alloc_function_info1.m_num_bytes_arg_source_text;
						if (llvm::isa<const clang::ArrayType>(TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								element_type_str = type_str;
							}
							adjusted_num_bytes_str = "(" + alloc_function_info1.m_num_bytes_arg_source_text + ")";
							if ((element_type.getAsString() != element_type_str) && ("void" != element_type_str)) {
								adjusted_num_bytes_str += " / sizeof(" + element_type.getAsString() + ") * sizeof(" + element_type_str + ")";
							}
						} else if (TP->isPointerType()) {
							auto target_type = TP->getPointeeType();
							auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								element_type_str = type_str;
							}
							adjusted_num_bytes_str = "(" + alloc_function_info1.m_num_bytes_arg_source_text + ")";
							if ((target_type.getAsString() != element_type_str) && ("void" != element_type_str)) {
								adjusted_num_bytes_str += " / sizeof(" + target_type.getAsString() + ") * sizeof(" + element_type_str + ")";
							}
						}
						if ("" != element_type_str) {
							bool is_char_star = (("char" == element_type_str) || ("const char" == element_type_str));

							std::string array_initializer_info_str;
							std::string pointer_initializer_info_str;

							if (alloc_function_info1.m_seems_to_be_some_kind_of_realloc) {
								array_initializer_info_str = "MSE_LH_REALLOC(" + element_type_str + ", " + alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text;
								array_initializer_info_str += ", " + adjusted_num_bytes_str + ")";

								if ("Dual" == ConvertMode) {
									array_initializer_info_str = "MSE_LH_REALLOC(" + element_type_str + ", " + alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text;
									array_initializer_info_str += ", " + adjusted_num_bytes_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									array_initializer_info_str = "mse::lh::reallocate(" + alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text;
									array_initializer_info_str += ", " + adjusted_num_bytes_str + ")";
								} else {
									array_initializer_info_str = "mse::lh::reallocate(" + alloc_function_info1.m_realloc_or_free_pointer_arg_adjusted_source_text;
									array_initializer_info_str += ", " + adjusted_num_bytes_str + ")";
								}

								/* We're going to assume here that any "realloc()"ed memory is a (dynamic)
								array. */
								pointer_initializer_info_str = array_initializer_info_str;

								if (alloc_function_info1.m_realloc_or_free_pointer_arg_DD) {
									auto VLD = dyn_cast<const clang::ValueDecl>(DD);
									auto first_arg_ii = IgnoreParenImpNoopCasts(CE->getArg(0), *(MR.Context));
									IF_DEBUG(std::string first_arg_ii_qtype_str = first_arg_ii->getType().getAsString();)
									auto first_arg_SR = first_arg_ii->getSourceRange();
									DEBUG_SOURCE_LOCATION_STR(first_arg_ii_debug_source_location_str, first_arg_SR, Rewrite);
									DEBUG_SOURCE_TEXT_STR(first_arg_ii_debug_source_text, first_arg_SR, Rewrite);
									if (VLD) {
										MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, first_arg_ii/*RHS*/, VLD/*VLD*/, MCSSSAssignment::EIsAnInitialization::Yes);
									}
								}
							} else {
								if ("Dual" == ConvertMode) {
									array_initializer_info_str = "MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_DYNAMIC_ARRAY_ITERATOR_TYPE(" + element_type_str + ")";
									//array_initializer_info_str = "MSE_LH_ALLOC_DYN_ARRAY1(MSE_LH_ARRAY_ITERATOR_TYPE(" + element_type_str + ")";
									array_initializer_info_str += ", " + adjusted_num_bytes_str + ")";

									pointer_initializer_info_str = "MSE_LH_ALLOC_POINTER1(" + element_type_str + ")";
								} else if ("FasterAndStricter" == ConvertMode) {
									array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<" + element_type_str + "> >(";
									//array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TLHNullableAnyRandomAccessIterator<" + element_type_str + "> >(";
									array_initializer_info_str += adjusted_num_bytes_str + ")";

									pointer_initializer_info_str = "mse::TRefCountingPointer<" + element_type_str + ">()";
								} else {
									array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TStrongVectorIterator<" + element_type_str + "> >(";
									//array_initializer_info_str = "mse::lh::allocate_dyn_array1<mse::lh::TLHNullableAnyRandomAccessIterator<" + element_type_str + "> >(";
									array_initializer_info_str += adjusted_num_bytes_str + ")";

									pointer_initializer_info_str = "mse::lh::allocate<mse::lh::TLHNullableAnyPointer<" + element_type_str + "> >()";
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
								auto cr_shptr = std::make_shared<CInitializerArray2ReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), array_initializer_info_str);

								if (lhs_has_been_determined_to_point_to_an_array || is_char_star) {
									(*cr_shptr).do_replacement(state1);
								} else {
									/* lhs has not (yet) been determined to be an array iterator. Here we apply the
									"replacement action" for non-array iterators. */
									CInitializerArray2ReplacementAction(Rewrite, MR, CDDeclIndirection(*DD, 0/*indirection_level*/), pointer_initializer_info_str).do_replacement(state1);

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

			if (/*(DS != nullptr) && */(CE != nullptr) && (DD != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(DD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(DD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto alloc_function_info1 = analyze_malloc_resemblance(*CE, m_state1, Rewrite);
				if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
					/* The argument is in the form "something * sizeof(something_else)" or
					* "sizeof(something) * something_else". So we're just going to assume that
					* this is an instance of an array being allocated. */

					if (ConvertToSCPP && SR.isValid()) {
						auto lambda = [MR, *this, CE, DD](){ s_handler1(MR, (*this).Rewrite, (*this).m_state1, CE, DD); };
						/* This modification needs to be queued so that it will be executed after any other
						modifications that might affect the relevant part of the source text. */
						(*this).m_state1.m_pending_code_modification_actions.add_replacement_action(SR, lambda);

						/* We've queued the modification action for deferred execution, but we don't want to delay the
						establishment of the expression conversion state because, among other reasons, it reads from 
						and stores the original source text and we want that done before the source text gets 
						potentially modified. */
						auto& ecs_ref = (*this).m_state1.get_expr_conversion_state_ref(*CE, Rewrite);
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

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1
			, const clang::FunctionDecl* FND, const clang::ReturnStmt* RS, const DeclRefExpr* DRE)
		{
			if ((FND != nullptr) && (DRE != nullptr) && (RS != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(FND->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(FND, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

#ifndef NDEBUG
				auto FND2 = Tget_containing_element_of_type<clang::FunctionDecl>(RS, *(MR.Context));
				if (FND2 != FND) {
					auto fname = FND->getNameAsString();
					if (FND2) {
						auto fname2 = FND2->getNameAsString();
						int q = 3;
					} else {
						int q = 3;
					}
				}
#endif /*!NDEBUG*/

				auto function_decl1 = FND;
				auto num_params = FND->getNumParams();
				if (function_decl1) {
					const std::string function_name = function_decl1->getNameAsString();
					const auto lc_function_name = tolowerstr(function_name);

					static const std::string free_str = "free";
					bool ends_with_free = ((lc_function_name.size() >= free_str.size())
							&& (0 == lc_function_name.compare(lc_function_name.size() - free_str.size(), free_str.size(), free_str)));

					auto alloc_info = analyze_malloc_resemblance(*function_decl1, state1, Rewrite);

					bool begins_with__builtin_ = string_begins_with(function_name, "__builtin_");

					if (ends_with_free || alloc_info.m_seems_to_be_some_kind_of_malloc_or_realloc || begins_with__builtin_) {
						return void();
					}

					auto retval_EX = RS->getRetValue();

					bool rhs_is_an_indirect_type = is_an_indirect_type(retval_EX->getType());

					auto retvalii_EX = RS->getRetValue()->IgnoreImplicit();
					if (rhs_is_an_indirect_type && (retvalii_EX->getStmtClass() == clang::Stmt::StmtClass::CStyleCastExprClass)) {
						auto CSCE = llvm::cast<const clang::CStyleCastExpr>(retvalii_EX);
						if (CSCE) {
							handle_c_style_cast_without_context(MR, Rewrite, state1, CSCE);
							auto cast_operation_SR = cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite);
						} else { assert(false); }
					}

					int lhs_indirection_level_adjustment = 0;
					auto rhs_res3 = leading_addressof_operator_info_from_stmt(*retval_EX);
					if (rhs_res3.without_leading_addressof_operator_expr_cptr) {
						assert(rhs_res3.leading_addressof_operator_detected && rhs_res3.addressof_unary_operator_cptr);

						retval_EX = rhs_res3.without_leading_addressof_operator_expr_cptr;
						lhs_indirection_level_adjustment += 1;
					}

					auto rhs_res2 = infer_array_type_info_from_stmt(*(retval_EX), "", state1);
					if (rhs_res2.ddecl_cptr && rhs_res2.update_declaration_flag) {
						update_declaration_if_not_suppressed(*(rhs_res2.ddecl_cptr), Rewrite, *(MR.Context), state1);
					}

					auto function_decls_range = function_decl1->redecls();
					for (const auto& function_decl : function_decls_range) {
						if (function_decl != function_decl1) {
							int q = 5;
						}

						MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr, RS->getRetValue(), function_decl, MCSSSAssignment::EIsAnInitialization::Yes);
					}
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::FunctionDecl* FND = MR.Nodes.getNodeAs<clang::FunctionDecl>("mcsssreturnvalue1");
			const clang::ReturnStmt* RS = MR.Nodes.getNodeAs<clang::ReturnStmt>("mcsssreturnvalue2");
			const DeclRefExpr* DRE = MR.Nodes.getNodeAs<clang::DeclRefExpr>("mcsssreturnvalue3");

			if ((FND != nullptr) && (DRE != nullptr) && (RS != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(FND->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(FND, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				if (ConvertToSCPP && SR.isValid()) {
					s_handler1(MR, (*this).Rewrite, (*this).m_state1, FND, RS, DRE);
				} else {
					int q = 7;
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
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					const std::string function_name = function_decl->getNameAsString();
					static const std::string fread_str = "fread";
					if (fread_str == function_name) {
						auto arg0_SR1 = CE->getArg(0)->getSourceRange();
						auto first_arg_to_end_SR = clang::SourceRange{ arg0_SR1.getBegin(), CE->getEndLoc() };
						auto first_arg_to_end_text = Rewrite.getRewrittenText(first_arg_to_end_SR);

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
							decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
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
								update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
							}
						}

						clang::QualType arg1_QT = CE->getArg(0)->IgnoreParenCasts()->getType();
						if (nullptr != DD) {
							arg1_QT = QT;
						}
						const clang::Type* arg1_TP = arg1_QT.getTypePtr();
						auto arg1_type_str = arg1_QT.getAsString();

						std::string arg1_element_type_str;
						if (llvm::isa<const clang::ArrayType>(arg1_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								arg1_element_type_str = type_str;
							}
						} else if (arg1_TP->isPointerType()) {
							auto target_type = arg1_TP->getPointeeType();
							auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								arg1_element_type_str = type_str;
							}
						}
						std::string ce_replacement_code;
						if (("" != arg1_element_type_str) && ("void" != arg1_element_type_str) && ("const void" != arg1_element_type_str)) {
							if ("Dual" == ConvertMode) {
								ce_replacement_code = "MSE_LH_FREAD(" + first_arg_to_end_text;
							} else if ("FasterAndStricter" == ConvertMode) {
								ce_replacement_code = "mse::lh::fread(" + first_arg_to_end_text;
							} else {
								ce_replacement_code = "mse::lh::fread(" + first_arg_to_end_text;
							}
						}

						if (ConvertToSCPP && (SR.isValid()) && ("" != ce_replacement_code)) {
							auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
							if ((nullptr != res2.ddecl_conversion_state_ptr)) {
								if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
									(*cr_shptr).do_replacement(state1);
								}
								state1.m_conversion_state_change_action_map.insert(cr_shptr);
							} else {
								(*cr_shptr).do_replacement(state1);
							}
						} else {
							int q = 7;
						}

						int q = 5;
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
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					const std::string function_name = function_decl->getNameAsString();
					static const std::string fread_str = "fread";
					if (fread_str == function_name) {
						if (ConvertToSCPP && SR.isValid()) {
							/* Obtaining a reference to the expression conversion state has the side effect of storing the 
							original text of the expression and its immediate child expressions. */
							auto& ecs_ref = (*this).m_state1.get_expr_conversion_state_ref(*CE, Rewrite);

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
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					const std::string function_name = function_decl->getNameAsString();
					static const std::string fwrite_str = "fwrite";
					if (fwrite_str == function_name) {
						auto arg0_SR1 = CE->getArg(0)->getSourceRange();
						auto first_arg_to_end_SR = clang::SourceRange{ arg0_SR1.getBegin(), CE->getEndLoc() };
						auto first_arg_to_end_text = Rewrite.getRewrittenText(first_arg_to_end_SR);

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
							decl_source_range = cm1_adj_nice_source_range(DD->getSourceRange(), state1, Rewrite);
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
								update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
							}
						}

						clang::QualType arg1_QT = CE->getArg(0)->IgnoreParenCasts()->getType();
						if (nullptr != DD) {
							arg1_QT = QT;
						}
						const clang::Type* arg1_TP = arg1_QT.getTypePtr();
						auto arg1_type_str = arg1_QT.getAsString();

						std::string arg1_element_type_str;
						if (llvm::isa<const clang::ArrayType>(arg1_TP)) {
							auto ATP = llvm::cast<const clang::ArrayType>(arg1_TP);
							assert(nullptr != ATP);
							auto element_type = ATP->getElementType();
							auto type_str = generate_qtype_replacement_code(element_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								arg1_element_type_str = type_str;
							}
						} else if (arg1_TP->isPointerType()) {
							auto target_type = arg1_TP->getPointeeType();
							auto type_str = generate_qtype_replacement_code(target_type, Rewrite, &state1);
							if (true || (("char" != type_str) && ("const char" != type_str))) {
								arg1_element_type_str = type_str;
							}
						}
						std::string ce_replacement_code;
						if (("" != arg1_element_type_str) && ("void" != arg1_element_type_str) && ("const void" != arg1_element_type_str)) {
							if ("Dual" == ConvertMode) {
								ce_replacement_code = "MSE_LH_FWRITE(" + first_arg_to_end_text;
							} else if ("FasterAndStricter" == ConvertMode) {
								ce_replacement_code = "mse::lh::fwrite(" + first_arg_to_end_text;
							} else {
								ce_replacement_code = "mse::lh::fwrite(" + first_arg_to_end_text;
							}
						}

						if (ConvertToSCPP && (SR.isValid()) && ("" != ce_replacement_code)) {
							auto cr_shptr = std::make_shared<CExprTextDDIReplacementAction>(Rewrite, MR, CDDeclIndirection(*DD, res2.indirection_level), CE, ce_replacement_code);
							if ((nullptr != res2.ddecl_conversion_state_ptr)) {
								if (true || (*(res2.ddecl_conversion_state_ptr)).has_been_determined_to_point_to_an_array(res2.indirection_level)) {
									(*cr_shptr).do_replacement(state1);
								}
								state1.m_conversion_state_change_action_map.insert(cr_shptr);
							} else {
								(*cr_shptr).do_replacement(state1);
							}
						} else {
							int q = 7;
						}

						int q = 5;
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
				auto SR = cm1_adj_nice_source_range(CE->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(CE, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto function_decl = CE->getDirectCallee();
				auto num_args = CE->getNumArgs();
				if (function_decl && (4 == num_args)) {
					const std::string function_name = function_decl->getNameAsString();
					static const std::string fwrite_str = "fwrite";
					if (fwrite_str == function_name) {
						if (ConvertToSCPP && SR.isValid()) {
							/* Obtaining a reference to the expression conversion state has the side effect of storing the 
							original text of the expression and its immediate child expressions. */
							auto& ecs_ref = (*this).m_state1.get_expr_conversion_state_ref(*CE, Rewrite);

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

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	class MCSSSDeclUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSDeclUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const clang::Decl* D)
		{
			if ((D != nullptr))
			{
				auto SR = cm1_adj_nice_source_range(D->getSourceRange(), state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(D, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto DD = dyn_cast<const DeclaratorDecl>(D);
				if (DD) {
					const auto qtype = DD->getType();
					const std::string qtype_str = DD->getType().getAsString();

					if (is_non_modifiable(*DD, *(MR.Context), Rewrite, state1)) {
						int q = 5;
						return;
					}

					auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*DD, &Rewrite);

					clang::Expr const* init_EX = nullptr;
					auto VD = dyn_cast<const clang::VarDecl>(D);
					auto FD = dyn_cast<const clang::FieldDecl>(D);
					auto FND = dyn_cast<const clang::FunctionDecl>(DD);
					if (VD) {
						const auto storage_duration = VD->getStorageDuration();
						const auto var_qualified_name = VD->getQualifiedNameAsString();
						const auto* CXXRD = qtype.getTypePtr()->getAsCXXRecordDecl();

						if ((clang::StorageDuration::SD_Static == storage_duration) || (clang::StorageDuration::SD_Thread == storage_duration)) {
							bool satisfies_checks = satisfies_restrictions_for_static_storage_duration(qtype);
							if (!satisfies_checks) {
								if (clang::StorageDuration::SD_Static == storage_duration) {
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

									if (!ddcs_ref.m_has_been_replaced_as_a_whole) {
										/* Here we're (unjustifiably) assuming that the program is single threaded 
										and changing variables with static duration to thread_local duration. */
										std::string l_source_text1 = Rewrite.getRewrittenText(SR);
										std::size_t replace_pos = 0;
										std::size_t replace_length = 0;
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

										if (1 <= replace_length) {
											ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = clang::SourceRange(SR.getBegin().getLocWithOffset(replace_pos), SR.getBegin().getLocWithOffset(replace_pos + replace_length - 1));
										} else {
											ddcs_ref.m_thread_local_specifier_SR_or_insert_before_point = SR.getBegin().getLocWithOffset(replace_pos);
										}

										int q = 5;
									}
								} else {
									assert(clang::StorageDuration::SD_Thread == storage_duration);
									satisfies_checks |= satisfies_restrictions_for_thread_local_storage_duration(qtype);
									if (!satisfies_checks) {
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
							init_EX = VD->getInit();
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

											/* We're noting that originally there was no initializer. */
											ddcs_ref.m_original_initialization_expr_str = "";
											ddcs_ref.m_original_initialization_has_been_noted = true;

											std::string initializer_info_str = default_init_value_str(qtype);
											ddcs_ref.m_fallback_current_initialization_expr_str = initializer_info_str;

											if (!(ddcs_ref.m_has_been_replaced_as_a_whole)) {
												/* Specify that the new initialization string should be
												inserted at the end of the declaration. */
												//ddcs_ref.m_maybe_embedded_initializer_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
												ddcs_ref.m_initializer_SR_or_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
											}

											update_declaration_if_not_suppressed(*l_DD, Rewrite, *(MR.Context), state1);
										}
									} else {
										/* todo: emit error that (uninitialized) 'extern' variables
										aren't supported?  */;
									}
								}
							}
						}

						init_EX = VD->getInit();
						if (init_EX) {

							auto individual_declarator_decls = IndividualDeclaratorDecls(DD);
							if (1 > individual_declarator_decls.size()) {
								assert(false);
							} else if (individual_declarator_decls.front() != DD) {
								assert(2 <= individual_declarator_decls.size());
								/* We've observed that for some reason with declarations statements that contain the declaration 
								of multiple items with initialization expressions, the "assignment" matcher will only match the 
								initialization of the first item. But this declaration seems to be one of those subsequent items 
								in a multiplee declaration statement. So here we'll maually call the assignment handler. */
								MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, init_EX/*RHS*/, VD, MCSSSAssignment::EIsAnInitialization::Yes);
							}

							if (qtype->isArrayType()) {
								const clang::ArrayType* ATP = qtype->getAsArrayTypeUnsafe();
								if (ATP) {
									clang::QualType QT = ATP->getElementType();
									IF_DEBUG(auto l_type_str = QT.getAsString();)

									/* This is the declaration of a native array with an initializer expression (which is presumably 
									likely to be an (aggregate) initializer list). While the initialization as a whole is present in 
									the AST, the initialization of each individual element does not seem to be present in the AST. But 
									we still need to ensure that any (pointer) fields of the element be converted to a type that 
									supports the being assigned the corresponding initalization value type. Rather than dissecting 
									the initialization expression to determine the types of each field of the initalization value(s), 
									we're just going to indicate that all the (pointer) fields of the assignee should be converted to 
									a type that supports both malloc()ed and non-malloc()ed targets. */

									auto RD = QT->getAsRecordDecl();
									if (RD) {

										/* This lambda function will, when passed a clang::FieldDecl of a pointer field, set the field's 
										conversion properties to indicate that the (pointer) field should be converted to a type that 
										supports both malloc()ed and non-malloc()ed targets. */
										auto set_pointer_field_conversion_properties = [&state1, &MR, &Rewrite](clang::QualType qtype_param, std::optional<clang::Decl const *> maybe_D = {}, std::optional<clang::CXXBaseSpecifier const *> maybe_CXXBS = {}) {
											MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(qtype_param);

											const auto qtype = get_cannonical_type(qtype_param);
											IF_DEBUG(std::string qtype_str = qtype.getAsString();)
											if (!(qtype->isPointerType())) {
												return;
											}
											if (maybe_D.has_value()) {
												auto D = maybe_D.value();
												auto FD = clang::dyn_cast<const clang::FieldDecl>(D);
												if (FD) {
													auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*FD, &Rewrite);
													if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
														size_t indirection_level = 0;
														for (auto& indirection_state_ref : ddcs_ref.m_indirection_state_stack) {
															auto ddecl_indirection = CDDeclIndirection(*FD, indirection_level);

															/* Here we're indicating that the (pointer) field should be converted to a type that supports both 
															malloc()ed and non-malloc()ed targets. */
															if (!indirection_state_ref.is_known_to_have_malloc_target()) {
																indirection_state_ref.set_is_known_to_have_malloc_target(true);
																state1.m_conversion_state_change_action_map.execute_matching_actions(state1, ddecl_indirection);
																if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
																	state1.m_dynamic_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
																}
															}
															if (!indirection_state_ref.is_known_to_have_non_malloc_target()) {
																indirection_state_ref.set_is_known_to_have_non_malloc_target(true);
																state1.m_conversion_state_change_action_map.execute_matching_actions(state1, ddecl_indirection);
																if (indirection_state_ref.is_known_to_be_used_as_an_array_iterator()) {
																	state1.m_native_array2_contingent_replacement_map.do_and_dispose_matching_replacements(state1, ddecl_indirection);
																}
															}

															++indirection_level;
														}
													}
												}
											}
										};

										checker::apply_to_all_owned_types(QT, set_pointer_field_conversion_properties, {}, {}, std::tuple{state1, &MR, &Rewrite});
									}
								}
							}


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

							auto init_CE = llvm::dyn_cast<const clang::CallExpr>(init_EX->IgnoreParenCasts());
							if (init_CE) {
								//MCSSSMallocInitializer2::s_handler1(MR, Rewrite, state1, init_CE, DD);
							}
						}
						if (VD->getType()->isPointerType()) {
							const std::string pointee_qtype_str = VD->getType()->getPointeeType().getAsString();
							if ("void" == pointee_qtype_str) {
								if (1 <= ddcs_ref.m_indirection_state_stack.size()) {
									ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
									state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*VD, CDDeclIndirection::no_indirection));
									size_t indirection_level = 0;
									for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
										indirection_state.set_xscope_eligibility(false);
										state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*VD, indirection_level));
										++indirection_level;
									}
								} else {
									assert(false);
								}
							}
						}
						auto PVD = dyn_cast<const clang::ParmVarDecl>(VD);
						if (PVD) {
							auto const& varname = PVD->getName();
							if ("argv" == varname) {
								if ("char **" == qtype_str) {
									/* Technically, we should verify that argv is a parameter of the main() function.*/
									auto DC = PVD->getParentFunctionOrMethod();
									FND = dyn_cast<const clang::FunctionDecl>(DC);
									if (FND) {
										auto FND_qname = FND->getQualifiedNameAsString();
										if ("main" == FND_qname) {
											auto [ddcs_ref, update_declaration_flag] = state1.get_ddecl_conversion_state_ref_and_update_flag(*PVD, &Rewrite);
											if (2 == ddcs_ref.m_indirection_state_stack.size()) {
												// Or should we be setting the species to "inferred array"?
												ddcs_ref.m_indirection_state_stack.at(0).set_current_species("native array");
												ddcs_ref.m_indirection_state_stack.at(0).set_xscope_eligibility(true);

												ddcs_ref.m_indirection_state_stack.at(1).set_current_species("native array");
												ddcs_ref.m_indirection_state_stack.at(1).set_xscope_eligibility(true);
											} else {
												int q = 3;
												assert(false);
											}
										}
									}
								}
							}
						}
					} else if (FD) {
						ddcs_ref.direct_type_state_ref().set_xscope_eligibility(false);
						state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*FD, CDDeclIndirection::no_indirection));
						size_t indirection_level = 0;
						for (auto& indirection_state : ddcs_ref.m_indirection_state_stack) {
							indirection_state.set_xscope_eligibility(false);
							state1.m_xscope_ineligibility_contingent_replacement_map.do_and_dispose_matching_replacements(state1, CDDeclIndirection(*FD, indirection_level));
							++indirection_level;
						}

						if (false && (qtype.getTypePtr()->isPointerType() || qtype.getTypePtr()->isReferenceType())) {
							/* These are handled in MCSSSRecordDecl2. */
						} else if (qtype.getTypePtr()->isScalarType()) {
							init_EX = FD->getInClassInitializer();
							if (!init_EX) {
								const auto parent_RD = FD->getParent();

								bool is_lambda_capture_field = false;
								assert(parent_RD);
								if (llvm::isa<CXXRecordDecl>(parent_RD)) {
									const auto CXXRD = llvm::cast<CXXRecordDecl>(parent_RD);
									assert(CXXRD);
									is_lambda_capture_field = CXXRD->isLambda();
								}

								bool is_implicit = false;
								if (FD->getSourceRange() == parent_RD->getSourceRange()) {
									/* If the FieldDecl has the same source location as its parent CXXRecordDecl,
									then we're going to assume that the FieldDecl is some implicit declaration
									(that doesn't concern us) (such as an implicit lambda capture). We're doing
									this check for now because we don't know the proper way to determine if this
									declaration is implicit or not. */
									is_implicit = true;
								}

								if ((!is_lambda_capture_field) && (!is_implicit)) {
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

											std::string initializer_info_str;
											if (qtype.getTypePtr()->isEnumeralType()) {
												if ("Dual" == ConvertMode) {
													/* Todo: add a macro to the SaferCPlusPlus library to switch between C and C++-style "enum value from 
													a given int" formats. */
													initializer_info_str = "";
												} else {
													initializer_info_str += qtype.getAsString();
													static const std::string enum_space_str = "enum ";
													if (string_begins_with(initializer_info_str, enum_space_str)) {
														initializer_info_str = initializer_info_str.substr(enum_space_str.length());
													}
													initializer_info_str += "(0)/*auto-generated init val*/";
												}
											} else if (qtype.getTypePtr()->isPointerType()) {
												if ("Dual" == ConvertMode) {
													initializer_info_str += "MSE_LH_NULL_POINTER/*auto-generated init val*/";
												} else {
													initializer_info_str += "nullptr/*auto-generated init val*/";
												}
											} else {
												initializer_info_str += "0/*auto-generated init val*/";
											}
											ddcs_ref.m_fallback_current_initialization_expr_str = initializer_info_str;

											if (!(ddcs_ref.m_has_been_replaced_as_a_whole)) {
												/* Specify that the new initialization string should be
												inserted at the end of the declaration. */
												ddcs_ref.m_initializer_SR_or_insert_before_point = ddcs_ref.m_ddecl_cptr->getSourceRange().getEnd().getLocWithOffset(+1);
											}

											update_declaration_if_not_suppressed(*l_DD, Rewrite, *(MR.Context), state1);
										}
									}
								}
							}
						}
					} else if (FND) {
						if (FND->hasAttrs()) {
							auto vec = FND->getAttrs();
							struct CAttrInfo {
								CAttrInfo(std::string_view text, clang::Attr const * attr_ptr, bool is_a_lifetime_note = false)
									: m_text(text), m_attr_ptr(attr_ptr), m_is_a_format_attribute(is_a_lifetime_note) {}
								std::string m_text;
								clang::Attr const * m_attr_ptr = nullptr;
								bool m_is_a_format_attribute = false;
							};
							std::vector<CAttrInfo> attr_infos;
							for (const auto& attr : vec) {
								auto attr_SR = attr->getRange();
								std::string raw_pretty_str;
								llvm::raw_string_ostream pretty_stream(raw_pretty_str);
								attr->printPretty(pretty_stream, clang::PrintingPolicy(clang::LangOptions()));
								pretty_stream.flush();

								std::string& pretty_str = raw_pretty_str;
								auto first_format_range = Parse::find_token_sequence({ "format", "(" }, raw_pretty_str);
								if (raw_pretty_str.length() <= first_format_range.begin) {
									//continue;
								}
								attr_infos.push_back( CAttrInfo{ pretty_str, attr, true/*m_is_a_format_attribute*/ } );
							}
							for (const auto& attr_info : attr_infos) {
								if (attr_info.m_attr_ptr) {
									auto attr_SR = write_once_source_range(cm1_adj_nice_source_range(attr_info.m_attr_ptr->getRange(), state1, Rewrite));
									std::string text1 = Rewrite.getRewrittenText(attr_SR);
									auto FA = dyn_cast<const clang::FormatAttr>(attr_info.m_attr_ptr);
									auto RA = dyn_cast<const clang::RestrictAttr>(attr_info.m_attr_ptr);
									auto AA = dyn_cast<const clang::AllocSizeAttr>(attr_info.m_attr_ptr);
									auto RNNA = dyn_cast<const clang::ReturnsNonNullAttr>(attr_info.m_attr_ptr);
									if (FA || RA || AA || RNNA) {
										/* These (function) attributes apply to C elements that we replace. So we need to 
										get rid of these attributes as well. */

										/* The problem is that attr_SR refers to the gnu attribute argument, but we really want the 
										whole "expression" including the `__attribute__` part, but that entity doesn't seem to have
										a corresponding node in the AST tree, so it's not clear how one would properly obtain the source
										range. So we'll ask cm1_adj_nice_source_range(), via its `may_be_a_gnu_attr` parameter, to try 
										to give us an extended range that covers the entire gnu attribute specifier. */
										auto adj_attr_SR = write_once_source_range(cm1_adj_nice_source_range(attr_info.m_attr_ptr->getRange(), state1, Rewrite
											, true/*may_be_a_gnu_attr*/));
										std::string attr_text = Rewrite.getRewrittenText(adj_attr_SR);
										std::string blank_text = attr_text;
										for (auto& ch : blank_text) {
											ch = ' ';
										}
										state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, adj_attr_SR, blank_text);
									} else {
										int q = 3;
									}
								}
							}
						}
					}

					if (init_EX) {
						auto init_EX_ii = IgnoreParenImpNoopCasts(init_EX, *(MR.Context));
						auto ILE = dyn_cast<const clang::InitListExpr>(init_EX_ii);
						if (ILE) {
							/* A (native) array with initializer list (including aggregate initialization). */
							MCSSSAssignment::s_handler1(MR, Rewrite, state1, nullptr/*LHS*/, init_EX_ii/*RHS*/, DD/*VLD*/, MCSSSAssignment::EIsAnInitialization::Yes);
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
								if (!is_async_shareable(base_qtype)) {
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
								if (!is_async_passable(base_qtype)) {
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
								if ((!is_async_shareable(base_qtype)) || (!is_async_passable(base_qtype))) {
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
								auto DC = VD->getParentFunctionOrMethod();
								if (DC) {
									auto FND = dyn_cast<const clang::FunctionDecl>(DC);
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

					if (update_declaration_flag) {
						update_declaration_if_not_suppressed(*DD, Rewrite, *(MR.Context), state1);
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
				auto SR = cm1_adj_nice_source_range(D->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(D, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				if (ConvertToSCPP && SR.isValid()) {
					s_handler1(MR, (*this).Rewrite, (*this).m_state1, D);
				} else {
					int q = 7;
				}
			}
		}

	private:
		Rewriter &Rewrite;
		CTUState& m_state1;
	};

	static void decl_util_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1, const clang::Decl* D) {
		MCSSSDeclUtil::s_handler1(MR, Rewrite, state1, D);
	}

	class MCSSSExprUtil : public MatchFinder::MatchCallback
	{
	public:
		MCSSSExprUtil (Rewriter &Rewrite, CTUState& state1) :
			Rewrite(Rewrite), m_state1(state1) {}

		static void s_handler1(const MatchFinder::MatchResult &MR, Rewriter &Rewrite, CTUState& state1)
		{
			const clang::Expr* E = MR.Nodes.getNodeAs<clang::Expr>("mcsssexprutil1");

			if ((E != nullptr))
			{
				auto SR = write_once_source_range(cm1_adj_nice_source_range(E->getSourceRange(), state1, Rewrite));
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
				if (std::string::npos != debug_source_text.find("png_malloc")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = state1.m_suppress_check_region_set.contains(E, Rewrite, *(MR.Context));
				//auto suppress_check_flag = state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				auto E_qtype = E->getType();
				IF_DEBUG(auto E_qtype_str = E_qtype.getAsString();)
				MSE_RETURN_IF_TYPE_IS_NULL_OR_AUTO(E_qtype);

				/* For some reason some of our expression matchers seem to be unreliable.
				So we (redundantly) implement (some of) them in this general expression matcher
				which seems to be more reliable. */

				auto E_ii = IgnoreParenImpNoopCasts(E, *(MR.Context));

				auto *CSCE = dyn_cast<const clang::CStyleCastExpr>(E);
				if (CSCE) {
					bool finished_cast_handling_flag = false;

					auto precasted_expr_ptr = CSCE->getSubExprAsWritten();
					assert(precasted_expr_ptr);
					auto precasted_CE = llvm::dyn_cast<const clang::CallExpr>(IgnoreParenImpNoopCasts(precasted_expr_ptr, *(MR.Context)));
					if (precasted_CE) {
						auto precasted_CE_qtype = precasted_CE->getType();
						IF_DEBUG(auto precasted_CE_qtype_str = precasted_CE_qtype.getAsString();)
						auto *CE = dyn_cast<const clang::CallExpr>(precasted_CE);
						if (CE && precasted_CE_qtype->isPointerType() && E_qtype->isPointerType()) {
							auto alloc_function_info1 = analyze_malloc_resemblance(*CE, state1, Rewrite);
							if (alloc_function_info1.m_seems_to_be_some_kind_of_malloc_or_realloc) {
								auto DD = NonParenImpNoopCastParentOfType<clang::DeclaratorDecl>(CSCE, *(MR.Context));
								if (DD) {
									/* This case is handled elsewhere. */
									finished_cast_handling_flag = true;
								} else {
									auto BO = NonParenImpNoopCastParentOfType<clang::BinaryOperator>(CSCE, *(MR.Context));
									if (BO) {
										const auto opcode = BO->getOpcode();
										const auto opcode_str= std::string(BO->getOpcodeStr());
										if ("=" == opcode_str) {
											/* This case is handled elsewhere. */
											finished_cast_handling_flag = true;
										}
									}
									if (!finished_cast_handling_flag) {
										MCSSSMalloc2::s_handler1(MR, Rewrite, state1, CSCE, CE);

										std::string blank_text;
										auto cast_operation_SR = cm1_adj_nice_source_range({ CSCE->getLParenLoc(), CSCE->getRParenLoc() }, state1, Rewrite);
										if (cast_operation_SR.isValid()) {
											std::string cast_operation_text = Rewrite.getRewrittenText(cast_operation_SR);
											/* We're going to "blank out"/erase the original source text of the C-style cast operation
											(including the parenthesis) (but not the expression that was being casted). */
											blank_text = cast_operation_text;
											for (auto& ch : blank_text) {
												ch = ' ';
											}
										} else {
											int q = 3;
										}
										if (true) {
											auto& ecs_ref = state1.get_expr_conversion_state_ref<CSingleArgCallExprConversionState>(*CSCE, Rewrite, *CE, blank_text);
											ecs_ref.update_current_text();

											auto CSCE_SR = cm1_adj_nice_source_range(CSCE->getSourceRange(), state1, Rewrite);
											if (CSCE_SR.isValid()) {
												state1.m_pending_code_modification_actions.add_expression_update_replacement_action(Rewrite, CSCE_SR, state1, CSCE);
												//state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, CSCE_SR, (*csce_shptr_ref).current_text());
											}
										} else {
											if (cast_operation_SR.isValid()) {
												state1.m_pending_code_modification_actions.add_straight_text_overwrite_action(Rewrite, cast_operation_SR, blank_text);
											} else {
												int q = 3;
											}
										}

										finished_cast_handling_flag = true;
									}
								}
							}
						}
					}
					if (!finished_cast_handling_flag) {
						handle_c_style_cast_without_context(MR, Rewrite, state1, CSCE);
						return;
					}
				}
				auto *CXXSCE = dyn_cast<const clang::CXXStaticCastExpr>(E);
				if (CXXSCE) {
					handle_cxx_static_cast_without_context(MR, Rewrite, state1, CXXSCE);
					return;
				}
				auto *CXXCCE = dyn_cast<const clang::CXXConstCastExpr>(E);
				if (CXXCCE) {
					handle_cxx_const_cast_without_context(MR, Rewrite, state1, CXXCCE);
					return;
				}
				auto CE = dyn_cast<const clang::CallExpr>(E);
				if (CE) {
					MCSSSArgToParameterPassingArray2::s_handler1(MR, Rewrite, state1, CE);
					MCSSSArgToReferenceParameterPassing::s_handler1(MR, Rewrite, state1, CE);
					MCSSSFunctionCall1::modifier(MR, Rewrite, state1, CE);

					if ((1 == CE->getNumArgs()) && (CE->getArg(0)->getType()->isPointerType())) {
						auto arg_EX = CE->getArg(0);
						auto arg_EX_ii = IgnoreParenImpNoopCasts(arg_EX, *(MR.Context));

						auto DRE = given_or_descendant_DeclRefExpr(arg_EX_ii, *(MR.Context));

						if (DRE) {
							MCSSSFree2::s_handler1(MR, Rewrite, state1, CE, DRE);
						}
					}
				}
				auto parent_ST_ii = NonParenImpNoopCastParentStmt(E, *MR.Context);
				if (parent_ST_ii) {
					auto RS = dyn_cast<const clang::ReturnStmt>(parent_ST_ii);
					if (RS) {
						const auto* FND = Tget_containing_element_of_type<clang::FunctionDecl>(RS, *MR.Context);
						auto DRE = given_or_descendant_DeclRefExpr(E, *(MR.Context));
						if (FND && DRE) {
							MCSSSReturnValue::s_handler1(MR, Rewrite, state1, FND, RS, DRE);
						} else {
							int q = 5;
						}
					}
				}

				if (E_qtype->isPointerType()) {
					auto& Ctx = *(MR.Context);
					auto parent_E_ii = NonParenImpNoopCastThisOrParent(Tget_immediately_containing_element_of_type<clang::Expr>(E, Ctx), Ctx);
					if (parent_E_ii) {
						/* For some reason our matcher for pointer arithmetic seems to be unreliable.
						So here we identify some of the cases of pointer arithmetic that our matcher
						has been observed to miss. */
						bool pointer_arithmetic_flag = false;
						auto ASE = dyn_cast<const clang::ArraySubscriptExpr>(parent_E_ii);
						if (ASE) {
							pointer_arithmetic_flag = true;
						} else {
							auto UO = dyn_cast<const clang::UnaryOperator>(parent_E_ii);
							auto BO = dyn_cast<const clang::BinaryOperator>(parent_E_ii);
							auto CAO = dyn_cast<const clang::CompoundAssignOperator>(parent_E_ii);
							if (CAO && (!BO)) {
								int q = 5;
							}
							if (BO) {
								/* "CompoundAssignOperator" is a class derived from BinaryOperator. BinaryOperators
								are addressed by the "ast matchers" we set up, but they don't seem to match
								CompoundAssignOperators, and an explicit matcher for CompoundAssignOperator doesn't
								seem to be available, so here we're matching them manually. Btw, a "compound
								assignment operator" would be something like "+=" or "-=". */
								const auto opcode = BO->getOpcode();
								const auto opcode_str= std::string(BO->getOpcodeStr());
								if (("+" == opcode_str) || ("+=" == opcode_str)
									|| ("-" == opcode_str) || ("-=" == opcode_str)
									|| ("<=" == opcode_str) || ("<" == opcode_str)
									|| (">=" == opcode_str) || (">" == opcode_str)
									) {

									pointer_arithmetic_flag = true;
								}
							} else if (UO) {
								const auto opcode = UO->getOpcode();
								const auto opcode_str= std::string(UO->getOpcodeStr(opcode));
								if (("++" == opcode_str) || ("--" == opcode_str)) {
									pointer_arithmetic_flag = true;
								}
							}
						}
						if (pointer_arithmetic_flag) {
							auto DRE = given_or_descendant_DeclRefExpr(E, *(MR.Context));
							if (DRE) {
								MCSSSPointerArithmetic2::s_handler1(MR, Rewrite, state1, E, DRE);
							}
						}
					}
				}

				auto BO = dyn_cast<const clang::BinaryOperator>(E);
				if (BO) {
					if (clang::BinaryOperator::Opcode::BO_Assign == BO->getOpcode()) {
						if (BO->getLHS()->getType()->isPointerType()) {
							MCSSSAssignment::s_handler1(MR, Rewrite, state1, BO->getLHS(), BO->getRHS());
						}
						auto adjusted_RHS_ii = IgnoreParenImpNoopCasts(BO->getRHS(), *(MR.Context));
						auto CSCE = dyn_cast<clang::CStyleCastExpr>(adjusted_RHS_ii);
						if (CSCE) {
							adjusted_RHS_ii = IgnoreParenImpNoopCasts(CSCE->getSubExpr(), *(MR.Context));
						}
						auto CE = dyn_cast<clang::CallExpr>(adjusted_RHS_ii);
						if (adjusted_RHS_ii->getType()->isPointerType() && CE) {
							MCSSSMalloc2::s_handler1(MR, Rewrite, state1, BO, CE);
						}
					}
				}

				auto CO = dyn_cast<const clang::ConditionalOperator>(E);
				if (CO) {
					int q = 5;
				}
			}
		}

		virtual void run(const MatchFinder::MatchResult &MR)
		{
			const clang::Expr* E = MR.Nodes.getNodeAs<clang::Expr>("mcsssexprutil1");

			if ((E != nullptr))
			{
				auto SR = write_once_source_range(cm1_adj_nice_source_range(E->getSourceRange(), m_state1, Rewrite));
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(E, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
				if (suppress_check_flag) {
					return;
				}

				if (ConvertToSCPP && SR.isValid()) {
					s_handler1(MR, (*this).Rewrite, (*this).m_state1);
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
				auto SR = cm1_adj_nice_source_range(RD->getSourceRange(), m_state1, Rewrite);
				RETURN_IF_SOURCE_RANGE_IS_NOT_VALID1;

				DEBUG_SOURCE_LOCATION_STR(debug_source_location_str, SR, Rewrite);

				RETURN_IF_FILTERED_OUT_BY_LOCATION_CONV1;

				DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);

#ifndef NDEBUG
				if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
					int q = 5;
				}
#endif /*!NDEBUG*/

				auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(RD, Rewrite, *(MR.Context));
				//auto suppress_check_flag = m_state1.m_suppress_check_region_set.contains(ISR);
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
										auto ICISR = write_once_source_range(cm1_adj_nice_source_range(ICIEX->getSourceRange(), m_state1, Rewrite));
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
										auto l_DD = field;
										auto [ddcs_ref, update_declaration_flag] = m_state1.get_ddecl_conversion_state_ref_and_update_flag(*l_DD, &Rewrite);

										update_declaration_if_not_suppressed(*l_DD, Rewrite, *(MR.Context), m_state1);
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
										auto constructor_SR = cm1_adj_nice_source_range(constructor->getSourceRange(), m_state1, Rewrite);
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
						if (checker::is_xscope_type(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_xscope_tag_base = true;
						}
						if (checker::contains_non_owning_scope_reference(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
							has_ContainsNonOwningScopeReference_tag_base = true;
						}
						if (checker::referenceable_by_scope_pointer(*(CXXRD->getTypeForDecl()), (*this).m_state1)) {
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

						if ((!has_xscope_tag_base) && checker::is_xscope_type(field_qtype, (*this).m_state1)) {
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
							&& checker::contains_non_owning_scope_reference(field_qtype, (*this).m_state1)) {
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
							&& checker::referenceable_by_scope_pointer(field_qtype, (*this).m_state1)) {
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

		/* For our purposes, we only need to import function definitions. (I think.) */
		auto FD = llvm::dyn_cast<clang::FunctionDecl>(D);
		if (!FD) {
			return;
		}
		if (!FD->isThisDeclarationADefinition()) {
			return;
		}

#if MU_LLVM_MAJOR <= 8
		auto *ToDecl = Importer.Import(D);
#elif MU_LLVM_MAJOR > 8
		const clang::Decl *ToDecl = nullptr;

		llvm::Expected<Decl *> ImportedOrErr = Importer.Import(D);
		if (!ImportedOrErr) {
			llvm::Error Err = ImportedOrErr.takeError();
			llvm::errs() << "ERROR: " << Err << "\n";
			consumeError(std::move(Err));
			//To->getTranslationUnitDecl()->dump();
			return;
		} else {
			ToDecl = *ImportedOrErr;
		}
#endif /*MU_LLVM_MAJOR*/
		if (ToDecl) {
			auto TDSR = cm1_nice_source_range(ToDecl->getSourceRange(), localRewriter);
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
					ASTImporter Importer(CI.getASTContext(), CI.getFileManager(),
								multi_tu_state_ptr->ast_units.at(I)->getASTContext(),
								multi_tu_state_ptr->ast_units.at(I)->getFileManager(),
								/* MinimalImport=*/false);

					clang::Rewriter localRewriter;
					localRewriter.setSourceMgr(multi_tu_state_ptr->ast_units.at(I)->getASTContext().getSourceManager(), multi_tu_state_ptr->ast_units.at(I)->getASTContext().getLangOpts());

					TranslationUnitDecl *TU = multi_tu_state_ptr->ast_units.at(I)->getASTContext().getTranslationUnitDecl();
					for (auto *D : TU->decls()) {
						assert(D);
						//D->dump();

						auto SR = cm1_nice_source_range(D->getSourceRange(), localRewriter);
						if (!SR.isValid()) {
							continue;
						}
						if (filtered_out_by_location<options_t<converter_mode_t> >(localRewriter.getSourceMgr(), SR.getBegin())) {
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
								const std::string function_name = FD->getNameAsString();
							} else if (llvm::isa<clang::NamespaceDecl>(D)) {
								auto NSD = llvm::cast<clang::NamespaceDecl>(D);
								assert(NSD);
								auto NNS = clang::NestedNameSpecifier::Create(multi_tu_state_ptr->ast_units.at(I)->getASTContext(), nullptr, NSD);
								if (false && NNS) {
#if MU_LLVM_MAJOR <= 8
									auto *NNSToDecl = Importer.Import(NNS);
#elif MU_LLVM_MAJOR > 8
									clang::NestedNameSpecifier *NNSToDecl = nullptr;

									llvm::Expected<NestedNameSpecifier *> ImportedOrErr = Importer.Import(NNS);
									if (!ImportedOrErr) {
										llvm::Error Err = ImportedOrErr.takeError();
										llvm::errs() << "ERROR: " << Err << "\n";
										consumeError(std::move(Err));
										//To->getTranslationUnitDecl()->dump();
										return;
									} else {
										NNSToDecl = *ImportedOrErr;
									}
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
			if (m_state1.m_ast_context_ptr != MR.Context) {
				m_state1.m_ast_context_ptr = MR.Context;
			}
			m_state1.m_ast_context_ptr = MR.Context;
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

	class MyASTConsumerPass1 : public ASTConsumer {
	public:
		MyASTConsumerPass1(Rewriter &R, CompilerInstance &CI, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param),
			HandlerForSSSExprUtil(R, tu_state()), HandlerForSSSRecordDecl(R, tu_state()), HandlerForSSSVarDecl2(R, tu_state()), 
			HandlerForSSSPointerArithmetic2(R, tu_state()), HandlerForSSSNullToPointer(R, tu_state()), HandlerForSSSMalloc2(R, tu_state()),
			HandlerForSSSMallocInitializer2(R, tu_state()), HandlerForSSSNullInitializer(R, tu_state()), HandlerForSSSFree2(R, tu_state()),
			HandlerForSSSSetToNull2(R, tu_state()), HandlerForSSSCompareWithNull2(R, tu_state()), HandlerForSSSFunctionCall1(R, tu_state()),
			HandlerForSSSConditionalExpr(R, tu_state()), HandlerForSSSAssignment(R, tu_state()), HandlerForSSSArgToParameterPassingArray2(R, tu_state()),
			HandlerForSSSArgToReferenceParameterPassing(R, tu_state()), HandlerForSSSReturnValue(R, tu_state()), HandlerForSSSFRead(R, tu_state()), HandlerForSSSFWrite(R, tu_state()), 
			HandlerForSSSAddressOf(R, tu_state()), HandlerForSSSDeclUtil(R, tu_state()), HandlerForMisc1(R, tu_state(), CI)
		{
			if (tu_state_param.m_Rewrite_ptr != &R) {
				tu_state_param.m_Rewrite_ptr = &R;
			}
			tu_state_param.m_Rewrite_ptr = &R;


			Matcher.addMatcher(DeclarationMatcher(anything()), &HandlerForMisc1);

			/* The ordering of the matchers has not yet been thoroughly considered, but generally you'd want
			elements more likely to contain subelements (that could be potentially modified) to be matched
			later. In this vein, we generally put the declaration matchers after the expression matchers. */

			Matcher.addMatcher(expr(anyOf(gnuNullExpr(), cxxNullPtrLiteralExpr(), 
					integerLiteral(equals(0), hasParent(expr(allOf(
						hasType(pointerType()), anyOf(clang::ast_matchers::implicitCastExpr(), clang::ast_matchers::cStyleCastExpr().bind("b"))
						)))))).bind("a"),
				&HandlerForSSSNullToPointer);

			Matcher.addMatcher(expr().bind("mcsssexprutil1"), &HandlerForSSSExprUtil);

			Matcher.addMatcher(expr(allOf(
					hasParent(expr(anyOf(
						unaryOperator(hasOperatorName("++")), unaryOperator(hasOperatorName("--")),
						binaryOperator(hasOperatorName("+=")), binaryOperator(hasOperatorName("-=")),
						castExpr(hasParent(expr(anyOf(
								binaryOperator(hasOperatorName("+")), binaryOperator(hasOperatorName("+=")),
								binaryOperator(hasOperatorName("-")), binaryOperator(hasOperatorName("-=")),
								binaryOperator(hasOperatorName("<=")), binaryOperator(hasOperatorName("<")),
								binaryOperator(hasOperatorName(">=")), binaryOperator(hasOperatorName(">")),
								arraySubscriptExpr()
							))))
						))),
					hasType(pointerType()),
					anyOf(
							memberExpr(expr(hasDescendant(declRefExpr().bind("mcssspointerarithmetic")))).bind("mcssspointerarithmetic2"),
							declRefExpr().bind("mcssspointerarithmetic"),
							hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("mcssspointerarithmetic")))).bind("mcssspointerarithmetic2")),
							hasDescendant(declRefExpr().bind("mcssspointerarithmetic"))
					)
					)).bind("mcssspointerarithmetic3"), &HandlerForSSSPointerArithmetic2);

			//Matcher.addMatcher(castExpr(allOf(hasCastKind(CK_ArrayToPointerDecay), unless(hasParent(arraySubscriptExpr())))).bind("mcsssarraytopointerdecay"), &HandlerForSSSArrayToPointerDecay);

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

			Matcher.addMatcher(
					callExpr(allOf(
							hasAnyArgument(
									ignoringParenCasts(expr(anyOf(
									memberExpr(expr(hasDescendant(declRefExpr().bind("functioncall2")))).bind("functioncall3"),
									declRefExpr().bind("functioncall2"),
									hasDescendant(memberExpr(expr(hasDescendant(declRefExpr().bind("functioncall2")))).bind("functioncall3")),
									hasDescendant(declRefExpr().bind("functioncall2"))
							)))),
							hasAnyArgument(hasType(pointerType()))
					)).bind("functioncall1"), &HandlerForSSSFunctionCall1);

			Matcher.addMatcher(binaryOperator(allOf(
					hasOperatorName("="),
					hasLHS(anyOf(
								ignoringParenCasts(declRefExpr().bind("mcssssettonull3")),
								ignoringParenCasts(expr(hasDescendant(declRefExpr().bind("mcssssettonull3"))))
						)),
						hasLHS(expr(hasType(pointerType())))
						)).bind("mcssssettonull1"), &HandlerForSSSSetToNull2);

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

			/*
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
					*/

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

			Matcher.addMatcher(returnStmt(allOf(
					hasAncestor(functionDecl().bind("mcsssreturnvalue1")),
						hasDescendant(declRefExpr().bind("mcsssreturnvalue3"))
						)).bind("mcsssreturnvalue2"), &HandlerForSSSReturnValue);

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

			Matcher.addMatcher(binaryOperator(allOf(
					anyOf(hasOperatorName("=="), hasOperatorName("!=")),
					hasLHS(anyOf(
								ignoringParenCasts(declRefExpr().bind("mcssscomparewithnull3")),
								ignoringParenCasts(expr(hasDescendant(declRefExpr().bind("mcssscomparewithnull3"))))
						)),
						hasLHS(expr(hasType(pointerType())))
						)).bind("mcssscomparewithnull1"), &HandlerForSSSCompareWithNull2);

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

			Matcher.addMatcher(clang::ast_matchers::recordDecl().bind("mcsssrecorddecl"), &HandlerForSSSRecordDecl);

			Matcher.addMatcher(varDecl(hasInitializer(ignoringParenCasts(
									callExpr().bind("mcsssmallocinitializer2")
					))).bind("mcsssmallocinitializer3"), &HandlerForSSSMallocInitializer2);

			//Matcher.addMatcher(varDecl(hasType(pointerType())).bind("mcsssnativepointer"), &HandlerForSSSNativePointer);
			//Matcher.addMatcher(clang::ast_matchers::declaratorDecl().bind("mcsssvardecl"), &HandlerForSSSVarDecl2);
			Matcher.addMatcher(varDecl(anyOf(
					hasInitializer(anyOf(
							expr(cStyleCastExpr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl3")).bind("mcsssvardecl2"),
							expr(hasDescendant(declRefExpr().bind("mcsssvardecl5"))).bind("mcsssvardecl2")
							)),
					clang::ast_matchers::anything()
					)).bind("mcsssvardecl"), &HandlerForSSSVarDecl2);

			Matcher.addMatcher(varDecl(hasInitializer(ignoringParenCasts(
									expr().bind("mcsssnullinitializer2")
					))).bind("mcsssnullinitializer3"), &HandlerForSSSNullInitializer);

			Matcher.addMatcher(varDecl(hasInitializer(ignoringParenCasts(
							anyOf(
									conditionalOperator(has(declRefExpr())).bind("mcsssconditionalinitializer2"),
									conditionalOperator(hasDescendant(declRefExpr())).bind("mcsssconditionalinitializer2")
							)
					))).bind("mcsssconditionalinitializer3"), &HandlerForSSSConditionalExpr);

			Matcher.addMatcher(conditionalOperator().bind("mcsssconditionalinitializer2"), &HandlerForSSSConditionalExpr);

			Matcher.addMatcher(decl().bind("mcsssdeclutil1"), &HandlerForSSSDeclUtil);

		}
 
		void HandleTranslationUnit(ASTContext &Context) override 
		{
			Matcher.matchAST(Context);
		}

		private:

		CTUState *m_tu_state_ptr = nullptr;
		CTUState& tu_state() { return *m_tu_state_ptr;}

		MCSSSExprUtil HandlerForSSSExprUtil;
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
		MCSSSFunctionCall1 HandlerForSSSFunctionCall1;
		MCSSSConditionalExpr HandlerForSSSConditionalExpr;
		MCSSSAssignment HandlerForSSSAssignment;
		MCSSSArgToParameterPassingArray2 HandlerForSSSArgToParameterPassingArray2;
		MCSSSArgToReferenceParameterPassing HandlerForSSSArgToReferenceParameterPassing;
		MCSSSReturnValue HandlerForSSSReturnValue;
		MCSSSFRead HandlerForSSSFRead;
		MCSSSFWrite HandlerForSSSFWrite;
		MCSSSAddressOf HandlerForSSSAddressOf;
		MCSSSDeclUtil HandlerForSSSDeclUtil;
		Misc1 HandlerForMisc1;

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

	class MyPPCallbacksPass1 : public PPCallbacks
	{
	public:
		MyPPCallbacksPass1(Rewriter& Rewriter_ref, CompilerInstance &CI_ref, CTUState &tu_state_param) : m_tu_state_ptr(&tu_state_param), m_Rewriter_ref(Rewriter_ref), CI(CI_ref) {}
		~MyPPCallbacksPass1() {
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
			if (("msetl.h" == file_name_str) || ("mselegacyhelpers.h" == file_name_str)) {
				current_fii_shptr()->m_legacyhelpers_include_directive_found = true;
			}
			int q = 5;
		}
	}

	void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) override {

		auto MNTSL = MacroNameTok.getLocation();
		auto MNTSLE = MacroNameTok.getEndLoc();
		auto MacroNameTokSR = COrderedSourceRange(MNTSL, MNTSLE);
		std::string macro_nametok_text = m_Rewriter_ref.getRewrittenText(MacroNameTokSR);
		bool is_function_macro = false;
		//std::string macro_name = MacroNameTok.getName();
		std::string macro_name = macro_nametok_text;
		if ((1 <= macro_name.length()) && ('(' == macro_name.back())) {
			is_function_macro = true;
			macro_name = macro_name.substr(0, macro_name.length() - 1);
		}

#ifndef NDEBUG
		if (string_begins_with(macro_nametok_text, "YY_CURRENT_BUFFER")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		{
			if ("MSE_SCOPEPOINTER_DISABLED" == macro_nametok_text) {
				(*this).tu_state().m_MSE_SCOPEPOINTER_DISABLED_defined = true;
			} else if ("MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED" == macro_nametok_text) {
				(*this).tu_state().m_MSE_SOME_NON_XSCOPE_POINTER_TYPE_IS_DISABLED_defined = true;
			} else if ("MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS" == macro_nametok_text) {
				(*this).tu_state().m_MSE_DISABLE_RAW_POINTER_SCOPE_RESTRICTIONS_defined = true;
			}
		}

		if (("" == macro_nametok_text)) {
			return;
		}

		if (!MD) {
			int q = 3;
			return;
		}

		auto macro_def = MD->getDefinition();
		auto MDSL = macro_def.getMacroInfo()->getDefinitionLoc();
		auto MDSLE = macro_def.getMacroInfo()->getDefinitionEndLoc();
		std::string macro_def_text = m_Rewriter_ref.getRewrittenText({ MDSL, MDSLE });
		std::string_view macro_def_body_sv;
		{
			std::string::size_type search_start_pos = 0;
			if (is_function_macro) {
				auto rparen_pos = macro_def_text.find(")");
				if (std::string::npos == rparen_pos) {
					int q = 3;
				} else {
					search_start_pos = rparen_pos;
				}
			}
			auto wh_pos = Parse::find_whitespace(macro_def_text, search_start_pos);
			auto nwh_pos = Parse::find_non_whitespace(macro_def_text, wh_pos);
			std::string_view macro_def_text_sv = macro_def_text;
			if (macro_def_text_sv.size() > nwh_pos) {
				macro_def_body_sv = macro_def_text_sv.substr(nwh_pos);
			}
		}

		std::vector<std::string> parameter_names;
		for (auto identifier_info_ptr : macro_def.getMacroInfo()->params()) {
			if (identifier_info_ptr) {
				auto param_name = identifier_info_ptr->getName();
				parameter_names.push_back(std::string(param_name));
			} else {
				int q = 3;
			}
		}

		if (current_fii_shptr()) {
			if (!(current_fii_shptr()->m_first_macro_directive_ptr_is_valid)) {
				current_fii_shptr()->m_first_macro_directive_ptr = MD;
				current_fii_shptr()->m_first_macro_directive_ptr_is_valid = true;
			}
		}

		auto ppmdi = CPPMacroDefinitionInfo(MacroNameTok, *MD, is_function_macro, macro_def_body_sv, std::move(parameter_names));
		auto iter1 = (*this).m_tu_state_ptr->m_pp_macro_definitions.find(macro_name);
		if ((*this).m_tu_state_ptr->m_pp_macro_definitions.end() != iter1) {
			IF_DEBUG(std::string found_macro_text = m_Rewriter_ref.getRewrittenText(iter1->second.definition_SR());)
			if (ppmdi.m_MacroNameTok.getName() != iter1->second.m_MacroNameTok.getName()) {
				int q = 5;
			} else {
				int q = 5;
			}
		} else {
			(*this).m_tu_state_ptr->m_pp_macro_definitions.insert({ macro_name, ppmdi });
		}
	}

	void MacroExpands(const Token &MacroNameTok, const MacroDefinition &MD, SourceRange Range,
								const MacroArgs *Args) override {
		if (!(*this).m_tu_state_ptr) { assert(false); return; }

		auto MNTSL = MacroNameTok.getLocation();
		auto MNTSLE = MacroNameTok.getEndLoc();
		auto MacroNameTokSR = COrderedSourceRange(MNTSL, MNTSLE);
		std::string macro_nametok_text = m_Rewriter_ref.getRewrittenText(MacroNameTokSR);
		std::string macro_name = MacroNameTok.getName();
		std::string macro_text = m_Rewriter_ref.getRewrittenText(Range);

		auto& SM = (*this).m_Rewriter_ref.getSourceMgr();
		IF_DEBUG(std::string debug_source_location_str = Range.getBegin().printToString(SM);)

		if (filtered_out_by_location<options_t<converter_mode_t> >(SM, Range.getBegin())) {
			return;
		}

#ifndef NDEBUG
		if (std::string::npos != debug_source_location_str.find("connect.c:942:")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		if (("" == macro_nametok_text) && ("" == macro_text) && (Range.getBegin() == Range.getEnd())) {
			return;
		}
		bool is_function_macro = false;
		if ((1 <= macro_nametok_text.size()) && ('(' == macro_nametok_text.back())) {
			is_function_macro = true;
			if (!((1 <= macro_text.size()) && (')' == macro_text.back()))) {
				int i = 5;
			}
		}

		auto& macro_def = MD;
		auto MDSL = macro_def.getMacroInfo()->getDefinitionLoc();
		auto MDSLE = macro_def.getMacroInfo()->getDefinitionEndLoc();
		auto MacroDefSR = COrderedSourceRange(MDSL, MDSLE);

#ifndef NDEBUG
		std::string macro_def_text = m_Rewriter_ref.getRewrittenText({ MDSL, MDSLE });

		if (string_begins_with(macro_nametok_text, "MACRO")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		auto ppmii = CPPMacroInstanceInfo{ MacroNameTok, MD, Range, Args, is_function_macro };

		auto iter1 = (*this).m_tu_state_ptr->m_pp_macro_instances.find(MacroNameTokSR.getBegin());
		if ((*this).m_tu_state_ptr->m_pp_macro_instances.end() != iter1) {
			IF_DEBUG(std::string found_macro_text = m_Rewriter_ref.getRewrittenText(iter1->second.instance_SR());)
			if (ppmii.m_MacroNameTok.getName() != iter1->second.m_MacroNameTok.getName()) {
				int q = 5;
			} else {
				int q = 5;
			}
		} else {
			(*this).m_tu_state_ptr->m_pp_macro_instances.insert({ MacroNameTokSR.getBegin(), ppmii });
		}

		auto nice_MacroNameTokSR = cm1_nice_source_range(MacroNameTokSR, (*this).m_Rewriter_ref);
		if (!(nice_MacroNameTokSR == MacroNameTokSR)) {
			auto iter1 = (*this).m_tu_state_ptr->m_pp_macro_instances.find(nice_MacroNameTokSR.getBegin());
			if ((*this).m_tu_state_ptr->m_pp_macro_instances.end() != iter1) {
				if (ppmii.m_MacroNameTok.getName() != iter1->second.m_MacroNameTok.getName()) {
					int q = 5;
				} else {
					int q = 5;
				}
			} else {
				(*this).m_tu_state_ptr->m_pp_macro_instances.insert({ nice_MacroNameTokSR.getBegin(), ppmii });
			}
		}

		auto instanceSPSL = SM.getSpellingLoc(MacroNameTokSR.getBegin());
		auto instanceSPSLE = SM.getSpellingLoc(MacroNameTokSR.getEnd());
		auto instanceSPSR = SourceRange{ instanceSPSL, instanceSPSLE };
		if ((!(instanceSPSR == MacroNameTokSR)) && (!(nice_MacroNameTokSR == instanceSPSR))) {
			auto iter1 = (*this).m_tu_state_ptr->m_pp_macro_instances.find(instanceSPSR.getBegin());
			if ((*this).m_tu_state_ptr->m_pp_macro_instances.end() != iter1) {
				if (ppmii.m_MacroNameTok.getName() != iter1->second.m_MacroNameTok.getName()) {
					int q = 5;
				} else {
					int q = 5;
				}
			} else {
				(*this).m_tu_state_ptr->m_pp_macro_instances.insert({ instanceSPSR.getBegin(), ppmii });
			}
		}
	}

	void FileChanged(SourceLocation Loc, FileChangeReason Reason,
							SrcMgr::CharacteristicKind FileType,
							FileID PrevFID = FileID()) override {

		bool filename_is_invalid = false;
		auto full_path_name = std::string(m_Rewriter_ref.getSourceMgr().getBufferName(Loc, &filename_is_invalid));
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

	struct CFileConversionRecord {
	public:
		std::string m_path;
		std::string m_original_filename;
		std::string m_target_filename;
		std::vector<size_t> m_converted_version_tu_numbers;
	};

	thread_local std::vector<CTUState> g_prepared_initial_tu_states;

	class MyFrontendActionPass1 : public ASTFrontendActionCompatibilityWrapper1
	{
	public:
		MyFrontendActionPass1() {
			if (true) {
				if (1 > g_prepared_initial_tu_states.size()) {
					//assert(false);
				} else {
					/* Initialize the "state" with information computed in the "checker" pass. */
					(*this).m_tu_state = g_prepared_initial_tu_states.back();
					g_prepared_initial_tu_states.pop_back();
				}
			}
		}
		~MyFrontendActionPass1() {
			if (ConvertToSCPP) {
				auto res = overwriteChangedFiles();
				int q = 5;
			}
		}

		bool BeginSourceFileAction(CompilerInstance &ci) override {
			s_source_file_action_num += 1;
			std::unique_ptr<MyPPCallbacksPass1> my_pp_callbacks_ptr(new MyPPCallbacksPass1(TheRewriter, ci, (*this).m_tu_state));
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

					auto SR = action.first;

					IF_DEBUG(std::string debug_source_location_str = SR.getBegin().printToString(SM);)
					DEBUG_SOURCE_TEXT_STR(debug_source_text, SR, Rewrite);
#ifndef NDEBUG
					if (std::string::npos != debug_source_location_str.find(g_target_debug_source_location_str1)) {
						int q = 5;
					}
#endif /*!NDEBUG*/

					/* If the range can only be reliably modified once, then we want to suppress any attempts to modify 
					the range until the last modification action is (about to be) executed. */
					m_tu_state.m_pending_code_modification_actions.set_ReplaceText_supression_mode(!(action.first.is_rewritable()));

					/* Execute and discard all source code text modification functions associated with this source range. */
					auto sub_it = action.second.begin();
					while (action.second.size() >= 1) {
						/* Note that we may cause the container to be modified as we're iterating over it. (So we can't use
						a "traditional" loop.) */

						if (1 == action.second.size()) {
							/* This appears to be the last modification action, so we want to disengage any suppression of modification 
							to the source range due to the range reliably supporting only one modification. */
							m_tu_state.m_pending_code_modification_actions.clear_ReplaceText_supression_mode();
						}

						auto& modification_function = *sub_it;
						modification_function();
						action.second.erase(sub_it);
						sub_it = action.second.begin();
					}
					m_tu_state.m_pending_code_modification_actions.clear_ReplaceText_supression_mode();
				}

				auto res1 = m_tu_state.m_pending_code_modification_actions.erase(retained_it);

				rit = m_tu_state.m_pending_code_modification_actions.rbegin();
			}

			TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(llvm::outs());

			{
				clang::CompilerInstance &ci = getCompilerInstance();
				clang::Preprocessor &pp = ci.getPreprocessor();
				//MyPPCallbacksPass1 *my_pp_callbacks_ptr = static_cast<MyPPCallbacksPass1 *>(pp.getPPCallbacks());
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
					MyPPCallbacksPass1 *my_pp_callbacks_ptr = static_cast<MyPPCallbacksPass1 *>(pp_callbacks_ptr);

					for (auto& item_ref : my_pp_callbacks_ptr->m_first_include_info_map) {
						auto& fii_ref = *(item_ref.second);
						assert(fii_ref.m_beginning_of_file_loc_is_valid);

						bool filename_is_invalid = false;
						auto full_path_name = std::string(TheRewriter.getSourceMgr().getBufferName(fii_ref.m_beginning_of_file_loc, &filename_is_invalid));

						if (filtered_out_by_location<options_t<converter_mode_t> >(TheRewriter.getSourceMgr(), fii_ref.m_beginning_of_file_loc)) {
							continue;
						}

						if (!(fii_ref.m_legacyhelpers_include_directive_found)) {
							if (false/* While it might be aesthetically nicer to put our include directive
								together with the (first) ones already present, it is sometimes not correct. */
								&& fii_ref.m_first_include_directive_loc_is_valid) {
								TheRewriter.InsertTextBefore(fii_ref.m_first_include_directive_loc,
										"\n#include \"mselegacyhelpers.h\"\n");
							} else if (false/* In auto-generated headers, the first directive might not
								be an include guard. */
								&& fii_ref.m_first_macro_directive_ptr_is_valid) {
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
			return std::make_unique<MyASTConsumerPass1>(TheRewriter, CI, (*this).m_tu_state);
		}

		bool overwriteChangedFiles() {
			std::set<std::pair<std::string, std::string> > filename_info_set;
			{
				for (auto  I = TheRewriter.buffer_begin(), E = TheRewriter.buffer_end(); I != E; ++I) {
					const FileEntry *Entry = TheRewriter.getSourceMgr().getFileEntryForID(I->first);

					auto pathname = std::string(Entry->tryGetRealPathName());
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
					auto res1 = evaluate_filtering_by_full_path_name<options_t<converter_mode_t> >(pathname);
					if (!(res1.m_do_not_process)) {
						filename_info_set.insert(item);
					} else {
						int q = 3;
					}

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
	int MyFrontendActionPass1::s_source_file_action_num = 0;
	std::map<std::string, CFileConversionRecord> MyFrontendActionPass1::s_file_conversion_record_map;


	int number_of_instances_of_given_strings(const std::string& str1, const std::vector<std::string>& waldos) {
		int count = 0;
		for (auto& waldo : waldos) {
			auto search_start_index = str1.find(waldo, 0);
			while (std::string::npos != search_start_index) {
				count += 1;
				search_start_index = str1.find(waldo, search_start_index + waldo.size());
			}
		}
		return count;
	}
	int number_of_instances_of_mse(const std::string& str1) {
		return number_of_instances_of_given_strings(str1, { "mse::", "MSE_" });
	}
	int number_of_instances_of_iterator(const std::string& str1) {
		return number_of_instances_of_given_strings(str1, { "Iterator", "_ITERATOR" });
	}
	int number_of_instances_of_TXScopeLHNullableAny_Pointer_and_Iterator(const std::string& str1) {
		return number_of_instances_of_given_strings(str1, { "::lh::TXScopeLHNullableAnyRandomAccessIterator", "_LH_LOCAL_VAR_ONLY_ARRAY_ITERATOR_TYPE"
			, "::lh::TXScopeLHNullableAnyPointer", "_LH_LOCAL_VAR_ONLY_POINTER_TYPE" });
	}
	int number_of_instances_of_TLHNullableAny_Pointer_and_Iterator(const std::string& str1) {
		return number_of_instances_of_given_strings(str1, { "::lh::TLHNullableAnyRandomAccessIterator", "_LH_ARRAY_ITERATOR_TYPE"
			, "::lh::TLHNullableAnyPointer", "_LH_POINTER_TYPE" });
	}
	int number_of_instances_of_TNativeArrayReplacement(const std::string& str1) {
		return number_of_instances_of_given_strings(str1, { "::lh::TNativeArrayReplacement", "MSE_LH_FIXED_ARRAY_TYPE_PREFIX" });
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
			} else {
				auto TXScopeLHNullableAny_count1 = number_of_instances_of_TXScopeLHNullableAny_Pointer_and_Iterator(first_option);
				auto TXScopeLHNullableAny_count2 = number_of_instances_of_TXScopeLHNullableAny_Pointer_and_Iterator(second_option);
				auto TLHNullableAny_count1 = number_of_instances_of_TLHNullableAny_Pointer_and_Iterator(first_option);
				auto TLHNullableAny_count2 = number_of_instances_of_TLHNullableAny_Pointer_and_Iterator(second_option);
				auto TNativeArrayReplacement_count1 = number_of_instances_of_TNativeArrayReplacement(first_option);
				auto TNativeArrayReplacement_count2 = number_of_instances_of_TNativeArrayReplacement(second_option);
				if (1 <= (TXScopeLHNullableAny_count1 + TXScopeLHNullableAny_count2 + TLHNullableAny_count1 + TLHNullableAny_count2 
					+ TNativeArrayReplacement_count1 + TNativeArrayReplacement_count2)) {

					/* If a header file has an `extern` declaration of a pointer, only the translation unit with the 
					corresponding (one-and-only) non-`extern` declaration (+definition) will be able to determine that 
					the pointer actually refers to a native array, and if so, what its size is. */
					if (TNativeArrayReplacement_count1 > TNativeArrayReplacement_count2) {
						return first_option;
					} else if (TNativeArrayReplacement_count2 > TNativeArrayReplacement_count1) {
						return second_option;
					}

					/* By default, legacy iterators and pointers are converted to (restricted) "xscope"
					types. But in some transation units, there is enough information to determine that
					(less restrictive) non-"xscope" versions are required in places. So when there is a
					conflict where conversion of one translation unit suggests an "xscope" type and
					another suggests a non-"xscope" type, we will choose the non-"xscope" option. */
					if ((TLHNullableAny_count2 < TLHNullableAny_count1) && (TXScopeLHNullableAny_count2 > TXScopeLHNullableAny_count1)) {
						return first_option;
					} else if ((TLHNullableAny_count2 > TLHNullableAny_count1) && (TXScopeLHNullableAny_count2 < TXScopeLHNullableAny_count1)) {
						return second_option;
					}
				}
				if (first_option.length() > second_option.length()) {
					return first_option;
				} else if (second_option.length() > first_option.length()) {
					return second_option;
				}
			}
		}
		return first_option;
	}

	auto num_newlines(std::string_view sv) {
		int retval = 0;
		auto last_eol_index = sv.find("\n");
		while (std::string_view::npos != last_eol_index) {
			retval += 1;
			last_eol_index = sv.find("\n", last_eol_index + 1);
		}
		return retval;
	}

	auto start_index_of_specified_line(std::string_view sv, size_t n) {
		decltype(sv.find("\n")) retval = 0;
		for (size_t i = 0; i < n; i+=1) {
			retval = sv.find("\n", retval + 1);
			if (std::string_view::npos == retval) {
				break;
			}
			retval += 1;
		}
		if (false && (sv.size() <= retval)) {
			return std::string_view::npos;
		}
		return retval;
	}

	auto improve_merge_output(const std::string& source_file_text) {
		std::string retval = source_file_text;

#ifndef NDEBUG
		if (false && std::string::npos != retval.find("png_set_read_status_fn")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		bool first_conflict_flag = true;
		bool first_option_was_chosen_in_the_previous_conflict = false;
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

#ifndef NDEBUG
			if (false && std::string::npos != second_option.find("png_set_read_status_fn")) {
				int q = 5;
			}
#endif /*!NDEBUG*/

			auto next_conflict_start_index = retval.find("<<<<<<< ", conflict_end_eol_index);
			if (std::string::npos == next_conflict_start_index) {
				break;
			}
			if ((conflict_end_eol_index + 2 == next_conflict_start_index) || (conflict_end_eol_index + 1 == next_conflict_start_index)) {
				/* The next merge conflict seems to immediately follow the current one. (I.e. they could have
				been almalgamated into a single merge conflict) */

				auto next_conflict_start_eol_index = retval.find("\n", next_conflict_start_index + 1);
				if (std::string::npos == next_conflict_start_eol_index) {
					break;
				}
				auto next_conflict_divider_index = retval.find("=======\n", next_conflict_start_eol_index + 1);
				if (std::string::npos == next_conflict_divider_index) {
					break;
				}
				auto next_conflict_divider_eol_index = retval.find("\n", next_conflict_divider_index + 1);
				auto next_conflict_end_index = retval.find(">>>>>>> ", next_conflict_divider_eol_index + 1);
				if (std::string::npos == next_conflict_end_index) {
					break;
				}
				auto next_conflict_end_eol_index = retval.find("\n", next_conflict_end_index + 1);
				if (std::string::npos == next_conflict_end_eol_index) {
					break;
				}

				auto next_conflict_first_option = retval.substr(next_conflict_start_eol_index + 1, next_conflict_divider_index - (next_conflict_start_eol_index + 1));
				auto next_conflict_second_option = retval.substr(next_conflict_divider_eol_index + 1, next_conflict_end_index - (next_conflict_divider_eol_index + 1));

				{
					const auto start_of_second_option_sv = std::string_view(second_option.data() + 0, first_option.length());
					if (start_of_second_option_sv == first_option) {
						/* We've observed with the default (ubuntu 20) system merge, that it sometimes it
						identifies two adjacent merge conflicts (that could've been classified as a
						single merge conflict), but doesn't divide the merge in the same place for the 
						two merge options. This can cause problems (i.e. duplicate code) if, for example,
						the second merge option is selected for the first conflict, and the first merge
						option is selected for the (adjacent) second conflict. */

						auto text_to_be_moved = second_option.substr(first_option.length());
						retval.insert(next_conflict_divider_eol_index + 1, text_to_be_moved);
						retval.replace(conflict_divider_eol_index + 1 + first_option.length(), text_to_be_moved.length(), "");
					}
				}
			}

			conflict_start_index = retval.find("<<<<<<< ", next_conflict_start_index);
		}

		return retval;
	}

	auto resolve_merge_conflicts_with_best_guess_text(const std::string& source_file_text) {
		std::string retval = source_file_text;

#ifndef NDEBUG
		if (false && std::string::npos != retval.find("png_set_read_status_fn")) {
			int q = 5;
		}
#endif /*!NDEBUG*/

		bool first_conflict_flag = true;
		bool first_option_was_chosen_in_the_previous_conflict = false;
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

#ifndef NDEBUG
				if (std::string::npos != first_option.find("png_set_PLTE")) {
					int q = 5;
				}
#endif /*!NDEBUG*/

			if ((first_option.empty() || second_option.empty()) && (!first_conflict_flag)) {
				/* We've observed with the default (ubuntu 20) system merge, that when one of the merge
				options is empty, it tends to be because the option that should be there was included
				as part of the option for the previous merge conflict. */
				if (first_option_was_chosen_in_the_previous_conflict) {
					retval.replace(conflict_start_index, conflict_end_eol_index - conflict_start_index, first_option);
				} else {
					retval.replace(conflict_start_index, conflict_end_eol_index - conflict_start_index, second_option);
				}
			} else {
				auto& chosen_option_ref = chosen_merge_option_ref(first_option, second_option);
				retval.replace(conflict_start_index, conflict_end_eol_index - conflict_start_index, chosen_option_ref);
				conflict_start_index += chosen_option_ref.size();
				first_option_was_chosen_in_the_previous_conflict = ((&chosen_option_ref) == (&first_option));
			}

			first_conflict_flag = false;

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

		content = improve_merge_output(content);

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
		AddressableVars = options.AddressableVars;

		std::cout << "\nNote, this program attempts to modify the specified source files in place";
		std::cout << ", and any directly or indirectly `#include`d files, which may include headers from 3rd party libraries or other files you may not expect. ";
		std::cout << "So be careful not to run this program with write permissions to files you can't risk being modified. ";
		std::cout << "(Running this program in a discardable, easily restorable container with an isolated filesystem would be ideal.) \n";
		std::cout << "Continue [y/n]? \n";
		int ich2 = 0;
		if (SuppressPrompts) {
			ich2 = int('Y');
		} else {
			do {
				ich2 = std::getchar();
				//std::putchar(ich2);
			} while ((int('y') != ich2) && (int('n') != ich2) && (int('Y') != ich2) && (int('N') != ich2));
		}
		if (((int('y') != ich2) && (int('Y') != ich2)) || (DoNotReplaceOriginalSource)) {
			std::cout << "\n\nOperation cancelled. Source files were not replaced/modified. \n";
			typedef decltype(Tool.run(newFrontendActionFactory<MyFrontendActionPass1>().get())) return_t;
			return return_t{-1};
		}

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

		auto retval = Tool.run(newFrontendActionFactory<MyFrontendActionPass1>().get());

		std::cout << "\nThe specified and dependent source files will now be replaced/modified. Make sure you have appropriate backup copies before proceeding!!! \n";
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
			for (auto& item_ref : MyFrontendActionPass1::s_file_conversion_record_map) {
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
#else /*!EXCLUDE_CONVERTER_MODE1*/
#endif /*!EXCLUDE_CONVERTER_MODE1*/

#endif //__CONVERTER_MODE1_H