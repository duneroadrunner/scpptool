// Copyright (c) 2019 Noah Lopez
// special thanks to Farzad Sadeghi
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef __SCPPTOOL_H
#define __SCPPTOOL_H

#if __cpp_exceptions >= 199711
#define SCPPT_THROW(x) throw(x)
#define SCPPT_TRY try
#define SCPPT_CATCH(x) catch(x)
#define SCPPT_CATCH_ANY catch(...)
#define SCPPT_FUNCTION_TRY try
#define SCPPT_FUNCTION_CATCH(x) catch(x)
#define SCPPT_FUNCTION_CATCH_ANY catch(...)
#else // __cpp_exceptions >= 199711
#define SCPPT_THROW(x) exit(-11)
#define SCPPT_TRY if (true)
#define SCPPT_CATCH(x) if (false)
#define SCPPT_CATCH_ANY if (false)
#define SCPPT_FUNCTION_TRY
#define SCPPT_FUNCTION_CATCH(x) void SCPPT_placeholder_function_catch(x)
#define SCPPT_FUNCTION_CATCH_ANY void SCPPT_placeholder_function_catch_any()
#endif // __cpp_exceptions >= 199711

#ifndef MU_LLVM_MAJOR
#ifdef __clang_major__z
#define MU_LLVM_MAJOR __clang_major__
#else /*__clang_major__*/
#define MU_LLVM_MAJOR 8
#endif /*__clang_major__*/
#endif /*MU_LLVM_MAJOR*/

#ifndef MSE_NAMESPACE_STR
#define MSE_NAMESPACE_STR "mse"
#endif //MSE_NAMESPACE_STR

#endif //__SCPPTOOL_H

