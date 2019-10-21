
Oct 2019

### How to Build:

[Download llvm 8](http://releases.llvm.org/download.html#8.0.0) and extract it to a directory of your choosing, say, `~/llvm8`. [Download scpptool](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract it to a directory of your choosing, say, `~/scpptool`. Open a shell and switch the the scpptool directory. Type:

`make LLVM_CONF=~/llvm8/bin/llvm-conf`

(If necessary, substitute the `~/llvm8` part with the directory you chose for llvm 8.)

### How to Use:

The syntax is as follows:

`scpptool {source filename(s)} -- {compiler options}`

So for example:

`scpptool hello_world.cpp -- -I./msetl -std=c++17`


### Overview

scpptool is a command line tool to help enforce a memory and data race safe subset of C++. It's designed to work with the [SaferCPlusPlus](https://github.com/duneroadrunner/SaferCPlusPlus) library. It analyzes the specified C++ file(s) and reports places in the code that it cannot verify to be safe. Currently it does not report all of the instances where it cannot verify safety, but that is the eventual goal.

Note that while the aims are similar, this tool enforces a slightly more restricted subset of the language than does the lifetime profile checker. The idea being that this restrictiveness is mitigated by additional flexible elements provided in the SaferCPlusPlus library (that may sometimes resort to run-time safety mechanisms when necessary). Code that conforms to the subset enforced by this tool should also be conformant to the subset enforced by an (eventually completed) lifetime profile checker.

Note that this tool is still in early development and not well tested.


### Local Suppression of the Checks

You can use a "check suppression directive" to indicate places in the source code that the tool should not report conformance violations. For example:

```cpp
{
    const auto ptr1 = new int(5); // scpptool will complain
    MSE_SUPPRESS_CHECK_IN_XSCOPE const auto ptr2 = new int(7); // scpptool will not complain
}
```

The presence of `MSE_SUPPRESS_CHECK_IN_XSCOPE` (a macro provided in the SaferCPlusPlus library) indicates that checking should be suppressed for the statement that follows it. The `XSCOPE` suffix means that this directive can only be used in "execution scopes" (sometimes referred to as "blocks"). Essentially places where you can execute a function. As opposed to "declaration scopes". For example:

```cpp
{
    struct A1 {
        int* const m_ptr1 = nullptr; // scpptool will complain
        MSE_SUPPRESS_CHECK_IN_XSCOPE int* const m_ptr2 = nullptr; // compile error
        MSE_SUPPRESS_CHECK_IN_DECLSCOPE int* const m_ptr3 = nullptr; // this will work
    };
}
```


### About the Enforced Subset

#### Restrictions on the use of native pointers and references

The SaferCPlusPlus library is designed to enable you to avoid potentially unsafe C++ elements, including native pointers and references. But in some cases it might be more convenient to use (or continue using) native pointers and references when their use can be verified to be safe. So "non-retargetable" (aka `const`) native pointers are treated in similar fashion to (and are in large part interchangeable with) [non-owning scope pointers](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#txscopeitemfixedpointer) by this tool, and similar restrictions are imposed. You can use `operator &` or `std::addressof()` in the usual way to initialize the value of a non-retargetable native pointer, but the tool restricts their application to expressions that it can verify are safe. These are generally limited to local (or `thread_local`) variables, objects of "scope type", or direct dereferences of a scope pointer/reference.

If you do make substantial use of native pointers, it's likely you'll encounter the need to convert between native and scope pointers. This can be done as follows:

```cpp
#include "msescope.h"

void main(int argc, char* argv[]){
	mse::mstd::string str1 = "abc"; //local variable
	auto* const non_retargetable_native_ptr1 = &str1; // scpptool can verify this is safe

	// obtaining a scope pointer from a (non-retargetable) native pointer
	auto xscope_ptr1 = mse::rsv::make_xscope_pointer_to(*non_retargetable_native_ptr1);

	// obtaining a (non-retargetable) native pointer from a scope pointer
	auto* const non_retargetable_native_ptr2 = *xscope_ptr1;
}
```

Retargetable (aka non-`const`) native pointers are not supported (at this time). You can instead use registered (or norad) pointers as shown in the the [`xscope_ifptr_to()` example](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#xscope_ifptr_to).

Native references are subject to essentially the same restrictions as non-retargetable native pointers. The functional difference between native references and non-retargetable native pointers is that C++ permits (`const`) native references to temporaries, perhaps making them a little more convenient to use as function parameters.

#### SaferCPlusPlus elements

Most of the restrictions required to ensure safety of the elements in the SaferCPlusPlus library are implemented in the type system. However, some of the necessary restrictions cannot be implemented in the type system. This tool is meant to enforce those remaining restrictions. Elements requiring enforcement help are generally relegated to the `mse::rsv` namespace. One exception is the restriction that scope types (regardless of the namespace in which they reside), cannot be used as members (or base classes) of structs/classes that are not themselves scope types. The tool currently enforces this restriction by disallowing the use scope types as members of any struct/class period. This "over-restriction" will be lifted in the future, but shouldn't be all that burdensome in the meantime, as generaly you can just use the "non-scope" version of the type instead, or use a (scope) tuple in place of the struct/class.

Note that the `mse::rsv::make_xscope_pointer_to()` function, which allows you to obtain a scope pointer to the resulting object of any eligible expression, is not listed in the documentation of the SaferCPlusPlus library, as without an enforcement helper tool like this one, it could significantly undermine safety.

#### Elements not (yet) addressed

The set of potentially unsafe elements in C++, and in the standard library itself, is pretty large. This tool does not yet address them all. In particular it does not complain about the use of essential elements for which the SaferCPlusPlus library does not (yet) provide a safe alternative, such as conatiners like maps, sets, etc.,. 

