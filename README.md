
Dec 2021

### Overview

scpptool is a command line tool to help enforce a memory and data race safe subset of C++. It's designed to work with the [SaferCPlusPlus](https://github.com/duneroadrunner/SaferCPlusPlus) library. It analyzes the specified C++ file(s) and reports places in the code that it cannot verify to be safe. Currently it does not report all of the instances where it cannot verify safety, but that is the eventual goal.

Note that this tool enforces a slightly more restricted subset of reference usages than the lifetime profile checker does. So code that conforms to the subset enforced by this tool should automatically be conformant to the subset enforced by an (eventually completed) lifetime profile checker. While the extra restrictions have a cost (notably with the ability to use standard library elements), they arguably also have the benefit of being easier to understand, with fewer "surprises" and less guessing about where the limits are.

Note that this tool is still in development and not well tested.

### Table of contents
1. [Overview](#overview)
2. [How to Build](#how-to-build)
3. [How to Use](#how-to-use)
4. [Local Suppression of the Checks](#local-suppression-of-the-checks)
5. [About the Enforced Subset](#about-the-enforced-subset)
    1. [Restrictions on the use of native pointers and references](#restrictions-on-the-use-of-native-pointers-and-references)
    2. [Range-based `for` loops by reference](#range-based-for-loops-by-reference)
    3. [Lambda captures](#lambda-captures)
    4. [SaferCPlusPlus elements](#safercplusplus-elements)
    5. [Elements not (yet) addressed](#elements-not-yet-addressed)
6. [Autotranslation](#autotranslation)
7. [Questions and comments](#questions-and-comments)

### How to Build:

On Ubuntu Linux: Download the [llvm 10 pre-built binaries](https://releases.llvm.org/download.html#10.0.0) and extract them to a directory of your choosing. Download the [scpptool source code](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract it to a directory of your choosing. 

llvm requires some additional libraries that can be installed as follows:
```
sudo apt-get update
sudo apt-get install zlib1g-dev
sudo apt-get install libtinfo-dev
sudo apt-get install libxml2-dev
```
And on Ubuntu 19 and later, llvm10 depends on an older version of libtinfo:
```
sudo apt-get install libtinfo5
```
scpptool also uses the `yaml-cpp` library which can be installed as follows:
```
sudo apt-get install libyaml-cpp-dev
```

From the `src` subdirectory of the scpptool directory, you can build the tool by typing:
```
make LLVM_CONF={the llvm10 directory}/bin/llvm-config
```

Substituting the `{the llvm10 directory}` part with the directory where you extracted llvm 10. (You can also add the `BUILD_MODE=DEBUG` option. Currently, the debug build is more extensively tested.)

### How to Use:

The syntax is as follows:

`scpptool {source filename(s)} -- {compiler options} -I'{the llvm10 directory}/lib/clang/10.0.0/include'`

So for example:

`scpptool hello_world.cpp -- -I./msetl -std=c++17 -I'/home/user1/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/lib/clang/10.0.0/include'`

(If you happen to have version 10 of the clang compiler installed on your system then you may be able to omit the lengthy "include directory specifier" option.)

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

The SaferCPlusPlus library is designed to enable you to avoid potentially unsafe C++ elements, including native pointers and references. But in some cases it might be more convenient to use (or continue using) native pointers and references when their use can be verified to be safe. So native pointers (including `this` pointers) are treated in similar fashion to (and are in large part interchangeable with) [non-owning scope pointers](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#txscopeitemfixedpointer) by this tool, and similar restrictions are imposed. You can use `operator &` or `std::addressof()` in the usual way to initialize the value of a native pointer, but the tool restricts their application to expressions that it can verify are safe. These are generally limited to local (or `thread_local`) variables, objects of "scope type", or direct dereferences of a scope pointer/reference.

So for example, this tool would not condone directly taking the address of an element in a (resizable) vector. In order to obtain a native pointer to a vector element, you would first need to obtain a scope pointer to the element (which in turn [requires obtaining](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#vectors) a [scope iterator](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#xscope_iterator) to the element). Converting between native and scope pointers can be done as follows:

```cpp
#include "msescope.h"
#include "msemstdstring.h"

void main(int argc, char* argv[]){
	mse::mstd::string str1 = "abc"; //local variable
	auto* const non_retargetable_native_ptr1 = &str1; // scpptool can verify this is safe

	// obtaining a scope pointer from a native pointer
	auto xscope_ptr1 = mse::rsv::make_xscope_pointer_to(*non_retargetable_native_ptr1);

	// obtaining a native pointer from a scope pointer
	auto* const non_retargetable_native_ptr2 = std::addressof(*xscope_ptr1);
}
```

Note that scope pointers can be "disabled" in the SaferCPlusPlus library by defining the `MSE_SCOPEPOINTER_DISABLED` preprocessor symbol. This causes the library to simply use scope pointers as an alias of native pointers. In this usage mode, converting between scope and native pointers would not be required.

The expressions used to assign a new value to a retargetable (aka non-`const`) pointer (subsequent to initialization) are even more restricted. The tool requires that the target object must outlive the pointer (regardless of how long it will remain the target of the pointer), so the tool only supports (a subset of) expressions for which this is readily apparent (at compile-time). When you need more flexibility, you can instead use registered (or norad) pointers as [demonstrated](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#tregisteredproxypointer) in the SaferCPlusPlus documentation.

Native references are subject to essentially the same restrictions as non-retargetable native pointers. The functional difference between native references and non-retargetable native pointers is that C++ permits (`const`) native references to temporaries, perhaps making them a little more convenient to use as function parameters.

So just as this tool wouldn't condone directly taking the address of an element of a (resizable) vector, it wouldn't condone directly creating a reference to an element of a vector either. This means you cannot directly pass a vector element by reference to a function. As with pointers, you would first need to obtain a scope pointer to the element, then you could pass the element (by reference) as a dereference of the scope pointer.

#### Range-based `for` loops by reference

Another common use case for native references that this tool can't generally verify to be safe is range-based `for` loops. So for example in this code:

```cpp
#include "msemstdvector.h"

void main(int argc, char* argv[]) {
    mse::mstd::vector<int> vec1{1, 2, 3};
    for (auto& item : vec1) {
        //vec1.clear();
        item += 5;
    }
}
```

the native reference `item` would be flagged as "not verifiably safe". You could instead use [`mse::for_each_ptr()`](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#for_each_ptr) like so

```cpp
#include "msemstdvector.h"
#include "msealgorithm.h"

void main(int argc, char* argv[]) {
    mse::mstd::vector<int> vec1{1, 2, 3};
    mse::for_each_ptr(vec1.begin(), vec1.end(), [](auto item_ptr){ *item_ptr += 5; });
}
```

or the "range-based" version

```cpp
#include "msemstdvector.h"
#include "msealgorithm.h"
#include "msescope.h"

void main(int argc, char* argv[]) {
    auto xscope_vec1 = mse::make_xscope(mse::mstd::vector<int>{1, 2, 3});
    mse::xscope_range_for_each_ptr(&xscope_vec1, [](auto item_ptr){ *item_ptr += 5; });
}
```

#### Lambda captures

Just as a class or struct that contains fields of scope type, [must itself be a scope type](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#defining-your-own-scope-types), a lambda expression with captures of scope type (including native references) must itself be a "scope lambda". Scope lambdas are created using one of:
`mse::rsv::make_xscope_reference_or_pointer_capture_lambda()`  
`mse::rsv::make_xscope_non_reference_or_pointer_capture_lambda()`  
`mse::rsv::make_xscope_non_capture_lambda()`  
(This tool will flag attempts to use one not appropriate for the given lambda.)

example:
```cpp
#include "msescope.h"

void main(int argc, char* argv[]) {
    int i1 = 5;
    auto xscope_lambda1 = mse::rsv::make_xscope_reference_or_pointer_capture_lambda([&] { return i1; });
    int i2 = xscope_lambda1();
}
```

#### SaferCPlusPlus elements

Most of the restrictions required to ensure safety of the elements in the SaferCPlusPlus library are implemented in the type system. However, some of the necessary restrictions cannot be implemented in the type system. This tool is meant to enforce those remaining restrictions. Elements requiring enforcement help are generally relegated to the `mse::rsv` namespace. One exception is the restriction that scope types (regardless of the namespace in which they reside), cannot be used as members of structs/classes that are not themselves scope types. The tool will flag any violations of this restriction.

Note that the `mse::rsv::make_xscope_pointer_to()` function, which allows you to obtain a scope pointer to the resulting object of any eligible expression, is not listed in the documentation of the SaferCPlusPlus library, as without an enforcement helper tool like this one, it could significantly undermine safety.

#### Elements not (yet) addressed

The set of potentially unsafe elements in C++, and in the standard library itself, is pretty large. This tool does not yet address them all. In particular it does not complain about the use of essential elements for which the SaferCPlusPlus library does not (yet) provide a safe alternative, such as conatiners like maps, sets, etc.,. 

#### Encoding an object's lifetime information in its type

The information needed to compare (at compile-time) the lifetimes of two (simultaneously existing) local variables can be separated into two categories: i) The relative lifetimes (or equivalently, the declaration locations) of the variables within the function in which they are declared, if indeed they are both declared in the same function. And ii) the relative lifetimes of the function calls in which they are instantiated, if they are instead instantiated in different function call instances.

The SaferCPlusPlus library already has a mechanism for addressing the latter category. When a pointer/reference object of concern is received as a function parameter (and its lifetime information is relevant), rather than being accessed directly, it is used to [create an](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#as_a_returnable_fparam) [equivalent object](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#as_an_fparam) of a compatible, but slightly different, type. The new type is the same as the original parameter type, but wrapped in a "transparent template wrapper" that indicates that it has been received as function parameter. An object that is passed down through multiple nested function calls will end up being wrapped with the multiple corresponding transparent template wrappers. (Note that the functions need to be function templates so that they can accept a range of different parameter types expressing a range of different object lifetimes.)

Also correspondingly, any of those pointer/reference objects used as a return value will have the added template wrapper removed during the return operation. These type modifications are verified/enforced by this scpptool. Currently they need to invoked manually by the programmer, but presumably in the future this tool will be able to add them automatically.

On the other hand, the mechanism for encoding the first category of lifetime information (that is, lifetimes of objects declared within the same function,) is not yet completed. Within the function in which the objects are declared, the encoding of lifetime information (in their types) is arguably not necessary as the static analyzer can just directly infer the lifetimes from the declaration location of the objects. But (relative) lifetime information may need to be explicitly indicated in cases when object references are passed to a function.

One option is to just somehow indicate the relative lifetimes (or their intended constraints) of the passed objects in the function interface. For example, the function parameter types could indicate the (intended) relative lifetimes of the parameters like so:

```cpp
void foo1(param_t<int*, 1> long_lived_pointer, param_t<int*, 3> short_lived_pointer, param_t<int*, 2> medium_lived_pointer);
```

Or perhaps less intrusively, the relative lifetimes could even be indicated by separate parameter declarations:

```cpp
void foo1(int* long_lived_pointer, int* short_lived_pointer, int* medium_lived_pointer, relative_param_lifetime_order<1, 3, 2>= {});
```

But another option is to encode the (post-preprocessing) declaration location of every local variable of concern into the variable's declared type. It seems that this could be done fairly automatically using macros and `std::source_location`. This option has its thorns (restrictions on copy construction, type deduction, ...) but there are arguably some benefits. Arguably. First, since the lifetime information is available right from the declaration, a lot of the lifetime safety enforcement can be done in the type system rather than relying on the static analysis tool. Also, lifetime information can be automatically passed with the arguments to a function (template) without the programmer having to make any explicit lieftime annotations in the function (template) interface.

At this point, this latter option is perhaps most likely to be implemented. But the options aren't mutually incompatible, and the implementation of one does not necessarily exclude the future availability of the other(s).

But until any of the options are fully implemented, you can instead use [run-time checked](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#tnoradproxypointer) pointers (which would be expected to often have their run-time checks discarded by the compiler optimizer).

### Autotranslation

This tool also has some ability to convert C source files to the memory safe subset of C++ it enforces and is demonstrated in the [SaferCPlusPlus-AutoTranslation2](https://github.com/duneroadrunner/SaferCPlusPlus-AutoTranslation2) project.

### Questions and comments
If you have questions or comments you can create a post in the [discussion section](https://github.com/duneroadrunner/scpptool/discussions).
