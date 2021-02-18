
Feb 2021

### Overview

scpptool is a command line tool to help enforce a memory and data race safe subset of C++. It's designed to work with the [SaferCPlusPlus](https://github.com/duneroadrunner/SaferCPlusPlus) library. It analyzes the specified C++ file(s) and reports places in the code that it cannot verify to be safe. Currently it does not report all of the instances where it cannot verify safety, but that is the eventual goal.

Note that while the aims are similar, this tool enforces a slightly more restricted subset of the language than the lifetime profile checker does. So code that conforms to the subset enforced by this tool should automatically be conformant to the subset enforced by an (eventually completed) lifetime profile checker. While the extra restrictions have a cost (notably with the ability to use standard library elements), they arguably also have the benefit of being easier to understand, with fewer "surprises" and less guessing about where the limits are.

Note that this tool is still in development and not well tested.


### How to Build:

On Ubuntu Linux: Download the [llvm 8 pre-built binaries](http://releases.llvm.org/download.html#8.0.0) and extract them to a directory of your choosing. Download the [scpptool source code](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract it to a directory of your choosing. 

llvm8 requires some additional libraries that can be installed as follows:
```
sudo apt-get update
sudo apt-get install zlib1g-dev
sudo apt-get install libtinfo-dev
sudo apt-get install libxml2-dev
```

scpptool also uses the `yaml-cpp` library which can be installed as follows:
```
sudo apt-get install libyaml-cpp-dev
```

From the `src` subdirectory of the scpptool directory, you can build the tool by typing:
```
make LLVM_CONF={the llvm8 directory}/bin/llvm-config
```

Substituting the `{the llvm8 directory}` part with the directory where you extracted llvm 8.

### How to Use:

The syntax is as follows:

`scpptool {source filename(s)} -- {compiler options} -I'{the llvm8 directory}/lib/clang/8.0.0/include'`

So for example:

`scpptool hello_world.cpp -- -I./msetl -std=c++17 -I'/home/user1/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04/lib/clang/8.0.0/include'`


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

So for example, this tool would not condone directly taking the address of an element in a (resizable) vector. In order to obtain a native pointer to a vector element, you would first need to obtain a scope pointer to the element (which in turn [requires obtaining a scope iterator](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#xscope_borrowing_fixed_nii_vector) to the element). Converting between native and scope pointers can be done as follows:

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

### Autotranslation

This tool also has some ability to convert C source files to the memory safe subset of C++ it enforces and is demonstrated in the [SaferCPlusPlus-AutoTranslation2](https://github.com/duneroadrunner/SaferCPlusPlus-AutoTranslation2) project.

### Questions and comments
If you have questions or comments you can create a post in the [discussion section](https://github.com/duneroadrunner/scpptool/discussions).
