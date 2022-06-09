
Apr 2022

### Overview

scpptool is a command line tool to help enforce a memory and data race safe subset of C++. It's designed to work with the [SaferCPlusPlus](https://github.com/duneroadrunner/SaferCPlusPlus) library. It analyzes the specified C++ file(s) and reports places in the code that it cannot verify to be safe.

Note that this tool enforces a slightly more restricted subset of reference usages than the lifetime profile checker does. So code that conforms to the subset enforced by this tool should generally be conformant[*](#third-party-lifetime-annotations) to the subset enforced by an (eventually completed) lifetime profile checker. While the extra restrictions have a cost (notably with the ability to use standard library elements), they [arguably](#flow-insensitive-analysis) also have the benefit of being easier to understand, with fewer "surprises" and less guessing about where the limits are.

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
    4. [Annotating lifetime constraints in function interfaces](#annotating-lifetime-constraints-in-function-interfaces)
    5. [SaferCPlusPlus elements](#safercplusplus-elements)
    6. [Elements not (yet) addressed](#elements-not-yet-addressed)
6. [Autotranslation](#autotranslation)
7. [Questions and comments](#questions-and-comments)

### How to Build:

On Ubuntu Linux: Download the [llvm 13 pre-built binaries](https://github.com/llvm/llvm-project/releases/tag/llvmorg-13.0.0) and extract them to a directory of your choosing. Download the [scpptool source code](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract it to a directory of your choosing. 

llvm requires some additional libraries that can be installed as follows:
```
sudo apt-get update
sudo apt-get install zlib1g-dev
sudo apt-get install libtinfo-dev
sudo apt-get install libxml2-dev
```
And on Ubuntu 19 and later, llvm 13 depends on an older version of libtinfo:
```
sudo apt-get install libtinfo5
```
scpptool also uses the `yaml-cpp` library which can be installed as follows:
```
sudo apt-get install libyaml-cpp-dev
```

From the `src` subdirectory of the scpptool directory, you can build the tool by typing:
```
make LLVM_CONF={the llvm 13 directory}/bin/llvm-config
```

Substituting the `{the llvm 13 directory}` part with the directory where you extracted llvm 13. (You can also add the `BUILD_MODE=DEBUG` option. Currently, the debug build is more extensively tested.)

### How to Use:

The syntax is as follows:

`scpptool {source filename(s)} -- {compiler options} -I'{the llvm 13 directory}/lib/clang/13.0.0/include'`

So for example:

`scpptool hello_world.cpp -- -I./msetl -std=c++17 -I'/home/user1/clang+llvm-13.0.0-x86_64-linux-gnu-ubuntu-20.04/lib/clang/13.0.0/include'`

(If you happen to have version 13 of the clang compiler installed on your system then you may be able to omit the lengthy "include directory specifier" option.)

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

#### Annotating lifetime constraints in function interfaces

The relative lifetimes of objects declared within the same function definition are directly deduced (at compile-time) by this tool from the location of their declarations. This deduction can't so readily be done with references to objects that are passed through function call boundaries (either as parameters or return values). So it can be helpful to allow the programmer to express constraints on the lifetimes of objects passed in (by reference) and returned in the function interface and have the analyzer tool verify/enforce that those constraints are observed on the sending and receiving side of the function call.

The following example demonstrates the "lifetime annotation attributes" supported by this tool. First we add attributes to the (native) reference parameters in the function declaration. (In our example it's a member function.) These attributes assign a "lifetime label" to the reference parameter. Multiple parameters can share the same lifetime label, in which case the lifetime label will represent the shortest (scope) lifetime of all the parameters associated with it. The attribute is appended to the parameter declaration and the syntax of the parameter attribute is:
```cpp
MSE_ATTR_PARAM_STR("mse::lifetime_label<#>")
```
where you would substitute the '#' with a number of your choosing. (Actually, non-numbers are also supported, but for now we recommend using numbers.) (Btw, `MSE_ATTR_PARAM_STR()` is a macro defined in the SaferCPlusPlus library.)

The remaining lifetime constraint information is incorporated into an attribute applied to the function declaration. This attribute can contain multiple elements that are each one of three kinds. One element assigns a lifetime label to the function return value (if any), one element assigns a lifetime label to the implicit `this` parameter (if any) and the other element(s) express the intended constraint(s) on those lifetimes. This attribute is appended (or prepended) to the function declaration and looks something like:
```cpp
MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<42>; this<42>; encompasses<42, 99> }")
```
where the `return_value<42>` element assigns the lifetime label "42" to the return value, the `this<42>` element assigns the lifetime label "42" to the implicit `this` parameter, and the `encompasses<42, 99>` element indicates the constraint that all elements associated with the lifetime label "42" must outlive all elements associated with the lifetime label "99". Note that in this case, because we assigned the same lifetime label to the return value and the implicit `this` parameter, this member function is be permitted to return references to data members.

example:
```cpp
#include "msescope.h"

void main(int argc, char* argv[]) {
    struct CK : mse::rsv::ContainsNonOwningScopeReferenceTagBase {
        int* good_foo1(int*& i_ptr1 MSE_ATTR_PARAM_STR("mse::lifetime_label<42>")
            , int*& i_ptr2 MSE_ATTR_PARAM_STR("mse::lifetime_label<99>")
            , int*& i_ptr3 MSE_ATTR_PARAM_STR("mse::lifetime_label<42>"))
            MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<42>; this<42>; encompasses<42, 99> }")
        {
            i_ptr2 = i_ptr3;
            int* l_i_ptr4 = i_ptr2;
            i_ptr2 = m_i_ptr1;
            return m_i_ptr1;
        }
        int* bad_foo2(int*& i_ptr1 MSE_ATTR_PARAM_STR("mse::lifetime_label<42>")
            , int*& i_ptr2 MSE_ATTR_PARAM_STR("mse::lifetime_label<99>")
            , int*& i_ptr3 MSE_ATTR_PARAM_STR("mse::lifetime_label<42>"))
            MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value<42>; this<42>; encompasses<42, 99> }")
        {
            i_ptr2 = i_ptr3;
            i_ptr3 = i_ptr2; // error, doesn't satisfy the specified lifetime constraints
            int* l_i_ptr4 = i_ptr2;
            m_i_ptr1 = i_ptr2; // error, doesn't satisfy the specified lifetime constraints
            i_ptr2 = m_i_ptr1;
            return i_ptr2; // error, doesn't satisfy the specified lifetime constraints
        }
        int m_i1 = 7;
        int* m_i_ptr1 = &m_i1;
    };
    CK k1;
    int i1 = 10;
    int i2 = 20;
    int* iptr1 = &i1;
    int* iptr2 = &i2;
    int* iptr3 = iptr2;
    iptr2 = k1.good_foo1(iptr1, iptr3, iptr1);
    iptr2 = CK().bad_foo2(iptr1, iptr3, iptr3); // errors, doesn't satisfy the specified lifetime constraints
    /* This last line violates three constraints: First, the implicit 'this' argument (associated with lifetime label 
    "42") is a short-lived temporary that doesn't outlive the second function call argument, iptr3 (associated with 
    lifetime label "99"). And also, the third argument, iptr3 (associated with lifetime label "42"), does not outlive 
    the second argument, which is also iptr3 (but associated with lifetime label "99" there). And finally, the assignment 
    of the return value to iptr2 is not permitted because the return value lifetime, which is the shortest lifetime of 
    elements associated with lifetime label "42" (which in this case is the CK() temporary expression) is too short-lived 
    to be assigned to iptr2. */
}
```

To be clear, the parameter lifetime label annotations refer to the lifetime of the object referred to directly by the (native) reference parameter, not any object referenced by that object. So for example in the parameter declaration:

```cpp
int*& i_ptr1 MSE_ATTR_PARAM_STR("mse::lifetime_label<42>")
```

The lifetime label refers to the object of type `int*`, not the `int` pointed to by that (pointer) object. By rule, we can infer that any target `int` object must outlive the `int*` object that points to it, but lifetime annotations to further constrain the lifetime of the (`int`) target of the pointer object are not yet supported.

##### third party lifetime annotations

Note that other static lifetime analyzers in development introduce their own distinct lifetime annotations (including the lifetime profile checker and [others](https://discourse.llvm.org/t/rfc-lifetime-annotations-for-c/61377)). Those analyzers may not recognize the lifetime annotations introduced here, so to be compliant with those analyzers you may have to use their lifetime annotations as well. Ideally, in the future scpptool would also support those lifetime annotations, reducing the need for redundant annotations.

#### SaferCPlusPlus elements

Most of the restrictions required to ensure safety of the elements in the SaferCPlusPlus library are implemented in the type system. However, some of the necessary restrictions cannot be implemented in the type system. This tool is meant to enforce those remaining restrictions. Elements requiring enforcement help are generally relegated to the `mse::rsv` namespace. One exception is the restriction that scope types (regardless of the namespace in which they reside), cannot be used as members of structs/classes that are not themselves scope types. The tool will flag any violations of this restriction.

Note that the `mse::rsv::make_xscope_pointer_to()` function, which allows you to obtain a scope pointer to the resulting object of any eligible expression, is not listed in the documentation of the SaferCPlusPlus library, as without an enforcement helper tool like this one, it could significantly undermine safety.

#### Elements not (yet) addressed

The set of potentially unsafe elements in C++, and in the standard library itself, is pretty large. This tool does not yet address them all. In particular it does not complain about the use of essential elements for which the SaferCPlusPlus library does not (yet) provide a safe alternative, such as conatiners like maps, sets, etc.,. 

Also `rsv::xscope_nii_vector<>` is not yet available. The other available (resizable) vectors do not support elements containing scope pointer/references, so in the meantime you'll have to use (run-time checked) [proxy pointers](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#tnoradproxypointer) (or (the unsafe) `std::vector<>`).

### Autotranslation

This tool also has some ability to convert C source files to the memory safe subset of C++ it enforces and is demonstrated in the [SaferCPlusPlus-AutoTranslation2](https://github.com/duneroadrunner/SaferCPlusPlus-AutoTranslation2) project.

### "Flow (in)sensitive" analysis

Some of the other C++ lifetime analyzers in development employ "flow sensitive" analysis. That is, they determine whether or not an operation is safe based, in part, on the operations that precede it in the program execution. So for example in this piece of code:

```cpp
int foo1(int* num_ptr) {
    int retval = 0;
    const bool b1 = !num_ptr;

    if (false) 			// condition 1
    //if ((!num_ptr)) 		// condition 2
    //if (!(!(!num_ptr))) 	// condition 3
    //if (b1) 			// condition 4
    {
        num_ptr = &retval;
    }
    retval = *num_ptr;
    return retval;
}
```

the lifetime profile checker [complains](https://godbolt.org/z/4G1Mj8x4j) that `num_ptr` is being dereferenced when it cannot verify that its value is not null:

```
<source>:12:14: warning: dereferencing a possibly null pointer [-Wlifetime-null]
    retval = *num_ptr;
             ^~~~~~~~
<source>:1:10: note: the parameter is assumed to be potentially null. Consider using gsl::not_null<>, a reference instead of a pointer or an assert() to explicitly remove null
int foo1(int* num_ptr) {
         ^~~~~~~~~~~~
1 warning generated.
```

But if we replace "condition 1" in the example with "condition 2", this will ensure that in the event `num_ptr` has a null value it will be replaced with a valid one. The lifetime profile checker recognizes this and in this case no longer complains about the possible null value.

"condition 3" is a slightly more complicated/obfuscated version of "condition 2", but the lifetime profile checker [is able](https://godbolt.org/z/76PGz6EdK) to recognize it as such.

However, "condition 4" is just an indirect version of "condition 2", but the lifetime profile checker (at the time of writing) [does not](https://godbolt.org/z/M9hzY4n4f) acknowledge this, and will again complain about the dereference of a possible null value.

While a static analyzer can get arbitrarily good at recognizing safe code, there will, in theory, always be some safe code that it won't be able to verify as such in a timely manner. (Because "halting problem", right?) So the question is whether or not a static analyzer should attempt to identify as much safe code as it can, or whether it should instead only accept a well-defined, easy to understand subset of safe code.

The advantage of maximizing the set of code accepted as safe is that it reduces the modifications of pre-existing/legacy code needed to bring it into safety conformance. The disadvantage is that it may take programmers (and code modification/generation tools) more time and effort to become familiar with the boundaries between code that will and won't be accepted as safe. (Assuming the boundary is even static.)

Currently, scpptool does not use "flow sensitive" criteria to determine whether or not code will be accepted as safe. As a result, the set of accepted code may be smaller / more restricted than those of "flow sensitive" analyzers. But perhaps also easier to understand. Pre-existing/legacy code is, to some degree, addressed via [autotranslation](https://github.com/duneroadrunner/scpptool#autotranslation).

With respect to the example given above, scpptool simply doesn't condone null (raw) pointer values at all. If null values are desired, the pointer would have to be wrapped in an "optional" container, or a smart pointer that (safely) supports null values would have to be used instead.

### Questions and comments
If you have questions or comments you can create a post in the [discussion section](https://github.com/duneroadrunner/scpptool/discussions).
