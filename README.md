
Mar 2023

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
    4. [Annotating lifetime constraints](#annotating-lifetime-constraints)
    5. [SaferCPlusPlus elements](#safercplusplus-elements)
    6. [Elements not (yet) addressed](#elements-not-yet-addressed)
6. [Autotranslation](#autotranslation)
7. [Questions and comments](#questions-and-comments)

### How to Build:

On Ubuntu Linux: Download the [llvm 15.0.6 pre-built binaries](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.6) and extract them to a directory of your choosing. Download the [scpptool source code](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract it to a directory of your choosing. 

llvm requires some additional libraries that can be installed as follows:
```
sudo apt-get update
sudo apt-get install zlib1g-dev
sudo apt-get install libtinfo-dev
sudo apt-get install libxml2-dev
```
And on Ubuntu 19 and later, llvm 15 depends on an older version of libtinfo:
```
sudo apt-get install libtinfo5
```
scpptool also uses the `yaml-cpp` library which can be installed as follows:
```
sudo apt-get install libyaml-cpp-dev
```

From the `src` subdirectory of the scpptool directory, you can build the tool by typing:
```
make LLVM_CONF={the llvm 15 directory}/bin/llvm-config
```

Substituting the `{the llvm 15 directory}` part with the directory where you extracted llvm 15. (You can also add the `BUILD_MODE=DEBUG` option. Currently, the debug build is more extensively tested.)

### How to Use:

The syntax is as follows:

`scpptool {source filename(s)} -- {compiler options} -I'{the llvm 15 directory}/lib/clang/15.0.6/include'`

So for example:

`scpptool hello_world.cpp -- -I./msetl -std=c++17 -I'/home/user1/clang+llvm-15.0.6-x86_64-linux-gnu-ubuntu-18.04/lib/clang/15.0.6/include'`

(If you happen to have version 15 of the clang compiler installed on your system then you may be able to omit the lengthy "include directory specifier" option.)

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

#### Annotating lifetime constraints

[*provisional*]

By default, this tool enforces that targets of scope (raw) pointers outlive the pointer itself. But sometimes it can be useful to enforce even more stringent restrictions on the lifespan of the target objects. Consider the following example:

```cpp
typedef int* int_ptr_t;

void foo1(int_ptr_t& i1_ptr_ref, int_ptr_t& i2_ptr_ref) {
	int_ptr_t i3_ptr = i1_ptr_ref; // clearly safe
	i2_ptr_ref = i1_ptr_ref; // ???
}
```

We (and the tool) can see that it is safe to assign the value of the `i1_ptr_ref` parameter to the `i3_ptr` local variable, because the target object of the pointer referred to by `i1_ptr_ref` comes from outside the function and can be assumed to outlive the function call itself and therefore any local variable within the function.

But what about assigning the value of `i1_ptr_ref` to `i2_ptr_ref`? In this case we (and the tool) don't have enough information to conclude that the target of the pointer referred to by `i1_ptr_ref` would outlive the pointer that `i2_ptr_ref` refers to.

##### Annotating function interfaces

Now imagine we had some way to specify in the function interface that the pointer referred to by `i1_ptr_ref`, and therefore its target object, must live at least as long as the pointer that `i2_ptr_ref` refers to.

The tool supports such a specification (referred to as "lifetime annotations") and might look something like this:

```cpp
typedef int* int_ptr_t;

void foo1(int_ptr_t& i1_ptr_ref MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"), int_ptr_t& i2_ptr_ref MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"))
MSE_ATTR_FUNC_STR("mse::lifetime_notes{ labels(42, 99); encompasses(42, 99) }")
{
    int_ptr_t i3_ptr = i1_ptr_ref; // clearly safe
    i2_ptr_ref = i1_ptr_ref; // the lifetime annotations tell us that this is safe
}
```

First note that the `42` and `99` are just an arbitrarily chosen labels used to distinguish between the lifetimes of the two parameters. So lets go through the "annotations" we added:

After the first parameter we added `MSE_ATTR_PARAM_STR("mse::lifetime_label(42)")`. ` MSE_ATTR_PARAM_STR()` is just a (preprocessor) macro function defined in the SaferCPlusPlus library that lets us add these annotations in such a way that the tool can read them, but they don't bother the compiler. The `"mse::lifetime_label(42)"` just associates a label (of our choosing) to the lifespan of the object bound to the (raw) reference first parameter. So we've assigned the labels `42` and `99` to the lifespans of objects bound to the two (raw) reference parameters.

After the function declaration (and before the body of the function), we added the annotation `MSE_ATTR_FUNC_STR("mse::lifetime_notes{ labels(42, 99); encompasses(42, 99) }")`. `mse::lifetime_notes{}` is used as a sometimes more compact way of expressing multiple annotations separated by semicolons, and without the `mse::lifetime_` prefix on each one. So for example, in place of this annotation we could have instead wrote `MSE_ATTR_FUNC_STR("mse::lifetime_labels(42, 99)") MSE_ATTR_FUNC_STR("mse::lifetime_encompasses(42, 99)")`, which is equivalent (and might even be preferred in cases where you want to put each annotation on its own line).

Ok, so the `labels(42, 99)` annotation is just the declaration of the lifetime labels, akin to declaring variables before use. Though here we place the declarations below the place where they are used, but that's just an asthetic choice on our part. You can place them before the function declaration if you prefer. (Note that at the time of writing the tool actually allows you to use lifetime labels without declaring them separately, but is expected to be more strict in the future.)

`encompasses(42, 99)` declares a constraint on the two lifespans. Namely that the `99` lifespan must be contained within the duration of the `42` lifespan. Or, essentially, that the object associated with the `42` lifespan must outlive the object associated with the `99` lifespan.

The tool will analyze every call of the `foo1()` function and complain if it cannot verify that the function call arguments satisfy the specified constraint. For example:

```cpp
#include "msescope.h"

typedef int* int_ptr_t;

void foo1(int_ptr_t& i1_ptr_ref MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"), int_ptr_t& i2_ptr_ref MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"))
MSE_ATTR_FUNC_STR("mse::lifetime_notes{ labels(42, 99); encompasses(42, 99) }")
{
    int_ptr_t i3_ptr = i1_ptr_ref; // clearly safe
    i2_ptr_ref = i1_ptr_ref; // the lifetime annotations tell us that this is safe
}

void main(int argc, char* argv[]) {
    int i1 = 5;
    int* i_ptr1 = &i1;
    {
        int i2 = 7;
        int* i_ptr2 = &i2;
        
        foo1(i_ptr1, i_ptr2); // fine because i_ptr1 outlives i_ptr2
        
        foo1(i_ptr2, i_ptr1); // scpptool will complain because the first argument does not outlive the second
    }
}
```

Ok, so we've demonstrated associating labels to the lifetimes of objects bound to raw references. But actually, raw references are kind of a special case "quasi-object" in the sense that the reference itself can never be the target of another reference or pointer, and can never be reassigned to reference a different object. (Raw) pointers, on the other hand, provide the functionality of raw references, but additionally can be reassigned to reference (aka "point to") different objects, and can themselves be targeted by references or other pointers. So if we use pointers in place of (raw) references in our first example:

```cpp
typedef int* int_ptr_t;

void foo2(int_ptr_t* i1_ptr_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"), int_ptr_t* i2_ptr_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"))
MSE_ATTR_FUNC_STR("mse::lifetime_notes{ labels(42, 99); encompasses(42, 99) }")
{
    int_ptr_t i3_ptr = *i1_ptr_ptr; // clearly safe
    *i2_ptr_ptr = *i1_ptr_ptr; // the lifetime annotations tell us that this is safe
}
```

It works the same way. Note that the lifetime labels refer to the lifetimes of the targets of the pointer parameters (which also happen to be pointers in this case), not the lifetime of the pointer parameters themselves.

But pointers can point to different objects during the execution of the program. Does this mean that lifetime labels associated with the target of a pointer can refer to different lifetimes at different points in the execution of a program?

No. Lifetime labels actually represent "the maximum known lower bound" lifespan of any referenced objects. By "known" we mean known when the pointer or reference object is declared (at compile-time). The lower bound is determined and set from either the initialization value, specified "constraint" annotations, or a default value based on the location of the declaration in the code. You might think of lifetime labels as sort of (deduced) template parameters (that can apply to types that aren't considered templates in traditional C++).

So in the above example, the "maximum known lower bound" lifespan value of the `42` lifespan is determined, from the declaration, to be the `99` lifespan (as specified in the `encompasses()` annotation). The "maximum known lower bound" lifespan value of the `99` lifespan is determined to be the default one, which in this case is the lifespan of the function call.

So we've seen lifetime labels associated with (raw) references and (raw) pointers, when used as function parameters. But lifetime labels can be associated with other types of reference objects, and not just when used as function parameters.

When not used as function parameters, reference objects may (be required to) have an initialization value, in which case any associated lifetime labels may have their "maximum known lower bound" lifespan value determined to be a lifespan of the initialization value object.

##### Annotating (user-defined) types

By "reference object" we mean basically any object that references (ultimately via pointer) any other object(s). A simple example would be just a `struct` that has a pointer member. So lets look at an example of a couple of `struct`s with a pointer member, one with and one without lifetime annotation:

```cpp
#include "msescope.h"

struct CRefObj1 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CRefObj1(int* i_ptr) : m_i_ptr(i_ptr) {}

    int* m_i_ptr;
};

struct CLARefObj1 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CLARefObj1(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)")) : m_i_ptr(i_ptr) {}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
} MSE_ATTR_STR("mse::lifetime_label(99)");

void main(int argc, char* argv[]) {
    int i1 = 5;
    int* i_ptr1 = &i1;

    {
        int i2 = 7;
        int* i_ptr2 = &i2;
        
        CRefObj1 ro2{ i_ptr2 };
        CLARefObj1 laro2{ i_ptr2 };

        {
            CRefObj1 ro3{ i_ptr1 };
            CLARefObj1 laro3{ i_ptr1 };

            ro2.m_i_ptr = ro3.m_i_ptr; // scpptool will complain because ro2.m_i_ptr outlives ro3.m_i_ptr

            laro2.m_i_ptr = laro3.m_i_ptr; // fine
            /* because the lower bound lifespan of the target of laro3.m_i_ptr1 was set (in the construction 
            of laro3) to be the lifespan of i_ptr1 (i.e. the construction argument), and the lower bound
            lifespan of laro2.m_i_ptr was set to be the lifespan of i_ptr2, and i_ptr1 outlives i_ptr2 */

            ro3.m_i_ptr = i_ptr2; // fine, i_ptr2 outlives ro3.m_i_ptr

            laro3.m_i_ptr = i_ptr2; // scpptool will complain
            /* because i_ptr2 does not outlive the lower bound lifespan of the target of laro3.m_i_ptr1 
            (which was set to i_ptr1) */
        }
    }
}
```

(Recall that currently the tool enforces that `class`/`struct`s that contain [scope reference objects](https://github.com/duneroadrunner/SaferCPlusPlus#scope-pointers), like raw pointers, must inherit from the [`mse::rsv::XScopeTagBase` and `mse::rsv::ContainsNonOwningScopeReferenceTagBase`](https://github.com/duneroadrunner/SaferCPlusPlus#defining-your-own-scope-types) (empty) base classes. It doesn't help the tool, but it facilitates doing as much of the lifetime safety enforcement as practical in the type system.)

So you can see the different restrictions on which objects the member pointers can point to, and how the tool uses those restrictions to determine which assignment operations it can verify to be safe.

So lets walk through the application of lifetime annotations to the `CLARefObj1` `struct` and its pointer member. The declaration of the pointer member gets an annotation in similar fashion to the function parameter declarations in our previous examples. We also use the same lifetime label annotation on the `struct` itself to "declare" the lifetime label, just like with functions. Note that (for now at least) the tool requires any `struct` with lifetime annotation to define an (annotated) constructor (from which it can infer the lifetime values associated with the lifetime labels). So in the `CLARefObj1` `struct`, the (maximum lower bound) lifetime value associated with lifetime label `99`, and the pointer member, is inferred from the constructor parameter associated with lifetime label `99`.

##### Annotating return values and `this` pointers

Ok, but if we want to use our annotated `CLARefObj1` type as a reference type, you could image we might want to provide, for example, member operators like `operator*()` and `operator->()`. Lets see how we would do that:

```cpp
struct CLARefObj1 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CLARefObj1(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)")) : m_i_ptr(i_ptr) {}

	int& operator*() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(99) }") {
		return *m_i_ptr;
	}
	int* operator->() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(99) }") {
		return m_i_ptr;
	}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
} MSE_ATTR_STR("mse::lifetime_label(99)");
```

Operators are just like any other functions and annotated in the same way. Our previous example functions didn't have return values, so we didn't get a chance to see how to annotate the return value. This example shows it. It seems these operators don't take any parameters so we don't have to deal with them here. But to be pedantic, since these are member operators, just like member functions, they actually take an implicit `this` pointer parameter. In some cases, you might need to associate a lifetime label to the implicit `this` pointer parameter. This would be done in similar fashion to the return value annotation above, but substituting the `return_value()` part with `this()`. But understand that the `this()` annotation is just associating a lifetime label to a function parameter (in this case the implicit `this` pointer parameter), and so the (maximum lower bound) lifespan value will be inferred from the parameter, whereas the `return_value()` annotation, on the other hand, is imposing a (lower bound) lifespan value associated with a lifetime label that has already been previously inferred (often from one of the (implicit or explicit) function parameters).

By adding dereference operators, we've made a reference object that kind of resembles the behavior of a pointer. But notice that, unlike the native pointer and reference types, the target of our reference object type is always constrained by the lower bound lifespan inferred from its initialization value (aka constructor argument). With native pointers and references, we have to associate the target object's lifespan with a lifetime label (by adding an annotation to the (parameter) variable or member field) in order to trigger this constraint, whereas a variable or member field of our reference object type will always have this constraint regardless. Native pointers and references are the only types that possess this "dual nature". With all other (user defined) reference object types it's either one or the other.

So currently our reference object stores one pointer, but what if we wanted it to store two different pointers to two different objects with different lifetime (lower bound) constraints?

```cpp
struct CLARefObj2 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CLARefObj2(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"), float* fl_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"))
        : m_i_ptr(i_ptr), m_fl_ptr(fl_ptr) {}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
    float* m_fl_ptr MSE_ATTR_STR("mse::lifetime_label(42)");
} MSE_ATTR_STR("mse::lifetime_labels(99, 42)");
```

A reference object can have more than one lifetime label associated with it.

Ok let's say, instead of dereference operators, we want to add some member functions that return the value of each pointer member field:

```cpp
struct CLARefObj2 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CLARefObj2(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"), float* fl_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"))
        : m_i_ptr(i_ptr), m_fl_ptr(fl_ptr) {}

	int* first() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(99) }") {
		return m_i_ptr;
	}
	float* second() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(42) }") {
		return m_fl_ptr;
	}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
    float* m_fl_ptr MSE_ATTR_STR("mse::lifetime_label(42)");
} MSE_ATTR_STR("mse::lifetime_labels(99, 42)");
```

##### Annotating template parameters

Ok, now let's say that instead of the member fields being of type `int*` and `float*`, we want those types to be generic template parameters. That's a little trickier. Because we know that pointers like `int*` and `float*` each have (at most) one reference lifetime to which a lifetime label can be associated. But if a type is a generic template parameter then we wouldn't know in advance how many, if any, lifetime labels can be associated with it. In this case we'll use a generic "lifetime label alias" that maps to the set of (reference) lifetimes the template parameter type has (when the template is instantiated).

```cpp
template<typename T, typename U>
struct TLARefObj2 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    TLARefObj2(T val1 MSE_ATTR_PARAM_STR("mse::lifetime_label(alias_11$)")
        , U val2 MSE_ATTR_PARAM_STR("mse::lifetime_label(alias_12$)"))
        : m_val1(val1), m_val2(val2) {}

	T first() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(alias_11$) }") {
		return m_val1;
	}
	U second() const MSE_ATTR_FUNC_STR("mse::lifetime_notes{ return_value(alias_12$) }") {
		return m_val2;
	}

    T m_val1 MSE_ATTR_STR("mse::lifetime_label(alias_11$)");
    U m_val2 MSE_ATTR_STR("mse::lifetime_label(alias_12$)");
} MSE_ATTR_STR("mse::lifetime_set_alias_from_template_parameter_by_name(T, alias_11$)")
 MSE_ATTR_STR("mse::lifetime_set_alias_from_template_parameter_by_name(U, alias_12$)")
 MSE_ATTR_STR("mse::lifetime_labels(alias_11$, alias_12$)");
```

We use the `mse::lifetime_set_alias_from_template_parameter_by_name()` annotation to define a lifetime label alias for the set of (reference) lifetimes the specified template parameter type has (or rather, will have whenever the template is instantiated).

##### Accessing sublifetimes

Now, if we can revisit the earlier part where we were learning to associate lifetime labels with function parameters, and consider a situation where we are interested in, not the lifetime of the parameter directly, but perhaps the (maximum known lower bound) lifespan of an object that the parameter references. We can use the `CLARefObj2` `struct` we defined earlier for this example:

```cpp
struct CLARefObj2 : public mse::rsv::XScopeTagBase, public mse::rsv::ContainsNonOwningScopeReferenceTagBase {
    CLARefObj2(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"), float* fl_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"))
        : m_i_ptr(i_ptr), m_fl_ptr(fl_ptr) {}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
    float* m_fl_ptr MSE_ATTR_STR("mse::lifetime_label(42)");
} MSE_ATTR_STR("mse::lifetime_labels(99, 42)");

float* foo2(const CLARefObj2& la_ref_obj_cref MSE_ATTR_PARAM_STR("mse::lifetime_labels(42 [421, 422])"))
MSE_ATTR_FUNC_STR("mse::lifetime_notes{ labels(42, 421, 422); return_value(422) }")
{
    return la_ref_obj_cref.m_fl_ptr;
}
```

Notice the annotation for the `foo2()` function's parameter, `mse::lifetime_labels(42 [421, 422])`. In this case, label `42` is associated with the lifespan of the object (of type `CRefObj2`) referenced by the native reference argument, label `421` is associated with the lifespan of the first object referenced by that object (aka the object's first "sublifetime"), and label `422` is associated with the lifespan of the second object (of type `float` in this case) referenced by that object. See, in the `mse::lifetime_labels()` annotation we can use commas and (possibly nested) square brackets to create a tree of lifetime labels that correspond to the tree of lifespan values of the argument object.

##### Future syntax options

This syntax for addressing sublifetimes might be considered a little messy (and maybe error prone), but results from the fact that, in the source text, our annotations are placed after the declarations rather than the directly after the types they might correspond to. This is, in part, an artifact of a historical limitation in one of the libraries the tool uses. In the future the tool may support placing the lifetime label annotations directly after the type.

##### More lifetime constraints

In the [first example](#annotating-function-interfaces) we saw a straightforward use of the `encompasses` lifetime constraint. Now let's look at a slightly more advanced example where we demonstrate two ways of annotating a `swap` function:

```cpp
template<typename T>
void swap1(T& item1 MSE_ATTR_PARAM_STR("mse::lifetime_label(42 [alias_421$])"), T& item2 MSE_ATTR_PARAM_STR("mse::lifetime_label(99 [alias_991$])"))
	MSE_ATTR_FUNC_STR("mse::lifetime_set_alias_from_template_parameter_by_name(T, alias_421$)")
	MSE_ATTR_FUNC_STR("mse::lifetime_set_alias_from_template_parameter_by_name(T, alias_991$)")
	MSE_ATTR_FUNC_STR("mse::lifetime_labels(42, 99, alias_421$, alias_991$)")
	MSE_ATTR_FUNC_STR("mse::lifetime_notes{ encompasses(alias_421$, alias_991$); encompasses(alias_991$, alias_421$) }")
{
	MSE_SUPPRESS_CHECK_IN_XSCOPE std::swap(item1, item2);
}

template<typename T>
void swap2(T& item1 MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"), T& item2 MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"))
	MSE_ATTR_FUNC_STR("mse::lifetime_labels(42, 99)")
	MSE_ATTR_FUNC_STR("mse::lifetime_notes{ first_can_be_assigned_to_second(42, 99); first_can_be_assigned_to_second(99, 42) }")
{
	MSE_SUPPRESS_CHECK_IN_XSCOPE std::swap(item1, item2);
}
```

Note that the `swap` functions take (raw) reference parameters. Determining whether or not the argument values are safe to swap does not depend on the "direct" lifetimes of the arguments, but rather on the (maximum lower bound) lifetimes of any objects that the arguments might refer to (aka the "sublifetimes), right? So for example, swapping the value of two `int`s is always safe because `int`s don't hold any references to any other objects (i.e. `int`s have no "sublifetimes"). Swapping the value of two pointers on the otherhand is not always safe. We can ensure that swapping the value of two pointers is safe if we can ensure that the (maximum lower bound) lifetimes of the objects the pointers reference (aka the "sublifetimes") are the same.

So in our annotation of the `swap1()` function we assign "lifetime set alias" labels to the sublifetimes (if any) of the arguments. Then we use the `encompasses` constraint to to ensure that none of the (maximum lower bound) sublifetimes of one argument outlives the corresponding (maximum lower bound) sublifetime of the other.

In the annotation of the `swap2()` function, we introduce the use of the `first_can_be_assigned_to_second` constraint to make the annotation a little cleaner. The `first_can_be_assigned_to_second` constraint is similar to the `encompasses` constraint except that it ignores the "direct" (maximum lower bound) lifetime and only constrains the sublifetimes (if any). Using the `first_can_be_assigned_to_second` constraint eliminates the need to assign lifetime (set alias) labels to the argument sublifetimes.

##### Annotating base classes

Just to mention it, base classes are, in terms of lifetime annotations, conceptually treated just like member fields. You can use the `labels_for_base_class()` annotation (on the derived type) to assign lifetime labels to the first base class.

##### Lifetime annotation implementation caveats

(Note, that at the time of writing, implementation of lifetime safety enforcement is not complete. Safety can be subverted through, for example, cyclic references with user-defined destructors, etc.. Also note that the process of adding lifetime annotated elements to the SaferCPlusPlus library is still in the early stages.)

##### third party lifetime annotations

Note that other static lifetime analyzers in development introduce their own distinct lifetime annotations (including the lifetime profile checker and [others](https://discourse.llvm.org/t/rfc-lifetime-annotations-for-c/61377)). Those analyzers may not recognize the lifetime annotations introduced here, so to be compliant with those analyzers you may have to use their lifetime annotations as well. Ideally, in the future scpptool would also support those lifetime annotations, reducing the need for redundant annotations.

##### Lifetime annotated elements in the SaferCPlusPlus library

The process of adding lifetime annotated elements to the SaferCPlusPlus library is still in the early stages.

Since lifetime annotations require the scpptool for enforcement, lifetime annotated elements generally reside in the `mse::rsv` namespace, like the other elements that require scpptool for safety enforcement. Most lifetime annotated elements are "[scope](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#scope-pointers)" elements and conform to the corresponding restrictions. 

##### rsv::TXSLTAPointer

`rsv::TXSLTAPointer<>` is just a class that acts like a (lifetime annotated) pointer.

usage example:

```cpp
    #include "mseslta.h"
    
    void main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        /* The (lower bound) lifetime associated with an rsv::TXSLTAPointer<> is set to the lifetime of 
        its initialization value. */
        auto ilaptr1 = mse::rsv::TXSLTAPointer<int>{ &i1 };
        auto ilaptr2 = mse::rsv::TXSLTAPointer<int>{ &i2 };
        
        /* The (lower bound) lifetime associated with ilaptr2 does not outlive the one associated with 
        ilaptr1, so assigning the value of ilaptr2 to ilaptr1 cannot be verified to be safe. */
        //ilaptr1 = ilaptr2;    // scpptool would complain
        ilaptr2 = ilaptr1;
        ilaptr2 = &i1;
        //ilaptr2 = &i3;    // scpptool would complain
    }
```

##### rsv::xslta_nii_array

`rsv::xslta_nii_array<>` is just the lifetime annotated version of [`xscope_nii_array<>`](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#xscope_nii_array).

usage example:

```cpp
    #include "mseslta.h"
    #include "msemsearray.h"
    
    void main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        
        /* The lifetime associated with the rsv::xslta_nii_array<>, and each of its contained elements, is
        the lower bound of the lifetimes of the elements in the initializer list. */
        auto arrwp2 = mse::rsv::xslta_nii_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i1, &i2 };
        auto ilaptr3 = arrwp2.front();
        //ilaptr3 = &i3;    // scpptool would complain
        ilaptr3 = &i1;
        
        /* Note that although the initializer list used in the declaration of arrwp3 is different than the
        initializer list used for arrwp2, the lower bound of the lifetimes of both initializer lists is
        the same. */
        auto arrwp3 = mse::rsv::xslta_nii_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i2, &i2 };
        
        /* Since the (lower bound) lifetime values of arrwp2 and arrwp3 are the same, their values can be 
        safely swapped.*/
        std::swap(arrwp2, arrwp3);
        arrwp2.swap(arrwp3);
        
        /* The lower bound lifetime of arrwp4's initializer list is not the same as that of arrwp2. */
        auto arrwp4 = mse::rsv::xslta_nii_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i3, &i1 };
        
        /* Since the (lower bound) lifetime values of arrwp2 and arrwp4 are not the same, their values 
        cannot be safely swapped.*/
        //std::swap(arrwp2, arrwp4);    // scpptool would complain
        //arrwp2.swap(arrwp4);    // scpptool would complain
    }
```

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
