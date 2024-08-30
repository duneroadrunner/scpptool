
Aug 2024

### Overview

scpptool is a command line tool to help enforce a memory and data race safe subset of C++. It's designed to work with the [SaferCPlusPlus](https://github.com/duneroadrunner/SaferCPlusPlus) library. It analyzes the specified C++ file(s) and reports places in the code that it cannot verify to be safe. By design, the tool and the library should be able to fully ensure "lifetime", bounds and data race safety. 

This safety necessarily comes at some (modest) expense of either flexibility or performance. Elements with a choice of tradeoffs are provided in the SaferCPlusPlus library. The goal is not to introduce new coding paradigms, but instead to impose only the minimum restrictions and departures from traditional C++ necessary to achieve practical performant memory safety.

Unfortunately even these minimized changes are not insignificant. For example, the tool considers the standard library containers "unsafe" (and will complain when they are used), and instead provides largely compatible replacement implementations. The other big restriction probably being that null values are not supported for raw pointers.

A notable difference between this tool and some others in development and in other languages is that the safe subset it enforces is (like traditional C++) not "flow (or path) sensitive". That is, whether an operation is allowed (by the tool/compiler) or not depends only on the declaration of the elements involved, not any other preceding operations or code. This is(/was) a common property of "statically typed" languages, that [arguably](#flow-insensitive-analysis) contributes to "scalability".

Some samples of conforming safe code can be found in the examples for the [provided library elements](#lifetime-annotated-elements-in-the-safercplusplus-library).

Note that due to its dependency on the clang+llvm libraries this tool only supports code that's compatible with the clang compiler.

Note that this tool is still in development and not well tested.

Quick intro video (with text-to-speech narration) (transcript with code samples [here](intro_video_transcript.md)) part 1:

https://github.com/duneroadrunner/scpptool/assets/10386072/864df66f-70fb-4e4f-a11f-ff9fee1feb7b

Quick intro video (with text-to-speech narration) part 2:

https://github.com/duneroadrunner/scpptool/assets/10386072/a80f55b5-19fd-4ba2-a121-d999eb5b8932


### Table of contents
1. [Overview](#overview)
2. [How to Build](#how-to-build)
3. [How to Use](#how-to-use)
4. [Local Suppression of the Checks](#local-suppression-of-the-checks)
5. [About the Enforced Subset](#about-the-enforced-subset)
    1. [Restrictions on the use of native pointers and references](#restrictions-on-the-use-of-native-pointers-and-references)
    2. [Referencing elements in a dynamic container](#referencing-elements-in-a-dynamic-container)
    3. [Annotating lifetime constraints](#annotating-lifetime-constraints)
    4. <details>
        <summary>Lifetime annotated elements in the SaferCPlusPlus library</summary>

        1. [Overview](#lifetime-annotated-elements-in-the-safercplusplus-library)
        1. [TXSLTAPointer](#txsltapointer)
        2. [TXSLTAOwnerPointer](#txsltaownerpointer)
        3. [xslta_array](#xslta_array)
        4. [xslta_vector, xslta_fixed_vector, xslta_borrowing_fixed_vector](#xslta_vector-xslta_fixed_vector-xslta_borrowing_fixed_vector)
        5. [TXSLTARandomAccessSection, TXSLTARandomAccessConstSection](#txsltarandomaccesssection-txsltarandomaccessconstsection)
        6. [TXSLTACSSSXSTERandomAccessIterator and TXSLTACSSSXSTERandomAccessSection](#txsltacsssxsterandomaccessiterator-and-txsltacsssxsterandomaccesssection)
        7. [xslta_optional, xslta_fixed_optional, xslta_borrowing_fixed_optional](#xslta_optional-xslta_fixed_optional-xslta_borrowing_fixed_optional)
        </details>
    5. [SaferCPlusPlus elements](#safercplusplus-elements)
    6. [Elements not (yet) addressed](#elements-not-yet-addressed)
6. [Autotranslation](#autotranslation)
7. [Questions and comments](#questions-and-comments)

### How to Build:

Ubuntu Linux is currently the only tested platform. (But there's no intrinsic reason it shouldn't work on any platform for which clang+llvm is available.)

First, the llvm+clang library used by scpptool requires some additional libraries that can be installed as follows:
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

Next, [download](https://github.com/duneroadrunner/scpptool/archive/master.zip) and extract the repository (or clone it). Then just run the `build_myscpptool.sh` script. 

If you're not running an Ubuntu x86_64 system, then it will instruct you to download the clang+llvm pre-built binaries for your system and indicate the directory where they were extracted to. (On some systems, clang+llvm may require you to install other prerequisites. Those should be indicated by link errors. At which point you can just install the requirements and rerun the build script.)

The build script does not require root privileges. 

(For those developing on Windows, the easiest route may be to build the tool in a WSL Ubuntu distro, and then invoke the tool from windows using the `wsl -e` command.)

(Note that the scpptool executable uses a clang include directory located in the relative path '../lib/clang/18' created by the build script. So copying or moving the scpptool executable would also require copying or moving that include directory so that the relative path remains the same.)

### How to Use:

The usage syntax is as follows:

`{scpptool src directory}/scpptool {source filename(s)} -- {compiler options}`

where the {scpptool src directory} is the `src` subdirectory of the repository you downloaded (or cloned).

So for example:

`~/dev/scpptool-master/src/scpptool hello_world.cpp -- -I./msetl -std=c++17`

(Note, the double dashes `--` cannot be omitted even if there are no compiler options.)

### Local Suppression of the Checks

You can use a "check suppression directive" to indicate places in the source code that the tool should not report conformance violations. For example:

```cpp
{
    auto ptr1 = new int(5); // scpptool will complain (because `new` is not allowed)
    MSE_SUPPRESS_CHECK_IN_XSCOPE auto ptr2 = new int(7); // scpptool will not complain
}
```

The presence of `MSE_SUPPRESS_CHECK_IN_XSCOPE` (a macro provided in the `msepointerbasics.h` include file of the SaferCPlusPlus library) indicates that checking should be suppressed for the statement that follows it. The `XSCOPE` suffix means that this directive can only be used in "execution scopes" (sometimes referred to as "blocks"). Essentially places where you can execute a function. As opposed to "declaration scopes". For example:

```cpp
{
    struct A1 {
        int* m_ptr1 = nullptr; // scpptool will complain (because raw pointers are not allowed to be null)
        MSE_SUPPRESS_CHECK_IN_XSCOPE int* m_ptr2 = nullptr; // compile error
        MSE_SUPPRESS_CHECK_IN_DECLSCOPE int* m_ptr3 = nullptr; // this will work
    };
}
```


### About the Enforced Subset

The main differences between "traditional C++" and the safe subset that this tool enforces are probably the restrictions on raw pointers/references and the method for accessing elements in dynamic containers (such as vectors). (Just to be clear, scpptool also undertakes the other basic enforcement tasks required for memory safety, like ensuring that objects are initialized, casts are safe, parts of the object aren't accessed before they are initialized during construction, etc.)

#### Restrictions on the use of native pointers and references

First, this tool does not allow null values for raw pointers. (If you need null pointer values, you can wrap the pointer in an [`optional<>`](#xslta_optional-xslta_fixed_optional-xslta_borrowing_fixed_optional), or use one of the provided smart pointers which safely support null values.)

Also, raw pointers and references are considered to be [scope](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#scope-pointers) references, which essentially means that their lifetime must be (verifiably at compile-time) bounded by an (execution) scope, and any object they target must (verifiably at compile-time) live at least to the end of that bounding scope.

These restrictions imposed on scope reference types are generally not super-onerous, but, for example, if you wanted a raw pointer that targets an object owned by a smart pointer with shared ownership, you would first need to instantiate a ["strong pointer store" object](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#make_xscope_strong_pointer_store) that ensures that the target object will outlive the raw pointer's bounding scope. (Btw, `std::shared_ptr<>` is not supported for other safety reasons, but [safe implementations](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#reference-counting-pointers) of smart pointers with shared ownership are available.)

When the value of one pointer is assigned to another, some static analyzers will attempt to determine the lifetime of the target object based on the "most recent" value that was assigned to the source pointer. scpptool does not do this. scpptool infers a lower bound for the target object lifespan based solely on the declaration of the source pointer ([not any subsequent assignment operation](#flow-insensitive-analysis)). By default, the only assumption made is that the target object outlives the source pointer. 

If [lifetime annotations](#annotating-lifetime-constraints) are applied to the pointer, then it can be assumed that the target object (also) outlives the pointer's intialization value target object. Often [`rsv::TXSLTAPointer<>`](#txsltapointer) will be used in place of raw pointers. It acts just like a raw pointer with lifetime annotations.

#### Referencing elements in a dynamic container

This tool does not support directly taking (raw) references to elements in a dynamic (resizable) container (such as a vector). The preferred way of obtaining a (raw) reference to an element in a dynamic container is via a corresponding ["borrowing fixed (size)"](#xslta_vector-xslta_fixed_vector-xslta_borrowing_fixed_vector) (proxy) container that, while it exists, ensures that no elements are removed or relocated.

Operations that could resize (or relocate) the contents of a "safe" dynamic container implementation provided by the library will incur some extra overhead. But these sorts of operations are generally avoided inside performance-critical inner loops anyway because they often incur unnecessary cost even without the extra overhead.

#### Annotating lifetime constraints

[*provisional*]

This next topic is kind of a longer one about "lifetime annotations". It is an important topic to understand if you ever intend to store raw pointers or references, or use them as function return values. (You can theoretically avoid those situations, and this topic, by using the provided non-owning run-time-checked smart pointers rather than raw pointers/references in those situations.) This topic introduces some new syntax, but don't get hung up on the details. Most of the time you won't be using the new syntax directly. Most of the time it will be [implied by default](#lifetime-elision) or you'll be using provided [library elements](#lifetime-annotated-elements-in-the-safercplusplus-library) that already incorporate the appropriate lifetime annotations. But still, it's recommended to at least give this topic a "once over" to get an idea of how things work and the principles underlying the associated imposed/enforced restrictions.

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

First note that the `42` and `99` are just arbitrarily chosen labels used to distinguish between the lifetimes of the two parameters. So lets go through the "annotations" we added:

After the first parameter we added `MSE_ATTR_PARAM_STR("mse::lifetime_label(42)")`. ` MSE_ATTR_PARAM_STR()` is just a (preprocessor) macro function defined in the SaferCPlusPlus library that lets us add these annotations in such a way that the tool can read them, without affecting compilation of the code. The `"mse::lifetime_label(42)"` just associates a label (of our choosing) to the lifespan of the object bound to the (raw) reference first parameter. So we've assigned the labels `42` and `99` to the lifespans of objects bound to the two (raw) reference parameters.

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

int main(int argc, char* argv[]) {
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

No. (At least not currently.) Our description thus far of what a lifetime label is has maybe been a little misleading. A lifetime label actually represents the lower bound of possible lifespans of any objects that might be targeted by the associated pointer/reference. (In the case of (raw) references (or `const` pointers), since they cannot be retargeted after initialization, there is only one possible object.) This lower bound is determined at the point where the pointer or reference object is declared (at compile-time). It's determined and set from either the initialization value, specified "constraint" annotations, and/or a default value based on the location of the declaration in the code. You might think of lifetime labels as sort of pseudo (deduced) template parameters.

Often there is not enough information to determine the lower bound precisely. In such cases, the tool will use what information is available to try to verify safety as best it can.

So in the above example, in the context of the function implementation (i.e. inside the function), since we don't have access to the parameters' initialization values (i.e. the passed arguments), their lower bound lifetimes are not precisely determined. We know that, by rule, the lower bounds are at least the lifespan of the function call. And that in this case, the lower bound of the `42` lifespan is determined, from the declaration, to be at least that of the `99` lifespan (as specified in the `encompasses()` constraint annotation).

On the other hand, in the context of invoking/calling the function (i.e. outside the function), the lower bound lifetimes of the function arguments often will be known precisely. Known precisely or not, those lower bound lifetimes will have to verifiably conform to the specified (`encompasses()`) constraint.

So we've seen lifetime labels associated with (raw) references and (raw) pointers when used as function parameters. But lifetime labels can be associated with other types of reference objects, and not just when used as function parameters.

##### Annotating (user-defined) types

By "reference object" we mean basically any object that references (ultimately via pointer) any other object(s). A simple example would be just a `struct` that has a pointer member. So lets look at an example of a couple of `struct`s with a pointer member, one with and one without lifetime annotation:

```cpp
#include "msescope.h"

struct CRefObj1 {
    CRefObj1(int* i_ptr) : m_i_ptr(i_ptr) {}

    int* m_i_ptr;
};

struct CLARefObj1 {
    CLARefObj1(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)")) : m_i_ptr(i_ptr) {}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
} MSE_ATTR_STR("mse::lifetime_label(99)");

int main(int argc, char* argv[]) {
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

So you can see the different restrictions on which objects the member pointers can point to, and how the tool uses those restrictions to determine which assignment operations it can verify to be safe.

So lets walk through the application of lifetime annotations to the `CLARefObj1` `struct` and its pointer member. The declaration of the pointer member gets an annotation in similar fashion to the function parameter declarations in our previous examples. We also use the same lifetime label annotation on the `struct` itself to "declare" the lifetime label, just like with functions. Note that (for now at least) the tool requires any `struct` with lifetime annotation to define an (annotated) constructor (from which it can infer the lifetime values associated with the lifetime labels). So in the `CLARefObj1` `struct`, the lifetime (lower bound) value associated with lifetime label `99`, and the pointer member, is inferred from the constructor parameter associated with lifetime label `99`.

##### Annotating return values and `this` pointers

Ok, but if we want to use our annotated `CLARefObj1` type as a reference type, you could image we might want to provide, for example, member operators like `operator*()` and `operator->()`. Lets see how we would do that:

```cpp
struct CLARefObj1 {
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

Operators are just like any other functions and annotated in the same way. Our previous example functions didn't have return values, so we didn't get a chance to see how to annotate the return value. This example shows it. It seems these operators don't take any parameters so we don't have to deal with them here. But to be pedantic, since these are member operators, just like member functions, they actually take an implicit `this` pointer parameter. In some cases, you might need to associate a lifetime label to the implicit `this` pointer parameter. This would be done in similar fashion to the return value annotation above, but substituting the `return_value()` part with `this()`. But understand that the `this()` annotation is just associating a lifetime label to a function parameter (in this case the implicit `this` pointer parameter), and so the lifespan (lower bound) value will be inferred from the parameter, whereas the `return_value()` annotation, on the other hand, is imposing a lifespan (lower bound) value associated with a lifetime label that has already been previously inferred (often from one of the (implicit or explicit) function parameters).

By adding dereference operators, we've made a reference object that kind of resembles the behavior of a pointer. But notice that, unlike the native pointer and reference types, the target of our reference object type is always constrained by the lower bound lifespan inferred from its initialization value (aka constructor argument). With native pointers and references, we have to associate the target object's lifespan with a lifetime label (by adding an annotation to the (parameter) variable or member field) in order to trigger this constraint, whereas a variable or member field of our reference object type will always have this constraint regardless. Native pointers and references are the only types that possess this "dual nature". With all other (user defined) reference object types it's either one or the other.

So currently our reference object stores one pointer, but what if we wanted it to store two different pointers to two different objects with different lifetime (lower bound) constraints?

```cpp
struct CLARefObj2 {
    CLARefObj2(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(99)"), float* fl_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)"))
        : m_i_ptr(i_ptr), m_fl_ptr(fl_ptr) {}

    int* m_i_ptr MSE_ATTR_STR("mse::lifetime_label(99)");
    float* m_fl_ptr MSE_ATTR_STR("mse::lifetime_label(42)");
} MSE_ATTR_STR("mse::lifetime_labels(99, 42)");
```

A reference object can have more than one lifetime label associated with it.

Ok let's say, instead of dereference operators, we want to add some member functions that return the value of each pointer member field:

```cpp
struct CLARefObj2 {
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

Ok, now let's say that instead of the member fields being of type `int*` and `float*`, we want those types to be generic template parameters. That's a little trickier. Because we know that pointers like `int*` and `float*` each have one reference lifetime to which a lifetime label can be associated. But if a type is a generic template parameter then we wouldn't know in advance how many, if any, lifetime labels can be associated with it. In this case we'll use a generic "lifetime label alias" that maps to the set of (reference) lifetimes the template parameter type has (when the template is instantiated).

```cpp
template<typename T, typename U>
struct TLARefObj2 {
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

##### Annotating base classes

Base classes are, in terms of lifetime annotations, conceptually treated just like member fields. You can use the `labels_for_base_class()` annotation (on the derived type) to assign lifetime labels to the first base class.

```cpp
    struct CLARefObj11 : public CLARefObj1 {
        CLARefObj11(int* i_ptr MSE_ATTR_PARAM_STR("mse::lifetime_label(42)")) : CLARefObj1(i_ptr) {}
    } MSE_ATTR_STR("mse::lifetime_label(42)") MSE_ATTR_STR("mse::lifetime_label_for_base_class(42)");
```
Note though that (at the time of writing) lifetimes are only properly transmitted to the immediate base class. So, for example, an inherited member function of a base class of a base class (i.e. two levels of inheritance) that uses lifetime annotations would need to be overridden (by a member function that reexpresses the lifetime annotations).

##### Accessing sublifetimes

Now, if we can revisit the earlier part where we were learning to associate lifetime labels with function parameters, and consider a situation where we are interested in, not the lifetime of the parameter directly, but perhaps the lifespan (lower bound) of an object that the parameter references. We can use the `CLARefObj2` `struct` we defined earlier for this example:

```cpp
struct CLARefObj2 {
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

Note that the `swap` functions take (raw) reference parameters. Determining whether or not the argument values are safe to swap does not depend on the "direct" lifetimes of the arguments, but rather on the lifetime (lower bound)s of any objects that the arguments might refer to (aka the "sublifetimes), right? So for example, swapping the value of two `int`s is always safe because `int`s don't hold any references to any other objects (i.e. `int`s have no "sublifetimes"). Swapping the value of two pointers on the other hand is not always safe. We can ensure that swapping the value of two pointers is safe if we can ensure that the lifetime (lower bound)s of the objects the pointers reference (aka the "sublifetimes") are the same.

So in our annotation of the `swap1()` function we assign "lifetime set alias" labels to the sublifetimes (if any) of the arguments. Then we use the `encompasses` constraint to ensure that none of the sublifetimes of one argument outlives the corresponding sublifetime of the other.

In the annotation of the `swap2()` function, we introduce the use of the `first_can_be_assigned_to_second` constraint to make the annotation a little cleaner. The `first_can_be_assigned_to_second` constraint is similar to the `encompasses` constraint except that it ignores the "direct" lifetime (lower bound) and only constrains the sublifetimes (if any). Using the `first_can_be_assigned_to_second` constraint eliminates the need to assign lifetime (set alias) labels to the argument sublifetimes.

##### Lifetime elision

In certain cases we add implicit ("elided") lifetime annotations to function interfaces, generally as described in the "Lifetime elision" section of the "[[RFC] Lifetime annotations for C++](https://discourse.llvm.org/t/rfc-lifetime-annotations-for-c/61377)" document. Quoting from that document:

> As in Rust, to avoid unnecessary annotation clutter, we allow lifetime annotations to be elided (omitted) from a function signature when they conform to certain regular patterns. Lifetime elision is merely a shorthand for these regular lifetime patterns. Elided lifetimes are treated exactly as if they had been spelled out explicitly; in particular, they are subject to lifetime verification, so they are just as safe as explicitly annotated lifetimes.
> 
> We propose to use the same rules as in Rust, as these transfer naturally to C++. We call lifetimes on parameters *input lifetimes* and lifetimes on return values *output lifetimes*. (Note that all lifetimes on parameters are called input lifetimes, even if those parameters are output parameters.) Here are the rules:
> 
> 	1. Each input lifetime that is elided (i.e., not stated explicitly) becomes a distinct lifetime.
> 	2. If there is exactly one input lifetime (whether stated explicitly or elided), that lifetime is assigned to all elided output lifetimes.
> 	3. If there are multiple input lifetimes but one of them applies to the implicit `this` parameter, that lifetime is assigned to all elided output lifetimes.

Note that we add a little qualification and modification to the rules as quoted. Specifically, we will use an input lifetime that has sublifetimes, but we only apply the input lifetime to the return value if the (sub)lifetime (tree) structure of the input parameter and return types match. Or, if the return type lifetime structure matches the input parameter's with one level of indirection removed (as might happen in the case, for example, where the input parameter is passed by reference, but the corresponding return value is not returned by reference), then we will use the input lifetime with the first level of indirection removed, so that it matches the lifetime of the return type.

##### Lifetime annotation implementation caveats

(Note, that at the time of writing, implementation of lifetime safety enforcement is not complete. Safety can be subverted through, for example, cyclic references with user-defined destructors, or using a type-erased function container to neutralize lifetime annotation specified restrictions, etc.. Also note that while the most essential elements are already available, the process of adding lifetime annotated elements to the SaferCPlusPlus library is still in progress.)

##### third party lifetime annotations

Note that other static lifetime analyzers in development introduce their own distinct lifetime annotations (including the lifetime profile checker and [others](https://discourse.llvm.org/t/rfc-lifetime-annotations-for-c/61377)). Those analyzers may not recognize the lifetime annotations introduced here, so to be compliant with those analyzers you may have to use their lifetime annotations as well. Ideally, in the future scpptool would also support those lifetime annotations, reducing the need for redundant annotations.

#### Lifetime annotated elements in the SaferCPlusPlus library

While the most essential elements are already available, the process of adding lifetime annotated elements to the SaferCPlusPlus library is still in progress.

Since lifetime annotations require the scpptool for enforcement, lifetime annotated elements generally reside in the `mse::rsv` namespace, like the other elements that require scpptool for safety enforcement. Most lifetime annotated elements are "[scope](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#scope-pointers)" elements and conform to the corresponding restrictions. 

##### TXSLTAPointer

`rsv::TXSLTAPointer<>` is just a (zero-overhead) class that acts like a [lifetime annotated](#annotating-lifetime-constraints) pointer. Like raw pointers, `rsv::TXSLTAPointer<>` is considered a [scope](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#scope-pointers) object and is subject to the restrictions of scope objects.

usage example: ([link to interactive version](https://godbolt.org/z/7d4MrG413))

```cpp
    #include "mseslta.h"
    
    int main(int argc, char* argv[]) {
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

##### TXSLTAOwnerPointer

`rsv::TXSLTAOwnerPointer<>` is a ["lifetime annotated"](#annotating-lifetime-constraints) version of [`TXScopeOwnerPointer<>`](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#txscopeownerpointer).

`rsv::TXSLTAOwnerPointer<>` is kind of like an `std::unique_ptr<>` whose use is restricted by the rules of scope objects. You can use it when you want to give scope lifetime to objects that are too large to be declared directly on the stack. 

Instead of its constructor taking a native pointer pointing to an already allocated object, it takes an (often temporary) instance of the desired value and allocates the object itself. You may also use `rsv::make_xslta_owner<>()` to create a `TXSLTAOwnerPointer<>` in a manner akin to `std::make_unique<>()`.

##### xslta_array

`rsv::xslta_array<>` is a [lifetime annotated](#annotating-lifetime-constraints) array.

usage example: ([link to interactive version](https://godbolt.org/z/5Kx9q1dEW))

```cpp
    #include "mseslta.h"
    #include "msemsearray.h"
    #include "msealgorithm.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        
        /* The lifetime (lower bound) associated with the rsv::xslta_array<>, and each of its 
        contained elements, is the lower bound of all of the lifetimes of the elements in the initializer 
        list. */
        auto arr2 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i1, &i2 };
        auto ilaptr3 = arr2.front();
        //ilaptr3 = &i3;    // scpptool would complain
        ilaptr3 = &i1;
        
        /* Note that although the initializer list used in the declaration of arr3 is different than the
        initializer list used for arr2, the lower bound of the lifetimes of both initializer lists is
        the same. */
        auto arr3 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i2, &i2 };
        
        /* Since the (lower bound) lifetime values of arr2 and arr3 are the same, their values can be 
        safely swapped.*/
        std::swap(arr2, arr3);
        arr2.swap(arr3);
        
        /* The lower bound lifetime of arr4's initializer list is not the same as that of arr2. */
        auto arr4 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 2>{ &i3, &i1 };
        
        /* Since the (lower bound) lifetime values of arr2 and arr4 are not the same, their values 
        cannot be safely swapped.*/
        //std::swap(arr2, arr4);    // scpptool would complain
        //arr2.swap(arr4);    // scpptool would complain
        
        {
            /* The standard iterator operations. */
            auto xslta_iter1 = std::begin(arr2);
            auto xslta_iter2 = std::end(arr2);
            //xslta_iter1[0] = &i3;    // scpptool would complain
            xslta_iter1[0] = &i1;
            
            auto xslta_citer3 = std::cbegin(arr2);
            xslta_citer3 = xslta_iter1;
            xslta_citer3 = std::cbegin(arr2);
            xslta_citer3 += 1;
            auto res1 = *(*xslta_citer3);
            auto res2 = *(xslta_citer3[0]);
            
            std::cout << "\n";
            for (auto xslta_iter5 = xslta_iter1; xslta_iter2 != xslta_iter5; ++xslta_iter5) {
                std::cout << *(*xslta_iter5) << " ";
            }
            std::cout << "\n";
        }
        {
            /* The same iterator operations using the SaferCPlusPlus library's make_*_iterator() functions. */
            auto arr2_xsltaptr = mse::rsv::TXSLTAPointer<decltype(arr2)>{ &arr2 };
            auto xslta_iter1 = mse::rsv::make_xslta_begin_iterator(arr2_xsltaptr);
            auto xslta_iter2 = mse::rsv::make_xslta_end_iterator(arr2_xsltaptr);
            //xslta_iter1[0] = &i3;    // scpptool would complain
            xslta_iter1[0] = &i1;
            
            auto xslta_citer3 = mse::rsv::make_xslta_begin_const_iterator(arr2_xsltaptr);
            xslta_citer3 = xslta_iter1;
            xslta_citer3 = mse::rsv::make_xslta_begin_const_iterator(arr2_xsltaptr);
            xslta_citer3 += 1;
            auto res1 = *(*xslta_citer3);
            auto res2 = *(xslta_citer3[0]);
            
            std::cout << "\n";
            mse::for_each_ptr(xslta_iter1, xslta_iter2, [](auto item_ptr) {
                    std::cout << *(*item_ptr) << " ";
                });
            std::cout << "\n";
        }
    }
```

##### xslta_vector, xslta_fixed_vector, xslta_borrowing_fixed_vector

`rsv::xslta_vector<>` is a [lifetime annotated](#annotating-lifetime-constraints) vector. 
`rsv::xslta_fixed_vector<>` is a lifetime annotated vector of a fixed size determined at construction. 
`rsv::xslta_borrowing_fixed_vector<>` is a lifetime annotated vector of fixed size that (exclusively) "borrows" the contents of the specified `rsv::xslta_vector<>`. 

"Dynamic containers", such as vectors, are containers whose (content's) size/structure/location can be changed during a program's execution. scpptool does not "support" direct raw references or pointers to elements of dynamic containers. So, for example, unlike their standard library counterparts, the element access operators and methods of `rsv::xslta_vector<>` do not return raw references. They instead return a "proxy reference" object that can (safely) act like a reference in some situations. But the preferred method of accessing elements of a dynamic container is via the dynamic container's "borrowing fixed" counterpart. These "borrowing fixed" counterparts, while they exist, (exclusively) borrow (access to) the contents of the specified dynamic container and ensure that the content's size/structure/location is not changed. scpptool does support raw references and pointers to elements of "borrowing fixed" containers. (Just as with "regular non-borrowing" "fixed" containers.)

Some other static safety enforcers/analyzers try to automatically and implicitly put vectors (and other dynamic containers) into a "fixed (size/structure) mode" without requiring the programmer to instantiate a "borrowing fixed" object. But such tools rely on "flow (or path) sensitive" analysis, which [arguably](#flow-insensitive-analysis) has undesirable scalability implications.

usage example: ([link to interactive version](https://godbolt.org/z/K9YenzE8h))

```cpp
    #include "mseslta.h"
    #include "msemsevector.h"
    #include "msealgorithm.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;

        /* The lifetime (lower bound) associated with the rsv::xslta_vector<>, and each of its
        contained elements, is the lower bound of all of the lifetimes of the elements in the initializer
        list. */
        auto vec2 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >{ &i1, &i2 };

        /* Note that although the initializer list used in the declaration of vec3 is different than the
        initializer list used for vec2, the lower bound of the lifetimes of both initializer lists is
        the same. */
        auto vec3 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >{ &i2, &i2 };
        std::swap(vec2, vec3);
        vec2.swap(vec3);

        auto vec4 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >{ &i3, &i1 };
        /* Since the (lower bound) lifetime values of arr2 and arr4 are not the same, their values
        cannot be safely swapped.*/
        //std::swap(vec2, vec4);    // scpptool would complain
        //vec2.swap(vec4);    // scpptool would complain

        /* Even when you want to construct an empty rsv::xslta_vector<>, if the element type has an annotated 
        lifetime, you would still need to provide (a reference to) an initialization element object from which 
        a lower bound lifetime can be inferred. You could just initialize the vector with a (non-empty) 
        initializer list, then clear() the vector. Alternatively, you can pass mse::nullopt as the first 
        constructor parameter, in which case the lower bound lifetime will be inferred from the second 
        (otherwise unused) parameter. */
        auto vec5 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >(mse::nullopt, &i2);
        assert(0 == vec5.size());
        //auto vec6 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >{};    // scpptool would complain
        auto vec7 = mse::rsv::xslta_vector<int>{};    // fine, the element type does not have an annotated lifetime

        {
            /* The preferred way of accessing the contents of an rsv::xslta_vector<> is via an associated 
            rsv::xslta_borrowing_fixed_vector<> (which, while it exists, "borrows" exclusive access to the
            contents of the given vector and (efficiently) prevents any of the elements from being 
            removed or relocated). */
            auto bf_vec2a = mse::rsv::make_xslta_borrowing_fixed_vector(&vec2);
            // or
            //auto bf_vec2a = mse::rsv::xslta_borrowing_fixed_vector(&vec2);

            auto& elem_ref1 = bf_vec2a[0];
            elem_ref1 = &i1;
            //elem_ref1 = &i3;    // scpptool would complain (because i3 does not live long enough)

            /* rsv::xslta_borrowing_fixed_vector<> has an interface largely similar to rsv::xslta_fixed_vector<>,
            which is essentially similar to that of std::vector<>, but without any of the methods or operators
            that could resize (or relocate the contents of) the container. */
        }

        /* While not the preferred method, rsv::xslta_vector<> does (currently) have limited support for accessing 
        its elements (pseudo-)directly. */

        /* non-const mse::rsv::xslta_vector<> element access operators and methods (like front()) do not return a
        raw reference. They return a "proxy reference" object that (while it exists, prevents the vector from 
        being resized, etc. and) behaves like a (raw) reference in some situations. For example, like a reference, 
        it can be cast to the element type. */
        typename decltype(vec2)::value_type ilaptr3 = vec2.front();
        ilaptr3 = &i1;
        //ilaptr3 = &i3; // scpptool would complain (because i3 does not live long enough)

        /* The returned "proxy reference" object also has limited support for assignment operations. */
        vec2.front() = &i1;
        //vec2.front() = &i3;    // scpptool would complain (because i3 does not live long enough)

        /* Note that these returned "proxy reference" objects are designed to be used as temporary (rvalue) objects, 
        not as (lvalue) declared variables or stored objects. */

        /* Note again that we've been using a non-const rsv::xslta_vector<>. Perhaps unintuitively, the contents of
        an rsv::xslta_vector<> cannot be safely accessed via const reference to the vector. */
        auto const& vec2_cref1 = vec2;
        //typename decltype(vec2)::value_type ilaptr3b = vec2_cref1.front();    // scpptool would complain

        {
            /* Like the (non-const) element access operators and methods, dereferencing operations of rsv::xslta_vector<>'s 
            (non-const) iterators return "proxy reference" objects rather than raw references. */
            auto xslta_iter1 = std::begin(vec2);
            auto xslta_iter2 = mse::rsv::make_xslta_end_iterator(&vec2);
            *xslta_iter1 = &i1;

            /* rsv::xslta_vector<>'s iterators can be used to specify an insertion or erasure position in the standard way. */
            vec2.insert(xslta_iter1, &i1);
            vec2.erase(xslta_iter1);
        }
        {
            /* rsv::xslta_fixed_vector<> is a (lifetime annotated) vector that doesn't support any operations that 
            could resize the vector or move its contents (subsequent to initialization). It can be initialized from 
            an rsv::xslta_vector<>. */
            auto f_vec1 = mse::rsv::xslta_fixed_vector<typename decltype(vec2)::value_type>(vec2);
        }
    }
```

##### xslta_accessing_fixed_vector

Note that the [`rsv::xslta_borrowing_fixed_vector<>`](#xslta_vector-xslta_fixed_vector-xslta_borrowing_fixed_vector) described above can only be obtained from a non-`const` pointer to the lending vector. In situations where only a `const` pointer to the vector is available, `rsv::xslta_borrowing_fixed_vector<>` has a counterpart, `rsv::xslta_accessing_fixed_vector<>`, which can be obtained from a `const` pointer to supported vectors (including [`rsv::xslta_vector<>`](#xslta_vector-xslta_fixed_vector-xslta_borrowing_fixed_vector)).

`rsv::xslta_accessing_fixed_vector<>`, like `rsv::xslta_borrowing_fixed_vector<>`, ensures, while it exists, that the vector contents are not deallocated/relocated/resized. But unlike `rsv::xslta_borrowing_fixed_vector<>`, `rsv::xslta_accessing_fixed_vector<>`'s access to the vector contents is not exclusive. So, for example, multiple `rsv::xslta_accessing_fixed_vector<>`s corresponding to the same vector can exist and be used at the same time. This lack of exclusivity results in `rsv::xslta_accessing_fixed_vector<>` being branded as ineligible to be passed to or shared with asynchronous threads.

usage example: ([link to interactive version](https://godbolt.org/z/djGqW94bG))

```cpp
    #include "mseslta.h"
    #include "msemsevector.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;

        /* The lifetime (lower bound) associated with the rsv::xslta_array<>, and each of its
        contained elements, is the lower bound of all of the lifetimes of the elements in the initializer
        list. */
        auto vec2 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> >{ &i1, &i2 };

        {
            auto const& vec2_cref = vec2;

            /* Obtaining an rsv::xslta_borrowing_fixed_vector<> requires a non-const pointer to the lending vector. 
            When only a const pointer is available we can instead use rsv::xslta_accessing_fixed_vector<> for supported vector types. */
            auto af_vec2a = mse::rsv::make_xslta_accessing_fixed_vector(&vec2_cref);
            // or
            //auto af_vec2a = mse::rsv::xslta_accessing_fixed_vector(&vec2_cref);

            auto& elem_ref1 = af_vec2a[0];
            int i4 = *elem_ref1;
        }
    }
```

##### TXSLTARandomAccessSection, TXSLTARandomAccessConstSection

`rsv::TXSLTARandomAccessSection<>` is a lifetime annotated [`TXScopeRandomAccessSection<>`](https://github.com/duneroadrunner/SaferCPlusPlus/blob/master/README.md#txscoperandomaccesssection-txscoperandomaccessconstsection-trandomaccesssection-trandomaccessconstsection).

A "random access section" is basically a convenient interface to access a (contiguous) subsection of an existing array or vector. You construct them, using the `make_xslta_random_access_section(...)` functions, by specifying an iterator to the start of the section, and the length of the section. Random access sections support most of the member functions and operators that [std::basic_string_view](http://en.cppreference.com/w/cpp/string/basic_string_view) does, except that the "[substr()](http://en.cppreference.com/w/cpp/string/basic_string_view/substr)" member function is named "xslta_subsection()".

Note that for convenience, random access sections can be constructed from just a pointer or reference to a supported container object. Also note that these `TXSLTARandomAccessSection<>`s are not the library elements that most directly correspond to `std::span<>`s, as they are "tied" to the iterator type of their template argument. [`TXSLTACSSSXSTERandomAccessSection<>`](#txsltacsssxsterandomaccessiterator-and-txsltacsssxsterandomaccesssection)s more closely correspond to `std::span<>`s.

usage example: ([link to interactive version](https://godbolt.org/z/Tj7hajxz6))

```cpp
    #include "mseslta.h"
    #include "msemsearray.h" //random access sections are defined in this file
    #include "msemsevector.h"
    
    class J {
    public:
        /* Remember that these functions will have implicit/elided lifetime annotations applied to their parameters. */
        template<class _TRASection>
        static void foo13lta(_TRASection ra_section) {
            for (typename _TRASection::size_type i = 0; i < ra_section.size(); i += 1) {
                ra_section[i] = ra_section[0];
            }
        }
        template<class _TRAConstSection>
        static int foo14lta(_TRAConstSection const_ra_section) {
            int retval = 0;
            for (typename _TRAConstSection::size_type i = 0; i < const_ra_section.size(); i += 1) {
                retval += *(const_ra_section[i]);
            }
            return retval;
        }
        template<class _TRAConstSection>
        static int foo15lta(_TRAConstSection const_ra_section) {
            int retval = 0;
            for (const auto& const_item : const_ra_section) {
                retval += *const_item;
            }
            return retval;
        }
    };

    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        int i4 = 11;
        int i5 = 13;

        mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 4> array1{ &i1, &i2, &i3, &i4 };
        mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int> > vec1{ &i1, &i2, &i3, &i4, &i5 };
        auto bfvec1 = mse::rsv::make_xslta_borrowing_fixed_vector(&vec1);

        auto xslta_ra_section1 = mse::rsv::make_xslta_random_access_section(array1.begin(), 2);
        J::foo13lta(xslta_ra_section1);

        auto xslta_ra_const_section2 = mse::rsv::make_xslta_random_access_const_section(++bfvec1.begin(), 3);
        auto res6 = J::foo15lta(xslta_ra_const_section2);
        auto res7 = J::foo14lta(xslta_ra_const_section2);

        auto xslta_ra_section1_xslta_iter1 = xslta_ra_section1.begin();
        auto xslta_ra_section1_xslta_iter2 = xslta_ra_section1.end();
        auto xslta_ra_section1_xslta_iter1b = xslta_ra_section1.xslta_begin();
        auto xslta_ra_section1_xslta_iter2b = xslta_ra_section1.xslta_end();
        auto res8 = xslta_ra_section1_xslta_iter2 - xslta_ra_section1_xslta_iter1;
        bool res9 = (xslta_ra_section1_xslta_iter1 < xslta_ra_section1_xslta_iter2);

        /* The library provides the rsv::xslta_random_access_subsection() function which takes a random access section and a
        tuple containing a start index and a length and returns a random access section spanning the indicated
        subsection. You could use this function to implement the equivalent of a "first_half()" function like so: */
        auto xslta_ra_section3 = mse::rsv::xslta_random_access_subsection(xslta_ra_section1, std::make_tuple(0, xslta_ra_section1.length() / 2));
        assert(xslta_ra_section3.length() == 1);

        {
            auto vector1 = mse::rsv::xslta_fixed_vector<mse::rsv::TXSLTAPointer<int> >{ &i1, &i2, &i3 };
            auto xslta_ra_csection1 = mse::rsv::make_xslta_random_access_const_section(&vector1);

            typedef decltype(xslta_ra_csection1) xslta_ra_csection1_t;
            /* You can (also) construct a TXSLTARandomAccessSection<> by passing (a reference to) the container, or a
            pointer to the container. (The former is for conformance with the interface of std::span<>, etc..) */
            auto xslta_ra_csection1b = xslta_ra_csection1_t(vector1);
            auto xslta_ra_csection1c = xslta_ra_csection1_t(&vector1);

            class CD {
            public:
                static bool second_is_longer(xslta_ra_csection1_t xslta_ra_csection1, xslta_ra_csection1_t xslta_ra_csection2) {

                    return (xslta_ra_csection1.size() > xslta_ra_csection2.size()) ? false : true;
                }

                /* Here we will demonstrate the creation of type-erased random access sections by instantiation with
                type-erased iterators. */
                static bool second_is_longer_CSSSXSTE(mse::rsv::TXSLTARandomAccessConstSection<mse::rsv::TXSLTACSSSXSTERAConstIterator<mse::rsv::TXSLTAPointer<int> > > xslta_ra_csection1
                    , mse::rsv::TXSLTARandomAccessConstSection<mse::rsv::TXSLTACSSSXSTERAConstIterator<mse::rsv::TXSLTAPointer<int> > > xslta_ra_csection2) {
                    return (xslta_ra_csection1.size() > xslta_ra_csection2.size()) ? false : true;
                }
            };

            auto res1 = CD::second_is_longer(xslta_ra_csection1, mse::rsv::make_xslta_random_access_const_section(
                mse::rsv::xslta_fixed_vector<mse::rsv::TXSLTAPointer<int> >{ &i1, & i2, & i3, & i4 }));

            auto res2 = CD::second_is_longer_CSSSXSTE(mse::rsv::make_xslta_random_access_const_section(mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 3>{ &i1, & i2, & i3 })
                , mse::rsv::make_xslta_random_access_const_section(mse::rsv::xslta_fixed_vector<mse::rsv::TXSLTAPointer<int> >{ &i1, & i2, & i3, & i4 }));
        }
    }
```

See also [`TXSLTACSSSXSTERandomAccessSection`](#txsltacsssxsterandomaccessiterator-and-txsltacsssxsterandomaccesssection).

##### TXSLTACSSSXSTERandomAccessIterator and TXSLTACSSSXSTERandomAccessSection

`rsv::TXSLTACSSSXSTERandomAccessIterator<>` and `rsv::TXSLTACSSSXSTERandomAccessSection<>` are "type-erased" template classes that can be used to enable functions to take as arguments iterators or sections of various container types (like arrays or (fixed size) vectors) without making the functions into template functions. But in this case there are limitations on what types can be converted. In exchange for these limitations, these types require less overhead. The "CSSSXSTE" part of the typenames stands for "Contiguous Sequence, Static Structure, XSLTA, Type-Erased". So the first restriction is that the target container must be recognized as a "contiguous sequence" (basically an array or vector). It also must be recognized as having a "static structure". This essentially means that the container cannot be resized. (At least not while the `rsv::TXSLTACSSSXSTERandomAccessIterator<>` or `rsv::TXSLTACSSSXSTERandomAccessSection<>` exists.) And these iterators and sections are ["lifetime annotated"](#annotating-lifetime-constraints).

`rsv::TXSLTACSSSXSTERandomAccessSection<>` might be considered, in essence, the primary safe counterpart of `std::span<>`.

usage example: ([link to interactive version](https://godbolt.org/z/h5x78nbE1))

```cpp
    #include "mseslta.h"
    #include "msemsearray.h" //TXSLTACSSSXSTERandomAccessIterator/Section are defined in this header
    #include "msemsevector.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        int i4 = 11;
        int i5 = 13;

        auto array1 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 4>{ &i1, &i2, &i3, &i4 };
        auto array2 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 5>{ &i1, &i2, &i3, &i4, &i5 };
        auto vec1 = mse::rsv::xslta_vector<mse::rsv::TXSLTAPointer<int>>{ &i1, &i2, &i3, &i4, &i5 };
        auto bfvec1 = mse::rsv::make_xslta_borrowing_fixed_vector(&vec1);
        class B {
        public:
            /* Remember that these functions will have implicit/elided lifetime annotations applied to their 
            parameters (that don't have explicit annotations). */
            static void foo1(mse::rsv::TXSLTACSSSXSTERandomAccessIterator<mse::rsv::TXSLTAPointer<int>> ra_iter1) {
                ra_iter1[1] = ra_iter1[2];
            }
            static mse::rsv::TXSLTAPointer<int> foo2(mse::rsv::TXSLTACSSSXSTERandomAccessConstIterator<mse::rsv::TXSLTAPointer<int>> const_ra_iter1 MSE_ATTR_PARAM_STR("mse::lifetime_labels(_[99])"))
                MSE_ATTR_FUNC_STR("mse::lifetime_notes{ label(99); return_value(99) }") {

                const_ra_iter1 += 2;
                --const_ra_iter1;
                const_ra_iter1--;
                return const_ra_iter1[2];
            }
            static void foo3(mse::rsv::TXSLTACSSSXSTERandomAccessSection<mse::rsv::TXSLTAPointer<int>> ra_section) {
                for (mse::rsv::TXSLTACSSSXSTERandomAccessSection<mse::rsv::TXSLTAPointer<int>>::size_type i = 0; i < ra_section.size(); i += 1) {
                    ra_section[i] = ra_section[0];
                }
            }
            static int foo4(mse::rsv::TXSLTACSSSXSTERandomAccessConstSection<mse::rsv::TXSLTAPointer<int>> const_ra_section) {
                int retval = 0;
                for (mse::rsv::TXSLTACSSSXSTERandomAccessSection<mse::rsv::TXSLTAPointer<int>>::size_type i = 0; i < const_ra_section.size(); i += 1) {
                    retval += *(const_ra_section[i]);
                }
                return retval;
            }
            static int foo5(mse::rsv::TXSLTACSSSXSTERandomAccessConstSection<mse::rsv::TXSLTAPointer<int>> const_ra_section) {
                int retval = 0;
                for (const auto& const_item : const_ra_section) {
                    retval += *(const_item);
                }
                return retval;
            }
        };

        auto xs_array_iter1 = mse::rsv::make_xslta_begin_iterator(&array1);
        xs_array_iter1++;

        auto res1 = B::foo2(xs_array_iter1);
        B::foo1(xs_array_iter1);

        auto xs_msearray_const_iter2 = mse::rsv::make_xslta_begin_const_iterator(&array2);
        xs_msearray_const_iter2 += 2;
        auto res2 = B::foo2(xs_msearray_const_iter2);

        auto res3 = B::foo2(mse::rsv::make_xslta_begin_const_iterator(&bfvec1));

        auto bfvec1_iter2 = ++mse::rsv::make_xslta_begin_iterator(&bfvec1);
        B::foo1(bfvec1_iter2);
        auto bfvec1_iter1 = mse::rsv::make_xslta_begin_iterator(&bfvec1);
        auto res4 = B::foo2(bfvec1_iter1);

        auto bfvec1_begin_iter1 = mse::rsv::make_xslta_begin_iterator(&bfvec1);
        mse::rsv::TXSLTACSSSXSTERandomAccessIterator<mse::rsv::TXSLTAPointer<int>> xs_te_iter1 = bfvec1_begin_iter1;
        auto bfvec1_end_iter1 = mse::rsv::make_xslta_end_iterator(&bfvec1);
        mse::rsv::TXSLTACSSSXSTERandomAccessIterator<mse::rsv::TXSLTAPointer<int>> xs_te_iter2 = bfvec1_end_iter1;
        auto res5 = xs_te_iter2 - xs_te_iter1;
        xs_te_iter2 = xs_te_iter1;

        mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 4> array3 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 4>({ &i1, &i2, &i3, &i4 });
        auto array_scpiter3 = mse::rsv::make_xslta_begin_iterator(&array3);
        ++array_scpiter3;
        B::foo1(array_scpiter3);

        mse::rsv::TXSLTACSSSXSTERandomAccessSection<mse::rsv::TXSLTAPointer<int>> xscp_ra_section1(xs_array_iter1++, 2);
        B::foo3(xscp_ra_section1);
        auto xs_mstd_vec_iter1 = mse::rsv::make_xslta_begin_iterator(&bfvec1);
        mse::rsv::TXSLTACSSSXSTERandomAccessSection<mse::rsv::TXSLTAPointer<int>> xscp_ra_section2(++xs_mstd_vec_iter1, 3);

        auto res6 = B::foo5(xscp_ra_section2);
        B::foo3(xscp_ra_section2);
        auto res7 = B::foo4(xscp_ra_section2);

        auto xs_ra_section1 = mse::rsv::make_xslta_random_access_section(xs_array_iter1, 2);
        B::foo3(xs_ra_section1);
        auto xs_ra_const_section2 = mse::rsv::make_xslta_random_access_const_section(mse::rsv::make_xslta_begin_const_iterator(&bfvec1), 2);
        B::foo4(xs_ra_const_section2);

        auto nii_array4 = mse::rsv::xslta_array<mse::rsv::TXSLTAPointer<int>, 4>{ &i1, &i2, &i3, &i4 };
        auto xscp_ra_section3 = mse::rsv::make_xslta_csssxste_random_access_section(&nii_array4);

        auto xscp_ra_section1_xscp_iter1 = mse::rsv::make_xslta_begin_iterator(xscp_ra_section1);
        auto xscp_ra_section1_xscp_iter2 = mse::rsv::make_xslta_end_iterator(xscp_ra_section1);
        auto res8 = xscp_ra_section1_xscp_iter2 - xscp_ra_section1_xscp_iter1;
        bool res9 = (xscp_ra_section1_xscp_iter1 < xscp_ra_section1_xscp_iter2);
    }
```

##### xslta_optional, xslta_fixed_optional, xslta_borrowing_fixed_optional

`rsv::xslta_optional<>` is a [lifetime annotated](#annotating-lifetime-constraints) optional. 
`rsv::xslta_fixed_optional<>` is a lifetime annotated optional whose empty/non-empty status is fixed and determined at construction. 
`rsv::xslta_borrowing_fixed_optional<>` is a lifetime annotated optional whose empty/non-empty status is fixed and (exclusively) "borrows" the contents of the specified `rsv::xslta_optional<>`. 

Conceptually, you can think of an optional as kind of like a [`vector<>`](#xslta_vector-xslta_fixed_vector-xslta_borrowing_fixed_vector) with at most one element.

usage example: ([link to interactive version](https://godbolt.org/z/jcvEb463r))

```cpp
    #include "mseslta.h"
    #include "mseoptional.h"
    #include "msealgorithm.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        auto ilaptr4 = mse::rsv::TXSLTAPointer<int>{ &i2 };
        auto ilaptr5 = mse::rsv::TXSLTAPointer<int>{ &i1 };

        mse::rsv::xslta_optional<mse::rsv::TXSLTAPointer<int> > maybe_int_xsltaptr3(ilaptr4);

        /* Even when you want to construct an empty rsv::xslta_optional<>, if the element type has an annotated
        lifetime, you would still need to provide (a reference to) an initialization element object from which
        a lower bound lifetime can be inferred. You could just initialize the option with a value, then reset()
        the rsv::xslta_optional<>. Alternatively, you can pass mse::nullopt as the first constructor parameter,
        in which case the lower bound lifetime will be inferred from the second (otherwise unused) parameter. */
        mse::rsv::xslta_optional<mse::rsv::TXSLTAPointer<int> > maybe_int_xsltaptr2(mse::nullopt, ilaptr4);
        //mse::rsv::xslta_optional<mse::rsv::TXSLTAPointer<int> > maybe_int_xsltaptr;    // scpptool would complain
        mse::rsv::xslta_optional<int> maybe_int;    // fine, the element type does not have an annotated lifetime

        auto maybe_int_xsltaptr5 = mse::rsv::make_xslta_optional(mse::nullopt, ilaptr4);
        auto maybe_int_xsltaptr6 = mse::rsv::make_xslta_optional(ilaptr4);
        {
            /* As with rsv::xslta_vector<>, the preferred way of accessing the contents of an rsv::xslta_optional<> 
            is via an associated rsv::xslta_borrowing_fixed_optional<> (which, while it exists, "borrows" exclusive 
            access to the contents of the given optional and (efficiently) prevents the element (if any) from being
            removed). */
            auto bfmaybe_int_xsltaptr6 = mse::rsv::make_xslta_borrowing_fixed_optional(&maybe_int_xsltaptr6);
            auto ilaptr26 = bfmaybe_int_xsltaptr6.value();
            std::swap(ilaptr26, ilaptr4);

            auto ilaptr7 = mse::rsv::TXSLTAPointer<int>{ &i3 };
            auto ilaptr28 = bfmaybe_int_xsltaptr6.value_or(ilaptr7);
            //std::swap(ilaptr28, ilaptr4);    // scpptool would complain
            std::swap(ilaptr7, ilaptr28);
        }

        /* While not the preferred method, rsv::xslta_optional<> does (currently) have limited support for accessing
        its element (pseudo-)directly. */

        /* As with rsv::xslta_vector<>, rsv::xslta_optional<>'s non-const accessor methods and operators do not
        return a raw reference. They return a "proxy reference" object that (while it exists, prevents the addition
        or removal of a value and) behaves like a (raw) reference in some situations. For example, like a reference,
        it can be cast to the element type. */
        typename decltype(maybe_int_xsltaptr6)::value_type ilaptr6 = (maybe_int_xsltaptr6.value());
        ilaptr6 = &i2;
        //ilaptr6 = &i3; // scpptool would complain (because i3 does not live long enough)

        /* The returned "proxy reference" object also has limited support for assignment operations. */
        maybe_int_xsltaptr6.value() = &i1;
        //maybe_int_xsltaptr6.value() = &i3;    // scpptool would complain (because i3 does not live long enough)

        /* Note that these returned "proxy reference" objects are designed to be used as temporary (rvalue) objects,
        not as (lvalue) declared variables or stored objects. */

        /* Note again that we've been using a non-const rsv::xslta_optional<>. Perhaps unintuitively, the contents of
        an rsv::xslta_optional<> cannot be safely accessed via const reference to the optional. */
        auto const& maybe_int_xsltaptr6_cref1 = maybe_int_xsltaptr6;
        //typename decltype(maybe_int_xsltaptr6)::value_type ilaptr3b = maybe_int_xsltaptr6_cref1.value();    // scpptool would complain

        {
            /* rsv::xslta_fixed_optional<> is a (lifetime annotated) optional that doesn't support any operations that
            could change its empty/non-empty state. */
            auto f_maybe_int_xsltaptr1 = mse::rsv::xslta_fixed_optional<typename decltype(maybe_int_xsltaptr6)::value_type>(maybe_int_xsltaptr6.value());
        }
    }
```

##### xslta_accessing_fixed_optional

`rsv::xslta_accessing_fixed_optional<>` is the [`rsv::xslta_accessing_fixed_vector<>`](#xslta_accessing_fixed_vector) counterpart for `optional`s.

usage example: ([link to interactive version](https://godbolt.org/z/G4jhKdY4E))

```cpp
    #include "mseslta.h"
    #include "mseoptional.h"
    
    int main(int argc, char* argv[]) {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;

        auto opt2 = mse::rsv::xslta_optional<mse::rsv::TXSLTAPointer<int> >{ &i2 };

        {
            auto const& opt2_cref = opt2;

            /* Obtaining an rsv::xslta_borrowing_fixed_optional<> requires a non-const pointer to the lending optional. 
            When only a const pointer is available we can instead use rsv::xslta_accessing_fixed_optional<> for supported optional types. */
            auto af_opt2a = mse::rsv::make_xslta_accessing_fixed_optional(&opt2_cref);
            // or
            //auto af_opt2a = mse::rsv::xslta_accessing_fixed_optional(&opt2_cref);

            auto& elem_ref1 = af_opt2a.value();
            int i4 = *elem_ref1;
        }
    }
```

#### SaferCPlusPlus elements

Most of the restrictions required to ensure safety of the elements in the SaferCPlusPlus library are implemented in the type system. However, some of the necessary restrictions cannot be implemented in the type system. This tool is meant to enforce those remaining restrictions. Elements requiring enforcement help are generally relegated to the `mse::rsv` namespace. One exception is the restriction that scope types (regardless of the namespace in which they reside), cannot be used as members of structs/classes that are not themselves scope types. The tool will flag any violations of this restriction.

Note that the `mse::rsv::make_xscope_pointer_to()` function, which allows you to obtain a scope pointer to the resulting object of any eligible expression, is not listed in the documentation of the SaferCPlusPlus library, as without an enforcement helper tool like this one, it could significantly undermine safety.

#### Elements not (yet) addressed

The set of potentially unsafe elements in C++, and in the standard library itself, is pretty large. This tool does not yet address them all. In particular it does not complain about the use of essential elements for which the SaferCPlusPlus library does not (yet) provide a safe alternative, such as conatiners like maps, sets, etc.,. 

### Autotranslation

This tool also has some ability to convert C source files to the memory safe subset of C++ it enforces and is demonstrated in the [SaferCPlusPlus-AutoTranslation2](https://github.com/duneroadrunner/SaferCPlusPlus-AutoTranslation2) project.

### "Flow (in)sensitive" analysis

Some of the other C++ lifetime analyzers in development employ "flow (or path) sensitive" analysis. That is, they determine whether or not an operation is safe based, in part, on the operations that precede it in the program execution. So for example in this piece of code:

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

While a static analyzer can get arbitrarily good at recognizing safe code, there will, in theory, always be some safe code that it won't be able to verify as such in a timely manner. (Because "halting problem", right?) So the question is whether or not a static analyzer should attempt to identify as much safe code as it can, or whether it should instead only accept a well-defined, "easy" to understand subset of safe code.

The advantage of maximizing the set of code accepted as safe is that it reduces the modifications of pre-existing/legacy code needed to bring it into safety conformance. The disadvantage is that it may take programmers (and code modification/generation tools) more time and effort to become familiar with the boundaries between code that will and won't be accepted as safe. To some extent, the scalability and debugability arguments in favor of static typing over dynamic typing apply here.

Currently, scpptool does not use "flow (or path) sensitive" criteria to determine whether or not code will be accepted as safe. As a result, the set of accepted code may be smaller / more restricted than those of "flow (or path) sensitive" analyzers. But perhaps also easier to understand. Pre-existing/legacy code is, to some degree, addressed via [autotranslation](https://github.com/duneroadrunner/scpptool#autotranslation).

With respect to the example given above, scpptool simply doesn't condone null (raw) pointer values at all. If null values are desired, the pointer would have to be wrapped in an "optional" container, or a smart pointer that (safely) supports null values would have to be used instead.

### Questions and comments
If you have questions or comments you can create a post in the [discussion section](https://github.com/duneroadrunner/scpptool/discussions). 

*New:* Just created a discord server: https://discord.gg/CVQcYweH

