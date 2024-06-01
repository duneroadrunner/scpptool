May 2024

Intro video transcript:

### A Quick Intro To The scpptool (Essentially) Safe Subset of C++

This video is an introduction to programming in the scpptool safe subset of C++. scpptool is a static analyzer that analyzes your C++ source code and complains about any code that it cannot verify to be memory and data race safe. 

Historically C++ has been famous for its lack of memory-safety and undefined behavior. In this video we assume the viewer is familiar with the C++ safety issues being addressed.

Now intuitively, it doesn't seem too hard to identify a very limited subset of C++ that's essentially memory-safe. For example you can imagine a subset that just excludes all unsafe elements, like pointers and references, as well as any unchecked container accessors or iterators that could result in an "out of bounds" access. You might also need to exclude native signed integers to avoid the undefined behavior associated with integer overflow. And so on.

But without safe replacements for those excluded elements, such a limited subset wouldn't really be very practical. So the idea behind this scpptool is to use static analysis to enforce a safe subset that, with the help of a companion library, is both practical and high performance. In fact, as far as we know, no other memory-safe language available surpasses (or arguably matches) the scpptool safe subset of C++ in terms of performance and expressive power.

Unlike some other efforts, we are not trying to introduce new programming paradigms. In fact the goal is to impose only the minimum necessary departures from traditional C++. The two main differences imposed are, i) that null pointer values are not permitted for raw pointers, and ii) that raw references to elements of dynamic containers, like vectors, can only be obtained through a separate interface that ensures that the container contents are not moved or deallocated while the raw reference exists.

Ok so let's start getting our hands dirty and try to write some safe code.

Let's start with an example based on one that was posted online recently related to the classic C++ footgun where references to vector elements become invalid when additional elements are added to the vector:

```cpp
#include<iostream>
#include<vector>

void print_int(const int& i) {
    std::cout<< i << std::endl;
}

int main(int argc, char* argv[]) {
    std::vector<int> list { 1, 2, 3 };

    for(const auto i : list){
        list.push_back(i);
        /* This is already undefined behavior because the hidden iterators of the `for` loop are now invalid. */
        print_int(i);
    }

    for(const auto i : list) {
        print_int(i);
    }

    return 0;
}
```

This example contains insidious undefined behavior because the first `for` loop is using hidden iterators whose initial values were obtained by the implicit calls `std::begin(list)` and `std::end(list)`. Those iterators are potentially  invalidated when the `push_back()` method is called.

We would expect the scpptool analyzer to complain about this unsafe code. Let's verify that it does. First we need to download and extract scpptool (or clone it if you prefer). In the resulting "scpptool-master" directory you'll find a build script. We'll just go ahead and run that script.

If you're running Ubuntu linux on an x86_64 platform, then the script will automatically download the appropriate clang+llvm pre-built library used by scpptool. If you're on a different platform, then the script will direct you to go to the specified webpage where you can download the clang+llvm pre-built library appropriate for your platform. It will then ask you to specify the (full path of the) directory where you extracted the clang+llvm pre-built library.

It will then proceed to build the scpptool executable. The scpptool executable will be located in the "src" subdirectory of the "scpptool-master" directory. If you prefer to move the scpptool executable to another location, note that you must follow the documentation and also move the associated clang include directory so that the path relative to the scpptool executable remains the same. We'll just leave it where it is for now. (Note that installation procedures may have changed since the recording of this video, so be sure to consult the latest installation instructions.)

Ok, now we're ready to use the scpptool to analyze our (unsafe) program.

```
~/dev/clang_tooling/scpptool/src/scpptool example1.cpp --

/home/user1/dev/clang_tooling/test/scpptool/example1/example1.cpp:9:5: error: 'std::vector' is not supported (in type 'std::vector<int>' used in this declaration). Consider using a corresponding substitute from the SaferCPlusPlus library instead. 


1 verification failures. 
```

Looking at the output, we see that scpptool does indeed complain about our unsafe program. scpptool considers `std::vector<>` to be irredeemably unsafe. And not just because of the issue we're experiencing here. For example, just the fact that `std::vector<>`'s iterators aren't (necessarily) bounds checked alone means that no degree of static analysis can salvage `std::vector<>`. So the companion SaferCPlusPlus (header-only) library provides an alternative vector with a safer implementation and interface while attempting to remain as compatible as possible with `std::vector<>`. In fact, we can almost use it as a drop-in replacement for `std::vector<>` in our example.

So now lets download and extract (or clone if you prefer) the SaferCPlusPlus (header) library. The project contains documentation and a directory of usage examples, but for right now we're only interested in the "include" directory of header files that we want to include in our programs. For this demonstration, we'll just copy the include files into a local subdirectory of our project where they'll be easy to find.

```cpp
#include<iostream>
#include "msemsevector.h"

void print_int(const int& i) {
    std::cout<< i << std::endl;
}

int main(int argc, char* argv[]) {
    mse::rsv::xslta_vector<int> list { 1, 2, 3 };

    for(const int i : list){
        list.push_back(i);
        /* This is no longer undefined behavior. `mse::rsv::xslta_vector<int>::iterator`s are not invalidated by the `push_back()`. */
        print_int(i);
    }

    for(const auto i : list) {
        print_int(i);
    }

    return 0;
}
```
So in this updated version of our program, we've replaced `std::vector<>` with `mse::rsv::xslta_vector<>`. The code is now safe and valid because `mse::rsv::xslta_vector<>`'s iterators are not invalidated by operations like `push_back()`. As some may suspect, these safe iterators store an index rather than a pointer directly targeting a vector element. A subtle difference that might be easy to miss is that we changed the loop variable to be explicitly declared as an `int` rather than `auto`. This is because, unlike with `std::vector<>`, dereferencing the iterator does not return a raw reference to the element type, but rather a "proxy reference" object that implicitly converts to the element type. This is because, like an `std::vector<>` iterator, a raw reference to an element would be prone to becoming invalid.

This does not mean that you cannot (safely) obtain a raw reference to a vector element, but doing so does require a little ceremony to ensure that the reference doesn't become invalid. First we need to instantiate a "borrowing fixed" vector. A borrowing fixed vector "borrows" the contents of a given vector and ensures that those contents are not moved or deallocated.

```cpp
#include<iostream>
#include "msemsevector.h"

void print_int(const int& i) {
    std::cout<< i << std::endl;
}

int main(int argc, char* argv[]) {
    mse::rsv::xslta_vector<int> list { 1, 2, 3 };

    for(const int i : list){
        list.push_back(i);
        /* This is no longer undefined behavior. `mse::rsv::xslta_vector<int>::iterator`s are not invalidated by the `push_back()`. */
        print_int(i);
    }

    {
        auto bf_vector = mse::rsv::make_xslta_borrowing_fixed_vector(&list);
        for(auto& i_ref : bf_vector) {
            i_ref *= 10;
            print_int(i_ref);
        }
    }

    for(const auto i : list) {
        print_int(i);
    }

    return 0;
}
```

So unlike `mse::rsv::xslta_vector<>`, dereferencing the iterators of a borrowing fixed vector does yield a raw reference. This is safe because the borrowing fixed vector ensures that its contents aren't going anywhere. Note that while the borrowing vector exists, the contents of the original lending vector are not accessible via the original vector's interface.

If you try to access the contents of the original lending vector, in the current implementation you'll find that the original vector is just empty. But in the future, its possible that attempts to access the original vector while its contents are borrowed may result in an exception. So try to avoid doing that.

Borrowing fixed vectors are expected to be the primary method of accessing vector elements. At least in performance sensitive code.

Just to be clear, the value of the elements can be modified via the borrowing fixed vector, but not the number of elements.

The borrow ends when the borrowing vector goes out of scope, at which point the (possibly modified) contents are returned to the original lending vector.

As you can see, most of the safety enforcement of these vectors and borrowing vectors are implemented in the type system without reliance on extra static analysis. But for complete safety, the scpptool static analysis will complain if you try to misuse these elements. For example, if you try to inappropriately use a borrowing vector as a function return value. 

The reason we need these borrowing fixed vectors is because vectors are so-called "dynamic" containers. Containers that can deallocate or relocate their elements arbitrarily at run-time. But vectors are not the only dynamic containers. For example, strings are also dynamic containers. And optionals, which can be thought of conceptually as kind of vectors that can only contain zero or one objects. "Borrowing fixed" counterparts are also provided for those containers.

So we've just addressed one of the remaining big lifetime issues in "modern" C++. Now let's take a look at how pointers and references are handled:

The first big restriction is that null values are not supported for raw pointers. If you need a null value, wrap the pointer in an optional. Or, if you prefer, you can use one of the provided (non-owning) smart pointers that safely support null values. In the general case, it's simply not practically possible to safely support null pointer values without inserting run-time dereference checks.

But in many cases where null pointer values are not needed, safety can be achieved without additional run-time overhead. But some restrictions apply. scpptool's strategy is to require that any object targeted by a pointer (or reference) outlives the pointer itself, regardless of how long the object will actually remain the target of the pointer. Again we provide (non-owning) smart pointers that can target objects that don't outlive them, but any target of a raw pointer must outlive the pointer.

So if we take this code:

```cpp
#include<iostream>

int main(int argc, char* argv[]) {
    {
        int i1 = 3;
        
        int * iptr1 = &i1; // no problem, target object outlives the pointer

        {
            int i2 = 5;
            iptr1 = &i2; // scpptool will complain that the target object does not outlive the pointer

            iptr1 = &i1; // note that promptly restoring the approved target does not suppress the original complaint
        }

        std::cout << *iptr1;
    }

    return 0;
}
```

and run the scpptool analyzer:

```
~/dev/clang_tooling/scpptool/src/scpptool example1.cpp --

/home/user1/dev/clang_tooling/test/scpptool/example1/example1.cpp:11:13: error: Unable to verify that this pointer assignment (of type 'int *') is safe and valid. (Possibly due to being unable to verify that the object(s) referenced by the new pointer live long enough.) 


1 verification failures.
```

We see that scpptool complains when you try to target a pointer at an object that doesn't outlive it. And even immediately restoring the original target does not placate the analyzer. As mentioned, if you really need to target an object that doesn't outlive the pointer, the library has non-owning smart pointers that (safely) support this. But I think you'll find this rarely necessary in practice.

Ok, but what about assigning the value of one pointer variable to another? Well the analyzer knows that a pointer's target must outlive it, so therefore it would be safe to assign the value of one pointer to another if the source pointer itself outlives the pointer being assigned to. So if we take this code:

```cpp
#include <iostream>

int main() {
    {
        int i1 = 3;
        
        int * iptr1 = &i1;
        int * iptr2 = &i1;

        iptr2 = iptr1; // no problem because iptr1 outlives iptr2

        iptr1 = iptr2; // scpptool will complain because iptr2 does not outlive iptr1

        std::cout << *iptr1 << std::endl;
    }

    return 0;
}
```

and run the scpptool analyzer:

```
~/dev/clang_tooling/scpptool/src/scpptool example1.cpp --

/home/user1/dev/clang_tooling/test/scpptool/example1/example1.cpp:12:9: error: Unable to verify that this pointer assignment (of type 'int *') is safe and valid. (Possibly due to being unable to verify that the object(s) referenced by the new pointer live long enough.) 


1 verification failures. 
```

We see that it allows the first pointer-to-pointer assignment, but complains about the second one, which is just the reverse of the first one. That's because in the second assignment the source pointer does not outlive the destination pointer and so could be potentially pointing to a target object that also doesn't outlive the destination pointer.

Now in this simple example, it's readily apparent to us that the target object actually outlives both pointers. But determining how long a pointer's target object lives is not so easy in every case, so, for the sake of consistency, the restriction is based on the lifetimes of the pointers themselves, not the object they happen to be pointing to at the time.

Still, we could imagine that there might be situations where we would want to be able to assign pointer values back and forth between each other. scpptool does support this, but requires the pointers to be annotated with "lifetime annotations". Adding a lifetime annotation to a pointer changes the pointer's restriction from the default one, that the pointer's target must outlive the pointer itself, to having the same restriction as the pointer value it was initialized with in its declaration. If the pointer value it was initialized with was a temporary pointer to an object, then the restriction is that the the pointer's target must live at least as long as that initialization target object. This means, for example, that two different annotated pointers initialized with (a temporary pointer to) the same target object would have exactly the same restrictions on the values they can have.

(This part can be a little confusing for the unfamiliar. If necessary, you may want to go back and listen to the last paragraph again carefully. The example to follow should help to clarify things.)

When two pointers have exactly the same restrictions on their values, assignments between them in either direction are safe and permitted.

While you can explicitly add lifetime annotations to raw pointers, we'll leave that for another discussion and note here that the library provides a pointer type template that is already defined with lifetime annotations. So if we consider this code:

```cpp
#include "mseslta.h"

int main(int argc, char* argv[]) {
    {
        int i1 = 3;
        int i2 = 5;
        int i3 = 7;
        /* The (lower bound) lifetime associated with an rsv::TXSLTAPointer<> is set to the lifetime of 
        its initialization value. */
        auto p2a = mse::rsv::TXSLTAPointer<int>{ &i2 };
        auto p2b = mse::rsv::TXSLTAPointer<int>{ &i2 };

        p2a = p2b;
        p2b = p2a;
        std::swap(p2a, p2b);
        
        auto p1 = mse::rsv::TXSLTAPointer<int>{ &i1 };

        /* The (lower bound) lifetime associated with p2a does not outlive the one associated with 
        p1, so assigning the value of p2a to p1 cannot be verified to be safe. */
        p1 = p2a;    // scpptool will complain
        p2a = p1;
        p2a = &i1;
        p2a = &i3;    // scpptool will complain
    }

    return 0;
}
```

`mse::rsv::TXSLTAPointer<int>` is a zero-overhead pointer type with the same performance characteristics as a raw pointer. Because it is defined with lifetime annotations (that's what the LTA in the name refers to), it is restricted to pointing to objects which live at least as long as the target object it was initialized with. Since `p2a` and `p2b` are both initialized with (temporary pointers to) the same target object, they have exactly the same restrictions, so their values can be swapped or assigned to each other in either direction.

Note that elements in a container, like an array, are all considered to have essentially the same lifetime. So a pointer that targets an element of a container can be assigned to target any other element in that container.

But notice that when we try to assign the value of `p2a` to `p1`, the scpptool analyzer complains even though `p2a` outlives `p1`:

```
~/dev/clang_tooling/scpptool/src/scpptool example1.cpp -- -I ./msetl/

/home/user1/dev/clang_tooling/test/scpptool/example1/example1.cpp:21:3: error: Unable to verify that in the 'mse::rsv::TXSLTAPointer<int>::operator=' member function call expression, the argument corresponding to a parameter with lifetime label id '99' has a lifetime (including any sublifetimes) that meets the (minimum required) lifetime set when the object was initialized.
  ./msetl/mseslta.h:199:4: function declared here 

/home/user1/dev/clang_tooling/test/scpptool/example1/example1.cpp:24:3: error: Unable to verify that in the 'mse::rsv::TXSLTAPointer<int>::operator=' member function call expression, the argument corresponding to a parameter with lifetime label id '99' has a lifetime (including any sublifetimes) that meets the (minimum required) lifetime set when the object was initialized.
  ./msetl/mseslta.h:199:4: function declared here 


2 verification failures. 
```

That's because even though `p2a` outlives `p1`, the restriction of `p2a` is more strict than that of `p1`. That is, the target object `p2a` was initialized with does not outlive the target object `p1` was initialized with.

But this means that the reverse assignment is allowed.

And similarly, `p2a` cannot be assigned `i3` as a target object. Because even though `i3` outlives `p2a`, it does not outlive `p2a`'s initialization target object, `i2`.

Under scpptool's restrictions, you'll likely find lifetime annotated pointers to be significantly more useful in practice than unannotated raw pointers. (Though there are a lot of situations where either can be used.) Indeed, despite the restrictions we demonstrated, lifetime annotated pointers should be able to fulfill most use cases where pointers are called for. And again, for the minority of cases where they can't, the library provides essentially unrestricted run-time checked non-owning pointers.

So there you have it. A quick demonstration of how scpptool ensures lifetime safety in modern C++. For more information, you can check out the documentation in the github repository.
