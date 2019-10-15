
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
