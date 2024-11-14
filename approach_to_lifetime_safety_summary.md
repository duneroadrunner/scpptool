Nov 2024

### Rough Summary of the Approach to Lifetime Safety For Those With Some Familiarity With Rust

Mutable aliasing can be a code correctness issue, but in most situations it's not a memory safety issue. For example, if one has two non-`const` pointers to an `int` variable, or even an element of a (fixed-sized) array of `int`s, there's no memory safety issue due to the aliasing. But, for example, if I have a non-`const` pointer to an `std::vector<>` and another pointer to one of its elements, that can be a lifetime safety issue if the vector contents are cleared or relocated due to an operation instigated via the non-`const` pointer to the vector.

So the premise is that there is only a limited set of situations where mutable aliasing is potentially a lifetime safety issue. Namely, when a mutable reference to a dynamic container (like a vector, optional, set, etc.) or a (dynamic) owning pointer (like a shared pointer or unique pointer) is used to modify the "structure" of the owned contents. (Modification includes relocation.) So in the scpptool solution, direct (raw) references to the contents of a dynamic container or owning pointer are simply not allowed. In the case of the "idiomatic" dynamic containers, for example, they don't even have any member function or operator that yields a raw reference.

You can obtain a raw reference to the contents indirectly via a "borrowing" object. A borrowing object is roughly analogous to a slice in Rust. The existence of the borrowing object prevents the "structure" of the contents from being modified. This is done via run-time mechanisms and the type system, and does not rely on the static analyzer. A couple of different mechanisms are used depending on the type. These run-time mechanisms generally don't have much effect on performance as they generally aren't applied in inner loops.

All dynamic container and owning pointer types have a corresponding borrowing object type. (Multiple types can share the same borrowing object type.) The claim is that this completely solves the lifetime safety issue due to mutable aliasing.

The remaining lifetime safety issues are addressed by enforcing scope lifetime restrictions essentially in similar fashion to the way Rust does (or originally did). The fact that the scpptool implementation does not use flow analysis makes the enforcement a little more restrictive than Rust's, but also simpler to implement (and theoretically faster to execute).

The approach does not intrinsically preclude the use of flow analysis if desired. But it isn't immediately clear that it would be a net benefit, irrespective of the additional implementation complexity. Time and experience will reveal the extent of any inconvenience resulting from the extra restrictiveness due to the lack of flow analysis, but we're fairly confident it's not a major issue. And unlike Rust, with the scpptool solution you can always resort to run-time checked pointers.

The lifetime annotations also work in somewhat similar fashion to Rust. (This is where the majority of the implementation complexity comes from.) There are differences, like for example how the scpptool version more reflects C++'s "duck typing" templates (i.e. you can refer to (sub)lifetimes of a generic type without any indication that the type actually has those (sub)lifetimes in advance).

