---
title: "Type Checking with Eqlog: Polymorphism"
lang: "en_US"
---

This is the fifth post in a series on implementing a type checker with the [Eqlog](https://github.com/eqlog/eqlog) Datalog engine.
In this post, we'll extend our type system with generics, or *polymorphism*, and implement [Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

You can find the other posts here:

1. [Parsing](../type-checking-with-eqlog-parsing) [[code](https://github.com/eqlog/examples-inference/tree/parsing)]
2. [Variable binding](../type-checking-with-eqlog-variable-binding) [[code](https://github.com/eqlog/examples-inference/tree/binding)]
3. [Types](../type-checking-with-eqlog-types) [[code](https://github.com/eqlog/examples-inference/tree/types)]
4. [Typing](../type-checking-with-eqlog-typing) [[code](https://github.com/eqlog/examples-inference/tree/typing)]
5. **Polymorphism [[code](https://github.com/eqlog/examples-inference/tree/hindley-milner)] (this post)**

After the last post, we now have a type checker for a *simply typed* language.
This means that our type checker accepts a program only if every expression and variable can be assigned a unique concrete type.
For example, consider the following program, which is rejected by our type checker:
```typescript
function id(x) {
    return x;
}

let a = id(5);
let b = id('xyz');
```
The problem here is the variable `x`:
Our type checker believes that it must have type `number` due to the call `id(5)`, but that it must also have type `string` due to the call `id('xyz')`.
On the other hand, if we removed the two calls to `id`, so that `id` was never called, then our type checker would reject the program because the type of `x` would not be determined.

Our goal for this post is to implement generics, which will make the program above type check.
To be specific, we'll implement first-order polymorphism.
This means that, in addition to the types we've already seen, which we'll refer to as *simple types* or *monotypes* from now on, we now also consider *polytypes*.
Polytypes are given by a list of *bound* type variables and a simple type that may contain any of the bound type variables.
We write polytypes as `forall a0 a1 <...> an. t`, where `t` is a simple type that can contain any of the type variables `a0, ..., an`.
The `id` function above will be inferred to have type `forall a. (x: a) => a`.
That our flavor of polymorphism is first-order means that polytypes cannot be nested:
There can only be one quantifier in the front of a type.

We still require each expression and each variable introduced by a `let` statement to have a simple type, but names introduced with `function` statements have polytypes.
At every usage site of a variable whose type is a polytype, we substitute the polytype into a monotype by replacing each of its bound type variable by a simple type.

Within the scope of a function body, we consider type variables of the function type to be simple types.
This means that the variable `x` in the program above has simple type `a` within the body of `id`.
And it also has implications for nested function definitions.
Consider the following program:
```
function id2(y) {
    function foo(z) {
        return y;
    }
    return foo(());
}
```
The type of `id2` is `forall b. (y: b) => b`.
The function `foo` has type `forall c. (z: c) => b`.
