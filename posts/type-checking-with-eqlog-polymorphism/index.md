---
title: "Type Checking with Eqlog: Polymorphism"
date: August 22, 2023
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

In the last post, we completed our type checker for a *simply typed* language.
This type checker accepts a program only if it can assign a uniquely determined, concrete type to every expression and variable in the program.
But this can be overly restrictive; our type checker rejects some sensible programs.
Consider this example:
```typescript
function id(x) {
    return x;
}

let a = id(5);
let b = id('xyz');
```
The problem here is a type conflict for the variable `x`:
Our type checker infers that `x` must have type `number` due to the call `id(5)`, but that it must also have type `string` due to the call `id('xyz')`.
On the other hand, if we removed the two calls to `id`, so that `id` was never called, then our type checker would reject the program because the type of `x` would not be determined.

Our goal for this post is to implement *polymorphism*, or *generics*, which will make the program above type check with inferred polymorphic type `id : (a) => a` where `a` is a *type variable*.
At the two call sites of the `id` function, our new version of the type checker implicitly instantiates the polymorphic type of `id` by plugging in `number` and `string`, respectively, for the type variable `a`.

## Monotypes and polytypes

Polymorphism makes it necessary for us to deal with type variables.
We represent these in our Eqlog program as a `Type` elements which cannot be obtained by applying one of the type constructors to other `Type` elements.
Our previous checker for simple types would report an error if it found such `Type` elements after Eqlog evaluation, but we will relax this check later on.

In addition to the monomorphic types or *monotypes* that we've seen so far, we now also consider polymorphic types or *polytypes*.
Both monotypes and polytypes are given by an underlying `Type` element.
```eqlog
Sort GeneralType;
Func MonoType : Type -> GeneralType;
Func PolyType : Type -> GeneralType;
```
The difference between monotypes and polytypes lies in how we interpret type variables occurring in the underlying `Type` element:

- A `MonoType(ty)` is well-formed only if all type variables occurring in `ty` are part of the ambient *type context*.
  Type contexts are sets of `Type` elements that parametrize a program fragment; more on this [below](#type-contexts).
  For example, monotypes occurring in top level module scope may not contain any type variables at all, and within the body of the `id` function, the only type variable that a monotype may refer to is the type of the argument `x`.

- In `PolyType(ty)`, the `ty` element represents a *type scheme*:
  We interpret type variables in `ty` that are not in the ambient type context as *bound* or *universally quantified*.
  The set of bound type variables in a polytype is often syntactically denoted like so:
  ```
  id: forall a. (a) => a
  ```

The flavor of polymorphism we're implementing here is usually called *prenex* or *rank 1* polymorphism.
"Prenex" because types can be (implicitly) quantified over only on the top level, i.e. in *front* of the type.
Thus `forall a. (a) => a` is valid, whereas `(forall a. a => a) -> (forall b. b => ())` is invalid.
And "rank 1" because our type system has rank 0 types (monotypes) and rank 1 types (polytypes) which quantify over rank 0 types, but no more:
We could also imagine rank 2 types that are allowed to quantify over rank 1 types, rank 3 types which quantify over rank 2 types and so forth.

Since types of variables can now be polymorphic, we have to change our `VarTypeInX : X -> Type` function family to be valued in `GeneralType`:
```eqlog
Func VarTypeInStmts : StmtListNode -> GeneralType;
Func VarTypeInExpr : ExprNode -> GeneralType;
...
```
While the axioms propagating variable bindings through syntax nodes can stay the same, we have to decide for each AST node that introduces a variable binding whether the variable has monotype or polytype.

- Variables introduced by function statements have polytypes.
  ```eqlog
  Axiom
      ConsStmtListNode(_, head, tail)
      & FunctionStmtNode(head, func)
      & Function(func, var, _, _ ,_)
      & ty = PolyType(FunctionNodeType(func))
      =>
      VarTypeInStmts(var, tail) = ty
      ;
  ```

- Variables introduced by let statements have monotypes.
  ```eqlog
  Axiom
      ConsStmtListNode(_, head, tail)
      & LetStmtNode(head, var, ty_annot, expr)
      & ty = SemanticOptType(ty_annot)
      & expr_type = ExprType(expr)
      & mono_expr_type = MonoType(expr_type)
      =>
      expr_type = ty
      & VarTypeInStmts(var, tail) = mono_expr_type
      ;
  ```
  We're thus following Rust's type system, where variables introduced in let statements must have monotypes, too.
  TypeScript, on the other hand, allows such variables to have polytypes.
  Since our let bindings have monotypes, our expressions have monotypes as well.
  This allows us to reuse most of the existing `ExprType` machinery that we've worked on in the last post, but with some straightforward adaptations we could also implement TypeScript's let bindings instead.

- Function argument variables have monotypes.
  ``` 
  Axiom
      ConsArgListNode(_, var, otn, tail)
      & ty = MonoType(SemanticOptType(otn))
      =>
      VarTypeInArgList(var, tail) = ty
      ;
  ```
  This is a general limitation of rank 1 polymorphism:
  The types of function argument variables appear in the domain of a function type.
  So if function argument types were polytypes, then this function type would be rank 2.

- Inside the argument list (and hence body) of a function literal, the function being defined has monotype.
  ```eqlog
  Axiom
      Function(func, var, arg, _ ,_)
      & ty = MonoType(FunctionNodeType(func))
      =>
      VarTypeInArgList(var, arg) = ty
      ;
  ```
  This means that recursive calls may not instantiate the function being defined with different type arguments.

## Type contexts

Which type variables in a polytype are bound depends on the context of the polytype:
If the polytype appears in the global module scope, then all type variables in it are bound.
But if a polytype appears in the body of function, then only those type variables that are not already introduced by arguments of the containing function are bound.
Consider the following convoluted way of implementing the identity function:
```typescript
function id(x) {
    function const_x(y) {
        return x;
    }
    return const_x(5);
}
```
Suppose that the type of `x` is `MonoType(a)` and that the type of `y` is `MonoType(b)` for type variables `a` and `b`.
Then the type of `id` is `PolyType((a) => a)`, and since `id` appears in the global module scope, `a` is bound.
The type of `const_x` is `PolyType((b) => a)`, but since it appears in the body of the `id` function, so that the ambient type context contains `a`, only `b` is bound.

Because of local functions such as the one above, we need to keep track of which type variables were already introduced in the body of a function, and we need to propagate this information into relevant subnodes.
This is the purpose of *type contexts*, which we define in Eqlog as follows:
```eqlog
Sort TypeContext;

Pred TypeInContext : Type * TypeContext;
Pred TypesInContext : TypeList * TypeContext;
```
Think of `TypeContext` elements as (non-[extensional](https://en.wikipedia.org/wiki/Axiom_of_extensionality)) sets of `Type` elements, and the `TypeInContext` predicate as the membership relation.
We add axioms (here omitted) so that `TypesInContext(tys, ctx)` holds if and only if every type in the `tys` type list is in `ctx`.

Next we introduce total functions to associate type contexts to various syntax nodes:
```eqlog
Func ModuleTypeContext : ModuleNode -> TypeContext;
Func FunctionTypeContext : FunctionNode -> TypeContext;
Func StmtTypeContext : StmtNode -> TypeContext;
Func ExprTypeContext : ExprNode -> TypeContext;
...

Axiom mn : ModuleNode => ModuleTypeContext(mn)!;
Axiom fn : FunctionNode => FunctionTypeContext(fn)!;
...

```
The type context we associate to an AST node usually agrees with the type context of its parent node.
For example, the axiom
```eqlog
Axiom
    EqualsExprNode(expr, lhs, rhs)
    & expr_ctx = ExprTypeContext(expr)
    & lhs_ctx = ExprTypeContext(lhs)
    & rhs_ctx = ExprTypeContext(rhs)
    =>
    expr_ctx = lhs_ctx
    & lhs_ctx = rhs_ctx
    ;
```
propagates type contexts through equality comparisons.

Crucially, though, the type context associated to function definition literals is not the same as the type context of its parent node but an *extension* of it:
A type context that contains all the types of the base type context it extends, but that may contain additional types.
We axiomatize type context extensions as follows:
```eqlog
Pred ContextExtension : TypeContext * TypeContext;
Axiom
    ContextExtension(base, ext)
    & TypeInContext(sigma, base)
    =>
    TypeInContext(sigma, ext)
    ;
```
We can then enforce our constraints on the type contexts of functions as follows:
```eqlog
Axiom
    FunctionStmtNode(stmt, func)
    & ambient_ctx = StmtTypeContext(stmt)
    & func_ctx = FunctionTypeContext(func)
    =>
    ContextExtension(ambient_ctx, func_ctx)
    ;
```

Within the scope of a function literal, we want to treat the types of function arguments as valid monotypes, so we add them to the type context of the function:
```eqlog
Axiom
    ConsArgListNode(arg_list, _, head_ty_node, _)
    & ctx = ArgListContext(arg_list)
    & head_ty = SemanticOptType(head_ty_node)
    =>
    TypeInContext(head_ty, ctx)
    ;
```

Eqlog does not support negations, but even if it did, Datalog engines can for fundamental reasons support negations only for rules that don't apply recursively.
Because of this, we cannot check that a `Type` element occurring inside a polytype is a bound type variable during Eqlog evaluation:
That would require us to hypothesize that a type element is *not* the result of applying a type constructor, and that it is *not* in the ambient type context.
Fortunately, we will only need to check whether a type element is *unbound*, i.e., that the type element is either a type variable in the ambient type context, or that it is the result of applying a type constructor to unbound types.
To prepare for this, we close type contexts under type constructors:
```eqlog
Axiom gamma: TypeContext & sigma = VoidType() => TypeInContext(sigma, gamma);
...
Axiom
    kappa = FunctionType(dom, cod)
    & TypesInContext(dom, gamma)
    & TypeInContext(cod, gamma)
    =>
    TypeInContext(kappa, gamma)
    ;
```
We also want type contexts to be closed under *inverses* of type constructors.
This is relevant for functions with arguments whose types involve a type variable that they are not equal to.
For example, consider the `apply` function:
```typescript
function apply (f, x) {
    return f(x);
}
```
The `apply` function should have type `((a) => b, a) => b`.
Our rules for arguments of functions imply that the types of `f: (a) => b` and `x: a` are in the type context of the body of `apply`, but only the following rule, which closes type contexts under inverses of the function type constructor, lets us conclude that then also `b` must be in the type context:
```eqlog
Axiom
    TypeInContext(FunctionType(dom, cod), gamma)
    =>
    TypesInContext(dom, gamma)
    & TypeInContext(cod, gamma)
    ;
```
With all rules about type contexts in place, we can now assume that a type element in a polytype scheme is unbound if and only if it is in the ambient type context.


## Instantiation

Our function `VarTypeInExpr : Var -> GeneralType` can result into a polytype, whereas we still expect the function `ExprType : ExprNode -> Type` to result into a type in the current context, i.e., into a monotype.
This means that we need to revisit our typing rules for variable usages.

In case the variable has monotype, we can continue to equate the type of the variable expression with the type of the variable:
```eqlog
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = MonoType(ty)
    =>
    ExprType(expr) = ty
    ;
```

However, if the variable has `PolyType(ty)`, then we need to *instantiate* the type scheme `ty` first, i.e., replace the type variables in `ty` by suitable types in the current context.
We introduce the following machinery for that purpose:
```eqlog
Sort Instantiation;

Func Instantiate : Instantiation * Type -> Type;
Func InstantiateList : Instantiation * TypeList -> TypeList;
```
Each `Instantiation` element represents a (partial, non-extensional) map `Type -> Type`, and the `Instantiate` function represents the application of such maps to `Type` elements.
As usual, we add rules that enforce that the `InstantiateList` function is given by `Instantiate` on each element of a `TypeList`.

We can now associate an `Instantiation` element to every usage of a variable with polytype and equate the expression type with an instantiation of the polytype scheme.
```
Func ExprInstantiation : ExprNode -> Instantiation;

Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(_)
    =>
    ExprInstantiation(expr)!
    ;
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(sigma)
    & inst = ExprInstantiation(expr)
    & expr_ty = ExprType(expr)
    =>
    Instantiate(inst, sigma) = expr_ty
    ;
```

The typing rules that we've added in the previous post constrain the expression types of usages of polymorphic variables based on where the variables appear.
For example, if a variable is used in position of the function in an application expression, then the instantation of the variable's type must be a function type.
However, we haven't encoded any constraints based on the type scheme of the variable's polytype so far, which is our next task.

### Instantiation and type constructors

The instantiation of a function type should be a function type, the instantiation of the string type should be the string type, and so forth.
In other words, `Instantiate` should *commute* with type constructors in its `Type` argument:
```eqlog
// Instantiate commutes with the number type constructor.
Axiom
    instance_number = Instantiate(_, NumberType())
    =>
    NumberType() = instance_number
    ;

// If a funnction type is instantiated, then also the domain and
// codomain are instantiated, and instantiate commutes with the
// function type constructor.
Axiom
    Instantiate(inst, FunctionType(dom, cod))!
    =>
    InstantiateList(inst, dom)!
    & Instantiate(inst, cod)!
    ;
Axiom
    dom_instances = InstantiateList(inst, dom)
    & cod_instance = Instantiate(inst, cod)
    & func_instance = Instantiate(inst, FunctionType(dom, cod))
    =>
    FunctionType(dom_instances, cod_instance) = func_instance
    ;
...
```
Note that commutativity of the `Instantiate(inst, -)` operation with type constructors for fixed `inst` implies that the operation is uniquely determined by its value on type variables.

### Instantiation and unbound type elements

Unbound type variables cannot be freely instantiated because they are fixed by the ambient type context of the polytype already.
In case of the convoluted identity function above, this means that every instantiation of `const_x : (b) => a` must map `a` to itself.

Since we're not allowing higher-ranked types, we don't allow returning values with polytypes from functions; even if we return a variable with polymorphic type, our type checker implicitly instantiates the variable's type scheme into a monotype first.
As a consequence, polytypes cannot escape the ambient type context in which they were defined:
The type context of a usage site of a variable with polytype is always an iterated extension of the ambient type context of the where the polytype was defined.
It follows that at every usage site of a polytype, a type element occuring in the polytype scheme is unbound if and only if it is in the ambient type context of a usage site.
```eqlog
Func InstantiationTarget : Instantiation -> TypeContext;
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(_)
    & instance = ExprInstantiation(expr)
    & ctx = ExprTypeContext(expr)
    =>
    InstantiationTarget(instance) = ctx
    ;
Axiom
    sigma_instance = Instantiate(instance, sigma)
    & TypeInContext(sigma, InstantiationTarget(instance))
    =>
    sigma = sigma_instance
    ;
```

## Type errors

Our introduction of polymorphism makes it necessary to revisit two error categories: Undetermined type errors and conflicting type constraints.

### Undetermined types

Recall that in the last post we introduced a predicate
```
Pred DeterminedType : Type;
```
and added rules so that `DeterminedType(ty)` holds if and only if `ty` does not contain any type variables.
If our type checker found a `Type` element that did not satisfy `DeterminedType` after Eqlog evaluation, it would report an error due to the undetermined type.

Now that we've added support for polymorphism, this check is overly restrictive, since type variables can now appear in polytypes and the definitions of polymorphic functions.
However, we cannot remove the check entirely.
Consider the following program:
```typescript
function absurd() {
    return absurd();
}

let x = absurd();
```
The type of `absurd` is `() => b`, so its return type is entirely unrestricted.
This means that also the type of `x` is undetermined (i.e., a type variable), but we require that all let bindings must have monotype.

To adapt the `DeterminedType` predicate to polymorphism, we add the following rules mirroring our rules for type contexts:
The argument and return types of functions are always determined, and determined types are closed also under inverses of type constructors (for cases such as the `apply` function above).
```eqlog
Axiom
    ConsArgListNode(_, _, arg_type, _)
    & ty = SemanticOptType(arg_type)
    =>
    DeterminedType(ty)
    ;
Axiom
    Function(_, _, _, ret_type, _)
    & ty = SemanticOptType(cod_type)
    =>
    DeterminedType(ty)
    ;
Axiom
    DeterminedType(FunctionType(dom, cod))
    =>
    DeterminedTypes(dom)
    & DeterminedType(cod)
    ;
```

### Type conflicts

Consider the following function definiton:
```typescript
function foo (x) {
    x(x);
}
```
Our type checker infers that `x` must be have function type `(a) => b`.
Since `x` is applied to itself, we infer a type equality `a = (a) => b`.
Every instantiation of `foo` in which the argument type is a concrete type (i.e., it does not contain type variables) would result into a type conflict.
But if `foo` is never used, then our type checker will not report an error, because our rules for type conflicts fire only for concrete types, not for type variables.

To detect errors such as the `foo` function, we introduce a predicate `SmallerType : Type * Type` such that `SmallerType(sigma, tau)` holds if and only if `sigma` is structurally strictly smaller than `tau`.
```eqlog
// SmallerType is transitive.
Axiom
    SmallerType(sigma, tau)
    & SmallerType(tau, kappa)
    =>
    SmallerType(sigma, kappa)
    ;

// A function type is structurally greater than all of its domain types and
// its codomain type.
Axiom
    kappa = FunctionType(sigmas, tau)
    =>
    SmallerTypes(sigmas, kappa)
    & SmallerType(tau, kappa)
    ;
```
Here `SmallerTypes : TypeList * Type` is a predicate that holds if and only if each type in the first argument is a `SmallerType` than the second argument.

Now consider again the example of the function `foo` above.
Our rules imply that `SmallerType(a, (a) => b)` holds, hence due to the type equality `a = (a) => b`, we have `SmallerType(a, a)`.
Thus, the rule
```eqlog
Axiom SmallerType(sigma, sigma) => ConflictingTypes();
```
lets us detect this and similar errors.
