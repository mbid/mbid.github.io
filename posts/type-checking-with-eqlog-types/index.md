---
title: "Type Checking with Eqlog: Types"
date: August 05, 2023
lang: "en_US"
---

This is the third post in a series on implementing a type checker with the [Eqlog](https://github.com/eqlog/eqlog) Datalog engine.
This post deals with our semantic model of the type system, and we'll finally see some of Eqlog's distinctive features in action.
You can find the other posts here:

1. [Parsing](../type-checking-with-eqlog-parsing) [[code](https://github.com/eqlog/examples-inference/tree/parsing)]
2. [Variable binding](../type-checking-with-eqlog-variable-binding) [[code](https://github.com/eqlog/examples-inference/tree/binding)]
3. **Types [[code](https://github.com/eqlog/examples-inference/tree/types)] (this post)**
4. Expression typing [[code](https://github.com/eqlog/examples-inference/tree/expr-types)]
5. Function literal typing [[code](https://github.com/eqlog/examples-inference/tree/function-types)]
6. Hindley-Milner polymorphism [[code](https://github.com/eqlog/examples-inference/tree/hindley-milner)]

As usual, the code we discuss in this post is available as a [branch](https://github.com/eqlog/examples-inference/tree/types) in the associated repository.

Our goal is to define a *semantic* model of our type system that complements the *syntactical* `TypeNode` AST nodes we've already discussed in the first post on parsing.
Our parser generates fresh `TypeNode` elements for each piece of source code representing a type.
As a consequence, there can be many different `TypeNode` elements representing the same type.
For example, the AST of the program
```typescript
function foo (num): boolean {
    let e: boolean = num == 5;
    return e;
}
```
contains two different `BooleanTypeNode` elements, one for each occurrence of the string `boolean`.
For type checking and inference we will eventually have to verify that certain types match, so `TypeNode` elements are unsuitable for this.

Instead, we introduce a new sort `Type` to represent *semantic* types.
In order to be useful for type checking and inference, semantic types should have the following properties:

1. There should be at most one `Type` element that represents `boolean`; similarly for `string`, `number` and so forth.
2. Various AST nodes should have associated `Type` element, e.g. `TypeNode`, `ExprNode` (the type of an expression) and `FunctionNode` (the type of a function literal).
3. Every variable binding should have an associated `Type` element.

All of these requirements are *functional* in nature:
For example, requirement 2 says that there should be a total function `TypeNode -> Type`.

## Functions and equality in Eqlog

Eqlog has a `Func` keyword that lets us declare (partial) functions, for example in the following Eqlog program:
```
Sort M;
Func Mul : M * M -> M;
```
Eqlog represents functions as [graph relations](https://en.wikipedia.org/wiki/Graph_of_a_function), i.e., as sets of value-result tuples.
Thus, the binary `Mul` function is represented in the same way as a ternary predicate `MulGraph : M * M * M`, and `MulGraph(a, b, c)` holds if and only if `Mul(a, b) = c`.
However, not every relation corresponds to a function:
This is only so if the last component is uniquely determined by the other components.
For the ternary `MulGraph` predicate, this means that the following axiom must hold:
```eqlog
Axiom MulGraph(x, y, z_0) & MulGraph(x, y, z_1) => z_0 = z_1;
```
Eqlog implicitly adds such *functionality axioms* for every function declaration.

The conclusion of a functionality axiom is an equality, and equalities are not part of standard Datalog either.
Eqlog implements equalities as follows.
Recall that Eqlog repeatedly matches the premise of each axiom on current data in the model and then adds the conclusion of the axiom for each match.
An equality `x = y` in the premise restricts matches to those where `x` and `y` are interpreted as the same element.

Equalities in the conclusion are more interesting:
Eqlog models contain union-find data structure to track equality among elements.
When Eqlog has matched the premise of an axiom and there is an equality `x = y` in the conclusion, then it merges the equivalence classes of the elements that are matched for `x` and `y`.

Crucially, Eqlog matches premises with respect to the equivalence relations stored in these union-find data structures.
For example, there are two occurrences of the variable `x` in the premise `MulGraph(x, y, z_0) & MulGraph(x, y, z_1)` of our functionality axiom above.
A match of this premise is allowed to interpret these two occurrences by distinct (in the sense that they are represented by different IDs) elements as long as the two elements are in the same equivalence class.
This means that more matches become possible after an equality has been found.

For example, suppose we have a model of our theory above given by four distinct elements `a, b, c, d` and the following `MulGraph` entries:
```eqlog
MulGraph(a, b, c)
MulGraph(a, b, d)
MulGraph(c, a, b)
MulGraph(d, a, a)
```
Let's walk through Eqlog evaluation.
At first, the only non-trivial (i.e. `z_0 != z_1`) match for the premise of the functionality axiom is `(a, b, c)`, `(a, b, d)`.
The conclusion of the functionality axiom then asserts `c = d`.
This means that `(c, a, b)`, `(d, a, a)` is a valid match now.
From this it follows that `b = a` must hold.
At this point there are only trivial matches, where the conclusion already holds, so evaluation stops.

## Type constructors

We return to defining our semantic model of types.
Our type system has the following type constants, which we declare as nullary functions:
```eqlog
Func VoidType : Type;
Func BooleanType : Type;
Func NumberType : Type;
Func StringType : Type;
```
The functionality axiom for nullary functions asserts that there is at most one such constant.
For example, if `sigma = VoidType()` and `tau = VoidType()`, then `sigma = tau`.

The only type constructor with non-trivial arguments in our toy language is the constructor of function types:
It takes as parameters a list of types for the domain (the types of arguments) and a type for the codomain (the result type).
To model a *list* of semantic types, we use a similar trick as for syntax nodes:
```eqlog
Sort TypeList;
Func NilTypeList : TypeList;
Func ConsTypeList : Type * TypeList -> TypeList;
```
However, here we're using Eqlog functions instead of predicates to represent empty lists and the cons operation.
This has the effect that Eqlog considers all `NilTypeList()` elements as equal, and that two type lists agree if they have equal heads and equal tails.
The analogous properties would be undesirable for AST nodes because in the AST we want to distinguish `NilTypeListNode` elements that that appear at different locations in source code.

We can now declare the function type constructor:
```eqlog
Func FunctionType: TypeList * Type -> Type;
```

## Total functions

One of our requirements on the semantic `Type` sort was that there should be functions associating `Type` elements to various AST nodes.
To that end, we declare the following functions:
```eqlog
Func SemanticType : TypeNode -> Type;
Func SemanticOptType : OptTypeNode -> Type;
Func SemanticArgTypes : ArgListNode -> TypeList;

// The types of expressions and function literals:
Func ExprType : ExprNode -> Type;
Func ExprTypes : ExprNodeList -> TypeList;
Func FunctionNodeType : FunctionNode -> Type;
```
However, Eqlog's `Func` keyword introduces *partial* functions, whereas we required *total* functions.

To enforce that these functions are total, we can use Eqlog's exclamation mark operator `!` like so:
```eqlog
Axiom tn: TypeNode => SemanticType(tn)!;
Axiom otn: OptTypeNode => SemanticOptType(otn)!;
Axiom ag: ArgListNode => SemanticArgTypes(ag)!;

Axiom expr: ExprNode => ExprType(expr)!;
Axiom exprs: ExprListNode => ExprTypes(exprs)!;
Axiom func: FunctionNode => FunctionNodeType(func)!;
```
These axioms make use of another Eqlog construct that we haven't seen before:
*Sort quantifiers*, for example `tn: TypeNode`, match every element of a given sort.
Sort quantifiers are needed in situations where a variable is not constrained by a predicate or function in the premise.

You can read the exclamation mark operator as "is defined".
During evaluation, when Datalog has matched the premise of an axiom and encounters an atom `t!` in the conclusion, it first checks whether the expression `t` and all its subexpressions are already defined.
If they are, then nothing needs to be done.
If some subexpression or `t` itself is not defined yet, then Eqlog adjoins fresh elements to the model to represent them.
For example, the axiom for `SemanticType` causes Eqlog to adjoin a fresh `Type` element `t` to the model for each `TypeNode` element `tn` and insert the tuple `(tn, t)` into the graph of `SemanticType`.

We have to be very careful about axioms involving the exclamation mark operator though, because such axioms can lead to non-termination.
Consider for example this Eqlog program that encodes the natural numbers:
```eqlog
Sort N;
Func Z : N;
Func S : N -> N;
Axiom => Z()!;
Axiom n : N => S(n)!;
```
Evaluation on even the empty model structure does not terminate for this Eqlog program, since Eqlog will adjoin more and more elements `Z(), S(Z()), S(S(Z())), ...` without halting.
In our case, termination is guaranteed because the number of `Type` elements adjoined during evaluation is bounded by the number of AST node elements, and AST node elements are only added to the model before Eqlog evaluation, by the parser.

## Translating type nodes to semantic types

Our `SemanticType`, `SemanticOptType` and `SemanticArgTypes` functions associate semantic type (list) elements to certain AST nodes.
However, we haven't set up any rules governing these functions.
For example, we need an axiom that enforces that the semantic type associated to a `BooleanTypeNode` is the semantic `BooleanType`.
The following axiom accomplishes this:
```eqlog
Axiom
    BooleanTypeNode(tn)
    & sigma = SemanticType(tn)
    =>
    BooleanType() = sigma
    ;
```
There are analogous axioms that encode the relationship between `NumberTypeNode` and `NumberType`, `VoidTypeNode` and `VoidType` and so forth.
The semantic type of a function type AST node depends recursively on the semantic types associated to domain and codomain AST nodes:
```eqlog
Axiom
    FunctionTypeNode(tn, args, codomain)
    & dom_types = SemanticArgTypes(args)
    & cod_type = SemanticType(codomain)
    & function_type = SemanticType(tn)
    =>
    FunctionType(dom_types, cod_type) = function_type
    ;
```

The `OptTypeNode` sort represents optional type node elements.
Our parser emits such elements for example in argument lists of function literals, where argument variables can have an optional type annotation.
In case an `OptTypeNode` element is given by a `TypeNode`, we enforce that `SemanticOptType` agrees with `SemanticType`:
```eqlog
Axiom
    SomeOptTypeNode(otn, tn)
    & sigma = SemanticType(tn)
    =>
    SemanticOptType(otn) = sigma
    ;
```
There are no axioms that govern how `SemanticOptType` interacts with `NilOptTypeNode` elements.
But by our totality axioms above, `SemanticOptType` is still defined on `NilOptTypeNode` elements.
The resulting `Type` elements will thus for now be undetermined, i.e., not equal to a type obtained from one of the type constructors.
However, we will later add axioms that imply indirectly that a `SemanticOptType` element must agree with certain other `Type` elements.
For example, we will add axioms that assert that the type of a variable must on the one hand be equal to the `SemanticOptType` of its optional type annotation, and on the other hand that the variable must be a `boolean` if it is used as condition of an `if` statement.

The `SemanticArgTypes : ArgListNode -> TypeList` should be given by mapping the `SemanticTypeOpt` function on optional type annotations, which we enforce as follows:
```eqlog
Axiom
    NilArgListNode(al)
    & semantic_types = SemanticArgTypes(al)
    =>
    NilTypeList() = semantic_types
    ;
Axiom
    ConsArgListNode(al, _, head_type, tail)
    & semantic_head = SemanticOptType(head_type)
    & semantic_tail = SemanticArgTypes(tail)
    & semantic_types = SemanticArgTypes(al)
    =>
    semantic_types = ConsTypeList(semantic_head, semantic_tail)
    ;
```

## Types of variables

In the previous post on variable binding, we set up predicates `VarInX : Var * X` for various AST node sorts `X`, for example `ExprNode`, and we added axioms that enforce `VarInX(var, node)` holds precisely when the variable `var` is accessible within `node`.
For type checking, this information is not enough:
We need to know which type the variable has.

Thus, we change the `VarInX` predicates to functions `VarTypeInX : Var * X -> Type`:
These functions should be defined on a pair of `(var, node)` if `var` is in scope for `node`, and if so, then `VarTypeInX(var, node)` is the type of the variable.
This change makes it necessary to adapt the existing propagation axioms so that they also propagate the type of the variable.
For example, the axiom that propagates variable bindings through `StmtNodeList` elements now looks like this:
```eqlog
Axiom
    ConsStmtListNode(stmts, _, tail)
    & sigma = VarTypeInStmts(var, stmts)
    =>
    VarTypeInStmts(var, tail) = sigma
    ;
```
Outside of propagation axioms we don't use results of the `VarTypeInX` functions for now; we just hypothesize or assert that one of the `VarInType` functions is defined on certain pairs of variable and node.
This will change in the next posts, where we collect typing constraints on variables based on their declaration and usage.

## Injectivity of type constructors

In most type systems, type constructors have *disjoint ranges*, and each type constructor is separately *injective*.
The disjoint range property means that types obtained using different type constructors do not agree.
In our case, this means for example that the `boolean` type is different from the `string` type, and that the `void` type is different from all function types.
Injectivity is only relevant for type constructors with arguments (so no type constants such as `number`).
For our toy language, injectivity thus only applies to function types, where it says that if two functions types are equal, then they must have the same domain types and the same codomain types.

Evaluation of our Eqlog program can currently not violate these properties, but this will change in the next posts due to typing constraints on expression or variables.
For example, we will enforce that the type of a variable that is used as condition for an `if` statement must be of type `boolean`, and that it must also be of type `string` if it is initialized with a string literal.
We enforce these constraints by imposing equalities on the `ExprType` of expressions, which by the functionality axioms can lead to a violation of the disjoint range or injectivity properties.
These violations only happen for ill-typed programs, so our type checker should report a type error in these situations.
Similarly to the `VariableShadowing` predicate that we used to report variable shadowing, we introduce a predicate
```eqlog
Pred ConflictingTypes: ();
```
that our Eqlog program should populate if it detects a violation of injectivity or the disjoint ranges property.

Detecting a violation of the disjoint ranges property is fairly straightforward:
```eqlog
Axiom VoidType() = BooleanType() => ConflictingTypes();
Axiom VoidType() = NumberType() => ConflictingTypes();
Axiom VoidType() = StringType() => ConflictingTypes();
Axiom VoidType() = FunctionType(_, _) => ConflictingTypes();

Axiom BooleanType() = NumberType() => ConflictingTypes();
// ...
```

We could encode injectivity of the function type constructor like this:
```eqlog
Axiom
    FunctionType(sigmas_0, tau_0) = FunctionType(sigma_1, tau_1)
    =>
    sigmas_0 = sigmas_1
    & tau_0 = tau_1
    ;
```
Instead, we'll go with an approach that will help us also later:
Axiomatizing inverse functions to `FunctionType` in each argument.

When we collect constraints on the types of expressions, we'll eventually have the need to assert that some type `kappa` is a function type, i.e., that there should *exist* `domain` and `codomain` such that `kappa = FunctionType(domain, kappa)`.
However, Eqlog does intentionally not support existential quantification, since [this would make Eqlog evaluation semantically ill-behaved](https://www.mbid.me/eqlog-semantics/).
To work around this, we introduce functions `DomainTypes : Type -> TypeList` and `CodomainType : Type -> Type` as inverse functions for the two arguments of `FunctionType`:
```eqlog
Axiom DomainTypes(tau)! => CodomainType(tau)!;
Axiom CodomainType(tau)! => DomainTypes(tau)!;
Axiom
    kappa = FunctionType(sigmas, tau)
    =>
    DomainTypes(kappa) = sigmas
    & CodomainType(kappa) = tau
    ;
Axiom
    sigmas = DomainTypes(kappa)
    & tau = CodomainType(kappa)
    =>
    FunctionType(sigmas, tau) = kappa
    ;
```
To assert that a given type is a function type, it now suffices to assert that `DomainTypes` or `CodomainType` is defined on it.
Since functions are injective if and only if they have an inverse function on their range, the presence of `DomainTypes` and `CodomainType` makes the explicit injectivity axiom above redundant.

Due to injectivity of `FunctionType : TypeList * Type -> Type`, it can happen that we equate `TypeList` elements during evaluation.
If type lists are equal, then they should contain the same types in the same order.
Our Eqlog program equates type lists with different lengths only if the input source code contains a variable number mismatch, e.g. a call of a function with the wrong number of arguments, so we want to report a type error in this case.
We can enforce these condition on type lists like so:
```eqlog
Axiom NilTypeList() = ConsTypeList(_, _) => ConflictingTypes();
Axiom
    ConsTypeList(head_0, tail_0) = ConsTypeList(head_1, tail_1)
    =>
    head_0 = head_1
    & tail_0 = tail_1
    ;
```
