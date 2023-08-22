---
title: "Type Checking with Eqlog: Typing"
date: August 06, 2023
lang: "en_US"
---

This is the fourth post in a series on implementing a type checker with the [Eqlog](https://github.com/eqlog/eqlog) Datalog engine.
In this post, we impose constraints on types of expressions and functions.
By the end, we'll have a full type checker for simple types.
You can find the other posts here:

1. [Parsing](../type-checking-with-eqlog-parsing) [[code](https://github.com/eqlog/examples-inference/tree/parsing)]
2. [Variable binding](../type-checking-with-eqlog-variable-binding) [[code](https://github.com/eqlog/examples-inference/tree/binding)]
3. [Types](../type-checking-with-eqlog-types) [[code](https://github.com/eqlog/examples-inference/tree/types)]
4. **[Typing](../type-checking-with-eqlog-typing) [[code](https://github.com/eqlog/examples-inference/tree/typing)] (this post)**
5. [Polymorphism](../type-checking-with-eqlog-polymorphism) [[code](https://github.com/eqlog/examples-inference/tree/hindley-milner)]

As usual, the code we discuss in this post is available as a [branch](https://github.com/eqlog/examples-inference/tree/typing) in the associated repository.

In the previous post on the `Type` sort we introduced the following:

- Type constructors such as `BooleanType : Type` and `FunctionType : TypeList -> Type`.
- Variable bindings associate types to variables in the current scope.
- Our Eqlog program populates the `ConflictingTypes` predicate if we equate types that cannot be equal, for example if `BooleanType() = NumberType()`.
- Total functions that associate `Type` elements to various AST nodes, in particular the `ExprType : ExprNode -> Type` and `FunctionNodeType : FunctionNode -> Type` functions.
  As of now, there are no laws these functions:
  The output of these functions is always an opaque `Type` element.

Our main task for this post is to impose constraints on the `ExprType` and `FunctionNodeType` functions.

## Types of expressions

We begin with the constraints on the `ExprType` function.
Here are the rules for constants:
```eqlog
Axiom FalseExprNode(expr) & et = ExprType(expr) => et = BooleanType();
Axiom TrueExprNode(expr) & et = ExprType(expr) => et = BooleanType();
Axiom
    StringLiteralExprNode(expr, _)
    & et = ExprType(expr)
    =>
    et = StringType()
    ;
// ...
```

And this is the rule for equality comparisons:
```eqlog
Axiom
    EqualsExprNode(eq, lhs, rhs)
    & eq_type = ExprType(eq)
    & lhs_type = ExprType(lhs)
    & rhs_type = ExprType(rhs)
    =>
    eq_type = BooleanType()
    & lhs_type = rhs_type
    ;
```
Thus, if `eq` is the expression `lhs == rhs`, then `eq` is of `boolean` type, and the types of `lhs` and `rhs` agree.

Note that we do not "check", "verify" or even "demand" that the types of `lhs` and `rhs` agree --- we simply assert that the types *are* equal.
So what happens if there is an equality expression such as `true == "asdf"` where the types of the two operands do not match?
As usual, Eqlog will enforce that our rules hold and thus equate the types of `true` and `"asdf"`.
This means that now `BooleanType() == StringType()` holds, which will make the rule
```eqlog
Axiom BooleanType() == StringType() => TypeConflict();
```
fire, so that `TypeConflict` holds.
Our Rust glue code then finds that the `TypeConflict` predicate is populated after evaluation and reports an error.

Recall the `VarTypeInExpr : Var * ExprNode -> Type` function:
`VarTypeInExpr(var, expr)` is defined if and only if there is a variable in scope for `expr`, and if so the result of `VarTypeInExpr` is the type of the variable.
With this function at hand, the typing rule of variable usage is as follows:
```eqlog
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = sigma
    =>
    ExprType(expr) = sigma
    ;
```
Note that it is important that we only hypothesize, but not assert, that `VarTypeInExpr` is defined on `var` and `expr`.
If we moved the `VarTypeInExpr` atom from the premise to the conclusion, then every usage of a variable would *introduce* a variable binding, which would make it impossible to detect usages of undeclared variables.

The most complex axiom governing `ExprType` is the rule for function applications.
If `AppExprNode(expr, func, args)` holds, then `expr` is an expression of the form `func(args)`.
In this situation `func` must be a function type with domain and codomain matching the types of `args` and `expr`.
Here it comes in handy that we've encoded injectivity of `FunctionType` by adding inverse functions `DomainType` and `CodomainType`:
If either of those functions is defined on a type `kappa`, then also the other function is defined, and `kappa = FunctionType(DomainType(kappa), CodomainType(kappa))`. 
We can thus encode typing constraints arising from function application as follows:
```eqlog
Axiom
    AppExprNode(expr, func, args)
    & res_ty = ExprType(expr)
    & func_ty = ExprType(func)
    & arg_tys = ExprTypes(args)
    =>
    arg_tys = DomainTypes(func_ty)
    & res_ty = CodomainType(func_ty)
    ;
```

Some of the constraints on expressions also arise from usage in statements.
For example, the expression used in the condition for a `while` loop or an `if` statement must be Boolean.

## Types of function literals

Next we consider the constraints on `FunctionNodeType : FunctionNode -> Type`.
To recap, the `FunctionNode` sort represents function literals, for example this source code fragment:
```typescript
function foo(x: number, y): boolean {
  return x == y;
}
```
Subnodes of the function literals are available via the `Function` predicate:
```
Pred Function : FunctionNode * Var * ArgListNode * OptTypeNode * StmtListNode;
// Function(func_node, name, args, result_type, body)
```

Our first constraint on `FunctionNodeType` is that it is always a `FunctionType`, which is equivalent to it having `DomainTypes` and a `CodomainType`:
```eqlog
Axiom kappa = FunctionNodeType(_) => DomainTypes(kappa)! & CodomainType(kappa)!;
```

### Argument types

The domain type of a function literal is given by the types of its arguments:
```eqlog
Axiom
    Function(func, _, args, _, _)
    & dom = DomainTypes(FunctionNodeType(func))
    =>
    SemanticArgTypes(args) = dom
    ;
```
Recall from the last post that `SemanticArgTypes : ArgListNode -> TypeList` is given by mapping the `SemanticOptType` function on the type annotations of each argument.
In that post we also added a rule to equate the `SemanticOptType` of a argument variable declaration with the `VarTypeInArgList` for that variable:
```eqlog
// Every function argument introduces a variable.
Axiom
    ConsArgListNode(_, var, ty_annot, tail)
    & ty = SemanticOptType(ty_annot)
    =>
    VarTypeInArgList(var, tail) = ty
    ;
```
Since we propagate the result of the `VarTypeInArgList` function into the function body, this means that the type of an argument as given by `FunctionNodeType` is tied to the type of the argument variable inside the function body.

### Return type annotations

The return type of a function should match an explicit return type annotation:
```eqlog
Axiom
    Function(func, _, _, cod_annot, _)
    & func_ty = FunctionNodeType(func)
    & cod_ty = SemanticOptType(cod_annot)
    =>
    CodomainType(func_ty) = cod_ty
    ;
```

### Types in return statements

We need rules that tie the return type of a function to the type of expressions that are returned from the function body.
For example, we should detect that the return type of the function
```typescript
function foo () { return 5; }
```
is `number` even though the return type annotation is missing.
If a function body contains multiple return statements with expressions of different types, then we should report a type conflict error, for example here:
```typescript
function bar (c) {
  if (c) {
    return 5;
  } else {
    return 'xyz';
  }
}
```

The body of a function is a `StmtListNode` element, so we introduce a `ReturnsType : StmtListNode -> Type` function for this analysis:
We should have `ReturnsType(stmts) = sigma` if and only if one of the (sub)statements in `stmts` is an explicit return statement with an expression of type `sigma`.
The functionality axiom for `ReturnsType` enforces that there can be at most one such type `sigma`.
This means that conflicting types in return statements surface the same way as for expressions:
They lead to an equality such as `BooleanType() == NumberType()`, which will then trigger one of our axioms that populates the `ConflictingTypes` predicate.


Our AST uses different nodes for return statements with or without an expression:
```typescript
return 5; // Returns the expression `5`.
return; // Equivalent to `return ();`.
```
The axiom relating `ReturnsTypes` to return statements with expressions is
```eqlog
Axiom
    ConsStmtListNode(stmts, head, _)
    & ReturnStmtNode(head, return_value)
    & et = ExprType(return_value)
    =>
    ReturnsType(stmts) = et
    ;
```
and the axiom for return statements without expressions is similar.

We also need axioms that propagate `ReturnsType` through statement nodes:
```eqlog
// If the tail of a statement list that can return a type, then the full
// statement list can also return that type.
Axiom
    ConsStmtListNode(stmts, _, tail)
    & rt = ReturnsType(tail)
    =>
    ReturnsType(stmts) = rt
    ;

// An if statement can return a type if at least one of its two branches
// returns that type.
Axiom
    ConsStmtListNode(stmts, head, _)
    & IfStmtNode(head, _, true_branch, _)
    & rt = ReturnsType(true_branch)
    =>
    ReturnsType(stmts) = rt
    ;
Axiom
    ConsStmtListNode(stmts, head, _)
    & IfStmtNode(head, _, _, false_branch)
    & rt = ReturnsType(false_branch)
    =>
    ReturnsType(stmts) = rt
    ;

// ...
```

We can now enforce that the return type of a function matches the type of returned expressions like so:
```eqlog
Axiom
    Function(func, _, _, _, body)
    & ft = FunctionNodeType(func)
    & rt = ReturnsType(body)
    =>
    rt = CodomainType(ft)
    ;
```

### Implicit `void` return

If control flow reaches the end of a function body, then the function implicitly returns `()`, so the return type of the function must be `void`.
Unfortunately, as with most questions concerning control flow, it is undecidable whether this rule applies to a given function.
But we can over-approximate an answer:
There will be cases where our analysis believes that control flow can reach the end of a function when that is not actually the case.
Our simple heuristic is based on the following rules:

- Control cannot flow past return statements.
  If our toy language had assertions or exceptions, we could also consider these here.
- Control can only flow past an `if` statement if it can flow through at least one of the two branches.

In particular, we don't attempt to analyze branching conditions to infer whether some branch is unreachable.

Our analysis works with the
```
Pred CanProceedStmt : StmtNode;
Pred CanProceedStmts : StmtListNode;
```
predicates:
We should have `CanProceedStmts(stmt)` if control can flow past `stmt`, and `CanProceedStmts(stmts)` if control can flow past all the statements in `stmts`:
```eqlog
Axiom LetStmtNode(stmt, _, _, _) => CanProceedStmt(stmt);
Axiom ExprStmtNode(stmt, _) => CanProceedStmt(stmt);
// ... and so forth for most other statement nodes but not return statements.

Axiom
    IfStmtNode(stmt, _, true_branch, _)
    & CanProceedStmts(true_branch)
    =>
    CanProceedStmt(stmt)
    ;
// ... and similarly for `false_branch`.
```

Finally, we enforce an implicit `void` return type on a function if control can flow past its body:
```eqlog
Axiom
    Function(func, _, _, _, body)
    & CanProceedStmts(body)
    & cod = CodomainType(FunctionNodeType(func))
    =>
    cod = VoidType()
    ;
```

## Undetermined types

Our type checker is now almost complete:
In case there are conflicting typing constraints on an expression or function, then the `ConflictingTypes` is populated.
However, we have not considered *undetermined* types yet.
Keeping in mind that our language does not have generics yet, which types should `x` and `id` have in the following program?
```typescript
function id(x) {
  return x;
}
```

To detect situations such as this one and report an error, we introduce predicates
```eqlog
Pred DeterminedType : Type;
Pred DeterminedTypes : TypeList;
```
which should hold only for types that are either base types or obtained from base types with type constructors:
```eqlog
Axiom sigma = VoidType() => DeterminedType(sigma);
Axiom sigma = BooleanType() => DeterminedType(sigma);
Axiom
    sigma = FunctionType(dom, cod)
    & DeterminedTypes(dom)
    & DeterminedType(cod)
    =>
    DeterminedType(sigma)
    ;
...
```

If Eqlog supported negations, we could now add an axiom such as
```eqlog
Axiom NOT DeterminedType(_) => UndeterminedType();
```
and check after Eqlog evaluation whether `UndeterminedType` holds.
As Eqlog does not have negations (yet?), we have to encode this axiom directly in Rust:
```rust
fn has_undetermined_type(p: &Program) -> bool {
    p.iter_type().any(|sigma| !p.determined_type(sigma))
}
```

This concludes the implementation of our first version our type checker.
Thanks for following along!
But the series is not done yet:
The next post will introduce generics, and we'll implement Hindley-Milner type inference in Eqlog.
With generics, the function `id` above will have inferred type `forall a. (x: a) => a`, so we won't report an error anymore.
