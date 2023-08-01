---
title: "Type Checking with Eqlog: Variable Binding"
date: "August 01, 2023"
lang: "en_US"
---

This is the second post in a series on implementing a type checker in [Eqlog](https://github.com/eqlog/eqlog).
This post deals with *variable binding*, or *name resolution*.
You can find the other posts here:

1. [Parsing](../type-checking-with-eqlog-parsing) [[code](https://github.com/eqlog/examples-inference/tree/parsing)]
2. **Variable binding [[code](https://github.com/eqlog/examples-inference/tree/binding)] (this post)**
3. Types [[code](https://github.com/eqlog/examples-inference/tree/types)]
4. Expression typing [[code](https://github.com/eqlog/examples-inference/tree/expr-types)]
5. Function literal typing [[code](https://github.com/eqlog/examples-inference/tree/function-types)]
6. Hindley-Milner polymorphism [[code](https://github.com/eqlog/examples-inference/tree/hindley-milner)]

As usual, the code we discuss in this post is available as a [branch](https://github.com/eqlog/examples-inference/tree/binding) in the associated repository.

So far we haven't done any computations in Eqlog, but that's going to change in this post.
Our goal is to detect two kinds of errors:

1. Shadowing:
   Multiple definitions of a variable in the same scope.
2. Undeclared variables:
   Usage of a variable without prior declaration.

The role of Eqlog here will be to propagate information about whether a variable is in scope through the syntax tree.

## Eqlog evaluation

Eqlog evaluates *axioms*, or *rules*.
We can declare axioms as follows:
```eqlog
Axiom <premise> => <conclusion>;
```
Premise and conclusion must be conjunctions of *atoms*.
In this blog post, we'll only need predicate atoms, which are given by application of predicates to variables.
For example, here is an Eqlog program for computing the transitive closure of a graph:
```eqlog
Sort Vertex;
Predicate Edge : Vertex * Vertex;
Axiom Edge(x, y) & Edge(y, z) => Edge(x, z);
```
During evaluation, Eqlog repeatedly matches the premise of each axiom on data in the model and then adds the conclusion.
Evaluation stops when nothing changes anymore, i.e., when all conclusions already hold.
In case of the above transitive closure program above, Eqlog will enumerate all pairs of entries `(a, b)` and `(b, c)` in the `Edge` relation and then insert the tuple `(a, c)` into `Edge`.
Evaluation stops when all tuples `(a, c)` found this way are already in `Edge`, i.e., when the edge relation is transitive.

As for our goal of detecting variable binding errors, we add for each relevant AST node `X` a predicate `VarInX : Var * X` to represent whether a variable is in scope for a given node:
```eqlog
Pred VarInStmts : Var * StmtListNode;
Pred VarInFunction : Var * FunctionNode;
Pred VarInArgList : Var * ArgListNode;
Pred VarInExpr : Var * ExprNode;
Pred VarInExprs : Var * ExprListNode;
```
We're going to add axioms that populate these predicates based on the program's AST.
Note that variable bindings are not relevant in AST nodes that contain only types, which is why there is no need for a `VarInType : Var * TypeNode` predicate.
Since our grammar is such that every `StmtNode` appears as head of some `StmtNodeList`, a separate `VarInStmt : Var * StmtNode` predicate would be redundant, but not every `ExprNode` is part of an `ExprNodeList`.

## Propagation

Our first axioms are concerned with *propagating* variable bindings into subnodes.
There is one such axiom for each node type, for example `IfStmtNode` or `ConsExprNode`.
The premise of each of these axioms matches the respective node and a variable that is in scope for the node.
The conclusion asserts that this variable is also in scope for all subnodes.
Here are some examples:
```eqlog
// If variable is in scope for a list of statements, then it is also in scope
// for the tail of the list.
Axiom
    ConsStmtListNode(stmts, _, tail)
    & VarInStmts(var, stmts)
    =>
    VarInStmts(var, tail)
    ;

// If a variable is in scope for an if statemennt, then it is also in scope for
// the branching condition and the two branches.
Axiom
    ConsStmtListNode(stmts, head, _)
    & IfStmtNode(head, cond, true_branch, false_branch)
    & VarInStmts(var, stmts)
    =>
    VarInExpr(var, cond)
    & VarInStmts(var, true_branch)
    & VarInStmts(var, false_branch)
    ;

// If a variable is in scope for an equality comparison expression, then it is
// also in scope for the left-hand and right-hand side expressions.
Axiom
    EqualsExprNode(expr, lhs, rhs)
    & VarInExpr(var, expr)
    =>
    VarInExpr(var, lhs)
    & VarInExpr(var, rhs)
    ;
```

### Propagating function arguments into function bodies

You might wonder what the `FunctionNode` sort represents.
Elements of this sort are what our parser emits for *function literals*, so for example for the following source code fragment:
```typescript
function foo (x: number): number { return x; }
```
The subnodes (function name, arguments, optional return type annotation, body) of a function node element are available via the
```
Function : FunctionNode * Var * ArgListNode * OptTypeNode * StmtListNode
```
predicate.

Function literals can appear in two contexts:
As function statements or as function expressions.
In both cases, variables available in the ambient scope of the function literal are also available inside the function.
We enforce this by the following axioms:
```
Axiom
    ConsStmtListNode(stmts, head, _)
    & FunctionStmtNode(head, func)
    & VarInStmts(var, stmts)
    =>
    VarInFunction(var, func)
    ;
Axiom
    FunctionExprNode(expr, func)
    & VarInExpr(var, expr)
    =>
    VarInFunction(var, func)
    ;
```

Arguments of function literals are represented as `ArgListNode` elements.
To make sure that we can detect variable shadowing for arguments, i.e., when a function argument has the same name as an ambient variable, we need to propagate variables in scope outside of the function into the argument list node:
```eqlog
Axiom
    Function(func, _, args, _, _)
    & VarInFunction(var, func)
    =>
    VarInArgList(var, args)
    ;
```
Thus, the `VarInArgList` predicate keeps track of all variables in scope where the argument list appears, not just of the variables that are listed in the argument list.
An `ArgListNode` is either empty, or it is given by a head variable with optional type annotation and a tail `ArgListNode`.
We consider the variable introduced in the head of an argument list to be in scope for the tail of the argument list, but not for the argument list itself.
This means that the following naturally seeming rule would, by our previous axiom, propagate ambient variable bindings into the function body, but it would not propagate the variables introduced in the argument list itself:
```eqlog
Axiom
    Function(_, _, args, _, body)
    // This doesn't quite work:
    & VarInArgList(var, args)
    =>
    VarInStmts(var, body)
    ;
```
Instead, we need a way to access the *end* of an argument list, i.e., the `NilArgList` from which a given argument list can be obtained by repeated consing.
This is the purpose of the following `ArgListEnd` predicate and its axioms:
```eqlog
Pred ArgListEnd : ArgListNode * ArgListNode;
Axiom NilArgListNode(arg_list) => ArgListEnd(arg_list, arg_list);
Axiom
    ConsArgListNode(arg_list, _, _, tail)
    & ArgListEnd(tail, end)
    =>
    ArgListEnd(arg_list, end)
    ;
```
We can now state a fixed version of the axiom that propagates variables introduced in argument lists into bodies:
```eqlog
Axiom
    Function(_, _, args, _, body)
    & ArgListEnd(args, args_end)
    & VarInArgList(var, args_end)
    =>
    VarInStmts(var, body)
    ;
```

## Base cases

So far we've only propagated existing variable bindings through their scope, but there are no axioms that initially populate the `VarInX` predicates.
We'll add such axioms next, one for each AST node that introduces a new variable.

### Let statements

```typescript
// A let statement.
let x: number = 5;
```
Let statements are represented by our parser as entries in the `LetStmtNode : StmtNode * Var * OptTypeNode * ExprNode` predicate.
Like every other statement, let statements always appear as first element in some `StmtNodeList`, and variables introduced by the let statements are in scope for all succeeding statements:
```eqlog
Axiom
    ConsStmtListNode(_, head, tail)
    & LetStmtNode(head, var, _, _)
    =>
    VarInStmts(var, tail)
    ;
```

### Function statements

Similarly to let statements, function statements introduce the variable of the function name into the scope of succeeding statements of the same block:
```
Axiom
    ConsStmtListNode(_, head, tail)
    & FunctionStmtNode(head, func)
    & Function(func, var, _, _ ,_)
    =>
    VarInStmts(var, tail)
    ;
```

### Function arguments

As mentioned before when we discussed propagation of function arguments, we consider a variable introduced in the head of an argument list to be in scope for the tail of the argument list.
This is enforced by the following axiom:
```eqlog
Axiom
    ConsArgListNode(_, var, _, tail)
    =>
    VarInArgList(var, tail)
    ;
```

### Function names

The name of a function is not only available outside of the function (in case of function statements), but also in its own body.
This enables recursive function definitions.

Note that we already propagate the variables in scope for the `ArgListNode` of a function into the body.
Thus, if we enforce that the name of the function is in scope of the argument list, then the name will also be available in the body:
```eqlog
Axiom Function(_, var, arg, _ ,_) => VarInArgList(var, arg);
```
The advantage of this approach over making the function name available in the body directly is that it enables us to detect shadowing of the function name by a function argument, for example here:
```typescript
function foo (foo) {}
```

## Error reporting

### Shadowing

Shadowing occurs when a variable is declared twice within the same scope.
Our Eqlog program reports shadowing to our Rust glue code via the following nullary predicate:
```
Pred VariableShadowing: ();
```
The only possible entry for a nullary predicate is the empty tuple, but the predicate can also be empty.
We can thus think of nullary predicates as truth values, or as Boolean values.
Our Eqlog program must ensure that the `VariableShadowing` predicate is populated whenever the program contains an instance of shadowing.
Because we do not add an entry to the predicate from outside the Eqlog program, we know that if the predicate holds after Eqlog evaluation, then the program must contain an instance of shadowing.

There is an axiom that concludes `VariableShadowing` for each of the base cases we considered in the previous section.
For example, the axiom for let statements is as follows:
```eqlog
Axiom
    LetStmtNode(stmt, var, _, _)
    & ConsStmtListNode(stmts, head, _)
    & VarInStmts(var, stmts)
    =>
    VariableShadowing()
    ;
```
Thus, shadowing occurs when there is a let statement `stmt` introducing a variable `var` such that `var` is already in scope for `stmt`.

### Undeclared variables

Following the approach we took for shadowing, we might attempt to add an axiom along the following lines to our Eqlog program:
```eqlog
Axiom
    VariableExprNode(expr, var)
    // Doesn't compile:
    & NOT VarInExpr(var, expr)
    =>
    UndeclaredVariable()
    ;
```
Thus, if a variable is used as an expression and that variable is *not* in scope, then the program contains the usage of an undeclared variable.
Unfortunately, while some other Datalog engines support negation, Eqlog does not, and so `NOT VarInExpr(var, expr)` is not valid Eqlog.
There is no fundamental reason why Eqlog cannot support negations with the same semantics as other Datalog engines; I've simply not come around to implementing it.

Instead, we'll encode this axiom explicitly in Rust:
```rust
fn has_undeclared_variables(p: &Program) -> bool {
    p.iter_variable_expr_node()
        .any(|(expr, var)| !p.var_in_expr(var, expr))
}
```

Eqlog does not automatically evaluate axioms; we have to call the `close` method to start evaluation.
After evaluation, we check whether there are variable binding errors.
Our top-level `check_source` function thus looks like this:
```rust
fn check_source(src: &str) -> Result<(Program, Literals, ModuleNode), LanguageError> {
    let no_comments_src = erase_comments(src);

    let mut p = Program::new();
    let mut lits = Literals::new();

    let module = ModuleParser::new()
        .parse(&mut p, &mut lits, &no_comments_src)
        .map_err(|err| LanguageError::from_parse_error(err, &no_comments_src))?;

    p.close();

    if p.variable_shadowing() {
        return Err(LanguageError::VariableShadowing);
    }

    if has_undeclared_variables(&p) {
        return Err(LanguageError::UndeclaredVariable);
    }

    Ok((p, lits, module))
}
```
