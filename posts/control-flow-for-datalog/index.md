---
title: "Control flow for Datalog"
date: "November 01, 2023"
lang: "en_US"
---

This post outlines some ideas for control flow features that I would like to add to the [Eqlog](https://github.com/eqlog/eqlog) Datalog engine:

1. [Interleaving premise and conclusion]
2. [Forks]
3. [Sum types]

I believe all of these features make sense for a general Datalog engine, but some aspects I'll discuss are about the interaction with native equality and function symbols, which Eqlog adds on top of basic Datalog.
If you haven't heard of Eqlog, I encourage you to have a look at the [README](https://github.com/eqlog/eqlog) or some of the posts I wrote on implementing a Hindley-Milner style type checker using Eqlog starting [here](https://www.mbid.me/posts/type-checking-with-eqlog-parsing/).

![Boka Waterfall, Slovenia](picture.jpg "Boka Waterfall, Slovenia")

## Interleaving premise and conclusion

Eqlog currently supports only one very basic kind of rule, which I will refer to as an *implication* in this post.
I've changed Eqlog's syntax recently (more on this below), but such implications used to look something like this:
```eqlog
Axiom
    LessThan(a, b)
    & LessThan(b, c)
    =>
    LessThan(a, c)
    & GreaterThan(c, a)
    ;
```
The part before the implication symbol `=>` is the *premise*, *query* or *body* of the implication, and everything after is the *conclusion* or *head*.
Both premise and conclusion are given by conjunctions of *clauses* or *atoms*.
Eqlog evaluates such an implication by repeatedly enumerating matches of the premise, and then adding data corresponding to the conclusion.

I don't want to change Eqlog's fundamental execution model, but I would like for it to be less verbose and less error-prone to specify certain sets of implications.
As an example, consider the following implications taken from the [Hindley-Milner type checker](https://www.mbid.me/posts/type-checking-with-eqlog-parsing/) I mentioned above, which has to do with instantiating types of variables with polymorphic type.
The definition of the predicates and functions in these implications are not relevant here, it's the general shape of those implications that I want to draw attention to:
```
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(_)
    =>
    ExprInstantiation(expr)!
    ;
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(_)
    & instance = ExprInstantiation(expr)
    & ctx = ExprTypeContext(expr)
    =>
    InstantiationTarget(instance) = ctx
    ;
```

Taken together, these two implications assert the following:

(A) If
    ```
    VariableExprNode(expr, var) & VarTypeInExpr(var, expr) = PolyType(_)
    ```
    holds,
(B) then `ExprInstantiation(expr)` is defined.
    We'll refer to this element by `instance` henceforth.
(C) Furthermore, if `ctx = ExprTypeContext(expr)`,
(D) then `InstantiationTarget(instance) = ctx`.

Abstracting away from the concrete formulas, these two implications are of the form
```
A => B        A & B & C => D
```
for formulas $A, B, C$ and $D$.
With Eqlog's current syntax, we're forced to state $A$ and $B$ twice, in each implication.
That's the first problem we're going to tackle:
Allow going back and forth between specifying premises and conclusions, so that implications can be specified similarly to the list above, without duplication.
Another way to look at this is that Eqlog should understand something to the effect of the English "furthermore" as used in $C$.

Unfortunately, I couldn't come up with a natural way of extending Eqlog's original syntax to accommodate for this.
But since Eqlog programs are supposed to be embedded into Rust projects, it would probably be better in any case if Eqlog looked more similar to Rust than the original syntax allowed it to.
So this was a good opportunity for a major revision of Eqlog's syntax.

With the new syntax I came up with, the two implications above can be written as a single *rule* as follows:
```eql
rule expr_instantiation_target {
    if variable_expr_node(expr, var);
    if var_type_in_expr(var, expr) = poly_type(_);
    then instance := expr_instantiation(expr)!;
    if ctx = expr_type_context(expr);
    then instantiation_target(instance) = ctx;
}
```

Thus:

* The `rule` keyword replaces the old `Axiom` keyword.
  Rules have an optional name, and are given by a list of statements:
  ```
  rule <optional_name> {
      stmt1;
      stmt2;
      ...
      stmtn;
  }
  ```
* Each statement is either an `if` statement of the form `if <atom>`, corresponding to what was previously a clause in a premise, or a `then` statement of the form `then <atom>`, corresponding to what was previously a clause in a conclusion.
* Crucially, `if` and `then` statements can be freely interleaved.
* Semantically, every rule is equivalent to a *set* of implications, one for each `then` statement in the rule:
  The implication corresponding to a statement `then <atom_k>` occurring as $k$th statement in a rule is as follows:
  If `<atom_1>, <atom_2>, ..., <atom_{k - 1}>` are the atoms of statements before statement $k$ (regardless of whether they're `if` or `then` statements!), then the implication is given by
  ```
  <atom_1> & ... & <atom_{k - 1}> => <atom_k>;
  ```

A naive Rust implementation of the example rule above might look like this:
```rust
// if variable_expr_node(expr, var);
for (expr, var) in iter_variable_expr_node() {
    // if var_type_in_expr(var, expr) = poly_type(_);
    for (var0, expr0, res0) in iter_poly_type() {
        if var0 != var || expr != expr0 {
            continue;
        }

        // then instance := expr_instantiation(expr)!;
        let instance = match expr_instantiation(expr) {
            Some(instance) => instance,
            None => {
                let instance = new_instantiation();
                insert_expr_instantation(expr, instance);
                instance
            }
        };

        // if ctx = expr_type_context(expr);
        if Some(ctx) = expr_type_context(expr) {
            // then instantiation_target(instance) = ctx;
            insert_instantiation_target(instance, ctx);
        }
    }
}
```
Thus, we don't actually have to emit the equivalent of separate nested loops for each `then` statement and can instead reuse outer loops.

I've already implemented the new syntax, but only original Eqlog semantics (i.e., implications) are supported yet.
Thus, every rule must be given by a list of `if` statements, followed by a list of `then` statements, and interleaving the two doesn't work yet.

## Forks

The Hindley-Milner type checker that the example implications above are taken from defines a third, related implication:
```eqlog
Axiom
    VariableExprNode(expr, var)
    & VarTypeInExpr(var, expr) = PolyType(sigma)
    & instance = ExprInstantiation(expr)
    & expr_ty = ExprType(expr)
    =>
    Instantiate(instance, sigma) = expr_ty
    ;
```
The first three clauses of this implication are the same as in one of the implications we've already seen, but the atom `expr_ty = ExprType(expr)` in the premise and the conclusion are specific to this implication.
Even with the ability to interleave `if` and `then` statement, we'd need to add a second rule for this implication that would duplicate the three initial clauses/statements.
What we would need to avoid this duplication is a mechanism to fork a rule after a number of statements.
That's the purpose of the `fork` keyword.

Using `fork`, this third premise can be incorporated into the existing `expr_instantiation_target` rule we've seen in the previous section as follows:
```eql
rule expr_instantiation_target {
    if variable_expr_node(expr, var);
    if var_type_in_expr(var, expr) = poly_type(sigma);
    then instance := expr_instantiation(expr)!;
    fork {
        if ctx = expr_type_context(expr);
        then instantiation_target(instance) = ctx;
    } or {
        if expr_ty = expr_type(expr);
        then instantiate(instance, sigma) = sigma;
    }
}
```

Thus:

* Rules can now contain statements of the form `fork { <block_1> } or { <block_2> } ... or { <block_n }`.
  Each block is a list of statements, and can recursively contain other `fork` statements.
* Semantically, usage of the `fork` statement in a rule can be desugared by duplicating the rule $n$ times, once for each block of the `fork` statement.
  In the $i$th copy of the rule, the `fork` statement is replaced by `<block_i>`.
* In particular, `fork` statements need not necessarily occur at the end of the rule.

A good implementation will probably desugar a `fork` statement into several rules though:
Instead, results of queries for the shared part of a rule can be used for each block of a subsequent `fork` statement.

I'm not completely sure about the `fork` keyword itself though, and especially whether `or` is the right keyword to separate the individual blocks.
I've also considered "`allof`" instead of `fork` and "`and`" instead of `or`, since *all* of the following blocks apply after the statements before the `fork` have matched.
But statements after the `fork` apply if *any* of the blocks of the fork statement have matched, suggesting keywords "`anyof`" and "`or`".
So perhaps `fork` is the best we can do here, although I don't like that it emphasizes the operational aspects of the mechanism instead of the semantics.
The `or` keyword is somewhat misleading though, and a good alternative eludes me at the moment.
One option might be to just not have a keyword between blocks, so that only closing and opening curly braces `} {` would separate blocks.

## Sum types

Sum types are probably among the most impactful features that have made it from the academic programming language ivory tower into mainstream languages in recent years.
Once you're used to sum types, you see them crop up almost everywhere, and they make many classes of problems much easier to deal with.

One such problem class are algorithms that have to traverse abstract syntax tree (AST) nodes.
For example, here's how we might define a type of AST nodes representing (a subset of) boolean expressions in Rust:
```rust
enum BoolExpr {
    True(),
    False(),
    Or(Box<BoolExpr>, Box<BoolExpr>),
    And(Box<BoolExpr>, Box<BoolExpr>),
}
```

The main benefit of sum types is that they enable destructuring pattern matching.
Continuing with the `BoolExpr` example, here's a Rust function that evaluates a `BoolExpr`:
```rust
fn eval(e: &BoolExpr) -> bool {
    use BoolExpr::*;
    match e {
        True() => { true }
        False() => { false }
        Or(lhs, rhs) => { eval(lhs) || eval(rhs) }
        And(lhs, rhs) => { eval(lhs) && eval(rhs) }
    }
}
```
Crucially, the Rust compiler checks statically that there is a match arm for every possible value of `e`.
If we forget one of the constructors of `BoolExpr`, for example `BoolExpr::Or`, then the compiler will immediately point us to the bug.

Standard Datalog doesn't have sum types, and neither does Eqlog.
Instead, Datalog programs that deal with ASTs typically define a separate relation/predicate for each different constructor of a given kind of AST node.
In Eqlog, we can use functions to encode AST nodes (unless we want to distinguish structurally equal AST nodes), for example like so:
```eql
type BoolExpr;
func true_expr() -> BoolExpr;
func false_expr() -> BoolExpr;
func or_expr(BoolExpr, BoolExpr) -> BoolExpr;
func and_expr(BoolExpr, BoolExpr) -> BoolExpr;
```

We can then define an analogue of the `eval` function above in Eqlog as follows:
```eql
// We'll add rules such that this predicate holds
// for expressions that evaluate to true.
pred evals_to_true(BoolExpr);

rule true_eval {
    if expr = true_expr();
    then evals_to_true(expr);
}
// No rule for false_expr -- it doesn't evaluate to true.
rule or_eval {
    if expr = or_expr(lhs, rhs);
    fork {
        if evals_to_true(lhs);
    } or {
        if evals_to_true(rhs);
    }
    then evals_to_true(expr);
}
rule and_eval {
    if expr = and_expr(lhs, rhs);
    if evals_to_true(lhs);
    if evals_to_true(rhs);
    then evals_to_true(expr);
}
```
Note that nothing prevents us from removing or forgetting to add a rule governing `evals_to_true`, say the `or_eval` rule.
What we'd need to catch such an error an is an equivalent of Rust's exhaustive match statement in Eqlog, hence sum types.

So here's what sum types could look like in Eqlog:
```eql
enum BoolExpr {
    True(),
    False(),
    Or(BoolExpr, BoolExpr),
    And(BoolExpr, BoolExpr),
}

pred evals_to_true(BoolExpr);
rule evals_to_true_laws {
    if e: BoolExpr;
    match e {
        True() => {
            then evals_to_true(e);
        }
        False() => {}
        Or(lhs, rhs) => {
            fork {
                if evals_to_true(lhs);
            } or {
                if evals_to_true(rhs);
            }
            then evals_to_true(e);
        }
        And(lhs, rhs) => {
            if evals_to_true(lhs);
            if evals_to_true(rhs);
            then evals_to_true(e);
        }
    }
}
```

Thus, every constructor of the `BoolExpr` enum implicitly defines a function, for example `Or(BoolExpr, BoolExpr) -> BoolExpr`.
If the type of a variable in a rule is an `enum` type, then the variable can be destructured with a `match` statement.
Such `match` statements desugar into `fork` statements, with each arm in the `match` statement corresponding to an alternative block in the `fork`.
The first statement in each block of the resulting `fork` statement corresponds to the constructor of the corresponding match arm and the rest of the block is given by the statements in the match arm.
Thus, the `evals_to_true_laws` rule above would desugar into the following rule:

```eql
rule evals_to_true_laws_desugared {
    if e: BoolExpr;
    fork {
        if e = True();
        then evals_to_true(e);
    } or {
        if e = False();
    } or {
        if e = Or(lhs, rhs);
        fork {
            if evals_to_true(lhs);
        } or {
            if evals_to_true(rhs);
        }
        then evals_to_true(e);
    } or {
        if e = And(lhs, rhs);
        if evals_to_true(lhs);
        if evals_to_true(rhs);
        then evals_to_true(e);
    }
}
```

So far I think there's not too much to argue about.
However, there are some considerations regarding sum types that are specific to the Datalog (or Eqlog's) evaluation model.

### Surjectivity of constructors

Does every element of an `enum` type need to be equal to the result of applying a constructor?
This is what you would expect from other languages with sum types.
However, it's not obvious how Eqlog can guarantee this property.

That's because Eqlog allows enforcing that a given function is defined on some element.
For example, the `expr_instantiation_target` law above enforces that the `expr_instantiation` function is defined on an `expr` under certain conditions:
```eql
rule expr_instantiation_target {
    ...
    then instance := expr_instantiation(expr)!;
    ...
}
```
When Eqlog infers during evaluation that a function should be defined on some arguments, it creates a new element (that is, Eqlog chooses a previously unused integer ID to represent the element), and records it as result of applying that function.
But what if the result type of the function was declared as an `enum`?
We now have an element of an enum type that is not given by one of the constructors.

One option here is to prevent such situations from arising in the first place using a static compile time check.
For example, we could require that every new element of an enum type that is introduced by a rule is later in that rule equated with the result of applying a constructor.
Such a check cannot also consider arbitrary applications of other rules as well though, since that would mean that the check does not necessarily terminate.
Thus, some generality would be lost.

In some cases it can even be desirable to have elements of an `enum`-like type that are not equal to any of the constructors.
One example is the type `Type` that the [Hindley-Milner type checker I mentioned above](https://www.mbid.me/posts/type-checking-with-eqlog-parsing/) uses to represent the types of its input language.
This type might be declared as an Eqlog enum like so:
```eql
enum Type {
    Boolean(),
    String(),
    Function(arg: Type, result: Type),
    ...
}
```
To account for generics in a Hindley-Milner type system, `Type` must be able to represent type variables. 
The type checker represents such type variables as elements of `Type` which are not given by any of the constructors, which is an intentional violation of surjectivity of constructors.
So it might seem that `Type` should just not be an `enum` to begin with.
But on the other hand, the type checker defines some rules that consider which, if any, of the constructors a type was obtained with and conclude accordingly, and there the `match` statement is useful.

The other option is to simply drop the requirement that every element of an `enum` type must be obtained via one of the constructors.
The desugaring of `match` to `fork` outlined above works perfectly fine; the `match` statement simply does nothing for elements that are not obtained via a constructor.
But still, this situation will be surprising at first and not intended in the majority of cases.
So a synthesis could be to make Eqlog recognize `enum` types declared as either `open` or `closed`, and then to let the user choose one or the other approach separately for each `enum`.

### Injectivity of constructors

When we speak of sum types, we usually mean a type representing the *disjoint* union of the summand types.
That the union is "disjoint" means that elements obtained by one constructor are never equal to the result of applying a different constructor.
As a consequence, there is a unique way in which a given element can be destructured in terms of the constructors of the enum type.

What distinguishes Eqlog from other Datalog engines is that Eqlog rules can equate elements.
When Eqlog infers during evaluation that an equation `lhs = rhs` of elements should hold, then `lhs` and `rhs` are completely interchangeable in all other contexts henceforth.

But then what should happen when a rule equates elements of an enum type that were obtained from different constructors, or from different arguments of the same constructor?
Here are two (nonsensical but valid) examples using the `Type` enum above:
```eql
rule bool_is_string {
    if b = Boolean();
    if s = String();
    then b = s;
}

rule function_is_commutative {
    if f = Function(arg, result);
    then f = Function(result, arg);
}
```

One option is again to just allow this:
This would mean that more than one arm of a match statement can apply to an element (for the example above, this could be both the `Boolean()` and `String()` arm of a match statement), or that one arm applies in more than one way (if `Function(arg, result)` matches, then also `Function(result, arg)` matches).

Another option would be to stop evaluation and report an error as soon as an equality among different constructors is found.
Such a dynamic check is perhaps the most pragmatic option, which is why I'm not very fond of it.

And a third option would be to statically ensure that no rule can be used to infer any equality whatsoever of elements of an enum type.
This is more difficult than it might seem at first, because many equalities stem from implicit functionality axioms and not explicitly enforced equalities in rules.

As an example, consider types `A` and `B` where `B` is declared as an enum
```
enum B {
    First(),
    Second(),
}
```
and suppose there is a function `foo(A) -> B` such that `foo(a0) = First()` and `foo(a1) = Second()` for elements `a0` and `a1` of `A`.
Now, if we infer that `a0 = a1` holds, then the functionality axiom for `foo` lets us deduce `First() = Second()`.
Thus, we sometimes infer equalities in a type even if no rule explicitly enforces that equality.
So what would be needed here is that we forbid rules from equating elements of enum types and all types for which there exists (transitively) a function into an enum type.
That would usage of `enum` types in many instances though.

From what I can tell, the most straightforward option for now is to just not enforce injectivity of constructors.
Obfuscated in category theoretic language, this would mean that Eqlog's `enum` types are [*amalgamated sum* or *pushout*](https://en.wikipedia.org/wiki/Pushout_(category_theory)) types.
As before, it could make sense for Eqlog to support an optional annotation on `enum` declarations that would enable a check that constructors must be injective.

### Wildcards

Rust and other languages typically support wildcard patterns `_` in match arms statements:
```rust
fn is_literal(e: BoolExpr) -> bool {
    match e {
        True() => { true }
        False() => { true }
        _ => { false }
    }
}
```
Wildcard patterns apply when all other arms have *not* matched.
That's a negation, and Datalog's fixed point semantics are incompatible with arbitrary negations.
To see why, consider this hypothetical Eqlog program:
```eql
pred p();
rule paradox {
    if not(p());
    then p();
}
```
After evaluation, should `p()` hold?

So Eqlog can't support arbitrary negations, but there's no fundamental reason why it could not support *stratified* negations in the same way that other Datalog engines support it.
Stratification prevents the paradox above by requiring that if a rule concludes from `p` that `q` holds, then there must be no rules that conclude (transitively) from `q` that `p` holds.
In other words, there must be no recursion involving negations.

Stratification could also be used to implement wildcards.
Eqlog would evaluate the program until the enum constructors stabilize, i.e., until no rule can fire anymore that would change anything about the graphs of the enum constructors.
At this point, Eqlog would apply the wildcard match arm (if any) to elements that no other arm applies to.

An alternative to this that doesn't require stratification could be for Eqlog to replace the wildcard with the complementary patterns to the ones that are explicitly given.
For example, the match statement in
```
if e: BoolExpr;
match e {
    True() => { <A> }
    False() => { <B> }
    _ => { <C> }
}
```
would desugar to
```
match e {
    True() => { <A> }
    False() => { <B> }
    Or(_, _) => { <C> }
    And(_, _) => { <C> }
}
```

If we enforce that enum constructors are surjective and injective, then this second approach is strictly more powerful than the first, because it gives the same results but does not require stratification.
But the first approach using stratification would still be useful for enums with non-surjective constructors, where the wildcard pattern would apply to elements that are not given by a constructor.
And in case of non-injective constructors, the second approach would mean that the wildcard pattern can match even when there is already some other pattern that matches, which is not the case with the approach based on stratified negation.
I don't know which alternative would be preferable here, but luckily there's no need to make a decision before sum types and `match` statements without wildcards are actually implemented :)
