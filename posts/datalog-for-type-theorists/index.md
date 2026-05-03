---
title: "The case for Datalog for type theorists"
date: "May 03, 2026"
lang: "en_US"
---

This post argues that type theorists should pay more attention to Datalog than they currently do.
My impression is that many type theorists view Datalog as perhaps vaguely related to their work but not particularly relevant to it.
I think this view is unjustified, and I will argue for two reasons in support of this position.
The first reason is that Datalog, in suitably extended form, ought to play a role for type checking that is closely analogous to the role parser generators play for parsing.
The second is that Datalog suggests an alternative to strong normalization for deciding equality during type checking, namely *equality saturation*.

I assume basic familiarity with type theory and with Datalog.

## Datalog ought to be to type checking as parser generators are to parsing

A parser generator takes a formal description of a language to be parsed, typically in the form of a context-free grammar, and produces an executable that recognizes the language and emits an abstract syntax tree.
Outside of educational contexts, no one writes parsers by hand for non-trivial grammars anymore.
The corresponding promise for type checking would be to take some formal description of a type theory and produce an executable type checker.
The claim of this section is that Datalog, in suitably extended form, is well-suited to play the role of the parser generator, even though it is not yet routinely used in this way.

The mechanical part of this correspondence is that the typical natural deduction style typing rules of a type theory translate almost line by line into Datalog rules.
For example, the typing rule for function application
```
   ctx |- f : fun_ty    fun_ty = a_ty -> b_ty    ctx |- a : a_ty
   --------------------------------------------------------------
                     ctx |- app(f, a) : b_ty
```
becomes the Datalog rule
```eql
rule type_app {
    if has_type(ctx, f, fun_ty);
    if fun_ty = arrow_type(a_ty, b_ty);
    if has_type(ctx, a, a_ty);
    then has_type(ctx, app(f, a), b_ty);
}
```
Each premise above the inference line corresponds to an `if` statement and the conclusion below the line corresponds to a `then` statement.
The Datalog encoding factors out the operational details of how typing rules are searched, matched and applied, and it leaves only the rules themselves to be specified by hand.

### Equality and fresh elements

Standard Datalog is too restrictive to fully play the role sketched above.
Two restrictions in particular are limiting.

The first is that standard Datalog has no native notion of equality.
This is a problem because most type theories of interest involve a non-trivial equality on types.
For example, in dependent type theory the type `Vec(2 + 2)` is meant to be the same type as `Vec(4)`, even though the two expressions are not syntactically equal.
A type checker has to decide such equalities, and any encoding of typing rules into Datalog must therefore be able to express that two elements of the `Type` sort should be considered equal.

The second is that standard Datalog cannot introduce fresh elements during evaluation.
Every element occurring in the database must already be present, either as part of the input or as the result of an explicit constructor application.
This is also problematic for type checking, because type inference frequently has to assert that some term or type exists before its concrete identity is known.
For example, when typing the variable bound by a function literal, the type checker must record that there is *some* type assigned to the variable, even though no concrete type is yet available.

The Datalog extensions implemented by [Eqlog](https://github.com/eqlog/eqlog) and [Egglog](https://github.com/egraphs-good/egglog) lift both of these restrictions.
Both engines support partial functions, conclusion of equalities between elements (with congruence closure to propagate inferred equalities through the database), and existential conclusions that introduce a fresh element when no suitable value already exists.
In Eqlog syntax, the existential conclusion is written using an exclamation mark.
The rule
```eql
rule {
    if v: Var;
    then var_type(v)!;
}
```
forces the partial function `var_type` to be defined on every variable, introducing a fresh `Type` element when no value has been assigned to `v` yet.

### The connection to essentially algebraic theories

The two extensions described above are not independent or arbitrary.
Together with partial functions, they make the resulting language equivalent in expressive power to *essentially algebraic theories* in the sense of Freyd, which are also known as *finite limit sketches* or theories of *partial Horn logic*.
I have written about [the relevant semantics elsewhere](https://www.mbid.me/eqlog-semantics/).

Many type theorists will already recognize that essentially algebraic theories are closely related to Cartmell's *generalized algebraic theories* (GATs).
Both formalisms have the same expressive power, but they encode the same content differently.
Generalized algebraic theories support dependent sorts directly, whereas essentially algebraic theories represent dependent sorts as ambient sorts together with predicates that pick out which elements lie in which dependent sort.

This correspondence is significant because most modern formulations of dependent type theory, in particular *categories with families* (CwFs), are essentially algebraic.
Up to encoding, specifying a type theory and specifying a Datalog program with equality and partial functions can therefore be the same activity.
What remains in turning such a specification into a working type checker is mostly engineering, namely fitting the specification to the operational behavior of a particular Datalog engine.
This remaining engineering effort is non-trivial, and as I will discuss further below, it is the main reason why Datalog is not yet routinely used in production type checkers.

## Equality saturation as an alternative to strong normalization

The previous section argues that Datalog could be a tidy specification language for type checkers.
This would already be valuable, but the parser generator analogy promises more.
Modern parser generators have not just made parser implementation more pleasant, they have also unlocked algorithms that no one would seriously consider implementing by hand, such as the construction of LR tables.
We can ask whether Datalog could also enable new algorithms for type checking, and I think the answer is yes.
The most striking example is *equality saturation* as an alternative to strong normalization for deciding type equality.

Equality saturation is well-established as a technique for compiler optimization, and the problem it solves there is the choice of a good ordering of optimization passes.
Phase ordering is a notorious source of brittleness, since an improvement to one pass can introduce regressions in another depending on the order in which the passes run.
Equality saturation circumvents this issue architecturally.
An *e-graph* compactly represents all expressions reachable from a given input expression by repeated application of a fixed set of rewrite rules, and the optimized expression is selected from this set according to some cost function.

Optimizing compilers operate on a best-effort basis and offer few formal guarantees about which optimizations they discover.
Type checkers cannot operate this way.
A type checker must accept exactly the well-typed programs and reject the others, which means that type equality must be decided definitely.
For this reason, most modern type theories rely on a strongly normalizing rewriting system on terms, where two terms are judged equal if and only if they reduce to the same normal form.

However, strongly normalizing rewriting systems do not exist for every type theory of interest.
The most famous example is extensional type theory, where definitional equality is undecidable.
This is the principal reason that most dependently typed proof assistants are based on intensional rather than extensional type theory.
The cost of this choice is well known.
Implementors must integrate every extension of the base theory into the rewriting system without breaking strong normalization, and users of the system must reason about two distinct notions of equality, namely *definitional* equality, which is checked automatically by the proof assistant, and *propositional* equality, which is not.
This distinction is the source of the familiar awkwardness whereby the expression `1 + n` cannot be substituted for `n + 1` in arbitrary positions even though the two expressions are provably equal.

Equality saturation suggests an alternative strategy that does not require a strongly normalizing rewriting system.
The type checker applies the rewriting rules of the type theory to the subexpressions of the input program and enumerates equivalence classes of expressions, either to a fixed point or up to a fixed iteration depth.
When the type checker requires two expressions to be equal, it consults the resulting e-graph.
The check succeeds if the two expressions belong to the same equivalence class, and otherwise it fails.

Readers will rightly object that the verdict of such a type checker depends on the chosen iteration depth.
This is awkward.
However, an analogous awkwardness already exists for proof assistants based on intensional type theory.
Verifying a proof there can require superexponentially many reduction steps, and so any agent who wishes to verify an untrusted proof must agree on a budget for the verification effort.
Without such a budget, there is no way to distinguish a proof that fails verification from a proof that simply has not finished verifying within the available time.
Equality saturation makes this budget explicit, rather than tucking it away inside the rewriting strategy of the proof checker.

To be clear, my claim is not that equality saturation is necessarily superior to strong normalization for deciding type equality, but rather that it represents a different and underexplored point in the design space, and that this design space deserves more attention from type theorists than it currently receives.

## Real-world type checkers based on Datalog

Type theorists familiar with the implementation of existing proof assistants will have noticed that the picture above is somewhat idealized.
No major proof assistant is currently implemented in Datalog.
Several efforts move in this direction, however.

The most prominent attempt to use Datalog in a production type checker is the Rust compiler's [Chalk](https://github.com/rust-lang/chalk) project, which started out as a Datalog-based engine for Rust's trait system.
Chalk was eventually superseded by Rust's next-generation trait solver, which is no longer a Datalog engine.
I do not know the precise reasons for this transition.
On a related front, the [Polonius](https://github.com/rust-lang/polonius) borrow checker is built on top of the [Datafrog](https://github.com/rust-lang/datafrog) Datalog engine.

A more pedestrian example is my [series on Hindley-Milner type checking with Eqlog](../type-checking-with-eqlog-parsing), which walks through a complete implementation of Hindley-Milner type inference as an Eqlog program.
The Eqlog compiler itself currently has its type checker [written in Eqlog](../self-hosting-eqlog), although I am working on reverting this because the Rust code generated by Eqlog from its own type checker takes a long time to compile.

The principal obstacle to using Datalog as the basis for type checkers of dependently typed proof assistants is, in my view, the lack of language-level support for composing self-contained Datalog programs.
This is the problem that *dependent Datalog* is intended to address.
I have written about dependent Datalog informally in an [earlier blog post](../dependent-types-for-datalog), and I have proposed the language design more formally in an extended abstract for TYPES 2026.
Even before dependent Datalog matures, however, it may be possible to isolate parts of an existing type checker that can profitably be moved into a Datalog engine.
