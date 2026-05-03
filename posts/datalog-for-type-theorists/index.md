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

## Datalog ought to be to type checking as parser generators are to parsing

A parser generator takes a formal description of a language to be parsed, typically in the form of a context-free grammar, and produces an executable that recognizes the language and emits an abstract syntax tree.
The corresponding promise for type checking would be to take some formal description of a type theory and produce an executable type checker.
The claim of this section is that Datalog, in suitably extended form, is well-suited to play the role of the parser generator, even though it is not yet routinely used in this way.

The mechanical part of this correspondence is that the typical natural deduction style typing rules of a type theory translate almost line by line into Datalog rules.
For example, the typing rule for function application
$$
\frac{\Gamma \vdash f : A \to B \qquad \Gamma \vdash a : A}{\Gamma \vdash f\,a : B}
$$
becomes the following rule in [Eqlog](https://github.com/eqlog/eqlog), the Datalog dialect with equality I will use as a running example:
```eql
rule type_app {
    if has_type(ctx, f, func_type(a_ty, b_ty));
    if has_type(ctx, a, a_ty);
    then has_type(ctx, app(f, a), b_ty);
}
```
Each premise above the inference line corresponds to an `if` statement and the conclusion below the line corresponds to a `then` statement.
The Eqlog encoding factors out the operational details of how typing rules are searched, matched and applied, and it leaves only the rules themselves to be specified by hand.

### Equality and fresh elements

Standard Datalog is too restrictive to fully play the role sketched above.
Two restrictions in particular are limiting.

The first is that standard Datalog has no native notion of equality.
This is a problem because most type theories of interest involve non-trivial equalities on types.
Even for non-dependent type systems, type checkers regularly have to compare two types that are not syntactically equal.
A Hindley-Milner inference engine, for example, repeatedly equates type variables with concrete types as it unifies the types of subexpressions, and Rust's trait system equates types via associated type projections such as `<T as Iterator>::Item`.
In dependent type theories the situation is even more pronounced, since type equality there can depend on arbitrary computation:
the type `Vec(2 + 2)` is meant to be the same type as `Vec(4)`, even though the two expressions are not syntactically equal.
A type checker has to decide such equalities, and any encoding of typing rules into Datalog must therefore be able to express that two elements of the `Type` sort should be considered equal.

The second is that standard Datalog cannot introduce fresh elements during evaluation.
Every element occurring in the database must already be present, either as part of the input or as the result of an explicit constructor application.
This is also problematic for type checking, because type inference frequently has to assert that some term or type exists before its concrete identity is known.
For example, when typing the variable bound by a function literal, the type checker must record that there is *some* type assigned to the variable, even though no concrete type is yet available.

The Datalog extensions implemented by [Eqlog](https://github.com/eqlog/eqlog) and [Egglog](https://github.com/egraphs-good/egglog) lift both of these restrictions.
Both engines support partial functions, conclusion of equalities between elements (with congruence closure to propagate inferred equalities through the database), and conclusions that force a partial function to be defined on a given input, introducing a fresh element when no value is yet associated.
In Eqlog syntax, the latter is written using an exclamation mark.
The rule
```eql
rule {
    if v: Var;
    then var_type(v)!;
}
```
forces the partial function `var_type` to be defined on every variable, introducing a fresh `Type` element when no value has been assigned to `v` yet.

### Essentially algebraic theories

The two extensions described above are not independent or arbitrary.
Together with partial functions, they make the resulting language equivalent in expressive power to *essentially algebraic theories* in the sense of Freyd, which are also known as *finite limit sketches* or theories of *partial Horn logic*.
I have written about [the relevant semantics elsewhere](https://www.mbid.me/eqlog-semantics/).

This is significant because most modern formulations of dependent type theory, in particular *categories with families* (CwFs), are essentially algebraic.
Up to encoding, specifying a type theory and specifying a Datalog program with equality and partial functions can therefore be the same activity.
In an ideal world, what would remain in turning such a specification into a working type checker is mostly engineering, namely fitting the specification to the operational behavior of a particular Datalog engine.
We are not quite in that world yet, however, since there are some technical problems to be solved before this becomes practical.
I will return to these problems further below.

## Equality saturation as an alternative to strong normalization

The previous section argues that Datalog could be a tidy specification language for type checkers.
This would already be valuable, but the parser generator analogy promises more.
Parser generators also enable parsing algorithms that nobody would write by hand, like the construction of LR tables.
We can ask whether Datalog enables new algorithms for type checking too, and I think the answer is yes.
The most striking example is *equality saturation* as an alternative to strong normalization for deciding type equality.

Equality saturation has been proposed in recent years as a technique for compiler optimization, and is currently an active research topic.
The problem it addresses there is the choice of a good ordering of optimization passes.
Phase ordering is a source of brittleness, since an improvement to one pass can introduce regressions in another depending on the order in which the passes run.
Equality saturation circumvents this issue architecturally.
An *e-graph* compactly represents all expressions reachable from a given input expression by repeated application of a fixed set of rewrite rules, and the optimized expression is selected from this set according to some cost function.

Optimizing compilers operate on a best-effort basis and offer few formal guarantees about which optimizations they discover.
Type checkers cannot operate this way.
A type checker must accept exactly the well-typed programs and reject the others, which means that type equality must be decided definitely.
For this reason, most modern type theories rely on a strongly normalizing rewriting system on terms, where two terms are judged equal if and only if they reduce to the same normal form.

However, strongly normalizing rewriting systems do not exist for every type theory of interest.
The most famous example is extensional type theory, where definitional equality is undecidable.
This is the principal reason that most dependently typed proof assistants are based on intensional rather than extensional type theory.

It comes at a cost, though.
Implementors must integrate every extension of the base theory into the rewriting system without breaking strong normalization, and users of the system must reason about two distinct notions of equality, namely *definitional* equality, which is checked automatically by the proof assistant, and *propositional* equality, which is not.
This distinction is the source of the familiar quirk whereby the expression `1 + n` cannot be substituted for `n + 1` in arbitrary positions even though the two expressions are provably equal.

Equality saturation suggests an alternative strategy that does not require a strongly normalizing rewriting system.
The type checker applies the rewriting rules of the type theory to the subexpressions of the input program and enumerates equivalence classes of expressions, either to a fixed point or up to a fixed iteration depth.
When the type checker requires two expressions to be equal, it consults the resulting e-graph.
The check succeeds if the two expressions belong to the same equivalence class, and otherwise it fails.

Readers will rightly object that the verdict of such a type checker depends on the chosen iteration depth.
This is awkward.
However, an analogous situation already exists for proof assistants based on intensional type theory.
Verifying a proof there can in principle require enormous numbers of reduction steps, since reduction in dependent type theory is non-elementary in the worst case.
Any agent who wishes to verify an untrusted proof must therefore agree on a budget for the verification effort.
Without such a budget, there is no way to distinguish a proof that fails verification from a proof that simply has not finished verifying within the available time.
Equality saturation makes this budget explicit, rather than tucking it away inside the rewriting strategy of the proof checker.

To be clear, my claim is not that equality saturation is necessarily superior to strong normalization for deciding type equality, but rather that it represents a different and underexplored point in the design space, and that this design space deserves more attention from type theorists than it currently receives.

## Real-world type checkers based on Datalog

Type theorists familiar with the implementation of existing proof assistants will have noticed that the picture above is somewhat idealized.
No major proof assistant is currently implemented in Datalog.
Several efforts move in this direction, however.

Within Rust, the most closely related effort is [Polonius](https://github.com/rust-lang/polonius), an experimental implementation of the borrow checker.
The original Polonius prototype is built on the [Datafrog](https://github.com/rust-lang/datafrog) Datalog engine, but the in-tree successor that is currently being prepared for stabilization is a hand-rolled graph reachability analysis rather than a Datalog program.
A separate Rust effort in the broader family of logic-programming-based type system implementations is the [Chalk](https://github.com/rust-lang/chalk) project, which experimented with a Prolog-style engine for Rust's trait system.
Chalk has since been sunset in favor of an in-tree next-generation trait solver in rustc that inherits its conceptual approach but is implemented as a recursive solver rather than as a tabling Prolog or Datalog engine.

Closer to a fully Datalog-implemented type checker, though strictly a toy and intended as an educational example, is my [series on Hindley-Milner type checking with Eqlog](../type-checking-with-eqlog-parsing), which expresses most of the type checker as an Eqlog program.
The Eqlog compiler itself has its type checker [written in Eqlog](https://github.com/eqlog/eqlog/blob/3337ec49c0acf9a610dfe660782a1806554c26fb/eqlog-eqlog/src/eqlog.eql), although I am working on reverting this, since compiling the generated Rust code is slow enough to make iteration painful.

Several fundamental issues currently stand in the way of using Datalog as the basis for a production type checker.
The one I find most central is that Datalog has no way to express copy-on-write sharing of related data, which leads to extreme memory duplication.
A motivating case is variable scopes that extend each other: without sharing, each new variable binding forces a deep copy of the entire scope table.
I sketch the problem and a proposed solution based on morphisms between Datalog model instances in the last section of [a separate blog post](../dependent-types-for-datalog) on dependent Datalog, and in an extended abstract for TYPES 2026.
Until problems of this kind are addressed, I would not recommend Datalog as the basis for a serious type checker, although it may already be possible to isolate parts of an existing type checker that can profitably be moved to a Datalog engine.
