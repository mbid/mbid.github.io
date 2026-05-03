# Datalog for Type Theorists

This text is meant as an introduction to Datalog specifically for type theorists.
At the moment, I believe many type theorists are not deeply familiar with Datalog, seeing it as perhaps vaguely related but not particularly relevant to their work.
I believe this to be wrong:
There are good reasons why type theorists should familiarize themselves with Datalog.

## Datalog is to type checking as parser generators are to parsing:

Given a formal grammar, parser generators provide an executable module that recognizes the grammar or generate an AST.
Similarly, Datalog engines (with some extensions) produce a type checker given just the formal description of the type system.
See: My blog posts on hindley-milner type checking with Eqlog.

<Add some examples: natural deduction rules and the equvialent datalog rule, how datalog evaluates them.>
<Why is equality needed? Why is non-surjectivity needed (i.e. introducing new elements during datalog evaluation)?>
<Add conceptual reason: These two extensions make Datalog equivalent to essentially algebraic theories/finite limit sketches. It's well-known that most type theories can be encoded as theories of this type, e.g. CwFs.>

## Datalog/equality saturation as alternative to strong normalization

The previous section just makes the point that Datalog is perhaps neater, tidier way of implementing type checkers.
But similarly to how parser generators unlock new algorithms (nobody's writing LR tables by hand), Datalog offers a new method for deciding equality during type checking:
Equality saturation.

This method is already being applied to compiler optimzations:
There, the problem it solves is that it removes the need for finding a good optimization pass ordering.
These orderings make changes in optimization passes brittle, since improvements in one pass can result in regression in unrelated ones.

Equality saturation tries to work around this architecturally:
e-graphs are used to efficiently enumerate all equivalent expressions according to repeated rewrite rules.
The optimized expression is then selected among this set according to a cost function.

Optimizing compilers work on a best-effort basis.
They typically make no or very few guarantees about which optimizations they find.
Type checkers cannot operate like this:
They must accept valid programs and reject invalid ones, and for that they need a definite equality decision procedure.

This is why most type systems rely on strongly normalization rewriting system for checking equality.
Under strong normalization, rewriting expressions eventually results in a unique normal form, and two expression are equal iff they have the same normal forms.

However, strongly normalizing rewriting systems do not exist for all type systems.
Famously, no such system exists for expressions in extensional type theory.
As a result, extensional type theory is considered unsuitable as a foundation for a practical proof checker, and most dependently typed proof systems are isntead based on intensional type theory.

The advantages of systems based on intenstional type theory are well-known (e.g. their application to higher mathematics), but they come at a cost:
Implementors need to fit every extension of the base type theory into its rewriting system while preserving the strong normalization property.
On the other end, users of the system need to be aware of the different kinds of equality:
Definitional equality, which is inferred automatically by the proof system, and propositional equality, which isn't.
This leads to the famous awkwardness where the expression 1 + n cannot be used interchangeably with the expression n + 1 even though they are provably (but not definitionally) equal.

Equality saturation offers an alternative strategy that does not require a strongly normalizing rewrite system:
The type checker applies the rewrite rules of type system to each context of the input program, enumerating expressions and their equivalence classes.
It applies rules iteratively until teither a fixed point is reached or up to fixed iteration number (i.e. derivation tree depth).
When it requires equality of two expressions in the input program, th type system reports an error if they haven't been found to be in the same equivalence class.

Readers will rightly object that program correctness under this system is now dependent on the number of rule application iterations.
This is, indeed, awkward.
However, this is arguably the case in practice already for systems based on intensional type systems, where proofs can require superexponentially many reduction steps.
To verify an untrusted proof, it is thus not enough to be only given the proof itself: an upper bound for the computation needed to verify it is for every verifies constraint by physical constraints.
<This is phrased very awkwardly. What I want to say is that every practically existing person that wants to accept a proof attempt needs to know how long they are supposed to run the proof checker, since otherwise they cannot distinguish between proofs that are wrong but hang until after the heat death of the universe on a superexponential blowup, and proofs that are correct but just take equally long to verify.>
In any case, the argument I want to make here is not that equality saturation is necessarily superior to strong normalization, but rather, that it represents a different and underexplored trade-off.

## Real-world type checkers using Datalog

I believe this is currently blocked on at least the problems described in <blog post to dependendent datalog, extended abstract>.
However, perhaps there are already some parts of the type checker that can isolated sufficiently so as to make Datalog a practical solution.

Prior work:
- Most relevant, Rust tried this.
  They ended up with something else than the Chalk Datalog engine they started out with.
  <TODO: What exactly do they have now?>
- This series shows how to implement a basic Hindley-Milner system:
  <TODO: Link to blog post series>
- My Eqlog Datalog engine is currently self-hosting its type checker.
  However, I'm currently working on reverting this, since the eqlog program used in eqlog itself takes very long to compile (or rather, the generated rust program takes very long to compile).
