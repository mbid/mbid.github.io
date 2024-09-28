---
title: "Dependent types for Datalog"
date: "September 28, 2024"
lang: "en_US"
---

*This is a high-level description of a Datalog extension that would allow instantiating multiple models of one Datalog program in a larger ambient Datalog program.
These instances are represented as elements in the ambient Datalog program, and types of the instantiated Datalog program are dependent on these elements.*

*The main purpose of this language feature is to make it possible to compose small self-contained Datalog programs into larger programs, similarly to how classes in object oriented languages enable multiple instantiations of part of a program and its associated data.
A second benefit is that it enables the Datalog compiler to detect more programming errors.
Finally, dependent models allow communicating locality to the Datalog compiler, which allows it to generate more efficient code.*

*Dependent models are a further generalization of Cartmell's framework of [generalized algebraic theories (GATs)](https://ncatlab.org/nlab/show/generalized+algebraic+theory):
Where GATs have a notion of dependent type or set, dependent models exist for every user-supplied Datalog theory, in particular the theory of a single type.*

This will be a rather conceptual post, and unfortunately I haven't even started to implement this.
But there's a good (bad?) chance I'll be a bit busier soon, so I figured it might be better to write down some of those ideas for when I can get back to them.

## A brief overview of Eqlog

In case you haven't followed the work on the [Eqlog Datalog compiler](https://github.com/eqlog/eqlog) or the similar [Egglog](https://github.com/egraphs-good/egglog) engine, let me try to briefly summarize.
Eqlog implements an extension of Datalog with support for (partial) functions and equality.
And while dependent models probably make sense without equality or functions, dependent models interact non-trivally with equality, so I decided to write directly about dependent models in Eqlog.

Types, functions and predicates in Eqlog are declared as follows:
```eql
type A;
type B;
func f(x: A) -> B;
pred p(x_0: A, x_1: A);
```

Rules consist of a sequence of `if` and `then` statements.
For example, the following rule enforces transitivity for `p`:
```eql
rule p_transitivity {
    if p(x_0, x_1);
    if p(x_1, x_2);
    then p(x_0, x_2);
}
```
In standard Datalog syntax this would be written as follows:
```prolog
p(x_0, x_2) :- p(x_0, x_1), p(x_1, x_2).
```

Compiled Eqlog programs represent elements of types as integer IDs, predicates as sets of tuples and functions via their graphs.
The graph of the function `f(a : A) -> B` is a predicate `graph_f : (a : A, b: B)` where `graph_f(x, y)` means that `f(x) = y`.
Eqlog's support for equality means that it can enforce that `graph_f` satisfies the functionality axiom, i.e. the following holds:
```eql
rule functionality {
    if graph_f(x, y_0);
    if graph_f(x, y_1);
    then y_0 = y_1;
}
```
During evaluation, when an Eqlog program infers an equality among elements `x` and `y`, it replaces all occurrences of `y` in predicates and functions by `x`.
Another extension over standard Datalog is the ability to quantify over all elements of a given type, and to enforce that a function is defined on given arguments via the exclamation mark operator:
```eql
func f(x: A) -> B;

rule {
    // if x is an element of type A
    if x: A;
    // then the function f is defined on x
    then f(x)!;
}
```
The exclamation mark operator make Eqlog Turing complete.
To prevent Eqlog programmers (so: me) from inadvertedly introducing new elements in rules, leading to non-termination, Eqlog enforces *surjectivity* restrictions in rule definitions:
Unless the exclamation mark operator is used, `then` statements can only mention elements that have been introduced earlier in the same rule by an `if` statement or via the exclamation mark operator.

The Eqlog compiler does not generate standalone executables.
Instead, the generated code is meant to be imported into a host program written in Rust.
This generated code provides a data structure to represent the data declared in the Eqlog program, and it provides a `close` method that operates on this data structure which adds data according to the rules of the Eqlog program until all rules are satisfied.

## Example: Control flow graphs

Consider reachability analysis for the control flow graph (CFG) of a function in a given programming language.
This is a common subtask during compilation and static analysis of programming languages.
A CFG is a representation of a function's execution flow as directed graph:
The nodes of the graph represent basic code blocks without branches, and edges represent possible direct execution flows between basic blocks.
The CFG of a function has a dedicated entry node, and we call a node `n` *reachable* if there is a path from the entry node to `n`.

Here's a straightforward implementation of the CFG data structure and reachability in Eqlog:
```eql
type Node;

pred edge(from: Node, to: Node);
pred reachable(node: Node);
func entry_node(): Node;

rule entry_reachable {
    if n = entry_node();
    then reachable(n);
}
rule reachable_step {
    if reachable(n);
    if edge(n, m);
    then reachable(m);
}
```

If we compile this Eqlog program, we get a Rust module that provides a `CFG` data structure.
Our host program can instantiate this data structure, populate `Node`, `edge` and `entry_node` data based on the abstract syntax tree (AST) of a given function, and then run the `close` method to compute reachability in the CFG.

While this works, it's somewhat unsatisfactory:
Ideally, we'd like to move more logic into the Eqlog program and only instantiate the Eqlog program once for the whole AST, which will contain the ASTs of many functions.
However, we can't easily use the Eqlog program above as part of a larger Eqlog program that analyzes multiple functions at once.
The problem is that we need to instantiate separate CFGs for each function in our Eqlog program, but Eqlog does not have a notion of "instance" of a subprogram, for example the CFG program.

We can work around this by parametrizing types, predicates and functions in our CFG program with elements of a new type to encode different instances of the original CFG program:
```eql
// A type representing instances of CFGs.
type CFG;

type Node;
func node_owner(node: Node) -> CFG;

pred edge(cfg: CFG, from: Node, to: Node);
pred reachable(cfg: CFG, node: Node);
func entry_node(cfg: CFG): Node;

rule entry_reachable {
    if n = entry_node(cfg);
    then reachable(cfg, n);
}
rule reachable_step {
    if reachable(cfg, n);
    if edge(cfg, n, m);
    then reachable(cfg, m);
}
```
Instantiating CFGs based on AST nodes in Eqlog, without involvement of the host program, will now look something like this:
```eql
// Types representing AST nodes.
type FunctionAST;
type BasicBlock;
pred basic_block_in_function(block: BasicBlick, function: FunctionAST);

func function_cfg(function: FunctionAST) -> CFG;
rule function_cfg_total {
    if function: FunctionAST;
    then function_cfg(function)!;
}

func basic_block_node(block: BasicBlock, function: Function) -> Node;
rule basic_block_node_total {
    if basic_block_in_function(block, function);
    then node := basic_block_node(block, function)!;
    then node_onwer(node) = function;
}

...
```

This accomplishes the goal of moving more logic from the host program into the Eqlog program, but at a cost:
We've had to clutter our original code with mentions of a `CFG` element everywhere.
Our Eqlog program also got more brittle, since it relies on the unenforced convention that we never create an edge between nodes of different CFGs.

Finally, evaluation of the rules governing the `reachable` predicate will now likely be slower than with the original approach.
In terms of runtime behavior, what we've done as we went from the first to the second solution is that we've combined many small graphs into a single large graph with many isolated subgraphs, and now we compute reachability in the larger graph.

The runtime of reachability computation is dominated by join computations on the respective edge tables.
And while the total number of joins required is roughly the same with both approaches, each join will be more costly with the second approach than with the first one because the edge table is much larger.

Is there way to retain the advantages of our first CFG program while making it compose well into larger Eqlog programs like our second version?
With dependent Datalog models, which I describe in the rest of this post, the answer is yes.

## Models and members

The `model` keyword is used to designate a fragment of an Eqlog program for future instantiation:
```
model CFG {
    // Same code as in the original CFG program, but now the
    // definitions are *members* of the CFG model.
    type Node;

    pred edge(from: Node, to: Node);
    pred reachable(node: Node);
    func entry_node(): Node;

    rule entry_reachable {
        if n = entry_node();
        then reachable(n);
    }
    rule reachable_step {
        if reachable(n);
        if edge(n, m);
        then reachable(m);
    }
}
```
Each `model` definition introduces a type (here: `CFG`) whose elements represent separate instances of the Eqlog program in the model definition body.
Similarly to classes in object oriented orientation, we'll refer to the definitions in the body as *members* (here: `Node, edge, reachable, entry_node`).

Members are only accessible via an element of the model type.
Whenever an expression `t` has model type, we expect a way to access members of the model instance associated to `t`.
Following the typical convention, I think the dot character is appropriate here, so that `t.<member>` refers to the member.

With the `CFG` model definition above, this allows us to write code like this:
```eql
type BasicBlock;

func basic_block_node(cfg: CFG, bb: BasicBlock) -> cfg.Node;
func node_basic_block(cfg: CFG, node: cfg.Node) -> BasicBlock;

rule node_basic_block_inverse {
    if cfg: CFG;
    if n_0: cfg.Node;
    if bb = node_basic_block(cfg, n_0);
    if n_1 = basic_block_node(cfg, bb);
    then n_0 = n_1;
}

pred is_entry_block(bb: BasicBlock);
rule cfg_entry_block {
    if n = basic_block_node(cfg, bb);
    if is_entry_block(bb);
    then cfg.entry_block() = n;
}

pred basic_block_reachable(bb: BasicBlock);
rule cfg_reachability {
    if n = basic_block_node(cfg, bb);
    if cfg.reachable(n);
    then reachable(bb);
}
```
This snippet defines function `basic_block_node` that associates a node in a given CFG to a basic block.
Observe that the result type `cfg.Node` depends on the parameter `cfg: CFG`.
The `node_basic_block` function is meant to represent an inverse function to `basic_block_node`.
Here the type of the second argument `node: cfg.Node` depends on the first argument `cfg: CFG`.

The rule `basic_block_node_inverse` formalizes the equality `basic_block_node(cfg, node_basic_block(cfg, n)) = n`.
Here the type `cfg.Node` of `n_0` and `n_1` depends on the element `cfg` that was introduced earlier in the rule.
The rules `cfg_entry_block` and `cfg_reachability` demonstrate access of member functions and member predicates, respectively.

Member types depend on the expressions they are accessed with.
Since type checking requires comparing types, we now have to decide expression equality during type checking.
Consider the following example:
```eql
rule bad_argument {
    if cfg_0: CFG;
    if cfg_1: CFG;
    if n: cfg_0.Node;

    // Type error: Expected argument of type cfg_1.Node, got cfg_0.Node.
    then cfg_1.reachable(n);
}
```
This program should not type check because `cfg_1.Node` is potentially different from `cfg_0.Node`.
But if we added the assumption `if cfg_0 = cfg_1`, then we would expect the program to type check.
This means that type checking Eqlog programs now involves deciding equality among expressions.

To enforce the surjectivity restrictions I alluded to earlier, the Eqlog type checker already needs to decide equality of expressions in rules, so I don't think this would add significant additional complexity.
Note that the equality decision procedure takes only implicit functionality rules (i.e. `x = y => f(x) = f(y)`) into account but not user-supplied rules in the Eqlog program, as this would make the type checker run indefinitely on some input programs.

## Runtime representation

How should model definitions and elements of model types be represented at runtime?

**Desugaring.**
One way would be to desugar models into non-dependent Eqlog, similarly to how we turned our first CFG implementation into a parametrized version.
So the Eqlog compiler would internally add the following definitions for each model definition:

* A global type `M` representing instances of the model.
* For each type member, a global type `T` and a function `belongs_to(x: T) -> M`.
* For each member predicate, a global predicate `p` that takes the arguments of the member and furthermore an argument of type `M`.
  Similarly for member functions.
* For every member rule, a desugared global rule in which a new argument `m: M` is added to all member function and member predicate applications.
  Quantifications `x: T` over member types desugar to equalities `belongs_to(x) = m`.

For the `CFG` model example this would ultimately compile into something like this Rust code:
```Rust
struct CFG(u32);
struct Node(u32);

struct CompiledProgramData {
    cfgs: Set<CFG>,
    node_belongs_to: Map<Node, CFG>,
    edge: Set<(CFG, Node, Node)>,
    reachable: Set<(CFG, Node)>,
    entry_node: Map<CFG, Node>,
    ...
}
```

**Maps to model instances.**
Alternatively, we could first compile the model body in isolation, which would give us a data structure representing just one instance of the model:
```Rust
struct Node(u32);
struct CFGMembers {
    edge: Set<(Node, Node)>,
    reachable: Set<Node>,
    entry_node: Option<Node>,
}
```
We would then use this data structure to represent instances in the ambient Eqlog program:
```Rust
struct CFG(u32);

struct CompiledProgramData {
    cfgs: Map<CFG, CFGMembers>,
    ...
}
```

This second representation leads to faster evaluation of member rules, since those can be evaluated in isolation for each instance.
But it also takes up more memory if the members of model instances are mostly empty, and it will probably make it more costly to access members when evaluating rules of the ambient program involving those members.

Eqlog and other Datalog engines often maintain indices on relations, i.e. multiple representations of the same data that are suitable for computing different types of joins efficiently.
So perhaps that's also the way to go here:
Simply use both representations to improve runtime performance at the cost of memory usage.

## Morphisms

Originally I intended to also write at length about [*morphisms*](https://en.wikipedia.org/wiki/Category_(mathematics)) between instances, but decided to postpone a proper discussion to some other time because I haven't worked out some relevant details.
The rough idea is that we add a built-in type of morphisms between two elements of the same model.
Semantically, morphisms should represent maps of data in the domain instance to data in the codomain instance.
So if the model formalizes graphs, then morphisms would be edge-preserving maps on vertices, and for groups you'd get group morphisms.

The point of having a built-in notion of morphisms is that the Datalog engine can implement those morphisms efficiently via *shallow* instead of deep copies of tables in the domain instance.
For example, if there's a morphisms `f : x -> y` and no other rules asserting facts about the codomain instance `y`, then tables in `y` can be represented simply as pointers to the tables of `x`.
And if we use [persistent insertion operations](https://en.wikipedia.org/wiki/Persistent_data_structure) into tables, then we can share chunks of tables among `x` and `y` even if the tables in `y` have some more entries.

### Scopes

The concrete problem this is supposed to solve is the maintenance of *scopes* when implementing programming languages in Eqlog.
A scope captures the set of variables that are accessible at a given AST node and possibly its type.
You could represent scopes like so:
```eql
func var_type(node: ASTNode, var: Identifier) -> Type;
```

The problem with this is that the program has to deal with a large number of AST nodes, and so the table representing `var_type` will be prohibitively large.

Since many nodes share the same scope, we can improve this by introducing a `Scope` model:
```
model Scope {
    func var_type(var: Identifier) -> Type;
}
func scope(node: ASTNode) -> Scope;
```
Now we can at least express when two nodes have the same scope.
For example, expressions (as opposed to statements) can usually not introduce new variables, so you can add a rule like this:
```
rule {
    if is_child_expression(child, parent);
    then scope(child) = scope(parent);
}
```
This way, there's only one `var_type` table for both `child` and `parent`.

But a problem remains:
Every variable declaration, for example by a `let` statement, increases the `var_type` table of the associated scope.
We could implement this like so:
```
pred scope_extension(smaller: Scope: larger: Scope);
rule scope_extension_var_type {
    if scope_extension(s_1, s_2);
    if s_1.var_type(var) = ty;
    then s_2.var_type(var) = ty;
}
rule {
    if second_stmt = successor_statement(first_stmt);
    if stmt_introduces_variable(first_stmt, var, ty);
    then scope(second_stmt).var_type(var) = typ
    then scope_extension(scope(first_stmt), scope(second_stmt));
}
```
But this would mean that whenever a new variable is introduced, we take a deep copy of the current scope and insert the new variable into it.
For a sequence of $n$ statements that each introduce a new variable, this would result in $\mathcal{O}(n^2)$ memory usage.

With morphisms, we could express that one scope is an extension of another.
If the compiler implements morphisms via shallow copies, then we'd cut the memory required for storing scope tables down to $\mathcal{O}(n \cdot \mathrm{log}(n))$, which is probably the best you can hope for in a language without mutation.
