---
title: "Type Checking with Eqlog: Parsing"
date: "July 23, 2023"
lang: "en_US"
---

This is the first post in a series I intend to write on implementing a type checker using [Eqlog](https://github.com/eqlog/eqlog), a Datalog engine for Rust.
The repository containing the complete type checker is available [here](https://github.com/eqlog/examples-inference), and each post in the series will explain one aspect of the type checker.
Datalog is popular for static analysis, and Rust's [Chalk project](https://github.com/rust-lang/chalk) is an implementation of Rust's trait system based on Datalog.
However, there's not a lot of work on using Datalog for type checking (though see [here](https://dl.acm.org/doi/pdf/10.1145/3428195)), which is what this series is about.

[Eqlog](https://github.com/eqlog/eqlog) is a Datalog engine which I and, for previous versions, [Jakob Nielsen](https://www.jakobbotsch.com/) have been working on from time to time over the last few years.
Eqlog implements an extension of Datalog that allows it to infer equalities among elements during evaluation.
For example, you cannot directly encode the anti-symmetry axiom $x \le y \land y \le x \implies x = y$ in standard Datalog because of the equality in the conclusion, but this is trivial in Eqlog.
I expect that instead of Eqlog you can also use the [egglog](https://github.com/egraphs-good/egglog) Datalog engine, which works similarly to Eqlog.

The ability to reason about equality will be critical when get to type unification.
Lack of native support for equality is probably the reason why type checking using Datalog isn't a well-known technique even though Datalog is quite popular in the programming languages space.

I intend for each post in this series to introduce one aspect of the type checker.
This is my current outline, which I might need to update as I write the other posts:

1. **Parsing [[code](https://github.com/eqlog/examples-inference/tree/parsing)] (this post)**
2. [Variable binding](../type-checking-with-eqlog-variable-binding) [[code](https://github.com/eqlog/examples-inference/tree/binding)]
3. Types [[code](https://github.com/eqlog/examples-inference/tree/types)]
4. Expression typing [[code](https://github.com/eqlog/examples-inference/tree/expr-types)]
5. Function literal typing [[code](https://github.com/eqlog/examples-inference/tree/function-types)]
6. Hindley-Milner polymorphism [[code](https://github.com/eqlog/examples-inference/tree/hindley-milner)]

The [code] links above after each post lead to a branch in the repository that contains only the code discussed so far, which will hopefully make it easier to follow along.

## Project structure

The project structure of our type checker is that of a normal Rust project, and so can be compiled as usual using Cargo.
However, we'll write very little Rust code ourselves:
This will mostly be for reading input source files, generating errors and other glue code.
Instead, we'll implement all core logic using either the [LALRPOP](https://github.com/lalrpop/lalrpop) parser generator or Eqlog.
Both LALRPOP and Eqlog compile their respective source files into Rust modules, which we then include into our Rust crate to glue everything together.

We invoke the LALRPOP and Eqlog compilers by adding the following `build.rs` file in the crate root directory:
```rust
fn main() {
    lalrpop::process_root().unwrap();
    eqlog::process_root();
}
```
If it exists, Cargo executes a `build.rs` file before compiling the crate itself.
The two `process_root` functions traverse the crate directory and generate a Rust module from each file with `.lalrpop` or `.eqlog` extension.
(The similarity between LALRPOP and Eqlog here is obviously not an accident; I followed LALRPOP's compilation model in the implementation of Eqlog.)

Our LALRPOP grammar file is at [`src/grammar.lalrpop`](https://github.com/eqlog/examples-inference/blob/parsing/src/grammar.lalrpop), and the Eqlog theory file describing our type system is at [`src/program.eqlog`](https://github.com/eqlog/examples-inference/blob/parsing/src/program.eqlog).
We declare the Rust modules generated from these two files by putting the following lines in our `main.rs` file:
```rust
use eqlog_runtime::eqlog_mod;
eqlog_mod!(program); // Declares the `program` module.

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar); // Declares the `grammar` module.
```
The generated `grammar.rs` and `program.rs` source files are available somewhere under the `target` directory after building the crate, but they're not meant to be human-readable.
However, the crate's documentation, which you can browse via `cargo doc --open`, lists the symbols exported by these files.

## Representing ASTs in Eqlog

The result of parsing is an abstract syntax tree (AST).
ASTs are typically defined using algebraic data types, for example like so:
```rust
enum ExprNode {
    False,
    True,
    Equal { lhs: Box<Expr>, rhs: Box<Expr },
    App { func: Box<Expr>, args: Vec<Expr> },
    // ...
}

enum TypeNode {
    Boolean,
    // ...
}
```
Similarly to SQL, Eqlog operates on tuples in relations and thus cannot directly consume such tree data structures.
Fortunately, we can encode ASTs in Eqlog's data model as follows.
For each type of AST node, we add a *sort* declaration to `program.eqlog`:
```eqlog
Sort ExprNode;
Sort TypeNode;
```
"Sort" is just what Eqlog calls a type.
Each node constructor with $n$ parameters corresponds to a relation (or *predicate* in Eqlog terminology) with $n + 1$ components.
The first component of the relation corresponds to the node itself and the last $n$ components correspond to the parameters of the node constructor.
We thus add the following declarations:
```eqlog
Pred FalseExprNode : ExprNode;
Pred TrueExprNode : ExprNode;
// EqualExprNode(equal_expr, lhs, rhs)
Pred EqualExprNode : ExprNode * ExprNode * ExprNode;
Pred BooleanTypeNode : TypeNode;
```
The `App` constructor is more difficult to express because its `args` parameter corresponds to a variable number of `ExprNode` elements, whereas each Eqlog predicate has a fixed number of parameters.
We can work around this limitation by introducing a new sort `ExprListNode` encoding lists of `ExprNode` elements:
```eqlog
Sort ExprListNode;
Pred NilExprListNode : ExprListNode;
Pred ConsExprListNode : ExprListNode * ExprNode * ExprListNode;
```
Here `NilExprListNode(node)` should hold whenever `node` represents an empty list of expressions.
We can now declare `App` as follows:
```eqlog
// App(app_expr, func, args)
Pred App : ExprNode * ExprNode * ExprListNode; 
```

Some AST nodes are given not just by a number of AST node children but also some additional data.
For example, a node representing a string literal contains its value as a `String` field, and a node representing a variable contains a `String` for the name of the variable.
While some Datalog engines support strings, numbers and other primitive types, Eqlog does not.
Instead, we declare an Eqlog sort for each type of data that we need:
```eqlog
Sort Var;
Sort StringLiteral;
Sort NumberLiteral;
```
For each of these sorts, our Rust glue code maintains a hash map to associate values to the Eqlog elements that represent them:
```
pub struct Literals {
    pub vars: HashMap<String, Var>,
    pub strings: HashMap<String, StringLiteral>,
    pub numbers: HashMap<String, NumberLiteral>,
}
```
Since the mapping between Eqlog elements and values is maintained outside of Eqlog, we cannot inspect those values in Eqlog.
However, since we make sure that each value is represented by at most one Eqlog element, our Eqlog code can assume that elements of these sorts are equal if and only if their attached values agree.
This is all we shall need.

The `program.eqlog` file also declares `StmtNode` and `StmtListNode` sorts representing statements and lists of statements, an `OptTypeNode` sort representing type nodes that might be absent, and an `ArgListNode` sort representing lists of `(Var, OptTypeNode)` pairs.
Finally, there is a `ModuleNode` sort representing the root of the syntax tree and a sort of function nodes representing function definitions:
```eqlog
Sort OptTypeNode;
Pred SomeOptTypeNode : OptTypeNode * TypeNode;
Pred NoneOptTypeNode : OptTypeNode;

Sort ArgListNode;
Pred NilArgListNode : ArgListNode;
Pred ConsArgListNode : ArgListNode * Var * OptTypeNode * ArgListNode;

Sort FunctionNode;
// Function(node, function_name, domain, codomain, body)
Pred Function : FunctionNode * Var * ArgListNode * OptTypeNode * StmtListNode;
```

## Parsing into the Eqlog model

Having defined the Eqlog data model, we now turn to populating such models.
We can accomplish this using the interface of the generated Rust module.
There's a type for each sort we have declared in the Eqlog file.
Each of these is just a wrapper around an integer ID:
```rust
struct ExprNode(u32);
struct ExprListNode(u32);
struct TypeNode(u32);
// ...
```
The main type we need is the *model* type, in our case called `Program` based on the name of our Eqlog file.
You can think of the model object as an in-memory instance of an SQL database, with schema determined by the sorts and relations declared in the Eqlog file.
Thus, our `Program` type looks something like this:
```rust
struct Program {
    // Carrier sets of elements:
    expr_nodes: Set<ExprNode>,
    expr_list_nodes: Set<ExprListNode>,
    // ...

    // Relations:
    false_expr_node: Set<(ExprNode,)>,
    equal_expr_node: Set<(ExprNode, ExprNode)>,
    function: Set<(FunctionNode, Var, ArgListNode, OptTypeNode, StmtListNode)>,
    // ...
}
```
In later posts we will see that a `Program` object maintains also indices into its relations and union-find data structures to keep track of equality among elements, but these are not relevant for now.

The associated functions of the `Program` type we need for this post are as follows:

- `fn new() -> Program`  
   This function creates an empty `Program` object, i.e., an object that doesn't contain any elements (and thus also no tuples).
- `fn new_{{sort}}(&mut self) -> {{sort}}`  
   There is a function like this for each sort, for example `new_expr_node`.
   This adds a new element of the specified sort with a fresh ID to the model.
- `fn insert_{{predicate}}(&mut self, Sort1 arg1, Sort2 arg2, ..., Sortn argn)`  
   There is a function like this for each predicate, for example `insert_cons_expr_list_node`.
   The arguments of the function correspond to the components of the predicate.
   Note that each element passed as parameter here must be known to the `&self` instance, which usually means that you've obtained the element by calling one of the `new_{{sort}}` functions.

The syntax of our toy language is mostly inspired by TypeScript syntax.
I won't go into details of how LALRPOP works; there's an excellent [book](https://lalrpop.github.io/lalrpop/) about that.
In a nutshell, LALRPOP grammar files are given by the usual production rules and an attached snippet of Rust code for each rule that is executed when the rule fires.
The purpose of these Rust snippets is to generate the appropriate AST nodes.

In our case, we pass an initially empty `Program` object `p` and a `Literals` object as state into the parser, and each production rule adds data to these using the functions explained above.
For example, here are some relevant production rules for `ExprNode`:
```rust
Var: Var = {
    <s: r"[A-Za-z][A-Za-z0-9_]*"> => {
        match literals.vars.get(s) {
            Some(v) => *v,
            None => {
                let v = p.new_var();
                literals.vars.insert(s.to_string(), v);
                v
            },
        }
    },
}

Intersperse<Item, Separator>: Vec<Item> = {
    // ...
}

Expr0: ExprNode = {
    <var: Var> => {
        let expr = p.new_expr_node();
        p.insert_variable_expr_node(expr, var);
        expr
    },
    "true" => {
        let expr = p.new_expr_node();
        p.insert_true_expr_node(expr);
        expr
    },

    <function: Expr0> "(" <args: Intersperse<Expr, ",">> ")" => {
        let args = expr_list_node(args.as_slice(), p);
        let expr = p.new_expr_node();
        p.insert_app_expr_node(expr, function, args);
        expr
    },

    // ...
}

Expr1: ExprNode = {
    <lhs: Expr0> "==" <rhs: Expr0> => {
        let expr = p.new_expr_node();
        p.insert_equals_expr_node(expr, lhs, rhs);
        expr
    },

    // ...
}
```

Here `expr_list_node` is a convenience function that helps us create `ExprListNode` elements, and there are similar functions for other list nodes.
This is the definition of `expr_list_node`:
```rust
pub fn expr_list_node(nodes: &[ExprNode], p: &mut Program) -> ExprListNode {
    let mut l = p.new_expr_list_node();
    p.insert_nil_expr_list_node(l);
    for node in nodes.iter().rev() {
        let cons = p.new_expr_list_node();
        p.insert_cons_expr_list_node(cons, *node, l);
        l = cons;
    }
    l
}
```
Note that we have to iterate the slice in reverse since, by convention, `ConsExprListNode` represents *pre*pending a node to a list of nodes.

Finally, the `check_source` function, which ties everything together, looks like this:
```rust
fn check_source(src: &str) -> Result<(Program, Literals, ModuleNode), LanguageError> {
    let no_comments_src = erase_comments(src);

    let mut p = Program::new();
    let mut lits = Literals::new();

    let module = ModuleParser::new()
        .parse(&mut p, &mut lits, &no_comments_src)
        .map_err(|err| LanguageError::from_parse_error(err, &no_comments_src))?;

    Ok((p, lits, module))
}
```
