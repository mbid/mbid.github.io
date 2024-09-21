---
title: "Self-hosting Eqlog"
date: "November 02, 2023"
lang: "en_US"
---

This post is about the my recent rewrite of Eqlog's type checker in Eqlog itself.

I finished prototyping a Hindley-Milner style type checker using Eqlog a while ago TODO:link.
Type checking is one of the supposed killer applications of Datalog and especially Eqlog. so it's only fair to expect that Eqlog's type checker is also written in Eqlog.

I don't want to go into too much detail, since a lot of what is entailed by implementing a type checker in Datalog or Eqlog is not particularly specific to Eqlog's type system:
We need to encode the AST emitted by the parser in a relational format so that it can be consumed by Eqlog, and, propagate data such as ambient symbol bindings through the AST and collect typing constraints on variables.
To avoid confusion, I'll refer to the Eqlog program that models Eqlog's type system as "eqlog.eql", whereas "Eqlog" refers to the Rust executable that compiles `.eql` files into Rust source code and happens to embed `eqlog.eql`.

## Locations & error messages

A generality that I do want to remark on is generating error messages, since I completely left this part out in the series on Hindley-Milner type checking.
To be easily actionable, error messages must be associated with a location in the input source code.
And to have access to locations also after parsing, we need to save locations along with AST nodes.
The approach Eqlog uses here now is to associate each AST node a `Loc` element representing a location in source code:
```eql
type Loc;
func type_decl_node_loc(TypeDeclNode) -> Loc;
func pred_decl_node_loc(PredDeclNode) -> Loc;
...
```
Like all Eqlog types, `Loc` is actually just an integer ID under the hood.
The actual source code range that a `Loc` represents is given by a `Map<Loc, (usize, usize)>` that is maintained in Rust, outside of `eqlog.eql`.
Ideally, Eqlog would support some kind of "external" types and provide this mapping for us, but for now we have to maintain it manually.

Now to the interesting part about error messages in Eqlog:
Sorting and prioritizing.

In a traditional compiler, there's a usually a ordering in which it encounters an error in the input source code.
For example, the compiler might have a name resolution phase, which reports errors about undeclared identifiers, followed by a type inference phase, which reports errors about ambiguous types.
Within each phase, or even when the compiler checks multiple properties in the same pass (i.e., in the same traversal of the AST), it will typically encounter the error that originates from the earliest position in the input first.
