## The case for morphisms: Shallow copies

Our original CFG example computed the set of nodes reachable from a designated entry node.
But sometimes we're interested in the binary reachability relation where the entry node is not fixed, i.e., the set of all tuples `n_1, n_2` such that there exists a path from `n_1` to `n_2`.
This is also known as the [reflexive transitive closure] of the original graph.

Here's a straightforward modification of our original CFG implementation that computes the transitive closure:
```eql
type Node;

pred edge(from: Node, to: Node);
pred reachable(from: Node, to: Node);

rule reachable_edge {
    if edge(n_1, n_2);
    then reachable(n_1, n_2);
}
rule reachable_reflexive {
    if n: Node;
    then reachable(n, n);
}
rule reachable_transitive {
    if reachable(n_1, n_2);
    if reachable(n_2, n_3);
    then reachable(n_1, n_3);
}
```
In the general case, the size of a transitive closure is up to quadratic in the size of the original relation.
But in many cases, if it is known that the graph structure is of a certain type, then more efficient representations exist.
Consider for example a linked list:
```
n_1 --> n_2 --> n_3 --> ... --> n_k
```
If we store tuples in the reachability relation naively in a balanced binary search tree (BST), then the transitive closure takes up $\mathcal{O}(k^2)$ space.

If we assume lexicographic ordering on tuples, then this naive representation allows efficient lookups into the `reachable` predicate with either both components or only the first of the tuple fixed:
We can quickly lookup whether there's a path between two given nodes, and we can quickly iterate the set of reachable nodes from a fixed node, but we cannot quickly iterate over all nodes for which a path to a fixed node exists.
If we also need this kind of lookup, then we would need to maintain another search tree with reverse lexicographic ordering.

### Persistent data structures

By using [persistent data structures](https://en.wikipedia.org/wiki/Persistent_data_structure) we can get to a more efficient representation of the `reachable` predicate that supports the same types of fast lookups.
Modifying a persistent data structure is non-destructive, i.e. the previous version of the data structure is preserved.
This can be achieved efficiently by sharing parts of the data structure that are not affected by the mutation between the old and the new version.
For example, insertion into BSTs can be made persistent by creating copies of all nodes with descendants that would be modified by the mutating insertion algorithm.
The number of copies required is only logarithmic in the size of the tree, which means that most nodes can be shared between the old and new trees.

In the linked list example, we can efficiently create a persistent BST for each node $n_i$ that represents the set of nodes reachable from $n_i$ by working backwards from the last node to the first:
For the last node $n_k$, this is the singleton set ${ n_k }$.
For all other nodes $n_i$ we can obtain the set by inserting $n_i$ into the reachable node set of $n_{i + 1}$.
Since each insertion increases memory consumption by only $\mathcal{O}(\mathrm{log}(k))$, our total memory consumption is in $\mathcal{O}(k \cdot \mathrm{log}(k))$.
We then create a binary search tree to associate each node $n_i$ to the set of reachable nodes of $n_i$, which again takes at most $\mathcal{O}(k \cdot \mathrm{log}(k))$ time and memory.

### Extensions and morphisms

How can we help the Datalog compiler choose a persistent data structure?
I argue we need two mechanism:

1. A way to tell the Datalog compiler to choose nested maps of small sets to represent data instead of a flat representation as large set of tuples: `nested: Map<S, Set<T>>` instead of `flat: Set<(S, T)>`.
   We've already provided such a mechanism via the `model` mechanism, see [SECTION].
   ```
   type Node;
   pred edge(from: Node, to: Node);

   model NodeSet {
       pred contains(n: Node);
   }
   func reachable(n: Node) -> NodeSet;

   rule self_reachable {
       if n: Node;
       then reachable(n)!;
       then reachable(n).contains(n);
   }
   rule self_reachable {
   }
   ```
2. A way to inform the Datalog compiler about subset relations between





### More abstract nonsense

If we want to allow using types of morphisms inside of model definitions, then we have to be careful about covariance and contravariance.
Consider this example:
```
model A {
    type Foo;
}

model K {
    func a(): A;

    // OK: The functor (k: K) |-> k.ok_map_into_a() is covariant in k.
    func map_into_a(): Hom(-, a());

    // K: The functor (k: K) |-> k.ok_map_out_a() is covariant in k.
    func map_out_of_a(): Hom(a(), -);

    func bad(): Hom(a(), a());
}
```
What data should a morphism `f : k_0 -> k_1` between instances of `K` represent?
For the `a()` constant, we want

I also thought a bit about incorporating the Yoneda embedding, so that inside of a model definition there would be a type `Hom(<foo>, <type dependent on this>)` of morphisms into a type dependent on the current instance of the model being defined.
This would be useful to representing *type contexts* in languages with generics.
In languages with generics, it's not enough to associate a simple type to a variable binding, since at the instantiation site you need to know which of the type variables must or must not be instantiated.

Compare these cases:
```Rust
// Case 1: Instantiate
fn foo<T>(t: T) { }
fn bar<S>(s: S) {
    foo(s); // OK: Instantiate foo with T := S.
}

// Case 2:
fn baz<T>(t: T) {
    fn qux<S>(t0: T, s: S) { ... }
    qux(t, t); // OK: Instantiate qux with S := T.
    qux(5, t); // Not OK: Inside the scope of baz, qux is generic only in S but not T.
}
```

An elegant way to handle this is by bundling both scope and the set of *simple* types, i.e. types that are already fully instantiated, into a *context* structure.
We represent simple types as Eqlog type within context and type formers as Eqlog functions on this type:
```
model Context {
    type SimpleType;
    func number(): SimpleType;
    func tuple(lhs: SimpleType, rhs: SimpleType) -> SimpleType;
    ....

    // Type variables that act as simple types in the current
    // context, e.g. T in the body of the foo function above.
    func type_var(ident: Ident) -> SimpleType;
}

model Scope {
    func context: Context;

    // The set of bound vars.
    // TODO: Doesn't work for generic variables bound to
    // generic definitions, e.g. foo within the scope of
    // bar the first example.
    func var_types(var: Ident) -> SimpleType;
```
The `var_types` needs some work though, since it models the type of variables in scope as simple types in the current context, whereas the types of generic definitions are simple only in their own context.
What's needed is that the typing information for bound variables is given by a diagram of context morphisms $p_1, p_2$ of shape
```
   base_ctx --- p_1 --> definition_ctx
      |
      |
     p_2
      |
      |
      ∨
   current_ctx
```
and a simple type in `definition_context`.
The `base_ctx` is the largest context shared by both `definition_context` and `current_ctx`.
In the first example, within the scope of `bar`, the type of `foo` would be given by `base_ctx` equal to the global context (no type variables) and `definition_ctx` equal to

In the second example involving nested generic functions, it's the context of the body of `foo`, where `T` is a simple type.

To instantiate the type into `current_context`, we would then add a morphism and demand that the following diagram commutes:
```
   base_ctx --- p_1 --> definition_context
      |                   /
      |                 /
     p_2            subst
      |             /
      |           /
      ∨         ∨
   current_ctx
```
If we find the existence of this `subst` morphism implies that bad type equation in current_ctx holds, for example that the number type must be equal to the string type, then we issue a typing error.
In the second example The commutativity of the diagram enforces that 
