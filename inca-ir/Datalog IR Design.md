# Design Document: Extensible Datalog IR



### Goals

- Support exploration of Datalog optimizations that exploit IR extensions.
- Easier compilation to Datalog by exploiting IR extensions.



### Design principles

- layered IR architecture: core Datalog extensible with different IR extensions, each lowering to simpler Datalog
- compatibility with module system



### IR extensions

#### Demand predicates

```
a ::= ... | demanded(t1,...,tn)
```

Can be used to realize demand-driven derivations. Demanded behaves like a family of built-in predicates that can be used to bind any number of variables (so the Datalog rule is range restricted). Applies a partial demand transformation where needed to satisfy the demanded predicates.

#### Disjunctions

```
a ::= ... | a* \/ ... \/ a*
```

Allows nested disjunctions at the level of atoms, whereas standard Datalog only allows top-level disjunctions at the level of rules.

```
lowering
R(X1,...,Xn) = b1 \/ .. \/ (as1 \/ ... \/ asm) \/ ... \/ bn
->
R(X1,...,Xn) = b1 \/ .. \/ as1 \/ ... \/ asm \/ ... \/ bn

apply repeatedly to eliminate nested disjunctions (when as_i also is a disjunction)
```



#### First-class sets (including empty set)



#### Booleans



#### 