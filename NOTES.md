## 'ord' intrinsic functor

The intrinsic functor ord(string) is used to return the ordinal number associated with string.
This is not a lexicographic ordering.
The ordinal number is based on the order of appearance (see example below).
(from https://souffle-lang.github.io/arguments)

## Desugaring rules

Surface:
	Multiple heads, disjunctions allowed

Core:
	One head, only conjunctions

```c
A, B :- C

A :- C
B :- C

// ---------------
A :- B; C.

A :- B.
A :- C.

// ---------------

A :- B, (C; D).

A :- B, C.
A :- B, D.

// ---------------

A :- B, (C; D; E, F)

A :- B, C
A :- B, D
A :- B, E, F
```

```c

(A; (B; C)) ===> (A; B; C)

TermDisjunction(Disjunction(
	Conjunction(TermAtom(A)),
	Conjunction(
		TermDisjunction(Disjunction(
			Conjunction(TermAtom(B)),
			Conjunction(TermAtom(C))
		))
	)
))
===>
TermDisjunction(Disjunction(
	Conjunction(TermAtom(A)),
	Conjunction(TermAtom(B)),
	Conjunction(TermAtom(C))
))

(A; (B; C), D) ===> (A; B, D; C, D)

TermDisjunction(Disjunction(
	Conjunction(TermAtom(A)),
	Conjunction(
		TermDisjunction(Disjunction(
			Conjunction(TermAtom(B)),
			Conjunction(TermAtom(C))
		)),
		TermAtom(D)
	)
))
===>
TermDisjunction(Disjunction(
	Conjunction(TermAtom(A)),
	Conjunction(TermAtom(B), TermAtom(D)),
	Conjunction(TermAtom(C), TermAtom(D))
))



A, (B; C) ===> A, B; A, C

Conjunction(
	TermAtom(A),
	TermDisjunction(Disjunction(
		TermAtom(B),
		TermAtom(C)
	))
)
===>
Conjunction(
	Term
)

```

A, (B; C), (D; E)
		A, B, (D; E)
				A, B, D
				A, B, E
		A, C, (D; E)
				A, C, D
				A, C, E
