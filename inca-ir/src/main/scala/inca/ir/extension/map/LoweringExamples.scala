package inca.ir.extension.map

import inca.ir.lowering.BaseLowering
/*
Example 1: Empty map
P(m: TMap[TNothing, TNothing]) :- m = MapLit.empty.
==>
data Map$TNothing$TNothing = Map$TNothing$TNothing$0.
P(m: Map$TNothing$TNothing) :- m = !Map$TNothing$TNothing$0().
Map$TNothing$TNothing$enum(m: TDemand(Map$TNothing$TNothing), key: TDemand(TNothing), value: TNothing) :- nil.
-------------------------------------------------------------------------------------------
Corner case, how does the lowering of the following code look like?
P(m: TMap[TString, TInt]) :- m = MapLit.empty, m += "A" -> 1.
------------------------------------------------------------------------------------------
Example 2: Map literal
P(m: TMap[TString, TInt]) :- m = MapLit.from(("A", 1), ("B", 2)).
==>
data Map$TString$Int = Map$String$Int$0.
P(m: Map$TString$TInt) :- m = !Map$TString$TInt$0.
Map$TString$TInt$enum(map: TDemand(Map$TString$TInt), key: TDemand(TString), value: TInt) :-
  ?Map$TString$TInt$0(map), {key = "A", value = 1} \/ {key = "B", value = 2}.
------------------------------------------------------------------------------------------
Corner case, how does the lowering of the following code look like (duplicate keys):
P(m: TMap[TString, TInt]) :- m = MapLit.from(("A", 1), ("A", 2)).
------------------------------------------------------------------------------------------
Example 3: Map union (map update is just a variant of map union)
P(m: TMap[TString, TInt]) :- m1 = MapLit.from(("A", 1)), m2 = m1 ∪ MapLit.from("B", 2).
==>
data Map$TString$TInt = Map$TString$TInt$0 | Map$TString$TInt$1 | Map$TString$TInt$2(m: Map$TString$TInt).
P(m: Map$TString$TInt) :- m1 = !Map$TString$TInt$0(), m2 = !Map$TString$TInt$3(m1).
Map$TString$TInt$enum(map: TDemand(Map$TString$TInt), key: TDemand(TString), value: TInt) :-
|  ?Map$TString$TInt$0(map), {key = "A", value = 1}.
|  ?Map$TString$TInt$1(map), {key = "B", value = 2}.
|  ?Map$TString$TInt$2(map, m1), { Map$TString$TInt$enum(m1, key, value) } \/
   { Map$TString$TInt$enum(!Map$TString$TInt$1(), key, value) }.
------------------------------------------------------------------------------------------
Example 4: Map from
Q(k: TString, v: TInt) :-
|  k = "A", v = 1.
|  k = "B", v = 2.
P(m: TMap[TString, TInt]) :- m = MapFrom("Q").
==>
data Map$TString$TInt = Map$TString$TInt$0.
P(m: Map$TString$TInt) :- m = !Map$TString$TInt()
Map$TString$TInt$enum(map: TDemand(Map$TString$TInt), key: TDemand(TString), value: TInt) :-
  ?Map$TString$TInt$0(map), {key = "A", v = 1} \/ {key = "B", v = 2}.
 */



trait LoweringExamples extends BaseLowering