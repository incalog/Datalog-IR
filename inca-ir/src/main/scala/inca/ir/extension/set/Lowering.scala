package inca.ir.extension.set

/* I'll briefly sketch to different, but similar approaches of how first-class sets
 * (without the empty set) could be handled:
 *
 * Proposal 1: Represent set with IDs expressed as ADTs
 * E.g
 * main(z: Set[Int]) :- y == Set(1,2,3), somCall(y, z).
 * someCall(y: Set[Int], z: Set[Int]) :- z == (y U Set(2,4))
 *
 * Lowering:
 * SetADT = Set$0 | Set$1 | Set$2(y)
 *
 * set(Set$0, x: Int) :- #prefix, (x == 1 v x == 2 v x == 3)
 * set(Set$1, x: Int) :- #prefix, (x == 2 v x == 4)
 *
 * set(Set$2(y), x: Int) :- #prefix (with y), (set(y, x) v set(Set$1, x))
 *
 * main(z: Int) :- y == Set$0, somCall(y, z).
 * someCall(y: SetADT, z: Int) :- set(Set$2(y), z).
 *
 * We replace a Set with an ID that we represent by an ADT. Instead of passing around
 * a set, we pass around an ADT value. We create one (or multiple) set relation that
 * include all atoms up to this point as a prefix. When we read a set, we query this
 * set relation with the ID we generated for the set.
 *
 *
 *
 * Proposal 2: Represent set with their Values
 * E.g
 *
 * main(z: Set[Int]) :- y == Set(1,2,3), somCall(y, z).
 * someCall(y: Set[Int], z: Set[Int]) :- z == (y U Set(2,4))
 *
 * Lowering:
 * set$0(x: Int) :- (x == 1 v x == 2 v x == 3)
 * set$1(x: Int) :- (x == 2 v x == 4)
 *
 * // All bound variables we need to construct the set are inputs to the relation
 * set$2(x: Int, z: Int, y: Int) :- (y == x v y == z)
 *
 * main(z: Int) :- set$0(tmp), y == tmp, someCall(y, z)
 * someCall(y: Int, z: Int) :- set$1(tmp), set$2(y, tmp, z).
 *
 * Instead of representing Set with ADT Ids, we pass around the values of a set directly.
 * I don't think we need a prefix here, since we just pass the concrete set value to the helper
 * relations.
 * Disadvantage: If the set contains tuple of size n we might end up passing around n values.
 * I'm still not sure if there are cases where this does not work...
 *
 * Problem: Eq / Neq does not work anymore
 * Before: Set(1,2) == Set(2,4) // False
 * After: 1 == 2 v 1 == 4 v 2 == 2 v 2 == 4 // One body is executed although no body should be executed
 */
