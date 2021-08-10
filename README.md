# IncA-Scala

## Overview
IncA-Scala is a reimplementation of the program analysis framework [IncA](https://github.com/szabta89/IncA)

It comes with two different DSLs for defining program analyses:
- A functional frontend inspired by functional programming
- A constraint-based frontend inspired by logic programming

The project also comes with a runtime system that evaluates program analyses incrementally to achieve the performance that is needed for real-time feedback in IDEs. When code gets changed, the IncA runtime system incrementally updates analysis results instead of the repeated recomputation from scratch. We translate program analyses to Datalog, and we reuse existing Datalog solvers to evaluate the analysis code on the AST of the analyzed program. The IncA runtime system reuses the existing Datalog solver [ViatraQuery](https://wiki.eclipse.org/VIATRA/Query) which is based on incremental graph pattern matching. We can further target [Soufflé](https://souffle-lang.github.io/) as a Datalog dialect which compiles to a scalable and efficient Datalog solver written in C++.

## Getting Started
To build, install the [sbt](https://www.scala-sbt.org) build tool and run `sbt compile` from the root directory of the project.

To execute the provided tests run `sbt test` from the root directory.

Example programs such as static analyses can be found in the test package `inca.examples` for the functional and the constraint frontend of IncA-Scala.

## Publications
IncA-Scala is heavily based on the research conducted on IncA. IncA's various features have been documented in the following publications:

* **Incremental Whole-Program Analysis in Datalog**, Tamás Szabó, Sebastian Erdweg, and Gábor Bergmann
In *Programming Language Design and Implementation (PLDI)*. 2021 [[pdf]](https://www.pl.informatik.uni-mainz.de/files/2021/06/inca-whole-program.pdf)

* **Incrementalizing inter-procedural program analyses with recursive aggregation in Datalog**, Tamás Szabó, Gábor Bergmann, and Sebastian Erdweg.
In *Workshop on Incremental Computing (IC)*, 2019. [[pdf]](https://szabta89.github.io/publications/inca-ic-2019.pdf)

* **Incrementalizing Lattice-Based Program Analyses in Datalog**, Tamás Szabó, Gábor Bergmann, Sebastian Erdweg, and Markus Völter.
In *Proceedings of Conference on Object-Oriented Programming, Systems, Languages, and Applications (OOPSLA)*, 2018. [[pdf]](https://szabta89.github.io/publications/inca-oopsla.pdf)

* **Incremental Overload Resolution in Object-Oriented Programming Languages**, Tamás Szabó, Edlira Kuci, Matthijs Bijman, Mira Mezini, and Sebastian Erdweg.
In *Proceedings of International Workshop on Formal Techniques for Java-like Programs (FTfJP)*, 2018. [[pdf]](https://szabta89.github.io/publications/inca-ftfjp.pdf)

* **IncAL: A DSL for Incremental Program Analysis with Lattices**, Tamás Szabó, Sebastian Erdweg, and Markus Völter.
In *Workshop on Incremental Computing (IC)*, 2017. [[pdf]](https://szabta89.github.io/publications/inca-ic.pdf)

* **IncA: A DSL for the Definition of Incremental Program Analyses**, Tamás Szabó, Sebastian Erdweg, and Markus Völter.
In *Proceedings of International Conference on Automated Software Engineering (ASE)*, 2016. [[pdf]](https://szabta89.github.io/publications/inca-ase.pdf)

## Acknowledgments
The IncA-Scala project is a joint effort of the following people (in alphabetical order):

André Pacak, Don Lihinikadu, Julian Cichorius, Mohammadsaleh Oshaghi, Moritz Schmidtgen, Paul Hempel, Ronja Schnur, Sebastian Erdweg, Tamas Szabó, Tomislav Pree
