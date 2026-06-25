# INCA CodeQL frontend

`inca-codeql` parses a deliberately executable subset of QL and lowers it to INCA's Datalog IR.

## Supported QL

- Primitive types: `int`, `float`, `string`, `boolean`, and `date`.
- Entity type names, represented as opaque `Any` values in INCA.
- Predicate and result-predicate declarations.
- Concrete classes, fields, inheritance, characteristic predicates, `this`, member predicates, and overrides.
- `external` predicates as EDB relations.
- `from`/`where`/`select` queries.
- Predicate calls, equality, comparisons, arithmetic, conjunction, disjunction, negation, `exists`, and finite integer ranges.
- Recursive predicates.

Predicate/member overloading, abstract classes, imports/modules, casts, aggregates, closures, and unbounded or non-constant ranges are rejected. They require additional relational lowering and must not be silently approximated.

Classes lower to finite membership relations. Fields are relation columns, inheritance joins the base membership relations, member predicates receive an explicit first `this` column, and generated dispatch relations select the most-specific applicable override.

## EDB generation

CodeQL extractors first emit TRAP files. `codeql database create` imports those files according to a language-specific `.dbscheme` into CodeQL's binary dataset. A finalized database is not a portable directory of CSV fact files.

`CodeQlCli` therefore uses an explicit bridge per INCA external predicate:

1. Create the CodeQL database with `CodeQlCli.createDatabase`.
2. Supply an `EdbExportQuery` whose normal CodeQL `select` columns match the external predicate.
3. The bridge runs `codeql query run`, decodes BQRS to CSV, converts values according to the external predicate signature, and inserts the resulting INCA relation.

```scala
val source = """
  external predicate pythonFile(string path);
  from string path where pythonFile(path) select path
"""

val compiled = executor.compileCodeQl(source)
val exports = Seq(EdbExportQuery(
  Name("pythonFile"),
  """
    import python
    from File f
    select f.getRelativePath()
  """
))
val loaded = executor.loadCodeQlDatabase(compiled, database, workDir, exports)
```

The default CLI path is `~/CodeQL/codeql/codeql` and can be overridden through the `CodeQlCli` constructor.
