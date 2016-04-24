APLite
======

Compile with `ghc -o aplite Repl.hs`

Calculating factorials
```
> + / iota 10
3628800
```

Matrices are supported
```
> 3 3 # 3 * iota 3*3
3   6   9
12  15  18
21  24  27
```

Supported functions
-------------------

### Monadic functions

| Symbol | Meaning          |
|--------|------------------|
| `~`    | Boolean not      |
| `|`    | Absolute value   |
| `-`    | Negation         |
| `iota` | Vector [1..n]    |
| `#`    | Dimension        |

### Dyadic functions

| Symbol   | Meaning                        |
|----------|--------------------------------|
|`+ - * %` | Plus, minus, times and divide  |
|`^`       | Exponentiation                 |
|`#`       | Reshape                        |

### Monadic operators

| Symbol | Meaning          |
|--------|------------------|
| `/`    | Reduce           |
| `\`    | Scan             |
