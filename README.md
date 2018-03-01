# Nim Asciimath

Asciimath parser and LaTeX generator implemented in Nim. It's based on
slightly modified [asciimath](http://asciimath.org/) grammar.

Currently the only purpose of this module is translating asciimath to LaTeX:
```nim
import asciimath

echo "sum_(i=1)^n i^3=((n(n+1))/2)^2".toLatex()
```
