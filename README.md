bindedEval
===

This is a R package which provides `let()` and `where()` functions which evaluates expressions with name binding. They mimicks Haskell's `let` formula and `where` clause.

Installation
---
```R
install_github("ngr-t/r-bindedEval")
```

Usage
---

```R
library(magrittr)
library(bindedEval)

z <- let(x = 1, y = 2, .in = x + y)
# We can't access x and y from the outside of `let()`

velocity_let <- function (x1, x2, t1, t2) {
  dx = x2 - x1,
  dt = t2 - t1,
  .in = dx / dt)
velocity_let(80, 90, 0, 0.5)

velocity_where <- (function (x1, x2, t1, t2) {
  dx / dt
}) %>% where(
  dx = x2 - x1,
  dt = t2 - t1)
velocity_where(80, 90, 0, 0.5)
```

The all variables binded by `let()` and `where()` are locked, so we can't overwrite them.
