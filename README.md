# brightbox

## Installation

```
require(devtools) 
install_github("breather/brightbox")
```

## About
Brightbox is an R package that tackles the problem of determining variable importance in the context of supervised machine learning. Traditional variable importance methods often rely on theoretical assumptions that fail to hold in the real world.

Brightbox is guided by a "learner agnostic" philosophy: 

1. The end user can specify any `caret` method as the supervised learning algorithm. 
2. All measures of importance are based on looking at empirical changes when a variable is removed or changed in some way.

## Documentation
https://github.com/breather/brightbox/blob/vignette/inst/doc/brightbox.pdf

## Sources
Paper describing motivation for `calculate_marginal_vimp`:
https://arxiv.org/pdf/1701.04944v4.pdf



