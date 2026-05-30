---
title: "Comparing `numpy.einsum` vs `einops.einsum`"
layout: "post"
author: "Vijay Lulla"
---

Numpy's [`np.einsum`](https://numpy.org/doc/2.4/reference/generated/numpy.einsum.html)

Einops [`einops.einsum`](https://einops.rocks/api/einsum/)

```python
>>> import numpy as np, einops as eo
>>> a = np.arange(25).reshape(5,5)
>>> b = np.arange(5)
>>> c = np.arange(6).reshape(2,3)
>>> a1 = np.arange(60.).reshape(3,4,5) ## for contraction examples
>>> b1 = np.arange(24.).reshape(4,3,2)
>>> # help(np.einsum)
>>> # help(eo.einsum)
```

| Operation (from np.einsum's docstring)             |  numpy                                    | einops                                        |
|----------------------------------------------------|-------------------------------------------|-----------------------------------------------|
| Trace of a matrix                                  | `np.einsum('ii->', a)`                    | `eo.einsum(a, 'i i ->')`                      |
| Sum over an axis                                   | `np.einsum('ij->i', a)`                   | `eo.einsum(a, 'i j -> i')`                    |
| Summing a single axis in higher dimensional arrays | `np.einsum('...j->...', a)`               | `eo.einsum(a, '... j -> ...')`                |
| Transpose a matrix                                 | `np.einsum('ij->ji', a)`                  | `eo.einsum(c, 'i j -> j i')`                  |
| Vector inner product                               | `np.einsum('i,i->', b, b)`                | `eo.einsum(b, b, 'i,i ->')`                   |
| Matrix vector multiplication                       | `np.einsum('ij,j->i', a, b)`              | `eo.einsum(a, b, 'i j,j -> i')`               |
| Broadcasting and scalar multiplication             | `np.einsum('..., ...->...', 3, c)`        | `eo.einsum(c, 3, '..., ... -> ...)`           |
| Vector outer product                               | `np.einsum('i,j->ij', np.arange(2)+1, b)` | `eo.einsum(np.arange(2)+1, b, 'i, j -> i j')` |
| Tensor contraction (einops shines here!)           | `np.einsum('ijk,jil->kl', a1, b1)`        | `eo.einsum(a1, b1, 'i j k, j i l -> k l')`    |

### NOTES

- Even though `np.einsum` allows *implicit* mode I prefer using the *explicit* mode (triggered by the presence of `->` in the substring).  This makes it easy to make connection between numpy and einops.  Implicit mode rearranges the axes in output alphabetically which can be tricky to debug if you don't remember this reordering!
- The two big differences betwen `eo.einsum` and `np.einsum` are:
  1. In einops the tensors/arrays come first and the subscript string notation for summation come at the end.  In numpy the subscript string notation comes first and then the tensors/arrays.
  2. Numpy's einsum does not allow spaces in the subscript string notation whereas einops allows it.  I find reading einops's subscript notation a little bit easier.
- Einops has `reduce`, `rearrange`, and `repeat` which can be quite handy for neural network code...especially for the attention transformer architecture related code!
- Explore `optimize` (optional argument) in `np.einsum` to eke out more performance gain.
