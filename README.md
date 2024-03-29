# DualNumbers

![GitHub Workflow Status](https://img.shields.io/github/workflow/status/TimeTravelPenguin/DualNumbers/Haskell%20CI?label=Build%20%26%20Test)

A simple toy implementation of the hypercomplex number system called the [Dual numbers](https://en.wikipedia.org/wiki/Dual_number).

Dual numbers are of the form $z = a + b\varepsilon$ where $a,b\in\mathbb{R}$, where $\varepsilon^2=0$.

Whereas for standard real $(\mathbb{R})$ algebra:

$$
\begin{align*}
  (a + b)(x + y) &= ax +ay + bx + by\\
  (a + b)^2 &= a^2 + 2ab + b^2
\end{align*}
$$

And how in complex $(\mathbb{C})$ domain, where $i^2=-1$:

$$
\begin{align*}
  (a + bi)(x + yi) &= ax - by + (ay + bx)i\\
  (a + bi)^2 &= a^2 + 2abi - b^2
\end{align*}
$$

In the Dual $(\mathbb{D})$ number system:

$$
\begin{align*}
  (a + b\varepsilon)(x + y\varepsilon) &= ax + (ay + bx)\varepsilon\\
  (a + b\varepsilon)^2 &= a^2 + 2ab\varepsilon
\end{align*}
$$

One property of interest is [_automatic differentiation_](https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers). For any **analytic** function $f:\mathbb{R}\mapsto\mathbb{R}$, it can be shown that the domain can be extended to include the Dual numbers in such a way that, for $z\in\mathbb{D}$, where $x,y\in\mathbb{R}$, $z=(x + y\varepsilon)$:

$$
  f(z) = f(a) + bf'(a)\varepsilon
$$

This is done by manipulating the function's [Taylor Series](https://en.wikipedia.org/wiki/Taylor_series), making use of the fact that, $\forall n\geq 2,\,\varepsilon^n=0$, which gives the prior result.

Resources:

- F. Messelmi,\
  Analysis of Dual Functions,\
  Annual Review of Chaos Theory, Bifurcations and Dynamical Systems Vol. 4, (2013) 37-54,\
  [DOI:10.13140/2.1.1006.4006](https://www.arctbds.com/volume4/arctbds_submission_28.pdf)

- Behr, Nicolas, Giuseppe Dattoli, Ambra Lattanzi, and Silvia Licciardi,\
  Dual Numbers and Operational Umbral Methods, Axioms 8, no. 3: 77 (2019)\
  [DOI:10.3390/axioms8030077](https://www.mdpi.com/2075-1680/8/3/77)
