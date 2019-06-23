# Similarities / Metric functions
Functions in this module are being used to:

    1. Compute similarity between vectors.
    2. All vectors contain positive number only.

Given **$n$** dimensional vectors **$X$** and **$Y$** , this module contains functions of following definitions:

> **Sum Absolute Differece** of  $X$ and $Y$.
> **Manhattan Distance**     of  $X$ and $Y$.
> **Taxicab metric** of          $X$ and $Y$.
> **$L_1$ norm** of $X-Y$ . 
> $$ \sum_i |(x_i-y_i)|$$

> **Mean Absolute Difference(MAD)** of $X$ and $Y$
> is above definition averaged by the vector dimension **n**.
> $$ \frac{\sum_i|(x_i - y_i)|}{n}$$


> **Euclidean Diststance**  of $X$ and $Y$
> **$L_2$ norm** of $X-Y$
> $$\sqrt{\sum_i(x_i-y_i)^2}$$

> **Sum Squred Difference** (SSD) of $X$ and $Y$
> **Squared $L_2$ norm**
> $$ \sum_i(x_i-y_i)^2 = (X-Y) \cdot (X-Y) $$

> **Mean Squared Error (MSE)** of $X$ and $Y$
> is above definition averaged by vector dimension **n**.
>  $$ \frac{\sum_i(x_i-y_i)^2}{n} = \frac{(X -Y) \cdot (X-Y)}{n}$$



=== part two ==
> Chebyshev Distance

> Minkowski Distance

> Canberra Distance

> Hamming Distance

=== part one ==
understanding all these distance, such as
L1 norm vs L2 norm

=== part three == 

- Standard deviation 
    - population standard deviation 
    - sample standard deviation

- cosine similarity:
- cosine distance:
- angular similarity:
- angular distance:

- z-score

- pearsoncorrelation 
    - z-score == centered
    - then cosin similarity

== intuitive understanding ===
why centered
why normalization


