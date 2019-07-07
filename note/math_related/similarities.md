#  Metric functions / Similarities
Functions in this module are being used to:

    1. Compute similarity between vectors.
    2. All vectors contain positive number only.

Given **$n$** dimensional vectors **$X$** and **$Y$** , this module contains functions of following definitions:

### 1. **norm** based 
> - **$L_1$- norm** based
> $$ \sum_i |(x_i-y_i)|$$
> - **Sum Absolute Differece** of  $X$ and $Y$.  
> - **Manhattan Distance**     of  $X$ and $Y$.  
> - **Taxicab metric** of          $X$ and $Y$.  
> - **$L_1$-norm** of $X-Y$ . 
>> - **Mean Absolute Difference(MAD)** of $X$ and $Y$  
>> is above definition averaged by the vector dimension **n**.  
>> $$ \frac{\sum_i|(x_i - y_i)|}{n}$$
>
>> -  
>> $$d_{CAD}:(x,y) \mapsto \sum_{i=1}^{n} \frac{|x_i-y_i|}{|x_i|+|y_i|}$$
>> - **Canberra Distance**
>> The Canberra distance is a weighted version of the Manhattan distance (**l1-norm**).
>> Functional geometry related usage:
>> https://pdfs.semanticscholar.org/a298/02f53f3bcf348e2c9a95df748277ae7571c5.pdf
>> http://shashi.biz/ijulia-notebooks/funcgeo/




> - **$L_2$-norm** based
> $$\sqrt{\sum_i(x_i-y_i)^2}$$
> - **Euclidean Diststance**  of $X$ and $Y$  
> - **$L_2$-norm** of $X-Y$  
>> - **Sum Squred Difference (SSD)** of $X$ and $Y$
>> - **Squared $L_2$- norm**
>> $$ \sum_i(x_i-y_i)^2 = (X-Y) \cdot (X-Y) $$
>
>> - **Mean Squared Error (MSE)** of $X$ and $Y$  
>> is above definition averaged by vector dimension **n**.  
>>  $$ \frac{\sum_i(x_i-y_i)^2}{n} = \frac{(X -Y) \cdot (X-Y)}{n}$$


>- **$L_{\_p}$-norm** based
> $$d_p:(x,y) \mapsto ||x_i - y_i||_p = (\sum_{i=1}^{n}|x_i-y_i|^p)^{\frac{1}{p}}$$
> **Minkowski Distance**
> The Minkowski distance is the generalized $L_p$-norm of the difference.
>
>> - **Chebyshev Distance**
>> - **Chess board distance**
>> $$d_\infty:(x,y) \mapsto ||x_i - y_i||_\infty = \lim_{p \rightarrow \infty}(\sum_{i=1}^{n}|x_i-y_i|^p)^{\frac{1}{p}} = \max_i|x_i-y_i|$$
>> Distance between two vectors is the greatest of their differences along any coordinate dimension.
>
>> - **Hanmming Distance**
>> Hamming Distance is minkowskiDistance with p=0.
>> $$d_0:(x,y)\mapsto||x_i-y_i||_0=\sum_{i=1}^{n}|x_i-y_i|^0$$
>> Between two equal length of same type vectors(instance of Eq), it is the **number** of positions at which corresponding elements are **different**.


> - **Intuitive Understanding** of **$L_1$-norm** and **L_2-norm**
> 
>> - as **Loss/Risk function**
>>> 1. Uniqueness
>>![l2_uniquess_yes](/imgs/l2_loss_unique.png)
>>![l1_uniquess_no](/imgs/l1_loss_unique.png)
>>
>>>
>>> 2. Robust
>>> The robust property referse to tolerant variance of 'X', how every this 'X' in most machine learning case is the target, this cannot tolerate the out of domain of the input of a machine learning model. 
>>> 3. Stability
>>> TODO
>
>> - as **Regularization term**
>>> 1. Uniqueness
>>> 2. Robust
>>> 3. Stability
>




> - **cosine similarity**
> $$s_{cos}:(x,y) \mapsto \frac{<x,y>}{||x||_2||y||_2} =\frac{\sum_{i=1}^{n}x_iy_i}{\sqrt{\sum_{i=1}^{n}x_i^2}\sqrt{\sum_{i=1}^{n}y_i^2}} $$
> $$s_{cos}(x,y) =\frac{ x \cdot y}{norm_2(x) * norm_2(y)} = \frac{x \cdot y}{||x||_2 * ||y||_2} $$
> ![cosin similarity](/imgs/cosineSimi.png)
> 1. $$ (x - \rho y) \cdot y = 0$$
> 2. $$\cos \theta = \frac{\rho * norm_2(y)}{norm_2(x)} = \frac{\rho * ||y||_2}{||x||_2}$$
> from equation 1 we have: 
> 3. $$ x \cdot y = \rho ||y||_2^2$$
> from equation 2 we have: 
> 4. $$ \rho = \frac{\cos \theta * ||x||_2}{||y||_2}$$
> replace $\rho$ in euqation 3 with the definition of $\rho$ in equation 4, we have:
> 5. $$ x \cdot y = \cos \theta * ||x||_2 * ||y||_2$$
> therefore:
> $$s_{cos}(x,y) = \cos \theta = \frac{x \cdot y}{||x||_2 * ||y||_2} = \frac{ x \cdot y}{norm_2(x) * norm_2(y)}$$ 
>
>> - **cosine distance** = 1 - cosine similarity
>> $ d_{cos} = 1 - s_{cos}$
>> Cosine Distance is not metric function,  as it does not have the triangle inequality property, see angular similarity and angular distance (valid metric function) below.
>
>> - **angular distance** 
>> angular distance represent the radians between two vector, it satisfies the triangle inequality , in a recommendation system, it is assumed that all vectors are  positive.
>>  $$d_{ang}(x,y) = \frac{2 * \cos^{-1}s_{cos}(x,y)}{\pi}$$
>
>> - **angular similarity** 
>> $s_{ang} = 1 - d_{ang}$


> - **Pearson correlation** , **z score** and **cosine similarity**
> zscore use population standard deviation



- Standard deviation 
    - population standard deviation 
    - sample standard deviation

- pearsoncorrelation 
    - z-score == centered
    - then cosin similarity

== intuitive understanding ===
why centered
why normalization
the role of normalization

> - 
> **To DO**
> Metrics which measure two distributions.
> Hamming Distance
> Jensen–Shannon divergance
> Kullback-Leibler Divergence
> etc...


degree of freedom
bit of information
number of observation 


