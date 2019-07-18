#  Metric functions / Similarities
Functions in this module are being used to:

    1. Compute similarity between vectors.
    2. All vectors contain positive number only.

Given **$n$** dimensional vectors **$X$** and **$Y$** , this module contains functions of following definitions:

### 1. **$L_p$-norm** based 
> - **$L_1$- norm** based
> $$ \sum_i |(x_i-y_i)|$$
> - **Sum Absolute Differece(SAD)** of  $X$ and $Y$.  
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

>> .When $p$ approaches `infinity`, we get:
>> - **Chebyshev Distance**
>> - **Chess board distance**
>> $$d_\infty:(x,y) \mapsto ||x_i - y_i||_\infty = \lim_{p \rightarrow \infty}(\sum_{i=1}^{n}|x_i-y_i|^p)^{\frac{1}{p}} = \max_i|x_i-y_i|$$
>> Distance between two vectors is the greatest of their differences along any coordinate dimension.

>> . When $p$ is `0`, we get:
>> - **Hanmming Distance**   
>> Hamming Distance is minkowskiDistance with p=0.
>> $$d_0:(x,y)\mapsto||x_i-y_i||_0=\sum_{i=1}^{n}|x_i-y_i|^0$$
>> Between two equal length of same type vectors(instance of Eq), it is the **number** of positions at which corresponding elements are **different**.


> - **Intuitive Understanding** of **$L_1$-norm** and **L_2-norm**   
>
> 
>> - Being **Loss/Risk function**   
>> As described in [wikipedia](https://en.wikipedia.org/wiki/Loss_function), risk function is the expect value of corresponding loss function.
>> $$R(\theta,\delta) = E_\theta L(\theta,\delta(X))$$
>> In `regression problem`, most lost functions heavily rely on **$L_1$-norm** or  **$L_2$-norm**.
>>
>>
>>> 1. Uniqueness  
>>> Assume the truth of $n$ time observation is vector: $Y = (y_1,y_2,...,y_n)$
>>> The output of our hypothesis is: $H=(h_1,h_2,...,h_n)$.  
>>> **$L_1$-norm** basd loss function relay on fumular : 
>>> $$\sum_i|y_i-h_i| = (|y_1-h_1| +|y_2-h_2|+...+ |y_i-h_i|)$$
>>> The sum of two functions $f_1(h)= |y_1-h|$ and $f_2(h)= |y_2-h|$ will be a constant number in range $h\in[y_1,y_2]$ and the proof is trivial. The absolute value of the derivative of these two function are constant number '1' and in range $h\in[y_1,y_2]$ their derivative has different direction so, the sum of these two function is guaranteed to be a stable number and this number is the minimal value of the sum of these two functions $f_1(h)+f_2(h)$. 
>>>
>>> The blue line is : $f(h) = |2-x|$, the yellow is $f(h) = |8-x|$, the red is the sum.
>>>![l1_uniquess_no](/imgs/l1_loss_unique.png)
>>> According to [Central_tendency#uniqueness](https://en.wikipedia.org/wiki/Central_tendency#Uniqueness).    
>>> - Several measures of central tendency can be characterized as solving a variational problem, in the sense of the [calculus of variations](https://en.wikipedia.org/wiki/Calculus_of_variations), namely minimizing variation from the center. That is, given a measure of statistical dispersion, one asks for a measure of central tendency that minimizes variation: such that variation from the center is minimal among all choices of center. In a quip, "dispersion precedes location". This center may or may not be unique. In the sense of Lp spaces, the correspondence is:
>>>
>>>  |Lp|	dispersion|	central| tendency|
>>>  |:--:|:--:|:--:|:--:|
>>>  |L0	|variation |ratio	            |mode
>>>  |L1	|average   |absolute deviation|	median
>>>  |L1	|average   |absolute deviation|	geometric median
>>>  |L2	|standard  |deviation	        |mean
>>>  |L∞	|maximum   |deviation	        |midrange
>>>
>>> In a real-world machine learning scenario, the situation is usually that we will get different results with same input (`This is why we need machine learning`), therefore, as demostrated in the image above, when there are even number of elements included in the $L_1$-norm based loss function, the result will be ambiguous.
>>
>>> The first derivate of **$L_2$-norm** based loss function is $x$, it is possible to find, but there is only one minimual value of the sum of these functions.
>>>![l2_uniquess_yes](/imgs/l2_loss_unique.png)
>>> 
>>
>>> 2. `Robust`   
>>> The robust property referse to tolerant variance of lablel `'Y'`.    
>>> Usually, $y$ outlier can have less influence on $L_1$ loss function than $L_2$ loss functions. 
>>> 3. `Stability`    
>>> The Stability property referse to tolerant variance of input `'X'`.    
>>> $x$ outliers could have great influence on both types of loss function and lead to different optimization hypothesis gramatically. More detailed analysis and intuitive understanding will be updated. An existing illustrationis this [reference document](http://www.chioka.in/differences-between-l1-and-l2-as-loss-function-and-regularization/).
>>
>>
>> - Being **Regularization term**
>>> 0. lasso effect
>>> 1. computational efficiency 
>>> 3. Feature selection
>




> - **cosine similarity**  
> Compute the cosine value of the angle between two vectors.
> $$s_{cos}:(x,y) \mapsto \frac{<x,y>}{||x||_2||y||_2} =\frac{\sum_{i=1}^{n}x_iy_i}{\sqrt{\sum_{i=1}^{n}x_i^2}\sqrt{\sum_{i=1}^{n}y_i^2}} $$
> $$s_{cos}(x,y) =\frac{ x \cdot y}{norm_2(x) * norm_2(y)} = \frac{x \cdot y}{||x||_2 * ||y||_2} $$
> ![cosin similarity](/imgs/cosineSimi.png)
> 1. $$ (x - \rho y) \cdot y = 0$$
> 2. $$\cos \theta = \frac{\rho * norm_2(y)}{norm_2(x)} = \frac{\rho * ||y||_2}{||x||_2}$$  
> from equation 1 we have:      
> $$$$
> 3. $$x \cdot y = \rho ||y||_2^2$$  
> from equation 2 we have:   
> $$$$
> 4. $$\rho = \frac{\cos \theta * ||x||_2}{||y||_2}$$  
> replace $\rho$ in euqation 3 with the definition of $\rho$ in equation 4, we have:  
> $$$$
> 5. $$ x \cdot y = \cos \theta * ||x||_2 * ||y||_2$$  
> therefore:
> $$s_{cos}(x,y) = \cos \theta = \frac{x \cdot y}{||x||_2 * ||y||_2} = \frac{ x \cdot y}{norm_2(x) * norm_2(y)}$$ 
>
>> - **cosine distance** = 1 - cosine similarity  
>> $d_{cos} = 1 - s_{cos}$  
>> Cosine Distance is not metric function,  as it does not have the triangle inequality property, see angular similarity and angular distance (valid metric function) below.
>
>> - **angular distance** 
>> angular distance represent the radians between two vector, it satisfies the triangle inequality , in a recommendation system, it is assumed that `all vectors are  positive`.
>>  $$d_{ang}(x,y) = \frac{2 * \cos^{-1}s_{cos}(x,y)}{\pi}$$
>
>> - **angular similarity** 
>> $s_{ang} = 1 - d_{ang}$

> - **centered**    
> `get ride of bias`    
> Vectors in the vector space could have many different types of relations.
> $$v + u = h$$
> ![vector_sum](/imgs/vector-sum.png)
> Another very important relation is the linear relation
> $$ Y = aX+b$$
> The strucutre contained in matrix $X$ (or even the hyperplane) will be preserved in $Y$. $X$ and $Y$ are isomorphsim.  
> In some cases, the information being carried by $b$ is not very important or uncessary.   
> Especially, when we would like to compare structures of two hyperplane $Y_1$ and $Y_2$ which could be transformed from different matrix $X_1$ and $X_2$ respectively.
> `centered operation` could effectively get ride of `bias` 
>  $$ Y - \hat Y$$
>  The math is simple
>  leave 'a' there
> - **Z-score**     
> `transform to same scale`
>>- Standard deviation 
>>    - population standard deviation 
>>    - sample standard deviation
> - **Pearson correlation**
>> - cosin similarity    
>> 0. Given two vectors $V_1$, $V_2$.
>> 1. Centered $V_1$ and $V_2$ respectively to get $Vc_1$ and $Vc_2$.
>> 2. Compute the cosine similairity between $Vc_1$ and $Vc_2$.  
>
>> - z-score transform
>>   expectation of covariance 
>>   notice that the variables are standadized, $X' X = R$, where $R$ is the correlation matrix of independent varialbes `Verify this statment` from this [doc](https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Ridge_Regression.pdf) and check the uniqueness in mentioned in this [video](https://www.youtube.com/watch?v=sO4ZirJh9ds) at 1:46



> - 
> **To DO**  
> 2. why normalization   
> 3. the role of normalization   
>  4. introduce collinearity mentioned in above docs.  
>  5. seperate branch with doc and library branch, modify the .gitignore files.  
>  6. loss function (classification), logistic regress, softmax regression, etc...  
>  7. metric (regression and classification) : softmax, log-loss, cross-entropy, etc...  
>  8. Metrics which measure two distributions.  
>  9. amming Distance  
>  10. Jensen–Shannon divergance  
>  11. Kullback-Leibler Divergence  
>  12. how to intuitively understand the value of pearson correlation, ex: what is 0.75 looks like?     
>  13. degree of freedom  
>  14. bit of information  
>  15. number of observation   


