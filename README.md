During the cource of Machine Learning - part I I was asked to predict the time needed to solve tickets of the service center of the "Zucchetti" firm. The goal is to understand when there are rush hours in order to improve 
the management of service centers is fundamental for a business company such as Zucchetti. 

I had to find appropriate Machine Learning procedure to predict the net amount of time in minutes needed by a service center to fulfill a given request, using the information 
on the entity, contract and ticket associated with such a request given by the data. The overall dataset contains 26.687 rows, with 12 features and 1 target variable
Dataset split: 18.000 rows for the training set and 8.687 rows for the test set.

After I started visualizing each variable, plotting it through histograms. For the y
variable, I immediately noticed that its distribution is highly skewed with long tails
(high kurtosis). Therefore, I decided to apply to it the **Yeo-Johnson transformation**.
As the majority of the variables are dummies coded as 0-1 I believed that dealing
with linearity through polynomial bases would not be worth it. Moreover, the
complexity of the model and the large span of the values of y indicated the necessity of using **regression trees**. 
I fitted several trees with different subset of variables.  However, their cross-validation error was high, and the test error was even worse
probably due to the high variance of such big trees. Therefore, I tried several **random forests**, without considering bagging as random forests do the same
averaging predictions but also decorrelate trees. Random forests gave a strong improvement and also provided important
information on the importance (through measures of importance) of the single
variables. Finally, I tried **boosting**, which provided the best results. Boosting learns sequentially
and in this model with around 100 parameters, a high span of y and some
strange outcome observation is very important. 

The code is in R becuase the Professor asked so. 

