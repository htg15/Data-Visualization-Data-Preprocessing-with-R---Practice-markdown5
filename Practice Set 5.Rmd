---
output:
  pdf_document: default
  html_document: default
---
```{r}
library("readr")
library("tidyr")
library("tidyverse")
library("dplyr")
library("ggplot2")
```


```{r}
#Source of file:- 
#kaggle.com/sidtwr/videogames-sales-dataset?select=Video_Games_Sales_as_at_22_Dec_2016.csv


#Variable Definition: 

#video_games_sales_data stores the dataset as it is.
dataset<-read_csv("D:/Documents/IPL Matches 2008-2020.csv")
dataset

data_sorted<- dataset %>%
    group_by(winner) %>%
     summarise(Count = n()) %>%
      arrange(desc(Count))
head(data_sorted,5)
data_sorted<-head(data_sorted,5)
```

```{r}
ggplot(data = data_sorted,aes(y,x=Count)) +
  geom_bar(stat='identity',aes(y=winner,fill=winner)) +labs(x= "No.of Wins",y="Teams",
title="Top 5 Teams from 2008-2020")
```
```{r}
dataset$date <- as.POSIXct(dataset$date,format = "%m/%d/%Y")
dataset$date <- format(dataset$date, format="%Y")
dataset

```
```{r}
data_sorted<- dataset %>%
    group_by(date,winner)%>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
data_sorted
```

```{r}
ggplot(data = data_sorted,aes(y,x=Count)) +
  geom_bar(stat='identity',aes(y=date,fill=winner)) +labs(x="Teams",y="Year",
title="No.of Matches won by each team over the years") + facet_wrap(~winner) 
```
### PART -B: Problems 3–5 use the PimaIndiansDiabetes2 dataset from the mlbench package. You do not need to
partition the dataset for any of the problems in Part A. 

#### Problem 3: We would like to know if there is difference in blood pressure between people with diabetes and people
without diabetes. First remove missing values from the data using ‘na.omit()‘. Then fit a model for blood pressure using diabetes as the only explanatory variable. Perform model diagnostics to check for any violations of model
assumptions. Visualize the relationship between blood pressure and diabetes. State the null and alternative hypotheses, choose an alpha value, and state the p-value and your conclusions.


```{r}
library(mlbench)
library(modelr)
data(PimaIndiansDiabetes2)
model_data <- as_tibble(na.omit(PimaIndiansDiabetes2))
model_data
```


```{r}
model_1 <- lm(pressure ~ diabetes, data=model_data)
model_data %>%
add_residuals(model_1) %>%
ggplot(aes(x=diabetes, y=resid)) +
geom_jitter() +
geom_boxplot() +
labs(y="residuals",
title="Residuals of Diabetes") +
theme_minimal()
```

```{r}
model_data %>%
add_residuals(model_1) %>%
ggplot(aes(sample=resid)) +
geom_qq() +
labs(title="Residuals of Normal Quantiles") +
theme_minimal()

summary(model_1)
```
##### No violation of assumptions were observed.

```{r}
model_data %>%
mutate(Diabetes=recode(diabetes,
pos="Positive", neg="Negative")) %>%
ggplot(aes(x=Diabetes, y=pressure, fill=Diabetes)) +
geom_boxplot() +
labs(y="Blood pressure (mm Hg)",
title="Relation b/w Diabetes & higher blood pressure") +
theme_minimal()
```
##### Based on Null Hypothesis there is no observed difference in blood pressure between people with and without diabetes. Based on Alternate Hypothesis there is observed difference between blood pressure in people with and without diabetes. p=0.000124 (significance cutoff of 0.05), we don't want to consider null hypothesis. Higher blood pressure can be observed for people with diabetes.


#### Problem 4: We would like to consider ‘glucose‘, ‘insulin‘, ‘triceps‘, ‘mass‘, and ‘age‘ as possible covariates. Plot them each against ‘pressure‘ for consideration in the model as explanatory variables. Which variables would
you consider including? Use AIC to select the best model that also includes ‘diabetes‘ as a factor. Show your steps and reasoning, and then state the final model. Hint: AIC can be calculated using ‘AIC()‘ or ‘extractAIC()‘; note that these two functions use different additive constants when calculating the likelihood, and so give differing values for AIC. However, they should lead to the same conclusions. You may find the ‘step()‘ function useful as well.


```{r}
ggplot(model_data, aes(x=glucose, y=pressure)) +
geom_point() +
geom_smooth() +
labs(x="Glucose", y="Blood pressue",
title="No relationship in Glucose vs Blood pressure") +
theme_minimal()

```

```{r}
ggplot(model_data, aes(x=insulin, y=pressure)) +
geom_point() +
geom_smooth() +
labs(x="Insulin", y="Blood pressue",
title="No relationship in Insulin vs Blood Pressure") +
theme_minimal()

```
```{r}
ggplot(model_data, aes(x=triceps, y=pressure)) +
geom_point() +
geom_smooth() +
labs(x="Triceps skin fold thickness", y="Blood pressue",
title="Weak positive relationship in Triceps skin fold thickness vs Blood Pressure") +
theme_minimal()

```
```{r}
ggplot(model_data, aes(x=mass, y=pressure)) +
geom_jitter() +
geom_smooth() +
labs(x="Body mass index (BMI)", y="Blood pressue",
title="Positive relationship in BMI vs Blood Pressure") +
theme_minimal()

```
```{r}
ggplot(model_data, aes(x=age, y=pressure)) +
geom_jitter() +
geom_smooth() +
labs(x="Age", y="Blood pressue",
title="Positive relationship in Age vs Blood Pressure") +
theme_minimal()

```
##### Graph signifies that triceps have weak positive relationship, while age, bmi has positive relationship. We can consider Triceps as well if needed.

```{r}
model_2 <- lm(pressure ~ diabetes + age + mass + triceps, data=model_data)
step(model_2)

```

```{r}
model_3 <- lm(pressure ~ diabetes + age + mass, data=model_data)
AIC(model_1)
AIC(model_2)
AIC(model_3)
model_3

```
##### We will only consider age, mass as factors as adding AIC is not providing progressive results.


#### Problem 5: Use your model from Problem 4 to test the same hypotheses as Problem 3. State the null and alternative hypotheses, choose an alpha value, and state the p-value and your conclusions.Are your results the same or different? How do you explain this?


```{r}
summary(model_3)

```
##### Based on Null hypothesis there is no observed difference in blood pressure (for both people with diabetes and without diabetes). However difference can be observed in blodd pressure between people with diabetes and without diabetes if based on alternative hypothesis. As the Significance cutoff of is 0.05, we will consider null hypothesis. Conclusion can be made that there is no relation between people with diabetes and without diabetes. Result varies from earlier as we considered BMI and age. The relationship doesnt exist if the age and BMI are same.
