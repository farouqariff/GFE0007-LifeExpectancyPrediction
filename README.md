# GFE00007 Assignment: Life Expectancy Prediction

## Introduction
This is the repository for submission for GFE00007 Data Analytics for Beginner project assignment for Semester 1, 2022/2023 session. This project is required to: 
1. come up a hypothesis
2. data collection and do Exploratory Data Analysis (EDA)
3. visualize with graphs and charts
4. create a linear regression model for prediction
5. conclusion

## Hypothesis
When The Income Composition Of Resources Increases, The Life Expectancy Will Also Increase

## Data Collection
The data was collected from WHO and United Nations website with the help of Deeksha Russell and Duan Wang made available to public for the purpose of health data analysis. The data-set related to life expectancy, health factors for 193 countries has been collected from the same WHO data repository website and its corresponding economic data was collected from United Nation website. Among all categories of health-related factors only those critical factors were chosen which are more representative. It has been observed that in the past 15 years , there has been a huge development in health sector resulting in improvement of human mortality rates especially in the developing nations in comparison to the past 30 years. 

## Exploratory Data Analysis

### About Dataset
Figure below shows glimpse() function used to display the characteristic of every column in a dataset.

![image](https://user-images.githubusercontent.com/68151938/220604917-83939310-9e06-4bab-9747-c1ed4d88a994.png)

### Descriptive Statistics
Figure below descr() function from summarytools package is used to display descriptive statistics on every column of a dataset.

![image](https://user-images.githubusercontent.com/68151938/220604289-6c65e593-fd66-4e0c-b1ce-9dfcd77c50d4.png)

### Data Cleaning

#### (a) Null values
This messy data problem is handled by replacing null values with the average value for integer column groupby country. If there is still null values, it will replaced with overall average respected to the column.

#### (b) Outliers
![image](https://user-images.githubusercontent.com/68151938/220607402-0834647b-74b3-4f29-941b-eea44d1dd8f0.png)

Figure above shows outliers on the columns using boxplot. These outliers problem is handled by using winsorizing method. Winsorizing is a method of replaces the smallest and largest values with the observations closest to them. This is done to limit the effect of outliers or abnormal extreme values, or outliers, on the calculation. Winsorize() method is used and can be found from DescTools package. Figure below shows boxplot after implementing winsorizing.

![image](https://user-images.githubusercontent.com/68151938/220608669-bb7c9f16-decd-4dc7-80a9-7aa880fc01e9.png)

## Data Visualization
Charts and graphs can be found from Scatterplot folder and Boxplot folder.

## Linear Regression Model
![image](https://user-images.githubusercontent.com/68151938/220609982-10e94ef4-5d82-4597-91eb-915646ced843.png)
![image](https://user-images.githubusercontent.com/68151938/220610079-ce06d9e3-e142-4370-a342-ac34e83bad7c.png)
![image](https://user-images.githubusercontent.com/68151938/220610143-0d96ea4b-2737-4f8a-bf0e-9e5675dc5c62.png)

## Conclusion
The equation from the linear regression model is, y = 43.9822X + 41.0992. Therefore, the conclusion that can be made from this project is, “life expectancy expects to increase by one unit when income composition of resources increases by 43.9822 unit”.
