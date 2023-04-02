# Packages that need to be installed
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape")
install.packages("DescTools")
install.packages("GGally")
install.packages("summarytools")

# Invoke library packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape)
library(DescTools)
library(GGally)
library(summarytools)


# 1. Data Exploration

# 1.1 Import the dataset
life_exp <- read.csv(file.choose())

# 1.2 View structure
str(life_exp)

# 1.3 View head dataset 
head(life_exp, n=10)

# 1.4 View tail dataset 
tail(life_exp, n=10)

# 1.5 Check if Step-6 is correct
summary(life_exp)

# 1.6 Glimpse the dataset
glimpse(life_exp)

# 1.7 View Description Statistics on dataset
descr(life_exp)


# 2. Data Cleaning (Replace NA)

# 2.1 View total of nulls in every column
colSums(is.na(life_exp))

# 2.2 Copy the dataset 
le_copy <- data.frame(life_exp)

# 2.3 Change int columns to numeric
le_copy[4:ncol(le_copy)] <- lapply(le_copy[4:ncol(le_copy)], as.numeric)

# 2.4 Replace null values with mean but group by Country (This may take a while)
le_cleaned <- le_copy %>% 
  group_by(Country) %>% 
  mutate_at(vars(Life.expectancy, Adult.Mortality, Alcohol, Hepatitis.B, BMI, 
                 Polio, Total.expenditure, Diphtheria, GDP, Population, 
                 thinness..1.19.years, thinness.5.9.years, 
                 Income.composition.of.resources, Schooling), ~replace_na(., mean(., na.rm = TRUE)))
le_cleaned <- data.frame(le_cleaned)

# 2.5 Check whether it is correct

# Before Replace N/A 
le_copy$Alcohol[le_copy$Country == 'Angola' & le_copy$Year == 2015]

# After Replace N/A with mean Group by Country
le_cleaned$Alcohol[le_cleaned$Country == 'Angola' & le_copy$Year == 2015]

# Calculate Angola's Alcohol Mean
Angola_db <- le_copy[le_copy$Country == 'Angola',]
mean(Angola_db$Alcohol, na.rm = TRUE)

# 2.6 View if there is any more null values
colSums(is.na(le_cleaned))

# 2.7 Replace null values with overall mean by columns (This may take a while)
le_cleanedv2 <- le_cleaned %>% 
  mutate_at(vars(Life.expectancy, Adult.Mortality, Alcohol, Hepatitis.B, BMI, 
                 Total.expenditure, GDP, Population, 
                 thinness..1.19.years, thinness.5.9.years, 
                 Income.composition.of.resources, Schooling), ~replace_na(.,mean(., na.rm = TRUE)))
le_cleanedv2 <- data.frame(le_cleanedv2)

# 2.8 View if there is any more null values
colSums(is.na(le_cleanedv2))


# 3. Data Cleaning (Winsorizing)

# 3.1 View all boxplot (This may take a while)
melt_le <- melt(le_cleanedv2)
p <- ggplot(melt_le, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

# 3.2 Copy dataset
le_cleanedv3 <- data.frame(le_cleanedv2)

# 3.3 Winsorize life expectancy
le_cleanedv3$Life.expectancy <- Winsorize(le_cleanedv2$Life.expectancy, 
                                          minval = NULL, maxval = NULL, 
                                          probs = c(0.01, 0.99), na.rm = FALSE, 
                                          type = 7)
boxplot(le_cleanedv2$Life.expectancy, at=1, xlim=c(0, 3), xlab="Life Expectancy")
boxplot(le_cleanedv3$Life.expectancy, at=2, add=TRUE)

# 3.4 Winsorize adult mortality
le_cleanedv3$Adult.Mortality <- Winsorize(le_cleanedv2$Adult.Mortality, 
                                          minval = NULL, maxval = NULL, 
                                          probs = c(0.03, 0.97), na.rm = FALSE, 
                                          type = 7)
boxplot(le_cleanedv2$Adult.Mortality, at=1, xlim=c(0, 3), xlab="Adult Mortality")
boxplot(le_cleanedv3$Adult.Mortality, at=2, add=TRUE)

# 3.5 Winsorize infant deaths
le_cleanedv3$infant.deaths <- Winsorize(le_cleanedv2$infant.deaths, 
                                          minval = NULL, maxval = NULL, 
                                          probs = c(0.11, 0.89), na.rm = FALSE, 
                                          type = 7)
boxplot(le_cleanedv2$infant.deaths, at=1, xlim=c(0, 3), xlab="Infant Deaths")
boxplot(le_cleanedv3$infant.deaths, at=2, add=TRUE)

# 3.6 Winsorize alcohol
le_cleanedv3$Alcohol <- Winsorize(le_cleanedv2$Alcohol, 
                                        minval = NULL, maxval = NULL, 
                                        probs = c(0.001, 0.999), na.rm = FALSE, 
                                        type = 7)
boxplot(le_cleanedv2$Alcohol, at=1, xlim=c(0, 3), xlab="Alcohol")
boxplot(le_cleanedv3$Alcohol, at=2, add=TRUE)

# 3.7 Winsorize percentage expenditure
le_cleanedv3$percentage.expenditure <- Winsorize(le_cleanedv2$percentage.expenditure, 
                                  minval = NULL, maxval = NULL, 
                                  probs = c(0.14, 0.86), na.rm = FALSE, 
                                  type = 7)
boxplot(le_cleanedv2$percentage.expenditure, at=1, xlim=c(0, 3), xlab="Percentage Expenditure")
boxplot(le_cleanedv3$percentage.expenditure, at=2, add=TRUE)

# 3.8 Winsorize Hepatitis B
le_cleanedv3$Hepatitis.B <- Winsorize(le_cleanedv2$Hepatitis.B, 
                                                 minval = NULL, maxval = NULL, 
                                                 probs = c(0.09, 0.91), na.rm = FALSE, 
                                                 type = 7)
boxplot(le_cleanedv2$Hepatitis.B, at=1, xlim=c(0, 3), xlab="Hepatitis B")
boxplot(le_cleanedv3$Hepatitis.B, at=2, add=TRUE)

# 3.9 Winsorize measles 
le_cleanedv3$Measles <- Winsorize(le_cleanedv2$Measles, 
                                      minval = NULL, maxval = NULL, 
                                      probs = c(0.19, 0.81), na.rm = FALSE, 
                                      type = 7)
boxplot(le_cleanedv2$Measles, at=1, xlim=c(0, 3), xlab="Measles")
boxplot(le_cleanedv3$Measles, at=2, add=TRUE)

# 3.10 No winsorize BMI because not outlier
boxplot.stats(le_cleanedv2$BMI)$out

# 3.11 Winsorize under five deaths 
le_cleanedv3$under.five.deaths <- Winsorize(le_cleanedv2$under.five.deaths, 
                              minval = NULL, maxval = NULL, 
                              probs = c(0.14, 0.86), na.rm = FALSE, 
                              type = 7)
boxplot(le_cleanedv2$under.five.deaths, at=1, xlim=c(0, 3), xlab="Under Five Deaths")
boxplot(le_cleanedv3$under.five.deaths, at=2, add=TRUE)

# 3.12 Winsorize polio 
le_cleanedv3$Polio <- Winsorize(le_cleanedv2$Polio, 
                                            minval = NULL, maxval = NULL, 
                                            probs = c(0.1, 0.9), na.rm = FALSE, 
                                            type = 7)
boxplot(le_cleanedv2$Polio, at=1, xlim=c(0, 3), xlab="Polio")
boxplot(le_cleanedv3$Polio, at=2, add=TRUE)


# 3.13 Winsorize total expenditure 
le_cleanedv3$Total.expenditure <- Winsorize(le_cleanedv2$Total.expenditure, 
                                minval = NULL, maxval = NULL, 
                                probs = c(0.02, 0.98), na.rm = FALSE, 
                                type = 7)
boxplot(le_cleanedv2$Total.expenditure, at=1, xlim=c(0, 3), xlab="Total Expenditure")
boxplot(le_cleanedv3$Total.expenditure, at=2, add=TRUE)

# 3.14 Winsorize Diphtheria 
le_cleanedv3$Diphtheria <- Winsorize(le_cleanedv2$Diphtheria, 
                                            minval = NULL, maxval = NULL, 
                                            probs = c(0.11, 0.89), na.rm = FALSE, 
                                            type = 7)
boxplot(le_cleanedv2$Diphtheria, at=1, xlim=c(0, 3), xlab="Diphtheria")
boxplot(le_cleanedv3$Diphtheria, at=2, add=TRUE)

# 3.15 Winsorize HIV/AIDS 
le_cleanedv3$HIV.AIDS <- Winsorize(le_cleanedv2$HIV.AIDS, 
                                     minval = NULL, maxval = NULL, 
                                     probs = c(0.19, 0.81), na.rm = FALSE, 
                                     type = 7)
boxplot(le_cleanedv2$HIV.AIDS, at=1, xlim=c(0, 3), xlab="HIV/AIDS")
boxplot(le_cleanedv3$HIV.AIDS, at=2, add=TRUE)

# 3.16 Winsorize GDP
le_cleanedv3$GDP <- Winsorize(le_cleanedv2$GDP, 
                                   minval = NULL, maxval = NULL, 
                                   probs = c(0.11, 0.89), na.rm = FALSE, 
                                   type = 7)
boxplot(le_cleanedv2$GDP, at=1, xlim=c(0, 3), xlab="GDP")
boxplot(le_cleanedv3$GDP, at=2, add=TRUE)

# 3.17 Winsorize Population
le_cleanedv3$Population <- Winsorize(le_cleanedv2$Population, 
                                     minval = NULL, maxval = NULL, 
                                     probs = c(0.07, 0.93), na.rm = FALSE, 
                                     type = 7)
boxplot(le_cleanedv2$Population, at=1, xlim=c(0, 3), xlab="Population")
boxplot(le_cleanedv3$Population, at=2, add=TRUE)

# 3.18 Winsorize thinness..1.19.years
le_cleanedv3$thinness..1.19.years <- Winsorize(le_cleanedv2$thinness..1.19.years, 
                              minval = NULL, maxval = NULL, 
                              probs = c(0.04, 0.96), na.rm = FALSE, 
                              type = 7)
boxplot(le_cleanedv2$thinness..1.19.years, at=1, xlim=c(0, 3), xlab="thinness..1.19.years")
boxplot(le_cleanedv3$thinness..1.19.years, at=2, add=TRUE)

# 3.19 Winsorize thinness.5.9.years
le_cleanedv3$thinness.5.9.years <- Winsorize(le_cleanedv2$thinness.5.9.years, 
                                     minval = NULL, maxval = NULL, 
                                     probs = c(0.04, 0.96), na.rm = FALSE, 
                                     type = 7)
boxplot(le_cleanedv2$thinness.5.9.years, at=1, xlim=c(0, 3), xlab="thinness.5.9.years")
boxplot(le_cleanedv3$thinness.5.9.years, at=2, add=TRUE)

# 3.20 Winsorize Income.composition.of.resources
le_cleanedv3$Income.composition.of.resources <- Winsorize(le_cleanedv2$Income.composition.of.resources, 
                                             minval = NULL, maxval = NULL, 
                                             probs = c(0.05, 0.95), na.rm = FALSE, 
                                             type = 7)
boxplot(le_cleanedv2$Income.composition.of.resources, at=1, xlim=c(0, 3), xlab="Income Composition of Resources")
boxplot(le_cleanedv3$Income.composition.of.resources, at=2, add=TRUE)

# 3.21 Winsorize Schooling
le_cleanedv3$Schooling <- Winsorize(le_cleanedv2$Schooling, 
                                             minval = NULL, maxval = NULL, 
                                             probs = c(0.03, 0.97), na.rm = FALSE, 
                                             type = 7)
boxplot(le_cleanedv2$Schooling, at=1, xlim=c(0, 3), xlab="Schooling")
boxplot(le_cleanedv3$Schooling, at=2, add=TRUE)

# 3.22 Plot boxplot again but after remove outlier (This may take a while)
melt_le2 <- melt(le_cleanedv3)
q <- ggplot(melt_le2, aes(factor(variable), value)) 
q + geom_boxplot() + facet_wrap(~variable, scale="free")

# 3.23 Double Check if there is outlier
boxplot.stats(le_cleanedv3$Life.expectancy)$out
boxplot.stats(le_cleanedv3$Adult.Mortality)$out
boxplot.stats(le_cleanedv3$infant.deaths)$out
boxplot.stats(le_cleanedv3$Alcohol)$out
boxplot.stats(le_cleanedv3$percentage.expenditure)$out
boxplot.stats(le_cleanedv3$Hepatitis.B)$out
boxplot.stats(le_cleanedv3$Measles)$out
boxplot.stats(le_cleanedv3$BMI)$out
boxplot.stats(le_cleanedv3$under.five.deaths)$out
boxplot.stats(le_cleanedv3$Polio)$out
boxplot.stats(le_cleanedv3$Total.expenditure)$out
boxplot.stats(le_cleanedv3$Diphtheria)$out
boxplot.stats(le_cleanedv3$HIV.AIDS)$out
boxplot.stats(le_cleanedv3$GDP)$out
boxplot.stats(le_cleanedv3$Population)$out
boxplot.stats(le_cleanedv3$thinness..1.19.years)$out
boxplot.stats(le_cleanedv3$thinness.5.9.years)$out
boxplot.stats(le_cleanedv3$Income.composition.of.resources)$out
boxplot.stats(le_cleanedv3$Schooling)$out


# 4. Data Visualization

# 4.1 Year vs Life expectancy
y_le <- data.frame(le_cleanedv3 %>% group_by(Year) %>%
                     summarise(avgle = mean(Life.expectancy)))
ggplot(data = y_le,
       mapping = aes(x=Year,
                     y=avgle)) +
  geom_point() +
  geom_line()

# 4.2 Income.composition.of.resources vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Income.composition.of.resources,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.3 Status vs Life expectancy
s_le <- data.frame(le_cleanedv3 %>% group_by(Status) %>%
                     summarise(avgle = mean(Life.expectancy)))
ggplot(data = s_le,
       mapping = aes(x=Status,
                     y=avgle,
                     fill=Status)) +
  geom_bar(stat = "identity")

# 4.4 Adult mortality vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Adult.Mortality,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.5 Infant deaths vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=infant.deaths,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.6 Alcohol vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Alcohol,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.7 Percentage expenditure vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=percentage.expenditure,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.8 Hepatitis B vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Hepatitis.B,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.9 Measles vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Measles,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.10 BMI vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=BMI,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.11 Under five deaths vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=under.five.deaths,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.12 Polio vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Polio,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.13 Total expenditure vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Total.expenditure,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.14 Diphtheria vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Diphtheria,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.15 HIV/AIDS vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=HIV.AIDS,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.16 GDP vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=GDP,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.17 Population vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Population,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.18 thinness..1.19.years vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=thinness..1.19.years,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.19 thinness.5.9.years vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=thinness.5.9.years,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.20 Income.composition.of.resources vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Income.composition.of.resources,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.21 Schooling vs Life expectancy
ggplot(data = le_cleanedv3,
       mapping = aes(x=Schooling,
                     y=Life.expectancy,
                     color=Status)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

# 4.22 Save all plot diagram to file
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE) 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/User/Pictures")


# 5. Prediction Model

# 5.1 Check the correlation between dependent variable and independent variable
cor.test(le_cleanedv3$Life.expectancy, le_cleanedv3$Income.composition.of.resources)

# 5.2 Subset dataset
final_data <- data.frame(le_cleanedv3[,c('Life.expectancy','Income.composition.of.resources')])

# 5.3 Single Linear Regression
slr <- lm(Life.expectancy ~ Income.composition.of.resources, data=final_data)
summary(slr)

# 5.4 Plot best fit line
plot(Life.expectancy ~ Income.composition.of.resources, data=final_data, col="green")
abline(lm(Life.expectancy ~ Income.composition.of.resources, data=final_data), col="red")

# 5.5 Make prediction
pred_data <- data.frame(le_cleanedv3[sample(nrow(le_cleanedv3), 10), 
                                  c('Income.composition.of.resources',
                                    'Life.expectancy')])
colnames(pred_data) <- c("X","y")
pred_data["y_"]<-data.frame(y_=c(predict(slr,
                                      data.frame(Income.composition.of.resources=pred_data$X))))
pred_data["residuals"] <- abs(pred_data$y - pred_data$y_)
pred_data