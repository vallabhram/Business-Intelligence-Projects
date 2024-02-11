#Load dataset
library(haven)
guns <- as.data.frame(read_dta('//Users/tashfiamamun/Spring 2023/Econometrics/Project/guns (2)(1).dta'))

#Summary 
summary(guns)

#Correlation plot
library(corrplot)
cor_matrix <- cor(guns) 
corrplot(cor_matrix, method = "color")
corrplot(cor_matrix, method = "number")


# scatterplot matrix
library(GGally)
map <- ggpairs(data = guns, 
        mapping = aes(), 
        lower = list(continuous = "smooth", combo = "facetdensity"),
        diag = list(continuous = "density", combo = "barDiag"))
map

ggpairs(guns, aes(color = factor(group)))


ggplot(data = guns, aes(x = year, y = avginc)) +
  geom_point() +
  labs(x = "Year", y = "Avginc") +
  theme_minimal()

library(ggplot2)

ggplot(guns, aes(x = factor(year), y = density)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("Density")

b1 = boxplot(guns$vio ~ guns$year, xlab = "year", ylab = "Violent Crime Rate", col="lightblue")

b2 = boxplot(guns$mur ~ guns$year, xlab = "year", ylab = "Murder Rate", col="lightpink")

b3 = boxplot(guns$rob ~guns$year, xlab = "year", ylab = "Robbery Rate", col="lightyellow")

# Creating a general model
model_z <- lm(log(vio+mur + rob) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
              + pop +log(density) +avginc + shall, guns)
summary(model_z)



#Best Subset Selection 

library(leaps)

# Use leaps to perform best subset selection

#Violence
BestSubset1 <- regsubsets(log(vio) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
                          + pop + log(density) + avginc + shall, guns, nvmax = 9)

summary(BestSubset1)
summary(BestSubset1)$bic
which.min(summary(BestSubset1)$bic)

model1 <-  lm(log(vio) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
                      + pop + shall, guns)

summary(model1)

#Murder
BestSubset2 <- regsubsets(log(mur) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
                          + pop + log(density) + avginc + shall, guns, nvmax = 9)

summary(BestSubset2)
which.min(summary(BestSubset2)$bic)
model2 <-  lm(log(mur) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
              + pop +log(density) + shall, guns)

summary(model2)

#Robbery

BestSubset3 <- regsubsets(log(rob) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
                          + pop + log(density) + avginc + shall, guns, nvmax = 9)
summary(BestSubset3)

which.min(summary(BestSubset3)$bic)

model3 <-  lm(log(rob) ~ year + incarc_rate + pb1064 + pw1064 + pm1029 
              + pop +log(density) +avginc + shall, guns)

summary(model3)

library(dplyr)



# Subset the data for shall = 0 and calculate the mean of vio for each state
mean_vio_shall_0 <- aggregate(guns$vio[guns$shall == 0], list(guns$stateid[guns$shall == 0]), mean)

# Subset the data for shall = 1 and calculate the mean of vio for each state
mean_vio_shall_1 <- aggregate(guns$vio[guns$shall == 1], list(guns$stateid[guns$shall == 1]), mean)

# Merge the two data frames on the state column
mean_vio_both <- merge(mean_vio_shall_0, mean_vio_shall_1, by = "Group.1")

# Rename the columns for clarity
colnames(mean_vio_both) <- c("State", "Mean_vio_shall_0", "Mean_vio_shall_1")

# Print the resulting data frame
mean_vio_both

mean_vio_both$difference <- mean_vio_both$Mean_vio_shall_1 - mean_vio_both$Mean_vio_shall_0

t.test(mean_vio_both$Mean_vio_shall_0, mean_vio_both$Mean_vio_shall_1, paired=TRUE)


## States before after shall analysis

library(ggplot2)

# create a list of stateIDs to filter by
state_new_law <-mean_vio_both$State
state_new_law
# create the plot
ggplot(guns[guns$stateid %in% state_new_law,], aes(x = year, y = vio, color = factor(shall))) +
  geom_point() +
  labs(x = "Year", y = "Violence Rate", color = "Shall Issue") +
  scale_color_manual(values = c("red", "blue")) +
  theme_classic()

#add a trend line

ggplot(guns[guns$stateid %in% state_new_law,], aes(x = year, y = vio, color = factor(shall))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # add a linear trend line
  labs(x = "Year", y = "Violence Rate", color = "Shall Issue") +
  scale_color_manual(values = c("red", "blue")) +
  theme_classic()

# create a subset of the data for the desired state IDs
subset_data <- subset(guns, stateid %in% state_new_law)

# calculate average violence rate for each year and for each value of 'shall'
avg_vio <- aggregate(vio ~ year + shall , subset_data, mean)

# plot the data using ggplot2
ggplot(avg_vio, aes(x = year, y = vio, color = factor(shall))) + 
  geom_line() +
  labs(title = "Average Violence Rate by Year and Shall-Issue Status",
       x = "Year", y = "Violence Rate", color = "Shall-Issue Status") +
  theme_bw()

#Average crime per year
library(dplyr)

# Group the data by year
crimes_by_year <- guns %>%
  group_by(year) %>%
  summarise(
    average_vio = mean(vio),
    average_mur = mean(mur),
    average_rob = mean(rob)
  )

# Print the results
print(crimes_by_year)

#crimes by time

library(ggplot2)
ggplot(crimes_by_year, aes(x = year)) + 
  geom_line(aes(y = average_vio, color = "Average Violent Crimes")) +
  geom_line(aes(y = average_mur, color = "Average Murder")) +
  geom_line(aes(y = average_rob, color = "Average Robbery")) +
  xlab("Year") +
  ylab("Average") +
  ggtitle("Average Crime Over Time") +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_minimal()




#FE models
library(plm)
options(scipen = 999)
# create a fixed-effects model
model_entity <- plm(log(vio+mur + rob) ~ incarc_rate + pb1064 + pw1064 + pm1029
                    + pop +log(density) +avginc + shall , effect = "individual",
                    data = guns, index = c("stateid", "year"), model = "within")
summary(model_entity)
fixef(model_entity)

# create a time-effects model
model_fetime <- plm(log(vio+mur + rob) ~ incarc_rate + pb1064 + pw1064 + pm1029
                    + pop +log(density) +avginc + shall, effect = "time",
                    data = guns, index = c("stateid", "year"), model = "within")
summary(model_fetime)
fixef(model_fetime)


# create a Two way entity and time-effects model
model_twoway <- plm(log(vio+mur + rob) ~ incarc_rate + pb1064 + pw1064 + pm1029
                    + pop +log(density) +avginc + shall + factor(year) ,
                    data = guns, index = c("stateid", "year"), model = "within")
summary(model_twoway)
fixef(model_twoway)

