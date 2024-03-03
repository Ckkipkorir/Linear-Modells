#install "daewr"
library(daewr)
PBDes(nruns=24,nfactors=23,randomize=TRUE)

#GLM
#Two way table
# create the two-way table data
heart_data <- matrix(c(60, 240, 40, 160), nrow = 2, byrow = TRUE)
colnames(heart_data) <- c("Yes", "No")
rownames(heart_data) <- c("Male", "Female")

# convert the table data to a data frame
heart_df <- as.data.frame.table(heart_data)

# rename the columns
names(heart_df) <- c("Gender", "Heart", "Count")

# fit a logistic regression model
logit_model <- glm(Heart ~ Gender, data = heart_df, family = binomial(link="logit"))

# print the model summary
summary(logit_model)

#log-linear model
data <- data.frame(
  High_School = c(200, 150, 350),
  College = c(150, 200, 350),
  Graduate_School = c(50, 100, 150),
  Total = c(400, 450, 850),
  Gender = c("Male", "Female", "Total")
)
#Data to contogency table
xtab <- xtabs(Total ~ Gender + High_School + College + Graduate_School, data=data)
library(MASS)
model <- loglm(Total ~ Gender + High_School + College + Graduate_School, data=xtab)
summary(model)
model$pearson

#Alternatively
# load mtcars dataset
data(mtcars)
attach(mtcars)
# create a 2x2 contingency table of cyl and vs
table(mtcars$cyl, mtcars$vs)

# fit a log-linear model
model <- glm(Freq ~ Var1 + Var2 + Var1:Var2, data = as.data.frame(table(mtcars$cyl, mtcars$vs)), family = poisson)

# view model summary
summary(model)

# create data frame with the data
lung_cancer <- data.frame(
  smoking_status = factor(rep(c("Never", "Current", "Former"), times = c(500, 350, 150))),
  lung_cancer = c(rep(0, 500), rep(1, 200), rep(0, 100), rep(1, 150), rep(0, 50))
)

# fit logistic regression model
model <- glm(lung_cancer ~ smoking_status, data = lung_cancer, family = binomial())

# view model summary
summary(model)

