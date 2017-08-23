#### MODELS ####

fooddata <- read.csv('~/Desktop/food-inspections/data/food_inspections_final_updated.csv', as.is=TRUE)
#want to restrict data to canvass inspections
fooddata2 <- fooddata[fooddata$Inspection.Type == "canvass" | fooddata$Inspection.Type == "canvass re-inspection", ]
fooddata2 <- fooddata2[, -1]

#convert Results variable to factor
fooddata2$Results[fooddata2$Results == "Pass"] <- 0
fooddata2$Results[fooddata2$Results == "Fail"] <- 1
fooddata2$Results <- as.numeric(fooddata2$Results)
fooddata2$Results <- as.factor(fooddata2$Results)

names(fooddata2) <- gsub(".", "_", names(fooddata2), fixed = TRUE)

#convert character variables to factors
fooddata2$Facility_Type <- as.factor(fooddata2$Facility_Type)
fooddata2$Risk <- as.factor(fooddata2$Risk)
fooddata2$Inspection_Type <- as.factor(fooddata2$Inspection_Type)

#split food data to training set, 2015 set, and test set
food_train <- fooddata2[fooddata2$inspect_year <= 2013, ]
food_2015 <- fooddata2[fooddata2$inspect_year == 2015, ]
food_test <- fooddata2[fooddata2$inspect_year == 2014, ]

#check which restaurants in training set were inspected in 2015
inspected_id <- which((food_train$License__ %in% food_2015$License__))
food_train$inspected_in_2015 <- 0
food_train$inspected_in_2015[inspected_id] <- 1
food_train$inspected_in_2015 <- as.factor(food_train$inspected_in_2015)

#check which restaurants in test set were inspected in 2015
inspected_id2 <- which((food_test$License__ %in% food_2015$License__))
food_test$inspected_in_2015 <- 0
food_test$inspected_in_2015[inspected_id2] <- 1
food_test$inspected_in_2015 <- as.factor(food_test$inspected_in_2015)

food_test_with_results <- food_test

sum(food_test$Results == 1)
#in 2014 there were 12144 inspections
#hit rate of 2003/12144 = 0.1649

sum(food_2015$Results == 1)/nrow(food_2015)
#in 2015 there were 11005 inspections
#hit rate of 1994/11005 = 0.18119

food_2016 <- fooddata2[fooddata2$inspect_year <= 2016, ]
sum(food_2016$Results == 1)/nrow(food_2016)

#### logistic regression with ridge and lasso ####

library(glmnet)
grid <- 10^seq(10, -2, length=100)

####ridge regression ####

#my model
my.x <- model.matrix(Results~. -License__ -inspected_in_2015, family = binomial, food_train)
my.y <- food_train$Results

my.ridge.model <- glmnet(my.x, my.y, alpha=0, family="binomial", lambda = grid, thresh = 1e-12) 
my.ridge.cv.out <- cv.glmnet(my.x, my.y, alpha=0, family="binomial", lambda = grid, thresh = 1e-12) 
my.ridge.bestlam <- my.ridge.cv.out$lambda.min
my.ridge.bestlam

my.test.x <- model.matrix(Results~. -License__ -inspected_in_2015, family = binomial, food_test)
my.ridge.pred <- predict(my.ridge.model, s = my.ridge.bestlam, newx = my.test.x, type="response")
my.ridge.pred <- as.data.frame(my.ridge.pred)
colnames(my.ridge.pred) <- c("my_ridge_1_prob")
food_test_with_results <- cbind(food_test_with_results, my.ridge.pred)

my.ridge.model.coeff <- predict(my.ridge.model, type="coefficients", s=my.ridge.bestlam)
my.ridge.model.coeff <- as.data.frame(as.matrix(my.ridge.model.coeff))
my.ridge.model.coeff$vari <- rownames(my.ridge.model.coeff)
nrow(my.ridge.model.coeff)
colnames(my.ridge.model.coeff) <- c("Coefficients", "Variable")
my.ridge.model.coeff <- my.ridge.model.coeff[order(abs(my.ridge.model.coeff$Coefficients), decreasing=TRUE), ] 
my.ridge.model.coeff <- my.ridge.model.coeff[1:10, ]
my.ridge.model.coeff <- my.ridge.model.coeff[, c(2,1)]
my.ridge.model.coeff$Variable <- c("(Intercept)", "PUBLIC_INDECENCY_crimerate", "NON_CRIMINAL_crimerate", "Facility_Type: church", "Facility_Type: movie theater", "Facility_Type: cafeteria", "Facility_Type: navy pier kiosk", "Inspection_Type: canvass re-inspection", "Facility_Type: other", "Facility_Type: rooftop")

library(stargazer)
stargazer(my.ridge.model.coeff, summary = FALSE, title = "10 Largest Coefficients in My Ridge Regression", rownames = FALSE)

#city's model
city.x <- model.matrix(inspected_in_2015~. -License__ -Results, food_train)
city.y <- food_train$inspected_in_2015

city.ridge.model <- glmnet(city.x, city.y, alpha=0, family="binomial", lambda = grid, thresh = 1e-12) 
coef(city.ridge.model)
city.ridge.cv.out <- cv.glmnet(city.x, city.y, alpha=0, family="binomial", lambda = grid, thresh = 1e-12) 
city.ridge.bestlam <- city.ridge.cv.out$lambda.min

city.test.x <- model.matrix(inspected_in_2015~. -License__ -Results, family = binomial, food_test)
city.ridge.pred <- predict(city.ridge.model, s = city.ridge.bestlam, newx = city.test.x, type="response")
city.ridge.pred <- as.data.frame(city.ridge.pred)
colnames(city.ridge.pred) <- c("city_ridge_1_prob")
food_test_with_results <- cbind(food_test_with_results, city.ridge.pred)

city.ridge.model.coeff <- predict(city.ridge.model, type="coefficients", s=city.ridge.bestlam)
city.ridge.model.coeff <- as.data.frame(as.matrix(city.ridge.model.coeff))
city.ridge.model.coeff$vari <- rownames(city.ridge.model.coeff)
nrow(city.ridge.model.coeff)
colnames(city.ridge.model.coeff) <- c("Coefficients", "Variable")
city.ridge.model.coeff <- city.ridge.model.coeff[order(abs(city.ridge.model.coeff$Coefficients), decreasing=TRUE), ] 
city.ridge.model.coeff <- city.ridge.model.coeff[1:10, ]
city.ridge.model.coeff <- city.ridge.model.coeff[, c(2,1)]
city.ridge.model.coeff$Variable <- c("(Intercept)", "Facility_Type: kiosk", "Facility_Type: church", "Facility_Type: children's services facility", "Facility_Type: daycare", "Facility_Type: butcher", "Facility_Type: college", "Facility_Type: nursing home", "NON_CRIMINAL_crimerate", "Risk: Risk 3 (Low)" )
stargazer(city.ridge.model.coeff, summary = FALSE, title = "10 Largest Coefficients in City Ridge Regression", rownames = FALSE)


####lasso ####

#my model
my.lasso.model <- glmnet(my.x, my.y, alpha=1, family="binomial", lambda = grid, thresh = 1e-12) 
coef(my.lasso.model)
my.lasso.cv.out <- cv.glmnet(my.x, my.y, alpha=1, family="binomial", lambda = grid, thresh = 1e-12) 
my.lasso.bestlam <- my.lasso.cv.out$lambda.min

my.test.x <- model.matrix(Results~. -License__ -inspected_in_2015, family = binomial, food_test)
my.lasso.pred <- predict(my.lasso.model, s = my.lasso.bestlam, newx = my.test.x, type="response")
my.lasso.pred <- as.data.frame(my.lasso.pred)
colnames(my.lasso.pred) <- c("my_lasso_1_prob")
food_test_with_results <- cbind(food_test_with_results, my.lasso.pred)

my.lasso.model.coeff <- predict(my.lasso.model, type="coefficients", s=my.lasso.bestlam)
my.lasso.model.coeff <- as.data.frame(as.matrix(my.lasso.model.coeff))
my.lasso.model.coeff$vari <- rownames(my.lasso.model.coeff)
nrow(my.lasso.model.coeff)
colnames(my.lasso.model.coeff) <- c("Coefficients", "Variable")
nrow(my.lasso.model.coeff) - length(which(my.lasso.model.coeff$Coefficients != 0)) - 1
my.lasso.model.coeff <- my.lasso.model.coeff[order(abs(my.lasso.model.coeff$Coefficients), decreasing=TRUE), ] 
my.lasso.model.coeff <- my.lasso.model.coeff[1:10, ]
my.lasso.model.coeff <- my.lasso.model.coeff[, c(2,1)]
my.lasso.model.coeff$Variable <- c("(Intercept)", "Inspection_Type: canvass re-inspection", "Facility_Type: other", "PUBLIC_INDECENCY_crimerate", "Facility_Type: bar", "Risk: Risk 3 (Low)", "Facility_Type: grocery", "past_failed_inspect", "INTERFERENCE_WITH_PUBLIC_OFFICER_crimerate", "BURGLARY_crimerate")
stargazer(my.lasso.model.coeff, summary = FALSE, title = "10 Largest Coefficients in my lasso Regression", rownames = FALSE)


#city's model
city.lasso.model <- glmnet(city.x, city.y, alpha=1, family="binomial", lambda = grid, thresh = 1e-12) 
coef(city.lasso.model)
city.lasso.cv.out <- cv.glmnet(city.x, city.y, alpha=1, family="binomial", lambda = grid, thresh = 1e-12) 
city.lasso.bestlam <- city.lasso.cv.out$lambda.min

city.test.x <- model.matrix(inspected_in_2015~. -License__ -Results, family = binomial, food_test)
city.lasso.pred <- predict(city.lasso.model, s = city.lasso.bestlam, newx = city.test.x, type="response")
city.lasso.pred <- as.data.frame(city.lasso.pred)
colnames(city.lasso.pred) <- c("city_lasso_1_prob")
food_test_with_results <- cbind(food_test_with_results, city.lasso.pred)

city.lasso.model.coeff <- predict(city.lasso.model, type="coefficients", s=city.lasso.bestlam)
city.lasso.model.coeff <- as.data.frame(as.matrix(city.lasso.model.coeff))
city.lasso.model.coeff$vari <- rownames(city.lasso.model.coeff)
nrow(city.lasso.model.coeff)
colnames(city.lasso.model.coeff) <- c("Coefficients", "Variable")
nrow(city.lasso.model.coeff) - length(which(city.lasso.model.coeff$Coefficients != 0)) - 1
city.lasso.model.coeff <- city.lasso.model.coeff[order(abs(city.lasso.model.coeff$Coefficients), decreasing=TRUE), ] 
city.lasso.model.coeff <- city.lasso.model.coeff[1:10, ]
city.lasso.model.coeff <- city.lasso.model.coeff[, c(2,1)]
city.lasso.model.coeff$Variable <- c("(Intercept)", "Facility_Type: daycare", "Facility_Type: children's services facility", "Risk: Risk 3 (Low)", "Facility_Type: kiosk", "Facility_Type: nursing home", "Facility_Type: school", "Facility_Type: golden diner", "Risk: Risk 2 (Medium)", "Facility_Type: church")
stargazer(city.lasso.model.coeff, summary = FALSE, title = "10 Largest Coefficients in city lasso Regression", rownames = FALSE)

# c <- coef(city.cv.out,s='lambda.min',exact=TRUE)
# inds <- which(c!=0)
# variables <- row.names(c)[inds]
# `%ni%` <- Negate('%in%')
# variables <- variables[variables %ni% '(Intercept)']

####evaluating model performance####

food_test_results_short <- food_test_with_results[, -c(2:5, 8:96)]

my_ridge <- food_test_results_short[order(-food_test_results_short$my_ridge_1_prob), ]
my_ridge <- my_ridge[, -c(4, 5:7)]
my_ridge$Results <- as.character(my_ridge$Results)
my_ridge$counter <- with(my_ridge, ave(Results, Results, FUN = seq_along))
which(my_ridge$counter == 2003 & my_ridge$Results == '1')[[1]]

city_ridge <- food_test_results_short[order(-food_test_results_short$city_ridge_1_prob), ] 
city_ridge <- city_ridge[, -c(5, 7:8)]
city_ridge$inspected_in_2015 <- as.character(city_ridge$inspected_in_2015)
city_ridge$counter <- with(city_ridge, ave(inspected_in_2015, inspected_in_2015, FUN = seq_along))
which(city_ridge$counter == 2003 & city_ridge$inspected_in_2015 == '1')[[1]]

my_lasso <- food_test_results_short[order(-food_test_results_short$my_lasso_1_prob), ]
my_lasso <- my_lasso[, -c(5:6, 8)]
my_lasso$Results <- as.character(my_lasso$Results)
my_lasso$counter <- with(my_lasso, ave(Results, Results, FUN = seq_along))
which(my_lasso$counter == 2003 & my_lasso$Results == '1')[[1]]

city_lasso <- food_test_results_short[order(-food_test_results_short$city_lasso_1_prob), ]
city_lasso <- city_lasso[, -c(5:7)]
city_lasso$inspected_in_2015 <- as.character(city_lasso$inspected_in_2015)
city_lasso$counter <- with(city_lasso, ave(inspected_in_2015, inspected_in_2015, FUN = seq_along))
which(city_lasso$counter == 2003 & city_lasso$inspected_in_2015 == '1')[[1]]

sum(food_test$Results == food_test$inspected_in_2015)
