#  In R, when using knn and pca steps in tidymodel/recipes package to create a index using first principal component with a large amount of indicator input variable,  we already specify the year and city number as id variable using `update_role(year, city_number, new_role = "id variable")`, and we printed out the `recipe` object in R, which shows that only predictors such as economic and education variables are used for imputation. However, when we bake the` recipe` object with new testing data set, the id variables year and city number became 'NA' if those id variables are not in the training dateset. For example, the year in training data set is '2000', and city numbers are between 1 and 100, but in the testing data set, the years are between 2000 and 2010, and the city numbers are between 1 and 200. Then after baking the recipe object in testing data set, in the output results, only year == 2000 is shown but other years have 'NA',  and only city numbers between 1 and 100 is shown but city number between 101 and 200 have 'NA'. This situation makes it is hard to link the estimated index/ first principal component to the year and city. Please find the solution.In R, when using knn and pca steps in tidymodel/recipes package to create a index using first principal component with a large amount of indicator input variable,  we already specify the year and city number as id variable using `update_role(year, city_number, new_role = "id variable")`, and we printed out the `recipe` object in R, which shows that only predictors such as economic and education variables are used for imputation. However, when we bake the` recipe` object with new testing data set, the id variables year and city number became 'NA' if those id variables are not in the training dateset. For example, the year in training data set is '2000', and city numbers are between 1 and 100, but in the testing data set, the years are between 2000 and 2010, and the city numbers are between 1 and 200. Then after baking the recipe object in testing data set, in the output results, only year == 2000 is shown but other years have 'NA',  and only city numbers between 1 and 100 is shown but city number between 101 and 200 have 'NA'. This situation makes it is hard to link the estimated index/ first principal component to the year and city. Please find the solution.

library(recipes) #1.3.0
library(dplyr)
library(tidyverse) #1.3.2)
library(tidymodels) #1.0.0

library(dplyr)

# --- Training Data with Missing Values ---
set.seed(123)
n_train <- 300
training_data <- tibble(
  year = rep(2000, n_train),
  city_number = sample(1:100, n_train, replace = TRUE),
  economic_var1 = rnorm(n_train, mean = 5, sd = 2),
  economic_var2 = rbeta(n_train, shape1 = 2, shape2 = 5) * 10,
  education_var1 = runif(n_train, min = 0, max = 1),
  education_var2 = rpois(n_train, lambda = 3),
  replicate(10, sample(0:1, n_train, replace = TRUE)) %>%
    as_tibble() %>%
    set_names(paste0("indicator_", 1:10))
) %>%
  mutate(id = row_number())

# Introduce missing values in predictor variables (excluding id)
cols_to_na_train <- names(training_data)[
  !names(training_data) %in% c("year", "city_number", "id")
]
for (col in cols_to_na_train) {
  n_missing <- sample(0:20, 1) # Introduce 0 to 20 missing values per column
  indices_to_na <- sample(1:n_train, n_missing)
  training_data[[col]][indices_to_na] <- NA
}

# --- Testing Data with Missing Values ---
set.seed(456)
n_test <- 500
testing_data <- tibble(
  year = sample(2000:2010, n_test, replace = TRUE),
  city_number = sample(1:200, n_test, replace = TRUE),
  economic_var1 = rnorm(n_test, mean = 6, sd = 2.5),
  economic_var2 = rbeta(n_test, shape1 = 3, shape2 = 4) * 10,
  education_var1 = runif(n_test, min = 0.2, max = 0.9),
  education_var2 = rpois(n_test, lambda = 4),
  replicate(10, sample(0:1, n_test, replace = TRUE)) %>%
    as_tibble() %>%
    set_names(paste0("indicator_", 1:10))
) %>%
  mutate(id = row_number())

# Introduce missing values in predictor variables (excluding id)
cols_to_na_test <- names(testing_data)[
  !names(testing_data) %in% c("year", "city_number", "id")
]
for (col in cols_to_na_test) {
  n_missing <- sample(0:30, 1) # Introduce 0 to 30 missing values per column
  indices_to_na <- sample(1:n_test, n_missing)
  testing_data[[col]][indices_to_na] <- NA
}

print("Head of Training Data with NAs:")
print(head(training_data))
print("\nHead of Testing Data with NAs:")
print(head(testing_data))


library(recipes)
library(dplyr)

# --- Initial Recipe (Likely Leading to the Issue) ---
initial_recipe <- recipe(~., data = training_data %>% select(-id)) %>%
  update_role(year, city_number, new_role = "id variable") %>%
  step_impute_knn(all_numeric(), neighbors = 5) %>%
  step_pca(all_numeric_predictors(), num_comp = 1, prefix = "PC")

print("Initial Recipe:")
print(initial_recipe)
tidy(initial_recipe, number = 1)

trained_initial_recipe <- prep(initial_recipe, training_data %>% select(-id))
baked_testing_initial <- bake(
  trained_initial_recipe,
  new_data = testing_data %>% select(-id)
)

print("\nBaked Testing Data (Initial Recipe - Expect NA's):")
print(head(
  baked_testing_initial %>% select(year, city_number, starts_with("PC"))
))

# --- Fixed Recipe (Explicitly Excluding ID Variables) ---
fixed_recipe_exclude <- recipe(~., data = training_data %>% select(-id)) %>%
  update_role(year, city_number, new_role = "id variable") %>%
  step_impute_knn(
    -year,
    -city_number,
    all_numeric_predictors(),
    neighbors = 5
  ) %>%
  step_pca(all_numeric_predictors(), num_comp = 1, prefix = "PC")

print("\nFixed Recipe (Excluding ID Variables):")
print(fixed_recipe_exclude)

trained_fixed_recipe_exclude <- prep(
  fixed_recipe_exclude,
  training_data %>% select(-id)
)
baked_testing_fixed_exclude <- bake(
  trained_fixed_recipe_exclude,
  new_data = testing_data %>% select(-id)
)

print("\nBaked Testing Data (Fixed Recipe - Excluding ID Variables):")
print(head(
  baked_testing_fixed_exclude %>% select(year, city_number, starts_with("PC"))
))

# --- Fixed Recipe (Selecting Predictors by Role) ---
fixed_recipe_role <- recipe(~., data = training_data %>% select(-id)) %>%
  update_role(year, city_number, new_role = "id variable") %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_pca(all_predictors(), num_comp = 1, prefix = "PC")

print("\nFixed Recipe (Selecting Predictors by Role):")
print(fixed_recipe_role)

trained_fixed_recipe_role <- prep(
  fixed_recipe_role,
  training_data %>% select(-id)
)
baked_testing_fixed_role <- bake(
  trained_fixed_recipe_role,
  new_data = testing_data %>% select(-id)
)

print("\nBaked Testing Data (Fixed Recipe - Selecting Predictors by Role):")
print(head(
  baked_testing_fixed_role %>% select(year, city_number, starts_with("PC"))
))
