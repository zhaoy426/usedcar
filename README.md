### Used Vehicle Price Prediction
#### Final Project for FA21-D590 Applied Data Science

About 40 million used vehicles are sold each year. There are many factors influence a used vehicle’s price. Using the "used-car-price-prediction" data from Kaggle, we aim to develop a machine learning model to predict the price of used vehicles based on various predictors and thus help customers to find a desired vehicle with a reasonable market price. The web app (https://gixq9a-yunyang-zhao.shinyapps.io/ShinyApp/) we designed allows users to interactively explore data, visualize data and obtain used vehicle’s price prediction based on customized feature selection.

Note: the data used for the project comes from 
https://www.kaggle.com/austinreese/craigslist-carstrucks-data. The data was originally collected from Craigslist. Austin Reese creates the data for a school project. This data is updated every few months. The most recent data was updated 7 months ago (Version 10). The data set includes 426,880 rows and 26 columns. The "price" variable is our target. By examining the missing values, data distributions and extreme values, a total of 351,675 observations, 16 features (4 numeric features + 12 categorical features) and one numeric target variable were finalized for car price prediction.

Our team members are: Yunyang Zhao and Xinyu Zhou. Yunyang is responsible for exploratory data analysis using data visualization techniques and feature engineering to fill missing values and variable transformation. Xinyu is responsible for correlation analysis and building regression models to predict car price. Yunyang developed three tab panels, including "Vehicle Data Table", "Vehicle Price by Year / Manufacturer" and "Correlation between Odometer and Price". Xinyu developed two tab panels, including “Regression models and train” and “Predictions”. "About" tab was designed and developed by both Yunyang and Xinyu.

R shiny code is available at github repository: https://github.iu.edu/yunyzhao/usedcar. 
