library(shiny)
library(shinythemes)
library(markdown)
library(readr)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

# read data
cardata <- read_csv('vehicles_new.csv')

cardata2 = as.matrix(cardata)
dele_tag=c()
for (mm in c(1:length(cardata2[,1]))) {
  if(TRUE==(NA%in%cardata2[mm,])){
    dele_tag = append(dele_tag,mm,after = length(dele_tag))
  }
}
cardata2 = as.data.frame(cardata2[-dele_tag,])
cardata3 = cardata2[,-c(4,6,10,15,16,17,18)]

# ## manufacturer
# manufacturer_v = cbind(as.matrix(unique(cardata3[,3])),c(1:length(unique(cardata3[,3]))))
# 
# for (cc in c(1:length(manufacturer_v[,1]))) {
#   cardata3[which(cardata3[,3]%in%manufacturer_v[cc,1]),3]=cc
# }
# fuel_v = as.matrix(unique(cardata3[,6]))
# for (cc in c(1:length(fuel_v[,1]))) {
#   cardata3[which(cardata3[,6]%in%fuel_v[cc,1]),6]=cc
# }
# 

match_list=list()
qqq=1
#### 之后预测的时候要找到固定的对应
for (mm in c(3,4,6,8,9,10,11)) {
  vet = as.matrix(unique(cardata3[,mm]))
  vet2=cbind(vet,c(1:length(vet)))
  match_list[[qqq]]=vet2
  names(match_list)[qqq]=colnames(cardata3)[mm]
  qqq=qqq+1
  for (cc in c(1:length(vet[,1]))) {
    cardata3[which(cardata3[,mm]%in%vet[cc,1]),mm]=cc
  }
}
save_names = colnames(cardata3)

cardata3 = t(apply(cardata3,1,function(x) as.numeric(x)))
colnames(cardata3) = save_names


#### 清洗数据
cardata3[,7] = floor(as.numeric(cardata3[,7])/5000)
cardata3[,2] = as.numeric(cardata3[,2])-1900
#### 去掉太老的车 和价格太低太高的车
rem = c(which(cardata3[,1]>40000),which(cardata3[,1]<1000),which(cardata3[,2]<80))
cardata3 = cardata3[-rem,]
cardata3 = as.data.frame(cardata3)

cardata3_scaled = scale(cardata3,center = TRUE,scale = TRUE)

y = cardata3_scaled[,1]
X = cardata3_scaled[,-1]
train_id = runif(floor(length(y)*0.8),1,length(y))

data_train = as.data.frame(cardata3_scaled[train_id,])
data_test = as.data.frame(cardata3_scaled[-train_id,])
# Linear Regression
formaula= paste(save_names[1],seq='~',paste(save_names[-1],collapse = '+'))
#训练
# linear_model = lm(formaula,data=data_train[1:10000,])
# # summary(linear_model)
# #预测
# predict(linear_model,data_train[1:10,-1])
# print(data_test[1:10,1])

##### Support Vector Machines
library(MASS)
library(e1071)


# svm_model = svm(price~.,data=data_train[1:10000,], type="eps-regression")

# predict(svm_model,data_test[1:10,-1])
# print(data_test[1:10,1])
# svm_pred_table <- table(pred=svm_pred,true=data_test[1:1000,1], type="eps-regression")
# svm_accuracy <- sum(diag(svm_pred_table))/sum(svm_pred_table)
# svm_accuracy
####
library(glmnet)
# glm_model = cv.glmnet(x=as.matrix(data_train[,-1]),y=as.matrix(data_train[,1]))
# summary(glm_model)
# y_p = predict(glm_model,s = glm_model$lambda.min,as.matrix(data_test[1,-1]))



# glm_model = cv.glmnet(x=as.matrix(cardata3[,-1]),y=as.matrix(cardata3[,1]))
# predict(glm_model,s = glm_model$lambda.min,as.matrix(cardata3[1:10,-1]))

# linear_model = lm(formaula,data=cardata3)
# summary(linear_model)
#预测
# predict(linear_model,cardata3[1,-1])
# X=as.matrix(0,nrow=1,ncol=11)



ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage(strong("Used Car Price Prediction"),
                           
                           # 1st Panel - About your data
                           tabPanel(strong("About"),
                                    fluidPage(
                                      titlePanel("About")),
                                    br(),
                                    hr(),
                                    fluidRow(
                                      column(6,
                                             includeMarkdown("about.md")
                                      ),
                                      column(4,
                                             img(class="gnt_em_img_i",
                                                 src=paste0("https://www.gannett-cdn.com/media/2018/06/14/USATODAY/usatsports/car-lot-square-e1461855298700.jpg?width=500&amp;height=500&amp;fit=crop&amp;format=pjpg&amp;auto=webp",
                                                            "Average used-car prices topped $20,000 in the third quarter.")),
                                             
                                             tags$small("Average used-car prices topped $20,000 in the third quarter.",
                                                        "Photo Source: ",
                                                        a(href="https://www.usatoday.com/story/money/cars/2018/11/08/used-car-prices/1928840002/", "https://www.usatoday.com/story/money/cars/2018/11/08/used-car-prices/1928840002/"),
                                             )
                                      )
                                    )
                                    
                           ),
                           # Second tab panel: data table
                           tabPanel("Vehicle DataTable",
                                    fluidPage(
                                      titlePanel("Vehicle DataTable"),
                                      
                                      sidebarPanel(
                                        selectInput("state",
                                                    "State:",
                                                    c("All",
                                                      unique(as.character(cardata$state)))),
                                        selectInput("man",
                                                    "Manufacturer:",
                                                    c("All",
                                                      unique(as.character(cardata$manufacturer)))),
                                        selectInput("condition",
                                                    "Condition:",
                                                    c("All",
                                                      unique(as.character(cardata$condition)))),
                                        selectInput("trans",
                                                    "Transmission:",
                                                    c("All",
                                                      unique(as.character(cardata$transmission)))),
                                        selectInput("cyl",
                                                    "Cylinders:",
                                                    c("All",
                                                      unique(as.character(cardata$cylinders)))),
                                        selectInput("fuel",
                                                    "Fuel:",
                                                    c("All",
                                                      unique(as.character(cardata$fuel)))),
                                        selectInput("color",
                                                    "Color:",
                                                    c("All",
                                                      unique(as.character(cardata$paint_color)))),
                                        selectInput("title",
                                                    "Title:",
                                                    c("All",
                                                      unique(as.character(cardata$title_status)))),
                                        selectInput("type",
                                                    "Type:",
                                                    c("All",
                                                      unique(as.character(cardata$type)))),
                                        sliderInput("year", 'Year range:', min=1980, max=2022, value=c(2000, 2021), step = 1),
                                        sliderInput("price", 'Price range:', min=100, max=60000, value=c(2000, 10000), step=100),
                                        sliderInput("odo", 'Odometer range:', min=0, max=10000000, value=c(50000, 200000), step=1000)),
                                      
                                      mainPanel(
                                        DT::dataTableOutput("table")
                                      )
                                    )
                           ),
                           
                           ## The 3rd tab: Vehicle Price by Year / Manufacturer
                           tabPanel("Vehicle Price by Year / Manufacturer", 
                                    fluidPage(
                                      titlePanel("Vehicle Price by Year / Manufacturer"),
                                      # Define the sidebar with one input
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("state2",
                                                      "State:",
                                                      c("All",
                                                        unique(as.character(cardata$state)))),
                                          selectInput("man2",
                                                      "Manufacturer:",
                                                      c("All",
                                                        unique(as.character(cardata$manufacturer)))),
                                          selectInput("condition2",
                                                      "Condition:",
                                                      c("All",
                                                        unique(as.character(cardata$condition)))),
                                          selectInput("trans2",
                                                      "Transmission:",
                                                      c("All",
                                                        unique(as.character(cardata$transmission)))),
                                          selectInput("cyl2",
                                                      "Cylinders:",
                                                      c("All",
                                                        unique(as.character(cardata$cylinders)))),
                                          selectInput("fuel2",
                                                      "Fuel:",
                                                      c("All",
                                                        unique(as.character(cardata$fuel)))),
                                          selectInput("color2",
                                                      "Color:",
                                                      c("All",
                                                        unique(as.character(cardata$paint_color)))),
                                          selectInput("title2",
                                                      "Title:",
                                                      c("All",
                                                        unique(as.character(cardata$title_status)))),
                                          selectInput("type2",
                                                      "Type:",
                                                      c("All",
                                                        unique(as.character(cardata$type)))),
                                          sliderInput("odo2", 'Odometer range:', min=0, max=10000000, value=c(0, 10000000), step=1000),
                                        ),
                                        
                                        # Create a spot for the barplot
                                        mainPanel(
                                          plotOutput("yearPlot"),
                                          plotOutput("manPlot")
                                        ) )
                                    )
                           )
                           ,
                           # The 4th tab: correlation between odometer and price
                           tabPanel("Correlation between Odometer and Price", 
                                    fluidPage(    
                                      # Generate a row with a sidebar
                                      sidebarLayout(      
                                        # Define the sidebar with one input
                                        sidebarPanel(
                                          selectInput("state3",
                                                      "State:",
                                                      c("All",
                                                        unique(as.character(cardata$state)))),
                                          selectInput("man3",
                                                      "Manufacturer:",
                                                      c("All",
                                                        unique(as.character(cardata$manufacturer)))),
                                          selectInput("condition3",
                                                      "Condition:",
                                                      c("All",
                                                        unique(as.character(cardata$condition)))),
                                          selectInput("trans3",
                                                      "Transmission:",
                                                      c("All",
                                                        unique(as.character(cardata$transmission)))),
                                          selectInput("cyl3",
                                                      "Cylinders:",
                                                      c("All",
                                                        unique(as.character(cardata$cylinders)))),
                                          selectInput("fuel3",
                                                      "Fuel:",
                                                      c("All",
                                                        unique(as.character(cardata$fuel)))),
                                          selectInput("color3",
                                                      "Color:",
                                                      c("All",
                                                        unique(as.character(cardata$paint_color)))),
                                          selectInput("title3",
                                                      "Title:",
                                                      c("All",
                                                        unique(as.character(cardata$title_status)))),
                                          selectInput("type3",
                                                      "Type:",
                                                      c("All",
                                                        unique(as.character(cardata$type)))),
                                          sliderInput("year3", 'Year range:', min=1960, max=2022, value=c(2000, 2021), step = 1),
                                          sliderInput("odo3", 'Odometer range:', min=0, max=1000000, value=c(0, 500000), step=1000),
                                        ),
                                        # Create a spot for the barplot
                                        mainPanel(
                                          plotOutput("corrPlot"),
                                        )
                                      )
                                    )
                           ),
                           # The 5th tab: Regression models and predictions
                           
                           tabPanel("Regression models and train",
                                    fluidPage(
                                      sidebarLayout(      
                                        # Define the sidebar with one input
                                        sidebarPanel(
                                          selectInput("different_model",
                                                      "model:",
                                                      c("Linear Regression","Support Vector Machines","Ridge Regressor")),
                                         
                                          sliderInput("year5", 'Year range:', min=1960, max=2022, value=c(2000, 2021), step = 1),
                                          sliderInput("odo5", 'Odometer range:', min=0, max=1000000, value=c(0, 500000), step=1000),
                                        ),
                                        # Create a spot for the barplot
                                        mainPanel(
                                          tabPanel("gai", verbatimTextOutput("summary")),
                                          DT::dataTableOutput("table2"),
                                          
                                        )
                                      )
                                      
                                      )

                           ),
                           ###predictions
                           tabPanel("predictions",
                                    fluidPage(
                                      sidebarLayout(      
                                        ## 适用于少量文本

                                        sidebarPanel(
                                          selectInput("different_model2",
                                                      "Select the model you want to use:",
                                                      c("Linear Regression","Support Vector Machines","Ridge Regressor")),
                                          selectInput("man6",
                                                      "Manufacturer:",
                                                      c(
                                                        unique(as.character(cardata$manufacturer)))),
                                          selectInput("condition6",
                                                      "Condition:",
                                                      c(
                                                        unique(as.character(cardata$condition)))),
                                          selectInput("trans6",
                                                      "Transmission:",
                                                      c(
                                                        unique(as.character(cardata$transmission)))),
                                          selectInput("cyl6",
                                                      "Cylinders:",
                                                      c(
                                                        unique(as.character(cardata2$cylinders_n)))),
                                          selectInput("fuel6",
                                                      "Fuel:",
                                                      c(
                                                        unique(as.character(cardata$fuel)))),
                                          selectInput("color6",
                                                      "Color:",
                                                      c(
                                                        unique(as.character(cardata$paint_color)))),
                                          selectInput("drive6",
                                                      "drive:",
                                                      c(
                                                        unique(as.character(cardata2$drive)))),
                                          selectInput("type6",
                                                      "Type:",
                                                      c(
                                                        unique(as.character(cardata$type)))),
                                          textInput("year6", "year"),
                                          textInput("odometer6", "odometer"),
                                        ),
                                        
                                        # textInput("miles", "miles"),
                                        # Create a spot for the barplot
                                        mainPanel(
                                          #plotOutput("corrPlot"),
                                          textOutput("text1"),
                                          DT::dataTableOutput("info"),
                                          textOutput("text2"),
                                          tabPanel("predict", verbatimTextOutput("predict")),
                                          tabPanel("M", plotOutput("M1")),
                                        )
                                      )
                                      
                                    )
                                    
                           )
                           
                           ))

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cardata
    if (input$state != "All") {
      data <- data[data$state == input$state,]
    }
    if (input$man != "All") {
      data <- data[data$manufacturer == input$man,]
    }
    if (input$condition != "All") {
      data <- data[data$condition == input$condition,]
    }
    if (input$cyl != "All") {
      data <- data[data$cylinders == input$cyl,]
    }
    if (input$fuel != "All") {
      data <- data[data$fuel == input$fuel,]
    }
    if (input$trans != "All") {
      data <- data[data$transmission == input$trans,]
    }
    if (input$color != "All") {
      data <- data[data$paint_color == input$color,]
    }
    if (input$title != "All") {
      data <- data[data$title_status == input$title,]
    }
    if (input$type != "All") {
      data <- data[data$type == input$type,]
    }
    
    data <- data[data$year %in% (input$year[1]: input$year[2]),]
    
    data <- data[data$price %in% (input$price[1]: input$price[2]),]
    
    data <- data[data$odometer %in% (input$odo[1]: input$odo[2]),]
    
    data
  }))
  
  # Fill in the spot we created for a plot
  output$yearPlot <- renderPlot({
    data2 <- cardata
    if (input$man2 != "All") {
      data2 <- data2[data2$manufacturer == input$man2,]
    }
    if (input$cyl2 != "All") {
      data2 <- data2[data2$cylinders == input$cyl2,]
    }
    if (input$fuel2 != "All") {
      data2 <- data2[data2$fuel == input$fuel2,]
    }
    if (input$trans2 != "All") {
      data2 <- data2[data2$transmission == input$trans2,]
    }
    if (input$condition2 != "All") {
      data2 <- data2[data2$condition == input$condition2,]
    }
    if (input$state2 != "All") {
      data2 <- data2[data2$state == input$state2,]
    }
    if (input$color2 != "All") {
      data2 <- data2[data2$paint_color == input$color2,]
    }
    if (input$title2 != "All") {
      data2 <- data2[data2$title_status == input$title2,]
    }
    if (input$type2 != "All") {
      data2 <- data2[data2$type == input$type2,]
    }
    data2 <- data2[data2$odometer %in% (input$odo2[1]: input$odo2[2]),]
    
    data2 <- group_by(data2, year) %>%
      summarise(avgPrice = mean(price))
    
    # Render a barplot
    ggplot(data2, aes(x=year), y=avgPrice) +
      geom_bar(aes(weight=avgPrice),colour="green") +
      xlab("Year") +
      ylab("Avg. Price")
      
    
  })
  
  output$manPlot <- renderPlot({
    data3 <- cardata
    if (input$man2 != "All") {
      data3 <- data3[data3$manufacturer == input$man2,]
    }
    if (input$cyl2 != "All") {
      data3 <- data3[data3$cylinders == input$cyl2,]
    }
    if (input$fuel2 != "All") {
      data3 <- data3[data3$fuel == input$fuel2,]
    }
    if (input$trans2 != "All") {
      data3 <- data3[data3$transmission == input$trans2,]
    }
    if (input$condition2 != "All") {
      data3 <- data3[data3$condition == input$condition2,]
    }
    if (input$state2 != "All") {
      data3 <- data3[data3$state == input$state2,]
    }
    if (input$color2 != "All") {
      data3 <- data3[data3$paint_color == input$color2,]
    }
    if (input$title2 != "All") {
      data3 <- data3[data3$title_status == input$title2,]
    }
    if (input$type2 != "All") {
      data3 <- data3[data3$type == input$type2,]
    }
    
    data3 <- data3[data3$odometer %in% (input$odo2[1]: input$odo2[2]),]
    data3 <- group_by(data3, manufacturer) %>%
      summarise(avgPrice = mean(price))
    
    # Render a barplot
    ggplot(data3, aes(x=manufacturer), y=avgPrice) +
      geom_bar(aes(weight=avgPrice),fill="#FF9999", colour="black") +
      xlab("Manufacturer") +
      ylab("Avg. Price") +
      theme(axis.text.x=element_text(angle =90, vjust = 0.5))
    
  })

  
  output$corrPlot <- renderPlot({
    data <- cardata
    if (input$man3 != "All") {
      data <- data[data$manufacturer == input$man3,]
    }
    if (input$trans3 != "All") {
      data <- data[data$transmission == input$trans3,]
    }
    if (input$condition3 != "All") {
      data <- data[data$condition == input$condition3,]
    }
    if (input$cyl3 != "All") {
      data <- data[data$cylinders == input$cyl3,]
    }
    if (input$fuel3 != "All") {
      data <- data[data$fuel == input$fuel3,]
    }
    if (input$state3 != "All") {
      data <- data[data$state == input$state3,]
    }
    if (input$color3 != "All") {
      data <- data[data$paint_color == input$color3,]
    }
    if (input$title3 != "All") {
      data <- data[data$title_status == input$title3,]
    }
    if (input$type3 != "All") {
      data <- data[data$type == input$type3,]
    }
    data <- data[data$year %in% (input$year3[1]: input$year3[2]),]
    data <- data[data$odometer %in% (input$odo3[1]: input$odo3[2]),]
    odometer <- data$odometer
    price <- data$price
    fuel <- data$fuel
    ggplot()+geom_point(aes(x=odometer,y=price,color=fuel))+
      labs(title = 'Correlation between Odometer and Price',
           subtitle = 'With respect to fuel type',
           x= 'Odometer',
           y='Price',
           caption = 'Data Source: Kaggle')
  })
  
  output$summary <- renderPrint({
    #print(input$year5[1])
    sum_data <- cardata3
    
    sum_data <- sum_data[sum_data$year %in% ((input$year5[1]: input$year5[2])-1900),]
    
    aa=which(sum_data$odometer>input$odo5[1]/5000)
    bb=which(sum_data$odometer<input$odo5[2]/5000)
    sum_data <- sum_data[c(aa,bb),]
    model = lm(formaula,data=sum_data)
    if (input$different_model != "Linear Regression") {
      if (input$different_model == "Support Vector Machines"){
        model = svm(price~.,data=sum_data[1:10000,], type="eps-regression")
      }else{
        model = cv.glmnet(x=as.matrix(sum_data[,-1]),y=as.matrix(sum_data[,1]),alpha=0)
      }
      
    }
    
summary(model)
    
    
    
     })
  

  output$M1 <- renderPlot({
    M<-cor(cardata3)
    library(corrplot)
    corrplot(M, method="circle")
    #par(mfrow = c(2, 2))
    corrplot(M, method="pie")
    #par(mfrow = c(1, 1))
  })
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    sum_data <- cardata3
    
    sum_data2 <- data_train[sum_data$year %in% ((input$year5[1]: input$year5[2])-1900),]
    
    aa=which(sum_data$odometer>input$odo5[1]/5000)
    bb=which(sum_data$odometer<input$odo5[2]/5000)
    sum_data2 <- sum_data2[c(aa,bb),]
    # 
    # print('Normalized matrix')
    sum_data2
  }))
  
  output$text1 <- renderText({ 
    "Please input the information of your car first"
  })
  
  output$text2 <- renderText({ 
    "The following is the estimated price of your car"
  })
  
  
  
  
  output$info<- DT::renderDataTable(DT::datatable({
    #print(input$year5[1])
    X=cardata3[1,]
    colnames(X)=save_names
    # X[,1]=0
    X[,3] = input$man6
    X[,8] = input$trans6
    X[,4] = input$condition6
    X[,9] = input$drive6
    X[,5] = as.numeric(input$cyl6)
    X[,6] = input$fuel6
    X[,11] = input$color6
    X[,10] = input$type6
    X[,2] = as.numeric(input$year6)
    X[,7] = as.numeric(input$odometer6)
    
    X[,-1]

    
    
  }))
  
  
  
  
  
  output$predict <- renderPrint({
    #print(input$year5[1])
    sum_data <- cardata3
    X=sum_data[1,]
    # colnames(X)=save_names
    # X[,1]=0
    X[,3] = as.numeric(which(match_list[[1]]%in%input$man6))
    X[,8] = as.numeric(which(match_list[[4]]%in%input$trans6))
    X[,4] = as.numeric(which(match_list[[2]]%in%input$condition6))
    X[,9] = as.numeric(which(match_list[[5]]%in%input$drive6))
    X[,5] = as.numeric(input$cyl6)
    X[,6] = as.numeric(which(match_list[[3]]%in%input$fuel6))
    X[,11] = as.numeric(which(match_list[[7]]%in%input$color6))
    X[,10] = as.numeric(which(match_list[[6]]%in%input$type6))
    X[,2] = as.numeric(input$year6)-1900
    X[,7] = floor(as.numeric(input$odometer6)/5000)
    
    X
    linear_model = lm(formaula,data=sum_data)
    #summary(linear_model)
    if (input$different_model2 != "Linear Regression") {
      if (input$different_model2 == "Support Vector Machines"){
        svm_model = svm(price~.,data=sum_data[1:10000,], type="eps-regression")
        predict(svm_model,as.data.frame(X[,-1]))
      }else{
        ridge_model = cv.glmnet(x=as.matrix(sum_data[,-1]),y=as.matrix(sum_data[,1]),alpha=0)
        predict(ridge_model,s = glm_model$lambda.min,as.matrix(X[,-1]))
      }

    }else{
    predict(linear_model,as.data.frame(X[,-1]))
    }
    

  })
  
  
}


shinyApp(ui, server)



# corrplot(M, method="color")
# corrplot(M, method="number")
# 
# corrplot(M, type="upper")
# corrplot(M, type="lower")
# 
# col<- colorRampPalette(c("red", "white", "blue"))(20)
# corrplot(M, type="upper", order="hclust", col=col)


