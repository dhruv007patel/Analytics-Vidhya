#Author : Dhruv Patel

#importing libraries
library(keras)
install_keras()
library(imager)

#Setting path
train <- read.csv("R-Datasets/Datasets/train_face/train.csv", header = TRUE)
btrain <- read.csv("R-Datasets/Datasets/train_face/bbox_train.csv")
test <- read.csv("R-Datasets/Datasets/train_face/test_Rj9YEaI.csv", header = TRUE)

#checking the format of train and test set
head(train)
tail(train)
str(train)
head(test)
tail(test)
str(test)

#Setting path to image dir in train_face folder
img <- image_load("R-Datasets/Datasets/train_face/10054.jpg", grayscale = TRUE, target_size = c(128,128))
img <- image_to_array(img)
files <- read.csv('R-Datasets/Datasets/train_face/test_Rj9YEaI.csv', header = TRUE)


#Pre-processing the image before using it in CNN
image_preprocessor = function(image_path){
  image = image_load(image_path, grayscale = TRUE, target_size = c(128,128))
  image = image_to_array(image)
  image = image/256
  return(image)
}

full_path <- NULL
j <- 1
while(j<=dim(train)[1])
{
    full_path <-c(full_path,paste("R-Datasets/Datasets/train_face/image_data/",train[j,1],sep="", collapse = " ") )
    j <- j+1
}

#Converting image list to image array 
images_list = lapply(full_path, image_preprocessor)
images_array <- array(unlist(images_list), dim = c( length(images_list),nrow(images_list[[1]]), ncol(images_list[[1]]) ))
images_array <- array_reshape(images_array, c(nrow(images_array), 128, 128, 1))

#Training Model using Keras
model <- keras_model_sequential()%>%
  layer_conv_2d(filters = 5, kernel_size = c(3,3), activation='relu', input_shape = c(128,128,1)) %>%
  layer_conv_2d(filters = 5, kernel_size = c(3,3), activation = 'relu') %>%
  layer_conv_2d(filters = 5, kernel_size = c(3,3), activation = 'relu')%>%
  layer_dropout(rate=0.25)%>%
  layer_conv_2d(filters = 10, kernel_size = c(3,3), activation = 'relu')%>%
  layer_conv_2d(filters = 10, kernel_size = c(3,3), activation = 'relu')%>%
  layer_conv_2d(filters = 10, kernel_size = c(3,3), activation = 'relu')%>%
  layer_dropout(rate=0.25)%>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = 'linear') 
  
#Using optimizer to reduce Error.
  model%>%compile(
    loss = loss_mean_squared_error,
    optimizer = optimizer_adadelta(),
    metrics = c('accuracy')
  )
#Fitting the model 
  model%>% fit(
    image_array, train$HeadCount,
    batch_size = 32,
    epochs = 20
  )
#Setting Test image path
  test_path <- NULL
  j <-1
  while(j<=dim(test)[1]){
    full_path <-c(full_path,paste("R-Datasets/Datasets/train_face/image_data/",test[j,1],sep="", collapse = " ") )
    j <- j+1
  }
  
#Predicting on Test images
predictions <- predict(model,test_path)

#write prediction into CSV file
submit <- data.frame(Name=test, HeadCount=predictions)
write.csv(submit, "R-Datasets/Datasets/train_face/solnn.csv", row.names = FALSE)
  