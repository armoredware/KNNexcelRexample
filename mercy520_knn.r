#set working directory
setwd("/root/Desktop/mercy520/classification")

#import csv database
KnnDB <- read.csv(file="gcreditHW4.csv")

myTrainingData = KnnDB[c(2:99), ];


print("Q1- My CWID is 20204002, Training Data:")
print(myTrainingData)


myTestData = KnnDB[c(121,221), ];

print("Q2- My Test Data:")
print(myTestData)

t1 = NULL


iter = 0
print("Q3- Distance from My Test Data, row numbers are based on gcreditHW4.csv:")
print("###Distance from 121:")
while(iter < 98){
iter = iter + 1
realrow = sum(iter,1)
print(paste("row number: ",realrow))
distance1 = sqrt(sum((myTrainingData[c(iter),1] - myTestData[c(1),1])^2,(myTrainingData[c(iter),2] - myTestData[c(1),2])^2,(myTrainingData[c(iter),3] - myTestData[c(1),3])^2))
print(paste("Distance: ",distance1))
t1 = rbind(t1, data.frame(realrow, distance1))
}

t2 = NULL

iter = 0
print("###Distances from 221:")
while(iter < 98){
iter = iter + 1
realrow2 = sum(iter,1)
print(paste("Row Number: ",realrow2))
distance2 = sqrt(sum((myTrainingData[c(iter),1] - myTestData[c(2),1])^2,(myTrainingData[c(iter),2] - myTestData[c(2),2])^2,(myTrainingData[c(iter),3] - myTestData[c(2),3])^2))
print(paste("Distance: ",distance2))
t2 = rbind(t2, data.frame(realrow2, distance2))
}


print("Q4- Top 3 Nearest Neighbors:")
n <- 3
Threenn1 = order(t1[2])[1:n]
print("The 3 Nearest Neighbors for 121 are(real row is the row as it is related to gcreditHW4.csv):")
NN121=t1[c(Threenn1),]
print(NN121)

Threenn2 = order(t2[2])[1:n]
print("The 3 Nearest Neighbors for 221 are(real row is the row as it is related to gcreditHW4.csv):")
NN221=t2[c(Threenn2),]
print(NN221)

print("Q5- Is test data 1 or 2 of cost matrix?:")
print("Actual Data for row 121:")
print(KnnDB[c(NN121[,1]),])
print("Row 69 is not correct  it should be 1, because all of the other nearest neighbors are 1:")

print("Actual Data for row 221:")
print(KnnDB[c(NN221[,1]),])
print("These are all correct, they should all be 1")

