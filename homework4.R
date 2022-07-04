# problem 1, part b
# Normalize the Preg and BP values by scaling the minimum-maximum range of each column to 0-1. Fill in the empty columns in the table.
Preg <- c(2,3,2,1,2)
BP <- c(74,58,58,54,70)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
Preg.Norm <- normalize(Preg)
BP.Norm <- normalize(BP)

# problem 1, part c
# Predict whether a subject with Preg=2, BP=70 will have diabetes using the 1-NN
a <- c(2,74)
b <- c(3,58)
c <- c(2,58)
d <- c(1,54)
predictor <- c(2,70)
a.norm <- c(.5,1)
b.norm <- c(1,.2)
c.norm <- c(.5,.2)
d.norm <- c(0,0)
predictor.norm <- c(.5,.8)

# Using Euclidean distance on the original variables:
euclidean <- function(a, b) sqrt(sum((a - b)^2))
euclidean(a,predictor)
euclidean(b,predictor)
euclidean(c,predictor)
euclidean(d,predictor)

# Using Manhattan distance on the original variables:
manhattan_dist <- function(a, b){
 	dist <- abs(a-b)
 	dist <- sum(dist)
 	return(dist)
}
manhattan(a,predictor)
manhattan(b,predictor)
manhattan(c,predictor)
manhattan(d,predictor)

# Using Euclidean distance on the normalized variables:
euclidean(a.norm,predictor.norm)
euclidean(b.norm,predictor.norm)
euclidean(c.norm,predictor.norm)
euclidean(d.norm,predictor.norm)

# Using Manhattan distance on the normalized variables:
manhattan(a.norm,predictor.norm)
manhattan(b.norm,predictor.norm)
manhattan(c.norm,predictor.norm)
manhattan(d.norm,predictor.norm)


# problem 2, part a
# Load and preprocess the data
pima <- read_csv("pima-indians-diabetes-resampled.csv")
nrow(pima)
ncol(pima)

# Remove rows which contain missing values in the Glucose, BP, BMI, Pedigree, or Age columns
pima <- pima %>% filter(Glucose > 0 & BP > 0 & BMI > 0 & Pedigree > 0 & Age > 0)

# Normalize each column by scaling the minimum-maximum range of each column to 0-1. (Hint: the normalize() function created in class can be used for this)
pima <- pima %>% mutate(Preg.Norm = normalize(Preg)) %>% mutate(Glucose.Norm = normalize(Glucose)) %>% mutate(BP.Norm = normalize(BP)) %>% mutate(BMI.Norm = normalize(BMI)) %>% mutate(Pedigree.Norm = normalize(Pedigree)) %>% mutate(Age.Norm = normalize(Age))

# Train and test a k-nearest neighbor classifier with the above datasets. Consider only the normalized Preg and Pedigree columns. 
# Set k=1. What is the error rate (number of misclassifications)?
trainindex <- 1:500
trainfeatures <- pima[trainindex, c(8,12)]
trainlabels <- pima[trainindex, 7]
testindex <- 501:724
testfeatures <- pima[testindex, c(8,12)]
testlabels <- pima[testindex, 7]
predicted <- knn(train=trainfeatures, test=testfeatures, cl=trainlabels$HasDiabetes, k=1)
table(testlabels$HasDiabetes, predicted)
# Error Rate = FP + FN / ALL = 50 + 38 / 224 = .3929

# Repeat part (d) but consider only the normalized Preg, Pedigree, and Glucose columns. Set k=1. What is the error rate? 
trainindex <- 1:500
trainfeatures <- pima[trainindex, c(8,9,12)]
trainlabels <- pima[trainindex, 7]
testindex <- 501:724
testfeatures <- pima[testindex, c(8,9,12)]
testlabels <- pima[testindex, 7]
predicted <- knn(train=trainfeatures, test=testfeatures, cl=trainlabels$HasDiabetes, k=1)
table(testlabels$HasDiabetes, predicted)
# Error Rate = FP + FN / ALL = 40 + 35 / 224 = .3348

# Repeat part (e) but set k=9. What is the error rate?
predicted <- knn(train=trainfeatures, test=testfeatures, cl=trainlabels$HasDiabetes, k=9)
table(testlabels$HasDiabetes, predicted)
#Error Rate = FP + FN / ALL = 41 + 19 / 224 = .2677

# Repeat part (e) but set k=15. What is the error rate? 
predicted <- knn(train=trainfeatures, test=testfeatures, cl=trainlabels$HasDiabetes, k=15)
table(testlabels$HasDiabetes, predicted)
#Error Rate = FP + FN / ALL = 44 + 15 / 224 = .25

# problem 4
# How many [B]enign cases are there in the data?: 444
cancer %>% filter(Class == "B") %>% summarise(Count = n())

# How many [M]alignant cases are there in the data?: 239
cancer %>% filter(Class == "M") %>% summarise(Count = n())

# Run the k-means clustering algorithm using all the rows and all the 9 features. Use nstart=10.
cancer <- read_csv("breast-cancer-wisconsin.csv")
cancerMeans <- kmeans(cancer[,2:10], center=4, iter.max=10, nstart=10)
cancerMeans$centers
table(cancer$Class, cancerMeans$cluster) 