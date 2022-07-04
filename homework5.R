# problem 2a and 2b
# Complete the R function below to compute the DTW distance between two time-series, v1 and v2, each containing 2D points and using the cost function as in Q1 above. 

dtw <- function (A, B) {
  M <- nrow(A)
  N <- nrow(B)
  Cost <- matrix(0,M,N) # Initialize with zeros
  for (i in 1:M) {
    for (j in 1:N) {
  Cost[i,j] <- as.numeric((A[i,1] - B[j,1])^2 + (A[i,2] - B[j,2])^2) # distance function
      }
  }
  C <- matrix(0,M,N) # Initialize with zeros
  C[1,1] <- Cost[1,1] # Initialize top left cell
  for (i in 2:M) { # Initialize first column
	C[i,1] <- C[i-1,1] + Cost[i,1]
  }
  for (j in 2:N) { # Initialize first row
	C[1,j] <- C[1,j-1] + Cost[1,j]
  }
  #
  # Complete the main loop
  #
  for (i in 2:M){
    for(j in 2:N){
        C[i,j] <- min(C[i-1,j], C[i,j-1], C[i-1,j-1]) + Cost[i,j]
        }
    }

  return (C[M,N])
}

# b) Verify your answer to Q1 using the above function. [show code]
A <- tibble(x=c(2,0,2,4,5), y=c(2,4,6,5,2))
B <- tibble(x=c(1,0,4,5), y=c(1,6,4,1))
dtw(A,B)

# problem 3
# You are given 5 time-series of 2D points (2 column tables) in CSV files: ts2.csv, ts3.csv, ts4.csv, ts5.csv, and tsX.csv. 
# Your goal is to identify which of the time series, ts2-ts5, is most similar to the tsX time series using DTW.

tsx <- read_csv("tsX.csv")
ts2 <- read_csv("ts2.csv")
ts3 <- read_csv("ts3.csv")
ts4 <- read_csv("ts4.csv")
ts5 <- read_csv("ts5.csv")

dtw(ts2,tsx)
dtw(ts3,tsx)
dtw(ts4,tsx)
dtw(ts5,tsx)
# tsX is most similar to: ts5 because 3192.354 is closer to 0 than all the other results
ggplot(tsx, mapping=aes(x=x, y=y)) + geom_path(color="black", size = 2) + geom_path(data=ts5, color="green", size =2)

# problem 5
# Write R code to create the final Tf-Idf weighted Document-Term matrix for the same three documents in the previous question
mydata <- tibble(document=1:3,text=c("good morning everybody", "good evening everybody", "good night"))

# problem 6
# Define a function that takes two vectors as input and computes their cosine similarity
cos_sim <- function(x, y) {sum(x*y)/sqrt(sum(x^2)*sum(y^2))}

# defining vectors
a = c(1,2,3)
b = c(0,2,5)

# testing functions
cos_sim(a,a)
cos_sim(a,b)

# problem 7
# Which of the three documents is the odd one out?
doc1 <- c(0,.366167,0,.135142,0)
doc2 <- c(0,0,.366167,.135142,0)
doc3 <- c(0,0,0,0,.549306)
cos_sim(doc1,doc2)
cos_sim(doc1,doc3)
cos_sim(doc2,doc3)
# doc3 is the odd one out because it has no similarites with doc1 or doc2

# problem 8
# The goal of this exercise is to use the word-based analysis from class to compare the six novels of Jane Austen
books <- janeaustenr::austen_books()
head(books)
summary(books)

# Convert your data to the “tidy” format, i.e., one word per row.
books %>% unnest_tokens(input=text, output = "word") 

# Retain only words that appear greater than 5 times in a novel (Hint: count())
austen_books() %>% unnest_tokens(input = text, output="word") %>% count(word, book) %>% filter(n>5) %>% view()

# Remove stop words
austen_books() %>% unnest_tokens(input = text, output="word") %>% count(word, book) %>% filter(n>5) %>% anti_join(stop_words) %>% view()

# Calculate Tf-Idf weights
books %>% unnest_tokens(input=text, output = "word") %>% count(word,book) %>% filter(n > 5) %>% anti_join(stop_words) %>% bind_tf_idf(term=word, document=book, n=n) %>% view()

# Convert to a table of vectors format
myvectors <- austen_books() %>% unnest_tokens(input = text, output="word") %>% count(word, book) %>% filter(n>5) %>% anti_join(stop_words) %>% bind_tf_idf(term=word, document=book, n=n) %>% select(-n, -tf, -idf) %>% pivot_wider(names_from=book, values_from= tf_idf, values_fill = 0)



# problem 9
# Write R code to compare every pair of Jane Austen’s novels by computing the cosine similarity between
# their corresponding tf-idf vectors (calculated in the previous problem) 
for (x in 2:6)  { 
    for (y in 2:6)
                {
                  print(paste(x,y,cos_sim(myvectors[[x]], myvectors[[y]])) ) 
                }
                }

colnames(myvectors)