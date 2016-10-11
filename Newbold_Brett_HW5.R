---
  HW5 
---
  
---
  # 1. Print to the console all methods and attributes associated with a dataframe. 
  # Write code to determine the number of columns in a dataframe
---

require(ggplot2)
methods(class = "data.frame")

# Only wanted to print the first ten row names so we can see column names on the 
# console as part of the attritbutes.
attributes(diamonds[1:10,])
ncol(diamonds)

---
  # 2. Write code to determine how many rows are in a dataframe
--- 
  
nrow(diamonds)

---
  # 3. Write code to extract the column names from a dataframe and print the
  # names of the columns (one per line) to the console.
---

cat(colnames(diamonds), sep = "\n")

---
  # 4. Write code to determine the type of each column (numeric, factor, logical,
  # etc.). Print the type of each column to the console.
---
  
lapply(diamonds, class)

---
  # 5. Write code that will loop through any dataframe and calculate the mean of
  # every numeric column. Label the output with the name of the column.
---

# sapply() uses "is.numeric" to find the columns of diamonds that are numeric, 
# colMeans() forms the mean of each numeric column found in diamonds
numeric_cols <- diamonds[sapply(diamonds, is.numeric)]
colMeans(numeric_cols)
  
---
  # 6. Write code that will loop through any dataframe and create a frequency table
  # for every factor column. Label the output with the name of the column.
---

# sapply() will reveal which columns in diamonds are factor columns. diamonds[] 
# outputs those column that are TRUE. summary() will create a frequency table 
# because factors are being passed through
fctr_cols <- diamonds[sapply(diamonds, is.factor)]
summary(fctr_cols) 

---
  # 7. Write code that will loop through any dataframe and determine the number of 
  # rows containing NA (missing value) in EACH column and the percentage of rows
  # containing an NA in ANY of the columns
---

# is.na() will create logical vectors for each entry in the dataframe, rowSums 
# will reveal which rows have an equal amount of missing values as they do 
# columns. sum() will then add the TRUEs from rowSums
sum(rowSums(is.na(diamonds))==ncol(diamonds))

# To find the number of rows containing an NA in any of the columns I took nrow()
# (total number of rows) and subtracted nrow(na.omit()) (the amount of rows 
# without NAs) 
N <- nrow(diamonds) - nrow(na.omit(diamonds))
N

# paste() works well to concatenate
paste(100*(N / nrow(diamonds)), "%")

# diamonds was a bad dataFrame to test with #7 considering 
sum(is.na(diamonds))
# there are no NA values in diamonds. To test I created the following dateFrame:

a <- c(NA,40,72,NA)
b <- c(NA, NA, 36, NA)
c <- c(NA,4,5,NA)
d <- c(NA,5,4,NA)
A <- data.frame(a,b,c,d)

print(A)
sum(is.na(A))

sum(rowSums(is.na(A))==ncol(A))

N <- nrow(A) - nrow(na.omit(A))
N
paste(100*(N / nrow(A)), "% of the rows contain NA in at least one column")


---
  # 8. Create an R function that can accept any dataframe as a parameter and
  # returns a dataframe that contains each pair of column names and their 
  # corresponding Pearson correlation coefficient
---

corCoef <- function(x){
#  This function corCoef() accepts any dataframe as a parameter and returns a
#  dataframe that contains each pair of column names in the first column as a 
#  single string separated by a "-" and their corresponding Pearson correlation
#  coefficient in the second column.
  
#  Parameters: 
#  x - dataFrame
  
#  Returns:
#  dataFrame 
  
#  bn-2016
    
# Making sure only to use the numeric columns 
  y <- x[sapply(x,is.numeric)]
    
# We need at least two numeric columns to run the function
  if(ncol(y)>=2) {

# Possible combinations for each pair of column names (lists them in columns)
    c <- combn(colnames(y), 2)

# Creating the pairs by pasting the rows of c
    pairs <- paste(c[1,], c[2,], sep = "-")

# Creates the correlation coefficients between the columns of y as a matrix
    m <- cor(y, method = "pearson")

# We only need the entries from the lower or upper triangular, choose lower 
# because their order corresponds to the strings created earlier. 
# which() tells us the index values of the lower triangular andcorrespond 
# to the order of the strings. m[] then outputs the values in that order
    correlation <- m[which(lower.tri(m))]

# Putting together the pieces for the new data frame
    new_df <- data.frame(pairs, correlation)
    return(new_df)
  }
  
  else 
    print(paste("This Data Frame does not have two or more numerical columns", 
        "to compute the Pearson correlation coefficient(s)."))
}  

# Feel free to test with various Data Frames
corCoef(cars)
corCoef(attenu)
corCoef(diamonds)
corCoef(sleep) 

# Or test with a simple Data Frame
a <- c(9,5,1)
b <- c(8,12,14)
x <- c(4,6,7)
y <- c(5,6,8)
z <- c(7,1,0)

df <- data.frame(a,b,x,y,z)
df

corCoef(df)
