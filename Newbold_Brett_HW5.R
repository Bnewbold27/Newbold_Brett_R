---
  HW5 
---
  
  ---
  # 1. Print to the console all methods and attributes associated with a dataframe. 
  # Write code to determine the number of columns in a dataframe
  ---
  
  require(ggplot2)

Df_Info <- function(x)
  #  This function Df_Info() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  x - dataFrame
  
  #  Returns:
  #  all methods and attributes of the data frame 
  
  #  bn-2016
  {
    methods(class = x)   # Will be the same for any data frame
    attributes(x)
  }

data("diamonds")
Df_Info(diamonds[1:10,])  # Only wanted to print the first ten row names so we can see column names on the
                          # console as part of the attritbutes.


Column_Num <- function(x)
  #  This function Column_Num() accepts any dataframe as a parameter
  
  #  Parameters: 
  #  x - dataFrame
  
  #  Returns:
  #  the number of columns in a dataFrame 
  
  #  bn-2016
  {
    ncol(x)
  }

data("diamonds")
Column_Num(diamonds)


---
  # 2. Write code to determine how many rows are in a dataframe
  --- 
  
  Row_Num <- function(x)
    #  This function Row_Num() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  the number of rows in a dataFrame
    
    #  bn-2016
  {
    nrow(x)
  }

data("diamonds")
Row_Num(diamonds)

---
  # 3. Write code to extract the column names from a dataframe and print the
  # names of the columns (one per line) to the console.
  ---
  
  Column_Names <- function(x)
    #  This function Column_Names() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  the names of the columns (one per line) 
    
    #  bn-2016
  {
    cat(colnames(x), sep = "\n")   # outputs each column name of the data frame while adding a new line
  }

data("diamonds")
Column_Names(diamonds)

---
  # 4. Write code to determine the type of each column (numeric, factor, logical,
  # etc.). Print the type of each column to the console.
  ---
  
  Column_Type <- function(x)
    #  This function Column_Type() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  the type of each column
    
    #  bn-2016
  {  
    lapply(x, class)  # returns a list of of the same length as dataFrame, specifiying the type of columns
  }

data("diamonds")
Column_Type(diamonds)

---
  # 5. Write code that will loop through any dataframe and calculate the mean of
  # every numeric column. Label the output with the name of the column.
  ---
  
  Column_Means <- function(x)
    #  This function Column_Means() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  the mean of every numeric column 
    
    #  bn-2016
  {
    numeric_cols <- x[sapply(x, is.numeric)]   # sapply() uses "is.numeric" to find the columns of dataFrame that are numeric
    colMeans(numeric_cols)    # colMeans() forms the mean of each numeric column found in dataFrame
  }

data("diamonds")
Column_Means(diamonds)

---
  # 6. Write code that will loop through any dataframe and create a frequency table
  # for every factor column. Label the output with the name of the column.
  ---
  
  Freq_Table <- function(x)
    #  This function Freq_Table() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  a frequency table for every factor column 
    
    #  bn-2016
  {
    fctr_cols <- x[sapply(x, is.factor)]   #  sapply() will reveal which columns in x are factor columns. 
                                           #  x[] outputs those column that are TRUE. 
    summary(fctr_cols)   # summary() will create a frequency table because factors are being passed through
  }

data("diamonds")
Freq_Table(diamonds)

---
  # 7. Write code that will loop through any dataframe and determine the number of 
  # rows containing NA (missing value) in EACH column and the percentage of rows
  # containing an NA in ANY of the columns
  ---
  
  NA_summary <- function(x)
    #  This function NA_Summary() accepts any dataframe as a parameter
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns: the number of rows containing NA (missing value) in EACH column and the percentage 
    #  of rows containing an NA in ANY of the columns
    
    #  bn-2016
  {
    sum(rowSums(is.na(x))==ncol(x))   # is.na() will create logical vectors for each entry in the dataframe, rowSums
                                      # will reveal which rows have an equal amount of missing values as they do
                                      # columns. sum() will then add the TRUEs from rowSums
    
    N <- nrow(x) - nrow(na.omit(x))   # To find the number of rows containing an NA in any of the columns I took nrow()
                                      # (total number of rows) and subtracted nrow(na.omit()) (the amount of rows without NAs) 
    
    print(paste(N, "row(s) contain NA in each column"))
    
    paste(100*(N / nrow(x)), sep = "", "% of the rows contain NA in at least one column")
  }

data("diamonds")
NA_summary(diamonds)
sum(is.na(diamonds))   # diamonds was a bad dataFrame to test with #7 considering there are no NA values in diamonds. 


#To test I created the following dataFrame:
a <- c(NA,40,72,NA)
b <- c(NA, NA, 36, NA)
c <- c(NA,4,5,NA)
d <- c(NA,5,4,NA)
A <- data.frame(a,b,c,d)

print(A) 

NA_summary(A)


---
  # 8. Create an R function that can accept any dataframe as a parameter and
  # returns a dataframe that contains each pair of column names and their 
  # corresponding Pearson correlation coefficient
  ---
  
  corCoef <- function(x)
    #  This function corCoef() accepts any dataframe as a parameter and returns a
    #  dataframe that contains each pair of column names in the first column as a 
    #  single string separated by a "-" and their corresponding Pearson correlation
    #  coefficient in the second column.
    
    #  Parameters: 
    #  x - dataFrame
    
    #  Returns:
    #  dataFrame 
    
#  bn-2016
  {
    y <- x[sapply(x,is.numeric)]   # Making sure only to use the numeric columns
    
    
    if(ncol(y)>=2)   # We need at least two numeric columns to run the function
    {
      
      c <- combn(colnames(y), 2)   # Possible combinations for each pair of column names (lists them in columns)
      
      pairs <- paste(c[1,], c[2,], sep = "-")   # Creating the pairs by pasting the rows of c
      
      m <- cor(y, method = "pearson")   # Creates the correlation coefficients between the columns of y as a matrix
      
      correlation <- m[which(lower.tri(m))]  # We only need the entries from the lower or upper triangular, choose lower 
                                             # because their order corresponds to the strings created earlier. 
                                             # which() tells us the index values of the lower triangular andcorrespond 
                                             # to the order of the strings. m[] then outputs the values in that order
      
      new_df <- data.frame(pairs, correlation)   # Putting together the pieces for the new data frame
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
