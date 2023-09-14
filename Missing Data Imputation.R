library(psych)
library(magrittr)
library(missForest)
library(doParallel)
library(Amelia)
library(mice)
library(gapminder)


# Trial Data sets
Starting <- iris
#Starting <- bfi[1:1000,]
#Starting <- gapminder[1:1000,]

# Takes only the complete cases for this tutorial
# This is literally all of the code for list-wise deletion
Complete <- Starting[complete.cases(Starting),]

# Randomly makes 20% of the data missing for imputation
Data <- prodNA(Complete, noNA = 0.2)

# Determines the type of vector that is given to it.
Detect_Type <- function(String){
  
  # If the vector contains a single unchanging value it is labelled invariant
  if(length(table(String)) == 1) {return("Invariant")}
  
  # If the vector is numeric it is labelled as such
  if(is.numeric(String) == TRUE) {return("Numeric")}
  
  # If the vector is categorical it is labelled appropriately
  if(is.numeric(String) != TRUE){
    ifelse(length(table(String)) == 2,
    return("Binary"),return("Multicategorical"))
  }
  
} # End of function

# Demonstration of what it does
Detect_Type(Data[,1])

# Similar although less useful answer(s) without custom function (Especially Gapminder)
class(Data[,1])

# Determines the percent missingness in a vector
Detect_Missingness <- function(String){
  
  # Forces lists to be a matrix
  String = as.matrix(String)
  
  # Percent missing values
  Percent_Missing = mean(is.na(String))*100
  
  #returns this percentage
  return(Percent_Missing)
  
} # End of function


# Generates meta data about a data set given to it.
Meta_Data <- function(data, Show_Plot = TRUE){
  
  # Ensures matrices are converted to data frames.
  data = data.frame(data)
  
  # Initializes a progress bar for the meta data calculations
  print("Meta Data Generation Progress Bar")
  pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)
  
  # Creates an empty data frame and assigns column names
  Meta_Data <- data.frame(matrix(NA, nrow = ncol(data), ncol = 9))
  names(Meta_Data) <- c("Column Name", "Data Type", "Percent Missing", 
        "Mean", "Median", "Var.", "Std. Dev.", "N Categories","Mode")

  # Loops through each column and generates the meta data
  for(i in 1:ncol(data)){ # Start of loop
    
    # Adds the column name, type, and missingness
    Meta_Data[i,1] <- names(data)[i]
    Meta_Data[i,2] <- Detect_Type(data[,i])
    Meta_Data[i,3] <- Detect_Missingness(data[,i])
    
    # If the data is numeric it gives information about it
    if(Meta_Data[i,2] == "Numeric") {
      Meta_Data[i,4] <- mean(data[,i], na.rm = TRUE)
      Meta_Data[i,5] <- median(data[,i], na.rm = TRUE)
      Meta_Data[i,6] <- var(data[,i], na.rm = TRUE)
      Meta_Data[i,7] <- sd(data[,i], na.rm = TRUE)}
    
    # If the data is categorical it gives the number of categories and mode
    if(Meta_Data[i,2] == "Binary" | Meta_Data[i,2] == "Multicategorical"){
      Meta_Data[i,8] <- length(table(data[,i]))
      Meta_Data[i,9] <- names(table(data[,i])[which.max(table(data[,i]))])}
    
    # changes the progress bar
    setTxtProgressBar(pb,i)
    
  } # End of loop
  
  # Creates the optional Missingness bar plot
  if(Show_Plot == TRUE) {barplot(Meta_Data$`Percent Missing`, 
    names.arg = Meta_Data$`Column Name`, 
    ylim = c(0,max(ceiling(Meta_Data$`Percent Missing`))), 
    main = "Missingness Plot", xlab = "Column names", ylab = "Percent Missing")}

  # Returns the data frame as the output of the function  
  return(Meta_Data)
  
  # Ends the progress bar
  dev.off()
  
} # End of function

# Assigns the meta data variable and prints it to console
(M_Data <- Meta_Data(data = Data))


# Creates a binary matrix that represent missing values
Missing_Matrix <- function(data){
  
  # An empty matrix for the nested loops below to fill in
  Missing_Matrix <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  
  # Initializes the progress bar for the calculation below
  print("Missing Matrix Generation Progress Bar")
  pb <- txtProgressBar(min = 0, max = nrow(data)*ncol(data), style = 3)
  
  # A counter to track the progress of the missing data
  counter <- 0
  
  # Searches for missing data and adds a 1 in the missing data matrix
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      if(is.na(data[i,j]) == TRUE){Missing_Matrix[i,j] = 1} 
      setTxtProgressBar(pb,counter) # For progress bar
      counter = counter + 1 # For progress bar
    }
  }
  
  # Returns the missing matrix
  return(Missing_Matrix)
  
  # Ends the progress bar
  dev.off()
  
} # End of function

# Assigns the missingness matrix as M_Mat
M_Mat <- Missing_Matrix(Data)


# Function for comparing real data and imputed data
Imputation_RMSE <- function(Imputed_Set, Original_Set, Missing_Mat){
  
  # Forces the data into a data frame
  Original_Set = data.frame(Original_Set)
  
  # Initializes variables and the progress bar
  SE <- 0; Counter <- 0; Imp <- Imputed_Set; Runs <- 0
  print("Imputation RMSE Calculation Progress Bar")
  pb <- txtProgressBar(min = 0, max = nrow(Imp)*ncol(Imp), style = 3)
  
  # Loops through the data sets and calculates the difference between
  # actual and imputed values using the missing matrix as a guide
  for(i in 1:nrow(Imputed_Set)){
    for(j in 1:ncol(Imputed_Set)){
      setTxtProgressBar(pb,Runs) # For progress bar
      Runs = Runs + 1 # For progress bar
      if(Missing_Mat[i,j] == 1){ # Where the missing mat is used
        if(is.numeric(Original_Set[i,j]) == TRUE){ # Checks that it is numeric data
            SE = SE + (Imputed_Set[i,j] - Original_Set[i,j])^2 # Sum of Squared differences
            Counter = Counter + 1 # Counter used for averaging below
        }
      }
    }
  }
  
  # Steps to create and return the desired RMSE value
  MSE <- SE/Counter; RMSE <- MSE^(1/2); return(RMSE)
  
  # Ends the progress bar
  dev.off()
  
} # End of function

# Demonstration where every imputed value is the number 1, high RMSE...
Imputation_RMSE(Imputed_Set = matrix(1,nrow = nrow(Complete),ncol = ncol(Complete)), 
          Original_Set = Complete, Missing_Mat = M_Mat)


# Mean imputation function that uses the pre-made meta data and missing mat functions.
Mean_Imputation <- function(data, Meta_Dat = NA, Missing_Mat = NA){
  
  # Forces the data into a data frame
  data = data.frame(data)
  
  # Turns character columns into factor columns
  for(i in 1:ncol(data)){
    if(is.character(data[,i]) == TRUE){
      data[,i] = factor(data[,i])
    }
  }
  
  # Generates Meta data if a two dimensional matrix is not provided.
  if(length(dim(Meta_Dat)) != 2){
  Meta_Dat <- Meta_Data(data, Show_Plot = FALSE)}
  
  # Generates the Missing Data Matrix if it is not provided.
  if(length(dim(Missing_Mat)) != 2){
  Missing_Mat <- Missing_Matrix(data)}
  
  # Initialing the progress bar and counter for the progress bar
  print("Mean Imputation Progress Bar")
  pb <- txtProgressBar(min = 0, max = nrow(data)*ncol(data), style = 3)
  Counter = 0
  
  # The Mean imputation loops, if categorical it uses mode imputation
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      setTxtProgressBar(pb,Counter) # For progress bar
      Counter = Counter + 1 # For progress bar
      if(Missing_Mat[i,j] == 1){
        if(is.numeric(data[i,j]) == TRUE){data[i,j] = Meta_Dat$Mean[j]}
        if(is.numeric(data[i,j]) == FALSE){data[i,j] = Meta_Dat$Mode[j]}
      }
    }
  }
  
  # Returns the imputed data
  return(data)
  
  # Ends the progress bar
  dev.off()
  
} # End of function

# The mean imputed data set assigned to "Mean_Test"
Mean_Test <- Mean_Imputation(Data)

# The RMSE value of actual versus imputed values
Mean_Test_RMSE <- Imputation_RMSE(Imputed_Set = Mean_Test, Original_Set = Complete, Missing_Mat = M_Mat)


# Median Imputation function
Median_Imputation <- function(data, Meta_Dat = NA, Missing_Mat = NA){
  
  # Forces the data into a data frame
  data = data.frame(data)
  
  # Turns character columns into factor columns
  for(i in 1:ncol(data)){
    if(is.character(data[,i]) == TRUE){
      data[,i] = factor(data[,i])
    }
  }
  
  # Generates Meta data if a two dimensional matrix is not provided.
  if(length(dim(Meta_Dat)) != 2){
    Meta_Dat <- Meta_Data(data, Show_Plot = FALSE)}
  
  # Generates the Missing Data Matrix if it is not provided.
  if(length(dim(Missing_Mat)) != 2){
    Missing_Mat <- Missing_Matrix(data)}
  
  # Initialing the progress bar and counter for the progress bar
  print("Median Imputation Progress Bar")
  pb <- txtProgressBar(min = 0, max = nrow(data)*ncol(data), style = 3)
  Counter = 0
  
  # The Mean imputation loops, if categorical it uses mode imputation
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      setTxtProgressBar(pb,Counter) # For progress bar
      Counter = Counter + 1 # For progress bar
      if(Missing_Mat[i,j] == 1){
        if(is.numeric(data[i,j]) == TRUE){data[i,j] = Meta_Dat$Median[j]}
        if(is.numeric(data[i,j]) == FALSE){data[i,j] = Meta_Dat$Mode[j]}
      }
    }
  }
  
  # Returns the imputed data frame
  return(data)
  
  # Ends the progress bar
  dev.off()
  
} # End of function

# Assigns the imputed data to "Median_Test"
Median_Test <- Median_Imputation(Data)

# Calculates the RMSE for Median imputation
Median_Test_RMSE <- Imputation_RMSE(Imputed_Set = Median_Test, Original_Set = Complete, Missing_Mat = M_Mat)


# Amelia based data imputation
Amelia_Imputation <- function(Unimputed_Data, Replicates = 50, Rounding = 8, Cores = 4){
  
  # Forces the data into a data frame
  Unimputed_Data = data.frame(Unimputed_Data)
  
  # Generates the meta data
  Meta <- Meta_Data(Unimputed_Data, Show_Plot = FALSE)
  
  # Turns character columns into factor columns
  for(i in 1:ncol(Unimputed_Data)){
    if(is.character(Unimputed_Data[,i]) == TRUE){
      Unimputed_Data[,i] = factor(Unimputed_Data[,i])
    }
  }
  
  # Creates a vector of the categorical variables, if none it will be null
  Cat_var <- Meta$`Column Name`[Meta$`Data Type` != "Numeric" & Meta$`N Categories` <= 9]
  if(length(Cat_var) == 0){Cat_var = NULL}
  
  # Creates a vector for ID variables, or variables with too many categories
  # In this case the values are imputed as Unknown instead of giving a value from the data set
  ID_var <- Meta$`Column Name`[Meta$`Data Type` != "Numeric" & Meta$`N Categories` > 9]
  for(i in ID_var){
    levels(Unimputed_Data[,i]) = c(levels(Unimputed_Data[,i]),"Unknown")
    for(j in 1:length(Unimputed_Data[,i])){
      if(is.na(Unimputed_Data[j,i]) == TRUE){
      Unimputed_Data[j,i] = "Unknown"
      }
    }
  }
  if(length(ID_var) == 0){ID_var = NULL}
  
  # Model Output, the "noms" argument treats the categorical variables as nominal.
  # If you are not running windows, you can switch to "multicore" for the "parallel" argument in linux...
  # you may need to remove the "parallel" and "ncpus" arguments entirely if you operating system is not compatible.
  Amelia_Output <- amelia(Unimputed_Data, m = Replicates, p2s = 0, idvars = ID_var,
                          parallel = "snow", ncpus = Cores, noms = Cat_var)
  
  # An empty matrix to fill with the average of the imputed data sets
  Imputation_Mat <- matrix(0, nrow = nrow(Unimputed_Data), ncol = ncol(Unimputed_Data))
  
  # Filling the matrix with these averaged data sets
  for(i in 1:Replicates){
    for(j in 1:nrow(Unimputed_Data)){
      for(k in 1:ncol(Unimputed_Data)){
        Imputation_Mat[j,k] <- Imputation_Mat[j,k] + as.numeric(Amelia_Output$imputations[[i]][j,k])/Replicates
      }
    }
  }
  
  # Turning the Output back into a data frame with the original names.
  Imputation_Mat <- data.frame(Imputation_Mat) 
  names(Imputation_Mat) <- names(Unimputed_Data) 
  
  # Handles rounding for numeric data, and creates factors for the categories
  for(i in 1:ncol(Imputation_Mat)){
    if(is.numeric(Unimputed_Data[,i]) == TRUE){
      Imputation_Mat[,i] = round(Imputation_Mat[,i],Rounding)
    }
    if(is.numeric(Unimputed_Data[,i]) == FALSE){
      Imputation_Mat[,i] = factor(names(table(Unimputed_Data[,i]))[round(Imputation_Mat[,i],0)])
    }
  }

  # Returning the Imputed data
  return(Imputation_Mat)
  
} # End of function

# The Amelia imputed data set assigned to "Amelia_Test"
Amelia_Test <- Amelia_Imputation(Data)

# The RMSE of the imputed data versus real data
Amelia_Test_RMSE <- Imputation_RMSE(Imputed_Set = Amelia_Test, Original_Set = Complete, Missing_Mat = M_Mat)


# Mice based data imputation with "cart" the default method
# Use help(mice) to learn about the possible imputation methods
Mice_Imputation <- function(data, N = 10, Iterations = 10, Rounding = 8,
                     Jitter = 0, Verbose = TRUE, guess = NULL, type = "cart"){
  
  # Forces the data into a data frame
  data = data.frame(data)

  # Turns character columns into factor columns
  for(i in 1:ncol(data)){
    if(is.character(data[,i]) == TRUE){
      data[,i] = factor(data[,i])
    }
  }
  
  # Sometimes Mice detects too much collinearity, and adding jitter can prevent the error
  if(Jitter > 0){
   for(i in 1:nrow(data)){
     for(j in 1:ncol(data)){
      if(is.numeric(data[i,j]) == TRUE){
        data[i,j] = data[i,j] + rnorm(1,sd = Jitter)
      }
     } 
    }
   }

  # The output of the mice function, where each data set is an element in a list
  Mice_Out <- complete(mice(data, m = N, maxit = Iterations, data.init = guess,
                                print = Verbose, method = type), action = "all")

  # An empty matrix to fill with the average of the imputed data sets
  Imputation_Mat <- matrix(0, nrow = nrow(data), ncol = ncol(data))

  # Filling the matrix with these averaged data sets
  for(i in 1:N){
    for(j in 1:nrow(data)){
      for(k in 1:ncol(data)){
          Imputation_Mat[j,k] <- Imputation_Mat[j,k] + as.numeric(Mice_Out[[i]][j,k])/N
      }
    }
  }

  # Turning the Output back into a data frame with the original names.
  Imputation_Mat <- data.frame(Imputation_Mat) 
  names(Imputation_Mat) <- names(data) 

  # Handles rounding for numeric data, and creates factors for the categories
  for(i in 1:ncol(Imputation_Mat)){
    if(is.numeric(data[,i]) == TRUE){
      Imputation_Mat[,i] = round(Imputation_Mat[,i],Rounding)
    }
    if(is.numeric(data[,i]) == FALSE){
      Imputation_Mat[,i] = factor(names(table(data[,i]))[round(Imputation_Mat[,i],0)])
    }
  }

  # Returns the averaged data set
  return(Imputation_Mat)

} # End of function

# The averaged imputed data set using the "pmm" method
Mice_Test_Output <- Mice_Imputation(Data, type = "cart")

# The resulting RMSE
Mice_Test_RMSE <- Imputation_RMSE(Imputed_Set = Mice_Test_Output, 
                                  Original_Set = Complete, Missing_Mat = M_Mat)


# Random Forest Missing data Imputation
Forest_Imputation <- function(Forest_Data, Meta_Dat = NA, Replace_ID = TRUE, Cores = 4){

  # Tells the missing forest calculation to use 4 cores
  registerDoParallel(cores=Cores)
  
  # Generates Meta data if a two dimensional matrix is not provided.
  if(length(dim(Meta_Dat)) != 2){
   Meta_Dat <- Meta_Data(Forest_Data, Show_Plot = FALSE)}

  # Turns character columns into factor columns
  for(i in 1:ncol(Forest_Data)){
    if(is.character(Forest_Data[,i]) == TRUE){
      Forest_Data[,i] = factor(Forest_Data[,i])
    }
  }

  # Forces the data into a data frame
  Forest_Data = data.frame(Forest_Data)

  # Removes ID variables that prevent the random forest model from running
  N_Levels = Meta_Dat$`N Categories`
  for(i in 1:length(N_Levels)){if(is.na(N_Levels[i]) == TRUE){N_Levels[i] = 0}}
  included_Columns = Meta_Dat$`Column Name`[N_Levels <= 50]
  if(length(included_Columns) != length(names(Forest_Data))){
    print("Warning: One or more ID column(s) have been identified and may be removed!")}
  Data = Forest_Data[,included_Columns]
  
  # If a single ID variable is removed, Replace_ID will add it back as the left most column with Unknowns
  if(Replace_ID == TRUE & (length(included_Columns)+1) == length(names(Forest_Data))){
    ID_Var = Forest_Data[,Meta_Dat$`Column Name`[N_Levels > 50]]
    levels(ID_Var) = c(levels(ID_Var),"Unknown")
    for(i in 1:length(ID_Var)){if(is.na(ID_Var[i])==TRUE){ID_Var[i]="Unknown"}}
  }

  # The output of the missing forest algorithm, with the output being a 2 element list
  Missing_For_Out <- missForest(Data, verbose = TRUE, parallelize = "forests", ntree = 250)

  # The first element in the previous list is the imputed data set, so we just select that.
  Forest_Out <- Missing_For_Out[[1]]

  # Returns the imputed ID variable if "Replace_ID" is TRUE and there is only one ID variable.
  if(Replace_ID == TRUE & (length(included_Columns)+1) == length(names(Forest_Data))){return(cbind(ID_Var,Forest_Out))}
  if(Replace_ID == FALSE | (length(included_Columns)+1) != length(names(Forest_Data))){return(Forest_Out)}
}

# The random forest imputed data set is assigned to "Forest_Test"
Forest_Test <- Forest_Imputation(Forest_Data = Data)

# The RMSE is calculated for random forest imputation
Forest_Test_RMSE <- Imputation_RMSE(Imputed_Set = Forest_Test, Original_Set = Complete, Missing_Mat = M_Mat)






