#' @title Outlier Analysis & Correlation Analysis
#'
#' @description It does outlier bounds and count detection with and without log transformations. And correlation strength between continous variables.
#'
#' @param (data,variables=c(1:ncol(data)),log=FALSE)
#'
#' @return
#'
#' @examples eda_variable(cars,c(1:5),log=TRUE"), eda_corr(cars,log=FALSE)
#'
#' @export eda_variable,eda_corr

eda_variable <- function(data,variables=c(1:ncol(data)),log=FALSE)
{
  if (!is.data.frame(data))
    stop("The given object is not a dataframe")

  print('------------------------------------------------------')
  print("Missing Values Analysis")
  print('------------------------------------------------------')
  cat("\n")
  print(paste(sum(is.na(data)),'is the total number of missing values'))
  cat("\n")
  print('------------------------------------------------------')
  print("Variable-wise Missing Value Percentage")
  print('------------------------------------------------------')
  cat("\n")
  print(apply(data,2,function(x) round(sum(is.na(x))/length(x)*100,3)))
  cat("\n")
  print('------------------------------------------------------')
  print("Categorical Variables")
  print('------------------------------------------------------')
  cat("\n")
  data1 <- data[complete.cases(data),]
  Upper_Bound <- c()
  Lower_Bound <- c()
  Upper_Count <- c()
  Lower_Count <- c()
  Column_Name <- c()

  for (x in variables)
  {

    if(!is.numeric(data[,x]) | (length(unique(data[,x]))/length(data) < 10))
    {
      print(paste(names(data)[x],' is categorical variable contains',paste(length(unique(data[,x]))),'levels'))
    }
    else if(is.numeric(data[,x]))
    {
      if (log == TRUE)
      {
        Upper_Bound[x]=min(max(log(data1[,x]+1)),quantile(log(data1[,x]+1),.75)+1.5*IQR(log(data1[,x]+1)))
        Lower_Bound[x]=max(min(log(data1[,x]+1)),quantile(log(data1[,x]+1),.25)-1.5*IQR(log(data1[,x]+1)))
        Column_Name[x]=colnames(data1[x])
        Upper_Count[x]=length(which(log(data1[,x]+1)>Upper_Bound[x]))
        Lower_Count[x]=length(which(log(data1[,x]+1)<Lower_Bound[x]))
      }
      else
      {
        Upper_Bound[x]=min(max(data1[,x]),quantile(data1[,x],.75)+1.5*IQR(data1[,x]))
        Lower_Bound[x]=max(min(data1[,x]),quantile(data1[,x],.25)-1.5*IQR(data1[,x]))
        Column_Name[x]=colnames(data1[x])
        Upper_Count[x]=length(which(data1[,x]>Upper_Bound[x]))
        Lower_Count[x]=length(which(data1[,x]<Lower_Bound[x]))
      }
    }
  }
  cat("\n")
  print('------------------------------------------------------')
  print("Outlier Analysis for Numerical Variables")
  print('------------------------------------------------------')
  cat("\n")
  df <- data.frame(Column_Name,Lower_Bound,Upper_Bound,Lower_Count,Upper_Count)
  df1 <- df[-which(is.na(df$Column_Name)), ]
  df1
}

#Function to find correlation between numerical varialbles

eda_corr <- function(data,log=FALSE)
{
  if (!is.data.frame(data))
  stop("The given object is not a dataframe")

  print('------------------------------------------------------')
  print("Correlation Matix for Numeric Variables")
  print('------------------------------------------------------')
  cat("\n")
  y <- c()

  for (i in 1:ncol(data))
  {
    if(!is.numeric(data[,i]) | (length(unique(data[,i]))/length(data) < 10))
    {
      l <- length(y)
      y[l+1] <- i
    }
  }

  if (log == FALSE)
  {
    df <- data[ -y ]
    print(cor(df))
  }
  else if (log == TRUE)
  {
    df <- data[ -y ]
    print(cor(log(df+1)))
  }
  pairs(df)
}
