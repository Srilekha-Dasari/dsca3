#give the file path and save the file in project folder
fpath <- file.path('hourly_irish_weather.csv')
#To import data in csv format
weather_data <- read.csv(fpath, header=TRUE, stringsAsFactors = FALSE, na.strings="" )
#data type of this file
class(weather_data)
#structure of weather_data data frame
str(weather_data)
# Number of rows in the data frame
nrow(weather_data)
#column names of the data frame
colnames(weather_data)
# Renaming the column named with X to identifier
names(weather_data)[names(weather_data) == 'X'] <- 'identifier'
#after renaming check the column names are updated or not
colnames(weather_data)
# time and date splitting
weather_data$Date <- as.Date(weather_data$date) 
weather_data$Time <- format(as.POSIXct(weather_data$date) ,format = "%H:%M:%S") 
head(weather_data)
colnames(weather_data)
#removing a column which is of no use 
weather_data$date <- NULL
#----Dealing with missing data---------
#missing values
sum(is.na(weather_data))
#total number of missing values for each column
colSums(is.na(weather_data))

#graphically display missing data
install.packages("mice")
library(mice)
md.pattern(weather_data)
#using vim packages to display the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(weather_data, prop = FALSE, numbers = TRUE)
# missing values summary
summary(missing_values)
#using subset of galway data
galway_df <- subset(weather_data, county == 'Galway')
#structure of galway_df
str(galway_df)
#number of rows in galway_df
nrow(galway_df)
#displays columns which do not have na values 
colSums(!is.na(galway_df))
#colSums(is.na(galway_df))
#grouping year and month
library(dplyr)
galway_df %>%
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = mean(temp))
# Get only the years
galway_df$year<-format(galway_df$Date,"%Y")
galway_df$year
#month in number
galway_df$mn <- format(galway_df$Date,'%m')
galway_df$mn
# Only months are extracted
galway_df$month<-format(galway_df$Date,'%B') 
galway_df$month
#only days are extracted
galway_df$day <- as.numeric(format(galway_df$Date, "%d")) 
galway_df$day
#checking the new columns in the data frame
colnames(galway_df)
# View the records with NA in the temp column
which (is.na(galway_df$temp))
# Display the Missing data of all galway_data columns
colSums(is.na(galway_df))
# Alternative data to find missing data
na_records <- galway_df[!complete.cases(galway_df$temp),]
na_records
nrow(na_records)
galway_df$temp
#calculating the mean value of temperature
mean(as.numeric(galway_df$temp),na.rm=TRUE)
#filling missing values of temp column in the galway_df
galway_df$temp<- ifelse(is.na(galway_df$temp), mean(galway_df$temp, na.rm=TRUE), galway_df$temp)
galway_df$temp
# filling missing values of column wetb
galway_df$wetb<- ifelse(is.na(galway_df$wetb), mean(galway_df$wetb, na.rm=TRUE), galway_df$wetb)
galway_df$wetb
# filling missing values of column dewpt
galway_df$dewpt<- ifelse(is.na(galway_df$dewpt), mean(galway_df$dewpt, na.rm=TRUE), galway_df$dewpt)
galway_df$dewpt
# filling missing values of column vappr
galway_df$vappr<- ifelse(is.na(galway_df$vappr), mean(galway_df$vappr, na.rm=TRUE), galway_df$vappr)
galway_df$vappr
#filling missing values of column rhum
galway_df$rhum<- ifelse(is.na(galway_df$rhum), mean(galway_df$rhum, na.rm=TRUE), galway_df$rhum)
galway_df$rhum
# filling missing values of column msl
galway_df$msl<- ifelse(is.na(galway_df$msl), mean(galway_df$msl, na.rm=TRUE), galway_df$msl)
galway_df$msl
# filling missing values of column wdsp
galway_df$wdsp<- ifelse(is.na(galway_df$wdsp), mean(galway_df$wdsp, na.rm=TRUE), galway_df$wdsp)
galway_df$wdsp
# filling missing values of column wddir
galway_df$wddir<- ifelse(is.na(galway_df$wddir), mean(galway_df$wddir, na.rm=TRUE), galway_df$wddir)
galway_df$wddir
#filling missing values of column rain 
galway_df$rain<- ifelse(is.na(galway_df$rain), mean(galway_df$rain, na.rm=TRUE), galway_df$rain)
galway_df$rain
#checking the temp column missing data after filling
which (is.na(galway_df$temp))
#for correlation, susbset the numerical data 
n_data1 <- subset(galway_df, select=c(rain,temp,wetb,dewpt,vappr,rhum,msl,wdsp,wddir))
colnames(n_data1)
str(n_data1)
# finding correlation
opar <- par(no.readonly = TRUE)
install.packages("corrplot")
library(corrplot)
corrplot(corr = cor(n_data1, use = "na.or.complete"),tl.col = "Black", tl.cex = 0.9)

#Augmented Dickey-Fuller Test
install.packages("tseries")
library(tseries)
adf.test(gdf1$temp) 

#dropping columns w, ww, sun, vis, clht, clamt which has no data to capture
galway_df[,c("w","ww","sun","vis","clht","clamt")] <- list(NULL)
colnames(galway_df)
#write it to a csv file and test it
write.csv(galway_df,"galwaydata.csv", row.names = FALSE)
fpath2 <- file.path("galwaydata.csv")
gdf1 <- read.csv(fpath2, header=TRUE, stringsAsFactors = FALSE, na.strings="" )
#make a copy of this data frame
gdf1 <- galway_df
colnames(gdf1)

#plotting graphs
library(ggplot2)

opar <- par(no.readonly = TRUE)
#par(mfrow = c(3, 1))
#grid <- matrix(c(1,1,1,1), nrow=2, ncol=2, byrow= TRUE)
#grid
#layout(grid)
#plot function to dispaly year wise temperature
plot(x = gdf1$year, y = gdf1$temp,main = "Galway: Year-wise Temperatures",
     ylab = "temperature", xlab = "year", col = "purple")
#plot function to dispaly month wise temperature
plot(x = gdf1$mn, y = gdf1$temp, main = "Galway: Monthly Temperatures",
     ylab = "temperature", xlab = "month", col = "green")
#plot function to dispaly month wise temperature
plot(x = gdf1$day, y = gdf1$temp, main = "Galway: daily Temperatures",
     ylab = "temperature", xlab = "month", col = "orange")

#plotting histogram for the temperature variable
hist(gdf1$temp, main = "Histogram for temp", 
     xlab = "Temperature", col = "red")
#plotting histogram for the year variable
gdf1$year <- as.numeric(gdf1$year)
#str(gdf1)
hist(gdf1$year, main = "Histogram for year", 
     xlab = " year", col = "red")
#box plot
library(ggplot2)
bxp <- ggboxplot(gdf1, x = "year", y = "temp", add = "point")
bxp
#plotting monthly and yearly weather data
require(ggplot2)
qplot(day, temp, data = gdf1, geom = "line", col = "red") +
  facet_grid(year ~ month) + theme_bw()
# have to do
#install.packages("ggplot2")
# Colour each chart point witha colour palette
#install.packages("viridis")
library(ggplot2)
library(viridis)
plot <- ggplot(gdf1, aes(x = year, y = temp, color = "White"))
plot <- plot + geom_point() + scale_color_viridis()
print((plot))
# subset numeric data
n_data <- subset(gdf1, select=c(rain,temp,wetb,dewpt,vappr,rhum,msl,wdsp,wddir))
colnames(n_data)
str(n_data)

#corr
#filter(c[(c > 0.5 | c < -0.5)])




# To find whether it is +ve ,-ve, or zero correlation
lag.plot(gdf1$temp)
install.packages("TSA")
library(TSA)
acf(gdf1$temp, lag.max = 30, type = c("correlation", "covariance", "partial")[1],
    plot = TRUE, na.action = na.fail, demean = TRUE, drop.lag.0 = TRUE)
pacf(df$temp, lag.max = 30, type = c("correlation", "covariance", "partial")[1],
     plot = TRUE, na.action = na.fail, demean = TRUE, drop.lag.0 = TRUE)
pacf (df$temp, lag = 30, pl = TRUE)
colnames(df)
which(is.na(df$Date))
which(is.na(df$temp))
#pca 
data_numeric_variables <- sapply(n_data1, is.numeric)
data_numeric_variables
pca <- prcomp(n_data1, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)
# Eigenvalues / Variances
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values
 
#scree plot is an alternative approach to find and plot eigen values from large to small
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50), barcolor = "Yellow", barfill = "green")

#get_pca_var() provides a list of matrices containing all the results 
# for the active variables (coordinates, correlation between variables 
#and axes, squared cosine and contributions)
pca_for_variables <- get_pca_var(pca)
pca_for_variables

# The components of the get_pca_var() can be used in the plot of variables as follow:

# -----------------------------------------------------------------------
# Using Correlation plot
# -----------------------------------------------------------------------
library("corrplot")
#plot the squared standard deviations variances
corrplot(pca_for_variables$cos2, is.corr = FALSE)

# A variable correlation plots shows the relationships between 
# all variables.
fviz_pca_var(pca, col.var = "black")

# -----------------------------------------------------------------------
# Cos2 - quality of representation
# -----------------------------------------------------------------------
# The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates). 
# We can access to the cos2 as follows:
head(pca_for_variables$cos2, 10)

# show a bar plot of variables cos2 using the function fviz_cos2()
#cos2 of variables on Dim.1 and Dim.2
library(factoextra)
fviz_cos2(pca, choice = "var", axes = 1:2)
# -----------------------------------------------------------------------
# Biplot
# -----------------------------------------------------------------------
# Colour by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  

# Contribution of variables to each PC.
#if its value is larger then it contributes more to the component.
head(pca_for_variables$contrib, 20)

# The most important (or, contributing) variables can be highlighted 
#on the correlation plot
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green")
)
# We can use the function fviz_contrib() from the factoextra package
# to draw a bar plot of variable contributions. If your data 
# contains many variables, you can decide to show only the top 
# contributing variables. This code shows the top 20 variables 
# contributing to the principal components:
# Contributions of variables to PC1
install.packages("factoextra")
library(factoextra)
?fviz_contrib
fviz_contrib(pca, choice = "var", axes = 1, top = 20)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)
# Contribution to PC1 - PC4
fviz_contrib(pca, choice = "var", axes = 1:4, top = 20)
# The red dashed line on the graphs indicate the expected 
# average contribution.
fviz_pca_ind(pca,
             axes = c(1, 2),
             geom.ind = "point", # show points only (but not "text values")
             col.ind = gdf1$temp, # colour by groups
             palette = c("Red", "Green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Vote"
)
# Graphical parameters
# We can change the graphical parameters using the function ggpar() from the ggpubr package.

biplot <- fviz_pca_ind(pca, geom = "point", col.ind = gdf1$temp)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Brexit dataset",
              caption = "Source: BBC",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

# Lets see how PC 3 and PC 4 represent voters data.
biplot <- fviz_pca_ind(pca, 
                       axes = c(3, 4),
                       geom = "point", 
                       col.ind = data_file$Vote)
ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Brexit dataset",
              caption = "Source: BBC",
              xlab = "PC 3", ylab = "PC 4",
              legend.title = "Vote", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

#seasonal_decomposition
n2 <- subset(gdf1,select = c("Date","temp"))
str(n2)
colSums(is.na(n2))
class(n2)
dsd <- ts(n2$temp, frequency = 30 * 12)
class(dsd)
str(dsd)
str(n2$Date)
str(n2$temp)
plot(dsd)
cd <- decompose(dsd, type = c("additive"), filter = NULL)
plot(cd)
which(is.na(dsd))
colnames(df)
colSums(is.na(n_data1))
colnames(gdf1)


# test and statistical method
#normality test by visual inspection
library("ggpubr")
# temp
ggqqplot(gdf1$temp, ylab = "temperature")
# year
ggqqplot(gdf1$year, ylab = "year")
#spearman rank correlation coefficient test
result <- cor.test(gdf1$year, gdf1$year, 
                method = "spearman")
result
#power analysis
install.packages("pwr")
library(pwr)

power_analysis <- pwr.r.test(r=.25,sig.level=0.02, power=.80)

power_analysis
