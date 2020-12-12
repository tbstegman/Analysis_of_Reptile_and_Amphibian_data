
################################################################################
################################################################################
#           Load the libraries that will be used in this analysis              #
################################################################################
################################################################################

# Load the tidyverse package. 
# This will be used for data transformations and plotting.
# install.packages("tidyverse")
library(tidyverse)
citation("tidyverse")

# Load the naniar package.  
# This will be used to analyze missing data.
# install.packages("naniar")
library(naniar)
citation("naniar")

# Load the lubridate package.
# This will be used for working with dates.
# install.packages("lubridate")
library(lubridate)
citation("lubridate")

# Load the sqldf package.
# This will bve used for joining data sets.
# install.packages("sqldf")
library(sqldf)
citation("sqldf")

# Load gridExtra package.
# This will be used to arrange plots.
# install.packages("gridExtra")
library(gridExtra)
citation("gridExtra")

# Load the sqldf package.
# This will be used to create map scatter plots.
# install.packages("ggmap")
library(ggmap)
citation("ggmap")

# Load the randomForest package.
# This will be used for classification.
# install.packages("randomForest")
library(randomForest)
citation("randomForest")

################################################################################
################################################################################
#                        Read in the raw data sets                             #
################################################################################
################################################################################

# Store the GitHub repository URL in a variable
repo_url <- "https://raw.githubusercontent.com/tbstegman/Analysis_of_Reptile_and_Amphibian_data/main/"

# Read in the iNaturalist csv file.
inaturalist_raw <- read.csv(paste0(repo_url,"GBIF_iNaturalist.csv"))

# Read in the common name groupings csv file.
groupings <- read.csv(paste0(repo_url,"common_name_groupings.csv"))

# Read in the NOAA csv files.
noaa_jan15_dec16 <- read.csv(paste0(repo_url,"NOAA_jan15_through_dec16.csv"))
noaa_jan17_dec18 <- read.csv(paste0(repo_url,"NOAA_jan17_through_dec18.csv"))
noaa_jan19_oct20 <- read.csv(paste0(repo_url,"NOAA_jan19_through_oct20.csv"))

# Combine the NOAA data sets.
noaa_raw <- rbind(noaa_jan15_dec16,
                  noaa_jan17_dec18,
                  noaa_jan19_oct20)


################################################################################
################################################################################
#    Perform transformations of the raw data and do basic data exploration     #
################################################################################
################################################################################


#######################################################
#    Transformations for the inaturalist data set     #
#######################################################

# Look at the structure and content of the data set.
glimpse(inaturalist_raw)
View(inaturalist_raw)

# Check to make sure there are no duplicate IDs (the two lengths below should be the same).
length(inaturalist_raw$gbifid) == length(unique(inaturalist_raw$gbifid))

# Set variable names to lower case.
names(inaturalist_raw) <- tolower(names(inaturalist_raw))

# Create a POSIXct variable from the character eventdate variable.
inaturalist_raw$event_dt_tm <- as.POSIXct(strptime(inaturalist_raw$eventdate,"%Y-%m-%dT%H:%M:%S"))
class(inaturalist_raw$event_dt_tm)

# Extract the hour.
inaturalist_raw$hour <- hour(inaturalist_raw$event_dt_tm)

# Extract the date from the event_dt_tm variable.
inaturalist_raw$event_dt <- date(inaturalist_raw$event_dt_tm)
class(inaturalist_raw$event_dt)

# Cast hour and month as factor variables.
inaturalist_raw$month <- as.factor(inaturalist_raw$month)
inaturalist_raw$hour <- as.factor(inaturalist_raw$hour)

# Select variables, filter for time period of interest, and sort.
inaturalist <- inaturalist_raw %>% 
  select(family, decimallatitude, decimallongitude, 
         month, hour, event_dt, event_dt_tm) %>% 
  filter(event_dt >= "2015-10-01" & event_dt <= "2020-09-30") %>% 
  arrange(event_dt_tm)

# Look at missing variable summary using the naniar package.
miss_var_summary(inaturalist) %>% View()

# Take a quick look at the transformed data set.
glimpse(inaturalist)

#######################################################
#    Transformations for the NOAA data set           #
#######################################################

# Look at the structure and content of the data set.
glimpse(noaa_raw)
View(noaa_raw)

# Set variable names to lower case.
names(noaa_raw) <- tolower(names(noaa_raw))

# Convert the character date variable to a date.
noaa_raw$date <- as.Date(noaa_raw$date)

# Select the variables of interest.
noaa_raw <- noaa_raw %>% 
  select(date,station,awnd,prcp,tavg,tmax,tmin)

# Look at missing variable summary by weather station.
noaa_raw %>% 
  group_by(station) %>% 
  miss_var_summary() %>% 
  View()

# Compute the median weather statistics by date and exclude NAs.
noaa <- noaa_raw %>% 
  group_by(date) %>% 
  summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE))) %>% 
  arrange(date)

# Check for missing values again.
miss_var_summary(noaa)

# Check to make sure there are no gaps in dates (the sum should be 0).
sum(!(seq.Date(min(noaa$date),max(noaa$date),by="day") == noaa$date))

# Summarize the NOAA data set.
summary(noaa)


##############################################################
#   Transformations for the common name groupings data set   #
##############################################################

# Convert the grouping variable to a factor.
groupings$grouping <- as.factor(groupings$grouping)


#######################################################
#   Join the inaturalist, NOAA, and groupings data    #
#######################################################

inat_noaa_grp <- sqldf("
                        select a.*,
                               b.grouping,
                               c.awnd,
                               c.prcp,
                               c.tavg,
                               c.tmin,
                               c.tmax
                        from inaturalist a inner join groupings b
                                                on a.family=b.family
                                           inner join noaa c
                                                on a.event_dt=c.date
                       ")

# Take a quick look at the joined data.
glimpse(inat_noaa_grp)
miss_var_summary(inat_noaa_grp)
summary(inat_noaa_grp)

# Generate counts and proportions by class, order, and family.
inat_noaa_grp %>% 
  group_by(grouping) %>% 
  tally() %>% 
  mutate(pct = n/sum(n)) %>% 
  View()


################################################################################
################################################################################
#                          Data visualization                                  #
################################################################################
################################################################################

# Function to plot the percentage of total observations by a particular 
# seasonal variable by grouping.
percentage_plots <- function(df,svar,xlabel,plot_title){
  svar <- enquo(svar)
  df %>% 
    group_by(grouping,!!svar) %>% 
    tally() %>% 
    mutate(percent_obs = n/sum(n)) %>% 
    ggplot(aes(x=!!svar,y=percent_obs)) + 
       geom_line(aes(group=grouping)) +
       facet_grid(grouping ~ .) +
       labs(x=xlabel,y="Percent of Total") +
       scale_y_continuous(labels = scales::percent) +
       ggtitle(plot_title) +
       theme(plot.title = element_text(hjust = 0.5))
}

# Plot the percentage of total observations by month by grouping.
p1 <- percentage_plots(inat_noaa_grp,month,"Month","Percentage of Total Observations by Month")

# Plot the percentage of total observations by hour by grouping.
p2 <- percentage_plots(inat_noaa_grp,hour,"Hour","Percentage of Total Observations by Hour")

# Arrange the plots.
grid.arrange(p1,p2, nrow=1)


# Generate a random sample of the data for use with geographic scatter plots.
set.seed(314)
smpl1 <- inat_noaa_grp[sample(nrow(inat_noaa_grp),5000),]

# Plot the latitude and longitude of the sample on a map.
qmplot(decimallongitude, decimallatitude, data=smpl1,
       color=I('blue'), size = I(0.2)) + facet_wrap(~ grouping, ncol=5)

# Pivot the weather variables for box plotting.
weather_vars_long <- inat_noaa_grp %>%  
  select(grouping,event_dt,awnd,prcp,tavg,tmin,tmax) %>% 
  pivot_longer(cols=!c(grouping,event_dt))

# Function to create box plots for weather variables by grouping.
weather_box <- function(df_name,var_name,plot_title,ylab){
  df_name %>% 
    filter(name==var_name) %>% 
    ggplot(aes(x=grouping,y=value,fill=grouping)) + 
      geom_boxplot() +
      labs(x=NULL, y=ylab) +
      ggtitle(plot_title) +
      theme(plot.title = element_text(hjust = 0.5,size=9), 
            axis.text=element_text(size=7),
            axis.title=element_text(size=8),
            legend.position = "none")
}

# Boxplot for average temperature by grouping.
p1 <- weather_box(weather_vars_long,"tavg","Average Temperature","Temperature (°F)")

# Boxplot for minimum temperature by grouping.
p2 <- weather_box(weather_vars_long,"tmin","Minimum Temperature","Temperature (°F)")

# Boxplot for maximum temperature by grouping.
p3 <- weather_box(weather_vars_long,"tmax","Maximum Temperature","Temperature (°F)")

# Boxplot for average wind speed by grouping.
p4 <- weather_box(weather_vars_long,"awnd","Average Wind Speed","Wind Speed (mph)")

# Boxplot for precipitation by grouping.
p5 <- weather_box(weather_vars_long,"prcp","Precipitation","Precipitaion (in)")

# Arrange the boxplots.
grid.arrange(p1,p2,p3,p4,p5, nrow=2)


# Generate a random sample of the data for use with the scatter plot matrix.
set.seed(2718)
smpl2 <- inat_noaa_grp[sample(nrow(inat_noaa_grp),1000),]

# Create a scatter plot matrix for the sample.
plot(data=smpl2, ~ hour + month + decimallatitude + decimallongitude + awnd + prcp + tavg + tmin + tmax, pch=21 , cex=1.5 , col="blue",
     lower.panel=panel.smooth, upper.panel=NULL,
     main="Scatter Plot Matrix" )

# Calculate the correlations between the temperature variables.
inat_noaa_grp %>% 
  select(tavg,tmin,tmax) %>% 
  cor(method="pearson")
#          tavg      tmin      tmax
# tavg 1.000000 0.9386120 0.9837840
# tmin 0.938612 1.0000000 0.8965472
# tmax 0.983784 0.8965472 1.0000000



################################################################################
################################################################################
#      Run the Random Forest algorithm to classify the grouping variable       #
################################################################################
################################################################################

# Create training and test data sets using a 60/40 split. 
# Also, set the seed for reproducibility.
set.seed(123)
ntrain <- round(0.6*nrow(inat_noaa_grp))
tindex <- sample(nrow(inat_noaa_grp), ntrain)
train <- inat_noaa_grp[tindex,]
test <- inat_noaa_grp[-tindex,]


#######################################################
#               Random Forest Classifier              #
#######################################################

# Create a user defined function which takes the ntree, mtry, and the 
# model specification as arguments, and returns a named vector with ntree,
# mtry, and the test misclassification error rate for the corresponding 
# random forest fit on the training set.
rf_acc <- function(n,m,specification){
  set.seed(n+m)
  rf <- randomForest(specification, 
                     data=train,
                     ntree=n,
                     mtry=m,
                     importance=TRUE)
  rf_prediction <- predict(rf,newdata=test,type="class") 
  cm <- table(rf_prediction,test$grouping)
  misclass <- 1 - sum(diag(cm))/sum(cm)
  v <- c(n,m,misclass)
  names(v) <- c("ntree","mtry","misclass")
  print(v)
  return(v)
}

# Create a formula to capture the model specification where the
# predictor variables are month, hour, decimallatitude, decimallongitude,
# tmin, tmax, prcp, and awnd.
spec <- grouping ~ month + hour +
                   decimallatitude + decimallongitude + 
                   tmin + tmax + prcp + awnd

# Create a data frame with the combinations of ntree and mtry that
# will be passed to the rf_acc function.
combos <- crossing(n_vals=seq(100,1000,by=100),
                   m_vals=3:5)

# Try out the function with ntree=500 and mtry=5 first.
rf_acc(500,5,spec)
#       ntree        mtry    misclass 
# 500.0000000   5.0000000   0.230406

# The function runs very slowly for the specified ntree and mtry values.
# Convert hour and month to numeric and run the function
# again to see how this affects performance.
train$month <- as.numeric(train$month)
train$hour <- as.numeric(train$hour)
test$month <- as.numeric(test$month)
test$hour <- as.numeric(test$hour)

rf_acc(500,5,spec)
#       ntree        mtry    misclass 
# 500.0000000   5.0000000   0.2299339 

# Running the function with month and hour as numeric is about
# 10 times faster than running the function with month and hour
# as factors, and the misclassification error rates are about the same.
# Therefore, we will use the numeric versions going forward.


# Use mapply to run the rf_acc function with all of the combinations in the combos
# data frame and create a data frame to store the results.
rf_acc_df <- data.frame(t(mapply(function(x,y) rf_acc(x,y,spec),combos$n_vals,combos$m_vals)))

# Sort the data frame by test misclassification error rates to find the best one.
rf_acc_df %>% 
  arrange(misclass)
#    ntree mtry  misclass
# 1    900    5 0.2292257
# 2    200    5 0.2295798
# 3    600    5 0.2296978
# 4    500    5 0.2299339
# 5   1000    5 0.2304060
# 6    700    5 0.2305241
# 7    300    5 0.2308782
# 8    400    5 0.2312323
# 9    300    4 0.2314684
# 10   400    4 0.2314684
# 11   800    4 0.2317044
# 12   800    5 0.2317044
# 13   100    5 0.2319405
# 14   200    4 0.2321766
# 15   900    4 0.2321766
# 16   700    4 0.2324127
# 17   600    4 0.2326487
# 18  1000    4 0.2330028
# 19   500    4 0.2335930
# 20   600    3 0.2337110
# 21   400    3 0.2338291
# 22   100    4 0.2343012
# 23  1000    3 0.2346553
# 24   900    3 0.2347734
# 25   800    3 0.2351275
# 26   500    3 0.2353636
# 27   700    3 0.2353636
# 28   200    3 0.2357177
# 29   300    3 0.2359537
# 30   100    3 0.2364259

# Plot the test misclassification error rates for each value of mtry.
rf_acc_df %>% 
  ggplot(aes(x=ntree, y=misclass, group=as.factor(mtry), color=as.factor(mtry))) + 
  geom_line() +
  labs(x="# of Trees", y="Error Rate", color="mtry") +
  ggtitle("Test Misclassification Error Rates") +
  theme(plot.title = element_text(hjust = 0.5,size=9), 
        axis.text=element_text(size=7),
        axis.title=element_text(size=9))

# Run the random forest model for ntree and mtry corresponding to 
# the lowest test misclassification error rate on the full data set 
# with hour and month as numeric, and produce variable importance plots.
inat_noaa_grp$hour <- as.numeric(inat_noaa_grp$hour)
inat_noaa_grp$month <- as.numeric(inat_noaa_grp$month)
set.seed(1234)
rf <- randomForest(spec, 
                   data=inat_noaa_grp,
                   ntree=900,
                   mtry=5,
                   importance=TRUE)
print(rf)
varImpPlot(rf)

# Calculate the test misclassification error rate for the model
# that excludes prcp as a predictor with ntree=900 and mtry=5
spec_no_prcp <- grouping ~ month + hour +
                           decimallatitude + decimallongitude + 
                           tmin + tmax + awnd
rf_acc(900,5,spec_no_prcp)
#       ntree        mtry    misclass 
# 900.0000000   5.0000000   0.2299339 



