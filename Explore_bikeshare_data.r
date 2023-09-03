
library(ggplot2)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

print(dim(ny))
head(ny)

print(dim(wash))
head(wash)

print(dim(chi))
head(chi)

# Your solution code goes here
print(summary(chi$Trip.Duration))

chi$trip.mins <- chi$Trip.Duration / 60 # create new col with Trip Duration in minutes
print(summary(chi$trip.mins))

q_95 = quantile(chi$trip.mins, 0.95) # we will use the 95-% quantile
print(q_95)

hist = qplot(x = trip.mins, data = chi, xlim = c(0,q_95), binwidth=1, main="Trip Duration", ylab="Count", xlab="Time in Mins")
hist

hist + facet_grid(Gender ~.)

print(sum(chi$Gender =="Male"))
print(sum(chi$Gender =="Female"))
print(sum(chi$User.Type =="Customer"))

print(sum(chi$Gender =="Male") + sum(chi$Gender =="Female")+ sum(chi$User.Type =="Customer"))
print(dim(chi))

gend = c("Female", "Male")
for (g in gend){
    print(g)
    print(summary(subset(chi, Gender==g)$trip.mins))
    print("----------------------------------------------------")
}

print("User Type = Customer")
print(summary(subset(chi, User.Type == "Customer")$trip.mins))

print(class(ny$Start.Time)) # check class of the column

ny$Start.Time[1:5]

head(ny)

library(zoo) # to make use of the func as.yearmon

# defining a function to create a new column with year and month information only
year_month = function(df, col){
    new_col <- as.character(df[,col]) # convert to string
    new_col <- (substr(new_col, 1, 10)) # get substring, since we are only interested in the dates -> YYYY-MM-DD
    new_col <- as.Date(new_col) # format as Date class
    new_col <- as.yearmon(new_col) # pull year and month only
    return(new_col)
}

# assigning new columns
chi$year_month = year_month(chi, "Start.Time")
ny$year_month = year_month(ny, "Start.Time")
wash$year_month = year_month(wash, "Start.Time")

# print summary of counts per dataframe
dfs = list(chi, ny, wash)

city_names = c("chicago", "new_york", "washington")

check_sum = 0
ind = 1
for (df in dfs){
    print(city_names[ind])
    print(table(df$year_month))
    ind = ind + 1
    check_sum = check_sum + table(df$year_month)[1] # running sum to check later
}

aggregate_counts = table(chi$year_month) + table(ny$year_month) + table(wash$year_month) # aggregate all counts

if (check_sum != aggregate_counts[1]){    # check if the aggregation worked
    stop("Error, Count does not match")
}

print("Aggregate Counts")
aggregate_counts

# it might be easier to store the results in a dataframe and work with them subsequently
# set up a new dataframe
ts_df = data.frame(matrix(nrow=6, ncol=5))
colnames(ts_df) = append(append(list("date"), city_names), "total")
ts_df$date = sort(unique(chi$year_month))
ts_df

# fill the dataframe with the respective values in the corresponding columns
i = 2
for (df in dfs){
    ts_df[,i] = table(df$year_month)
    i = i + 1
}

ts_df$total = ts_df[,city_names[1]] + ts_df[,city_names[2]] + ts_df[,city_names[3]]

ts_df

# changing the data frame to long format to facilitate plotting 
library(reshape2) # to use "melt"

ts_df_melted = melt(ts_df, id="date")
colnames(ts_df_melted) = c("date", "city", "count")
ts_df_melted

# Plotting
library(ggplot2)
# date is class "yearmon" / total is class "table"
ggplot(aes(x=as.Date(date), y=as.numeric(count), col=city), data=ts_df_melted) + 
ggtitle("Count of bike sharings per city and total") +
xlab("Month")+
ylab("Count")+
geom_line()

chi$start_end = paste(chi$Start.Station, " \n", chi$End.Station)
ny$start_end = paste(ny$Start.Station, " \n", ny$End.Station)
wash$start_end = paste(wash$Start.Station, " \n", wash$End.Station)

trips = c(chi$start_end, ny$start_end, wash$start_end)


# check the length of the resulting vector
if (length(trips) != length(chi$start_end) + length(ny$start_end) + length(wash$start_end)){
    stop("Error, Count does not match")
}

top_10_trips = sort(table(trips), decreasing = TRUE)[1:10]

#convert table to dataframe
top_10_trips = data.frame(cbind(top_10_trips))
top_10_trips

# https://community.rstudio.com/t/how-to-change-row-index-into-a-column/67919
library(tibble)
top_10_trips = rownames_to_column(top_10_trips)
colnames(top_10_trips) = c("trip", "count")
top_10_trips

?reorder

# https://stackoverflow.com/questions/18401931/ggplot2-ordering-y-axis
ggplot(aes(x = reorder(trip,count), y=count), data = top_10_trips) + # using reorder to sort with respect to count
geom_bar(stat="identity") + 
coord_flip()

print("Total count of Trips:")
print(length(trips))
print("\n---------------")

print("Count of unique trip combinations")
print(length(table(trips)))

print("\n---------------")

print("Fraction of unique trip combinations to total count")
print(length(table(trips))/length(trips))

# another check
dim(chi)[1] + dim(ny)[1] + dim(wash)[1] == length(trips)

system('python -m nbconvert Explore_bikeshare_data.ipynb')

print("first change")
