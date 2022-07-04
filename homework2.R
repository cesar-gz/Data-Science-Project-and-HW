# problem 2, parts 1-13, using nycflights13 dataset
# List data only for flights that departed on February 12, 2013.
flights %>% filter(month == 2, day == 12, year == 2013)

# List data only for flights that were delayed (both arrival and departure) by more than 2 hours.
flights %>% filter(dep_delay > 120, arr_delay > 120)

# List data only for flights that were delayed (either arrival or departure) by more than 2 hours.
flights %>% filter(dep_delay > 120 | arr_delay > 120)

# List data only for flights that were operated by United, American, or Delta.
flights %>% filter(carrier == c("UA", "DL","AA"))

# Sort data in order of fastest flights (air_time).
flights %>% arrange(air_time)
flights %>% arrange(aesc(air_time))

# Sort data in order of longest duration flights (air_time).
flights %>% arrange(-air_time)
flights %>% arrange(desc(air_time))

# Show only the origin and destination of flights sorted by longest flights.
flights %>% arrange(-air_time) %>% select(origin, dest)

# Add a new variable that indicates the total delay (both departure and arrival delay).
flights %>% mutate(total_delay = arr_delay + dep_delay)

# Show only the origin and destination of flights sorted by descending order of total delay.
flights %>% mutate(totalDelay = arr_delay + dep_delay) %>% arrange(desc(totalDelay)) %>% select(origin, dest)

# Show only the origin and destination of 10 most delayed flights [Hint: there are multiple ways of solving this. Some additional functions that you will find useful are head(), slice(), min_rank().]
flights %>% mutate(totalDelay = arr_delay + dep_delay) %>% arrange(desc(totalDelay)) %>% select(origin, dest) %>% slice(1:10)

# Show the average total delay for all flights
flights %>% summarise(avgTotalDelay = mean(arr_delay + dep_delay, na.rm = TRUE))

# Show the average total delay for every departure city.
flights %>% group_by(origin) %>% summarise(avgTotalDelay = mean(arr_delay + dep_delay, na.rm = TRUE))

# Show the average total delay for every departure-arrival city pair.
flights %>% group_by(origin, dest) %>% summarise(avgTotalDelay = mean(arr_delay + dep_delay, na.rm = TRUE))


# problem 4, parts a-h, using billboard dataset (Billboard's top 100 song rankings in the year 2000)

# Show for each track, how many weeks it spent on the chart
billboard %>% pivot_longer(4:79, names_to="Weeks", values_to="Position", values_drop_na = TRUE) %>% group_by(track) %>% summarise("Weeks on chart"=n())

# List tracks in decreasing order of number of weeks spent on the chart
billboard %>% pivot_longer(4:79, names_to="Weeks", values_to="Position", values_drop_na = TRUE) %>% group_by(track) %>% summarise("Weeks on chart"=n()) %>% arrange(desc(`Weeks on chart`))

# Show for each track, its top rank
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(track, ranking) %>% arrange(ranking) %>% group_by(track) %>% summarise(TopRank = min(ranking))

# List tracks in increasing order of its top rank
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(track, ranking) %>% group_by(track) %>% summarise(TopRank = min(ranking)) %>% arrange(TopRank)

# Show for each artist, its top rank
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(artist, ranking) %>% group_by(artist) %>% summarise(TopRank = min(ranking)) 

# List artists in increasing order of their top rank
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(artist, ranking) %>% group_by(artist) %>% summarise(TopRank = min(ranking)) %>% arrange(TopRank)

# List tracks that only spent one week in the charts
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(track, ranking) %>% group_by(track) %>% summarise('weeksOnTop' = n()) %>% filter(weeksOnTop == 1)

# List tracks that only spent one week in the charts along with its artist
billboard %>% pivot_longer(4:79, names_to = "week", values_to = "ranking", values_drop_na = TRUE) %>% select(artist, track, ranking) %>% group_by(artist, track) %>% summarise('weeksOnTop' = n()) %>% filter(weeksOnTop == 1)



# problem 5, part b, approach 1

# Read the .csv file using read_csv (NOT read.csv) and store it in a table.
InsurPrems <- read_csv("insurance_premiums.csv")

# Select only the Location column and the columns containing the word Employee
InsurPrems %>% select(Location, 2,4,6,8,10,12)

# Pivot the data in the Employee columns into a pair of names_to and values_to columns called Year and Employee_Contribution
InsurPrems %>% select(Location, 2,4,6,8,10,12) %>% pivot_longer(-1, names_to = "Year", values_to = "Employee_Contribution")

# The Year column has values such as 2013__Employee_Contribution. It should contain only the year (e.g., 2013). Use separate() to separate the values into the year component and discard the remaining portion. [Hint: use “__” as the separator. Specifying NA in the into parameter discards the variable.]
InsurPrems %>% select(Location, 2,4,6,8,10,12) %>% pivot_longer(-1, names_to = "Year", values_to = "Employee_Contribution") %>% separate(Year, into="Year", sep="_")

# Store the result of the above pipeline in a table.
FormattedEmployeeCont <- InsurPrems %>% select(Location, 2,4,6,8,10,12) %>% pivot_longer(-1, names_to = "Year", values_to = "Employee_Contribution") %>% separate(Year, into="Year", sep="_") %>% view()

# Repeat the above steps for the “Employer” columns and store the result in another table.
FormattedEmployerCont <- InsurPrems %>% select(Location, 3,5,7,9,11,13) %>% pivot_longer(-1, names_to = "Year", values_to = "Employer_Contribution") %>% separate(Year, into="Year", sep="_") %>% view()

# Join the two tables. Check that the resulting table matches the desired tidy format.
FinalTable <- FormattedEmployeeCont %>%  full_join(FormattedEmployerCont)


# problem 5, part b, approach 2

# Read the .csv file using read_csv (NOT read.csv) and store it in a table.
InsurPrems <- read_csv("insurance_premiums.csv")

# Pivot_longer all columns (except Location) into two names_to columns. This requires a names_sep to be specified. Read the help for pivot_longer().
InsurPrems %>% pivot_longer(-1, names_to= c("Year", "Contributor"), names_sep = "__")

# Pivot_wider a pair of columns from the previous step. Check that the resulting table matches the desired tidy format.
InsurPrems %>% pivot_longer(-1, names_to= c("Year", "Contributor"), names_sep = "__") %>% pivot_wider(names_from= "Contributor", values_from = value)
