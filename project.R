# ----Part 1, data preparation and data wrangling----

# to get all the data into one table that can be used for linear modeling
# reading the data files using read_csv()
vaccines <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
gdp <- read_csv("gdp.csv")
demo <- read_csv("demographics.csv")

# removing province states and places with 0 population
vaccines <- vaccines %>% filter(Population >= 0, is.na(Province_State))

# removing unnecessary columns
vaccines <- vaccines %>% select(-Admin2, -FIPS, -Province_State, -UID, -iso2,-code3, -Lat, -Long_, -Combined_Key)

# removing unnecessary columns
demo <- demo %>% select(-`Series Name`)

#tidying vaccine table to show iso3, country, population, date of vaccine, shots.
vaccines <- vaccines %>% pivot_longer(-c(Country_Region, Population, iso3), names_to = "date", values_to = "shots") %>% view()

#removing any days where any given country didnt start vaccinating yet
vaccines <- vaccines %>% filter(shots > 0) %>% view()

#pivot_wider to get all demo data tidy
demo <- demo %>% pivot_wider(names_from = `Series Code`, values_from = YR2015)

#add vaccination rate to vaccine table
vaccines <- vaccines %>% mutate(vacRate = shots/Population) %>% view()

#add daysSinceStart column.
vaccines <- vaccines %>% group_by(Country_Region) %>% mutate(daysSinceStart = 1:n()) %>% view()

#selecting only 2020 and removing NAs
gdp <- gdp %>% select(c(2,65)) %>% drop_na() %>% view()

#removing unneeded columns and NAs from demo
demo <- demo %>% select(-1) %>% drop_na() %>% view()

#removing date since we are using since start method, and dropping NAs
vaccines <- vaccines %>% select(-date) %>% drop_na() %>% view()

# combine male and female demos into TOTL column, and dropped the gendered
cleaneddemo <- demo %>% mutate(SP.POP.80UP.TOTL=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% mutate(SP.POP.1564.IN.TOTL=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% mutate(SP.POP.0014.IN.TOTL=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% mutate(SP.DYN.AMRT.TOTL=SP.DYN.AMRT.MA+SP.DYN.AMRT.FE) %>% mutate(SP.POP.TOTL.IN.TOTL=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% mutate(SP.POP.65UP.IN.TOTL=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% select(-c(5:16)) %>% view()

#renames 2020 to GDP
gdp <- gdp %>% rename("GDP" = `2020`)


gdp <- gdp %>% rename("iso3" = `Country Code`)
cleaneddemo <- cleaneddemo %>% rename("iso3" = `Country Code`)

#the merge
cleaneddataset <- vaccines %>% inner_join(gdp) %>% inner_join(cleaneddemo)

# ----Part 2, Modeling----

# Tried different combinations of variables for modeling 
lifeExpectancy <- lm(formula = vacRate~GDP+SP.DYN.LE00.IN, data=clean_data)

mortalityRateTotal <- lm(formula = vacRate~SP.DYN.AMRT.TOTL, data=clean_data)

daysSinceStartModel <- lm(formula = vacRate ~ GDP + daysSinceStart, data = clean_data)

lm(formula = vacRate ~ GDP + daysSinceStart + SP.DYN.AMRT.TOTL, data = clean_data)

# Tried some variable transformations
testData <- mutate(clean_data, sqrtDaysSinceStart=sqrt(daysSinceStart))
mod_daysSS <- lm(formula = vacRate ~ GDP + sqrtDaysSinceStart, data = testData)

testData <- mutate(clean_data, proportions=SP.POP.0014.IN.TOTL/SP.POP.TOTL.IN.TOTL)
mod_proportions <- lm(formula = vacRate ~ GDP + proportions, data = testData)

testData <- mutate(clean_data, proportions=SP.POP.1564.IN.TOTL/SP.POP.TOTL.IN.TOTL)
mod_proportions2<- lm(formula = vacRate ~ GDP + proportions, data = testData)

# data visualization
ggplot(clean_data) + geom_point(mapping = aes(x = daysSinceStart, y = vacRate, color=Country_Region))

# A plot that shows the R2 values of the different models
# vector of models
smaller_names = c('gdp+LE','pop','gdp+days', 'gdp+days+pop', 'gdp+sqtDays','gdp+vac0_14','gdp+15-64')
# vector of rsquared values
values <- c(0.2122,0.1747,0.5831,0.7121,0.5562,0.1699,0.1163)
# creating dataframe
bar_graph_data <- data.frame(model = smaller_names, rsqu = values)
# creating bar_graph
bar_graph <- ggplot(data = bar_graph_data, aes(x = model, y = rsqu)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))