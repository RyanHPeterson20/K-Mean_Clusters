# Read in data:
my.data <- read.csv("~/Documents/API_4_DS2_en_csv_v2_820954.csv", skip = 4,
                    header = TRUE,
                    stringsAsFactors = FALSE)

# Read in country metadata:
my.countries <- read.csv("~/Documents/Metadata_Country_API_4_DS2_en_csv_v2_820954.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE)

## Libraries
library(dplyr)
library(tidyr)                        

# Left joint country codes to data:
countries <- left_join(my.countries, my.data, by = "Country.Code")

# Select one year.  I am using, as an example, 2010:
countries <- select(countries, Country.Code, Region, IncomeGroup,
                    Country.Name, Indicator.Code, X2010)

# Convert from narrow to wide form. The pivot_wider() function is a more mod- ern version of spread():
countries <- pivot_wider(data = countries,
                         names_from = Indicator.Code,
                         values_from = X2010,
                         id_cols = c(Country.Code, Region, IncomeGroup, Country.Name))

##identify non-NA variables in data set
n.NotNA <- function(x) sum(!is.na(x))
my.summary <- summarize_all(countries, n.NotNA) %>% as.data.frame
my.summary

##k mean clustering library
library(mclust)

##select variabels and clean of NA
# SE.SEC.ENRR School enrollment, secondary (% gross)	
# SE.SEC.DURS Secondary education, duration (years)	
# SE.PRM.ENRR School enrollment, primary (% gross)	
# SE.PRM.DURS Primary education, duration (years)	
education <- select(countries, Country.Code, SE.SEC.ENRR, SE.SEC.DURS, SE.PRM.ENRR, SE.PRM.DURS)
education <- filter(.data = education, !is.na(SE.SEC.ENRR),
                    !is.na(SE.SEC.DURS), !is.na(SE.PRM.ENRR), !is.na(SE.PRM.DURS))

##add row names to education data frame for easier viewing of cluster vector data
rownames(education) <- c(education$Country.Code)
## save cleaned country codes for later use
countrycode <- c(education$Country.Code)
## reomve country code from data frame for k mean clustering
education <- select(education, -Country.Code)

##set seed and run k mean cluster
set.seed(15)
edu_cluster <- kmeans(education, centers = 4)
edu_cluster

View(edu_cluster$cluster)

##add cluster data to education data frame
education <- education %>% mutate(cluster = as.character(edu_cluster$cluster))
##check to ensure cluster variable is a character for future graphing
is.character(merged_education$cluster)
##add country codes back into data frame
education$Country.Code <- countrycode

##left join education and countries data frames back together
merged_education <- left_join(education, countries, by = "Country.Code")


##graph to identify income groups and clusters
library(ggplot2)

g <- ggplot(data = merged_education, aes(x = SE.SEC.ENRR.x, y = SE.PRM.ENRR.x)) +
       geom_point(aes(color = cluster, shape = IncomeGroup), size=2) +
       xlab("Secondary Enrollment (% Gross)") +
       ylab("Primary Enrollment (% Gross)")
g
