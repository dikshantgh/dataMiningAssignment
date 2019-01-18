# Author : Dikshant Ghimire
#Matriculation Number : 24905
#Information Engineering and Computer Science(MSc)
#Second Semester
#Subject : Data Mining


#Including required libraries for this code

# using dplyr to use filter() and arrange() function
library(dplyr)
# using readr to use read_delim() function
library(readr)
# using ggplot2 to use ggplot() function
library(ggplot2)

# simply loading of the .csv file using either read_delim() or read.csv() into the dataset consumption
consumption <- read_delim("dataM/HH_SAMPLE_POWER_CONSUMPTION_GRID_A.csv",";",na="empty",escape_double = FALSE, trim_ws = TRUE)
#consumption <- read.csv("dataM/HH_SAMPLE_POWER_CONSUMPTION_GRID_A.csv",na="empty",sep=";",dec=",",header = TRUE)


# creating a new tibble with some values from consumption dataset and storing into new tibble 'hh_spc' 
hh_spc <- tibble(month_billing = consumption$MONTH_BILLING, 
                 swt_reason = consumption$SWITCHING_REASON,
                 cust_decl_kwh = consumption$DECLARATION_CONSUMP_CUST, 
                 grid_decl_kwh = consumption$DECLARATION_CONSUMP_CUST + consumption$CORRECTIVE_VALUE,
                 bill = consumption$BILLING_TYPE, 
                 inv_kwh_365 = round((consumption$CONSUMPTION_INVOICED * 365 / consumption$DAYS_INVOICED),digits = 0))

# Displaying the hh_spc tibble
View(hh_spc)

# Filtering some of the columns of the 'hh_spc' tibble with required logical operators and finally arranging the output by month_billing column
#Using pipeline to feed the filter output to the arrange operation and finally storing in hh_spc tibble again.
hh_spc <- filter(hh_spc , inv_kwh_365< 10000,cust_decl_kwh < 7500,cust_decl_kwh > 0) %>% arrange(month_billing)

# Displaying the hh_spc tibble
View(hh_spc)

#Using ggplot to plot only random 400 points with x axis being "Customer's declared consumption" and y axis being "Customer's (invoiced) 365 day consumption"
#Separating the points using switching reason(swt_reason) column
ggplot(data=sample_n(hh_spc, size = 400),aes(x=cust_decl_kwh,y=inv_kwh_365, color = swt_reason))+ 
              geom_point(size=1, alpha=1) + geom_smooth(se=FALSE)+
              labs(title = " A plot for Customers’ declared consumption and (invoiced) 365 day consumption",
              x = "Customer's declared consumption",y="Customer's (invoiced) 365 day consumption",color="Switching Reasons")


#Using ggplot to boxplot only 10% of all values of hh_spc tibble with x acis being  "Switching Reasons" and y axis being "365 day consumption"
ggplot(data=sample_frac(hh_spc, size = 0.1), aes(x=swt_reason, y=inv_kwh_365)) + geom_boxplot()+
              labs(title = " A boxplot with switching reason and 365 day consumption",
              x = "Switching Reasons",y="365 day consumption")
