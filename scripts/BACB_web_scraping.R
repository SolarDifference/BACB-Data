# Owner: Zachary Morford
# Date updated: July 30, 2022

# This script scrapes the most recent BACB data from their website


# Packages ----------------------------------------------------------------

library(rvest)
library(plyr)
library(XML)
library(tidyverse)
library(tsibble)
library(readxl)


# Scrape Website ----------------------------------------------------------

# Scrape the BACB data site for current monthly certificant numbers
# These data show the total cumulative certificants per subregion (all available)
# Once data are imported, they're converted into a tibble

url <- "https://www.bacb.com/services/o.php?page=101134"
website <- read_html(url)
cert_table <- website %>%
  html_elements(".resp-table-row") %>%
  html_text2() %>%
  as_tibble() %>%
  filter(value != "") %>%
  separate(value,
           into = c("Region", "BCBA-D", "BCBA", "BCaBA", "RBT", "Total"), 
           sep = "\n") %>%
  mutate(`BCBA-D` = str_remove_all(`BCBA-D`, ","),
         BCBA = str_remove_all(BCBA, ","),
         BCaBA = str_remove_all(BCaBA, ","),
         RBT = str_remove_all(RBT, ","),
         Total = str_remove_all(Total, ",")) %>%
  mutate(across(`BCBA-D`:Total, ~ as.double(.)),
         Month = yearmonth(date())) %>%
  filter(!(Region %in% c("United States Histogram", "Canada Histogram")))

# The data have two "Georgias" in the data set. In order to distinguish between them,
# I'm looping through the Regions and converting the second instance of "Georgia" to the country
# This is valid at the moment because the states are listed before the country.

georgia_counter <- 0

for (c in 1:length(cert_table$Region)) {

  if (cert_table[[1]][[c]] == "Georgia") {
    georgia_counter <- georgia_counter + 1
  }
  
  if (cert_table[[1]][[c]] == "Georgia" & georgia_counter == 2) {
    cert_table[[1]][[c]] <- "Georgia (country)"
  }
}


# Update Existing Data ----------------------------------------------------

data_to_update <- read_excel("data/01_raw/BACB_certificants_update.xlsx")

updated_certificants <- data_to_update %>%
  rbind(cert_table) %>%
  distinct() %>%
  mutate(Month = yearmonth(Month))

write_excel_csv(updated_certificants, 
                file = paste("data/01_raw/BACB_certificants_", today(), ".xlsx", sep = ""))

write_excel_csv(updated_certificants, 
                file = "data/01_raw/BACB_certificants_update.xlsx")

