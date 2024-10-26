library(danstat)
library(statsDK)
library(dkstat)


### danstat
all_subjects <- get_subjects()

all_tables <- get_tables()

tab_HFUDD11 <- get_table_metadata(table_id = "HFUDD11", variables_only = TRUE)

get_tables(table_id = "HFUDD11")


tab_folk1c <- get_table_metadata(table_id = "folk1c", variables_only = TRUE)

