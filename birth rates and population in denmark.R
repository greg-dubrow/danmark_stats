# look at changes in birth rates over time and population changes.
# compare immigration numbers

# birth stats
 # avg age of mother by year, by decade
 # group mother's age...12-14, 15-17, 18-22, 23-25, 26-29, 30-34, 35-39,
 #                      40-44, 45-49, 50-59, 60+

# visualise
# bar chart births by age facet by year or half-decade
# gganimtate by year?

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(gganimate)
library(CodeAndRoll2)
library(RColorBrewer) # color palettes
library(ggokabeito) # color palettes for colorblind friendly charts

source("~/Data/r/basic functions.R")
options(scipen=10000)

# metadata for table variables
table_meta <- danstat::get_table_metadata(table_id = "FOD", variables_only = TRUE)
glimpse(table_meta)

# mothers age
glimpse(table_meta$values[[1]][1])

# years
glimpse(table_meta$values[[3]][1])

# turns mothers age and years into vector objects to use in variable list
agevals <- dfcol_to_vector(table_meta$values[[1]][1])
yearvals <- dfcol_to_vector(table_meta$values[[3]][1])

variables_birth <- list(
	list(code = "modersalder", values = agevals),
			 	# c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
			 	# 	"21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
			 	# 	"32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
			 	# 	"43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53",
			 	# 	"54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64")),
	list(code = "tid", values = yearvals))
			 	# c(1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
			 	# 	1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988,
			 	# 	1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
			 	# 	1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
			 	# 	2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
			 	# 	2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
			 	# 	2021, 2022, 2023)))

birthsdk1 <- get_data("FOD", variables_birth, language = "da") %>%
	as_tibble() %>%
	select(year = TID, age_mother_c = MODERSALDER, count = INDHOLD)

glimpse(birthsdk1)

# group mother's age...10-14, 15-17, 18-22, 23-25, 26-29, 30-34, 35-39,
#                      40-44, 45-49, 50-59, 60+

birthsdk <- birthsdk1 %>%
	mutate(age_mother = as.numeric(str_remove(age_mother_c, " år"))) %>%
	mutate(age_mother_grp = case_when(
		between(age_mother, 10, 14) ~ "10-14",
		between(age_mother, 15, 17) ~ "15-17",
		between(age_mother, 18, 22) ~ "18-22",
		between(age_mother, 23, 25) ~ "23-25",
		between(age_mother, 26, 29) ~ "26-29",
		between(age_mother, 30, 34) ~ "30-34",
		between(age_mother, 35, 39) ~ "34-39",
		between(age_mother, 40, 44) ~ "40-44",
		between(age_mother, 45, 49) ~ "45-49",
		between(age_mother, 50, 59) ~ "50-59",
		TRUE ~ "60+")) %>%
	group_by(age_mother_grp, year) %>%
	mutate(count_age_grp = sum(count)) %>%
	ungroup() %>%
	mutate(year_grp = case_when(
		between(year, 1973, 1975) ~ "1973-1975",
		between(year, 1976, 1979) ~ "1976-1979",
		between(year, 1980, 1984) ~ "1980-1984",
		between(year, 1985, 1989) ~ "1985-1989",
		between(year, 1990, 1994) ~ "1990-1994",
		between(year, 1995, 1999) ~ "1995-1999",
		between(year, 2000, 2004) ~ "2000-2004",
		between(year, 2005, 2009) ~ "2005-2009",
		between(year, 2010, 2014) ~ "2010-2014",
		between(year, 2015, 2019) ~ "2015-2019",
		between(year, 2020, 2024) ~ "2020-2024")) %>%
	mutate(year = as.integer(year)) %>%
	select(year, age_mother, count_age = count, age_mother_grp, count_age_grp,
				 year_grp)


glimpse(birthsdk)

birthsdk %>%
	distinct(year, .keep_all = TRUE) %>%
	view()


births_age_grp <- birthsdk %>%
	distinct(year, age_mother_grp, .keep_all = TRUE) %>%
	select(-age_mother, -count_age)

births_age_grp %>%
	group_by(age_mother_grp) %>%
	summarise(count = sum(count_age_grp))



birthsdk %>%
	distinct(year, age_mother_grp, .keep_all = TRUE) %>%
	group_by(age_mother_grp) %>%
	count(year, count_age_grp) %>%
	view()

birthsdk %>%
	distinct(year, age_mother_grp, .keep_all = TRUE) %>%
	select(-age_mother, -count_age) %>%
	ggplot(aes(age_mother_grp, count_age_grp, fill = age_mother_grp)) +
	geom_bar(stat = "identity") +
	scale_fill_brewer(palette = "Set3") +
	facet_wrap(~ year, scales = "free_x") +
	theme(legend.position = "none")

birthsbyyearplot <-
birthsdk %>%
	distinct(year, age_mother_grp, .keep_all = TRUE) %>%
	select(-age_mother, -count_age) %>%
	ggplot(aes(age_mother_grp, count_age_grp, fill = age_mother_grp)) +
	geom_bar(stat = "identity") +
	scale_fill_brewer(palette = "Set3") +
	ylim(0, 30000) +
	labs(title = 'Year: {frame_time}', x = 'Age group') +
	theme(legend.position = "none") +
	transition_time(year) +
	ease_aes('linear') +
	view_step(pause_length = 2, step_length = 1)

animate(birthsbyyearplot, nframes = 51 * 3)

birthsdk %>%
	distinct(year, age_mother_grp, .keep_all = TRUE) %>%
	select(-age_mother, -count_age) %>%
	ggplot(aes(age_mother_grp, count_age_grp)) +
	geom_bar(stat = "identity") +
	facet_wrap(~ year_grp, scales = "free_x")


## average age first time mothers

table_meta_fod11 <- danstat::get_table_metadata(table_id = "FOD11", variables_only = TRUE)
glimpse(table_meta_fod11)

glimpse(table_meta_fod11$values[[2]][1])

yearvals2 <- dfcol_to_vector(table_meta_fod11$values[[2]][1])

yearvals2 <- yearvals2["73":"123"]

glimpse(yearvals2)

variables_birth_fod11 <- list(
	list(code = "alder", values = "610"),
	list(code = "tid", values = yearvals2))
			 	# c(1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
			 	# 	1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988,
			 	# 	1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996,
			 	# 	1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
			 	# 	2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
			 	# 	2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
			 	# 	2021, 2022, 2023)))

birthsdk_fos11_1 <- get_data("FOD11", variables_birth_fod11, language = "da") %>%
	as_tibble() %>%
	select(year = TID, avg_avg_mother_1st = INDHOLD)
