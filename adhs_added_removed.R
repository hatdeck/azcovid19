suppressMessages(expr = {
    library(dplyr)
    library(tidyr)
    library(lubridate)})

setwd("~/Programming/R/COVID")

today <- Sys.Date()

dates <- c(today - 1, today)
days <- formatC(day(dates), width = 2, flag = "0")
months <- formatC(month(dates), width = 2, flag = "0")


death_objects <- paste0("DD", months, days)
case_objects <- paste0("DC", months, days)

objects <- c(death_objects, case_objects)


death_files <- paste0("AZDD", dates, ".rds")
case_files <- paste0("AZDC", dates, ".rds")

files <- c(death_files, case_files)

for (i in seq_along(objects) ){
    assign(objects[i], readRDS(files[i]))
}

DD <- full_join(get(objects[1]), get(objects[2]), by = "date", ) %>%
    tidyr::replace_na(list(deaths.x = 0)) %>%
    mutate(added = deaths.y - deaths.x) %>%
    as_tibble()
names(DD) <- c("sample_date", dates, "added")
deaths_added <- filter(DD, added >0) %>% select(sample_date, added) %>%
    uncount(added)
deaths_removed <- filter(DD, added < 0) %>% 
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)

DC <- full_join(get(objects[3]), get(objects[4]), by = "date", ) %>%
    tidyr::replace_na(list(cases.x = 0)) %>%
    mutate(added = cases.y - cases.x) %>%
    as_tibble()
names(DC) <- c("sample_date", dates, "added")
cases_added <- filter(DC, added >0) %>% select(sample_date, added) %>%
    uncount(added)
cases_removed <- filter(DC, added < 0) %>%
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)



cases_removed <- mutate(cases_removed, status = -1, reported =today, event = "case")
cases_added <- mutate(cases_added, status = 1, reported = today, event = "case")

deaths_removed <- mutate(deaths_removed, status = -1, reported =today, event = "death")
deaths_added <- mutate(deaths_added, status = 1, reported = today, event = "death")

filename <- paste0("cases_added", today, ".rds")
saveRDS(cases_added, filename)

filename <- paste0("cases_removed", today, ".rds")
saveRDS(cases_removed, filename)

filename <- paste0("deaths_added", today, ".rds")
saveRDS(deaths_added, filename)

filename <- paste0("deaths_removed", today, ".rds")
saveRDS(deaths_removed, filename)


# construct daily_total

final_objects <- character(4)
final_objects[1:2] <- paste("cases", c("added", "removed"), sep = "_")
final_objects[3:4] <- paste("deaths", c("added", "removed"), sep = "_")

# nr is the named vector with row numbers that correspond to final_objects
nr <- sapply(final_objects, FUN = function(c) nrow(get(c)))
n <- sum(nr)

# allocate and assign 

daily_total <- tibble(sample_date = Date(n), status = numeric(n), reported = Date(n), event = character(n))
for (i in seq_along(objects)){
    # index vector: get the sum of all the prior rows. Add 1.
    #       that's the first row to assign
    first_row <- (sum(nr[0:(i-1)]) + 1)
    # then get the sum of all the rows up to and including current object
    #       that's the last row to assign
    last_row <- sum(nr[0:i])
    index_v <- first_row:last_row
    # some objects do not have any rows (e.g., no deaths removed that day)
    # only assign objects with rows
    if (nr[i] > 0) daily_total[index_v ,] <- get(final_objects[i])
}

# add to prior total
most_recent_filename <- tail(dir(pattern = "^AZevents_added_removed"), 1)
prior_total <- readRDS(most_recent_filename)
total <- rbind(prior_total, daily_total)

filename <- paste0("AZ", "events_added_removed", Sys.Date(), ".rds")
saveRDS(total, filename)



# construct older


target <- as.Date("2020-08-01")
target_day <- day(target)
days <- c(target_day -1, target_day)
death_objects <- paste0("DD", days)
case_objects <- paste0("DC", days)

objects <- c(death_objects, case_objects)

dates <- as.character(c(target -1, target))
death_files <- paste0("AZDD", dates, ".rds")
case_files <- paste0("AZDC", dates, ".rds")

files <- c(death_files, case_files)

for (i in seq_along(objects) ){
    assign(objects[i], readRDS(files[i]))
}

DD <- full_join(get(objects[1]), get(objects[2]), by = "date", ) %>%
    tidyr::replace_na(list(deaths.x = 0)) %>%
    mutate(added = deaths.y - deaths.x) %>%
    as_tibble()
names(DD) <- c("sample_date", dates, "added")
deaths_added <- filter(DD, added >0) %>% select(sample_date, added) %>%
    uncount(added)
deaths_removed <- filter(DD, added < 0) %>% 
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)

DC <- full_join(get(objects[3]), get(objects[4]), by = "date", ) %>%
    tidyr::replace_na(list(cases.x = 0)) %>%
    mutate(added = cases.y - cases.x) %>%
    as_tibble()
names(DC) <- c("sample_date", dates, "added")
cases_added <- filter(DC, added >0) %>% select(sample_date, added) %>%
    uncount(added)
cases_removed <- filter(DC, added < 0) %>%
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)



cases_removed <- mutate(cases_removed, status = -1, reported = target)
cases_added <- mutate(cases_added, status = 1, reported = target)

deaths_removed <- mutate(deaths_removed, status = -1, reported = target)
deaths_added <- mutate(deaths_added, status = 1, reported = target)

filename <- paste0("cases_added", target, ".rds")
saveRDS(cases_added, filename)

filename <- paste0("cases_removed", target, ".rds")
saveRDS(cases_removed, filename)

filename <- paste0("deaths_added", target, ".rds")
saveRDS(deaths_added, filename)

filename <- paste0("deaths_removed", target, ".rds")
saveRDS(deaths_removed, filename)


# construct total
# TODO: set first and last accordingly
first <- NULL #  as.Date("2020-08-21")
last <- NULL # as.Date("2020-08-26")

dates <- seq(from = first, to = last, by = 1)
days <- formatC(day(dates), width = 2, flag = 0)
months <- formatC(month(dates), width = 2, flag = 0)
object_prefixes <- c("CA", "CR", "DA", "DR")
f <- function(ob_pr){
    paste0(ob_pr, formatC(days, width = 2, flag = 0))
}
objects <- sapply(object_prefixes, f)

file_prefixes <- c("cases_added", "cases_removed", "deaths_added", "deaths_removed")
g <- function(fi_pr){
    paste0(fi_pr, dates, ".rds")
}
files <- sapply(file_prefixes, g)


for (i in seq_along(objects)){
    assign(objects[i], readRDS(files[i]))
}


# nr is the named vector with row numbers that correspond to objects
nr <- sapply(objects, FUN = function(c) nrow(get(c)))
n <- sum(nr)

# allocate and assign 

total <- tibble(sample_date = Date(n), status = numeric(n), reported = Date(n), event = character(n))
for (i in seq_along(objects)){
    # index vector: get the sum of all the prior rows. Add 1.
    #       that's the first row to assign
    first_row <- (sum(nr[0:(i-1)]) + 1)
    # then get the sum of all the rows up to and including current object
    #       that's the last row to assign
    last_row <- sum(nr[0:i])
    index_v <- first_row:last_row
    # some objects do not have any rows (e.g., no deaths removed that day)
    # only assign objects with rows
    if (nr[i] > 0) total[index_v ,] <- get(objects[i])
}




prior_total <- readRDS("AZevents_added_removed2020-08-20.rds")
total <- rbind(prior_total, total)
 
filename <- paste0("AZ", "events_added_removed", Sys.Date(), ".rds")
saveRDS(total, filename)

# add jamal to total

jdeathdates <- as.Date(gsub(pattern = "^jjAZDD(.{10})\\.csv$", x = dir("jamal"), replacement = "\\1")[22:36])
dates_range <- seq(from = as.Date("2020-07-11"), to = as.Date("2020-07-30"), by = 1)
jmissing <- dates_range[!(dates_range %in% jdeathdates)]
dir.create("collabs")
file.copy(paste("jamal", dir("jamal"), sep = .Platform$file.sep), to = "collabs") 
adeathdates <- as.Date(gsub("^andyAZDD(.{10})\\.rds$", "\\1", dir("andy")))
adeathdates %in% jmissing
# whelp, need to go back and add those in
andyAZDD <- read.csv("~/andy_azdeaths.csv", 
                     nrows = 131,
                     stringsAsFactors = FALSE) %>%
    select(1:6)


andyAZDD0726 <- select(andyAZDD, 1, 2) %>% 
    rename(deaths = X2020.07.26)

andyAZDD0721 <- filter(andyAZDD, date < "2020-07-21") %>%
    select(1, 3) %>% 
    rename(deaths = X2020.07.21) 

andyAZDD0718 <- filter(andyAZDD, date < "2020-07-18") %>%
    select(1, 4) %>% 
    rename(deaths = X2020.07.18) 

andyAZDD0714 <- filter(andyAZDD, date < "2020-07-14") %>%
    select(1, 5) %>% 
    rename(deaths = X2020.07.14) 


andyAZDD0712 <- filter(andyAZDD, date < "2020-07-12") %>%
    select(1, 6) %>% 
    rename(deaths = X2020.07.12) 

days <- formatC(day(jmissing), width = 2, flag = "0")
months <- formatC(month(jmissing), width = 2, flag = "0")
object_names <- paste0("andyAZDD", months, days)
file_names <- paste0("andyAZDD", jmissing, ".rds")

for ( i in seq_along(object_names)){
    saveRDS(get(object_names[i]), paste("andy", file_names[i], sep = "/"))
}
file.copy(paste("andy", dir("andy")[24:28], sep = .Platform$file.sep), to = "collabs") 

# for reference
target <- as.Date("2020-07-12")

dates <- c(target - 1, target)
days <- formatC(day(dates), width = 2, flag = "0")
months <- formatC(month(dates), width = 2, flag = "0")


death_objects <- paste0("DD", months, days)
case_objects <- paste0("DC", months, days)

objects <- c(death_objects, case_objects)


death_files <- paste0("AZDD", dates, ".rds")
case_files <- paste0("AZDC", dates, ".rds")

files <- c(death_files, case_files)

for (i in seq_along(objects) ){
    assign(objects[i], readRDS(files[i]))
}

DD <- full_join(get(objects[1]), get(objects[2]), by = "date", ) %>%
    tidyr::replace_na(list(deaths.x = 0)) %>%
    mutate(added = deaths.y - deaths.x) %>%
    as_tibble()
names(DD) <- c("sample_date", dates, "added")
deaths_added <- filter(DD, added >0) %>% select(sample_date, added) %>%
    uncount(added)
deaths_removed <- filter(DD, added < 0) %>% 
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)

DC <- full_join(get(objects[3]), get(objects[4]), by = "date", ) %>%
    tidyr::replace_na(list(cases.x = 0)) %>%
    mutate(added = cases.y - cases.x) %>%
    as_tibble()
names(DC) <- c("sample_date", dates, "added")
cases_added <- filter(DC, added >0) %>% select(sample_date, added) %>%
    uncount(added)
cases_removed <- filter(DC, added < 0) %>%
    mutate(removed = -1*added) %>%
    select(sample_date, removed) %>%
    uncount(removed)



cases_removed <- mutate(cases_removed, status = -1, reported =today, event = "case")
cases_added <- mutate(cases_added, status = 1, reported = today, event = "case")

deaths_removed <- mutate(deaths_removed, status = -1, reported =today, event = "death")
deaths_added <- mutate(deaths_added, status = 1, reported = today, event = "death")

