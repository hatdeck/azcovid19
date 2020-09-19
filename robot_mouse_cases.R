
now <- Sys.time()
glue::glue("\n\n\n{now}: started adhs_extract_cases.\n\n")

setwd(dir = "~/Projects/COVID/")
source("robot_mouse_functions.R")

browser <- "chrome"
# use large window size for cases.
size <- "large"
# case curve
curve <- "case"

index_url <- "https://azdhs.gov/preparedness/epidemiology-disease-control/infectious-disease-epidemiology/covid-19/dashboards/index.php"
# when dashboards are viewed through main page, tooltip values are hidden
# use tableau.azdhs.gov source to extract values from tooltips
summary_url <- "https://tableau.azdhs.gov/views/COVID-19Summary/Overview2?:embed=y&:showVizHome=no&:host_url=https%3A%2F%2Ftableau.azdhs.gov%2F&:embed_code_version=3&:tabs=no&:toolbar=no&:showAppBanner=false&:display_spinner=no&iframeSizedToWindow=true&:loadOrderID=5"
case_url <- "https://tableau.azdhs.gov/views/EpiCurve/EpiCurveDashboard?:embed=y&:showVizHome=true&:host_url=https%3A%2F%2Ftableau.azdhs.gov%2F&:embed_code_version=3&:tabs=no&:toolbar=no&:showAppBanner=no&:display_spinner=no&iframeSizedToWindow=true&:loadOrderID=1"
death_url <- "https://tableau.azdhs.gov/views/COVID-19Deaths/Deaths?:embed=y&:showVizHome=true&:host_url=https%3A%2F%2Ftableau.azdhs.gov%2F&:embed_code_version=1&:tabs=no&:toolbar=no&:showAppBanner=no&:display_spinner=no&iframeSizedToWindow=true&:loadOrderID=0"
test_url <- "https://tableau.azdhs.gov/views/ELRcovid/LaboratoryTestingExternalDraft?:embed=y&:showVizHome=no&:host_url=https%3A%2F%2Ftableau.azdhs.gov%2F&:embed_code_version=3&:tabs=no&:toolbar=no&:showAppBanner=false&:display_spinner=no&iframeSizedToWindow=true&:loadOrderID=1"

first_case <- as.Date("2020-01-22")
first_death <- as.Date("2020-03-17")
first_pcr <- as.Date("2020-02-03")
first_ab <- as.Date("2020-04-09")
last_date <- Sys.Date() - 1

###################### PREPARE TO EXTRACT CASE DATA ###########################
now <- Sys.time()
glue::glue("{now}: preparing to extract case data.\n\n")

# get previously saved mid_graph position for a speed up
# needs to include browser and window size information
mid_graph <- get_saved_position(curve = curve,
                                browser = browser,
                                size = size)

# set position and driver in .GlobalEnv
position <- c(x = 0, y = 0)
driver <- open_headless_browser(browser = browser, curve = curve)

# set dimensions and get parameters for today's graph
params <- gather_params(driver,
                        browser = browser,
                        size = size, # this is where you set dimensions
                        curve = curve,
                        mid_graph = mid_graph)
# THIS IS MUCH SLOWER FOR CASES THAN FOR DEATHS.
# GOOD PLACE TO LOOK FOR IMPROVEMENT IF NEEDED

bar_width <- params$bar_width
x_step <- params$x_step
dims <- params$dims
# in the event you had to find mid_graph
mid_graph_new <- params$mid_graph
if (! all(mid_graph_new == mid_graph)){
    message("gather_params returned a different mid_graph position")
}

# place mouse at the mid x-point for the first case
start_graph <- place(driver,
                     target_date = first_case,
                     bar_width = bar_width,
                     curve = curve,
                     window_width = dims$width)

############################# EXTRACT DATA #####################################
now <- Sys.time()
glue::glue("{now} extracting case data.\n\n")

dates <- seq(from = first_case, to = last_date, by = 1)
DailyCases <- data.frame(date = dates, cases = numeric(length(dates)))


# The average width of a date is bar_width
# Make step smaller than bar_width to account for:
#    a. You can only advance the mouse whole numbers of pixels
#    b. Bars are almost always not whole numbers of pixels wide
# Number of steps needed in this strategy, the number of average widths
# from the first bar to the last bar, divided by the distance in a step

# this is in fact one more than you need to move, but you are extracting
# first, then moving
n <- ceiling((length(dates)* bar_width) / x_step)


for (i in seq_len(n)){
    # Loop starts on a bar. Extract first, then move
    tip <- extract_tooltip(driver)
    tip_date <- attr(tip, "date")
    DailyCases[DailyCases$date == tip_date,2] <- attr(tip, "number")
    
    # don't run a test on the tip_date unless it exists
    if (! is.null(tip_date)){
        # if you've reached the last date, you're done
        # Prevents moving the mouse after the last extraction
        if (tip_date == last_date) break
    }
    
    move_mouse(driver, x = x_step, y = 0)
}

###################### VALIDATE and IDENTIFY MISSING DATA ######################
now <- Sys.time()
glue::glue("{now}: identifying dates with missing cases.\n\n")

# The unpredictable error here is that you might miss a bar, typically
# somewhere in March. If you do, it will show up as a day with 0 cases.
# find the 0 case days
no_cases <- filter(DailyCases, cases == 0) %>%
    select(date) %>%
    getElement("date")


# Compare them to the 0 case days on your reference date (which you have
# manually confirmed)

# Missing bars are typically low case days near true 0 case days
# Once we start seeing days with 0 reported cases again,
# use a different standard


ref_date <- as.Date("2020-08-20")
glue::glue("\nREFERENCE DATE: {ref_date}\n\n")
glue::glue("Dates that have cases on the curve posted on {ref_date} are",
           " expected to\nhave cases today. This is your reference date.\n\n")
ref_file <- paste0("AZDC", ref_date, ".rds")

DCref <- readRDS(ref_file)
none_ref <- filter(DCref, cases == 0) %>%
    select(date) %>%
    getElement("date")



check_dates <- no_cases[! (no_cases %in% none_ref)]
l <- length(check_dates)
glue::glue("{l} dates with expected cases have none.\n\n")

# Identify when early cases are added that are not on your reference date.
# When this happens, you may need to change your reference date.

# if there are dates (prior to ref_date) that don't have cases in DCref but
# do in DailyCases, consider changing your reference day
check_ref <- no_cases[no_cases < ref_date]
new_reference <- none_ref[! (none_ref %in% check_ref)]
lref <- length(new_reference)
if (lref){
    glue::glue("{lref} cases added to a date that had none on {ref_date}.\n",
               "Consider setting a new reference date.\n")
    glue::glue("Cases added to: {new_reference}\n\n")
}

########################### FIND MISSING DATES #################################
if (l > 0){
    now <- Sys.time()
    glue::glue("{now}: looking for cases on missing dates.\n\n")
} else {
    glue::glue("No dates to evaluate for missing cases.\n\n")
}


# You have 3 methods for finding missing bars
# 1. Go directly to the date (solves for inconsistent width)
#    Method 1 often finds missing bars
# 2. Change the bin width to weeks, look for the difference
#    between sum for the week in extracted data and report for
#    the week on the graph.
#    Method 2 can solve for missing alone in a week
# 3. Restart the browser with different dimensions and look again

corrected_dc <- NULL
for (i in seq_len(3)){
    # l is the number of dates to check.
    # if you don't have any, you can stop
    if (l == 0) break
    
    if (i == 1){
        # METHOD 1:
        now <- Sys.time()
        print(glue::glue("{now}: Using METHOD 1 to find cases on",
                         " missing dates.\n\n"))
        # Correct for missing bars due to irregular bar widths
        # go directly to the expected position of the dates
        # Don't refresh. It crashes. driver$refresh()
        # mid_graph is passed directly to find_date
        # no need to move in a separate step
        corrected_dc <- fix_missing(driver,
                                    missing_dates = check_dates,
                                    bar_width = bar_width,
                                    window_width = dims$width,
                                    df = DailyCases,
                                    mid_graph = mid_graph)
        
    } else if (i == 2){
        # METHOD 3:
        now <- Sys.time()
        print(glue::glue("{now}: Using METHOD 2 to find cases on",
                         " missing dates.\n\n"))
        # NOTE: firefox has been buggy. Don't switch browsers. Just dimensions
        
        driver <- open_headless_browser(browser = browser, curve = curve)
        position <- c(x = 0, y = 0)
        mid_graph <- get_saved_position(curve = curve,
                                        browser = browser,
                                        size = "medium")
        params <- gather_params(driver,
                                size = "medium",
                                curve = curve,
                                mid_graph = mid_graph)
        
        bar_width <- params$bar_width
        x_step <- params$x_step
        dims <- params$dims
        mid_graph <- params$mid_graph
        
        corrected_dc <- fix_missing(driver,
                                    missing_dates = check_dates,
                                    bar_width = bar_width,
                                    window_width = dims$width,
                                    df = corrected_dc,
                                    mid_graph = mid_graph)
        
    } else if (i == 3){
        # METHOD 3
        # Yes, this is faster than METHOD 2, but it needs to go last
        # This is how you catch stragglers
        now <- Sys.time()
        print(glue::glue("{now}: Using METHOD 3 to find cases on",
                         " missing dates.\n\n"))
        # Assign check_dates to their respective sundays
        # and get those dates that are alone in a week
        can_fix <- assign_sundays(first_case,
                                  last_date,
                                  check_dates)
        
        # Switch graph bin width to week
        # be sure to switch back once you're done. Especially if you want to
        # put this code block in a function
        drill_up(driver,
                 height = dims$height,
                 curve = curve,
                 mid_graph = mid_graph)
        
        # Go through each of check_dates that is alone in a week
        # find the difference between weekly events in data missing that
        # check date, and weekly events on the graph. This is the value
        # for the missing date.
        
        corrected_dc <- fix_missing_by_wk(driver,
                                          window_width = dims$width,
                                          mid_graph = mid_graph,
                                          df = corrected_dc,
                                          wks_missing_1 = can_fix)
        
        # reset the bin width to daily
        drill_down(driver, 
                   height = dims$height, 
                   curve = curve, 
                   mid_graph = mid_graph)
        
    }
    # Have you fixed the problem?
    no_cases <- filter(corrected_dc, cases == 0) %>%
        select(date) %>%
        getElement("date")
    check_dates <- no_cases[! (no_cases %in% none_ref)]
    l <- length(check_dates)
    print(glue::glue("{l} dates with expected cases have none."))
    # if you are still missing dates, you need to try another method
}


############################ WRAP UP AND WRITE #################################
now <- Sys.time()
glue::glue("{now} reporting dates to confirm and writing case data.\n\n")
if (! is.null(corrected_dc)) DailyCases <- corrected_dc
no_cases <- filter(DailyCases, cases == 0) %>%
    select(date) %>%
    getElement("date")
check_dates <- no_cases[! (no_cases %in% none_ref)]
l <- length(check_dates)
if (l == 0) check_dates <- "0000-00-00"
glue::glue("****************************** VISUALLY CONFIRM",
           " ********************************\n",
           "*                                       ",
           "                                       *\n",
           "*    Found {l} dates that may be missing cases. ",
           "                                 *\n")
glue::glue("*    {check_dates}",
           "                                                            ",
           "     *\n")
glue::glue("*                                       ",
           "                                       *\n",
           "****************************** VISUALLY CONFIRM",
           " ********************************\n")

s <- sum(DailyCases$cases)
print(glue::glue("Found {s} total cases. ",
                 "This should match the cases on the summary dashboard."))


filename <- paste0("AZDC", Sys.Date(), ".rds")
saveRDS(DailyCases, filename)

driver$close()
driver$quit()
rm(list = ls())
gc()
