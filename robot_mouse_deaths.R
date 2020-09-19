# executable ~/Commands/adhs_extract_deaths

now <- Sys.time()
glue::glue("{now}: started azdhs_extract_deaths.\n\n")

setwd(dir = "~/Programming/R/COVID/")
source("robot_mouse_functions.R")

browser <- "chrome"
# use medium window for deaths
size <- "medium"
# death curve
curve <- "death"

index_url <- "https://azdhs.gov/preparedness/epidemiology-disease-control/infectious-disease-epidemiology/covid-19/dashboards/index.php"
# note: when dashboards are viewed through main page, tooltip values are hidden
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

###################### PREPARE TO EXTRACT DEATH DATA ###########################
now <- Sys.time()
glue::glue("{now} preparing to extract death data.\n\n")

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



bar_width <- params$bar_width
x_step <- params$x_step
dims <- params$dims
# in the event you had to find mid_graph...
mid_graph_new <- params$mid_graph
if (! all(mid_graph_new == mid_graph)){
    message("gather_params returned a different mid_graph position")
}

# place mouse at the mid x-point for the first death
start_tip <- place(driver,
                   target_date = first_death,
                   bar_width = bar_width,
                   curve = "death",
                   window_width = dims$width)

############################# EXTRACT DATA ####################################
now <- Sys.time()
glue::glue("{now} extracting death data.\n\n")

dates <- seq(from = first_death, to = last_date, by = 1)
DailyDeaths <- data.frame(date = dates, deaths = numeric(length(dates)))


# The average width of a date is bar_width
# Make step smaller than bar_width to account for:
#    a. You can only advance the mouse whole numbers of pixels
#    b. Bars are almost always not whole numbers of pixels wide
# Number of steps needed in this strategy, the number of average widths
# from the first bar to the last bar, divided by the distance in a step

# this is in fact one more than you need to move, but you are extracting
# first, then moving
n <- ceiling((length(dates) * bar_width) / x_step)


for (i in seq_len(n)){
    # Loop starts on a bar. Extract first, then move
    tip <- extract_tooltip(driver)
    tip_date <- attr(tip, "date")
    DailyDeaths[DailyDeaths$date == tip_date,2] <- attr(tip, "number")
    
    # For tips that exist, have you reached the end?
    if (! is.null(tip_date) && tip_date == last_date) break
    
    move_mouse(driver, x = x_step, y = 0)
}

###################### IDENTIFY DATES WITH MISSING DEATHS ######################
now <- Sys.time()
glue::glue("{now}: identifying dates with missing deaths.\n\n")

# The unpredictable error here is that you might miss a bar, typically
# when there are 0 death days surrounding it. If you do, it will show up as a
# day with 0 deaths. Find the 0 death days
no_deaths <- filter(DailyDeaths, deaths == 0) %>%
    select(date) %>%
    getElement("date")


# Compare them to the expected 0 death days:
# 2020-03-18, now more than 120 days out. Unlikely to be assigned a death.
#   Do not check 2020-03-18

check_dates <- no_deaths[ no_deaths != "2020-03-18" ]
l <- length(check_dates)
glue::glue("Found {l} dates that may be missing deaths.\n")

########################### FIND MISSING DATES #################################
if (l > 0){
    now <- Sys.time()
    # prints, it's the last line of the block
    glue::glue("{now} looking for deaths on missing dates.\n\n")
} else {
    glue::glue("No dates to evaluate for missing deaths.\n\n")
}

# Two categories of 0 death days are common, but should still be checked
# `last_date`, will often have no deaths, due to reporting lag.
#   If `last_date` is missing, write a message to the log, visually confirm
# Some of the prior 7 days will sometimes have no deaths, due to reporting lag
#   Look for these if they are missing. Write a message to the log.
# As epidemic improves, days with 0 deaths NOT due to reporting lag will occur.
#   Adjust your validation code accordingly.

# Look for a bar at last_date
if (last_date %in% check_dates) {
    now <- Sys.time()
    print(glue::glue("Checking {last_date}.\n"))
    last_known_death <- filter(DailyDeaths, deaths > 0) %>%
        slice(n()) %>%
        select(date) %>%
        getElement("date")
    
    # if it's not already there,
    # place mouse on the last bar you found with a death
    
    # find out where you are
    tip <- extract_tooltip(driver)
    # test is TRUE if you're not over a bar, or it's not last_known_death
    # NOTE: use || to short circuit
    test <- ! is_daily_bar(tip, curve = curve) || 
        attr(tip, "date") == last_known_death
    if (test){
        # now find_date puts the mouse on mid_graph if it's 
        # supplied
        find_date(driver,
                  target_date = last_known_death,
                  bar_width = bar_width,
                  window_width = dims$width,
                  curve = curve,
                  mid_graph = mid_graph)
    }
    # Mouse is now positioned at last_known_death

    # move by half a step to the right until you either
    # don't detect a bar, or find `last_date`
    # where will you be at the end of this loop? 
    # One of two places
    #   1.  tip_date == last_date evaluates FALSE:
    #       break is encountered, you are at `last_date`
    #   2. ! is.null(tip) evaluates FALSE
    #       while expression terminates, you are just passed
    #       last_known_death
    while(! is.null(tip <- extract_tooltip(driver))){
        # evaluate the tip first
        tip_date <- attr(tip, "date")
        # tip_date will be valid after while terminates,
        # because code block isn't executed unless tip exists
        
        # You've extracted the tip here already, use it now
        if (tip_date == last_known_death){
            last_known_death_position <- attr(tip, "position")
        }
        
        # if it's last_date, you're done. Assign and get out
        if (tip_date == last_date){
            DailyDeaths[DailyDeaths$date == tip_date,2] <- attr(tip, "number")
            break
        }
        # if it's not, move half a step and check again
        move_mouse(driver, x = x_step%/% 2, y = 0)
        # while will look for a new tip, and see if it's null
    }

    
    # provided while expression evaluated true at least once:
    #   tip_date is valid
    #   tip is not (tip is NULL)
    # while expression SHOULD evaluate true at least once, 
    #   as you move to last_known_date first

    if (tip_date != last_date){
        fail <- TRUE
        if (last_known_death < (last_date - 1)){
            move_mouse_to(driver, x = last_known_death_position)
            # DON'T USE mid_graph. You're at lkdp
            fail <- as.logical(find_date(driver, 
                                         target_date = last_date, 
                                         bar_width = bar_width,
                                         window_width = dims$width,
                                         curve = curve))
        }
        if (fail){
            print(glue::glue("No deaths reported for {last_date}.\n",
                   "This is not uncommon. Confirm with visual inspection."))
            check_dates <- check_dates[check_dates != last_date]
            l <- length(check_dates)
            print(glue::glue("{l} additional dates may be missing deaths."))
        } else {
            tip <- extract_tooltip(driver)
            tip_date <- attr(tip, "date")
            DailyDeaths[DailyDeaths$date == tip_date,2] <- attr(tip, "number")
        }
    }
    # moved this up to inside the last_date is in check_dates block
    # remove last_date from no_deaths, if it's there

}

corrected_dd <- NULL
for (i in seq_len(3)){
    # l is the number of dates to check
    # if you don't have any, you can stop
    if (l == 0) break
    
    if (i == 1){
        # METHOD 1
        now <- Sys.time()
        print(glue::glue("{now} using METHOD 1 to find deaths", 
                         " on missing dates.\n\n"))
        # Correct for missing bars due to irregular bar widths
        # go directly to the expected position of the dates
        # Don't refresh. It crashes. driver$refresh()
        # mid_graph is passed directly to find_date
        # no need to move in a separate step
        corrected_dd <- fix_missing(driver,
                                    missing_dates = check_dates,
                                    bar_width = bar_width,
                                    window_width = dims$width,
                                    df = DailyDeaths,
                                    mid_graph = mid_graph)
        
    } else if (i == 2){
        # METHOD 2
        now <- Sys.time()
        print(glue::glue("{now} using METHOD 2 to find deaths",
                         " on missing dates.\n\n"))
        # Assign check_dates to their respective sundays
        # and get those dates that are alone in a week
        can_fix <- assign_sundays(first_death,
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
        
        corrected_dd <- fix_missing_by_wk(driver,
                                          window_width = dims$width,
                                          mid_graph = mid_graph,
                                          df = corrected_dd,
                                          wks_missing_1 = can_fix,
                                          curve = "death")
        
        # reset the bin width to daily
        drill_down(driver, height = dims$height, curve = curve, mid_graph = mid_graph)
        
    } else if (i == 3){
        # METHOD 3:
        now <- Sys.time()
        print(glue::glue("{now} using METHOD 3 to find deaths",
                         " on missing dates.\n\n"))
        # NOTE: firefox has been buggy. Don't switch browsers. Just dimensions
        
        driver <- open_headless_browser(browser = browser, curve = curve)
        position <- c(x = 0, y = 0)
        mid_graph <- get_saved_position(curve = curve,
                                        browser = browser,
                                        size = "large")
        params <- gather_params(driver,
                                size = "large",
                                curve = curve,
                                mid_graph = mid_graph)
        
        bar_width <- params$bar_width
        x_step <- params$x_step
        dims <- params$dims
        mid_graph <- params$mid_graph
        
        corrected_dd <- fix_missing(driver,
                                    missing_dates = check_dates,
                                    bar_width = bar_width,
                                    window_width = dims$width,
                                    df = corrected_dd,
                                    mid_graph = mid_graph)
        
    }
    # Have you fixed the problem?
    no_deaths <- filter(corrected_dd, deaths == 0) %>%
        select(date) %>%
        getElement("date")
    check_dates <- no_deaths[! (no_deaths %in%
                                    c(as.Date("2020-03-18"), last_date))]
    l <- length(check_dates)
    # prints, last line of block
    glue::glue("Found {l} dates that may be missing deaths.\n")
}

############################ WRAP UP AND WRITE ################################
now <- Sys.time()
glue::glue("{now} reporting dates to confirm and writing death data.\n\n")
if (! is.null(corrected_dd)) DailyDeaths <- corrected_dd
no_deaths <- filter(DailyDeaths, deaths == 0) %>%
    select(date) %>%
    getElement("date")

# Alert to visually confirm last_date (if it is missing) and any other not found
confirm_dates <- no_deaths[ no_deaths != "2020-03-18" ]
l <- length(confirm_dates)
glue::glue("****************************** VISUALLY CONFIRM",
           " ********************************\n",
           "*                                       ",
           "                                       *\n",
           "*    Found {l} dates that may be missing deaths.",
           "                                 *\n")
glue::glue("*    {confirm_dates}",
           "                                                            ",
           "    *\n")
glue::glue("*                                       ",
           "                                       *\n",
           "****************************** VISUALLY CONFIRM",
           " ********************************\n")


s <- sum(DailyDeaths$deaths)
glue::glue("Found {s} total deaths.\n",
           "This corresponds to the sum of deaths on the death dashboard.\n",
           "It is typically ~10-20 less than",
           " the value reported on the summary dashboard.\n\n",
           "Some reported deaths may require further investigation",
           " to verify date of death.\n")
filename <- paste0("AZDD", Sys.Date(), ".rds")
saveRDS(DailyDeaths, filename)

######################## CAPTURE CONFIRMED - PROBABLE ##########################
file1 <- paste0("AZcases_screenshot", Sys.Date(), ".png")
file2 <- paste0("AZdeaths_screenshot", Sys.Date(), ".png")
now <- Sys.time()
glue::glue("{now} capturing confirmed and probable proportions of",
           " cases and deaths.\n")
glue::glue("Image in tooltip may not render.\n")
glue::glue("Verify {file1} and {file2}.\n\n")

# get a screenshot of the summary
# table and tooltip values are.... images without alt text. Seriously, wtf
driver$close()
driver$quit()
curve = "index"
driver <- open_headless_browser(browser = browser, curve = curve)
Sys.sleep(2)
NumberElem <- driver$findElements(using = "css selector",
                                  value = "[tb-test-id^=Number]")
Sys.sleep(2)
driver$mouseMoveToLocation(webElement = NumberElem[[1]])
driver$mouseMoveToLocation(x = 0, y = 30)
Sys.sleep(1)

driver$screenshot(file = file1)

driver$mouseMoveToLocation(webElement = NumberElem[[2]])
driver$mouseMoveToLocation(x = 0, y = 30)
Sys.sleep(1)
driver$screenshot(file = file2)

