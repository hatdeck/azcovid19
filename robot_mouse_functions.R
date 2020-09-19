suppressMessages(expr = {
    library(dplyr, quietly = TRUE)
    library(RSelenium, quietly = TRUE)
    library(lubridate, quietly = TRUE)
    library(zoo, quietly = TRUE)
    library(tidyr, quietly = TRUE)
})

# A note on traversing bars in these Tableau visualizations to extract tooltip 
# text:

# This is an awkward process. RSelenium remoteDriver is slow not especially
# flexible, and missing some expected features.  I'm using this, instead of
# other web scraping tools because the relevant data is not accessible without
# interacting with the visualization using a mouse. 

# In addition to hidden elements, the coordinate system has been a challenge. 
# The elements we need to interact with in the Tableau visualization do not 
# line up with the window coordinate system. Though there are windowSizes 
# that align them on a given date, it doesn't stay that way as the dashboard 
# is updated.

# So what is the challenge? Though we can only move the mouse whole number 
# coordinate units (e.g., you can't move 3.14 units to the right. You can only
# move 3 or 4), the bars in the Daily Cases and Daily Death bar charts do not 
# have whole number widths. This led me to  a number of contortions, as you'll
# see. There are a variety of approaches to solve this problem. I chose the 
# one that occurred to me and worked. I'm open to suggestions for more 
# intuitve, efficient, or maintainable approaches.

# A note on side effects:
# 
# Moving the mouse with the remoteDriver class is a side effect. It is not
# encapsulated in the environment of, e.g., a function that calls the  
# mouseMoveToLocation method. Because of this, I track position in the global 
# environment. Functions that move the mouse will get position from the global 
# environment, and update position in the global environment. This is 
# typically something I avoid, but it seems to make sense here.

# Finding a bar, finding the base, finding the width, and then moving to the 
# first bar:
# 
# Functions here are intended to allow a script to find the first bar without
# supervision. As the AZDHS dashboard continues reporting, the reporting 
# period will change. This impacts the position and dimensions of the bars. 
# Finding that first bar, and estimating an average width requires a good 
# amount of the overall time it takes this script to run. 

# A known position for the base of a bar midway through the graph is saved
# in a file in this directory. The function get_saved_position  loads that
# file and extracts the position. Changes to the graph will make the data
# associated with this position change, but, unless there is a major overhaul
# to the dashboard, the position will continue to be at the base of a bar
# midway through the graph. Using this position improves the time performance
# of prep tasks.





extract_tooltip <- function(remoteDriver){
    # extracts tooltip text if it exists
    
    # return value: if tooltip text exists, object of class tooltip with 
    #               attributes based on the text and position
    #               otherwise, returns NULL

    webElem <- tryCatch(suppressMessages(remoteDriver$findElement(using = "css selector", 
                                                                  value = ".tab-ubertipTooltip")), 
                        error = function(e) NULL)
    if (is.null(webElem)) return(NULL)
    tip <- unlist(webElem$getElementText())
    if (grepl("^Date:.*\\d{1,2}, \\d{4}.*deaths: [0-9,]+$", tip)){
        attributes(tip) <- list(class = c("tooltip", "daily_death"),
                                date = mdy(sub("^Date: (.*\\d{1,2}, \\d{4}).*$", "\\1", tip)),
                                number = as.numeric(gsub(",",
                                                         "", 
                                                         sub("^.*deaths: ([0-9,]+)$", 
                                                             "\\1", 
                                                             tip))),
                                position = get("position", 1))
        
    } else if (grepl("^Date:.*\\d{4}.*deaths: [0-9,]+$", tip)){
        attributes(tip) <- list(class = c("tooltip", "monthly_death"),
                                date = mdy(sub("^Date: (.{3,9}) (\\d{4}).*$", "\\1 1, \\2", tip)),
                                number = as.numeric(gsub(",",
                                                         "", 
                                                         sub("^.*deaths: ([0-9,]+)$", 
                                                             "\\1", 
                                                             tip))),
                                position = get("position", 1))
        
    } else if (grepl("^Collection Date: .*\\d{1,2}, \\d{4}.*Cases: [0-9,]+$", tip)){
        attributes(tip) <- list(class = c("tooltip", "daily_case"),
                                date = mdy(sub("^Collection Date: (.*\\d{1,2}, \\d{4}).*$", "\\1", 
                                               tip)),
                                number = as.numeric(gsub(",",
                                                         "", 
                                                         sub("^.*Cases: ([0-9,]+)$", 
                                                             "\\1", 
                                                             tip))),
                                position = get("position", 1))
        
    } else if (grepl("^Collection Date:.*\\d{4}.*Cases: [0-9,]+$", tip)){
        attributes(tip) <- list(class = c("tooltip", "monthly_case"),
                                date = mdy(sub("^Collection Date: (.{3,9}) (\\d{4}).*$", 
                                               "\\1 1, \\2", 
                                               tip)),
                                number = as.numeric(gsub(",", 
                                                         "", 
                                                         sub("^.*Cases: ([0-9,]+)$", 
                                                             "\\1", 
                                                             tip))),
                                position = get("position", 1))
        
    }
    else {
        class(tip) <- "tooltip"
    }
    tip
}

get_dimensions <- function(size = c("medium", "large"), 
                           browser = c("chrome", "firefox"),
                           curve = c("case", "death")){
    # size: character, medium or large
    
    # return value: one of two lists containing 3 dimensions
    #               width and set_height are used to size the window
    #               height is the in bounds vertical distance
    
    # dimensions work with either dashboard, either browser (one exception), 
    # are less likely to result in bars that do not render

    size <- match.arg(size)
    browser <- match.arg(browser)
    curve <- match.arg(curve)
    if (curve == "death" & browser == "firefox" & size == "large"){
        stop("Firefox does not support a death curve with large dimensions. ",
             "Use size = \"medium\"")
    }
    switch(size,
           medium = return(list(width = 1536, 
                                set_height = 1250,
                                height = 1176)),
           large = return(list(width = 2259, 
                               set_height = 1506, 
                               height = switch(browser, 
                                               chrome = 1390,
                                               firefox = 1430))))
}

is_daily_bar <- function(tooltip, curve = c("case", "death")){
    curve <- match.arg(curve)
    if (is.null(tooltip)) return (FALSE)
    curve_class <- switch(curve,
                          case = "daily_case",
                          death = "daily_death")
    # explicitly set which to FALSE to be clear that value is logical(1)
    # specifying "tooltip" is not be strictly necessary, but you may
    # want to do more with this object
    inherits(tooltip, what = c(curve_class, "tooltip"), which = FALSE)
}

# is_daily_death_bar <- function(tooltip){
#     # tooltip: result of extract_tooltip()
#     # result: logical, is mouse positioned over a bar in the Daily Death chart
#     # Note: extract_tooltip() returns NULL if there is no tooltip text
#     # is_daily_x_bar functions should return FALSE for a NULL tooltip
#     if (is.null(tooltip)) return(FALSE)
#     inherits(tooltip, c("daily_death", "tooltip"))
# }
# 
# is_daily_case_bar <- function(tooltip){
#     # tooltip: result of extract_tooltip()
#     # result: logical, is mouse positioned over a bar in the COVID-19 Cases by Day chart
#     # Note: extract_tooltip() returns NULL if there is no tooltip text
#     # is_daily_x_bar functions should return FALSE for a NULL tooltip
#     if (is.null(tooltip)) return(FALSE)
#     inherits(tooltip, c("daily_case", "tooltip"))
# }

compare_tip_date <- function(tip1, tip2){
    # tip1: return value from extract_tooltip. Expects either NULL , or an 
    #       object of class tooltip with a date attribute
    # tip2: as with tip1, a second return value from extract_tooltip
    
    # return value: logical, length 1, do tip1 and tip2 have the same date
    # 
    # edge case not accounted for, tooltip is present, with no date attribute
    # occurs if DOM contains a match to css selector ".tab-ubertipTooltip" 
    # that does not follow the pattern of a case or death bar. (e.g., mouse
    # position is over "i" icon). Expected edge case return value: logical(0) 
    if (is.null(tip1)){
        if (is.null(tip2)) return(TRUE)
        else return(FALSE)
    } else {
        if (is.null(tip2)) return(FALSE)
        else return (attr(tip1, "date") == attr(tip2, "date"))
    }
}

find_date <- function(remoteDriver, 
                      target_date, 
                      bar_width, 
                      window_width, 
                      curve = c("case", "death"),
                      mid_graph = NULL){
    min_x <- 0.3*window_width
    max_x <- 0.98*window_width
    curve <- match.arg(curve)
    
    # if a mid_graph position is supplied, use it
    if (! is.null(mid_graph)){
        move_mouse_to(remoteDriver, x = mid_graph)
    }
    
    wm_list <- wiggle_mouse(remoteDriver, curve = curve)
    if (! wm_list$daily_bar){
        if (is.null(mid_graph)){
            stop("Mouse position is not on the graph or tooltip is not",
                 " rendering. No mid_graph position was supplied.")
        } else {
            stop("Position from \"mid_graph\" is not on the graph,",
                 " or tooltip is not rendering.")
        }
    } 
    current_date <- attr(wm_list$tooltip, "date")
    date_diff <- as.numeric(target_date - current_date)
    
    # If you have to go through this code block more than twice
    # it's a special case (e.g., target_date is in a cluster of 0 event dates)
    # Just got through it twice
    for (i in seq_len(2)){
        jump <- round(date_diff * bar_width)
        move_mouse(remoteDriver, x = jump, y = 0)
        
        tip <- extract_tooltip(remoteDriver)
        
        if (is.null(tip) || ! is_daily_bar(tip)) break
        
        current_date <- attr(tip, "date")
        date_diff <- as.numeric(target_date - current_date)
        
        if (date_diff == 0){
            print(glue::glue("Found {target_date}."))
            return(0)
        }
    }
    # tip was extracted last in the above for loop, with no conditions
    # it will be define,d though it may be null
    # If you need to move to the right, d is 1, to the left, d is -1
    pos_diff <- date_diff > 0
    d <- if(pos_diff) 1 else -1
    while (in_range(min_x, max_x) && not_there_yet(tip, target_date)){
        move_mouse(remoteDriver, x = d, y = 0)
        tip <- extract_tooltip(remoteDriver)
        if (is.null(tip) || ! is_daily_bar(tip, curve = curve)) next
        current_date <- attr(tip, "date")
        date_diff <- as.numeric(target_date - current_date)
        # check to see if the sign of date_diff has changd
        if ((date_diff > 0 ) != pos_diff){
            print(glue::glue("Moved passed {target_date} while advancing 1 unit at a time.
                             {target_date} appears to be missing."))
            return(-1)
        }
        if (date_diff == 0) return(0)
    }
    
    if (! in_range(min_x, max_x)){
        print(glue::glue("Moved out of range while searching for {target_date}.\n",
                         "{target_date} appears to be missing."))
        return(1)
    }
}

# Helpers for find_date
in_range <- function(x_min, x_max){
    # logical: are you still between x_min and x_max
    x <- get("position", 1)['x']
    x_min <= x && x <= x_max
}

not_there_yet <- function(tooltip, target){
    # logical: you haven't reached target yet
    if (is.null(tooltip)) return(TRUE)
    current <- attr(tooltip, "date")
    target != current
}

find_date_legacy <- function(remoteDriver, 
                      target_date, 
                      bar_width, 
                      window_width, 
                      curve = c("case", "death"),
                      mid_graph = NULL){
    # Note: out of bounds errors from move_mouse will not be caught in the first step.
    #       Consider adding error checking.
    #       If the first step DOES send you out of bounds, you likely have an error
    #       in your position, or one of the arguments here.
    
    min_x <- 0.3*window_width
    max_x <- 0.98*window_width
    curve <- match.arg(curve)
    
    # if a mid_graph position is supplied, use it
    if (! is.null(mid_graph)){
        move_mouse_to(remoteDriver, x = mid_graph)
    }
    
    wm_list <- wiggle_mouse(remoteDriver, curve = curve)
    if (! wm_list$daily_bar){
        if (is.null(mid_graph)){
            stop("Mouse position is not on the graph or tooltip is not",
                 " rendering. No mid_graph position was supplied.")
        } else {
            stop("Position from \"mid_graph\" is not on the graph,",
                 " or tooltip is not rendering.")
        }
    } 
    current_date <- attr(wm_list$tooltip, "date")
    
    # 3 while loops that should probably be rewritten
    date_diff <- as.numeric(target_date - current_date)
    while (abs(date_diff) > 1){
        move_mouse(remoteDriver, x = round(date_diff * bar_width), y = 0)
        tip <- extract_tooltip(remoteDriver)
        if (is.null(tip)) break

        if (! is_daily_bar(tip, curve = curve)) break
        
        current_date <- attr(tip, "date")
        date_diff <- as.numeric(target_date - current_date)
        if (date_diff == 0) return(0)
    }
    while (target_date > current_date){
        move_mouse(remoteDriver, x = 1, y = 0)
        x <- get("position", 1)['x']
        if (x >= max_x | x <= min_x){
            print(glue::glue("Moved out of range while searching for {target_date}.
                             {target_date} appears to be missing."))
            return(1)
        }
        tip <- extract_tooltip(remoteDriver)
        if (is.null(tip)) next
        current_date <- attr(tip, "date")
        if (target_date == current_date) return(0)
        if (current_date > target_date) {
            print(glue::glue("Moved passed {target_date} while advancing 1 unit at a time.
                             {target_date} appears to be missing."))
            return(-1)
        }

    }
    while (target_date < current_date){
        move_mouse(remoteDriver, x = -1, y = 0)
        x <- get("position", 1)['x']
        if (x <= min_x | max_x <= x){
            print(glue::glue("Moved out of range while searching for {target_date}.
                             {target_date} appears to be missing."))
            return(1)
        }
        tip <- extract_tooltip(remoteDriver)
        if (is.null(tip)) next
        current_date <- attr(tip, "date")
        if (target_date == current_date) return(0)
        if (current_date < target_date) {
            print(glue::glue("Moved passed {target_date} while advancing 1 unit at a time.
                             {target_date} appears to be missing."))
            return(-1)
        }

    }
    

}

find_mid <- function(remoteDriver, curve = c("case", "death")){
    curve <- match.arg(curve)
    start_tip <- extract_tooltip(remoteDriver)
    if (! is_daily_bar(start_tip, curve = curve)){
        stop("Position mouse over a daily bar.")
    }

    while(compare_tip_date(start_tip, tip <- extract_tooltip(remoteDriver))){
        move_mouse(remoteDriver, x = 1, y = 0)
    }
    move_mouse(remoteDriver, x = -1, y = 0)
    right <- get("position", 1)
    move_mouse_to(remoteDriver, x = attr(start_tip, "position"))
    while(compare_tip_date(start_tip, tip <- extract_tooltip(remoteDriver))){
        move_mouse(remoteDriver, x = -1, y = 0)
    }
    move_mouse(remoteDriver, x = 1, y = 0)
    left <- get("position", 1)
    mid <- left + (right - left) %/% 2
    move_mouse_to(remoteDriver, x = mid)
    mid
    
}
    

open_headless_browser <- function(browser = c("chrome", "firefox"),
                                  curve = c("case", "death", "pcr", "serology", "index")){
    browser <- match.arg(browser)
    curve <- match.arg(curve)
    start_selenium(browser = browser)
    driver <- remoteDriver(port = 4445L, browserName = browser)
    switch(curve,
           case = {
               open_dashboard(driver, url = get("case_url", 1), wait = 3)
           },
           death = {
               open_dashboard(driver, url = get("death_url", 1), wait = 3)
           },
           pcr = {
               open_dashboard(driver, url = get("test_url", 1), wait = 3)
           },
           serology = {
               open_dashboard(driver, url = get("test_url", 1), wait = 3)
           },
           index = {
               open_dashboard(driver, url = get("summary_url", 1), wait = 3)
           })
    
    driver
}

open_alternate_browser <- function(remoteDriver, current_browser, curve = c("case", "death")){
    # closes current browser, reopens browser using alternate case
    # remoteDriver: object of class remoteDriver. This is the browser you want to close
    # current_browser: "chrome" or "firefox", whichever you were using previously
    remoteDriver$quit()
    curve <- match.arg(curve)
    choices <- c("chrome", "firefox")
    browser <- choices[choices != current_browser]
    driver <- open_headless_browser(browser, curve)
    driver
}

fix_missing <- function(remoteDriver, 
                        missing_dates, 
                        bar_width, 
                        window_width, 
                        df, 
                        mid_graph = NULL){
    # Use after run_alternate_browser, and finding a bar
    
    # remoteDriver: object of class remoteDriver, with a headless browser open
    #               to the AZDHS dashboard
    # missing_dates: date, vector of dates that are missing cases or deaths
    # bar_width: numeric, the estimated avg bar width over the graph
    # window_width: integer, the width returned by remoteDriver$getWindowSize()
    #               calculate this ahead of time to save computational cost
    # df: data.frame, the DailyCases or DailyDeaths data frame that is missing
    #     cases or deaths
    # return value: df, with the missing dates filled in if possible
    for (i in seq_along(missing_dates)){
        found <- find_date(remoteDriver, 
                  target_date = missing_dates[i], 
                  bar_width = bar_width, 
                  window_width = window_width,
                  mid_graph = mid_graph)
        if (found == 0){
            tip <- extract_tooltip(remoteDriver)
            tip_date <- attr(tip, "date")
            tip_number <- attr(tip, "number")
            if (tip_date == missing_dates[i]){
                print(glue::glue("Adding {tip_number} events to",
                                 " {tip_date}.\n\n"))
                df[df$date == attr(tip, "date"),2] <- attr(tip, "number") 
            } else {
                print(glue::glue("Extracted events at {tip_date}.",
                                " This does not match {missing_dates[i]}."))
            }
        } else {
            print(glue::glue("Could not find {missing_dates[i]}."))
        }
    }
    missing_values <- filter(df, date %in% missing_dates) %>% select(2)
    needed_fixing <- nrow(missing_values)
    fixed <- sum(missing_values[[1]] > 0)
    print(glue::glue("Values found for {fixed} of {needed_fixing} rows."))
    df
    
}

start_selenium <- function(browser = c("chrome", "firefox"), tag = "latest", name = "sel1"){
    browser <- match.arg(browser)
    image <- paste0("selenium/standalone-", browser, ":", tag)
    filter <- paste0("name=", name)
    test <- system2("docker", args = c("ps", "-q", "-f", filter), stdout = TRUE)
    if (length(test) > 0){
        # depends on using your convention where container is destroyed when stopped
        # if you started 'name' without the --rm flag, stop won't be enough
        system2("docker", args = c("stop", name))
        Sys.sleep(1)
    }
    system2("docker", args = c("run", "-d", "-p", "4445:4444", "--name", name, 
                               "--rm", image))
    
    
}

open_dashboard <- function(remoteDriver, url, wait = 2, attempts = 10){
    
    # if (! remoteDriver$getStatus()$ready) {
    #     print(glue::glue("remoteDriver is not ready. Waiting {wait} seconds..."))
    #     Sys.sleep(wait)
    # } else{
    #     print(glue::glue("remoteDriver is ready."))
    # }
    test_open <- tryCatch(remoteDriver$open(silent = TRUE), error = function(e) {
        print(glue::glue("Opening remoteDriver threw an error with the following message:",
                         "\n{conditionMessage(e)}\nWaiting {wait} seconds..."))
        Sys.sleep(wait)
        e
    })
    if (! inherits(test_open, "condition")) print(glue::glue("No error when opening remoteDriver."))
    i <- 0
    while (inherits(test_open, "condition")){
        test_open <- tryCatch(remoteDriver$open(silent = TRUE), error = function(e) {
            print(glue::glue("Opening remoteDriver threw an error with the following message:",
                             "\n{conditionMessage(e)}\nWaiting {wait} seconds..."))
            Sys.sleep(wait)
            e
        })
        # prevent infinite loop
        i <- i + 1
        if (i > attempts) break
        
    }
    if (inherits(test_open, "condition")) stop(attempts, " attempts to open remoteDriver failed.")
    # if (! remoteDriver$getStatus()$ready) {
    #     print(glue::glue("remoteDriver is not ready. Waiting {wait} seconds..."))
    #     Sys.sleep(wait)
    # }
    test_navigate <- tryCatch(remoteDriver$navigate(url),
                              error = function(e) {
                                  print(glue::glue("Navigating remoteDriver to url threw an ",
                                                   "error with the following message:",
                                                   "\n{conditionMessage(e)}",
                                                   "\nWaiting {wait} seconds..."))
                                  Sys.sleep(wait)
                                  e
                              })
    if (! inherits(test_navigate, "condition")) print(glue::glue("No error when navigating ",
                                                                 "remoteDriver to supplied url."))
    i <- 0
    while (inherits(test_navigate, "condition")) {
        test_navigate <- tryCatch(remoteDriver$navigate(url),
                                  error = function(e) {
                                      print(glue::glue("Navigating remoteDriver to url threw an ", 
                                                       "error with the following message:\n",
                                                       "{conditionMessage(e)}\n",
                                                       "Waiting {wait} seconds..."))
                                      Sys.sleep(wait)
                                      e
                                  })
        # prevent infinite loop
        i <- i + 1
        if (i > attempts) break
    }
    if (inherits(test_navigate, "condition")){
        stop(attempts, " attempts to navigate remoteDriver to url failed.")
    }
}


move_mouse <- function(remoteDriver, x, y = NULL){
    # remoteDriver: a remoteDriver created by RSelenium::remoteDriver()
    # x: either a numeric vector of length 2, with x and y coordinates, 
    #    or a numeric vector of length 1, with x coordinate
    # y: if present, numeric vector of length 1, with y coordinate.
    
    # return value: True if no error. All effects are side effects
    # effects: moves mouse. Updates position in .GlobalEnv. 
    
    # for simplicity sake, rather than taking and returning a position, 
    # we're just going to update position in the parent environment
    # check to see if position is defined in .GlobalEnv
    if (! exists("position", where = 1)) stop("Set position before moving the mouse.")
    
    # argument checking
    if (! inherits(remoteDriver, "remoteDriver")){
        stop("remoteDriver must be of class remoteDriver.")
    }
    if (! is.null(y)){
        if (! (is.numeric(x) & length(x) == 1 & is.numeric(y) & length(y) == 1) ) {
            stop("If a value is passed for y, x and y must be numeric vectors of length 1")
        }
    } else {
        if (! is.numeric(x) & length(x) == 2){
            stop("If there is no value for y, x must be a numeric vector of length 2.")
        }
    }
    
    # move the mouse. Note that, because the remoteDriver object has no data on its
    # position, there is no need to update that object
    # the method simply moves the mouse in the web driver
    if (is.null(y)){
        remoteDriver$mouseMoveToLocation(x[1], x[2])
        position <<- position + x
    }
    else{
        remoteDriver$mouseMoveToLocation(x, y)
        position <<- position + c(x, y) 
    }
    TRUE
}

move_mouse_to <- function(remoteDriver, x, y = NULL){
    # remoteDriver: object of class remoteDriver
    # x: either a length 2 numeric vector, containing x and y coordinates, or
    #    a length 1 numeric vector, containing the x coordinate
    # y: if provided, a length 1 numeric vector, containing the y coordinate
    
    # side effect: moves mouse to the specified coordinates
    # compare to move_mouse, which moves the mouse an x, y, offset
    # returns: logical, are you at the coordinate position you intended 
    start_position <- get("position", 1)
    if (is.null(y)){
        d <- x - start_position
        move_mouse(remoteDriver, d)
        return(get("position", 1) == x)
    } 
    else {
        d <- c(x, y) - start_position
        move_mouse(remoteDriver, d)
        return(get("position", 1) == c(x = x, y = y))
    }
    
}

find_window_height <- function(remoteDriver, height, quiet = TRUE){
    # the height reported by the remoteDriver methods getWindowSize()
    # or set by setWindowSize() is not all available for navigation
    # get the navigable y boundary of the window
    yd <- height
    # get the start position, so you can move back to it:
    start_position <- get("position", 1)
    # start at 0, 0
    move_mouse_to(remoteDriver, x = 0, y = 0)
    if (quiet){
        for (i in seq_len(ceiling(log(height, 2)))){
            in_bounds <- tryCatch(suppressMessages(move_mouse(remoteDriver, x = 0, y = yd)),
                                  error = function(e) FALSE,
                                  finally = yd <- yd %/% 2)
        }
    } else {
        for (i in seq_len(ceiling(log(height, 2)))){
            in_bounds <- tryCatch(move_mouse(remoteDriver, x = 0, y = yd),
                                  error = function(e) FALSE,
                                  finally = yd <- yd %/% 2)
        }        
    }
    boundary <- get("position", 1)
    # move back to where you started
    move_mouse_to(remoteDriver, x = start_position)
    # return the actual height
    boundary['y']
}

find_y_boundary <- function(remoteDriver, height, curve = c("case", "death")){
    curve <- match.arg(curve)

    start_tip <- get_check_tooltip(remoteDriver, curve = curve)
    lower <- height
    upper <- attr(start_tip, "position")['y']
    x_pos <- attr(start_tip, "position")['x']
    start_diff <- lower - upper
    for (i in seq_len(log2(start_diff))){
        guess <- upper + (lower - upper) %/% 2
        move_mouse_to(remoteDriver, x = x_pos, y = guess)
        tip <- extract_tooltip(remoteDriver)
        if (is_daily_bar(tip, curve = curve)){
            upper <- attr(tip, "position")['y']
        } else {
            lower <- get("position", 1)['y']
        }
    }
    i <- 0
    while (! is_y_boundary(remoteDriver, 
                           tip <- extract_tooltip(remoteDriver), 
                           curve = curve)){
        i <- i + 1
        print(glue::glue("{i}th time through the 'is_y_boundary' loop."))
    }
    tip
    
}

is_y_boundary <- function(remoteDriver, tooltip, curve = c("case", "death")){
    # COMPARE check_base
    # advances by 1 if FALSE, so you can use this in a while loop
    # yes, this is weird 
    curve = match.arg(curve)
    A <- is_daily_bar(tooltip, curve = curve)
    move_mouse(remoteDriver, x = 0, y = 1)
    B <- is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)
    if (A & ! B){
        move_mouse(remoteDriver, x = 0, y = -1)
        return(TRUE)
    } else {
        return(FALSE)
    }

}

# find_y_boundary_guess <- function(remoteDriver, height, curve = c("case", "death")){
#     guess <- switch(curve,
#                     case = round(0.86*height),
#                     death = round(0.55*height))
#     start_tip <- get_check_tooltip(remoteDriver, curve = curve)
#     lower <- height
#     upper <- attr(start_tip, "position")['y']
#     move_mouse_to(remoteDriver, x = 0, y = guess)
#     # 
#     if (is_daily_bar(tip <- extract_tooltip(remoteDriver, curve = curve))){
#         
#     }
#     
# }

find_y_boundary_legacy <- function(remoteDriver, step = 1L, curve = c("case", "death")){
    # remoteDriver: a remoteDriver created by RSelenium::remoteDriver()
    # step: whole number (default is integer, but not enforced)
    # return: tooltip at the boundary
    # effects: moves mouse to the edge of a bar. Updates position in .GlobalEnv. 
    
    curve <- match.arg(curve)
    # check tooltip, get it, extract the number
    start_tip <- get_check_tooltip(remoteDriver, curve = curve)
    n <- attr(start_tip, "number")
    
    # correct for steps that are too big, based on curve
    switch(curve,
           death = which_step <- c(n*3, step), 
           case = which_step <- c(n %/% 10, step))

    yd <-which_step[min(abs(which_step)) == abs(which_step)][1]
    # [1] is for the case where n*3 == step
    while (abs(yd) > 0.5){
        
        # If moving same direction as when you started, you're on the bar, moving off
        if ((yd * step) > 0 ){
            
            # while you're still on the bar, moving off, 
            while (is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)){
                # move toward the boundary (from on the bar)
                move_mouse(remoteDriver, x = 0, y = yd)

            }
        } else {
            # If you're moving in the opposite direction as when you started (yd * step isn't positive). 
            # You've gone passed the bar, moving back toward it.
            while (! is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)){
               # move toward the boundary (from on the bar)
                move_mouse(remoteDriver, x = 0, y = yd)

            }
        }
        # if the step is still > 1, halve the step and reverse direction
        yd <- if(abs(yd) > 1) round(-yd/2) else -yd/2
    }
    # Remember, after the last move, you flip the signs
    # SO... if you were on the bar, moving OFF (direction is same as when you started), 
    # Then the sign flipped (for a positive step, this gives you yd = -0.5)
    # final_move should get c(0, -1)
    
    # If you were off the bar moving ON (direction opposite of when you started)
    # Then the sign flipped (giving you yd = 0.5)
    # final_move should get c(0, 0)
    final_move <-  if ((yd * step) < 0) c(0, -step/step) else c(0,  0)

    move_mouse(remoteDriver, x = final_move)
    final_tip <- extract_tooltip(remoteDriver)
    final_tip
}



find_next_bar_get_tip <- function(remoteDriver, step = 1L){
    start_x <- extract_tooltip(remoteDriver)
    while (compare_tip_date(tip1 = next_x <- extract_tooltip(remoteDriver), tip2 = start_x)){
        move_mouse(remoteDriver, x = step, y = 0)
    }
    next_x
}

find_avg_width <- function(remoteDriver, window_width, step = 500L){

    # make sure you're not jumping off the chart
    x_step <- min(step, floor(window_width* 0.95) - get("position", 1)['x'])
    # start at the first pixel of first_bar
    first_bar <- find_next_bar_get_tip(remoteDriver)
    
    # move some number of pixels
    move_mouse(remoteDriver , x = x_step, y = 0)
    
    # get to the first pixel of last_bar
    last_bar <- find_next_bar_get_tip(remoteDriver)
    
    # average width will be pixel distance / time distance
    pixel_distance <- attr(last_bar, "position")['x'] - attr(first_bar, "position")['x']
    time_distance <- as.numeric(attr(last_bar, "date") - attr(first_bar, "date"))
    pixel_distance / time_distance
    
}

get_saved_position <- function(curve =c("case", "death"), 
                              browser = c("chrome", "firefox"), 
                              size = c("medium", "large"), 
                              file = "saved_tip_array.rds"){
    # browser may not matter
    curve <- match.arg(curve)
    browser <- match.arg(browser)
    size <- match.arg(size)
    tips <- readRDS(file = file)
    tips[, browser = browser, size = size, curve = curve]
}

get_check_tooltip <- function(remoteDriver, curve = c("case", "death")){
    # check for errors in a find boundary function
    # return value is the starting tooltip
    curve <- match.arg(curve)
    # argument checking
    if (! inherits(remoteDriver, "remoteDriver")) stop("remoteDriver must be of class remoteDriver.")
    
    # check that tooltip text exists and is the appropriate data
    # TODO: abstract this so that it can be used with cases as well as deaths
    tip <- extract_tooltip(remoteDriver)
    switch(curve,
            death = if (! is_daily_bar(tip, curve = curve)){
                stop("Mouse position must start on a bar in the \"COVID-19 deaths by Date\" chart.")
            },
            case = if (! is_daily_bar(tip, curve = curve)){
                stop("Mouse position must start on a bar in the \"COVID-19 Cases by Day\" chart.")
            })
    
    tip
} 

# find_case_bar <- function(remoteDriver, 
#                           width, 
#                           height,
#                           guess = c(0.5, 0.7),
#                           step_p = 0.1){
#     start <- round(guess * c(width, height))
#     step <- round(step_p*width)
#     move_mouse_to(remoteDriver, x = start)
#     distance <- floor(width *0.98 - get("position", 1)['x'])
#     for (i in seq_len(distance %/% step)){
#         if (is_daily_case_bar(tip <- extract_tooltip(remoteDriver))) break
#         move_mouse(remoteDriver, x = step, y = 0)
#     }
#     tip
# }



find_bar <- function(remoteDriver, 
                           width, 
                           height, 
                           curve = c("case", "death"),
                           guess = NULL, 
                           step_p= 0.05){
    curve <- match.arg(curve)
    if (is.null(guess)){
        guess <- switch(curve,
                        case = c(0.4, 0.7),
                        death = c(0.4, 0.4))
    }
    start <- round(guess * c(width, height))
    step <- round(step_p*width)
    move_mouse_to(remoteDriver, x = start)
    distance <- floor(width*0.95 - get("position", 1)['x'])
    for (i in seq_len(distance %/% step)){
        if (is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)) break
        move_mouse(remoteDriver, x = step, y = 0)
    }

    tip
}


gather_params <- function(remoteDriver, 
                          browser = c("chrome", "firefox"),
                          size = c("medium", "large"), 
                          curve = c("case", "death"),
                          mid_graph = NULL){

    # return value: list with parameters for traversing the epi curve
    #               bar_width: numeric, average width in pixels of a bar
    #               x_step: numeric, whole number, distance to step in x
    #                       direction when traversing curve
    #               dims: list length 3, containing width, set_height, height 
    #                     of window
    #               mid_graph: length 2 named numeric, with x and y, 
    #                          corresponds to the mouse position of a bar
    #               
    # side effects: moves mouse to the base of the graph, at the beginning of a bar

    browser <- match.arg(browser)
    size <- match.arg(size)
    curve <- match.arg(curve)
    
    
    dims <- get_dimensions(size = size, browser = browser)
    remoteDriver$setWindowSize(width = dims$width, height = dims$set_height)
    if (is.null(mid_graph)){
        found_bar <- find_bar(remoteDriver, width = dims$width, height = dims$height, curve = curve)
        found_base <- find_y_boundary(remoteDriver, height = dims$height, curve = curve)
        mid_graph <- attr(found_base, "position")
        # at the end of the block, you're at the base of a bar
    } else {
        # this block executes if mid_graph is supplied
        move_mouse_to(remoteDriver, x = mid_graph)
        wm_list <- wiggle_mouse(remoteDriver, curve = curve)
        # wiggle_mouse produces a list, $tooltip: the tooltip
        # $daily_bar: logical re, if it's a daily bar
        if (! wm_list$daily_bar){
            # block executes if no daily_bar tooltip produced
            stop("Position from \"mid_graph\"is not on the graph,",
                 " or tooltip is not rendering.")            
        } else if (! suppressMessages(check_base(remoteDriver))){
            # block executes if daily_bar tooltip is produced
            # AND you're not at the base of a bar.
            # Move there:
            found_base <- find_y_boundary(remoteDriver, 
                                          height = dims$height, 
                                          curve = curve)
            mid_graph <- attr(found_base, "position")
        }

        # end of block that executes if mid_graph is supplied
        # at the end of the block, you're at the base of a bar
        # or, an error has been thrown
    }
    
    bar_width <- find_avg_width(remoteDriver, window_width = dims$width)
    x_step <- floor(bar_width - 0.5)
    
    res <- list(bar_width = bar_width,
                x_step = x_step,
                dims = dims,
                mid_graph = mid_graph)
    res
}

check_base <- function(remoteDriver){
    # return value: logical, does moving the mouse 1 pixel south result in a
    #               null tooltip. This should correspond to the base of a bar
    # no side effects. 
    message("'check_base' has no error checking.\n",
            "Only used it if you have already confirmed the mouse position ",  
            "is over a daily bar.")
 
    move_mouse(remoteDriver, x = 0, y = 1)
    border_tip <- extract_tooltip(remoteDriver)
    move_mouse(remoteDriver, x = 0, y = -1)
    return(is.null(border_tip))

}

place <- function(remoteDriver, 
                  target_date = NULL, 
                  bar_width, 
                  curve = c("case", "death"), 
                  window_width){
    # side effects: moves mouse to center of first bar
    # return value: position of center of first bar
    curve <- match.arg(curve)
    if (is.null(target_date)){
        target_date <- switch(curve,
                              case = get("first_case", 1),
                              death = get("first_death", 1))
    }
    find_date(remoteDriver = remoteDriver, 
              target_date = target_date, 
              bar_width = bar_width, 
              curve = curve, 
              window_width = window_width)
    start_position <- find_mid(remoteDriver, curve = curve)
    start_position
}

roll_m <- function(x, n, na.rm = TRUE){
    n <- min(length(x), n)
    c(dplyr::cummean(x[1:(n-1)]), rollmean(x, n, na.rm = na.rm))
}

drill_up <- function(remoteDriver, 
                     height,
                     curve = c("case", "death"), 
                     mid_graph){
    curve <- match.arg(curve)
    # no return value. Only side effects
    # side effects: moves the bin width wider (from day, to week, month,
    #               quarter, year)
    #               Note: you may expect this function to destroy the 
    #               record of position. It doesn't.

    
    # mouse position should be at the base of a bar
    if (! is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)){
        move_mouse_to(remoteDriver, x = mid_graph)
    }
    # you've ensured mouse is on a bar, so suppress warnings
    if (! suppressMessages(check_base(remoteDriver))){
        find_y_boundary(remoteDriver, height = height, curve = curve)
    }

    # down 12 and click to expose the drill down button
    move_mouse(remoteDriver, x = 0, y = 12)
    remoteDriver$click()
    
    # get the element and click it
    DrillUp <- remoteDriver$findElement(using = "css selector",
                                          value = "span.tab-tvLevelDrillUp")
    DrillUp$clickElement()
}

drill_down <- function(remoteDriver, height, curve = c("case", "death"), mid_graph){
    # note, yes you do need to expose the element
    # TODO: encapsulate exposing the drill buttons as its own function
    curve <- match.arg(curve)
    # no return value. Only side effects
    # side effects: moves the bin width wider (from day, to week, month,
    #               quarter, year)
    #               Note: you may expect this function to destroy the 
    #               record of position. It doesn't.
    
    
    # mouse position should be at the base of a bar
    if (! is_daily_bar(tip <- extract_tooltip(remoteDriver), curve = curve)){
        move_mouse_to(remoteDriver, x = mid_graph)
    }
    # you've ensured mouse is on a bar, so suppress warnings
    if (! suppressMessages(check_base(remoteDriver))){
        find_y_boundary(remoteDriver, height = height, curve = curve)
    }
    
    # down 12 and click to expose the drill down button
    move_mouse(remoteDriver, x = 0, y = 12)
    remoteDriver$click()
    DrillDown <- remoteDriver$findElement(using = "css selector",
                                          value = "span.tab-tvLevelDrillDown")
    DrillDown$clickElement()
}

assign_sundays <- function(first_case, last_date, check_dates){
    # RETURN THE DATE, not the SUNDAY
    # NOTE: don't use epiweek. Bars are labeled by the date on sun,
    #       not mmwr weeks.
    first_sun <- first_case - wday(first_case) + 1
    last_sun <- last_date - wday(last_date) + 1
    sundays <- seq(from = first_sun, to = last_sun, by = 7)
    sunday_bins <- cut(check_dates, breaks = sundays)
    
    dup_values <- sunday_bins[duplicated(sunday_bins)]
    check_dates[! (sunday_bins %in% dup_values)]
}

fix_missing_by_wk <- function(remoteDriver, 
                              window_width,
                              mid_graph,
                              df,
                              wks_missing_1,
                              curve = c("case", "death")){
    # wks_missing_1: Date, the days with expected events that have none
    #                Output of assign_sundays(). Each date must be alone
    #                in its week.
    #                This should be the actual day missing data. Not the
    #                start of the week. The week start is computed here.
    
    curve = match.arg(curve)
    for (i in seq_along(wks_missing_1)){
        missing_date <- wks_missing_1[i]
        target_week <- missing_date - wday(missing_date) + 1
        # TODO: bar_width is not going to correspond to actual bar width...
        # Consider writing a week method for find date
        # in the meantime, put yourself at the beginning of a weekly bar 
        # this corresponds to the beginning of a daily bar with the same date
        # allowing date diff and bar_width to operate as they would for 
        # daily bars
        move_mouse_to(remoteDriver, x = mid_graph)
        week_mid_graph <- attr(find_next_bar_get_tip(remoteDriver), "position")
        # DON'T SUPPLY mid_graph. You want to start here
        find_date(remoteDriver,
                  target_date = target_week,
                  bar_width = bar_width,
                  window_width = window_width,
                  mid_graph = NULL,
                  curve = curve)
        wm_list <- wiggle_mouse(remoteDriver, curve = curve)
        if (! wm_list$daily_bar){
            print(glue::glue("Can't find week starting",
                             " {wks_missing_1[i]}.\n\n"))
            return(df)
        }
        tip <- wm_list$tooltip
        events_in_week <- attr(tip, "number")
        week_start <- attr(tip, "date")
        current_sum <- filter(df, between(date, week_start, (week_start + 6))) %>%
            select(2) %>% sum()
        found_events <- events_in_week - current_sum
        # Account for date that is, in fact, missing an event
        if (found_events == 0){
            next
        } else if (df[df$date == missing_date, 2] == 0){
            print(glue::glue("Adding {found_events} events",
                             " to {missing_date}.\n\n"))
            df[df$date == missing_date, 2] <- found_events
        }
    }
    df
}


wiggle_mouse <- function(remoteDriver, 
                         curve = c("case", "death"), 
                         attempts = 5){
    # purpose of function is side effect
    # tooltip can fail to render. 
    # If a DAILY BAR tooltip is expected, but not present, use this function
    # value: list, tooltip and daily_bar
    #       tooltip: a valid tooltip, or NULL. 
    #        There are 3 options here:
    #        1. object with class: "tooltip" and "daily_death" or "daily_case"
    #        2. NULL
    #        3. a tooltip not matching the daily_death or daily_case pattern
    #       daily_bar: logical re, is the tooltip option 1 above
    curve <- match.arg(curve)
    
    for (i in seq_len(attempts)){
        tip <- extract_tooltip(remoteDriver)
        test <- is_daily_bar(tip, curve = curve)
        if (test){
            print(glue::glue("Mouse is over a daily bar and tooltip rendered."))
            return(list(tooltip = tip, daily_bar = TRUE))
        } else {
            print(glue::glue("Move one to the right and back, check again."))
            move_mouse(remoteDriver, x = 1, y = 0)
            move_mouse(remoteDriver, x = -1, y = 0)
        }
    }
    # if the attempt-th attempt failed, else block runs, mouse moves one more time
    # You've moved the mouse. You might as well check.
    tip <- extract_tooltip(remoteDriver)
    if (is_daily_bar(tip, curve = curve)){
        print(glue::glue("Mouse is over a daily bar and tooltip rendered."))
        return(list(tooltip = tip, daily_bar = TRUE))
    } else {
        print(glue::glue("Mouse is not positioned over a bar,",
                         " or tooltip is not rendering."))
        print(glue::glue("Returning result for tooltip.\n"))
        return(list(tooltip = tip, daily_bar = FALSE))
    }
    
}
