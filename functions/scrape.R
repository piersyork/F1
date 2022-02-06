

#' @export
scrape_f1 = function(y_url = "https://www.formula1.com/en/results.html/2021/races.html") {
  # import functions used
  box::use(dplyr[...],
           magrittr[extract, extract2],
           stringr[word, str_sub],
           lubridate[ms, dmy, year, period],
           rvest[read_html, html_nodes, html_text, html_table, html_attr])
  
  # read the website url into xml
  f1 <- read_html(y_url)
  
  # get the urls of each race weekend
  urls <- f1 %>% 
    html_nodes(".resultsarchive-filter") %>% 
    extract(3) %>% 
    html_nodes(".resultsarchive-filter-item a") %>% 
    html_attr("href") 
  
  # count number of races done in the year so far. This is only for when reading the current year's
  # page, all other years will have all races complete
  number_of_races <- f1 %>% 
    html_table() %>% 
    extract2(1) %>% 
    nrow()
  
  # get the name of each race
  name <- f1 %>% 
    html_nodes(".resultsarchive-filter") %>% 
    extract(3) %>% 
    html_nodes(".resultsarchive-filter-item a span") %>% 
    html_text() %>% 
    extract(1:number_of_races + 1)
  
  # for each race weekend extract each type of table and store in list
  all_races <- list()
  dates <- vector()
  # i starts at 2 because the first url is a summary of all races not an individual race weekend
  for (i in 1:number_of_races+1) {
    # try catch will in effect skip if there is an error. A likely error will be cannot connect to page
    tryCatch({
      # url of each weekend event
      race_urls <- read_html(paste0("https://www.formula1.com/", urls[i])) %>% 
        html_nodes(".resultsarchive-col-left .side-nav-item .side-nav-item-link") %>% 
        html_attr("href")
      
      # for each event get table
      race_tables <- list()
      data_types <- vector()
      for (j in 1:length(race_urls)) {
        # the race event
        race <- read_html(paste0("https://www.formula1.com/", race_urls[j]))
        race_tables[[j]] <- race %>% 
          html_table(convert = FALSE) %>% 
          extract2(1) %>% 
          select(where(~!all(.x == ""))) %>% 
          mutate(Driver = gsub("\\n                   ", "", Driver))
        data_types <- race %>% 
          html_nodes(".resultsarchive-col-left .side-nav-item .side-nav-item-link") %>% 
          html_attr("data-value")
      }
      dates[i-1] <- race %>% 
        html_nodes(".full-date") %>% 
        html_text()
      names(race_tables) <- make.names(data_types)
      all_races[[i-1]] <- race_tables
      print(paste("done", name[i-1]))
    }, error = function(err) {}) # will probs need to add an error message here
  }
  
  names(all_races) <- make.names(name)
  
  create_NA <- function(x) {
    x = NA
    return(x)
  }
  
  # for each race combine into one complete dataframe
  combined <- list()
  for (i in 1:length(all_races)) {
    finish <- all_races[[i]]$race.result %>% 
      select(Driver, Pos, PTS, Car, Laps) %>% 
      rename(f_result = Pos, points = PTS)
    
    if (!is.null(all_races[[i]]$practice.1)) {
      fp1 <- all_races[[i]]$practice.1 %>% 
        select(Driver, Pos, Time, Gap, Laps) %>% 
        rename(fp1_result = Pos, fp1_time = Time, fp1_gap = Gap, fp1_laps = Laps)
    } else {
      fp1 <- tibble(Driver = finish$Driver, fp1_result = NA, fp1_time = NA, fp1_gap = NA, fp1_laps = NA)
    }
    
    
    if (!is.null(all_races[[i]]$practice.2)) {
      fp2 <- all_races[[i]]$practice.2 %>% 
        select(Driver, Pos, Time, Gap, Laps) %>% 
        rename(fp2_result = Pos, fp2_time = Time, fp2_gap = Gap, fp2_laps = Laps)
    } else {
      fp2 <- fp1 %>% 
        rename(fp2_result = fp1_result, fp2_time = fp1_time, fp2_gap = fp1_gap, fp2_laps = fp1_laps) %>% 
        mutate(across(fp2_result:fp2_laps, .fns = create_NA))
    }
    
    if (!is.null(all_races[[i]]$practice.3)) {
      fp3 <- all_races[[i]]$practice.3 %>% 
        select(Driver, Pos, Time, Gap, Laps) %>% 
        rename(fp3_result = Pos, fp3_time = Time, fp3_gap = Gap, fp3_laps = Laps)
    } else {
      fp3 <- fp1 %>% 
        rename(fp3_result = fp1_result, fp3_time = fp1_time, fp3_gap = fp1_gap, fp3_laps = fp1_laps) %>% 
        mutate(across(fp3_result:fp3_laps, .fns = create_NA))
    }
    
    if ("Q3" %in% colnames(all_races[[i]]$qualifying)) {
      quali <- all_races[[i]]$qualifying %>% 
        select(Driver, Pos, Q1, Q2, Q3, Laps) %>% 
        rename(quali_result = Pos, quali_laps = Laps)
    } else {
      quali <- all_races[[i]]$qualifying %>% 
        select(Driver, Pos, Q1, Q2, Laps) %>% 
        rename(quali_result = Pos, quali_laps = Laps) %>% 
        mutate(Q3 = NA)
    }
    
    # join to create dataframe
    combined[[i]] <- finish %>% 
      mutate(race = names(all_races)[i], 
             date = dates[i]) %>% 
      left_join(quali) %>% 
      left_join(fp3) %>% 
      left_join(fp2) %>% 
      left_join(fp1) %>% 
      mutate(across(everything(), as.character))
  }
  
  df2 <- bind_rows(combined)
  
  gap_NA <- function(x) {
    
  }
  fp_seconds = function(x) {
    box::use(stringr[str_sub],
             lubridate[seconds],
             dplyr[...])
    return({
      x %>% 
        str_sub(2, 6) %>% 
        if_else(. == "", "0", .) %>% 
        seconds() %>% 
        if_else(is.na(.), seconds(10), .)
    })
  }
  
  # tidy final dataframe and return
  df <- df2 %>% 
    mutate(across(c(f_result, points, quali_result, quali_laps, fp3_result, fp3_laps,
                    fp2_result, fp2_laps, fp1_result, fp1_laps), as.numeric),
           across(c(Q1:Q3, fp3_time, fp2_time, fp1_time), ms),
           across(contains("gap"), fp_seconds),
           date = dmy(date),
           year = year(date))
  return(df)
}


#' @export
get_races = function(years = 2020) {
  options(box.path = getwd())
  # import functions
  box::use(functions/scrape[scrape_f1],
           dplyr[bind_rows])
  
  # for each year get all races for that year using above scrape_f1 function
  race_data <- list()
  for (i in 1:length(years)) {
    race_data[[i]] <- scrape_f1(paste0("https://www.formula1.com/en/results.html/", 
                                       years[i], "/races.html"))
    print(paste0("### done ", years[i], " ###"))
  }
  # combine years into final dataset and return
  df <- bind_rows(race_data)
  return(df)
}

#' @export
get_latest_race = function() {
  # import functions
  box::use(dplyr[...],
           magrittr[extract, extract2],
           stringr[word, str_sub],
           lubridate[ms, dmy, year],
           rvest[read_html, html_nodes, html_text, html_table, html_attr])
  
  # get url of most recent year
  current_year_url <- read_html("https://www.formula1.com/en/results.html/2021/races.html") %>% 
    html_nodes(".resultsarchive-filter-item-link") %>% 
    extract(1) %>% 
    html_attr("href")
  
  # get number of races for the year so far
  number_of_races <- read_html(paste0("https://www.formula1.com/", current_year_url)) %>% 
    html_table() %>% 
    extract2(1) %>% 
    nrow()
  
  # get latest race url
  latest_url <- read_html(paste0("https://www.formula1.com/", current_year_url)) %>% 
    html_nodes(".resultsarchive-filter-wrap") %>% 
    extract(3) %>% 
    html_nodes(".resultsarchive-filter-item-link") %>% 
    extract(number_of_races + 1) %>% 
    html_attr("href")
  
  # get table of race result
  result <- read_html(paste0("https://www.formula1.com/", latest_url)) %>% 
    html_table(convert = FALSE) %>% 
    extract2(1) %>% 
    select(where(~!all(.x == ""))) %>% 
    mutate(Driver = gsub("\\n                   ", "", Driver))
  
  # return that result
  return(result)
  
}

## note to piers: Build in function options to select different race info. Have an argument option be
## to input a vector of either race result, fp1, quali etc. Then use %in% to allow for those in output

#' @export
get_standings = function(drivers = TRUE) {
  # import functions used
  box::use(dplyr[...],
           magrittr[extract, extract2],
           stringr[word, str_sub, str_split],
           lubridate[ms, dmy, year],
           rvest[read_html, html_nodes, html_text, html_table, html_attr])
  
  # get system current year
  current_year <- year(Sys.Date())
  
  # get standings for current year
  driver_standings <- read_html(paste0("https://www.formula1.com/en/results.html/", current_year, "/drivers.html")) %>% 
    html_table(convert = FALSE) %>% 
    extract2(1) %>% 
    select(where(~!all(.x == ""))) %>% 
    mutate(Driver = gsub("\\n                   ", "", Driver),
           PTS = as.numeric(PTS))
  
  # argument drivers is true then return driver standing, else summarise to get team standings
  if (drivers) {
    return(driver_standings)
  } else {
    return({
      suppressWarnings({
        driver_standings %>% 
          mutate(Constructor = ifelse(sapply(strsplit(Car, " "), length) == 1, 
                                      Car, 
                                      word(Car, 1, -2))) %>% 
          group_by(Constructor) %>% 
          summarise(PTS = sum(PTS)) %>% 
          arrange(desc(PTS)) %>% 
          mutate(Pos = row_number()) %>% 
          select(Pos, Constructor, PTS)
      })
    })
  }
}


get_a_race = function(year = 2021) {
  box::use(dplyr[...],
           magrittr[extract, extract2],
           stringr[word, str_sub],
           lubridate[ms, dmy, year],
           rvest[read_html, html_nodes, html_text, html_table, html_attr])
  
  current_year_url <- read_html("https://www.formula1.com/en/results.html/2021/races.html") %>% 
    html_nodes(".resultsarchive-filter-item-link") %>% 
    extract(1) %>% 
    html_attr("href")
  
  number_of_races <- read_html(paste0("https://www.formula1.com/", current_year_url)) %>% 
    html_table() %>% 
    extract2(1) %>% 
    nrow()
  
  latest_url <- read_html(paste0("https://www.formula1.com/", current_year_url)) %>% 
    html_nodes(".resultsarchive-filter-wrap") %>% 
    extract(3) %>% 
    html_nodes(".resultsarchive-filter-item-link") %>% 
    extract(number_of_races + 1) %>% 
    html_attr("href")
  
  result <- read_html(paste0("https://www.formula1.com/", latest_url)) %>% 
    html_table(convert = FALSE) %>% 
    extract2(1) %>% 
    select(where(~!all(.x == ""))) %>% 
    mutate(Driver = gsub("\\n                   ", "", Driver))
  
  return(result)
}









