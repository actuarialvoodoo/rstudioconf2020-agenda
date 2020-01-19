library(googlesheets4)
library(googledrive)

source("functions.R")
agenda_raw <- drive_get("Rstudioconf2020-Agenda-raw")

sheet_names <- sheets_sheet_names(agenda_raw)

raw_data <- sheet_names %>%
    purrr::map(read_sheet, ss = agenda_raw, col_names = "txt") %>%
    stats::setNames(sheet_names)

# Step 1: Find the Rooms because these seem to be the easiest to identify piece
# of information that is constant. It appears to be the case that the room is 
# always the 4th row in each talk group.

tidy_data <- purrr::map(raw_data, tidy_agenda)



# Need to check that all talks have the same number of rows and determine why some
# differ (if they do)

odd_talks <- purrr::map(tidy_data, check_agenda)

# All checks out

# Other checks
# 
# 1. It looks like I accidentally included a keynote in day 2

tidy_data[[2]] <- dplyr::filter(tidy_data[[2]], talk_num > 1)


# Some of the timeblocks are off by a min. Find the issues and fix them.

tidy_data[[1]] %>% 
    dplyr::filter(line_type == "TimeBlock") %>%
    dplyr::group_by(new_txt) %>%
    dplyr::count() %>%
    dplyr::filter(n != 4) %>%
    dplyr::arrange(new_txt)
# A tibble: 4 x 2
# Groups:   new_txt [4]
#new_txt             n
#<chr>           <int>
#   1 2:15 PM-2:37 PM     3
#   2 2:15 PM-2:38 PM     1
#   3 4:00 PM-4:22 PM     3
#   4 4:00 PM-4:23 PM     1    

tidy_data[[1]] <- tidy_data[[1]] %>% 
    dplyr::select(-line_num) %>% 
    tidyr::spread(line_type, new_txt) %>%
    dplyr::arrange(TimeBlock, Room) %>%
    dplyr::mutate( TimeBlock = dplyr::case_when(
        TimeBlock == "2:15 PM-2:38 PM" ~ "2:15 PM-2:37 PM",
        TimeBlock == "4:00 PM-4:23 PM" ~ "4:00 PM-4:22 PM",
        TRUE ~ TimeBlock
    ))
    

# Add Session Number
tidy_data[[1]] <- tidy_data[[1]] %>%
    dplyr::mutate(start_time = make_datetime(
        stringr::str_extract(TimeBlock, "^[^-]+"), "2020-01-29")) %>%
    dplyr::group_by(Program, Room) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(
        time_diff = c(25, diff(start_time)), 
        start_group = as.integer(time_diff > 23),
        group = cumsum(start_group)
    )

sessions <- tidy_data[[1]] %>%
    dplyr::group_by(Program, Room, group) %>%
    dplyr::summarise(min_start_time = min(start_time)) %>%
    dplyr::arrange(min_start_time, Room) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(session = match(min_start_time, unique(min_start_time)))
    
tidy_data[[1]] <- tidy_data[[1]] %>%
    dplyr::left_join(sessions, by = c("Program", "Room", "group"))

tidy_data[[2]] %>% 
    dplyr::filter(line_type == "TimeBlock") %>%
    dplyr::group_by(new_txt) %>%
    dplyr::count() %>%
    dplyr::filter(n != 4) %>%
    dplyr::arrange(new_txt)

# There are eleven timeslots with something other than 4 talks

# So look at it by Program

tidy_data[[2]] %>% 
    dplyr::filter(line_type %in% c("TimeBlock", "Program")) %>% 
    tidyr::pivot_wider(
        id_cols = talk_num, 
        names_from = line_type, 
        values_from = new_txt
    ) %>% 
    dplyr::group_by(Program) %>% 
    dplyr::count()

# looks like Lightning Talks (17), Medicine 3, and Panel (1) are the culprits
# There are two 5 min Lightning talks, in each 5 mins block from 2:45 to to 3:25
# and 1 from 3:25 to 3:30 (Are they in the same room?)
# 
# The 3 Medicine talks are from 10:53-11:59. The other Program's in it's block
# Should have a 10:30 to 10:52 slot which is missing
# 
# The Panel is from 2:45 PM - 3:30, which is concurrent with the lightning talks
# 

#new_ss <- sheets_create(name = "RstudioConf2020-Agenda", sheets = sheet_names)

#sheets_write(a3, ss = new_ss, sheet = sheet_names[1])

saveRDS(tidy_data, file = "agenda_data.rds")
