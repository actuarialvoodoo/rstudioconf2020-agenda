
tidy_agenda <- function(x) {
    
    labelled_data <- x %>% 
        dplyr::mutate(
            is_room_info = stringr::str_detect(txt, "^Room"), 
            talk_num = cumsum(as.integer(dplyr::lead(is_room_info, 3, default = 0)))
        ) %>%
        dplyr::group_by(talk_num) %>%
        dplyr::mutate(
            line_num = dplyr::row_number(),
            line_type = dplyr::case_when(
                line_num == 1 ~ "Title",
                line_num == 2 ~ "TimeBlock",
                line_num == 3 ~ "Program",
                line_num == 4 ~ "Room",
                line_num == 5 ~ "Speaker",
                max(line_num) >= 7 & line_num == max(line_num) ~ "Abstract",
                max(line_num) == 6 & line_num == max(line_num) & nchar(txt) > 30 ~ "Abstract",
                TRUE ~ "SpeakerInfo"
            )
        ) 
    
    # collapse multiline SpeakerInfo 
    labelled_data <- labelled_data %>%
        dplyr::group_by(talk_num, line_type) %>%
        dplyr::mutate(
            new_txt = dplyr::case_when(
                line_type == "SpeakerInfo" ~ paste0(txt, collapse = "\n"),
                TRUE ~ txt
            )
        ) %>%
        dplyr::filter(
            dplyr::row_number() == 1
        ) %>%
        dplyr::ungroup()
    
    # only keep relavent columns
    dplyr::select(labelled_data, talk_num, line_num, line_type, new_txt)
}


check_agenda <- function(x){
    #typical talk has 7 sections. Identify the ones with few and check them manually
    tidyr::nest(x, talk_info = c(line_num, line_type, new_txt)) %>% 
    dplyr::mutate(num_row = purrr::map_int(talk_info, nrow)) %>%
    dplyr::filter(num_row != 7)
}

make_datetime <- function(timestr, day) {
    datetimestr <- paste0(day, " ", timestr)
    
    as.POSIXct(
        lubridate::as_datetime(datetimestr, 
                               tz = "PST", 
                               format = "%Y-%m-%d %I:%M %p")
    )
}
