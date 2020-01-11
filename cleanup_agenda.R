library(googlesheets4)
library(googledrive)

agenda_raw <- drive_get("Rstudioconf2020-Agenda-raw")

sheet_names <- sheets_sheet_names(agenda_raw)

raw_data <- purrr::map(sheet_names, ~ read_sheet)

a <- read_sheet(agenda_raw, sheet = sheet_names[1], col_names = "txt" )

# Step 1: Find the Rooms because these seem to be the easiest to identify piece
# of information that is constant. It appears to be the case that the room is 
# always the 4th row in each talk group.

a2 <- a %>% 
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
    ) %>%
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
    dplyr::ungroup() %>%
    dplyr::select(talk_num, line_num, line_type, new_txt)



# Need to check that all talks have the same number of rows and determine why some
# differ (if they do)

odd_talks <- a2 %>%
    tidyr::nest(talk_info = c(line_num, line_type, new_txt)) %>% 
    dplyr::mutate(num_row = purrr::map_int(talk_info, nrow)) %>%
    dplyr::filter(num_row != 7)

# All checks out

a3 <- a2 %>% 
    dplyr::select(-line_num) %>% 
    tidyr::spread(line_type, new_txt) %>%
    dplyr::arrange(TimeBlock, Room) %>%
    dplyr::mutate( TimeBlock = dplyr::case_when(
        TimeBlock == "2:15 PM-2:38 PM" ~ "2:15 PM-2:37 PM",
        TimeBlock == "4:00 PM-4:23 PM" ~ "4:00 PM-4:22 PM",
        TRUE ~ TimeBlock
    ))

new_ss <- sheets_create(name = "RstudioConf2020-Agenda", sheets = sheet_names)

sheets_write(a3, ss = new_ss, sheet = sheet_names[1])

f <- read_sheet(agenda_raw, sheet = sheet_names[2], col_names = "txt" )

# Step 1: Find the Rooms because these seem to be the easiest to identify piece
# of information that is constant. It appears to be the case that the room is 
# always the 4th row in each talk group.

f2 <- f %>% 
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
    ) %>%
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
    dplyr::ungroup() %>%
    dplyr::select(talk_num, line_num, line_type, new_txt)



# Need to check that all talks have the same number of rows and determine why some
# differ (if they do)

odd_talks_f <- f2 %>%
    tidyr::nest(talk_info = c(line_num, line_type, new_txt)) %>% 
    dplyr::mutate(num_row = purrr::map_int(talk_info, nrow)) %>%
    dplyr::filter(num_row != 7)

# All checks out

f3 <- f2 %>% 
    dplyr::select(-line_num) %>% 
    tidyr::spread(line_type, new_txt) %>%
    dplyr::arrange(TimeBlock, Room)

sheets_write(f3, ss = new_ss, sheet = sheet_names[2])


library(gt)

a3 %>% 
    dplyr::mutate(TimeBlock = forcats::as_factor(TimeBlock)) %>%
    dplyr::filter(
        as.integer(TimeBlock) <= 4
        ) %>%
    dplyr::select(TimeBlock, Program, Room, Title, Speaker, SpeakerInfo, Abstract) %>%
    tidyr::pivot_longer(
        cols = c("Title", "Speaker", "SpeakerInfo", "Abstract"), 
        names_to = "TalkInfoType", 
        values_to = "TalkInfoValue"
    ) %>%
    dplyr::select(-Room) %>%
    tidyr::pivot_wider(
        id_cols = c("TimeBlock", "TalkInfoType"),
        names_from = "Program",
        values_from = "TalkInfoValue"
        
    ) %>%
    dplyr::group_by(TimeBlock) %>%
    gt(rowname_col = c("TalkInfoType")) %>%
    tab_header(
        title = "Session 1"
    ) %>%
    tab_style(
        style = cell_text( v_align = "top"),
        locations = cells_body(
            rows = TalkInfoType == "Abstract"
        )
    )
    
    
    
