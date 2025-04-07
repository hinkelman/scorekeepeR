
#' Initialize players table
#'
#' Initialize players table with no data
#'
#' @md
#'
#' @export
#'

init_players_table <- function(){
  data.frame(PlayerID = character(),
             FirstName = character(),
             LastName = character())
}


#' Add players row
#'
#' Add new row to players table
#'
#' @md
#' @param players_table  Players table
#'
#' @export
#'

add_players_row <- function(players_table){
  rbind(players_table,
        data.frame(PlayerID = ids::random_id(),
                   FirstName = NA_character_,
                   LastName = NA_character_))
}

#' Replace space
#'
#' Replace empty strings with any number of spaces with NA
#'
#' @md
#' @param x     Vector of strings
#'
#' @export
#'

replace_space <- function(x){
  ifelse(grepl("^\\s*$", x), NA_character_, x)
}

#' Create player name
#'
#' Create player name for display in app
#'
#' @md
#' @param first_name    Vector of player first names
#' @param last_name     Vector of player last names
#'
#' @export
#'

create_player_name <- function(first_name, last_name){
  first = replace_space(first_name)
  last = replace_space(last_name)
  dplyr::case_when(
    is.na(first) & is.na(last) ~ NA_character_,
    is.na(first) ~ last,
    is.na(last) ~ first,
    .default = paste(first, last))
}

#' Create player name with number
#'
#' Create player name with number for display in app
#'
#' @md
#' @param first_name    Player first name
#' @param last_name     Player last name
#' @param number        Player number
#'
#' @export
#'

create_player_namenum <- function(first_name, last_name, numbers){
  first = replace_space(first_name)
  last = replace_space(last_name)
  numbers = replace_space(numbers)
  name_tmp = dplyr::case_when(
    is.na(first) & is.na(last) ~ NA_character_,
    is.na(first) ~ last,
    .default = first)
  dplyr::case_when(
    is.na(name_tmp) & is.na(numbers) ~ NA_character_,
    is.na(numbers) ~ name_tmp,
    is.na(name_tmp) ~ paste0("#", numbers),
    .default = paste0(name_tmp, " (#", numbers, ")"))
}
