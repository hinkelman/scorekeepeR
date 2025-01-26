
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

#' Create player names
#'
#' Create player names for display in app
#'
#' @md
#' @param data    Dataframe that includes columns for FirstName, LastName, and Number
#'
#' @export
#'

create_player_names <- function(data){
  data |>
    dplyr::mutate(
      Name = dplyr::case_when(
        is.na(FirstName) & is.na(LastName) ~ NA_character_,
        is.na(FirstName) ~ LastName,
        is.na(LastName) ~ FirstName,
        .default = paste(FirstName, LastName)),
      name_tmp = dplyr::case_when(
        is.na(FirstName) & is.na(LastName) ~ NA_character_,
        is.na(FirstName) ~ LastName,
        .default = FirstName),
      NameNum = dplyr::case_when(
        is.na(name_tmp) & is.na(Number) ~ NA_character_,
        is.na(Number) ~ name_tmp,
        is.na(name_tmp) ~ paste0("#", Number),
        .default = paste0(name_tmp, " (#", Number, ")"))) |>
    dplyr::select(-name_tmp)
}

