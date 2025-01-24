
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
#' @param first_names    Vector of players' first names
#' @param last_names     Vector of players' last names
#' @param numbers        Vector of players' numbers
#' @param tab            Tab where name will be displayed: roster or scorekeeper
#'
#' @export
#'

create_player_names <- function(first_names, last_names, numbers,
                                tab = c("roster", "scorekeeper")){

  out = data.frame(FirstName = first_names,
                   LastName = last_names,
                   Number = numbers) |>
    dplyr::mutate(
      roster = dplyr::case_when(
        is.na(FirstName) & is.na(LastName) ~ NA_character_,
        is.na(FirstName) ~ LastName,
        is.na(LastName) ~ FirstName,
        .default = paste(FirstName, LastName)),
      Name = dplyr::case_when(
        is.na(FirstName) & is.na(LastName) ~ NA_character_,
        is.na(FirstName) ~ LastName,
        .default = FirstName),
      scorekeeper = dplyr::case_when(
        is.na(Name) & is.na(Number) ~ NA_character_,
        is.na(Number) ~ Name,
        is.na(Name) ~ paste0("#", Number),
        .default = paste0(Name, " (#", Number, ")")))

  out[[tab]][!is.na(out[[tab]])]
}

