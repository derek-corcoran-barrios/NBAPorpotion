library(tidyverse)
library(rvest)
library(readxl)
library(polite)
library(rvest)
library(purrr)
library(dplyr)

#session <- bow("https://www.cheese.com/alphabetical")
session <- bow("https://www.basketball-reference.com/leagues/")

Years <- 1962:2023

Seasons  <- paste(1961:2022, str_trunc(Years, width = 2, side = "left", ellipsis = ""), sep = "-")

ScoringSeasons  <- read_csv("sportsref_download.csv") |> 
  dplyr::select(Season, PTS) |> 
  dplyr::filter(Season %in% Seasons) |> 
  dplyr::rename(SEASON_PTS = PTS)

Tests <- list()

for(i in 1:length(Years)){
  try({
    Tests[[i]] <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", Years[i], "_per_game.html#per_game_stats::pts_per_g")) |> 
      html_element("#per_game_stats") |>  
      html_table() |> 
      mutate(PTS = as.numeric(PTS), Rk = as.numeric(Rk)) |> 
      dplyr::select(PTS,Player) |> 
      dplyr::slice_max(order_by = PTS, n = 10) |> 
      mutate(Season = Seasons[i]) |> 
      left_join(ScoringSeasons) |> 
      mutate(prop_scor = round((PTS/SEASON_PTS)*100, 2))
  })
  Sys.sleep(6)
}

Tests <- Tests |> 
  purrr::reduce(bind_rows)
  

