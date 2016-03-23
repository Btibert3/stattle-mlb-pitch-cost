options(stringsAsFactors = FALSE)

## load the libraries
library(stattleshipR)
library(dplyr)

## set the token
set_token(Sys.getenv("STATTLE_TOKEN"))

## helper function
parse_stattle <- function(stattle_list, entry) {
  x <- do.call("rbind", lapply(stattle_list, function(x) x[[entry]]))
  stopifnot(is.data.frame(x))
  return(x)
}

## some constants
SPORT = "baseball"
LEAGUE = "mlb"
SEASON = "mlb-2015"

## get the teams
teams_raw <- ss_get_result(sport=SPORT, league=LEAGUE, ep = "teams", walk=TRUE)
teams <- parse_stattle(teams_raw, "teams")

## testing: get the picthers from the redsox
# sox_raw <- ss_get_result(sport=SPORT, league=LEAGUE, ep="players", query = list(team_id = "mlb-bos"), walk=TRUE)
# sox_players <-  parse_stattle(sox_raw, "players") 
# with(sox_players, table(position_abbreviation))
# sox_pitchers <- filter(sox_players, position_abbreviation %in% c('RP','SP') & salary > 0)


## loop through the teams get the pitchers with salalry
pitchers <- data.frame()
for (TEAM in teams$slug) {
  ## get the players
  tmp_raw <- ss_get_result(sport=SPORT, 
                           league=LEAGUE, 
                           ep="players", 
                           query = list(team_id = TEAM, season_id=SEASON), 
                           walk=TRUE)
  tmp_players <- parse_stattle(tmp_raw, "players") 
  ## get just the pitchers with salary
  tmp_p <- filter(tmp_players, position_abbreviation %in% c('RP','SP') & salary > 0)
  ## add the team
  tmp_p$team_slug = TEAM
  ## bind
  pitchers = bind_rows(pitchers, tmp_p)
  rm(tmp_raw, tmp_p)
  cat("finished ", TEAM, "\n")
}

## for each pitcher, get their total pitches thrown
p_stats <- data.frame()
for (PLAYER in pitchers$slug) {
  ## get the player total stats
  tmp_raw <- ss_get_result(sport=SPORT, 
                           league=LEAGUE, 
                           ep="total_stats", 
                           query = list(player_id = PLAYER,
                                        interval_type = "regularseason",
                                        type = "baseball_pitcher_stat",
                                        stat = "pitches_thrown",
                                        season_id = SEASON))
  ## parse out the data
  tmp_stat <- tmp_raw[[1]]$total_player_stat$total
  ## make a data frame
  tmp_stat <- data.frame(slug = PLAYER, pitches = tmp_stat)
  ## bind to the stats
  p_stats = bind_rows(p_stats, tmp_stat)
  rm(tmp_raw, tmp_stat)
  cat("finished ", PLAYER, "\n")
}


## join the data
pitchers <- left_join(pitchers, p_stats)

## keep just those with pitches
pitchers_clean = filter(pitchers, pitches > 0  & salary > 0)

## add the cost per pitch
pitchers_clean = transform(pitchers_clean, cpp = salary / pitches)

## sort
pitchers_clean = arrange(pitchers_clean, desc(cpp))

## the data
pitchers_df = select(pitchers_clean, name, team_slug, position_abbreviation, salary, pitches, cpp)
head(pitchers_df, 25)

save(pitchers_df, pitchers_clean, pitchers, p_stats, file="datasets.Rdata")


