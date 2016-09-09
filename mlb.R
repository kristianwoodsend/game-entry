library(utils)
library(dplyr)
library(Hmisc)
library(ggplot2)


# To generate contests.csv:
# select * from "table" join game on "table".game_id=game.game_id where tournament_name='mlb-squeeze' and gs>10000 and create_date between '2016-06-16' and '2016-06-30';
# 
# To generate entries.csv:

# with contest as (
#   select table_id from "table" join game on "table".game_id=game.game_id where tournament_name='mlb-squeeze'
#   and gs>10000 and total_prize_pool >= 100000
#   and create_date between '2016-08-01' and '2016-08-30'
# )
# select event_name, performing_user_id, server_timestamp, fixture_list_id, contest_id,
# entry_id, roster_id, entry_was_multi_entry, entry_tx_id
# from event where contest_id in (select table_id from contest)
# and event_name='entry';

# with contest as (select table_id from "table" where tournament_name='mlb-squeeze' and gs>10000 and create_date between '2016-06-16' and '2016-06-18'),
# entries as (
#   select distinct entry_id from event where contest_id in (select table_id from contest) and event_name='entry' 
# )
# select event_name, performing_user_id, server_timestamp, fixture_list_id, contest_id, entry_id, roster_id, entry_was_multi_entry, entry_tx_id from event where entry_id in (select entry_id from entries);

# To get cancel_entry events too:
# with contest as (select table_id from "table" where tournament_name='mlb-squeeze' and gs>10000 and create_date between '2016-06-16' and '2016-06-18'),
# select ec.event_name, ec.performing_user_id, ec.server_timestamp, ec.entry_id, ee.contest_id
# from event ec 
# join event ee on ec.entry_id=ee.entry_id
# where ec.event_name='cancel_entry' and ee.event_name='entry'
# and ee.contest_id in (select table_id from contest)
# limit 1 ;

setwd("~/src/fd-projects/game-entry")
contests <- tbl_df(read.csv('mlb-contests.csv'))
# contests$table_id <- as.factor(contests$table_id)

events <-  tbl_df(read.csv('mlb-entries.csv', stringsAsFactors=FALSE))
events <- events %>% select(event_name, performing_user_id, server_timestamp, contest_id, entry_id)
entries <- events %>% filter(event_name=='entry')

# cancel events are missing contest_id. Need to get it from the original entry, using SQL above
cancels <- tbl_df(read.csv('mlb-cancels.csv', stringsAsFactors=FALSE))

entries$seat = +1
cancels$seat = -1

entries <- entries %>% union(cancels)


# Note: contests$table_id -> entries$contest_id
big.contests <- contests$table_id[contests$total_prize_pool >= 100000]
# entries <- entries %>% filter(contest_id %in% big.contests)

# entries <- entries %>% filter(contest_id %in% c(25000330))
# entries <- entries[1:100,]
entries <- entries %>% select(event_name, server_timestamp, contest_id, seat)
entries <- entries %>% inner_join(contests %>% select(table_id, game_date, gs), by=c("contest_id" = "table_id"))
entries$sts <- as.POSIXct(entries$server_timestamp)
entries$gts <- as.POSIXct(entries$game_date)
entries$tminus <- as.numeric(as.POSIXct(entries$server_timestamp) - as.POSIXct(entries$game_date), units='mins')

# make n.tminus bins
n.tminus = 200
tminus.start = min(entries$tminus)
tminus.binwidth = tminus.start/n.tminus

# use dplyr window functions and cumsum() to get each group to start counting at 0
entries2 <- entries %>% 
  select(contest_id, seat, tminus, gs) %>% 
  mutate(tminus.bin = ceiling(-(tminus - tminus.start)/tminus.binwidth)) %>%
  group_by(contest_id, gs, tminus.bin) %>% 
  arrange(tminus.bin) %>% 
  summarise(seat=sum(seat), n=n(), tminus=min(tminus)) %>%
  mutate(nc=cumsum(seat), nr=cumsum(n), filled=nc/gs, entry=nr/gs)

entries2$contest_id <- as.factor(entries2$contest_id)

# ggplot(entries2, aes(x=tminus/60, y=filled, group=contest_id, colour=contest_id)) + 
#   geom_line(aes(y=filled)) + 
#   geom_line(aes(y=entry),linetype="dotted") + 
#   geom_line(aes(y=entry-filled),linetype="dashed")

# Very big plot
ggplot(entries2, aes(x=tminus/60, y=filled, group=contest_id, colour=contest_id, fill=contest_id)) + 
  geom_line() + geom_area(alpha=0.2, position='dodge') +
  ggtitle('Fill curve as % of contest size')

ggplot(entries2, aes(x=tminus/60, y=nc, group=contest_id, colour=contest_id, fill=contest_id)) + 
  geom_line() + geom_area(alpha=0.2, position='dodge') +
  ggtitle('Fill curve as number of entries')

# -- event_name possibilities: edit_entry entry cancel_entry quick_player_replace create_league create_head_to_head

# ------------
# Rate of entries. Make cuts using tminus not ntile
entry.rate <- 
  entries %>% 
  select(contest_id, seat, tminus, gs) %>% 
  mutate(tminus.bin = ceiling(-(tminus - tminus.start)/tminus.binwidth)) %>%
  group_by(contest_id, gs, tminus.bin) %>% 
  summarise(nc=sum(seat), nr=n(), tmin=min(tminus) ) %>%
  mutate(filled=nc/gs, entry=nr/gs)

entry.rate$contest_id <- as.factor(entry.rate$contest_id)

ggplot(entry.rate, aes(x=tmin/60, y=filled, group=contest_id, colour=contest_id, fill=contest_id)) + 
  geom_line() + geom_area(alpha=0.2, position='dodge') +
  ggtitle('Fill rates as proportion of contest size')


ggplot(entry.rate, aes(x=tmin, y=nc, group=contest_id, colour=contest_id, fill=contest_id)) + geom_line() + geom_area(alpha=0.2, position='dodge')




