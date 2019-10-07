library(tidyverse)

slot <- read_csv("data/slot_tgts.csv")
wide <- read_csv("data/wide_tgts.csv")
airyards <- read_csv("data/airyards.csv")
seperation <- read_csv("data/seperation.csv")

slot_wide_joined <- slot %>%
   left_join(wide, by = c("Season", "Player", "Team")) %>%
   select(player = Player, team = Team, slot_targets = Tgts.x, wide_targets = Tgts.y, slot_catchable = Catchable.x, wide_catchable = Catchable.y) %>%
   mutate(slot_pct = round(slot_targets / (slot_targets + wide_targets), 2))

slot_and_adot <- slot_wide_joined %>%
   inner_join(airyards, by = c("player" = "full_name"))

all <- slot_and_adot %>%
   inner_join(seperation, by = c("player" = "name")) %>%
   unique()

model <- lm(data = all, seperation ~ slot_pct + adot)
summary(model)

all$predict <- round(predict(model, all), 1)

all_over_expected <- all %>%
   mutate(seperation_oe = seperation - predict) %>%
   select(player, seperation_oe) %>%
   unique()
