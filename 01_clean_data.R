# =============================================================================
# MTH 420 Final Project: Predicting NHL Skater Salaries
# 01_clean_data.R: turn raw_stats.csv into the analysis dataset
#
# Input:  raw_stats.csv          (project root, git-ignored)
# Output: data/clean_stats.csv   (one row per skater, 20+ games)
# =============================================================================

suppressPackageStartupMessages(library(dplyr))
library(readr)
library(stringr)

raw <- read_csv("raw_stats.csv", show_col_types = FALSE)

# Quick sanity check that we loaded the file we expect
stopifnot(nrow(raw) == 1139, ncol(raw) == 40)


# -----------------------------------------------------------------------------
# Step 1: Fix text and labels
# -----------------------------------------------------------------------------
team_fixes <- c(
  "Loas Angles Kings"          = "Los Angeles Kings",
  "Toronto Maple Leafers"      = "Toronto Maple Leafs",
  "Winnepeg Jets"              = "Winnipeg Jets",
  "Seattle Kracken"            = "Seattle Kraken",
  "New York islanders"         = "New York Islanders",
  "Las Vegas Golden Knights"   = "Vegas Golden Knights",
  "Utah Hockey Club (Mammoth)" = "Utah"
)

df <- raw |>
  mutate(
    Team       = recode(Team, !!!team_fixes),
    Conference = recode(Conference, "Wstern" = "Western")
  )

stopifnot(n_distinct(df$Team) == 32,
          all(df$Conference %in% c("Eastern", "Western")))


# -----------------------------------------------------------------------------
# Step 2: Convert text columns to numbers
#   AAV / Value are stored like "$7,000,000".
#   TOI.min is text and actually in SECONDS, so we drop it and use ATOI.min
#   (average minutes per game, already numeric).
# -----------------------------------------------------------------------------
df <- df |>
  mutate(
    AAV   = parse_number(AAV),
    Value = parse_number(Value)
  ) |>
  select(-TOI, -TOI.min, -ATOI)   # keep ATOI.min as the ice-time variable


# -----------------------------------------------------------------------------
# Step 3: Drop goalies
# -----------------------------------------------------------------------------
df <- df |> filter(Pos != "G")
stopifnot(nrow(df) == 1027)


# -----------------------------------------------------------------------------
# Step 4: Fix contracts that were matched to the wrong player
#   The file gives the 20-year-old defenseman Elias Pettersson the $11.6M
#   contract that belongs to the Canucks center with the same name.
#   His real deal is a 3-year entry-level contract (PuckPedia, signed
#   Apr 20, 2023): $863,333 cap hit, $2,589,999 total.
#   Add any other mismatches you find during the spot-check here.
#   A row with AAV_fix = NA is dropped below.
# -----------------------------------------------------------------------------
contract_fixes <- tribble(
  ~Player.name,       ~Age, ~Pos, ~AAV_fix, ~Value_fix, ~Length_fix,
  "Elias Pettersson", 20,   "D",  863333,   2589999,    3
) |>
  mutate(fixed = TRUE)

df <- df |>
  left_join(contract_fixes, by = c("Player.name", "Age", "Pos")) |>
  mutate(
    fixed  = coalesce(fixed, FALSE),
    AAV    = if_else(fixed, AAV_fix,    AAV),
    Value  = if_else(fixed, Value_fix,  Value),
    Length = if_else(fixed, Length_fix, Length)
  ) |>
  select(-AAV_fix, -Value_fix, -Length_fix, -fixed)

n_missing_aav <- sum(is.na(df$AAV))
if (n_missing_aav > 0) {
  message("Dropping ", n_missing_aav,
          " row(s) with no valid contract yet (see contract_fixes).")
  df <- df |> filter(!is.na(AAV))
}


# -----------------------------------------------------------------------------
# Step 5: Contract timing (GROUP DECISION)
#   Some AAVs are contracts signed after 2024-25 (e.g. Draisaitl's $14M
#   extension started in 2025-26). Set USE_2024_25_CAP_HITS to TRUE to swap
#   in the 2024-25 cap hit for the players listed below.
#   Verify every value against a contract database before using it.
# -----------------------------------------------------------------------------
USE_2024_25_CAP_HITS <- FALSE

cap_hit_2024_25 <- tribble(
  ~Player.name,      ~Age, ~AAV_2425,
  "Leon Draisaitl",  29,   8500000,   # confirmed
  "Mitch Marner",    27,   10903000,  # verify
  "Mikko Rantanen",  28,   9250000    # verify
)

if (USE_2024_25_CAP_HITS) {
  df <- df |>
    left_join(cap_hit_2024_25, by = c("Player.name", "Age")) |>
    mutate(AAV = coalesce(AAV_2425, AAV)) |>
    select(-AAV_2425)
}


# -----------------------------------------------------------------------------
# Step 6: Merge traded players (one row per player)
#   Match on name AND age so different players with the same name stay apart.
#   Counting stats are summed; ice time is a games-weighted average;
#   percentages are recalculated from the summed totals.
#   Team / conference / division = the team he played the most games for.
# -----------------------------------------------------------------------------
count_stats <- c("GP", "G", "A", "PTS", "+/-", "PIM", "EVG", "PPG", "SHG",
                 "GWG", "EV", "PP", "SH", "SOG", "TSA", "FOW", "FOL",
                 "BLK", "HIT", "TAKE", "GIVE")

df <- df |>
  group_by(Player.name, Age) |>
  arrange(desc(GP), .by_group = TRUE) |>        # first row = main team
  summarise(
    n_teams     = n(),
    Team        = first(Team),
    Conference  = first(Conference),
    Division    = first(Division),
    Pos         = first(Pos),
    Player.ascii = first(Player.ascii),
    AAV         = first(AAV),
    Value       = first(Value),
    Length      = first(Length),
    Awards      = first(na.omit(Awards)),
    ATOI.min    = weighted.mean(ATOI.min, GP),
    across(all_of(count_stats), sum),
    .groups = "drop"
  ) |>
  mutate(
    SPCT  = if_else(SOG > 0, 100 * G / SOG, NA_real_),
    `FO%` = if_else(FOW + FOL > 0, 100 * FOW / (FOW + FOL), NA_real_)
  )

stopifnot(!any(duplicated(df[c("Player.name", "Age")])))
message(sum(df$n_teams > 1), " traded players merged into one row each.")


# -----------------------------------------------------------------------------
# Step 7: Keep skaters with at least 20 games
#   Done after merging, so a traded player's games are added up first.
#   This also removes the two 1-game call-ups with no age.
# -----------------------------------------------------------------------------
df <- df |> filter(GP >= 20)
message(nrow(df), " skaters with 20+ games.")


# -----------------------------------------------------------------------------
# Step 8: Assign the generic "F" / "W" players to C, LW, or RW
#   These are the 30 F/W players left after merging and the 20-game filter.
#   Replace each NA with "C", "LW", or "RW" (in quotes).
#   Faceoffs per game are shown as a hint only: a player can line up at
#   center without taking the draws, so go by the position he actually played.
#   Names use the accent-free spelling (Player.ascii).
#   Anyone still NA is flagged in the final check.
# -----------------------------------------------------------------------------
fw_positions <- tribble(
  ~Player.ascii,          ~Age, ~Pos_fix,
  "Sebastian Aho",         27, "C",   # 15.2 FO/game, 79 GP
  "Macklin Celebrini",     18, "C",   # 15.0 FO/game, 70 GP
  "Pius Suter",            28, "C",   # 10.8 FO/game, 81 GP
  "Parker Kelly",          25, "C",   # 8.8 FO/game, 80 GP
  "Kyle MacLean",          25, "C",   # 7.0 FO/game, 81 GP
  "Max Sasson",            24, "C",   # 3.8 FO/game, 29 GP
  "Ivan Ivan",             22, "C",   # 3.4 FO/game, 40 GP
  "Teuvo Teravainen",      30, "RW",   # 1.9 FO/game, 82 GP
  "Drew O'Connor",         26, "LW",   # 1.4 FO/game, 84 GP
  "Ben Jones",             25, "C",   # 1.0 FO/game, 26 GP
  "Fabian Zetterlund",     25, "LW",  # 0.6 FO/game, 84 GP
  "Joel Kiviranta",        28, "LW",  # 0.6 FO/game, 79 GP
  "Adam Klapka",           24, "RW",  # 0.5 FO/game, 31 GP
  "Mathieu Olivier",       27, "RW",  # 0.4 FO/game, 82 GP
  "Mason Marchment",       29, "LW",  # 0.4 FO/game, 62 GP
  "Walker Duehr",          27, "RW",  # 0.4 FO/game, 24 GP
  "Collin Graf",           22, "RW",  # 0.4 FO/game, 33 GP
  "Marc Gatcomb",          25, "RW",  # 0.3 FO/game, 39 GP
  "Maxim Tsyplakov",       26, "RW",  # 0.3 FO/game, 77 GP
  "Bobby McMann",          28, "LW",  # 0.3 FO/game, 74 GP
  "Tanner Jeannot",        27, "LW",  # 0.3 FO/game, 67 GP
  "Cole Smith",            29, "RW",  # 0.2 FO/game, 71 GP
  "Tye Kartye",            23, "LW",  # 0.2 FO/game, 63 GP
  "Michael Carcone",       28, "LW",  # 0.2 FO/game, 53 GP
  "Justin Brazeau",        26, "RW",  # 0.2 FO/game, 76 GP
  "Mitchell Chaffee",      27, "RW",  # 0.2 FO/game, 66 GP
  "Ilya Mikheyev",         30, "RW",  # 0.1 FO/game, 80 GP
  "Ethen Frank",           26, "RW",  # 0.0 FO/game, 24 GP
  "Jesper Bratt",          26, "LW",  # 0.0 FO/game, 81 GP
  "Andrei Kuzmenko",       28, "LW"   # 0.0 FO/game, 66 GP
) |>
  mutate(Pos_fix = as.character(Pos_fix))

stopifnot(all(na.omit(fw_positions$Pos_fix) %in% c("C", "LW", "RW")))

df <- df |>
  left_join(fw_positions, by = c("Player.ascii", "Age")) |>
  mutate(Pos = if_else(Pos %in% c("F", "W"), Pos_fix, Pos)) |>
  select(-Pos_fix)


# -----------------------------------------------------------------------------
# Step 9: Build the model variables
# -----------------------------------------------------------------------------
df <- df |>
  mutate(
    log_AAV    = log(AAV),
    PPG_rate   = PTS / GP,                        # points per game
    Award      = factor(if_else(is.na(Awards), "No", "Yes"),
                        levels = c("No", "Yes")),  # any votes / All-Star
    Pos        = factor(Pos, levels = c("C", "LW", "RW", "D")),
    Conference = factor(Conference)
  )

analysis <- df |>
  select(
    Player.name, Age, Team,                       # identifiers / context
    log_AAV, AAV,                                 # outcome (+ raw salary)
    Pos, Conference, Award,                       # categorical predictors
    GP, PPG_rate, G, A, `+/-`, PIM, SOG,          # continuous predictors
    ATOI.min, BLK, HIT, TAKE, GIVE, Length
  )


# -----------------------------------------------------------------------------
# Step 10: Final check and save
# -----------------------------------------------------------------------------
n_unassigned <- sum(is.na(analysis$Pos))
if (n_unassigned > 0) {
  warning(n_unassigned, " F/W players still need C, LW, or RW in fw_positions (step 8):\n",
          paste(analysis$Player.name[is.na(analysis$Pos)], collapse = ", "))
}

model_vars <- c("log_AAV", "Conference", "Award", "Age", "GP", "PPG_rate",
                "G", "A", "+/-", "PIM", "SOG", "ATOI.min", "BLK", "HIT",
                "TAKE", "GIVE", "Length")
na_counts <- colSums(is.na(analysis[model_vars]))
stopifnot(all(na_counts == 0))

message("Final dataset: ", nrow(analysis), " rows x ", ncol(analysis), " columns")
print(table(analysis$Pos, useNA = "ifany"))

dir.create("data", showWarnings = FALSE)
write_excel_csv(analysis, "data/clean_stats.csv")
message("Saved to data/clean_stats.csv")