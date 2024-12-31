# install.packages("devtools")
#devtools::install_github("JaseZiv/worldfootballR")
#install.packages("writexl")
library(writexl)
library(worldfootballR)

#----- for one team: -----#
VVV <- tm_team_transfers(team_url = "https://www.transfermarkt.com/vvv-venlo/startseite/verein/1426/saison_id/2023", transfer_window = "all")
dplyr::glimpse(VVV)
write_xlsx(VVV, path = "vvv_transfers_2023.xlsx")
