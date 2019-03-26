

library(tidyverse)
library(rjson)
library(rvest)
library(ggtern)

#

teams_list <- vector("list", 10)

for(y in 1:10) {
  
  dt <- paste0("https://kenpom.com/index.php?y=", y + 2009) %>%
    
    read_html() %>%
    html_nodes(xpath = '//*[@id="ratings-table"]') %>%
    html_table() %>%
    .[[1]] %>%
    janitor::clean_names() %>%
    mutate(year = y + 2009) %>%
    select(
      
      team = x_2,
      year,
      conf = x_3,
      off = x_6,
      def = x_8,
      tempo = x_10,
      sos_off = strength_of_schedule_3,
      sos_def = strength_of_schedule_5)
  
  teams_list[[y]] <- dt
  
}

team_data <- bind_rows(teams_list) %>%
  filter(!team == "Team") %>%
  mutate(off = as.numeric(off),
         def = as.numeric(def),
         tempo = as.numeric(tempo),
         sos_off = as.numeric(sos_off),
         sos_def = as.numeric(sos_def)) %>%
  mutate(team = gsub('[[:digit:]]+', '', team)) %>%
  mutate(team = str_trim(team)) %>%
  filter(!is.na(off)) %>%
  mutate(margin = (off - 104) + (104 - def))

#

f_l <- list.files("Torvik/")

#

data_list <- vector("list", length(f_l))
  
#

for(y in 1:length(f_l)) {
  
  d <- read_csv(paste0("Torvik/", f_l[[y]])) %>%
    
    janitor::clean_names() %>%
    
    mutate(inches = as.numeric(str_sub(x26, 1, 1)) * 12 + as.numeric(str_sub(x26, 3, nchar(x26)))) %>%
    
    select(name = x0,
           school = x1,
           conf = x2,
           class = x25,
           year = x31,
           inches,
           games = x3,
           mins = x4,
           ortg = x5,
           usg = x6,
           efg = x7,
           ts = x8,
           orr = x9,
           drr = x10,
           ast = x11,
           to = x12,
           ftm = x13,
           fta = x14,
           ftp = x15,
           x2m = x16,
           x2a = x17,
           x2p = x18,
           x3m = x19,
           x3a = x20,
           x3p = x21,
           blk = x22,
           stl = x23,
           ftr = x24,
           portp = x28)
  
  data_list[[y]] <- d
  
}
  
#

all_data <- bind_rows(data_list) %>%
  
  filter(!(school == "Eastern Illinois" & year == 2015)) %>%
  
  mutate(inches = ifelse(inches < 65, 77, inches)) %>%
  
  mutate(fgs = x2a + x3a) %>%
  
  group_by(school, year) %>%
  
  mutate(teammates = sum(mins * portp, na.rm = T) / sum(mins, na.rm = T),
         team_fg_game = sum(fgs, na.rm = T) / max(games),
         team_height = sum(inches * (mins / 100), na.rm = T) / sum(mins / 100, na.rm = T)) %>%
  
  ungroup() %>%
  
  inner_join(
    
    team_data %>%
      select(-conf, -margin, -off, -def), by = c("school" = "team", "year")
    
  ) %>%
  
  mutate(team_fg_game = team_fg_game * (mins / 100),
         involve = (team_fg_game * (ast / 100)) / tempo) %>%
  
  select(-fgs, -team_fg_game) %>%
  
  filter(mins > 30 & games > 14) %>%

  mutate(sh_poss = ((fta / 2.2) + (x2a + x3a)),
         x3r = x3a / sh_poss,
         x2r = x2a / sh_poss) %>%
  select(-sh_poss, -fta, -ftm, -x2a, -x2m, -x3a, -x3m) %>%
  
  mutate(class = ifelse(class == "N/A", "Fr", class),
         zblk = (blk - mean(blk)) / sd(blk),
         zstl = (stl - mean(stl)) / sd(stl),
         zorr = (orr - mean(orr)) / sd(orr)) %>%
  
  mutate(athletic = (zblk + zstl + zorr) / 3) %>%
  
  select(-zblk, -zstl, -zorr) %>%
  
  mutate(teammates = ((480 * teammates) - (mins * portp)) / (480 - mins),
         team_height = inches - ((4.8 * team_height) - (mins / 100 * inches)) / (4.8 - (mins / 100))) %>%
  
  select(name, school, conf, class, year, inches, games, mins, teammates, team_height, tempo, sos_off, sos_def, 
         ortg, usg, involve, portp, athletic, x3r, x2r, ftr, x3p, x2p, ftp, ts, orr , drr, to, blk, stl) %>%
  
  mutate(name = ifelse(name == "Lamont Robinson" & year == 2015, "Junior Robinson",
                       ifelse(name == "Temetrius Morant" & year == 2018, "Ja Morant",
                              ifelse(name == "BK Ashe" & year == 2016, "Byron Ashe", name))))

#

set.seed(17843)

pca <- prcomp(all_data[, 14:30], scale = T)

#

all_data <- cbind(
  
  all_data,
  
  pca$x[, 1:5]
)

#
#
#
#
#

ggtern_data <- all_data %>%
  mutate(PC1 = PC1 * -1, 
         PC2 = PC2 * -1, 
         PC3 = PC3 * -1) %>%
  
  group_by(name, school) %>%
  
  summarize(PC1 = sum(PC1 * mins, na.rm = T) / sum(mins, na.rm = T),
            PC2 = sum(PC2 * mins, na.rm = T) / sum(mins, na.rm = T),
            PC3 = sum(PC3 * mins, na.rm = T) / sum(mins, na.rm = T)) %>%
  
  ungroup() %>%
  
  select(PC1, PC2, PC3, name, school) %>%
  mutate(PC1 = PC1 / sd(PC1), 
         PC2 = PC2 / sd(PC2), 
         PC3 = PC3 / sd(PC3)) %>%
  #filter(school == "Duke") %>%
  mutate(PC1 = (PC1 * 25) + 50, 
         PC2 = (PC2 * 25) + 50, 
         PC3 = (PC3 * 25) + 50) %>%
  mutate(PC1 = ifelse(PC1 > 99, 99, ifelse(PC1 < 1, 1, PC1)), 
         PC2 = ifelse(PC2 > 99, 99, ifelse(PC2 < 1, 1, PC2)), 
         PC3 = ifelse(PC3 > 99, 99, ifelse(PC3 < 1, 1, PC3)))

#

team_type_data <- all_data %>%
  mutate(PC1 = PC1 * -1, 
         PC2 = PC2 * -1, 
         PC3 = PC3 * -1) %>%
  
  select(PC1, PC2, PC3, name, school, year, mins) %>%
  mutate(PC1 = PC1 / sd(PC1), 
         PC2 = PC2 / sd(PC2), 
         PC3 = PC3 / sd(PC3)) %>%
  #filter(school == "Duke") %>%
  mutate(PC1 = (PC1 * 25) + 50, 
         PC2 = (PC2 * 25) + 50, 
         PC3 = (PC3 * 25) + 50) %>%
  mutate(PC1 = ifelse(PC1 > 99, 99, ifelse(PC1 < 1, 1, PC1)), 
         PC2 = ifelse(PC2 > 99, 99, ifelse(PC2 < 1, 1, PC2)), 
         PC3 = ifelse(PC3 > 99, 99, ifelse(PC3 < 1, 1, PC3))) %>%
  
  group_by(school, year) %>%
  
  summarize(PC1 = sum(PC1 * mins, na.rm = T) / sum(mins, na.rm = T),
            PC3 = sum(PC3 * mins, na.rm = T) / sum(mins, na.rm = T)) %>%
  
  ungroup()

#

ggtern(ggtern_data, aes(PC1, PC2, PC3, label = name)) + 
  geom_point(fill="red",shape=21,size=3)+ geom_label(size = 3)

#

ggplot(ggtern_data %>% filter(school == "Maryland"),
       aes(x = PC1, y = PC3, label = name))+
  
  geom_hline(yintercept = 50)+
  geom_vline(xintercept = 50)+
  geom_point(fill="red", shape=21, size=3)+
  geom_label(size = 4)+
  coord_cartesian(xlim = c(-10,110), 
                  ylim = c(0,100))+
  annotate(x = c(100,100,0,0), 
           y = c(100,0,0,100), 
           label = c("Big man creators", "Big man role players", "Wing role players", "Guard Creators"), 
           geom = "label", 
           size = 6, 
           fill = "black", 
           color = "white")

#
#
#
#
#

player_comps <- all_data %>%
  
  mutate(sos_em = sos_off - sos_def) %>%
  
  select(-PC4, -PC5, -ortg, -sos_off, -sos_def, -x2p, -ftp, -ts, -tempo) %>%
  
  gather(stat, value, -name, -school, -conf, -class, -year, -inches, -games) %>%
  
  group_by(stat, year) %>%
  
  mutate(z = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
  
  ungroup() %>%
  
  group_by(school, name, stat) %>%
  
  summarize(zscore = mean(z, na.rm = T)) %>%
  
  ungroup() %>%
  
  filter(school == "Duke" & name == "Zion Williamson") %>%
  
  select(-school) %>%
  
  rename(arch = name, arch_value = zscore) %>%
  
  inner_join(
    
    all_data %>%
      
      mutate(sos_em = sos_off - sos_def) %>%
      
      select(-PC4, -PC5, -ortg, -sos_off, -sos_def, -x2p, -ftp, -ts, -tempo) %>%
      
      gather(stat, value, -name, -school, -conf, -class, -year, -inches, -games) %>%
      
      group_by(stat, year) %>%
      
      mutate(z = (value - mean(value, na.rm = T)) / sd(value, na.rm = T)) %>%
      
      ungroup() %>%
      
      group_by(school, name, stat) %>%
      
      summarize(zscore = mean(z, na.rm = T)) %>%
      
      ungroup(), by = c("stat")
    
  ) %>%
  
  mutate(weight = ifelse(stat %in% c("sos_em", "usg", "portp", "involve", "athletic", "x3r", "team_height"), 2,
                         ifelse(stat %in% c("PC1", "PC3"), 4,
                                ifelse(stat %in% c("PC2", "mins"), 0, 1))),
         diff = abs(arch_value - zscore)) %>%
  
  group_by(name, school, arch) %>%
  
  summarize(sim = sum(diff * weight, na.rm = T) / sum(weight, na.rm = T)) %>%
  
  ungroup()
  
  

