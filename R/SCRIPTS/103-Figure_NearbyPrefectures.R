###------Nearby Figure-----
## @knitr NearbyFigure

fuk <- filter(regdat, origin == "Fukushima",
              X1 %in% c("Gumma", "Ibaraki", "Miyagi", "Niigata", "Tochigi", "Yamagata")) %>%
  left_join(., evacueeper) %>%
  dplyr::select(origin, X1, destper2010, destper2011, destper2012, destper2013, evac_per) %>%
  gather(MigYear, Per, destper2010:evac_per) %>%
  mutate(MigrationType = ifelse(MigYear == "evac_per", "Evacuees", "Permanent Migrants"),
         Year = case_when(
           MigYear == "destper2010" ~ "2010",
           MigYear == "destper2011" ~ "2011",
           MigYear == "destper2012" ~ "2012",
           MigYear == "destper2013" ~ "2013",
           MigYear == "evac_per"  ~ "Evacuees"
         )) %>%
  group_by(Year, MigrationType) %>%
  dplyr::summarise(migration = sum(Per))

ggplot() +
  geom_bar(data = fuk, aes(x=Year, y = migration, fill = MigrationType), stat = "identity") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("gray70", "black")) +
  theme(legend.position="none") +
  labs(x = "Migration Year",
       y = "Percent of Out Migrants to Surrounding Prefectures") +
  NULL
