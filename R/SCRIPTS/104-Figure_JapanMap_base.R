###------Japan Map-----
## @knitr JapanMapbase

fukushima <- subset(migdata5, origin == "Fukushima") %>%
  filter(., year %in% c("X2010", "X2011")) %>%
  #mutate(NAM = "FUKUSHIMA") %>%
  separate(X1, "NAM", sep = "-") %>%
  ungroup()

f2010 <- fukushima %>%
  filter(year == "X2010") %>%
  dplyr::select(NAM, m2010= dest_per)


f2011 <- fukushima %>%
  filter(year == "X2011") %>%
  dplyr::select(NAM, m2011= dest_per)

fmig <- left_join(f2010, f2011, by = "NAM") %>%
  mutate(NAM = str_upper(NAM),
         NAM = case_when(
           NAM == "GIFU" ~ "GIHU",
           NAM == "GUMMA" ~ "GUNMA",
           TRUE ~ as.character(NAM)
         ),
         Impacted = ifelse(NAM %in% c("FUKUSHIMA", "MIYAGI", "IWATE"), "gray70", "gray93"))

# Coordinates come from https://earthobservatory.nasa.gov/images/49621/earthquake-and-tsunami-near-sendai-japan
eqloc<- SpatialPoints(data.frame(x = 142.4, y = 38.3))

japan <- append_data(japan, fmig, key.shp= "NAM", key.data ="NAM")
japan2 <- japan
japan2$NAM <- str_title_case(str_lower_case(japan2$NAM))




tm_shape(japan2, bbox= japan_bb) +
  tm_fill(col = "Impacted") +
  tm_borders(col = "white") +  
  tm_text("NAM", size =0.3) +
tm_shape(eqloc) +
  tm_symbols(col = "red", size = 0.5) +
  # tm_credits("Fukushima", position = c(0.61, 0.47)) +
  # tm_shape(japan[japan$NAM == "FUKUSHIMA",]) +
  #   tm_text("NAM", just= "left", xmod=1.4) +
  tm_compass() +
  tm_scale_bar()

