###------Japan Map-----
## @knitr JapanMap

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
         ))

# Coordinates come from https://earthobservatory.nasa.gov/images/49621/earthquake-and-tsunami-near-sendai-japan
eqloc<- SpatialPoints(data.frame(x = 142.4, y = 38.3))

japan <- append_data(japan, fmig, key.shp= "NAM", key.data ="NAM")

tm_shape(japan, bbox = japan_bb) +
  tm_fill(c("m2010", "m2011"),
          title = c("% out-migrants 2010", "% out-migrants 2011"),
          textNA = "Fukushima",
          breaks = c(0, 0.01, 0.030, 0.04, 0.060, 0.10, 0.184)) +
  tm_layout(legend.format=list(fun=function(x) paste0(x*100, "%"))) +
  tm_borders() +
  tm_shape(eqloc) +
  tm_symbols(col = "red", size = 0.5) +
  tm_credits("Fukushima", position = c(0.61, 0.47)) +
  tm_compass() +
  tm_scale_bar()

