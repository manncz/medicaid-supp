##'
##' title: "Explore additional counties added week by week in CDC data"
##' output: github_document
##' 
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "stringr")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' We have been downloading the CDC provisional data weekly for around 7 weeks (missing 1 week).

weeks <- str_extract(list.files(path = "../data-raw/cdc-2020-mort"), "\\d{1,2}.\\d\\d")
raw.dat <- list()

weeks <- weeks[c(20:32, 8:19, 1:7)]

for (week in weeks){
  raw.dat[[paste0("cdc_", week)]] <- read.csv(paste0("../data-raw/cdc-2020-mort/Provisional_COVID-19_Death_Counts_in_the_United_States_by_County_", week, ".csv")) %>%
    dplyr::select(fips = 6) %>%
    mutate(week = 1)
}

cdc_weeks <- raw.dat[["cdc_5.16"]] %>%
  rename_with(~paste0(.x, "5.16"),  contains("week"))

for (week in weeks[-1]){
  cdc_weeks <-  raw.dat[[paste0("cdc_", week)]] %>%
    rename_with(~paste0(.x, week),  contains("week")) %>%
    full_join(cdc_weeks, by = c("fips"))
}

cdc_incl <- cdc_weeks %>%
  pivot_longer(cols = contains("week"),
               names_to = "week", names_prefix = "week", 
               values_to = "incl") %>%
  filter(incl == 1) %>%
  dplyr::select(FIPS = fips, week) %>%
  group_by(FIPS) %>%
  mutate(date.raw = case_when(week %in% weeks[26:32] ~ paste0(week, ".21"),
                              TRUE ~ paste0(week, ".20")),
         date = mdy(date.raw)) %>%
  mutate(min.week = min(date))

base_cnty <- read.csv("../data/base_cnty_all.csv")

cdc_incl <- cdc_incl %>%
  left_join(base_cnty, by = "FIPS")

##' We calculate the total age adjusted population in our county data and the 
##' average of the "% Urban" measurement to compare with what we see over time in
##' the censored data.

all_pop <- sum(base_cnty$adjcnt_adult_w_a[!(base_cnty$stateName %in% c("Alaska", "Hawaii"))])
av_urbn <- mean(base_cnty$pctUrban_2010[!(base_cnty$stateName %in% c("Alaska", "Hawaii"))])

##' Summarizing over the weeks, the percent of total population represented
##' in the uncensored counties and the average of the "% Urban" measurement in 
##' the uncensored counties.

summary_by_week <- cdc_incl %>%
  group_by(week, date) %>%
  filter(!(stateName %in% c("Alaska", "Hawaii"))) %>%
  summarize(urban = mean(pctUrban_2010),
            pop = sum(adjcnt_adult_w_a),
            n_county = n()) %>%
  mutate(perc_pop = pop/all_pop)

g <- ggplot(data = summary_by_week)

##' As expected, the \% of population represented starts growing more slowly
##' over time as the counties remaining to be included are small counties. Currently
##' around 85\% of the age adjusted population is represented in uncensored counties.

#+ echo = FALSE
g + geom_point(aes(x = date, y = perc_pop)) +
  ggtitle("Percent of Total Age Adjusted Population Uncensored") +
  ylab("% Total Population in Uncensored Counties")

##' The \% urban drops over time as expected, although the average overall is 41.48\%,
##' while the average of the uncensored data at it's lowest is around 70\%.

#+ echo = FALSE
g + geom_abline(intercept = av_urbn) +
  geom_point(aes(x = date, y = urban)) +
  ggtitle("Average % Urban Measure") +
  ylab("Average % County Urban")
  

##' Now lets just see how counties have been filled in over time. We still don't have many counties from the
##' big plains states, but otherwise, have representation from different states.

county_mapping2 <- county_mapping %>%
  dplyr::select(-FIPS) %>%
  rename(FIPS = fips) 

plot.dat <- cdc_incl %>%
  right_join(county_mapping2)


cols <- colorRampPalette(brewer.pal(9, "Blues"))(32)
  
ggplot() +
  geom_polygon(data = plot.dat, aes(x=long, y=lat, group = group, fill = factor(min.week)), color = 'gray75', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = cols, name = "",
                    na.value="grey") +
  ggtitle("Status of County Censoring over Time") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
  )



#rmarkdown::render("05-cdc-censored-counties.R", "github_document")





