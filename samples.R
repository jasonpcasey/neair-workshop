ipeds_adm <- read.csv('IPEDS_admissions_subset.csv',
                      check.names = FALSE,
                      stringsAsFactors = FALSE)
names(ipeds_adm)

str(ipeds_adm)

summary(ipeds_adm)

table(ipeds_adm$`Control of institution`)

table(ipeds_adm$`Control of institution`,ipeds_adm$ACAD_YEAR)

table(ipeds_adm$`State abbreviation`)

library(tidyverse)

ds <- ipeds_adm %>%
  filter(`State abbreviation_value` %in% c('MA','NY','NJ') &
           `Control of institution`=='Public') %>%
  mutate(`Admit rate`=`Admissions total`/`Applicants total`,
         `Yield`=`Enrolled total`/`Admissions total`)

plot(ds$`Enrolled total`,ds$`Admissions total`)
points(ds$`Enrolled total`, ds$`Applicants total`, col='red')

ggplot(ds,
       aes(x=`Enrolled total`,
           y=`Admissions total`)) +
  geom_point() +
  geom_point(aes(x=`Enrolled total`,
                 y=`Applicants total`),
             color='red')

ggplot(ds,
       aes(x=`Enrolled total`,
           y=`Admissions total`,
           color=`Applicants total`)) +
  geom_point()

ipeds_adm %>%
  filter(`State abbreviation_value` %in% c('MA','NY','NJ') &
           `Control of institution`=='Public') %>%
  mutate(`Admit rate`=`Admissions total`/`Applicants total`,
         `Yield`=`Enrolled total`/`Admissions total`) %>%
  ggplot(aes(x=`Admit rate`)) +
  geom_histogram(binwidth=0.1)

ggplot(ds,
       aes(x=Yield)) +
  geom_histogram(binwidth=0.1)

ggplot(ds,
       aes(x=`Admit rate`,
           y=Yield,
           color=`State abbreviation`)) +
  geom_point() +
  scale_x_continuous(name='Admit Rate',
                     limits=c(0,1),
                     labels=scales::percent) +
  scale_y_continuous(name='Yield',
                     limits=c(0,1),
                     labels=scales::percent) +
  ggtitle('Admit vs Yield Rate') +
  theme_classic()

ds1_group <- ds %>%
  group_by(`State abbreviation`,
           ACAD_YEAR) %>%
  summarise(N = n(),
            avg_admit = mean(`Admit rate`))

p2 <- ggplot(ds1_group,
       aes(x=ACAD_YEAR,
           y=avg_admit,
           group=`State abbreviation`,
           color=`State abbreviation`,
           shape=`State abbreviation`)) +
  geom_line(lwd=1.5)

p2 <- p2 +
  geom_point(size=3)

p2 <- p2 +
  scale_x_continuous(name='Academic Year',
                     breaks=seq(2009,2016, by=1)) +
  scale_y_continuous(name='Average Admit Rate',
                     limits=c(0,1),
                     labels=scales::percent) +
  scale_color_manual(name='',
                     labels=c('MA','NJ','NY'),
                     values=c('darkred','darkgreen','darkblue')) +
  scale_shape_manual(name='',
                     labels=c('MA','NJ','NY'),
                     values=c(15,16,17))
p2 <- p2 +
  ggtitle('Average Admit Rate Over Time') + 
  theme_bw()

p2

ds3 <- ds %>% 
  select(UNITID,
         `Institution (entity) name`,
         `State abbreviation`,
         `Admission test scores`,
         ACAD_YEAR)

ggplot(ds3,
       aes(x=ACAD_YEAR,
           fill=`Admission test scores`)) +
  geom_bar(position='fill') +
  scale_y_continuous(name='Percentage of Institutions',
                     limits = c(0,1),
                     labels=scales::percent) +
  scale_x_continuous(name='Academic Year',
                     breaks=seq(2009,2016, by=2)) + 
  scale_fill_manual(name='Admission Test Policy',
                    values=c('gold','blue','purple')) +
  facet_grid(.~`State abbreviation`) +
  ggtitle('Admission Test Score Requirements Over Time') +
  theme_dark()
