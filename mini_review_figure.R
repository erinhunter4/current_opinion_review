library("tidyverse")
library("dplyr")
library("data.table")
library("lubridate")
library("viridis")

setwd()
initial_input <- fread('sequences_2.csv', na.strings = c("", "NA"))

#remove dates before 1990
#remove entries where host is human 
edited_input <- initial_input%>%
  mutate(Release_year = year(Release_Date))%>%
  filter(Release_year > 1989 , Release_year != 2021)%>%
  mutate(Host = replace_na(Host, 'unknown'))%>%
  filter(Host != "Homo sapiens")

#calculate entries per year 

fig_1_total_entries <- edited_input %>%
  select(Release_year) %>%
  group_by(Release_year)%>%
  count()%>%
  rename(entries_per_year = n)

fig1_host_nas <- edited_input %>%
  select(Release_year, Host) %>%
  mutate(Host = na_if(Host, "unknown"))%>%
  group_by(Release_year)%>%
  summarise(host_nas = sum(is.na(Host)))

#reformat data for graph

total_data <- merge(fig_1_total_entries, fig1_host_nas, by = "Release_year") %>%
  mutate(assigned_host = entries_per_year - host_nas)%>%
  select(Release_year, assigned_host, entries_per_year)

#convert to cumulative count

cumulative <- total_data %>%
  mutate(cumulate_total = cumsum(entries_per_year))%>%
  mutate(cumulate_host = cumsum(assigned_host))%>%
  select(Release_year, cumulate_total, cumulate_host)

total_data_reformat <- cumulative %>%
  pivot_longer(!Release_year, names_to = "types", values_to = "count")

#plot graph

test_plot_1 <- ggplot(total_data_reformat, aes(x = Release_year, y = count, fill = fct_reorder(types, count, .desc = T)))+
  geom_area(position = "identity")+
  scale_fill_viridis(discrete = T, option = "viridis", name = "Legend", labels = c( "Total", "Assigned host"))+
  scale_y_continuous(labels = scales::scientific)+
  ggtitle("")+
  xlab("Year")+
  ylab("Virus nt records")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), text= element_text(size=25, family = "Helvetica"))
test_plot_1

ggsave("", test_plot_1, device = "png")

