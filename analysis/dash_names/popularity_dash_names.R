### setup

## paths

setup_path <- file.path(root_path,"functions","general purpose","setup.RData")
load_path <- file.path(root_path,"datasets","data","first_names_births_belgium")
save_path <- file.path(root_path,"first_names","output","dash_names")

## setup environment

load(setup_path)

setup()

### load

load(file.path(load_path,"clean_first_names_births_belgium.RData"))

### create variable indicating if is dash name or not?

first_names$dash <- grepl(pattern = "-", x = first_names$name)

### dash names by year

dash_names_by_year <- first_names %>%
                          group_by(year) %>%
                              summarise(nr_dash = sum(dash))

graph_dash_names_by_year <- ggplot(data = dash_names_by_year,
                                  aes(x = year, y = nr_dash)) +
                            geom_line(group = 1) + 
                            theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(graph_dash_names_by_year)

### save

ggsave(plot = graph_dash_names_by_year,
       filename = file.path(save_path,"graph_dash_names_by_year.png"))
                              