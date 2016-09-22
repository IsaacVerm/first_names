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

### unique dash names by year

## function to calculate number of dash names by variables of choice

calculate.nrdash <- function(df, ...) {

  dash_names_var <- df %>%
    group_by_(...) %>%
      summarise(nr_dash = sum(dash))  
  
  return(dash_names_var)
}

## calculate number of unique dash names

# by year, belgium only

unique_dash_belgium <- calculate.nrdash(df = filter(first_names, region == "België"),
                                    "year")

# by year, region and gender

unique_dash_cross_all <- calculate.nrdash(df = filter(first_names, region != "België"),
                                      "year","region","gender")

## function to make graph by year

graph.year <- function(df) {
  
  graph <- ggplot(data = df,
                  aes(x = year, y = nr_dash)) +
    geom_line(group = 1) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(graph)
  
  }

## graph

# by year, belgium only

graph_unique_dash_belgium <- graph.year(unique_dash_belgium) +
                         labs(title = "Number of unique dash names by year",
                              y = "number of names with dashes")
plot(graph_unique_dash_belgium)

# by year, region and gender

graph_unique_dash_cross_all <- graph.year(unique_dash_cross_all) + 
                           facet_grid(gender~region) +
                           labs(title = "Number of unique dash names by year, region and gender",
                                y = "number of names with dashes") +
                            scale_y_continuous(breaks = seq(from = 0, to = 15, by = 5),
                                               minor_breaks = waiver())
plot(graph_unique_dash_cross_all)

### total dash names by year

## function to calculate total dash names

calculate.totaldash <- function(df, ...) {
  
  total <- df %>%
    group_by_(...) %>%
    summarise(total_dash = sum(dash*count))  
  
  return(total)
}

## calculate total dash names by year

total_dash_belgium <- filter(first_names, region == "België") %>%
                          calculate.totaldash("year")

## graph

graph_total_dash_belgium <- ggplot(data = total_dash_belgium,
                                   aes(x = year, y = total_dash)) +
  geom_line(group = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot(graph_total_dash_belgium)

### investigate conspicuous patterns

### save

graphs <- mget(ls()[grep(x = ls(), pattern = "graph_")])

mapply(ggsave, file = file.path(save_path,paste0(names(graphs),".png")), plot = graphs)

                              