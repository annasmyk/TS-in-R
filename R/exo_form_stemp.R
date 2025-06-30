#Exercice 2.2

AC <- 5
BC <- 13
AB <- sqrt(BC^2-AC^2)

# Exercice 2.3
nom <- "Keihanaikukauakahihuliheekahaunaele"
nchar(nom)

# Exercice 2.4
alice <- sample(x = 1:100,size = 3)
min(alice)

rep(x=c(1,3), times = 4)
rep(x=c(1,3), each = 4)
?rep

#Ex 2.5
sqrt(1:100)

#Ex 2.6
seq (from = 2004, to = 2050, by = 4)

#Ex 2.7
letters
letters[24]

pop <- data.frame(
  code = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  dep = c(42L, 42L, 7L, 91L, 91L, 7L, 7L, 7L, 42L, 42L),
  pop = c(530, 150542, 24561, 1021312, 552654, 202135,
          88541, 12021, 2100000, 987661)
)
head(pop)

pop_tri <- pop %>% arrange (pop,dep)

pop_commune <- pop %>% filter(pop == min(pop, na.rm = TRUE), .by = dep)

pop_ratio <- pop %>% summarise (ratio = min(pop)/max(pop), .by = dep)

long_pop <- pop %>%
  pivot_longer(cols = c(dep, pop))
head(long_pop)

ipi <- read.csv("Z:/Series-Temporelles-R_Initiation/data/IPI_nace4.csv",sep=";")
ipi$date <- as.Date(ipi$date,format = "%d/%m/%Y")

ipi_filtre <- ipi %>% filter(date >="2012-01-01" & date <= "2020-12-01")

ipi_min <- ipi_filtre %>% pivot_longer(cols=(c(RF0610:E4300Y1)))
ipi_min <- ipi_min %>% filter(value == min(value)|value==max(value))

RF0899 <- ipi %>% select(date,RF0899) %>% 
  mutate (month = month(date)) %>% 
  mutate (month_mean = mean(RF0899), .by = month) %>% 
  mutate (month_mean = month_mean - mean(month_mean),
          RF0899_sa = RF0899 - month_mean)

ipi3 <- ipi %>% pivot_longer(cols = -date) %>% 
  mutate (month = month(date)) %>% 
  mutate (month_mean = mean(value), .by = c(month, date)) %>% 
  mutate (month_mean = month_mean - mean(month_mean), .by = name) %>% 
  mutate (value_sa = value - month_mean) %>% 
  pivot_wider (id_cols = date, values_from = value_sa)

ipi_ts <- ts(ipi %>% pull(RF0899), start = 1990, frequency = 12)  
mean_monthly <- apply.quarterly(as.xts(ipi_ts), FUN = colMeans)
