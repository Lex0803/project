# install.packages("rnaturalearth")

# Load it PROPERLY (many people forget this step)
library(rnaturalearth)

# Now this will work:
nl_map <- ne_states(country = "netherlands", returnclass = "sf")
plot(nl_map$geometry)