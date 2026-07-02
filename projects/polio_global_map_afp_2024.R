## R9 - EOC Map Update weekly 
## Global Polio Map - need to check output and reference 
## Updated - 13-02-2025 
## By: JPB 

# Load Packages 
source("./reference/obx_packages_01.R", local = T)

# Load Data 
if (exists("raw.data") == TRUE){
  cli::cli_alert("Core data already loaded")
}else{
  raw.data <- get_all_polio_data(size = "small")
}

# Load Inputs 
# Dates (last 12 months)
data_date <- lubridate::floor_date(raw.data$metadata$download_time, "week", week_start = 1)

date_end <- lubridate::as_date("2024-12-31")
date_start <- lubridate::as_date("2024-01-01")
# Epi Week 
e_w <- paste0("W",as.character(epiweek(date_end)),",", sub="")
e_w1 <- paste0("W",as.character(epiweek(date_end)), sub="")
# Virus types 
o_vtype <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "WILD 1")
# Surveillance sources 
s1 <- c("AFP")
# Endemic Countries to highlight  
endem_ctry <- c("AFGHANISTAN", "PAKISTAN")

# Output 
datt_task_id <- "R37"
sub_folder <- "3. Figures"

# with table 
fig_name2 <- paste("map_pvdetect_12m_tab_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path2 <- file.path(tempdir(), fig_name2)
sp_path2 <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name2, sep ="")


fig_name1 <- paste("map_pvdetect_12m_fig_",e_w1,"_", format(today(), "%y%m%d"),".png", sep="")
temp_path1 <- file.path(tempdir(), fig_name1)
sp_path1 <- paste("./Data Analytics Task Team/",sub_folder, "/", datt_task_id,"/", fig_name1, sep ="")

# Do you want to upload to sharepoint 
uploadtosp <- "yes"

## Load Sharepoint Site & Writing Functions 
get_sharepoint_site(
  site_url = "https://cdc.sharepoint.com/teams/GHC_GID_Data__Strategy_Tiger_Team",
  tenant = Sys.getenv("CLIMICROSOFT365_TENANT", "common"),
  app = Sys.getenv("CLIMICROSOFT365_AADAPPID"),
  scopes = c("Group.ReadWrite.All", "Directory.Read.All", "Sites.ReadWrite.All",
             "Sites.Manage.All"),
  token = NULL)


# Get dataset 
pos1 <- raw.data$pos %>% 
            filter(dateonset >= date_start & 
                   dateonset <= date_end & 
                   report_date <= date_end & 
                   measurement %in%  o_vtype &
                   source %in% s1) %>%
            distinct(epid, measurement, .keep_all = T) %>% 
            mutate(
              vtype_all = case_when(
                measurement == "cVDPV 1" &  source == "AFP" ~ "cVDPV1 case", 
                # measurement == "cVDPV 1" &  source == "ENV" ~ "cVDPV1 ES", 
                measurement == "cVDPV 2" &  source == "AFP" ~ "cVDPV2 case", 
                # measurement == "cVDPV 2" &  source == "ENV" ~ "cVDPV2 ES", 
                measurement == "cVDPV 3" &  source == "AFP" ~ "cVDPV3 case", 
                # measurement == "cVDPV 3" &  source == "ENV" ~ "cVDPV3 ES", 
                measurement == "WILD 1"  &  source == "AFP" ~  "WPV1 case"), 
                # measurement == "WILD 1"  &  source == "ENV" ~  "WPV1 ES"),
                vtype_all1 = factor(vtype_all, levels = 
                                    c("WPV1 case","cVDPV1 case", "cVDPV2 case", "cVDPV3 case")))

# Pull coordinates for map sizing - will need to check each week: 
# Some suggested ones based on current data 
# coord_sf(xlim = c(-55, 145), ylim = c(-35, 68), expand = FALSE) + 

t1a <- pos1 %>% 
       mutate(latitude = as.numeric(latitude)) %>% 
      arrange(latitude) %>% 
      summarise(min_lat = min(latitude), 
                max_lat = max(latitude)) 

t1b <- pos1 %>% 
  mutate(longitude = as.numeric(longitude)) %>% 
  arrange(longitude) %>% 
  summarise(min_long = min(longitude), 
            max_long = max(longitude)) 
  # Fix proxy to get shape files back 


ctry <- raw.data$global.ctry %>% 
         mutate(endemic = if_else(ADM0_NAME %in% endem_ctry, "Y", "N"))

ctry_sub <- ctry %>% filter(endemic == "Y")

# Create graph 
g1 <- ggplot() + 
  geom_sf(data=ctry, fill = "white", color = "grey70", lwd = 0.5, show.legend = NA) +
  geom_sf(data=ctry_sub, aes(fill=endemic), color = "grey70", lwd = 0.5, show.legend = NA) +
  geom_point(data = pos1 %>% filter(source=="ENV"), aes(x = as.numeric(longitude),
                                y = as.numeric(latitude),
                                color=vtype_all1, shape = vtype_all1), size = 2.0, show.legend = T) +
  geom_point(data = pos1 %>% filter(source=="AFP"), aes(x = as.numeric(longitude),
                              y = as.numeric(latitude),
                              color=vtype_all1, shape = vtype_all1), size = 2.4, show.legend = NA) +
  # coord_sf(xlim = c(-55, 145), ylim = c(-35, 68), expand = FALSE) + 
  coord_sf(xlim = c(t1b$min_long-5, t1b$max_long+5), ylim = c(t1a$min_lat-10.5, t1a$max_lat+7), expand = FALSE) + 
  scale_color_manual(name = "Poliovirus\ndetection:", 
                    values = c("cVDPV1 case" = "#FF9600",
                               "cVDPV2 case"= "#38A800", 
                               "cVDPV3 case" = "#B30DA7", 
                               "WPV1 case" = "#FF0000",
                               "cVDPV1 ES" = "#F4CC00",
                               "cVDPV2 ES"= "#61DB2C", 
                               "cVDPV3 ES" = "#A658FF", 
                               "WPV1 ES" = "#F9DADB"), 
                    breaks = c("WPV1 case", "WPV1 ES", "cVDPV1 case", "cVDPV1 ES", 
                               "cVDPV2 case", "cVDPV2 ES", "cVDPV3 case", "cVDPV3 ES"), 
                    drop = FALSE) +
  scale_shape_manual(name = "Poliovirus\ndetection:",
                     values = c("cVDPV1 case" = 16,
                                "cVDPV2 case"= 16,
                                "cVDPV3 case" = 16,
                                "WPV1 case" = 16,
                                "cVDPV1 ES" = 15,
                                "cVDPV2 ES"= 15,
                                "cVDPV3 ES" = 15,
                                "WPV1 ES" = 15),
                     breaks = c("WPV1 case", "WPV1 ES", "cVDPV1 case", "cVDPV1 ES", 
                                "cVDPV2 case", "cVDPV2 ES", "cVDPV3 case", "cVDPV3 ES"),
  drop = FALSE)  +
  scale_fill_manual(name = " \n \n", values = c("Y"="#FFFFBE"), labels = "Endemic country (WPV1)") +
  theme_bw() + 
  theme(legend.position = "none",
        legend.title = element_text(face="bold", size = 16),
        legend.text = element_text(size = 14),
        axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "#E3EBFF"),
        legend.key = element_blank(),
        plot.title = element_markdown(hjust = 0.5, face="bold", size = 24),
        plot.caption = element_text(size =8)) +
  guides(
    color = guide_legend(override.aes = list(size = 4), nrow = 2),
     fill = guide_legend(override.aes = list(size = 0), nrow = 1)) 
 
ggsave(filename = "./24_map_afp.png", height = 6, width = 16, units = "in", dpi = 300, bg = "white")


# # Test print -> check what rolw is missing from the scale 
print(g1)


# Quick count Table
# Create Table
# Flextable Defaults
set_flextable_defaults(
  font.size = 32,
  text.align = "center",
  valign = "center",
  padding = 8,
  layout = "autofit")

tab1a <- pos1 %>%
          arrange(measurement, dateonset) %>%
          group_by(measurement, place.admin.0) %>%
          summarise(N = n()) %>% 
          mutate(place.admin.0 = str_to_sentence(place.admin.0), 
                 place.admin.0 = dplyr::case_when(
                   place.admin.0 == "Occupied palestinian territory, including east jerusalem" ~ "Palestine", 
                   place.admin.0 == "South sudan" ~ "South Sudan", 
                   place.admin.0 == "Democratic republic of the congo" ~ "DRC", 
                   TRUE ~ place.admin.0), 
                 measurement = dplyr::case_when(
                   measurement == "WILD 1" ~ "WPV1", 
                   measurement == "cVDPV 1" ~ "cVDPV1", 
                   measurement == "cVDPV 2" ~ "cVDPV2", 
                   measurement == "cVDPV 3" ~ "cVDPV3")) %>% 
          arrange(measurement, desc(N))
                   
write_xlsx(tab1a, path = "24_counts.xlsx")               


