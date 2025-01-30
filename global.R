
library(shiny)
library(tidyverse)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(DentalLibrary)

options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

googledrive::drive_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")
googlesheets4::gs4_auth(cache = ".secrets", email = "innovativeopticsdatabase@gmail.com")

sheet_id <- googledrive::drive_get("Dental_data")$id


# addResourcePath("www", "www")
# list loupe image paths to filter from
loupe_image_paths <- tibble("LoupeImages" = list.files(path = "www/LoupeImages/"))

# Load QOptics loupe data
#qoptics_data <- readxl::read_excel("data/Dental_data.xlsx",
#                                   sheet = "Loupe_types") %>%
#  filter(`Mfg` == "Q-Optics") %>%
#  rename(`Q Optics Loupe` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number`)

qoptics_data <- googlesheets4::read_sheet(sheet_id, sheet = "Loupe_types", col_types = "c")  %>%
  filter(`Mfg` == "Q-Optics") %>%
  rename(`Q Optics Loupe` = Mod, Style = Size, `Innovative Optics Insert` = `Insert Part Number`)


# Load dental data for lens details?? not sure... dig deep into this
#lens_data <- readxl::read_excel("data/Dental_data.xlsx", sheet = "Lens_details") %>%
#  select(-VLT)

lens_data <- googlesheets4::read_sheet(sheet_id, sheet = "Lens_details") %>%
  select(-VLT)

#dental_data <- readxl::read_excel("data/Dental_data.xlsx") %>%
#  filter(`Laser Mfg` != "") %>%
#  select(-Website) %>%
#  mutate(VLT = scales::percent(as.numeric(VLT))) %>%
#  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))

dental_data <- googlesheets4::read_sheet(sheet_id, sheet = "laser_info") %>%
  filter(`Laser Mfg` != "") %>%
  select(-Website) %>%
  mutate(VLT = scales::percent(as.numeric(VLT))) %>%
  left_join(lens_data, by = join_by(`Eyewear Lens Compatible` == Lens))
#

