# ---------------------------------------------
#           Countries by Regional Groups
#               Belicia Rodriguez
# ---------------------------------------------

# -----------------------------------------------------------------------------
# Note: These regional groups are based on the Sustainable Development Goals
# (SDG) regional groupings. There are a total of 240 countries grouped into
# 7 regional groups, and they are stored in vectors. Additionally, the
# "add_UN_regions" function adds these regional groupings to a dataframe, and
# it is designed for the "unhcr_popstats.csv".
#
# The SDG regional groupings can be found on this UN website:
# https://unstats.un.org/sdgs/indicators/regional-groups
# -----------------------------------------------------------------------------

# vectors of countries and their regions (total: 240)
sub_saharan_africa <- c(
  "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde",
  "Cameroon", "Central African Rep.", "Chad", "Comoros", "Congo",
  "Côte d'Ivoire", "Dem. Rep. of the Congo", "Djibouti", "Equatorial Guinea",
  "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
  "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania",
  "Mauritius", "Mayotte", "Mozambique", "Namibia", "Niger", "Nigeria",
  "Réunion", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles",
  "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Swaziland", "Togo",
  "Uganda", "United Rep. of Tanzania", "Zambia", "Zimbabwe"
) #50

northern_africa_western_asia <- c(
  "Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "Western Sahara",
  "Azerbaijan", "Armenia", "Bahrain", "Cyprus", "Georgia", "Iraq", "Israel",
  "Jordan", "Kuwait", "Lebanon", "State of Palestine", "Oman", "Qatar",
  "Saudi Arabia", "Syrian Arab Rep.", "Turkey", "United Arab Emirates", "Yemen",
  "Saint Helena", "West Bank and Gaza Strip"
) #27

central_southern_asia <- c(
  "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan",
  "Afghanistan", "Bangladesh", "Bhutan", "India", "Iran (Islamic Rep. of)",
  "Maldives", "Nepal", "Pakistan", "Sri Lanka"
) #14

eastern_southeastern_asia <- c(
  "China", "China, Hong Kong SAR", "China, Macao SAR", "Rep. of Korea", "Japan",
  "Mongolia", "Republic of Korea", "Brunei Darussalam", "Cambodia", "Indonesia",
  "Lao People's Dem. Rep.", "Malaysia", "Myanmar", "Philippines", "Singapore",
  "Thailand", "Timor-Leste", "Viet Nam"
) #18

latin_america_caribbean <- c(
  "Anguilla", "Antigua and Barbuda", "Aruba", "Bahamas", "Barbados",
  "Bonaire, Sint Eustatius and Saba", "Bonaire", "British Virgin Islands",
  "Cayman Islands", "Cuba", "Curaçao", "Dominica", "Dominican Rep.", "Grenada",
  "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat", "Puerto Rico",
  "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
  "Sint Maarten (Dutch part)", "Suriname", "Trinidad and Tobago",
  "Turks and Caicos Islands", "United States Virgin Islands", "Honduras",
  "Costa Rica", "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Panama",
  "Argentina", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Chile",
  "Colombia", "Ecuador", "French Guiana", "Falkland Islands (Malvinas)",
  "South Georgia & the South Sandwich Islands", "Guyana", "Paraguay", "Peru",
  "Uruguay", "Venezuela (Bolivarian Republic of)"
) #49

oceania <- c(
  "Australia","Christmas Island", "Cocos (Keeling) Islands",
  "Heard Island & McDonald Islands", "Norfolk Island", "New Zealand", "Fiji",
  "New Caledonia", "Papua New Guinea", "Solomon Islands", "Vanuatu", "Kiribati",
  "Marshall Islands", "Micronesia (Federated States of)", "Nauru",
  "Northern Mariana Islands", "Palau", "Guam", "French Polynesia",
  "Wallis and Futuna Island", "Pitcairn", "Cook Islands", "Niue", "Tokelau",
  "Tonga", "Tuvalu", "American Samoa", "Samoa", "Wallis and Futuna Islands"
) #29

europe_northern_america <- c(
  "Bermuda", "Canada", "Greenland", "United States of America", "Bulgaria",
  "Belarus", "Czech Rep.", "Hungary", "Rep. of Moldova", "Poland", "Romania",
  "Russian Federation", "Slovakia", "Ukraine", "Åland Islands",
  "Channel Islands", "Denmark", "Estonia","Faroe Islands", "Finland",
  "Isle of Man", "United Kingdom", "Iceland", "Ireland", "Latvia", "Lithuania",
  "Norway", "Sweden", "Albania", "Andorra", "Bosnia and Herzegovina", "Croatia",
  "Greece", "Italy", "Malta",  "Montenegro", "Portugal", "San Marino", "Serbia",
  "Serbia and Kosovo", "Slovenia", "Spain", "Gibraltar" ,
  "The former Yugoslav Republic of Macedonia", "Austria", "Belgium",
  "Switzerland", "Germany", "France", "Liechtenstein", "Luxembourg", "Monaco",
  "Netherlands", "Saint Pierre and Miquelon"
) #53

# Define add_UN_regions
add_UN_regions <- function(df) {
  # creating list with regions
  regions <- list(
    sub_saharan_africa = sub_saharan_africa,
    northern_africa_western_asia = northern_africa_western_asia,
    central_southern_asia = central_southern_asia,
    eastern_southeastern_asia = eastern_southeastern_asia,
    latin_america_caribbean = latin_america_caribbean,
    oceania = oceania,
    europe_northern_america = europe_northern_america
    )

  # add region variables to data-frame
  df$region_asylum_residence <- rep(NA_character_,nrow(df))
  df$region_origin <- rep(NA_character_, nrow(df))

  # loop through each country to match region
  region_id_number = 1
  for (r in regions) {
    for (c in r) {
      # store row region matches
      row_matches_asylum <- as.numeric(
        c == df[["country_territory_of_asylum_residence"]]
      )
      row_matches_origin <- as.numeric(
        c == df[["origin"]]
      )
      # match region name to row
      for (n in 1:nrow(df)) {
        if (!is.na(row_matches_asylum[n]) & row_matches_asylum[n] == 1)
          df$region_asylum_residence[n] <- names(regions)[region_id_number]
        if(!is.na(row_matches_origin[n]) & row_matches_origin[n] == 1)
          df$region_origin[n] <- names(regions)[region_id_number]
      }
    }
  # add to region id number for naming
  region_id_number = region_id_number + 1
  }

  # make region variables categorical variables
  df <- df %>% mutate_at(
    c("region_asylum_residence", "region_origin"),
    as.factor
  )

  # return new data-frame
  return(df)
}

rename_region <- c(
  central_southern_asia = "Central & Southern Asia",
  eastern_southeastern_asia = "Eastern & Southeastern Asia",
  europe_northern_america = "Europe & Northern America",
  latin_america_caribbean = "Latin America & Caribbean",
  northern_africa_western_asia = "Northern Africa & Western Asia",
  oceania = "Oceania",
  sub_saharan_africa = "Sub Saharan Africa"
)
