#' Parse locations
#'
#' @param dat dataframe containing a column which includes location info to parse
#' @param loc_col the name of the column with location info to be parsed
#' @param region_type Approach to group nation states into larger global regions
#'
#' @return dataframe with columns representing parsed location and wider region
#' @export
#'
#' @examples
parse_locations <- function(dat = NA, loc_col = 'loc_code',
                            region_type = 'regions_WHO') {


  ## Some basic tidying of input to be returned
  x <- dat[[loc_col]]
  x <- toupper(x)
  x <- gsub('^GERMANY-[A-Z][A-Z]', 'GERMANY', x)
  x <- gsub('^LISBON$', 'PORTUGAL', x)
  x <- gsub('GOOSE_DENMARK', 'DENMARK', x)
  x <- gsub('22P019377', 'FRANCE', x)
  x <- gsub('^SVA[0-9A-Z]+$', 'SWEDEN', x)

  # if numbers are not present in x, str_extract will generate NA so keep these
  # and NA others
  x <- ifelse(is.na(str_extract(x, pattern = '[0-9]')),
              x, NA)

  ## Some states often have sequence names with state or within country region
  locs_canada <- c('BC', 'NL')
  locs_china <- c('ANHUI', 'BEIJING', 'CHIBA', 'CHONGQING', 'GUANGDONG', 'GUANGXI',
                  'GUIZHOU', 'HANGZHOU', 'HEBEI', 'HENAN', 'HUBEI', 'HUNAN',
                  'INNER_MONGOLIA', 'JIANGSU', 'JIANGXI', 'LIAONING', 'NINGXIA',
                  'NORTHERN_CHINA', 'SANMENXIA', 'SHAANXI', 'SHANDONG', 'SHANGHAI',
                  'SHANXI', 'SICHUAN', 'SOUTHWESTERN_CHINA', 'SUZHOU', 'TIBET',
                  'TUMEN', 'XINJIANG', 'YUNNAN', 'ZHEJIANG')
  locs_indonesia <- c('EAST_JAVA')
  locs_japan <- c('FUKUOKA', 'FUKUSHIMA', 'GIFU', 'HIROSHIMA', 'HOKKAIDO', 'HYOGO',
                  'IBARAKI', 'KAGAWA', 'KAGOSHIMA', 'KOCHI', 'MIYAGI', 'MIYAZAKI',
                  'NARA', 'NIIGATA', 'OITA', 'OKAYAMA', 'SHIGA', 'TOCHIGI', 'TOKUSHIMA',
                  'TOYAMA', 'WAKAYAMA')
  locs_mexico <- c('AGUASCALIENTES', 'AGUASCALLIENTES', 'GUANAJUATO', 'HIDALGO',
                   'JALISCO', 'PUEBLA', 'QUERETARO', 'SAN_LUIS_POTOSI')
  locs_mongolia <- c('KHUNT_LAKE', 'KHUVSGUL')
  locs_russia <- c('AMUR_REGION', 'ASTRAKHAN', 'CHANY_LAKE', 'CHELYABINSK', 'KOSTROMA',
                   'KRASNODAR', 'KURGAN', 'NORTH_OSSETIA-ALANIA', 'NOVOSIBIRSK_REGION',
                   'OMSK', 'OMSK_REGION', 'ROSTOV-ON-DON', 'RUSSIA_NOVOSIBIRSK_REGION',
                   'RUSSIA_OMSK_REGION', 'RUSSIA_PRIMORJE', 'RUSSIAN_FEDERATION', 'SARATOV',
                   'STAVROPOL', 'TATARSTAN', 'TYUMEN')
  locs_usa <- c('ALASKA', 'COLORADO', 'CONNECTICUT', 'DELAWARE', 'DELAWARE_BAY',
                'FLORIDA', 'KANSAS', 'IDAHO', 'ILLINOIS', 'INDIANA', 'IOWA', 'KENTUCKY',
                'MAINE', 'MARYLAND', 'MASSACHUSETTS', 'MICHIGAN', 'MINNESOTA', 'MISSOURI',
                'MONTANA', 'NEBRASKA', 'NEW_HAMPSHIRE', 'NEW_YORK', 'NORTH_CAROLINA',
                'NORTH_DAKOTA', 'OHIO', 'PENNSYLVANIA', 'SOUTH_CAROLINA', 'SOUTH_DAKOTA',
                'TEXAS', 'VIRGINIA', 'WISCONSIN', 'WYOMING')
  locs_vietnam <- c('BA_RIA-VUNG_TAU', 'DONG_NAI', 'HA_TINH', 'LONG_AN', 'NGHE_AN',
                    'QUANG_TRI', 'THANH_HOA', 'VIET_NAM')
  locs_uk <- c('ENGLAND', 'ISLE_OF_MAN', 'JERSEY', 'NORTHERN_IRELAND', 'SCOTLAND', 'WALES')

  # resolve state at international level
  x_state <- x
  x_state <- ifelse(x_state %in% locs_canada, 'CANADA', x_state)
  x_state <- ifelse(x_state %in% locs_china, 'CHINA', x_state)
  x_state <- ifelse(x_state %in% locs_indonesia, 'INDONESIA', x_state)
  x_state <- ifelse(x_state %in% locs_japan, 'JAPAN', x_state)
  x_state <- ifelse(x_state %in% locs_mexico, 'MEXICO', x_state)
  x_state <- ifelse(x_state %in% locs_mongolia, 'MONGOLIA', x_state)
  x_state <- ifelse(x_state %in% locs_russia, 'RUSSIA', x_state)
  x_state <- ifelse(x_state %in% locs_usa, 'USA', x_state)
  x_state <- ifelse(x_state %in% locs_vietnam, 'VIETNAM', x_state)
  x_state <- ifelse(x_state %in% locs_uk, 'UK', x_state)

  # WHO regions
  locs_africa <- c('BURKINA_FASO', 'LESOTHO', 'MALI', 'NIGER', 'NIGERIA', 'SENEGAL')
  locs_america <- c('CANADA', 'DOMINICAN_REPUBLIC', 'MEXICO', 'USA')
  locs_europe <- c('ALBANIA', 'AUSTRIA', 'BELGIUM', 'BULGARIA', 'CROATIA', 'CZECH_REPUBLIC',
                   'DENMARK', 'ESTONIA', 'FINLAND', 'FRANCE',
                   'GEORGIA', 'GERMANY', 'GREECE', 'HUNGARY', 'IRELAND', 'ISRAEL', 'ITALY',
                   'KAZAKHSTAN', 'KOSOVO', 'LATVIA', 'LITHUANIA', 'LUXEMBOURG', 'MOLDOVA',
                   'NETHERLANDS', 'NORWAY', 'POLAND', 'PORTUGAL', 'ROMANIA', 'RUSSIA',
                   'SLOVAKIA', 'SLOVENIA', 'SPAIN', 'SWEDEN', 'SWITZERLAND')
  locs_middleeast <- c('EGPYT', 'IRAN', 'IRAQ')
  locs_se_asia <- c('INDONESIA')
  locs_west_pacific <- c('BANGLADESH', 'CAMBODIA', 'CHINA', 'JAPAN', 'KOREA', 'LAOS',
                         'MONGOLIA', 'SOUTH_KOREA', 'TAIWAN', 'VIETNAM')
  locs_eastAsia <- c(locs_se_asia, locs_west_pacific)

  if (region_type == 'regions_WHO') {
    x_region <- rep(NA, length(x_state))
    x_region <- ifelse(x_state %in% locs_africa, 'AFRICA', x_region)
    x_region <- ifelse(x_state %in% locs_america, 'AMERICAS', x_region)
    x_region <- ifelse(x_state %in% locs_europe, 'EUROPE', x_region)
    x_region <- ifelse(x_state %in% locs_middleeast, 'MIDDLEEAST', x_region)
    x_region <- ifelse(x_state %in% locs_se_asia, 'SE_ASIA', x_region)
    x_region <- ifelse(x_state %in% locs_west_pacific, 'WEST_PACIFIC', x_region)
  }

  dat[[loc_col]] <- x
  dat$location <- x_state
  dat$location.region <- x_region

  dat
}
