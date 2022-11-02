rm(list = ls())
library(tidyverse)
library(janitor)

# Data fra Udirs statistikkbank: https://www.udir.no/tall-og-forskning/statistikk/statistikk-videregaende-skole/karakterer-vgs/?rapportsideKode=VGO_VGOkarakterer&filtre=EierformID(-10_2_8)_EnhetID(-12)_FagID(-12_127_157_224_315_498_553_690_815_890_976_996_1007_1064_1128_1144_1151_1168_1184_1232_1373_1484_1571_1621_1630_1714_1733_1964_2003_2151_2200_2221_2265_2330_2385_2462_2463_2521_2561_2619_2726_2735_2790_2828_2878_2900_2963_3126_3175_3227_3253_3283_3294_3354_3531_3552_3790_6457_27776_29033_29034_29035_29036_29767_29768_29939_29940_29941_30076_30117_30118_30119_30120_30121_30122_30123_30124_30125_30126_30127_30128_30129_30130_30131_30133_30135_30136_30137_30138_30139_30151_30152_30153_30154_30155_30156_30157_30158_30159_30160_30161_30182_30183_30979_30980_30982_30986_30988_30996_31000_35598_35599)_KaraktertypeID(1)_KjoennID(-10)_TidID(200806_200906_201006_201106_201206_201306_201406_201506_201606_201706_201806_201906_202006_202106_202206)_UtdanningsprogramvariantID(-10)_VisAntallPersoner(1)_VisKarakterfordeling(1)&radsti=F!(315)_(*)_(315.*)
dat.raw <- read_tsv("~/Downloads/20220916-1444_Karakterer_i_videregaaende_skole.csv", locale = locale(encoding = "UTF-16")) %>% janitor::clean_names()

parse_numeric <- function(nums) {
  nums %>%
    str_replace("," , ".") %>%
    str_replace(" ", "") %>%
    as.numeric()
}

data_cols <- "^(x|forelopige).+(snittkarakter|antall_elever|karakteren_(\\d+))"

dat <- dat.raw %>%
  mutate(
    across(matches(data_cols), ~parse_numeric(.x)),
    fag_kode = str_extract(vurderingsfagkode, "^.{3}"),
    fag_gruppe = case_when(
      fag_kode %in% c("SAF", "SAK") ~ "SAF/SAK",
      T ~ fag_kode
    )
  )

dat.tidy <- dat %>%
  pivot_longer(matches(data_cols)) %>%
  extract(
    name,
    into = c('period', 'type', 'eieform', 'kjonn', 'variable'),
    regex = "^(?:x|forelopige_tall_)([\\d_]+)_(standpunkt)_(alle_eierformer|offentlig_skole|privat_eiet)_(begge_kjonn)_((?:snittkarakter|karakteren_\\d|antall_elever)(?:_brudd)?)",
  )

dat.tidy.wide <- dat.tidy %>% pivot_wider(names_from=variable, values_from=value)

#### Analyse 1: Andel seksere, privat vs offentlig

dat.tidy.wide %>%
  filter(enhet_nivaa == 0) %>%
  group_by(period, eieform) %>%
  summarise(
    k6.n = sum((karakteren_6/100)*antall_elever, na.rm = T),
    N = sum(antall_elever, na.rm = T),
    k6.prop = (k6.n/N)*100,
    .groups = 'drop'
  )

# Analyse 2: Snittkarakter

dat.tidy.wide %>%
  filter(enhet_nivaa == 3) %>%
  group_by(period) %>%
  summarise(
    snitt = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    .groups = 'drop'
  )

# ... etter eieform
dat.tidy.wide %>%
  filter(enhet_nivaa == 3) %>%
  group_by(period, eieform) %>%
  summarise(
    snitt = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    .groups = 'drop'
  )

# ... per skole

dat.tidy.wide %>%
  filter(enhet_nivaa == 3) %>%
  group_by(organisasjonsnummer, enhet_navn, period) %>%
  summarise(
    snittkarakter = weighted.mean(snittkarakter, antall_elever, na.rm = T),
    antall_elever = sum(antall_elever, na.rm = T),
    .groups = "drop_last"
  ) %>%
  arrange(period) %>%
  mutate(
    abs_change = snittkarakter - lag(snittkarakter),
    rel_change = snittkarakter / lag(snittkarakter)
  ) %>%
  ungroup() %>%
  filter(period %in% c('2018_19', '2019_20')) %>%
  pivot_wider(c(organisasjonsnummer, enhet_navn), names_from=period, values_from=c(abs_change, rel_change, antall_elever, snittkarakter)) %>%
  filter(!is.na(rel_change_2019_20)) %>%
  arrange(desc(rel_change_2019_20)) %>%
  transmute(
    Skole = enhet_navn,
    `2018/19` = round(snittkarakter_2018_19, 2),
    `2019/20` = round(snittkarakter_2019_20, 2),
    'Endring (%)' = round((rel_change_2019_20 - 1) * 100, 1),
    'Endring' = round(abs_change_2019_20, 2)
  )
