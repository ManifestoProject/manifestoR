mp_setapikey("../manifesto_apikey.txt")

require(magrittr)

test_that("Bischof nicheness works and produces correct results", {
  
  mpds <- mp_maindataset("MPDS2015a")
  
  mpds %>%
    subset(countryname == "France") %>%
    mutate(., nicheness_bischof = nicheness_bischof(.)) %>%
    mutate(., nicheness_mp_bischof = mp_nicheness(., method = "bischof")) %>%
    select(party, date, nicheness_bischof, nicheness_mp_bischof) -> niche_test_fr
  mpds %>%
    subset(countryname %in% c("Germany", "France")) %>%
    mutate(., nicheness_bischof = nicheness_bischof(.)) %>%
    mutate(., nicheness_mp_bischof = mp_nicheness(., method = "bischof")) %>%
    select(party, date, nicheness_bischof, nicheness_mp_bischof) -> niche_test_multi
  
  ## Consistency
  expect_false(anyNA(niche_test_fr$nicheness_bischof))
  expect_is(niche_test_fr$nicheness_bischof %>% unlist(), "numeric")
  expect_is(niche_test_fr$nicheness_mp_bischof %>% unlist(), "numeric")
  expect_equivalent(niche_test_fr$nicheness_bischof, niche_test_fr$nicheness_mp_bischof)
  ## Amount/Selection of data does not matter
  expect_equivalent(niche_test_multi %>% subset(substr(party, 1, 2) == 31), niche_test_fr)
  
  ## Order invariance
  left_join(
    mpds %>%
      subset(countryname == "France") %>%
      arrange(party, date) %>%
      mutate(., nicheness_pd = nicheness_bischof(.)),
    mpds %>%
      subset(countryname == "France") %>%
      arrange(date, party) %>%
      mutate(., nicheness_dp = nicheness_bischof(.))) %>%
    arrange(party, date) %$%
    expect_equivalent(nicheness_pd, nicheness_dp)
  
  read.csv("../data/niche_replication.csv") %>%
    right_join(niche_test_fr) %$%
    expect_equivalent(nicheness_replication, nicheness_bischof)
})
  