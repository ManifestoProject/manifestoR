mp_setapikey("../manifesto_apikey.txt")

require(magrittr)

test_that("Bischof nicheness works and produces correct results", {
  
  mpds <- mp_maindataset("MPDS2015a")
  
  vars <- c("nicheness", "specialization", "nicheness_two")
  
  mpds %>%
    subset(countryname == "France") %>%
    nicheness_bischof(out_variables = c(vars, "party", "date")) -> niche_test_fr
  mpds %>%
    subset(countryname %in% c("Germany", "France")) %>%
    nicheness_bischof(out_variables = c(vars, "party", "date")) -> niche_test_multi
  
  ## Consistency
  for (n in c("nicheness", "specialization", "nicheness_two")) {
    expect_false(anyNA(niche_test_fr[[n]]))
    expect_false(anyNA(niche_test_multi[[n]]))
    expect_is(niche_test_fr[[n]] %>% unlist(), "numeric")
    expect_is(niche_test_multi[[n]] %>% unlist(), "numeric")
  }

  ## Amount/Selection of data does not matter
  expect_equivalent(niche_test_multi %>%
                      subset(substr(party, 1, 2) == 31),
                    niche_test_fr)
  
  ## Order invariance
  left_join(
    mpds %>%
      subset(countryname == "France") %>%
      arrange(party, date) %>%
      nicheness_bischof(., out_variables = c("party", "date", "nicheness_two")),
    mpds %>%
      subset(countryname == "France") %>%
      arrange(date, party) %>%
      nicheness_bischof(., out_variables = c("party", "date", "nicheness_two")),
    by = c("party", "date")) %>%
    arrange(party, date) %$%
    expect_equivalent(nicheness_two.x, nicheness_two.y)
    
  read.csv("../data/niche_replication.csv") %>%
    rename(replication_spec = specialization,
           replication_niche = nicheness,
           replication_niche_two = nicheness_two) %>%
    right_join(niche_test_fr) %>%
    transmute(spec_err = replication_spec - specialization,
              niche_err = replication_niche - nicheness,
              niche_two_err = replication_niche_two - nicheness_two) %>%
    mutate_each(funs(abs(.) < 0.001), vars = one_of("spec_err", "niche_err")) %>%
    mutate(niche_two_err = abs(niche_two_err) < 0.5) %>%  ## some error is allows
                                                          ## since we are not using
                                                          ## Bischof's original bounds
    unlist() %>%
    all() %>%
    expect_true()
})

test_that("Meyer Miller nicheness", {
  
  ## TODO tests with theoretical values; for working of different parameters
  
  ## TODO consistency tests
  
  ## TODO replication test
  
  mpds %>%
    subset(country == 41 & date == 200909) %>%
    aggregate_pers(groups = baeck_policy_dimensions(), keep = TRUE) %>%
    meyer_miller_single_election(vars = names(baeck_policy_dimensions()), weights = "pervote")
  
  mpds %>%
    subset(country == 41 & date == 200909) %>%
    nicheness_meyer_miller(groups = baeck_policy_dimensions())
  
})
  