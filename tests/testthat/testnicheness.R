mp_setapikey("../manifesto_apikey.txt")
library(magrittr)

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
  
  read.csv("../data/niche_bischof_replication.csv") %>%
    rename(replication_spec = specialization,
           replication_niche = nicheness,
           replication_niche_two = nicheness_two) %>%
    right_join(niche_test_fr) %>%
    transmute(spec_err = replication_spec - specialization,
              niche_err = replication_niche - nicheness,
              niche_two_err = replication_niche_two - nicheness_two) %>%
    #mutate_each(funs(abs(.) < 0.001), vars = one_of("spec_err", "niche_err")) %>%
    #the line above mutates two columns that are dropped in the next line
    transmute(niche_two_err = abs(niche_two_err) < 0.5) %>%  ## some error is allows
                                                          ## since we are not using
                                                          ## Bischof's original bounds
    unlist() %>%
    all() %>%
    expect_true()
})

test_that("Meyer Miller nicheness", {
  
  ## tests with theoretical values
  
  ## weighted
  fake_data <- data.frame(
    party   = c(11,  12,  13),
    country = c(1,   1,   1),
    date    = c(1,   1,   1),
    pervote = c(1,   1,   1),
    issue1  = c(1,   0L,  0L),
    issue2  = c(0.3, 0.3, 0.4),
    issue3  = c(0.6, 0.2, 0.2)
  )
  fake_data_agg <- data.frame(
    party   = c(11,  12,  13),
    country = c(1,   1,   1),
    date    = c(1,   1,   1),
    pervote = c(1,   1,   1),
    issue1  = c(1,   0L,  0L),
    issue2  = c(0.9, 0.5, 0.6),
    issue3  = c(0.0, 0.0, 0.0)
  )
  fake_data_2 <- data.frame(
    party   = c(11,  12,  13),
    country = c(1,   1,   1),
    date    = c(2,   2,   2),
    pervote = c(1,   1,   1),
    issue1  = c(0L,  1,   0L),  ## swapped issue values of parties 1 and 2
    issue2  = c(0.3, 0.3, 0.4),
    issue3  = c(0.2, 0.6, 0.2)
  )
  means  <- data.frame(
    issue1 = c(0L,   0.5,  0.5),
    issue2 = c(0.35, 0.35, 0.3),
    issue3 = c(0.2,  0.4,  0.4)
  )
  sigma_p <- fake_data %>%
    select(starts_with("issue")) %>%
    { (. - means)^2 } %>%
    rowSums() %>%
    { . /3 } %>%
    sqrt()
  
  fake_data %>%
    meyer_miller_single_election(vars = c("issue1", "issue2", "issue3"),
                                 weights = 1,
                                 party_system_normalization = FALSE) %>%
    expect_equivalent(sigma_p)
  
  expect_equivalent(
    fake_data %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3"), weights = "pervote"),
    fake_data %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3"), weights = 1)
  )
  
  fake_data %>%
    nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3")) %>%
    select(nicheness) %>%
    unlist() %>%
    expect_equivalent(sigma_p - rival_mean(sigma_p))
  
  suppressWarnings(left_join(
    fake_data %>% 
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = c("issue2", "issue3"))),
    fake_data_agg %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3")),
    by = c("party", "date")) %>%
    mutate(diff = abs(nicheness.x - nicheness.y)) %$%
    expect_lt(max(diff), 0.0001)
  )
  
  expect_equivalent(
    fake_data %>% 
      mutate_at(vars(starts_with("issue")), .funs = funs({log(. + 1)})) %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3")),
    fake_data %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3"),
                             transform = "bischof")
  )
  
  expect_equivalent(
    fake_data %>% 
      mutate_at(vars(starts_with("issue")), .funs = funs({exp(- .^2)})) %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3")),
    fake_data %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3"),
                             transform = function(x) { exp(-x^2)} )
  )
  
  ## consistency tests with multiple elections and regarding order
  expect_equivalent(
    bind_rows(fake_data, mutate(fake_data, date = 2)) %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3")) %>%
      group_by(country, party) %>%
      summarise(nicheness = unique(nicheness)) %>%
      ungroup() %>%
      mutate(date = 1) %>%
      select(country, party, date, nicheness),
    fake_data %>%
      nicheness_meyer_miller(groups = list(issue1 = "issue1", issue2 = "issue2", issue3 = "issue3"))
  )
  
  mpds <- mp_maindataset("MPDS2015a")
  
  expect_equivalent(
    mpds %>%
      subset(country == 53) %>%
      nicheness_meyer_miller(),
    mpds %>%
      subset(country == 53) %>%
      mp_nicheness(method = "meyermiller")
  )
  
  ## There is no replication test for the Meyer & Miller nicheness, since we
  ## made substantial changes to the algorithm (compared to the STATA script)
  ## to make it agree with the algorithm described in the paper.

  #   mpds %>%
  #     subset(country == 53) %>%
  #     nicheness_meyer_miller() %>%
  #     left_join(read.csv("../data/niche_mm_replication.csv")) %>%
  #     mutate(deviation = abs(nicheness - nnbdd_dw)) %$%
  #     expect_true(all(deviation < 0.2))

})
  