mp_setapikey("../manifesto_apikey.txt")

test_that("issue attention diversity works", {
  
  methods <- list(shannon = "enmi_sh",
                  herfindahl = "enmi_herf")
  
  test_data <- mp_maindataset("MPDS2015a") %>%
    subset(country %in% c(64, 87))
  
  for (method in names(methods)) {

    expect_equivalent(
      test_data %>%
        subset(country == 64) %>%
        issue_attention_diversity(method = method),
      test_data %>%
        issue_attention_diversity(method = method) %>%
        subset(test_data$country == 64)
    )
    
    expect_false(any(abs(
      test_data %>%
        issue_attention_diversity(method = method) -
        read.csv("../data/iad_replication.csv")[methods[[method]]]) > 0.01))
    
  }
  
  

  
})