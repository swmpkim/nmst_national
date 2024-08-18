explanatory_scaled <- explanatory %>% 
    mutate(across(where(is.numeric), 
                  function(x) scale(x)[,1]),
           Geomorphology = case_when(Geomorphology == "Back Barrier" ~ "Back barrier",
                                     .default = Geomorphology))

test_perm <- adonis2(responses_scaled ~ NERR_Region + Salinity_category + 
                         Geomorphology + Tidal_Range + condition + SLR_last19yrs +
                         SET_minus_SLR_19yrs + proportion_low,
                     data = explanatory_scaled,
                     method = "euclidean",
                     na.action = "na.omit",
                     by = "margin"  # assess using variable-added-last. by = "terms" does testing sequentially.
                     )


test_perm <- adonis2(responses_scaled ~ NERR_Region,
                     data = explanatory,
                     method = "euclidean",
                     na.action = "na.omit",
                     by = "margin"  # assess using variable-added-last. by = "terms" does testing sequentially.
)

test_perm
summary(test_perm)

library(ecole)

test_pair_perms <- permanova_pairwise(responses_scaled, grp = explanatory$NERR_Region,
                   method = "euclidean",
                   padj = "fdr")

test_pair_perms

test_simper <- simper(responses_scaled, group = explanatory$NERR_Region)
summary(test_simper)
# remember the responses have been centered and scaled; need to do some math 
# if we want ava and avb to be actually meaningful

test_perm2 <- adonis2(responses_scaled ~ NERR_Region + Salinity_category + 
                         Geomorphology + Tidal_Range + condition + SLR_last19yrs +
                         SET_minus_SLR_19yrs + proportion_low,
                     data = explanatory_scaled,
                     method = "euclidean",
                     na.action = "na.omit",
                     by = "terms"  # assess using variable-added-last. by = "terms" does testing sequentially.
)
test_perm2
summary(test_perm2)


test_perm3 <- adonis2(responses_scaled ~ Salinity_category + 
                         Geomorphology + Tidal_Range + condition + SLR_last19yrs +
                         SET_minus_SLR_19yrs + NERR_Region + proportion_low,
                     data = explanatory_scaled,
                     method = "euclidean",
                     na.action = "na.omit",
                     by = "terms"  # assess using variable-added-last. by = "terms" does testing sequentially.
)
test_perm3
summary(test_perm3)



test_rda <- rda(responses_scaled ~ NERR_Region + Salinity_category + 
                         Geomorphology + Tidal_Range + condition + SLR_last19yrs +
                         SET_minus_SLR_19yrs + proportion_low,
                     data = explanatory_scaled,
                     na.action = "na.omit")
RsquareAdj(test_rda)
vif.cca(test_rda) 
anova.cca(test_rda)  # for model overall
anova.cca(test_rda, by = "terms")  # sequential terms
anova.cca(test_rda, by = "margin") # variable added last
anova.cca(test_rda, by = "onedf")  # performs contrasts, but sequentially


test_cca <- cca(responses_scaled ~ NERR_Region + Salinity_category + 
                    Geomorphology + Tidal_Range + condition + SLR_last19yrs +
                    SET_minus_SLR_19yrs + proportion_low,
                data = explanatory_scaled,
                na.action = "na.omit")
rowSums(responses_scaled)

# row sums have to be positive..... add a column that will make that so?
# smallest row sum seems to be MAR-HF1 at -5.3
# so make a column of sixes?
responses_scaled2 <- responses_scaled %>% 
    mutate(Sixes = 6)
test_cca <- cca(responses_scaled2 ~ NERR_Region + Salinity_category + 
                    Tidal_Range + Geomorphology +
                    condition + SLR_last19yrs +
                    SET_minus_SLR_19yrs + proportion_low +
                    temp_avg_slope + precip_sum_slope +
                    tide_range_slope + Latitude,
                data = explanatory_scaled,
                na.action = "na.omit")

expl_numeric <- explanatory %>% 
    select(where(is.numeric))

expl_numeric_scaled <- explanatory_scaled %>% 
    select(where(is.numeric))
rownames(expl_numeric_scaled) = rownames(explanatory_scaled)

test_bioenv <- bioenv(responses_scaled, expl_numeric_scaled, index = "euclidean")

# turn character cols into factors
expl2 <- explanatory %>% 
    mutate(across(where(is.character),
                  as.factor))
# Make appropriate columns ordered factors
expl2$Tidal_Range = factor(expl2$Tidal_Range, levels = c("Microtidal", "Mesotidal", "Macrotidal"),
                            ordered = TRUE)
expl2$Salinity_category = factor(expl2$Salinity_category,
                                 levels = c("Fresh", "Oligohaline",
                                            "Mesohaline", "Polyhaline"),
                                 ordered = TRUE)

expl3 <- expl2 %>% 
    select(Geomorphology, Tidal_Range, Salinity_category,
           SLR_last19yrs, NERR_Region, condition,
           SET_minus_SLR_19yrs, proportion_low)

rownames(expl3) <- rownames(responses_scaled)

test_gower <- cluster::daisy(expl3, metric = "gower")
test_bioenvall <- bioenv(responses, expl3, 
                         method = "spearman",
                         index = "euclidean",
                         metric = "gower")
test_gower_nmds <- metaMDS(test_gower)


data.scores <- as.data.frame(scores(test_gower_nmds))
ggplot(data.scores,
       aes(x = NMDS1,
           y = NMDS2)) +
    geom_text(label = rownames(responses_scaled),
              size = rel(3)) +
    labs(title = "NMDS of Environmental Factors",
         subtitle = "Gower's dissimilarity")


expl_numeric2 <- expl_numeric %>% 
    select(SLR_last19yrs, condition, SET_minus_SLR_19yrs, proportion_low)
env <- envfit(test_gower_nmds, expl_numeric2, na.rm = TRUE)
loadings <- as.data.frame(scores(env,"vectors"))


ggplot(data.scores,
       aes(x = NMDS1,
           y = NMDS2)) +
    geom_text(label = rownames(responses_scaled),
              size = rel(3),
              aes(col = expl3$NERR_Region)) +
    geom_segment(data = loadings,
                 aes(x = 0, y = 0,
                     xend = NMDS1, yend = NMDS2),
                 col = "red") +
    geom_text(data = loadings,
              label = rownames(loadings),
              col = "red") +
    labs(title = "NMDS of Environmental Factors",
         subtitle = "Gower's dissimilarity")
