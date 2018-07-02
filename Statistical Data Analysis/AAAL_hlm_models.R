############ Pre-only Models #####################
pre.model.1 <- lmer(PRE_RAPID_RC_AB ~ (1|CLASS_ID), data = UDIO_DATASET_C)   ## Unconditional Model
pre.model.2 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.3 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with SK
pre.model.4 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.5 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.6 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with WR
pre.model.7 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.8 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.9 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with VK
pre.model.10 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.11 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
pre.model.12 <- lmer(PRE_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model


############ Post-only Models #####################
post.model.1 <- lmer(POST_RAPID_RC_AB ~ (1|CLASS_ID), data = UDIO_DATASET_C)   ## Unconditional Model
post.model.2 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.3 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with SK
post.model.4 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_SK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.5 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_SK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.6 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_SK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with WR
post.model.7 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_WR_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.8 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_WR_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.9 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_WR_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with VK
post.model.10 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_VK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.11 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_VK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.12 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_VK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

post.model.13 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_VK_AB_C:FARMS + POST_RAPID_VK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
post.model.14 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + POST_RAPID_WR_AB_C + POST_RAPID_VK_AB_C + POST_RAPID_SK_AB_C + POST_RAPID_VK_AB_C:FARMS + POST_RAPID_VK_AB_C:ELL + POST_RAPID_VK_AB_C:FARMS:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
## No three-way interaction effects

############ Post-on-Pre Models #####################
#cross.model.1 <- lmer(POST_RAPID_RC_AB ~ (1|CLASS_ID), data = UDIO_DATASET_C)   ## Unconditional Model
#cross.model.2 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.3 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with SK
cross.model.4 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.5 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.6 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_SK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with WR
cross.model.7 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.8 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.9 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_WR_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

### Interaction Models with VK
cross.model.10 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:IEP + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.11 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:ELL + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model
cross.model.12 <- lmer(POST_RAPID_RC_AB ~ IEP + ELL + FARMS + PRE_RAPID_WR_AB_C + PRE_RAPID_VK_AB_C + PRE_RAPID_SK_AB_C + PRE_RAPID_VK_AB_C:FARMS + (1|CLASS_ID), data = UDIO_DATASET_C) ## SES-only Model

