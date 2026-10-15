Pre-registered analysis plan: https://osf.io/9v84f
Pre-registered design and procedure: https://osf.io/7283r

Pre-registered AOI size is using 400 x 400 px.

In exploratory analysis, AOI size is 350 x 350 px.

```
.
├── data
│   ├── expl.parquet
│   ├── prereg.parquet
│   └── prereg_latency.parquet
├── goal_attribution_repo.Rproj
├── output
├── readme.md
└── scripts
    ├── 1_fit_models.R
    ├── 2_knit.Rmd
    └── functions
        ├── boot_glmmTMB_edit.r
        ├── diagnostic_fcns.r
        ├── functions_analysis.R
        └── glmmTMB_stability_edit.r
    
```
## data
- **prereg.parquet**: 
- **prereg_latency.parquet**: 
- **expl.parquet**:
## output
- **model_object.rds**:

## scripts
- **1_fit_models.Rmd**: file with time consuming processes, creates model_object.rds
- **2_knit.Rmd**: creates word document

Variables in data files:
- **`recname`**: Identifier for each recording/trial.
- **`recording_date`**: Date when the recording took place.
- **`participant_name`**: Name of the participant.
- **`species`**: Species of the participant.
- **`presented_stimulus_name`**: Name of video.
- **`condition`**: Experimental condition (hand, agent, mech) (factor with ref mech).
- **`claw_type`**: Type of claw, if agent or mech condition (1 or 2).
- **`trial_within`**: Trial number within condition (1-4).
- **`trial_consecutive`**: Consecutive trial number across the study (1-12).
- **`cond_order`**: Order of conditions presented to the participant (1-3).
- **`handseen`**: Whether the hand was seen by the participant (binary).
- **`agentseen`**: Whether the agent was seen by the participant (binary).
- **`action`**: Action phase within each video (test, reach1, grasp1, etc.).
- **`action_dur`**: Duration of the test scene (ms), slightly different between conditions.
- **`mean_dur`**: Mean of duration of test scene across all videos.
- **`sd_dur`**: SD of duration of test scene across all videos.
- **`dist`**: Total fixation duration on the distractor object.
- **`tgt`**: Total fixation duration on the target object.
- **`tgt_sc`**: Total fixation duration on the target object standardised to mean of scene duration.
- **`dist_sc`**: Total fixation duration on the distractor object standardised to mean of scene duration.
- **`pls`**: Dependent variable measuring preferential looking score.
- **`either_time`**: Total looking time at target or distractor object.
- **`pls_transform`**: Transformed version of the preferential looking score for modeling.
- **`target_firsthit`**:
- **`latency_filtered`**:





