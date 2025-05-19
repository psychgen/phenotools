# *Changelog for phenotools package*

# phenotools 0.4.3 (XXXXXX)

* (pending) Adds functionality to allow user-specified MoBa questionnaire files to be use in dataset curation (i.e., files other than the main MoBa questionnaires)
* (pending) Resolves an issue where preg_ids were duplicated in datasets curated with return_items=T for some variables (Github issue #3)
* (pending) Resolves an issue where, when datasets are curated with KUHR and NPR data simultaneously, the diagnosis lists get mixed up (Github issue #9)
* (pending) Adds functionality for extracting information from SSB data
*  (pending) Changes dependence on MBRN file to avoid missing BARN_NR for those without linkage and enhance cross-project robustness, since MoBa no longer provides MBRN file 
  -- NB, users will still have to provide a file with PREG_IDs and birth year for all
  -- individuals, as curate_npr and curate_kuhr need the child's birth year to calculate the age_at variables
  
# phenotools 0.4.2 (14.05.25)

* Added maternal reports on child depression (SMFQ) and anxiety (SCARED) from the 14 year questionnaire
* Added query_scale_items function to allow users to extract relevant info about how scales are made from the internal metadata easily


# phenotools 0.4.1 (13.03.25)

* Fixes a bug causing the project_specific option in available_variables() to misbehave
* Fixes the small but potentially consequential mislabelling of the "dx_owner" option in curate_npr
and curate_kuhr and associated documentation (as "dx_owners", which would have been ignored) - 
NB, this fix requires the "dx_owner" option being renamed to "dx_recipient" in this version to 
avoid confusion with an internal column name
* Fixes a bug in which the wrong item was being reversed in the CCC2 5yr scale due to use of outdated MoBa documentation
* Fixes a bug in which those without linkage to MBRN having a missing BARN_NR (now set BARN_NR=1 as there are no 
multiple births in this group; Github issue #11))
* Resolves an issue where dplyr::na_if() generates an error when using newer versions of dplyr (Github issue #12)



# phenotools 0.4.0.9999 (11.06.24)

* moba_varnames (internal data) updated to allow all newly available moba variables to be retrieved


# phenotools 0.4.0 (04.06.24)

* available_variables() now returns a complete list, including raw MoBa items that can be retrieved (but are not processed)
* available_variables() now includes the project_specific option, to allow output to be restricted to those variables actually available in a given project, rather than just theoretically available from phenotools. The help pages of available_variables are updated to make clear that this is, in fact, the default behaviour of this function
* linkage_file_root_dir options in curate_npr and curate_kuhr updated to be source-specific (i.e., npr_linkage_file_root_dir); this is necessary to allow sources to be accessed in the same call 
* NPR filename and linkage filename defaults updated to reflect latest data in p471
* New variables added:

    * scl_full_m_14m
    * scl_anx_m_14m
    * scl_dep_m_14m
    * ksq_short_m_14m
    * ksq_short_f_far2
    * ksq_short_c_14c
    * swls_full_m_14m

    See `available_variables()` for details.

# phenotools 0.3.3.9999 (21.05.24)

* Fixed a bug where certain NPR codes could not be retrieved by curate_npr due to an incomplete lists in the package's internal metadata

# phenotools 0.3.3 (17.01.24)

* Added new options to curate_dataset to facilitate checking whether or not individuals have genotype data available
* Added new options to curate_kuhr and curate_npr to allow a specified date range to be used when retrieving diagnoses

# phenotools 0.3.2 (12.01.24)

* Bug fixes:
    *Resolved significant issue with bmi_derived_c_14c variable processing, which was previously being based on miscoded values
    *Resolved issue with BMI code in moba_helpers introduced in 0.3.1 which was preventing outliers from being excluded in all BMI variables


# phenotools 0.3.1 (05.10.23)

* New variables added: 
    * ipip_extra_c_14c
    * ipip_agree_c_14c
    * ipip_consc_c_14c
    * ipip_stab_c_14c
    * ipip_intellec_c_14c
    * edeq_restraint_c_14c
    * edeq_shape_c_14c
    * bmi_derived_c_14c
    * nhipic_extra_c_8yr
    * nhipic_imag_c_8yr
    * nhipic_neurot_c_8yr
    * mchat_full_c_18m
    * mchat_crit_c_18m
    
    See `available_variables()` for details.

* Bug fixes: 
    * moved moba_filepaths creation outside of moba specific section so that it is always available to other curate_ fns, even if no
    moba vars are requested

# phenotools 0.3.0 

* Updated basic curate code and internal data to work with 14yr data 
* Updated vignette and curate doc to highlight implicit call to MBRN and SV_INFO, which was causing problems when filenames were
non-standard

# phenotools 0.2.9 

* Update default filepaths for TSD storage changes 
* Updates to handle changes in NPR data delivery 
* All bug fixes from 0.2.8 fully implemented


...

# phenotools 0.1.0 

## Initial Release to GitHub

* Initial Release to GitHub
* Prior to this it was private package
