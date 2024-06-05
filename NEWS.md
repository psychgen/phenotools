# *Changelog for phenotools package*

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
