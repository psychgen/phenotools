# *Changelog for phenotools package*



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
