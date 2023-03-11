# labNorm 1.0.3

* Fix: `ln_normalize_ukbb` and `ln_normalize_clalit` now return NA when the lab code is not found in the reference table.

# labNorm 1.0.2

* Added `ln_normalize_ukbb` and `ln_normalize_clalit` utility function for normalizing using lab codes. 
* Fixed the wrong formula for converting hemoglobin A1C from mmol/mol to % 

# labNorm 1.0.1

* Fixed an issue with downloading to home directory in unix (#4)
* Allow NA's when normalizing if `na.rm=TRUE`
* Fix: hang when age wasn't an integer. 

# labNorm 1.0.0

* Initial release.
