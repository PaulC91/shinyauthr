## Resubmission
* Added return value documentation to exported `runExample` function (no return value)
* Removed .Rd files for internal functions

## Notes on previous submissions
* Updated an outdated URL
* Ensured any code related to tests that should not be run on CRAN servers is not run. 

## Test environments

### Local
* macOS 10.14.6 Mojave, R 4.0.3

### github actions CI
* windows-latest (release)
* macOS-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)

### win-builder
* `devtools::check_win_release`
* `devtools::check_win_devel`

### R-hub
* `rhub::check_for_cran`

## R CMD check results

New submission

0 errors ✓ | 0 warnings ✓ | 1 note x
