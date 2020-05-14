#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
source('lib/estimate.R')
#* @apiTitle Plumber Example API

#* Calculate r0 values based on most current infection counts
#* @get /r0
function() {
  calculateR0()
}
