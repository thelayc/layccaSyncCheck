layccaSyncCheck
===============

# Installtion
```R
# install.packages("devtools")
devtools::install_github("thelayc/layccaSyncCheck")
```

This package implements two functions to help identifying discrepancies between LAYC internal database and the DC school system database.

### check_attendance()
The check_attendance() function automatically identifies attendance records that are not in sync

### check_pro_dmg()
The check_pro_dmg() function automatically identifies students records that are not in sync (names, usi numbers, strat date, exit date)
