# calculation coverage report


===================================================================
EXPERIMENT: study=ELISA_atr  experiment=FcgR2a_exp|D  project_id=64
===================================================================
registered curves (curve_lookup, this scope): 0
  -> curve_lookup has NO rows for this exact scope.
  -> but similar experiment strings exist in this study:
       study=ELISA_atr exp=FcgR2a_exp project=64

===================================================================
EXPERIMENT: study=INCEN_IN_QIV1  experiment=FcgR2a  project_id=17
===================================================================
registered curves (curve_lookup, this scope): 96
computed curves (>=1 calib_fit): 6 of 96  (6%)
  by method: bayesian=6  frequentist=6

-- by feature --
  feature=FcgR2a     curves  96 | computed   6 | remaining  90

-- by plate --
  plate=plate_1    curves  32 | computed   2 | remaining  30
  plate=plate_2    curves  32 | computed   2 | remaining  30
  plate=plate_3    curves  32 | computed   2 | remaining  30
plates: 3 total | 0 fully computed | 3 partial | 0 untouched

-- by antigen (curves | computed | remaining) --
  antigen=A_Brisbane_HA      6 |   0 |   6
  antigen=A_California_HA    6 |   0 |   6
  antigen=A_California_NA    6 |   0 |   6
  antigen=A_Darwin_HA        6 |   0 |   6
  antigen=A_Hong.Kong_HA     6 |   0 |   6
  antigen=A_Hong.Kong_NA     6 |   0 |   6
  antigen=A_Massachusetts_HA   6 |   0 |   6
  antigen=A_Singapore_HA     6 |   0 |   6
  antigen=A_Tasmania_HA      6 |   0 |   6
  antigen=A_Victoria_HA      6 |   0 |   6
  antigen=A_Wisconsin_HA     6 |   0 |   6
  antigen=B_Austria_HA       6 |   0 |   6
  antigen=B_Phuket_HA        6 |   6 |   0
  antigen=B_Washington_HA    6 |   0 |   6
  antigen=gB                 6 |   0 |   6
  antigen=TT                 6 |   0 |   6

SUMMARY: 96 curves across 16 antigens x 3 plates x 1 features; 6 computed, 90 remaining.
