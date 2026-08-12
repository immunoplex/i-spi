# calib_standards duplicate-source diagnostic


## curve_id 9057
  NK: study=INCEN_IN_QIV1 exp=FcgR2a plateid=INCENTIVE_INDIA.FCGR2A.PLATE1_15000_QIV1 antigen=B_Phuket_HA feature=FcgR2a source=Multigam wl=__none__ nom=15000
  [A] header_unmasked rows for this plate: 0  (rows per plate should be 1)
  [A] VERDICT: single header row per plate (join is 1:1 here)
  [B] RAW xmap_standard (well,dilution) duplicates: 0
  [B] VERDICT: raw source is unique per (well,dilution) -- claim holds
  [C] joined+sliced rows for this curve: 60; distinct (well,dilution): 10; duplicated keys: 10
      (well,dilution)=(H1 25) -> 6 rows after the header join
      (well,dilution)=(H10 492075) -> 6 rows after the header join
      (well,dilution)=(H2 75) -> 6 rows after the header join
      (well,dilution)=(H3 225) -> 6 rows after the header join
      (well,dilution)=(H4 675) -> 6 rows after the header join
  [C] VERDICT: the header JOIN duplicates wells -> this is what breaks the calib_standards PK

## curve_id 9089
  NK: study=INCEN_IN_QIV1 exp=FcgR2a plateid=INCENTIVE_INDIA.FCGR2A.PLATE1_15000_QIV1 antigen=B_Phuket_HA feature=FcgR2a source=Multigam wl=__none__ nom=16000
  [A] header_unmasked rows for this plate: 0  (rows per plate should be 1)
  [A] VERDICT: single header row per plate (join is 1:1 here)
  [B] RAW xmap_standard (well,dilution) duplicates: 0
  [B] VERDICT: raw source is unique per (well,dilution) -- claim holds
  [C] joined+sliced rows for this curve: 60; distinct (well,dilution): 10; duplicated keys: 10
      (well,dilution)=(H1 25) -> 6 rows after the header join
      (well,dilution)=(H10 492075) -> 6 rows after the header join
      (well,dilution)=(H2 75) -> 6 rows after the header join
      (well,dilution)=(H3 225) -> 6 rows after the header join
      (well,dilution)=(H4 675) -> 6 rows after the header join
  [C] VERDICT: the header JOIN duplicates wells -> this is what breaks the calib_standards PK
