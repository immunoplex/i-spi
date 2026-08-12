# xmap_buffer direct examination

study=INCEN_IN_QIV1  experiment=FcgR2a

## column population on xmap_buffer (blank source)
  rows=352  |  nominal: 352 non-null (2 distinct)  |  plate: 352 non-null (3 distinct)  |  plateid: 352 non-null (3 distinct)
  distinct antigen=16 feature=1 source=0 wavelength=1
  stype='B' rows=NA  |  antibody_mfi>0 rows=352  (fetch_blanks keeps only these)
  >>> KEY QUESTION: nominal non-null = 352 of 352 rows -> nominal is ALWAYS populated

## distinct nominal_sample_dilution values on blanks
   nominal=15000 : 176 rows
   nominal=16000 : 176 rows

## sample blank rows (stype='B', up to 12) — the columns the worker uses
   well=A10  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_2  nom=15000    mfi=23
   well=A10  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_2  nom=16000    mfi=23
   well=A6   ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_3  nom=16000    mfi=25
   well=A6   ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_3  nom=15000    mfi=25
   well=B12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_3  nom=15000    mfi=27
   well=B12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_3  nom=16000    mfi=27
   well=C12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_1  nom=16000    mfi=25
   well=C12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_2  nom=15000    mfi=31
   well=C12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_1  nom=15000    mfi=25
   well=C12  ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_2  nom=16000    mfi=31
   well=E6   ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_1  nom=16000    mfi=26
   well=E6   ag=A_Brisbane_HA  feat=FcgR2a   src=NA         wl=__none__ plate=plate_1  nom=15000    mfi=26

## contrast: xmap_standard nominal population (standards persisted fine)
   standard rows=960  nominal non-null=960 (distinct=2)
