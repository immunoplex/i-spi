---
id: import.bead.xponent.upload
title: Importing xPONENT (.csv) bead files
audience: user
category: procedural
see_also: [import.plate_grid.confirm, compute.import.description_shape_binding]
---
Select one or more Luminex xPONENT `.csv` export files and click **Parse uploaded
file(s)**. Unlike the Bio-Plex `.rbx`/`.srbx` format, xPONENT exports are plain
text — the file carries its own plate map and bead counts, but doesn't report its
own dilutions the same way, so Standards wells still typically need a manual
dilution reference (see [[import.plate_grid.confirm|confirming the plate layout]]).
From there, continue through [[compute.import.description_shape_binding|configuring
the description field]] as usual.
