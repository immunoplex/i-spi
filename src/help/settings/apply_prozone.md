---
id: apply_prozone
title: Prozone (high-dose hook) correction
audience: user
params: [apply_prozone]
---

At very high analyte concentrations some assays produce a *lower* signal rather
than a higher one — the response curve bends back down, or "hooks." This
**prozone** (high-dose hook) effect can make a genuinely high sample look
moderate, and it distorts the top of a standard curve if those hooked points are
fed to the fit as if they were monotonic.

Enabling the correction detects the hook at the high-concentration end and
prevents those points from pulling the fitted curve down, so the calibration
stays monotonic through its upper range. Leave it **on** when your standards can
reach concentrations high enough to hook; it is unnecessary — and best left off —
for assays or dilution ranges that never approach the hook region. If you are
unsure, inspect the top of the standard curve: a downturn at the highest
standards is the signature of prozone.
