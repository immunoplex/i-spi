# Use of generative AI in this project

*Last reviewed: 2026-11-12. Maintainer: Michael S. Zens.*

This statement describes how a generative AI assistant (Anthropic's Claude,
used through the Claude Code command-line tool) has been used in developing
the curveR packages and the I-SPI application, and how its output is checked.
It follows the principle that a project should say plainly what AI did, what
people did, and who is accountable.

## Summary

| | |
|---|---|
| **Tool** | Claude (Anthropic) via Claude Code; model versions are recorded in the working notes, not in commits |
| **Where it was used** | (1) refining the user interfaces of I-SPI; (2) reviewing and checking the integrity of the statistical analysis that processes assay responses (calibration curves, precision weights, censoring); (3) drafting code, tests and documentation under direction |
| **Where it was not used** | choosing scientific questions or study designs; interpreting results for any publication; handling participant-level data outside the access controls of the institution |
| **Accountability** | The maintainer reviews and is responsible for every change that is merged. AI is not an author of the software or of any analysis |

## What the assistant did

* **User interface refinement (I-SPI).** Proposed and implemented layout,
  labelling, help text and navigation changes in the Shiny application, which
  the maintainer reviewed in the running application.
* **Statistical integrity review.** Re-derived and challenged the variance
  models used to weight assay measurements, for example the move from a
  multiplicative to an additive measurement-error model, the treatment of
  censored (out-of-range) samples, and the handling of dilution scales.
  Findings were checked by simulation with known true values and are kept as
  automated tests (`tests/testthat` in the packages, `src/test-*.R` in I-SPI).
* **Engineering.** Wrote code, database migrations, tests and documentation
  (vignettes, glossary, data dictionary) from instructions given by the
  maintainer.

## What people did

* Set the scientific goals and requirements, for instance that existing
  behaviour and the existing test suite must remain unchanged, and that a new
  statistical model is opt-in until it has been validated.
* Decided which statistical approach to adopt and which results to trust.
* Reviewed all diffs, approved every commit, release and database change, and
  operated the deployed systems.
* Hold all credentials. The assistant works with a restricted set of
  permissions and does not store secrets in repositories.

## How output is verified

1. **Automated tests.** The unit tests of each package, including tests that
   pin pre-existing behaviour, must pass unmodified before a change is merged.
2. **Simulation with ground truth.** Statistical claims are checked on
   simulated data where the true answer is known (for example, recovering a
   known group difference, bias of weighted versus unweighted means).
3. **End-to-end checks on disposable infrastructure.** Database changes are
   rehearsed on a throwaway database; a snapshot of the affected rows is taken
   before any overwrite, and a rollback path exists.
4. **Independent package checks.** `R CMD check` with no errors or warnings
   before each release.
5. **Human review.** Nothing reaches the public repositories or the deployed
   application without the maintainer approving it.

Assistant output can be wrong in plausible-sounding ways. Where the
documentation makes a numerical claim, the number is produced by code in the
same document.

## Data and privacy

The assistant runs on the maintainer's workstation and can read the files and
database queries the maintainer permits during a session; content it reads is
processed by the AI provider's service. The maintainer limits that access to
what the task needs and applies the institution's rules for research data.
Exports that contain patient identifiers (for example weight snapshots taken
before a database change) are stored outside the repositories, and credentials
are kept in files that are excluded from version control.

## Reproducibility and provenance

Release notes and the git history record what changed and when. The packages'
`NEWS.md` files and I-SPI's `CHANGELOG.md` list each change. Commits are authored by the
maintainer and, from I-SPI v1.0.0 on, do not carry an AI co-author trailer; this document is
the project's disclosure instead. Some earlier commits carry such a trailer from before this
policy was adopted.

## Contact

Questions about the use of AI in this project or about a specific analytic
decision: open an issue in the relevant repository or contact the maintainer.
