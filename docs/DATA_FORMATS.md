# Data formats

HalfSibMap Studio reads the CSV formats of the HalfSibMap package. Templates
can be downloaded from stage 2 (Data). Files must be comma-separated and
UTF-8 (a byte-order mark is accepted).

## Pedigree CSV

| Column | Required | Meaning |
| --- | --- | --- |
| `id` | yes | Individual identifier (unique). Kept exactly as text. |
| `mother` | yes | Mother's `id` for offspring; empty for the maternal parent. |
| `father` | yes | Leave empty. If any value is present, the app requires an explicit, recorded confirmation that the unknown-father (open-pollinated) model is used; sire identities and genotypes are not used. |
| `generation` | yes | `1` = maternal parent (founder), `2` = offspring. This is how `read_HSMap_data()` identifies offspring. |
| `family_id` | yes | Family identifier. Each family must have exactly one genotyped mother. |

Extra columns are kept in the stored raw file and ignored.

## Genotype CSV

| Column | Required | Meaning |
| --- | --- | --- |
| `marker_id` | yes | Unique marker identifier. |
| `REF`, `ALT` | yes | Allele labels, stored as annotation only. Calls are never recoded from them. |
| `chrom`, `position` | no | Physical annotation, shown as *supplied* annotation only. Never used as the genetic order. |
| one column per sample | yes | Header equal to the pedigree `id`. |

Genotype calls are hard-call allele counts of one allele per marker:
`0` (aa), `1` (Aa), `2` (AA). Missing: empty cell, `NA`, or `.`.
Anything else blocks import, including decimal dosages such as `1.5` (never
rounded or truncated), decimal-formatted integers such as `1.0`, `Inf`, values
outside 0–2, and text.

Genotype columns that match no pedigree individual are reported and excluded
from the analysis input (never interpreted as samples). Pedigree individuals
without a genotype column are reported and excluded.

## Validation rules

Blocking errors: unreadable or empty files, non-comma delimiter, missing
required columns, duplicated or empty headers, headers or IDs with leading or
trailing spaces, empty or duplicated individual IDs, empty or duplicated marker
IDs, `generation` other than 1 or 2, offspring without a mother, an individual
that is its own mother, an ID used both as a mother and as an offspring,
invalid calls, an all-missing matrix, no matching sample columns, no
analysable family, and (until confirmed) father information.

Per-family checks (a family that fails is listed but cannot be selected):
exactly one mother among its offspring, the mother has a genotype column, at
least one genotyped offspring, at least 2 markers heterozygous in the mother.

Warnings: unmatched genotype columns, pedigree rows without genotypes, markers
with no observed calls, founders with a mother value, families with several
mothers. The full report (code, message, affected IDs, locations) can be
downloaded as CSV.

## Identifier preservation

All values are read as text. Sample, family and marker IDs are mapped to
internal IDs (`S000001`, `F0001`, `M0000001`) before HalfSibMap's reader is
called, because that reader renames or retypes identifiers (see
`UPSTREAM_API_CONTRACT.md`). The mapping is stored in
`metadata/identifier_mapping.csv`, verified after import, and every table,
figure and export uses the original IDs.

## Supplied marker order

CSV with columns `marker_id`, `linkage_group`, `rank`. `linkage_group` is a
group number or its display name; `rank` is numeric (sorted ascending). The
app reports duplicates, unknown markers, markers from another group,
non-numeric or duplicated ranks, and group members that are omitted. Omitted
markers are excluded from that group's map only after you confirm the
inclusion rule.

## Project bundle (`.hsmap.zip`)

`bundle.json` (format, schema version, file list with SHA-256), `project.json`,
`metadata/`, `history/`, optionally `inputs/` and `artifacts/`. Only JSON,
CSV and RDS files under those folders are accepted.

## Results package

See the README inside each ZIP. Key tables: `marker_positions.csv`
(`family_id, mother_id, linkage_group, rank, marker_id, phase_block,
segment_id, position_cM, singleton_segment, model, map_function, run_id,
result_label`) and `adjacent_intervals.csv` (`linkage_group, interval,
from_marker, to_marker, phase_block, segment_id, r, distance_cM, status, note,
model, map_function, run_id, result_label`). `position_cM` restarts at 0 in
every segment; `distance_cM` is `NA` for every interval that is not `linked`.
