# Purpose of this repo

1. Learning platform: Konrad learns algorithms, data structures, statistics,
   C++ and R by building them himself.
2. Mini R video series (YouTube, or maybe just a GitHub repo).
   One folder per episode, named `<Topic>_R/`.

## R episodes

| Folder          | Topic                                   | Status      |
|-----------------|-----------------------------------------|-------------|
| `Assignment_R/` | `=`, `<-`, `->`, `assign`, ...          | done        |
| `Types_R/`      | see `Types_R/PLAN.md` (6 levels)         | in progress |

Episode style:
- plain `.R` scripts; file naming is intentionally inconsistent, keep whatever is there
- sections with `# ====` headers
- expected output as a trailing comment (`typeof(a) # double`)
- show R's surprising behavior (no errors, silent NA, coercion)
- English comments
- don't shadow base functions with variable names (`ls`, `is`, `c`, ...)

## Working mode

- If an episode is still in progress, help Konrad keep track of
  what is done and what comes next.
- When starting a new episode, add it to the table above.
