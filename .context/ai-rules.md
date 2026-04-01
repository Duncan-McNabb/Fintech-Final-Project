# AI Rules

These constraints override everything else. When in doubt, follow these.

---

## General Rules for Code Generation

1. Accuracy is more important than speed. Work through problems carefully.
2. Simplicity is preferred. Keep code as simple and straightforward as possible.
3. Solve the problem asked, nothing else. No extra features, functions, or fluff unless asked.
4. For larger tasks, review and rewrite your code before presenting it. Improvements mean simpler and more streamlined, not more complicated or fancy.
5. After accuracy is verified, optimize for speed and runtime where it matters.

---

## Rules for Interactions

1. No extra fluff. Give the most direct and straightforward answer possible. Being blunt is fine.
2. Ask before providing suggestions or recommendations on next steps — do not automatically give unsolicited advice.
3. If a revision makes code more complex, briefly explain why it was necessary. If it makes things simpler, no explanation needed unless asked.
4. Do not refactor, clean up, or improve surrounding code that was not part of the request.

---

## R Naming Conventions

```yaml
variables:         snake_case
functions:         snake_case
R6_classes:        PascalCase
constants:         SCREAMING_SNAKE_CASE
files:             snake_case  (e.g., fct_data.R, mod_inputs.R)
golem_modules:     mod_<name>.R
golem_helpers:     fct_<name>.R
golem_top_level:   app_ui.R, app_server.R, run_app.R
```

Never use `library()` or `require()` inside package code. Use `::` notation (e.g., `dplyr::filter()`).

---

## Documentation Rules

- All exported functions require roxygen2 comments with `@param`, `@return`, and `@export`.
- Complex logic requires an inline comment explaining *why*, not *what*.
- Do not add comments to straightforward code.

---

## Dependency Rules

- Prefer base R and packages already declared in `DESCRIPTION`.
- Do not add new package dependencies without asking first.
- When adding a dependency, add it to `DESCRIPTION` via `usethis::use_package()`, not manually.
- `RTL` is installed from GitHub (`patzlaw/RTL`) — it is not on CRAN.

---

## Error Handling

- Fail loudly with `stop()` and a descriptive message. Do not swallow errors silently.
- Use `req()` in Shiny contexts to guard against NULL inputs before computing.
- Do not use catch-all `tryCatch` without re-throwing or logging the error.
- Never use `browser()` or `debugonce()` in committed code.

---

## Testing

- Write a unit test for every new `fct_` function.
- Use `shiny::testServer()` for module server logic.
- Tests live in `tests/testthat/` and follow the naming pattern `test-<filename>.R`.
- Do not mock data sources in tests — use small inline fixtures instead.

---

**Note:** These rules override any conflicting patterns or defaults. When in doubt, follow these constraints.
