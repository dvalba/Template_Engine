# Template_Engine

A small template engine written in Haskell (student project, 2023).

It parses templates with a Jinja-like syntax:

- `{% for-x in list %} ... {% endfor-x %}` — loops
- `{% if-cond %} ... {% endif %}` — conditionals
- `{{ var }}` — variable interpolation
- `{* comment *}` — comments

Embedded conditions and values are evaluated with an arithmetic expression parser/evaluator built on **Parsec** (`ParserExpresion.hs`), so templates can compute values while rendering.

## Modules

- `ParserExpresion.hs` — expression parsing and evaluation (Parsec + `Text.Parsec.Expr`).
- `Template_Engine.hs` — template element model and template rendering.
