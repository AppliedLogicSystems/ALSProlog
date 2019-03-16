{% comment %}

# Book Citations

This include produces rich links to books based on book citation data files
with optional search or page numbers.

Required Arguments:

- `id`  - name of book's data (i.e. base name of data file)
- `sec` - section of book being references

Example Usage:

```
{% include book.md id="bowen91" sec="9.1" %}
```

Book data files (in `_data` dir) have the following structure:

```
cite:
  short: <brief citation string like "Bowen 91">
  long: <one-line citation, suitable for link title attribute, etc>

url: <URL to book with query string suitable for search or page-link>

<optional map from section names to target page numbers>
sec2page:
  <section>: <page number>
  ...
```

{% endcomment -%}

{%- assign book = site.data[ include.id ] -%}

<a title="{{ book.cite.long | escape }}"
href="
{{- book.url -}}
{%- if book.sec2page -%}
  {{ book.sec2page[include.sec] }}
{%- else -%}
  {{ include.sec }}
{%- endif -%}
">[
{{- book.cite.short | escape }}, {{ include.sec | escape -}}
]</a>