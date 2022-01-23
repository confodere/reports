{{#> layout}}
{{#*inline "thingo"}}
    {{#each metrics as |metric|}}
- {{> (partial_name metric) fig=(num metric ../words_render_context) prev=(prev_span metric)}}

    {{/each}}
{{/inline}}
{{#*inline "table"}}
| Frequency | Calculation | Value |
| --------- | ----------- | ----- |
{{#each metrics}}
| {{frequency}} | {{calculation_type}} | {{num this ../nums_render_context}} |
{{/each}}
{{/inline}}
{{/layout}}
{{#*inline "layout"}}
# Top Highlights
{{> thingo}}

{{> table}}
{{/inline}}
{{> layout}}