{{#> layout}}
{{#*inline "thingo"}}
    {{#each contents as |line| }}
- {{#each line as |metric|}}{{num metric @root.words_render_context}}{{#unless @last}}, {{/unless}}{{#if @last}}.{{/if}}{{/each}}
{{/each}}
{{/inline}}
{{#*inline "table"}}
| Frequency | Calculation | Value |
| --------- | ----------- | ----- |
{{#each contents as |line|}}
{{#each line as |metric|}}
| {{frequency}} | {{calculation_type}} | {{num metric @root.nums_render_context}} |
{{/each}}
{{/each}}
{{/inline}}
{{/layout}}
{{#*inline "layout"}}

{{#each paragraphs}}
# {{name}}
{{> thingo}}

{{> table}}

{{/each}}

## Table

| Source | Number | Description |
| ------ | ------ | ----------- |
{{#each data_table}}
{{#table this @root.date}}
{{#each this}}{{#if @first}}|{{/if}} {{this}} | {{/each}}
{{/table}}
{{/each}}

{{/inline}}
{{> layout}}