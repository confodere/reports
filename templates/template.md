## {{name}}
{{#each contents}}
    {{#with this.contents}}
- {{#each this}}{{#unless @first}}- {{/unless}}{{pp this}}{{#unless @last}} {{/unless}}{{/each}}
    {{/with}}
{{/each}}