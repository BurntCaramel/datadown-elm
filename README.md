# Datadown for Elm

Parse Datadown content.

## Datadown format

```markdown
# A Bootstrap button


## classes:
- btn
- btn-primary
-? tags.primary
- btn-secondary
-? tags.secondary
- btn-large
-? tags.large


## primitive:
### component: @View
### classes: @classes


## html:
\`\`\`mustache
<button class="{{ classes }}">{{ content }}</button>
\`\`\`
```
