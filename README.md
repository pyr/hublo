hublo: the world needed another site generator
==============================================

### Configuration

```elisp
(source-dir "site/sources")
(output-dir "site/out")

(handle! "\\.org$"  :org)
(handle! "\\.html$" :identity)
(handle! "\\.mustache$"   :mustache)

(page "/entries/.*"
      (slugify)
      (route "/entries/{{year}}/{{month}}/{{day}}/{{slug}}")
      (group-with :entries)
      (layout "entry")
      (layout "default"))

(page "/index.mustache"
      (title "spootnik.org some title")
      (route "/")
      (layout "default"))
```

### Running

```
hublo clean
hublo publish
hublo phases <phase-list>
```
