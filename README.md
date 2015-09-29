hublo: the world needed another site generator
==============================================

### Configuration

```elisp
(source-dir "site/sources")
(output-dir "site/out")

(handle! "\\.org$"      :org)
(handle! "\\.html$"     :noop)
(handle! "\\.css$"      :noop)
(handle! "\\.mustache$" :mustache)

(page "/entries/.*"
      (slugify)
      (route "/entries/{{year}}/{{month}}/{{day}}/{{slug}}.html")
      (group-with :entries)
      (layout "blog")
      (layout "default"))

(page "/index.mustache"
      (route "/index.html")
      (title "home")
      (layout "default"))

(page "/index.xml.mustache"
      (title "spootnik")
      (metadata :description "my blog")
      (route "/index.xml"))

(page "/static/.*")
```

### Running

```
hublo clean
hublo publish
hublo phases <phase-list>
```

### Philosophy

Hublo privdes a way to build a site by publishing pages in different *phases*.
A phase is a barrier that must be reached by all pages before advancing to the next.
Each hublo command consists of a list of phases to run through, some shortcuts
are defined in `hublo-cli.el`.

For instance, the most common hublo command is `publish` and translates to
the following phases: `:bootstrap`, `:metadata`, `:route`, `:content`, `:augment`, `:publish`, and `:clean`.

This allows some steps to gather information or metadata from other pages which provides
an elegant way to build lists and incrementally produce content.

In the above example, blog entries are grouped together with the label `:entries` which is
then accessible in templates. This allows the index page and an rss feed page to go through
the list. Similar techniques can be used for publishing content dynamically.

The configuration is processed as elisp code and can thus accomodate for plenty of scenarios.

There is an example configuration at https://github.com/pyr/blog with the corresponding output
at: http://spootnik.org.
