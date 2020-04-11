# Marked data

This is a collection of libraries which offer data provenance facilities and
then use that to provide a variety of marked data structures.

The data marks are intended to be similar to, though not entirely the same as
data structures seen in other systems as `Span` or similar.  Instead data marks
are a combination of that span-like data and also file sources.  The intent here
being that combining data from multiple sources is a safe and sensible thing to
do and that after doing so, it is important to be able to discover where something
came from.

Currently we provide:

* `marked-data`: The core data mark library
* `marked-yaml`: A deliberately simplified YAML subset which supports data marking
