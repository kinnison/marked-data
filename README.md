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

* `marked-yaml`: A deliberately simplified YAML subset which supports data marking

## Marked YAML

The `marked-yaml` library offers a subset of YAML designed for configuration files
and similar use.  The data it reads in is "marked" with its origin.  If you want
to use `marked-yaml` with your existing `serde` applications, you can enable the
`serde` feature, and if you want the errors produced by the `marked-yaml`
deserializer to include nice paths to any problem, along with ensuring the marker
for the problem area is populated in any errors, use the `serde-path` feature.
