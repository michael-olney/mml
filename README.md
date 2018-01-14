# mml: Minimal Markup Language

MML is a markup language based partly on Erik Naggum's unreleased
[Enamel][1] (NML) markup language. The advantages is a simple and
concise syntax for writing markup. The primary use-case is
generating HTML documents.  The following table shows the relation
between the syntax of MML and HTML:

| HTML                             | MML                |
| :------------------------------- | :----------------- |
| &lt;br/&gt;                      | {br}               |
| &lt;em&gt;&lt;/em&gt;            | {em→}              |
| &lt;a href=cat&gt;meow&lt;/a&gt; | {a{href→cat}→meow} |

Unlike HTML or XML, whitespace reduction within a tag body is part
of the syntax. In general, whitespace is eliminated completely.
The only case where whitespace is preserved is where it occurs
immediately between two non-whitespace characters that themselves
occur at or below the level at which the whitespace occurs in the
document structure. For example:

| Before Reduction                 | After Reduction         |
| :------------------------------- | :---------------------- |
| {p→   Hello,   whitespace.   }   | {p→Hello, whitespace.}  |
| {p→foo    {em→bar} baz}          | {p→foo {em→bar} baz}    |
| {p→Foo, {br} bar.}               | {p→Foo, {br}bar.}       |

Note the difference between the second and third cases. In the
second case the whitespace is broken by "bar" within the "em" tag,
while in the third case the "br" tag contains no text in its body
and so does not break whitespace.

[1]: http://www.schnada.de/grapt/eriknaggum-enamel.html

