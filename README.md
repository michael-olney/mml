# mml: Minimal Markup Language

Author: Michael Olney  
Date: January, 2017  

MML is a markup language based partly on Erik Naggum's unreleased
[Enamel][1] (NML) markup language. The advantages are concise
syntax for writing markup and support for macros and scripting.
The primary use-case is generating HTML documents.

| HTML                             | MML                |
| :------------------------------- | :----------------- |
| &lt;br/&gt;                      | {br}               |
| &lt;em&gt;&lt;/em&gt;            | {em:}              |
| &lt;a href=cat&gt;meow&lt;/a&gt; | {a{href:cat}:meow} |

MML also supports macros:

    MML:

    {ul:
        {%foreach:{^:dogs~cats} x {^:
            {li:{$x}}
        }}
    }

    Output HTML:

    <ul><li>dogs</li><li>cats</li></ul>

Here is an example using the (experimental) pattern matching
support:

    MML:

    {%case:
        {^:dog~cat~mouse}
        {^:{$x}{$y}{$x}}{^:one}
        {^:{$x}{$y}{$z}}{^:two}
    }

    Output HTML:

    two

The syntax is intended to make writing markup easy while also
allowing for the precision required for machine processing (e.g.
macros) while remaning concise. There are no closing tags, no
quotation marks, and the whitespace rules are closer to what
you would expect from a typical programming language than to
those of SGML. For example, all unescaped whitespace between two
characters where either is a special character, is eliminated
(note that the HTML pretty printer may reintroduce whitespace).
All unescaped whitespace between any two ordinary characters is
collapsed down to a single space. These rules can be a pain for
cases where you want the whitespace around a tag to be preserved,
e.g.:

    MML:

    {p:This is the {em:wrong} syntax!}

    HTML:

    <p>This is the<em>wrong</em>syntax!</p>

So there is a syntax for tags using the angle brackets which
consumes *internal* whitespace as normal, but collapses
extrernal whitespace as if it was an ordinary character, e.g.:

    MML:

    {p:This is the <em:right> syntax!}

    HTML:

    <p>This is the <em>right</em> syntax!</p>

I have been using and developing this as an internal tool for
a few years now. It is still in an experimental stage and there
is no documentation yet other than this README. However, the
parsing is done using the [parsec][3] package for Haskell, and
the rules can be understood from *MML/Parse.hs* if you
understand parser combinators. *MML/Lex.hs* implements the
lexical rules (e.g. whitespace).

Important: the HTML output module is a simple placeholder, and
is not safe! For example, it will not handle script tags
correctly.

[Here][2] is a page built using MML. It uses the *foreach*
macro and some external scripts to generate thumbnails from a
directory pf images and place them into a grid. The scripting
interface is not yet documented, but uses a binary format to
exchange abstract syntax without any of the complications of
concrete syntax, e.g. whitespace, special characters, etc.
Support for a new language then consists in a small library
written for that language to serialize and deserialize
expressions. This allows all the libraries from the new
language (e.g. image processing libraries, as in the linked
page) to be brought into the processing of the markup.

[1]: http://www.schnada.de/grapt/eriknaggum-enamel.html
[2]: https://spectralforms.com/astrofuse/presskit/
[3]: https://hackage.haskell.org/package/parsec

