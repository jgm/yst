`yst` - static websites from YAML and string templates
======================================================

`yst` is a tool for generating a static website by filling [string
template][]s with data taken from [YAML][] or [CSV][] text files. This approach
combines the speed, security, and ease of deployment of a static
website with the flexibility and maintainability of a dynamic site that
separates presentation and data.

Installing yst
--------------

`yst` is written in Haskell. The easiest way to install `yst` is by using
Haskell's [cabal install][] tool. The best way to get this tool is to
install the [Haskell platform][], which includes a complete installation
of the [GHC][] compiler and the `cabal` executable.

Once you have `cabal`, you can install yst with two commands:

    cabal update
    cabal install yst

(Note that by default, `cabal` installs the `yst` executable into
a special directory:  `~\.cabal\bin` on unix systems.  You will need
to make sure that directory is in your system path.)

If you get the error "yst-0.2.3 depends on template-haskell-2.4.0.0
which failed to install," then try the following:

    cabal install syb-with-class-0.6
    cabal install yst

Getting started
---------------

To get started with `yst`, use the command:

    yst create mysite

This will create a directory `mysite` and populate it with the files
needed for a sample site.  Change to this directory and run `yst` with
no arguments to create the site:

    cd mysite
    yst

The site will be created in the `site` directory.  Open up `site/index.html`
to take a look.

The configuration file `index.yaml` tells `yst` which pages to build,
and from which templates and data files.  Let's take a look, so we can see
what went into `index.html`. The file is a YAML list of YAML hashes
(name/value pairs). The first item is

    - url      : index.html
      title    : Home
      template : index.st
      requires : event.st
      data     :
        recentevents : FROM events.yaml ORDER BY date DESC LIMIT 2

This says:  build the page `index.html` from the string template `index.st`
(and subtemplate `event.st`) and data from `events.yaml`. Sort this data
(a list of events) by date in descending order, and discard all but the
first two items. Put the resulting data in the string template attribute
`recentevents`. Give the page the title "Home."

Now take a look at `events.yaml`, the data source. Again it is a YAML
list of YAML hashes, each item in the list representing one event.
The first is:

    - date: 2009-06-28
      speaker:  Sam Smith
      title: Building a static website

Pretty self-explanatory!  Try adding a new event, then rebuild the
site by typing `yst` and see how it looks.

If you have problems, make sure `events.yaml` is a valid YAML file.
Structure is shown through indentation, so make sure things line
up right.  And occasionally you may need to use quotation marks
around string values---for example, when a title contains a colon.

There's one more ingredient in our recipe---the string templates.
Take a look at `index.st`, the template from which `index.html`
is built:

    # Welcome
    
    Here's our website. Have a look around.
    
    Our last two events:
    
    $recentevents:event()$
    
    For a complete list of events, see the [events](events.html) page.

The first thing to notice is that this is in [markdown][] format (or,
to be precise, markdown with [pandoc][] extensions).  So, for example,
the first line is a level-one header, and there is a hyperlink
to the events page on the last line.

The second thing to notice is the bit between dollar signs.
This is a placeholder for some formatted data.  The rendered
template will include the list `recentevents` (remember, this
was populated from `events.yaml` after some transformations---see
above).  And each element of this list will be formatted by
a subtemplate called `event.st`:

    - $if(it.speaker)$$it.speaker; separator=" and "$, $endif$ *$it.title$*.

Let's break this down.  The whole line will print a bulleted list
item. `it` here refers to the event that is being processed by the
template. So the first part says: if this event has a `speaker` field,
print the speaker, or, if the `speaker` field is a list, print all the
speakers separated by the word "and", followed by a comma. And the
second part says, print the contents of the `title` field, surrounding
it with asterisks (which is the markdown way of making it *emphasized*).

(See the [string template][] documentation for details of template syntax,
and examples.) 

If you look at `index.html`, you'll see this rough structure, but in
an HTML file, not a markdown file. `yst` converts the rendered markdown
template to HTML (using pandoc), and inserts it into a "layout" file
called `layout.html.st`.  If you look at this file, you'll see that it's
an HTML file with blanks for `$contents$` and `$nav$`.  The `$contents$`
blank gets filled by the rendered template, converted to HTML, and
the `$nav$` blank gets filled by an HTML navigation menu (an unordered
list with links).

To summarize our example, then: `yst` sorts and filters the data in
`events.yaml` according to our instructions; inserts this data into
the `events.st` template, formatting each item using the `event.st`
template; uses pandoc to convert the rendered template to HTML;
constructs a navigation menu; and puts the contents and navigation
menu in the layout template `layout.html.st`. The result is our page,
`index.html`.

Reference
---------

### The `yst` command

Synopsis:

    yst                    # rebuilds site, using default config.yaml
    yst -f myconf.yaml     # rebuilds site, using myconf.yaml as config
    yst create newsite     # creates a starter (demo) site in newsite directory

When run without arguments, `yst` looks at `index.yaml` to determine
the dependencies of each page, and rebuilds only the pages whose
dependencies have changed since the last build.

In order for this to work properly, you must be sure to list all
subtemplates included recursively in the main page template using
the `requires` field.  This field takes a single filename or a
YAML list, which may be in either of two formats:

    requires: [event.st, date.st]

or

    requires:
      - event.st
      - date.st

If you don't list all the subtemplates needed to render a page
under `requires`, `yst` will still work, but it might sometimes
fail to rebuild a page when one of these subtemplates has been changed.

### `config.yaml`

The configuration file specifies the following:

- `indexfile`: the filename of the index file (default: `index.yaml`)
- `title`:  the title of the whole site
- `sourcedir`:  the directory containing all the templates and page sources
  (default: `.`)
- `datadir`:  the directory containing yaml data files (default: `.`)
- `filesdir`: the directory containing static files (default: `files`)
- `layout`: the default layout template for the site, relative to
  `sourcedir` (default: `layout.html.st`)

### `index.yaml` and submenus

The index file is a YAML list of pages. Each page may have the following
fields:

- `url`:   the relative URL of the page to be built
- `title`:   the title of the page
- `template`:   the main string template from which the page will be built
- `source`:   the markdown source from which the page will be built
- `requires`:   other files changes to which should trigger a page rebuild
  (primarily subtemplates of the main page template)
- `data`:   string template attributes, data sources, and transformations
  (see below)
- `layout`:   a layout template to use, if other than the site default
- `inmenu`:   if 'no', the page will not be included in the site navigation
   menu

Each page must have at least `url`, `title`, and either `template` or
`source`. Values for `template`, `source`, and `layout` are relative to
`sourcedir` specified in `config.yaml`.

The pages may be organized into a tree-like hierarchy, which will be
reflected in the site navigation menu.  It is easiest to see how this
works by example:

    - Rooms:
      - url      : room101.html
        title    : Room 101
        source   : room101.txt
    
      - url      : room203.html
        title    : Room 203
        source   : room203.txt

Here we have a subtree called "Rooms" with two pages under it.
Subtrees can contain other subtrees.  Just be consistent about indentation.

### The `data` field

The `data` field in `index.yaml` can populate any number of stringtemplate
attributes with data from YAML or CSV files.  The syntax is easiest to
explain by example (note that the keywords do not have to be in ALL CAPS,
although they may, and the query doesn't have to end with a semicolon,
though it may):

    data:
        events:  from events.yaml order by date desc group by title then location
        people:  from people.csv order by birthday then lastname where
                  birthstate = 'CA' limit 5

First we have the name of the stringtemplate attribute to be populated
(say, `events`).  Then, after the colon, we have the data source
(`events.yaml`) followed by one or more *transformations*, which will
be applied in order.  Here are the possible transformations.  In
what follows, brackets denote an optional component, `|` denotes
alternatives, and `*` indicates that the component may be repeated
several times:

`ORDER BY field [ASC|DESC] [THEN field [ASC|DESC]]*`

>   Sorts a list by comparing the value of `field`.  `ASC`
(the default) means "ascending", and `DESC` means "descending".
The keyword `THEN` is used to separate fields that will be
compared in order.  So, if we are ordering by `birthday then lastname`,
we will compare birthdays, and if these are equal, we will break
the tie by comparing last names. 

`GROUP BY field [THEN field]*`

>   Converts a list into a list of lists, where each sublist contains
only items with the same value for `field`.  So, for example,
`group by date` takes a list of events and produces a list of
lists of items, where each sublist contains events occuring at
a single date.  `GROUP BY date THEN venue` would produce a list
of lists of lists, and so on.

`LIMIT n`

>   Removes all but the *n* top items from a list.  *n* must be a number.

`WHERE condition`

>   Selects only items that meet a condition.

>   A *condition* in a `WHERE` statement is a Boolean combination (using
`NOT`, `AND`, `OR`, and parentheses for disambiguation) of *basic
conditions*.  A *basic condition* is of the form `value op value`,
where `value` may be either a fieldname or a constant.  Note that
all constants must be enclosed in quotes.  `op` may be one of the
following:  `=`, `>=`, `<=`, `>`, `<`.

Note that the order of transformations is significant.  You can get
different results if you use `LIMIT` before or after `ORDER BY`,
for example.

If you want to specify an attribute's value directly, rather than
reading it from a file, just omit the "FROM":

    date:
      deadline: 11/20/2009

Any YAML value can be given to an attribute in this way.

### Static files

Any file or subdirectory in the `files` directory (or whatever is
the value of `filesdir` in `config.yaml`) will be copied verbatim to
the site.  So this is the place to put javascripts, css files, images,
PDFs, and the like.

### Date fields

`yst` will recognize date fields in data files automatically, if the
dates are in one of the following formats:

 - the locale's standard date format
 - MM/DD/YYYY (e.g. 04/28/1953)
 - MM/DD/YY (e.g. 04/28/53)
 - YYYY-MM-DD (e.g. 1953-04-28)
 - DD MON YYYY (e.g. 28 Apr 1953)

Dates may be formatted in templates using a stringtemplate "format"
directive. There's an example in the demo file `date.st`:

    $it; format="%B %d, %Y"$

The following codes may be used in format strings (taken from
Haskell's `Date.Time.Format` documentation):

- `%D` :   same as `%m/%d/%y`
- `%F` :   same as `%Y-%m-%d`
- `%x` :   as dateFmt locale (e.g. `%m/%d/%y`)
- `%Y` :   year
- `%y` :   last two digits of year, 00 - 99
- `%C` :   century (being the first two digits of the year), 00 - 99
- `%B` :   month name, long form (fst from months locale), January - December
- `%b, %h` :   month name, short form (snd from months locale), Jan - Dec
- `%m` :   month of year, leading 0 as needed, 01 - 12
- `%d` :   day of month, leading 0 as needed, 01 - 31
- `%e` :   day of month, leading space as needed, 1 - 31
- `%j` :   day of year for Ordinal Date format, 001 - 366
- `%G` :   year for Week Date format
- `%g` :   last two digits of year for Week Date format, 00 - 99
- `%f` :   century (first two digits of year) for Week Date format, 00 - 99
- `%V` :   week for Week Date format, 01 - 53
- `%u` :   day for Week Date format, 1 - 7
- `%a` :   day of week, short form (snd from wDays locale), Sun - Sat
- `%A` :   day of week, long form (fst from wDays locale), Sunday - Saturday
- `%U` :   week number of year, where weeks start on Sunday (as sundayStartWeek), 00 - 53
- `%w` :   day of week number, 0 (= Sunday) - 6 (= Saturday)
- `%W` :   week number of year, where weeks start on Monday (as mondayStartWeek), 00 - 53

### Lists as values

In some cases, a field may have one or several values.  For example, an
event might occur at a date or a date range, and an article may have one
author or a list of authors.

An elegant way to handle these cases is to let the field take either a
scalar or a list value, and use stringtemplate's "separator" directive
to format the result appropriately.  So, for example, in our `events.yaml`
we have:

    - date: 2009-06-28
      speaker:  Sam Smith
      title: Building a static website
    
    - date:  2009-04-15
      speaker:
        - Sam Smith
        - '[Jim Jones](http://foo.bar/baz)'
      title: Advantages of static websites
    
    - date:
        - 2009-04-20
        - 2009-04-22
      title: Seminar on web security
    
    - date: 2009-04-15
      speaker: Jim Jones
      title:  XSS attacks for dummies

Note that the `date` field is sometimes a single date, sometimes a
list (with start and end date of a range), and the `speaker` field is
sometimes a single speaker, and sometimes a range.

Here is how we handle the date in `eventgroup.st`:

    **$first(it).date:date(); separator=" - "$**

Here `first(it).date` is the raw data, which may be a single date
or a list.  `first(it).date:date()` is the result of formatting each
date using the `date.st` template (discussed above).  And
`first(it).date:date; separator=" - "` is the result of taking this
list of formatted dates and concatenating them, separated by a hyphen.
When there is just one date, we just get a date. When there are two,
we get a date range.

We can use the same trick in the case of `speaker`.  If `it` is
an event record, then `it.speaker; separator=" and "` will be
either a single speaker (if the value is not a list) or a list
of speakers separated by "and" (if it is a list).

In sorting lists with `order by`, `yst` compares two lists by
comparing the first members, then (in case of a tie) the second
members, and so on.  If one item is a list and the other a scalar,
the scalar is compared to the first item of the list.  So, in
the example above, `Seminar on web security` will be sorted
an earlier than an event with date `2009-04-21`, and later than
an event with date range `2009-04-20 - 2009-04-21`.

### YAML gotchas

If you have a colon in a YAML value, be sure to enclose it in quotes,
or you'll get an error.  So,

    title:  "Cheever: A Life"

not

    title:  Cheever: A Life

Or (especially if the string is long), use `>` or `|` for a wrapped
or unwrapped multiline string literal:

    title: |
      A very long string that
      goes on and on.

      You can even have blank lines,
      but be sure to maintain indentation.

### Using CSV files instead of YAML

If you like, you can use a CSV file instead of YAML for your data source.
Just give it the extension `.csv`.  In `index.yaml`, you'd have:

    data:
      events: from events.csv order by date desc

This can be handy if you're using existing data, because spreadsheets
and databases can easily be dumped to CSV. In the case of a SQL
database, you can use a query like this to get the CSV:

    SELECT * INTO OUTFILE 'result.csv'
    FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
    LINES TERMINATED BY '\n'
    FROM my_table;

(Thanks to
<http://www.terminally-incoherent.com/blog/2006/07/20/dump-mysql-table-into-csv-file/>.)

### Using HTML in the templates

Markdown allows raw HTML to be used, so you can embed HTML in templates.
Pandoc's extended markdown is different from standard markdown in that
it parses text within HTML block elements as markdown.  So, for example,
you can include a section in `<div>` tags, or use raw `<ul>` and `<li>`
tags instead of markdown formatting.

### Layout templates

Layout files are also string templates, but they are not treated as
markdown by default.  They should use a double extension to indicate
the format.  So, for example, an HTML layout could be `standard.html.st`,
and a LaTeX layout could be `printed.tex.st`.  `yst` will convert the
page contents appropriately for the format of the layout template.
Here are the supported formats and extensions:

- HTML:   `.html.st`, `.xhtml.st`
- LaTeX:   `.tex.st`, `.latex.st`
- ConTeXt:   `.context.st`
- Groff man:   `.`1`.st`
- Rich text format:   `.rtf.st`
- Texinfo:   `.texi.st`
- DocBook:   `.db.st`
- OpenDocument XML:   `.fodt.st`
- Plain text (markdown):   `.txt.st`, `.markdown.st`

The demo site shows how you can use `yst` to produce a LaTeX document
from the same data sources you use to produce HTML pages.

The following stringtemplate attributes are defined when layouts
are rendered:

- `$contents$`:   the result of rendering the page and converting to the layout's format
- `$nav$`:   an HTML navigation menu created from `index.yaml`
- `$gendate$`:   the date the page was generated
- `$sitetitle$`:   the site title from `config.yaml`
- `$pagetitle$`:   the page title as defined in `index.yaml`
- `$root$`: the path to the website's root, relative to the page being
  rendered.  So, for example, if we are rendering `rooms/room503.html`,
  `$root$` will have the value `../`. Put `$root$` in front of relative URLs
  in your layout file, so that the links aren't broken on pages in
  subdirectories.

### Previewing a site

If you use only relative URLs in your site, you can preview it by
opening any of the HTML files in site in your web browser. If you use
absolute links, this won't work, but you can use Jinjing Wang's simple
static web server `maid`:

    cabal update
    cabal install maid

To use maid to preview your site, just change to the site directory and
start `maid`:

    cd site
    maid

The site will appear at <http://localhost:3000>.  If you want to serve it
at another port, just pass the port number as an argument to `maid`:

    maid 5999

## Development

### Source code

yst's source code lives on github at <http://github.com/jgm/yst/tree/master>.
You can clone the repository with

   git://github.com/jgm/yst.git

To install the development code once you've checked it out, just do

    cabal install

(But please stick to the released version if you don't like things to break
unexpectedly!)

### Reporting bugs

If you find a bug, please report it using
[the issue tracker on yst's github page](http://github.com/jgm/yst/issues).


[string template]: http://www.stringtemplate.org/
[YAML]: http://www.yaml.org/
[CSV]: http://en.wikipedia.org/wiki/Comma-separated_values
[cabal install]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall
[Haskell platform]: http://hackage.haskell.org/platform/
[markdown]: http://daringfireball.com/markdown
[pandoc]: http://johnmacfarlane.net/pandoc/
[vim]: http://www.vim.org/
[GHC]: http://www.haskell.org/ghc/

