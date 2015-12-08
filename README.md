Elfeed goodies
==================

[![MELPA](https://melpa.org/packages/elfeed-goodies-badge.svg)](https://melpa.org/#/elfeed-goodies)

Various bits and pieces to enhance the [Elfeed][elfeed] user experience.

 [elfeed]: https://github.com/skeeto/elfeed

[![Screenshot](data/screenshot.png)](https://raw.githubusercontent.com/algernon/elfeed-goodies/master/data/screenshot.png)

Features
-------

* An adaptive, powerline-based header for the `*elfeed-search*` and
  `*elfeed-entry*` buffers, with a matching entry format.
* Split pane setup.
* A function to toggle the `*elfeed-log*` buffer in a popup window. 
* Easy customisation.

Getting started
------------

The easiest way to get started is to install the package via [MELPA][melpa]:

```elisp
(package-install 'elfeed-goodies)
```

 [melpa]: https://melpa.org/#/elfeed-goodies

Following which, it is recommended to call the `(elfeed-goodies/setup)`
function, that sets up the various bits and pieces of the enhancements.

```elisp
(require 'elfeed)
(require 'elfeed-goodies)

(elfeed-goodies/setup)
```

You can customise some aspects of the package with `M-x customize-group
elfeed-goodies`.

Copyright & License
------------------------

Copyright (c) 2015 Gergely Nagy, released under the terms of the GNU GPLv3+.
