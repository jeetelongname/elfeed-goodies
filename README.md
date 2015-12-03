Elfeed goodies
==================

Various bits and pieces to enhance the [Elfeed][elfeed] user experience.

 [elfeed]: https://github.com/skeeto/elfeed

![Screenshot](data/screenshot.png)

Features
-------

* An adaptive, powerline-based header for the `*elfeed-search*` buffer, with a matching entry format.
* Split pane setup.
* A more compact, powerline-based `*elfeed-entry*` buffer.
* Easy customisation.

Getting started
------------

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
