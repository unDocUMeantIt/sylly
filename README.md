# sylly

[![Flattr this git repo](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=m.eik&url=https://github.com/unDocUMeantIt/sylly&title=sylly&language=en_GB&tags=github&category=software)

Provides the hyphenation algorithm used for 'TeX'/'LaTeX' and similar software, as proposed
by [Liang (1983)](https://tug.org/docs/liang/). Mainly contains the function 'hyphen()' to
be used for hyphenation/syllable counting of text objects. It was originally developed
for and part of the 'koRpus' package, but later released as a separate package so it's
lighter to have this particular functionality available for other packages. Support for
additional languages can be added on-the-fly or by plugin packages. Due to some restrictions on
CRAN, the full package sources are only available from the project homepage. To ask for
help, report bugs, request features, or discuss the development of the package, please
subscribe to the [koRpus-dev mailing list](http://korpusml.reaktanz.de).

More information on sylly is available on the [project homepage](https://reaktanz.de/?c=hacking&s=koRpus).

## Language support

This package contains class definitions, generic methods and tools for hyphenation, but no
out-of-the-box support for any particular language. If you're interested in using the 'sylly'
package for actual hyphenation, look for packages called 'sylly.XX', where 'XX' abbreviates
the language you're looking for (e.g., 'sylly.en' for English or 'sylly.de' for German).

## Installation

### Installation via GitHub

To install the package directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("unDocUMeantIt/sylly") # stable release
install_github("unDocUMeantIt/sylly", ref="develop") # development release
```

## Contributing

To ask for help, report bugs, suggest feature improvements, or discuss the global
development of the package, please either subscribe to the
[koRpus-dev mailing list](http://korpusml.reaktanz.de), or
use the issue tracker on GitHub.

### Branches

Please note that all development happens in the `develop` branch. Pull requests against the `master`
branch will be rejected, as it is reserved for the current stable release.

## License

sylly Copyright (C) 2017 m.eik michalke, released under the
GNU General Public License (GPL) version 3 or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the license with the
source package as the file COPYING or LICENSE.
