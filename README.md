This project was automatically exported from code.google.com/p/arbitrary-precision-float 

ArbitraryPrecisionFloat is a new Smalltalk Number subclass that can perform arithmetic operations, and evaluate the principal mathematical functions.

Computations are performed with inexact arithmetic like other Floating point numbers. However, the precision of representation is programmable, and the arithmetic operations evaluations are accurate to 1/2 unit of least precision (ulp).

For elementary mathematical functions, the precision is of 1 ulp with faithfully rounded results, but a long term goal would be to achieve i1/2 ulp - a.k.a. exactly rounded function evaluation.

ArbitraryPrecisionFloat is able of mixed arithmetic operations like the other Number subclasses. A conversion to the most accurate inexact number class will occur in this case.

Other requirements are listed [here](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/Requirements) and are (or should be) implemented in [ArbitraryPrecisionFloatTests](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/ArbitraryPrecisionFloatTests). A quick introduction on how to use the library is explained [here](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/UsingArbitraryPrecisionFloat).

ArbitraryPrecisionFloat implementation relies on LargeInteger arithmetic that exists on most Smalltalk dialects. The design and implementation will be detailed in the wiki.

ArbitraryPrecisionFloat has been implemented in [Cincom Visualworks](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/ArbitraryPrecisionFloatForVisualWorks), [Squeak/Pharo](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/ArbitraryPrecisionFloatForSqueakAndPharo) and [Dolphin](https://github.com/nicolas-cellier-aka-nice/arbitrary-precision-float/wiki/ArbitraryPrecisionFloatForDolphin) dialects, but port to other dialects shall not be too hard. You can contact the author if help is required for such a port.

Except for Dolphin, code is not directly hosted on github, but rather in dialect specific source code repositories. Also note that a fork of ArbitraryPrecisionFloat has been integrated in the [SciSmalltalk](https://github.com/SergeStinckwich/SciSmalltalk) project. An effort will be made to sync further developments with this project.

For links to reference work in the domain, and sources of inspiration of this library, please visit InspirationSources.

ArbitraryPrecisionFloat is licensed under MIT. See : http://opensource.org/licenses/MIT
