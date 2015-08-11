This project was automatically exported from https://code.google.com/p/arbitrary-precision-float 

ArbitraryPrecisionFloat is a new Smalltalk Number subclass that can perform arithmetic operations, and evaluate the principal mathematical functions.

Computations are performed with inexact arithmetic like other Floating point numbers. However, the precision of representation is programmable, and the arithmetic operations and elementary function evaluations are accurate to 1/2 unit of least precision (ulp).

ArbitraryPrecisionFloat is able of mixed arithmetic operations like the other Number subclasses. A conversion to the most accurate inexact number class will occur in this case.

Other requirements are listed [here](wiki/Requirements) and are (or should be) implemented in [ArbitraryPrecisionFloatTests](wiki/ArbitraryPrecisionFloatTests). A quick introduction on how to use the library is explained [here](wiki/UsingArbitraryPrecisionFloat).

ArbitraryPrecisionFloat implementation relies on LargeInteger arithmetic that exists on most Smalltalk dialects. The design and implementation will be detailed in the wiki.

ArbitraryPrecisionFloat has been implemented in [Cincom Visualworks](wiki/ArbitraryPrecisionFloatForVisualWorks), [Squeak/Pharo](wiki/ArbitraryPrecisionFloatForSqueakAndPharo) and [Dolphin](wiki/ArbitraryPrecisionFloatForDolphin) dialects, but port to other dialects shall not be too hard. You can contact the author if help is required for such a port.

For links to reference work in the domain, and sources of inspiration of this library, please visit InspirationSources.
