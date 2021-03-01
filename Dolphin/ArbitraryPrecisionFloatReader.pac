| package |
package := Package name: 'ArbitraryPrecisionFloatReader'.
package paxVersion: 1;
	basicComment: 'This package contains the code for reading ArbitraryPrecisionFloat from a decimal floating point String representation'.


package methodNames
	add: #NumberParser -> #makeArbitraryPrecisionFloatFromMantissa:exponent:base:numBits:;
	add: #NumberParser -> #nextArbitraryPrecisionFloatNumBits:;
	add: 'ArbitraryPrecisionFloat class' -> #readFrom:numBits:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'ArbitraryPrecisionFloat';
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: 'NumberParser';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!ArbitraryPrecisionFloat class methodsFor!

readFrom: aStream numBits: n
	"read a number from an ASCII encoded decimal representation with
	- an optional sign {-|+}
	- an integer part [0-9]+
	- an optional decimalPoint and fractionPart {.[0-9]*}
	- an optional exponent {e{-|+}[0-9]+}"

	^(SmalltalkNumberParser on: aStream)
		failBlock: [self error: 'invalid ArbitraryPrecisionFloat format'];
		nextArbitraryPrecisionFloatNumBits: n! !
!ArbitraryPrecisionFloat class categoriesFor: #readFrom:numBits:!instance creation!public! !

!NumberParser methodsFor!

makeArbitraryPrecisionFloatFromMantissa: m exponent: k base: aRadix numBits: nBits

nextArbitraryPrecisionFloatNumBits: numBits
!NumberParser categoriesFor: #makeArbitraryPrecisionFloatFromMantissa:exponent:base:numBits:!private! !
!NumberParser categoriesFor: #nextArbitraryPrecisionFloatNumBits:!parsing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!
