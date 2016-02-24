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

makeArbitraryPrecisionFloatFromMantissa: m exponent: k base: aRadix numBits: nBits	"Convert infinite precision arithmetic into Floating point with prescribed precision."	^(k positive		ifTrue: [m * (aRadix raisedToInteger: k)]		ifFalse: [Fraction numerator: m denominator: (aRadix raisedToInteger: k negated)]) asArbitraryPrecisionFloatNumBits: nBits!

nextArbitraryPrecisionFloatNumBits: numBits	"Always make an ArbitraryPrecisionFloat whether there is a decimal point or not.	Do not bother with radix scale or other things"		| numberOfTrailingZeroInIntegerPart mantissa numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart |	base := 10.	neg := self peekSignIsMinus.	integerPart := self nextUnsignedIntegerBase: base.	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.	((sourceStream peekFor: $.) and: [(fractionPart := self nextUnsignedIntegerOrNilBase: base) notNil])		ifTrue: 			[numberOfNonZeroFractionDigits := lastNonZero.			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero]		ifFalse:			[fractionPart := 0.			numberOfNonZeroFractionDigits := 0.			numberOfTrailingZeroInFractionPart := 0].	self readExponent.	fractionPart isZero		ifTrue:			[mantissa := integerPart // (base raisedToInteger: numberOfTrailingZeroInIntegerPart).			exponent := exponent + numberOfTrailingZeroInIntegerPart]		ifFalse:			[mantissa := integerPart * (base raisedToInteger: numberOfNonZeroFractionDigits)				+ (fractionPart // (base raisedToInteger: numberOfTrailingZeroInFractionPart)).			exponent := exponent - numberOfNonZeroFractionDigits].	neg ifTrue: [mantissa := mantissa negated].	^self makeArbitraryPrecisionFloatFromMantissa: mantissa exponent: exponent base: base numBits: numBits! !
!NumberParser categoriesFor: #makeArbitraryPrecisionFloatFromMantissa:exponent:base:numBits:!private! !
!NumberParser categoriesFor: #nextArbitraryPrecisionFloatNumBits:!parsing!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

