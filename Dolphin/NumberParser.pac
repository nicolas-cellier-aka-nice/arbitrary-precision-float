| package |
package := Package name: 'NumberParser'.
package paxVersion: 1;
	basicComment: 'NumberParser provide a class for parsing literal numbers
Main advantages are:
- use stream or string input
- able of error handling
- factor some code utility for several language/dialect syntaxes

Subclasses are used for specific syntax
- Fortran
- Smalltalk
- - Dolphin
- - Squeak
- - Visualworks'.


package classNames
	add: #DolphinNumberParser;
	add: #FORTRANNumberParser;
	add: #NumberParser;
	add: #SmalltalkNumberParser;
	add: #SqueakNumberParser;
	add: #VWNumberParser;
	yourself.

package methodNames
	add: #Integer -> #isLarge;
	add: #LargeInteger -> #isLarge;
	add: #SmallInteger -> #isLarge;
	add: 'Float class' -> #negativeZero;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #NumberParser
	instanceVariableNames: 'sourceStream base neg integerPart fractionPart exponent nDigits lastNonZero requestor failBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NumberParser subclass: #FORTRANNumberParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NumberParser subclass: #SmalltalkNumberParser
	instanceVariableNames: 'scale'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkNumberParser subclass: #DolphinNumberParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkNumberParser subclass: #SqueakNumberParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SmalltalkNumberParser subclass: #VWNumberParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Float class methodsFor!

negativeZero
	"Answer a negative zero which is the result of an underflow on a negative result"

	^self fminDenormalized negated / self radix asFloat! !
!Float class categoriesFor: #negativeZero!constants!public! !

!Integer methodsFor!

isLarge
	^self subclassResponsibility! !
!Integer categoriesFor: #isLarge!public!testing! !

!LargeInteger methodsFor!

isLarge
	^true! !
!LargeInteger categoriesFor: #isLarge!public!testing! !

!SmallInteger methodsFor!

isLarge
	^false! !
!SmallInteger categoriesFor: #isLarge!public!testing! !

"End of package definition"!

"Source Globals"!

"Classes"!

NumberParser guid: (GUID fromString: '{8A729B92-F3DB-44D1-8C04-EF8E602BB4C6}')!
NumberParser comment: 'This is a class specialized in parsing character string or stream and building numbers.
It offers a framework with utility methods and exception handling.

Number syntax is not defined and should be subclassResponsibility.
'!
!NumberParser categoriesForClass!Unclassified! !
!NumberParser methodsFor!

allowPlusSign
	"return a boolean indicating if plus sign is allowed or not"

	^self subclassResponsibility!

allowPlusSignInExponent
	"return a boolean indicating if plus sign is allowed or not in exponent"

	^self allowPlusSign!

expected: errorString 
	requestor isNil 
		ifFalse: 
			["Code for interactive error to be inserted here...
			example of code for Squeak compiler:
				requestor
					notify: errorString , ' ->'
					at: sourceStream position
					in: sourceStream"
			].
	self fail!

exponentLetters
	"answer the list of possible exponents for Numbers."

	^self subclassResponsibility!

fail
	failBlock isNil ifFalse: [^failBlock value].
	self error: 'Reading a number failed'!

failBlock: aBlockOrNil 
	failBlock := aBlockOrNil!

makeFloatFromMantissa: m exponent: k base: aRadix 
	"Convert infinite precision arithmetic into Floating point.
	This algorithm rely on correct IEEE rounding mode
	being implemented in Integer>>asFloat and Fraction>>asFloat"

	^(k positive 
		ifTrue: [m * (aRadix raisedToInteger: k)]
		ifFalse: [Fraction numerator: m denominator: (aRadix raisedToInteger: k negated)]) asFloat!

nextElementaryLargeIntegerBase: aRadix	"Form an unsigned integer with incoming digits from sourceStream.	Return this integer, or zero if no digits found.	Stop reading if end of digits or if a LargeInteger is formed.	Count the number of digits and the position of lastNonZero digit and store them in instVar."	| value digit |	value := 0.	nDigits := 0.	lastNonZero := 0.	[value isLarge or: [sourceStream atEnd		or: [digit := sourceStream next digitValue.			(0 > digit or: [digit >= aRadix])				and: [sourceStream skip: -1.					true]]]]		whileFalse: [			nDigits := nDigits + 1.			0 = digit				ifFalse: [lastNonZero := nDigits].			value := value * aRadix + digit].	^value!

nextInteger	"Read an Integer from sourceStream, asnwser that Integer.	This is a generic version dealing with an optional sign and a simple sequence of decimal digits.	Subclass might define extended syntax."		base := 10.	^self nextIntegerBase: base ifFail: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: base - 1))]!

nextIntegerBase: aRadix 
	"Form an integer with following digits"

	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerBase: aRadix.
	^isNeg ifTrue: [value negated] ifFalse: [value]!

nextIntegerBase: aRadix ifFail: aBlock	"Form an integer with optional sign and following digits from sourceStream."		| isNeg value |	isNeg := self peekSignIsMinus.	value := self nextUnsignedIntegerOrNilBase: aRadix.	value ifNil: [^aBlock value].	^isNeg		ifTrue: [value negated]		ifFalse: [value]!

nextLargeIntegerBase: aRadix nPackets: nPackets 	"Form a Large integer with incoming digits from sourceStream.	Return this integer, or zero if no digits found.	Stop reading when no more digits or when nPackets elementary LargeInteger have been encountered.	Count the number of digits and the lastNonZero digit and store them in instVar"		| high nDigitsHigh lastNonZeroHigh low nDigitsLow halfPackets |	halfPackets := nPackets bitShift: -1.	halfPackets = 0 ifTrue: [^self nextElementaryLargeIntegerBase: aRadix].	high := self nextLargeIntegerBase: aRadix nPackets: halfPackets.	high isLarge ifFalse: [^high].	nDigitsHigh := nDigits.	lastNonZeroHigh := lastNonZero.	low := self nextLargeIntegerBase: aRadix nPackets: halfPackets.	nDigitsLow := nDigits.	nDigits := nDigitsHigh + nDigitsLow.	lastNonZero := lastNonZero = 0		ifTrue: [lastNonZeroHigh]		ifFalse: [lastNonZero + nDigitsHigh].	^high * (aRadix raisedToInteger: nDigitsLow) + low!

nextMatchAll: aCollection 
	"this message was originally in Squeak Stream"

	| savePosition |
	savePosition := sourceStream position.
	(aCollection anySatisfy: 
			[:each | sourceStream atEnd or: [sourceStream next ~= each]])
		ifTrue: 
			[sourceStream position: savePosition.
			^false].
	^true!

nextNumber
	"read next number from sourceStream contents"

	^self subclassResponsibility!

nextUnsignedInteger	"Read an Integer from sourceStream, asnwser that Integer.	This is a generic version dealing with a simple sequence of decimal digits.	Subclass might define extended syntax."		base := 10.	^self nextUnsignedIntegerBase: base ifFail: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: base - 1))]!

nextUnsignedIntegerBase: aRadix 	"Form an unsigned integer with incoming digits from sourceStream.	Fail if no digit found.	Count the number of digits and the lastNonZero digit and store int in instVar "		| value |	value := self nextUnsignedIntegerOrNilBase: aRadix.	value ifNil: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: aRadix - 1))].	^value!

nextUnsignedIntegerBase: aRadix ifFail: errorBlock	"Form an unsigned integer with incoming digits from sourceStream.	Answer this integer, or execute errorBlock if no digit found.	Count the number of digits and the position of lastNonZero digit and store them in instVar"		| value |	value := self nextUnsignedIntegerOrNilBase: aRadix.	value ifNil: [^errorBlock value].	^value!

nextUnsignedIntegerOrNilBase: aRadix	"Form an unsigned integer with incoming digits from sourceStream.	Answer this integer, or nil if no digit found.	Count the number of digits and the position of lastNonZero digit and store them in instVar"		| nPackets high nDigitsHigh lastNonZeroHigh low |	"read no more digits than one elementary LargeInteger"	high :=  self nextElementaryLargeIntegerBase: aRadix.	nDigits = 0 ifTrue: [^nil].		"Not enough digits to form a LargeInteger, stop iteration"	high isLarge ifFalse: [^high].	"We now have to engage arithmetic with LargeInteger	Decompose the integer in a high and low packets of growing size:"	nPackets := 1.	nDigitsHigh := nDigits.	lastNonZeroHigh := lastNonZero.	[	low := self nextLargeIntegerBase: aRadix nPackets: nPackets .	high := high * (aRadix raisedToInteger: nDigits) + low.	lastNonZero = 0 ifFalse: [lastNonZeroHigh := lastNonZero + nDigitsHigh].	nDigitsHigh := nDigitsHigh + nDigits.	low isLarge]		whileTrue: [nPackets := nPackets * 2].	nDigits := nDigitsHigh.	lastNonZero := lastNonZeroHigh.	^high!

on: aStringOrStream 
	sourceStream := aStringOrStream isString 
				ifTrue: [ReadStream on: aStringOrStream]
				ifFalse: [aStringOrStream].
	base := 10.
	neg := false.
	integerPart := fractionPart := exponent := 0.
	requestor := failBlock := nil!

peekSignIsMinus
	"Peek an optional sign from sourceStream.
	Answer true if it is minus sign"

	| isMinus |
	isMinus := sourceStream peekFor: $-.
	isMinus ifFalse: [self allowPlusSign ifTrue: [sourceStream peekFor: $+]].
	^isMinus!

readExponent	"read the exponent if any (stored in instVar).	Answer true if found, answer false if none.	If exponent letter is not followed by a digit,	this is not considered as an error.	Exponent are always read in base 10."		| eneg epos |	exponent := 0.	sourceStream atEnd ifTrue: [^ false].	(self exponentLetters includes: sourceStream peek)		ifFalse: [^ false].	sourceStream next.	eneg := sourceStream peekFor: $-.	epos := eneg not and: [self allowPlusSignInExponent and: [sourceStream peekFor: $+]].	exponent := self nextUnsignedIntegerOrNilBase: 10.	exponent ifNil: ["Oops, there was no digit after the exponent letter.Ungobble the letter"		exponent := 0.		sourceStream						skip: ((eneg or: [epos])								ifTrue: [-2]								ifFalse: [-1]).					^ false].	eneg ifTrue: [exponent := exponent negated].	^true!

requestor: anObjectOrNil 
	requestor := anObjectOrNil! !
!NumberParser categoriesFor: #allowPlusSign!accessing!public! !
!NumberParser categoriesFor: #allowPlusSignInExponent!accessing!public! !
!NumberParser categoriesFor: #expected:!error handling!public! !
!NumberParser categoriesFor: #exponentLetters!accessing!public! !
!NumberParser categoriesFor: #fail!error handling!public! !
!NumberParser categoriesFor: #failBlock:!accessing!public! !
!NumberParser categoriesFor: #makeFloatFromMantissa:exponent:base:!private! !
!NumberParser categoriesFor: #nextElementaryLargeIntegerBase:!private! !
!NumberParser categoriesFor: #nextInteger!parsing!public! !
!NumberParser categoriesFor: #nextIntegerBase:!parsing!public! !
!NumberParser categoriesFor: #nextIntegerBase:ifFail:!parsing!public! !
!NumberParser categoriesFor: #nextLargeIntegerBase:nPackets:!private! !
!NumberParser categoriesFor: #nextMatchAll:!private! !
!NumberParser categoriesFor: #nextNumber!parsing!public! !
!NumberParser categoriesFor: #nextUnsignedInteger!parsing!public! !
!NumberParser categoriesFor: #nextUnsignedIntegerBase:!parsing!public! !
!NumberParser categoriesFor: #nextUnsignedIntegerBase:ifFail:!parsing!public! !
!NumberParser categoriesFor: #nextUnsignedIntegerOrNilBase:!parsing!public! !
!NumberParser categoriesFor: #on:!initialize/release!public! !
!NumberParser categoriesFor: #peekSignIsMinus!parsing!private! !
!NumberParser categoriesFor: #readExponent!parsing!private! !
!NumberParser categoriesFor: #requestor:!accessing!public! !

!NumberParser class methodsFor!

on: aStringOrStream
	^self new on: aStringOrStream!

parse: aStringOrStream 
	^(self new)
		on: aStringOrStream;
		nextNumber!

parse: aStringOrStream onError: failBlock 
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber! !
!NumberParser class categoriesFor: #on:!instance creation!public! !
!NumberParser class categoriesFor: #parse:!instance creation!public! !
!NumberParser class categoriesFor: #parse:onError:!instance creation!public! !

FORTRANNumberParser guid: (GUID fromString: '{77D4E71B-D173-4478-91A7-518CB9B8AB89}')!
FORTRANNumberParser comment: 'This class is able to parse ASCII representation of numbers generated by FORTRAN programs.

Possible syntax:
	digit = ''0'' | ''1'' | ''2'' | ''3'' | ''4'' | ''5'' | ''6'' | ''7'' | ''8'' | ''9'' ;
	sign =  ''+'' | ''-'';
	integer = [sign] digit{digit} ;
	float = [sign] [digit{digit}] [''.''] digit{digit} [(''E'' | ''D'' ) [sign] digit{digit} ] ;
	number = integer | float ;
	
Examples:
	124
	+124
	-124
	1.0
	1.
	.23
	1E+5
	1.0E-3
	.1E-22
	3.01D+55

Not accepted: exponent letter is sometimes omitted for double precision with 3 digits exponent...
	1.001-123

Not accepted: complex numbers into parentheses
	(1.0 , 3.11)'!
!FORTRANNumberParser categoriesForClass!Unclassified! !
!FORTRANNumberParser methodsFor!

allowPlusSign
	^true!

exponentLetters
	"answer the list of possible exponents for Numbers.
	Note: this parser will not honour precision attached to the exponent.
	different exponent do not lead to different precisions.
	only IEEE 754 double precision floating point numbers will be created"

	^'ED'!

nextFloat
	^self nextNumber asFloat!

nextNumber
	"main method for reading a number with FORTRAN syntax.
	This one can read Real and Integer (not complex)"

	| numberOfTrailingZeroInIntegerPart numberOfNonZeroFractionDigits mantissa value numberOfTrailingZeroInFractionPart noInt |
	base := 10.
	(self nextMatchAll: 'NaN') ifTrue: [^Float nan].
	neg := self peekSignIsMinus.
	(self nextMatchAll: 'Infinity') 
		ifTrue: [^neg ifTrue: [Float infinity negated] ifFalse: [Float infinity]].
	(noInt := sourceStream peekFor: $.) 
		ifTrue: 
			[integerPart := 0.
			numberOfTrailingZeroInIntegerPart := 0]
		ifFalse: 
			[integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	(noInt or: [sourceStream peekFor: $.]) 
		ifTrue: 
			[fractionPart := self nextUnsignedIntegerBase: base ifFail: [nil].
			fractionPart isNil 
				ifTrue: 
					[noInt 
						ifTrue: 
							["no interger part, no fraction part..."
							self expected: 'a digit 0 to 9'.
							^nil].
					fractionPart := 0]
				ifFalse: 
					[numberOfNonZeroFractionDigits := lastNonZero.
					numberOfTrailingZeroInFractionPart := nDigits - lastNonZero].
			self readExponent]
		ifFalse: 
			[self readExponent ifFalse: [^neg ifTrue: [integerPart negated] ifFalse: [integerPart]].
			fractionPart := 0].
	fractionPart isZero 
		ifTrue: 
			[mantissa := integerPart // (base raisedTo: numberOfTrailingZeroInIntegerPart).
			exponent := exponent + numberOfTrailingZeroInIntegerPart]
		ifFalse: 
			[mantissa := integerPart * (base raisedTo: numberOfNonZeroFractionDigits) 
						+ (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)).
			exponent := exponent - numberOfNonZeroFractionDigits].
	value := self 
				makeFloatFromMantissa: mantissa
				exponent: exponent
				base: base.
	^neg ifTrue: [value isZero ifTrue: [Float negativeZero] ifFalse: [value negated]] ifFalse: [value]! !
!FORTRANNumberParser categoriesFor: #allowPlusSign!accessing!public! !
!FORTRANNumberParser categoriesFor: #exponentLetters!accessing!public! !
!FORTRANNumberParser categoriesFor: #nextFloat!parsing!public! !
!FORTRANNumberParser categoriesFor: #nextNumber!parsing!public! !

SmalltalkNumberParser guid: (GUID fromString: '{9D05B2BF-FB07-4FB9-ABD5-90A55668723A}')!
SmalltalkNumberParser comment: 'This is a class specialized in parsing and building numbers.
Number syntax should follow Smalltalk syntax.

It does handle number literals
- float
- integer
- scaled decimal'!
!SmalltalkNumberParser categoriesForClass!Unclassified! !
!SmalltalkNumberParser methodsFor!

allowPlusSign
	"default behaviour is not to allow + sign in literal number syntax"

	^false!

exponentLetters
	"answer the list of possible exponents for Numbers.
	Note: this parser will not honour precision attached to the exponent.
	different exponent do not lead to different precisions.
	only IEEE 754 double precision floating point numbers will be created"
	
	^'edq'!

makeIntegerOrScaledInteger
	"at this point, there is no digit, nor fractionPart.
	maybe it can be a scaled decimal with fraction omitted..."

	neg ifTrue: [integerPart := integerPart negated].
	self readExponent 
		ifTrue: [integerPart := integerPart * (base raisedTo: exponent)]
		ifFalse: 
			[(self readScaleWithDefaultNumberOfDigits: 0) 
				ifTrue: 
					[nil.
					^ScaledDecimal newFromNumber: integerPart scale: scale]].
	^integerPart!

nextNumber
	"main method for reading a number.
	This one can read Float Integer and ScaledDecimal"

	| numberOfTrailingZeroInIntegerPart numberOfNonZeroFractionDigits mantissa decimalMultiplier decimalFraction value numberOfTrailingZeroInFractionPart numberOfFractionDigits |
	base := 10.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerBase: base.
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r) 
		ifTrue: 
			["<base>r<integer>"
			(base := integerPart) < 2 ifTrue: [^self expected: 'an integer greater than 1 as valid radix'].
			(sourceStream peekFor: $-) ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			^neg ifTrue: [integerPart negated ] ifFalse: [integerPart]].
	^(sourceStream peekFor: $.) 
		ifTrue: 
			[fractionPart := self nextUnsignedIntegerBase: base
						ifFail: 
							[sourceStream skip: -1.
							^neg ifTrue: [integerPart negated] ifFalse: [integerPart]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			numberOfFractionDigits := nDigits.
			self readExponent 
				ifFalse: 
					[(self readScaleWithDefaultNumberOfDigits: numberOfFractionDigits) 
						ifTrue: 
							[decimalMultiplier := base raisedTo: numberOfNonZeroFractionDigits.
							decimalFraction := (integerPart * decimalMultiplier + fractionPart) / decimalMultiplier.
							neg ifTrue: [decimalFraction := decimalFraction negated].
							^ScaledDecimal newFromNumber: decimalFraction scale: scale]].
			fractionPart isZero 
				ifTrue: 
					[mantissa := integerPart // (base raisedTo: numberOfTrailingZeroInIntegerPart).
					exponent := exponent + numberOfTrailingZeroInIntegerPart]
				ifFalse: 
					[mantissa := integerPart * (base raisedTo: numberOfNonZeroFractionDigits) 
								+ (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)).
					exponent := exponent - numberOfNonZeroFractionDigits].
			value := self 
						makeFloatFromMantissa: mantissa
						exponent: exponent
						base: base.
			^neg ifTrue: [value negated] ifFalse: [value]]
		ifFalse: [self makeIntegerOrScaledInteger]!

readScaleWithDefaultNumberOfDigits: anInteger
	"read the scale if any (stored in instVar).
	Answer true if found, answer false if none.
	If scale letter is not followed by a digit,
	then answer the default number of digits (anInteger)"

	scale := 0.
	sourceStream atEnd ifTrue: [^false].
	('s' includes: sourceStream next) 
		ifFalse: 
			[sourceStream skip: -1.
			^false].
	scale := self nextUnsignedIntegerBase: 10 ifFail: [anInteger].
	^true! !
!SmalltalkNumberParser categoriesFor: #allowPlusSign!accessing!public! !
!SmalltalkNumberParser categoriesFor: #exponentLetters!accessing!public! !
!SmalltalkNumberParser categoriesFor: #makeIntegerOrScaledInteger!private! !
!SmalltalkNumberParser categoriesFor: #nextNumber!parsing!public! !
!SmalltalkNumberParser categoriesFor: #readScaleWithDefaultNumberOfDigits:!private! !

DolphinNumberParser guid: (GUID fromString: '{AC513FE1-F5CC-44BC-9977-69F3A405AB7B}')!
DolphinNumberParser comment: 'Parse and build numbers according to Dolphin syntax.

Dolphin specific:
- allow + sign after exponent'!
!DolphinNumberParser categoriesForClass!Unclassified! !
!DolphinNumberParser methodsFor!

allowPlusSignInExponent
	^true! !
!DolphinNumberParser categoriesFor: #allowPlusSignInExponent!accessing!public! !

SqueakNumberParser guid: (GUID fromString: '{859A0728-0881-4753-9300-2681FDF5B6EA}')!
SqueakNumberParser comment: 'Parse and build numbers according to squeak syntax.

Squeak specific:
- allow 2r-10 and -2r10 and even -2r-10
- allow floating point with radix 2r10.011
- do not allow single s without following digits as ScaledDecimal

Handle special case of Float (NaN Infinity and -0.0 as negative zero)'!
!SqueakNumberParser categoriesForClass!Unclassified! !
!SqueakNumberParser methodsFor!

nextNumber
	"main method for reading a number.
	This one can read Float Integer and ScaledDecimal"

	| numberOfTrailingZeroInIntegerPart numberOfNonZeroFractionDigits mantissa decimalMultiplier decimalFraction value numberOfTrailingZeroInFractionPart |
	(self nextMatchAll: 'NaN') ifTrue: [^Float nan].
	neg := sourceStream peekFor: $-.
	(self nextMatchAll: 'Infinity') 
		ifTrue: [^neg ifTrue: [Float infinity negated] ifFalse: [Float infinity]].
	base := 10.
	integerPart := self nextUnsignedIntegerBase: base.
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r) 
		ifTrue: 
			["<base>r<integer>"
			(base := integerPart) < 2 ifTrue: [^self expected: 'an integer greater than 1 as valid radix'].
			(sourceStream peekFor: $-) ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	^(sourceStream peekFor: $.) 
		ifTrue: 
			[fractionPart := self nextUnsignedIntegerBase: base
						ifFail: 
							[sourceStream skip: -1.
							^neg ifTrue: [integerPart negated] ifFalse: [integerPart]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			self readExponent 
				ifFalse: 
					[self readScale 
						ifTrue: 
							[decimalMultiplier := base raisedTo: numberOfNonZeroFractionDigits.
							decimalFraction := (integerPart * decimalMultiplier + fractionPart) / decimalMultiplier.
							neg ifTrue: [decimalFraction := decimalFraction negated].
							^ScaledDecimal newFromNumber: decimalFraction scale: scale]].
			fractionPart isZero 
				ifTrue: 
					[mantissa := integerPart // (base raisedTo: numberOfTrailingZeroInIntegerPart).
					exponent := exponent + numberOfTrailingZeroInIntegerPart]
				ifFalse: 
					[mantissa := integerPart * (base raisedTo: numberOfNonZeroFractionDigits) 
								+ (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)).
					exponent := exponent - numberOfNonZeroFractionDigits].
			value := self 
						makeFloatFromMantissa: mantissa
						exponent: exponent
						base: base.
			^neg ifTrue: [value isZero ifTrue: [Float negativeZero] ifFalse: [value negated]] ifFalse: [value]]
		ifFalse: [self makeIntegerOrScaledInteger]!

readScale
	"read the scale if any (stored in instVar).
	Answer true if found, answer false if none.
	If scale letter is not followed by a digit,
	this is not considered as an error.
	Scales are always read in base 10, though i do not see why..."

	scale := 0.
	sourceStream atEnd ifTrue: [^false].
	('s' includes: sourceStream next) 
		ifFalse: 
			[sourceStream skip: -1.
			^false].
	scale := self nextUnsignedIntegerBase: 10
				ifFail: 
					[sourceStream skip: -1.
					^false].
	^true!

readScaleWithDefaultNumberOfDigits: anInteger
	"read the scale if any (stored in instVar).
	Answer true if found, answer false if none.
	SQUEAK SPECIFIC:If scale letter is not followed by a digit
	Do not use default number of digits, but rather answer false"

	^self readScale! !
!SqueakNumberParser categoriesFor: #nextNumber!parsing!public! !
!SqueakNumberParser categoriesFor: #readScale!private! !
!SqueakNumberParser categoriesFor: #readScaleWithDefaultNumberOfDigits:!private! !

VWNumberParser guid: (GUID fromString: '{2614DEFE-DA3A-4307-8D95-9DC9A98A0464}')!
VWNumberParser comment: 'Parse and build numbers according to VisualWorks syntax.

VW specific:
- allow exponent character without following digits'!
!VWNumberParser categoriesForClass!Unclassified! !
!VWNumberParser methodsFor!

makeIntegerOrScaledInteger
	"Same as super except that exponent letter will lead to "

	neg ifTrue: [integerPart := integerPart negated].
	self readExponent 
		ifTrue: 
			[^self 
				makeFloatFromMantissa: integerPart
				exponent: exponent
				base: base].
	(self readScaleWithDefaultNumberOfDigits: 0) 
		ifTrue: 
			[nil.
			^ScaledDecimal newFromNumber: integerPart scale: scale].
	^integerPart!

readExponent
	"read the exponent if any (stored in instVar).
	Answer true if found, answer false if none.
	If exponent letter is not followed by a digit,
	it is assumed to be zero"

	| eneg epos |
	exponent := 0.
	sourceStream atEnd ifTrue: [^false].
	(self exponentLetters includes: sourceStream next) 
		ifFalse: 
			[sourceStream skip: -1.
			^false].
	eneg := sourceStream peekFor: $-.
	epos := (eneg not and: [self allowPlusSignInExponent]) ifTrue: [sourceStream peekFor: $+] ifFalse: [false].
	exponent := self nextUnsignedIntegerBase: 10
				ifFail: 
					[eneg | epos ifTrue: [sourceStream skip: -1].
					0].
	eneg ifTrue: [exponent := exponent negated].
	^true! !
!VWNumberParser categoriesFor: #makeIntegerOrScaledInteger!private! !
!VWNumberParser categoriesFor: #readExponent!private! !

"Binary Globals"!

