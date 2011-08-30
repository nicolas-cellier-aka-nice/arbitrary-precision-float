| package |
package := Package name: 'ArbitraryPrecisionFloat'.
package paxVersion: 1;
	basicComment: 'ArbitraryPrecisionFloat is an implementation of Floating Point Numbers with a fixed number of binary digits.

It can do arithmetic with other numbers and use IEEE rounding to nearest even mode.

It has no limit on the exponent, except memory limitations of the VM of course.

Implementation is based on Smalltalk LargeInteger arithmetic.
The class is composed of 3 instance variables which should be integer:

    * a mantissa,
    * a power of two (biasedExponent)
    * and number of bits.

The sign is stored in the mantissa.

Note that modern implementations of multiple precision packages use array of floating point storage for efficiency.
Also, as far as i know, Smalltalk LargeInteger multiplication is not optimized (it is a naive n*n implementation, not a n*log(n) as could give a FFT)
So don''t expect the ultimate performance of this package.
Take it for what it is worth : a very easy to program and extend framework thanks to the Smalltalk language.
It will eventually improve in the future.

Only a few functions are implemented by now: exp ln sqrt.
Also the rounded value of pi can be computed.
TODO: implement arbitrary precision other usual functions (like sin, cos, ...).

Note: this package has been published in Squeak source and Visualworks public store.

License is MIT

Copyright (c) <2006-2008> <Nicolas Cellier>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

 '.


package classNames
	add: #ArbitraryPrecisionFloat;
	add: #ArbitraryPrecisionFloatTest;
	yourself.

package methodNames
	add: #ArithmeticValue -> #addToArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #divideIntoArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #greaterThanArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #multiplyByArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #subtractFromArbitraryPrecisionFloat:;
	add: #Float -> #asArbitraryPrecisionFloatNumBits:;
	add: #Fraction -> #asArbitraryPrecisionFloatNumBits:;
	add: #Integer -> #asArbitraryPrecisionFloatNumBits:;
	add: #Number -> #asArbitraryPrecisionFloatNumBits:;
	add: #ScaledDecimal -> #asArbitraryPrecisionFloatNumBits:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'E:\Documents and Settings\nicolas\My Documents\Dolphin Smalltalk X6\Object Arts\Dolphin\Base\Dolphin';
	add: 'E:\Documents and Settings\nicolas\My Documents\Dolphin Smalltalk X6\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

Number subclass: #ArbitraryPrecisionFloat
	instanceVariableNames: 'nBits mantissa biasedExponent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #ArbitraryPrecisionFloatTest
	instanceVariableNames: 'zero one two half minusOne minusTwo huge'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ArithmeticValue methodsFor!

addToArbitraryPrecisionFloat: anArbitraryPrecisionFloat
	"Private - Answer the result of adding the receiver to the known ArbitraryPrecisionFloat,
	anArbitraryPrecisionFloat, by coercing the less general of it and the receiver.
	Overridden by subclasses which can implement more efficiently."

	^anArbitraryPrecisionFloat retry: #+ coercing: self!

divideIntoArbitraryPrecisionFloat: anArbitraryPrecisionFloat
	"Private - Answer the result of dividing the known ArbitraryPrecisionFloat,
	anArbitraryPrecisionFloat, by the receiver by coercing the less general of it and the receiver.
	Overridden by subclasses which can implement more efficiently."

	^anArbitraryPrecisionFloat retry: #/ coercing: self!

greaterThanArbitraryPrecisionFloat: anArbitraryPrecisionFloat
	"Private - Answer whether the receiver is greater than the known ArbitraryPrecisionFloat,
	anArbitraryPrecisionFloat, by coercing the less general of it and the receiver.
	Overridden by subclasses which can implement more efficiently."

	^anArbitraryPrecisionFloat retry: #< coercing: self!

multiplyByArbitraryPrecisionFloat: anArbitraryPrecisionFloat
	"Private - Answer the result of multiplying the receiver by the known ArbitraryPrecisionFloat,
	anArbitraryPrecisionFloat, by coercing the less general of it and the receiver.
	Overridden by subclasses which can implement more efficiently."

	^anArbitraryPrecisionFloat retry: #* coercing: self!

subtractFromArbitraryPrecisionFloat: anArbitraryPrecisionFloat
	"Private - Answer the result of subtracting the receiver from the known ArbitraryPrecisionFloat,
	anArbitraryPrecisionFloat, by coercing the less general of it and the receiver.
	Overridden by subclasses which can implement more efficiently."

	^anArbitraryPrecisionFloat retry: #- coercing: self! !
!ArithmeticValue categoriesFor: #addToArbitraryPrecisionFloat:!double dispatch!private! !
!ArithmeticValue categoriesFor: #divideIntoArbitraryPrecisionFloat:!double dispatch!private! !
!ArithmeticValue categoriesFor: #greaterThanArbitraryPrecisionFloat:!double dispatch!private! !
!ArithmeticValue categoriesFor: #multiplyByArbitraryPrecisionFloat:!double dispatch!private! !
!ArithmeticValue categoriesFor: #subtractFromArbitraryPrecisionFloat:!double dispatch!private! !

!Float methodsFor!

asArbitraryPrecisionFloatNumBits: n 
	| mantissa exponent |
	self isZero ifTrue: [^0 asArbitraryPrecisionFloatNumBits: n ].
	exponent := (self exponent max:Float emin - 1) 
				- Float precision + 1.
	mantissa := (self timesTwoPower: exponent negated) truncated. 
	^ ArbitraryPrecisionFloat
		mantissa: mantissa
		exponent: exponent
		nBits: n


! !
!Float categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !

!Fraction methodsFor!

asArbitraryPrecisionFloatNumBits: n 
	"Answer a Floating point with arbitrary precision
	close to the receiver."

	"Note: form below would not be the closest approximation
	^ (numerator asArbitraryPrecisionFloatNumBits: n)
		inPlaceDivideBy: (denominator asArbitraryPrecisionFloatNumBits: n)"

	| a b q r exponent nBits ha hb hq q1 |
	a := numerator abs.
	b := denominator abs.
	ha := a highBit.
	hb := b highBit.

	"If both numerator and denominator are represented exactly in floating point number,
	then fastest thing to do is to use hardwired float division"
	nBits := n + 1.
	(ha < nBits and: [hb < nBits]) 
		ifTrue: 
			[^(numerator asArbitraryPrecisionFloatNumBits: n) 
				inPlaceDivideBy: (denominator asArbitraryPrecisionFloatNumBits: n)].

	"Try and obtain a mantissa with n+1 bits by integer division.
	This is n bits for mantissa plus 1 bit for rounding
	First guess is rough, we might get one more bit or one less"
	exponent := ha - hb - nBits.
	exponent > 0 
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	q := a quo: b.
	r := a - (q * b).
	hq := q highBit.

	"Use exactly nBits"
	hq > nBits 
		ifTrue: 
			[exponent := exponent + hq - nBits.
			r := (q bitAnd: (1 bitShift: hq - nBits) - 1) * b + r.
			q := q bitShift: nBits - hq].
	hq < nBits 
		ifTrue: 
			[exponent := exponent + hq - nBits.
			q1 := (r bitShift: nBits - hq) quo: b.
			q := (q bitShift: nBits - hq) bitAnd: q1.
			r := (r bitShift: nBits - hq) - (q1 * b)].

	"check if we should round upward.
	The case of exact half (q bitAnd: 1) = 1 & (r isZero)
	will be handled by Integer>>asDouble"
	((q bitAnd: 1) isZero or: [r isZero]) ifFalse: [q := q + 1].

	"build the Double"
	^(self positive 
		ifTrue: [q asArbitraryPrecisionFloatNumBits: n]
		ifFalse: [q negated asArbitraryPrecisionFloatNumBits: n]) 
			inPlaceTimesTwoPower: exponent! !
!Fraction categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !

!Integer methodsFor!

asArbitraryPrecisionFloatNumBits: n
	^ArbitraryPrecisionFloat 
		mantissa: self
		exponent: 0
		nBits: n! !
!Integer categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !

!Number methodsFor!

asArbitraryPrecisionFloatNumBits: n 
	self subclassResponsibility! !
!Number categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !

!ScaledDecimal methodsFor!

asArbitraryPrecisionFloatNumBits: n
	^self asFraction asArbitraryPrecisionFloatNumBits: n! !
!ScaledDecimal categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

ArbitraryPrecisionFloat guid: (GUID fromString: '{A31D459E-87E4-41E8-8EC6-05E017548CC7}')!
ArbitraryPrecisionFloat comment: 'I store floating point numbers in base 2 with some arbitrary precision (arbitrary number of bits).
I do inexact arithmetic like Float.
But I am very slow due to emulated (Large) Integer arithmetic... (compared to IEEE 754 hardwired)

Unlike Float, mantissa is not normalized under the form 1.mmmmmm
It is just stored as an integer.
The sign is stored in the mantissa.
biasedExponent is the power of two that multiply the mantissa to form the number. there is no limitation of exponent (overflow or underflow), unless you succeed in exhausting the VM memory...

Like Float, my arithmetic operations are inexact. They will round to nearest nBits ArbitraryPrecisionFloat.

If two different precisions are used in arithmetic, the result is expressed in the higher precision.

Default operating mode is rounding, but might be one of the other possibility (truncate floor ceiling).

Instance Variables:
	biasedExponent	<Integer>	the times two power to multiply the mantissa (floating binary scale)
	mantissa	<Integer>	the bits of mantissa including sign
	nBits	<Magnitude>	number of bits to be stored in mantissa when i am normalized
'!
!ArbitraryPrecisionFloat categoriesForClass!ArbitraryPrecisionFloat! !
!ArbitraryPrecisionFloat methodsFor!

- aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber subtractFromArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n)
		subtract: (aNumber asArbitraryPrecisionFloatNumBits: n)!

* aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber multiplyByArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n)
		multiplyBy: (aNumber asArbitraryPrecisionFloatNumBits: n)!

/ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber divideIntoArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n)
		divideBy: (aNumber asArbitraryPrecisionFloatNumBits: n)!

+ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber addToArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n)
		add: (aNumber asArbitraryPrecisionFloatNumBits: n)!

< aNumber
	aNumber class = self class ifTrue:
		[self negative == aNumber negative
			ifTrue: [self negative
						ifTrue: [^ (self digitCompare: aNumber) > 0]
						ifFalse: [^ (self digitCompare: aNumber) < 0]]
			ifFalse: [^ self negative]].
	^ aNumber greaterThanArbitraryPrecisionFloat: self!

= aNumber
	aNumber understandsArithmetic ifFalse: [^ false].
	aNumber class = self class ifTrue:
		[aNumber negative == self negative
			ifTrue: [^ (self digitCompare: aNumber) = 0]
			ifFalse: [^ false]].
	^ super = aNumber!

absPrintExactlyOn: aStream base: base
	"Print my value on a stream in the given base. 
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version guarantees that the printed representation exactly represents my value
	by using exact integer arithmetic."

	| fBase significand exp baseExpEstimate be be1 r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount |
	fBase := base asFloat.
	self reduce.
	significand := mantissa abs.
	roundingIncludesLimits := significand even.
	exp := biasedExponent.
	baseExpEstimate := (self exponent * 2 ln / fBase ln - 1.0e-10) ceiling.
	exp >= 0
		ifTrue:
			[be := 1 << exp.
			significand ~= 1
				ifTrue:
					[r := significand * be * 2.
					s := 2.
					mPlus := be.
					mMinus := be]
				ifFalse:
					[be1 := be * 2.
					r := significand * be1 * 2.
					s := 4.
					mPlus := be1.
					mMinus := be]]
		ifFalse:
			[significand ~= 1
				ifTrue:
					[r := significand * 2.
					s := (1 << (exp negated)) * 2.
					mPlus := 1.
					mMinus := 1]
				ifFalse:
					[r := significand * 4.
					s := (1 << (exp negated + 1)) * 2.
					mPlus := 2.
					mMinus := 1]].
	baseExpEstimate >= 0
		ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
		ifFalse:
			[scale := base raisedToInteger: baseExpEstimate negated.
			r := r * scale.
			mPlus := mPlus * scale.
			mMinus := mMinus * scale].
	(r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s))
		ifTrue: [baseExpEstimate := baseExpEstimate + 1]
		ifFalse:
			[r := r * base.
			mPlus := mPlus * base.
			mMinus := mMinus * base].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue:
			[decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [aStream nextPutAll: ('0.000000' copyFrom: 1 to: 2 - baseExpEstimate)]]
		ifFalse:
			[decPointCount := 1]. 
	[d := r // s.
	r := r \\ s.
	(tc1 := (r < mMinus) | (roundingIncludesLimits & (r = mMinus))) |
	(tc2 := (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s)))] whileFalse:
		[aStream nextPut: (Character digitValue: d).
		r := r * base.
		mPlus := mPlus * base.
		mMinus := mMinus * base.
		decPointCount := decPointCount - 1.
		decPointCount = 0 ifTrue: [aStream nextPut: $.]].
	tc2 ifTrue:
		[tc1 not | (tc1 & (r*2 >= s)) ifTrue: [d := d + 1]].
	aStream nextPut: (Character digitValue: d).
	decPointCount > 0
		ifTrue:
		[decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
		aStream nextPutAll: '.0'].
	fixedFormat ifFalse:
		[aStream nextPut: $e.
		aStream nextPutAll: (baseExpEstimate - 1) printString]!

add: b
	"This must be sent with an ArbitraryPrecisionFloat argument"
	
	^self copy inPlaceAdd: b!

agm: aNumber 
	"Answer the arithmetic geometric mean of self and aNumber"

	| a b am gm |
	a := self.
	b := aNumber.
	
	[am := a + b timesTwoPower: -1.	"am is arithmetic mean"
	gm := (a * b) sqrt.	"gm is geometric mean"
	a = am or: [b = gm]] 
			whileFalse: 
				[a := am.
				b := gm].
	^am!

asArbitraryPrecisionFloatNumBits: n 
	^ nBits = n
		ifTrue: [self]
		ifFalse: [self copy setPrecisionTo: n]!

asFloat
	"Convert to a IEEE 754 double precision floating point.
	Take care of IEEE handling of Underflow."
	
	| e n |
	e := self exponent.
	n := e >= (Float emin-1) "1022"
		ifTrue: [Float precision "53"]
		ifFalse: [Float precision + e + Float emin - 1].
	nBits > n ifTrue: [^(self copy setPrecisionTo: n) asFloat].
	^mantissa asFloat timesTwoPower: biasedExponent!

asTrueFraction

	"First remove lowBits from mantissa.
	This can save a useless division and prevent gcd: cost"
	self reduce.
	
	^ biasedExponent >= 0
		ifTrue: [self shift: mantissa by: biasedExponent]
		ifFalse: [
			"Now avoid a painfull GCD: algorihm.
			mantissa is odd and cannot be reduced by a power of two.
				mantissa / (1 bitShift: exponent negated)"
			^ Fraction numerator: mantissa denominator: (1 bitShift: biasedExponent negated)]!

biasedExponent
	^biasedExponent!

coerce: anArithmeticValue
	"Private - Answer the lower generality ArithmeticValue, anArithmeticValue, 
	converted to an ArbitraryPrecisionFloat."

	^anArithmeticValue asArbitraryPrecisionFloatNumBits: nBits
!

copy
	"Answer a copy of the receiver (by default a copy which shares the receiver's instance variables)"

	^self shallowCopy!

digitCompare: b 
	"both are positive or negative.
	answer +1 if i am greater, -1 if i am smaller, 0 if equal"
	
	| compare |
	self isZero
		ifTrue: [b isZero
				ifTrue: [^ 0]
				ifFalse: [^ -1]].
	b isZero
		ifTrue: [^ 1].
	compare := (self numBitsInMantissa + biasedExponent - b numBitsInMantissa - b biasedExponent) sign.
	^ compare = 0
		ifTrue: [(self - b) sign]
		ifFalse: [compare]!

divideBy: b
	"This must be sent with an ArbitraryPrecisionFloat argument"
	
	^self copy inPlaceDivideBy: b!

exp
	"Answer the exponential of the receiver."

	| ln2 x q r ri res n maxIter p one two |
	one := self one.
	two := one timesTwoPower: 1.
	"Use following decomposition:
		x exp = (2 ln * q + r) exp.
		x exp = (2**q * r exp)"
	ln2 := two ln.
	x := self / ln2.
	q := x truncated.
	r := (x - q) * ln2.

	"now compute r exp by power series expansion
	we compute (r/(2**p)) exp ** (2**p) in order to have faster convergence"
	p := 10 min: nBits // 2.
	r := r timesTwoPower: p negated.
	ri := one asArbitraryPrecisionFloatNumBits: nBits + 16.
	res := ri copy.
	n := 0.
	maxIter := 1 + ((nBits + 16) / p) ceiling.
	[n <= maxIter] whileTrue: 
			[n := n + 1.
			ri inPlaceMultiplyBy: r / n.
			res inPlaceAdd: ri].
	p timesRepeat: [res inPlaceMultiplyBy: res].
	res inPlaceTimesTwoPower: q.

	"now use a Newton iteration to refine the result
	res = res * (self - res ln + 1)"
	[| oldres delta |
	oldres := res.
	res := res asArbitraryPrecisionFloatNumBits: res numBits + 32.
	res inPlaceMultiplyBy: self - res ln + 1.
	delta := (res - oldres) exponent.
	delta = 0 or: [delta <= (res exponent - nBits - 8)]] 
			whileFalse.
	
	^res asArbitraryPrecisionFloatNumBits: nBits!

exponent
	"anwser the floating point like exponent e,
	of self normalized as
	1.mmmmmm * (2 raisedTo: e)"
	
	self isZero ifTrue: [^0].
	^biasedExponent + self numBitsInMantissa - 1!

generality
	"Private - Answer the generality of the receiver.
	Answer a priority higher than Float if more digits..."

	^nBits > Float precision
		ifTrue: [41]
		ifFalse: [39]
!

hash
	"Hash is reimplemented because = is implemented."
	
	^ self asTrueFraction hash!

inPlaceAdd: b 
	| delta |
	b isZero ifTrue: [^self round].
	self isZero 
		ifTrue: 
			[mantissa := b mantissa.
			biasedExponent := b biasedExponent]
		ifFalse: 
			[biasedExponent = b biasedExponent 
				ifTrue: [mantissa := mantissa + b mantissa]
				ifFalse: 
					["check for early truncation. beware, keep 1 bit for rounding"

					delta := self exponent - b exponent.
					delta > (nBits max: self numBitsInMantissa) 
						ifFalse: 
							[delta negated > (nBits max: b numBitsInMantissa) 
								ifTrue: 
									[mantissa := b mantissa.
									biasedExponent := b biasedExponent]
								ifFalse: 
									[delta := biasedExponent - b biasedExponent.
									delta > 0 
										ifTrue: 
											[mantissa := (self shift: mantissa by: delta) + b mantissa.
											biasedExponent := biasedExponent - delta]
										ifFalse: [mantissa := mantissa + (self shift: b mantissa by: delta negated)]]]]].
	self round!

inPlaceAddNoRound: b 
	| delta |
	b isZero ifTrue: [^self].
	self isZero 
		ifTrue: 
			[mantissa := b mantissa.
			biasedExponent := b biasedExponent]
		ifFalse: 
			[delta := biasedExponent - b biasedExponent.
			delta isZero 
				ifTrue: [mantissa := mantissa + b mantissa]
				ifFalse: 
					[delta > 0 
						ifTrue: 
							[mantissa := (self shift: mantissa by: delta) + b mantissa.
							biasedExponent := biasedExponent - delta]
						ifFalse: [mantissa := mantissa + (self shift: b mantissa by: delta negated)]]]!

inPlaceCopy: b 
	"copy another arbitrary precision float into self"

	mantissa := b mantissa.
	biasedExponent := b biasedExponent.
	nBits := b numBits!

inPlaceDivideBy: y 
	"reference http://citeseer.ist.psu.edu/rd/34625967%2C717775%2C1%2C0.25%2CDownload/http://citeseer.ist.psu.edu/cache/papers/cs2/516/http:zSzzSzperso.ens-lyon.frzSzjean-michel.mullerzSzDivIEEETC-aug04.pdf/brisebarre04accelerating.pdf"

	| zh x q |
	zh := y reciprocal reduce.
	x := self copy.
	self inPlaceMultiplyBy: zh.
	q := self copy.
	"r := "self inPlaceMultiplyBy: y negated andAccumulate: x.
	"q' := "self inPlaceMultiplyBy: zh andAccumulate: q.

	"ALGO 4
	| zh r zl |
	zh := b reciprocal.
	r := b negated inPlaceMultiplyBy: zh andAccumulate: (1 asArbitraryPrecisionFloatNumBits: nBits).
	zl := (b asArbitraryPrecisionFloatNumBits: nBits + 1) reciprocal inPlaceMultiplyBy: r.
	self inPlaceMultiplyBy: zh andAccumulate: (zl inPlaceMultiplyBy: self)"!

inPlaceMultiplyBy: b
	self inPlaceMultiplyNoRoundBy: b.
	self round!

inPlaceMultiplyBy: b andAccumulate: c 
	"only do rounding after the two operations.
	This is the traditional muladd operation in aritmetic units"
	
	self inPlaceMultiplyNoRoundBy: b.
	self inPlaceAdd: c!

inPlaceMultiplyNoRoundBy: b
	mantissa := mantissa * b mantissa.
	biasedExponent := biasedExponent + b biasedExponent.!

inPlaceNegated
	mantissa := mantissa negated!

inPlaceReciprocal
	| ma h |
	self isZero ifTrue: [(ZeroDivide dividend: self) signal].
	ma := mantissa abs.
	h := ma highBit + 1.
	"this one would be inefficient, we have to open code it
		m := ((1 bitShift: h + nBits) / mantissa) rounded.
	it is also:
		((num/den)+(sign/2)) truncated
	it is also:
		(num*2)+(sign*den) quo: den*2"
		
	mantissa := (1 bitShift: h + nBits + 1) + ma quo: (self shift: mantissa by: 1).
	biasedExponent := biasedExponent negated - h - nBits.
	self round!

inPlaceSubtract: b 
	| delta |
	b isZero ifTrue: [^self round].
	self isZero 
		ifTrue: 
			[mantissa := b mantissa negated.
			biasedExponent := b biasedExponent]
		ifFalse: 
			[biasedExponent = b biasedExponent
				ifTrue: [mantissa := mantissa - b mantissa]
				ifFalse: 
					["check for early truncation. beware, keep 1 bit for rounding"

					delta := self exponent - b exponent.
					delta > (nBits max: self numBitsInMantissa) 
						ifFalse: 
							[delta negated > (nBits max: b numBitsInMantissa) 
								ifTrue: 
									[mantissa := b mantissa negated.
									biasedExponent := b biasedExponent]
								ifFalse: 
									[delta := biasedExponent - b biasedExponent.
									delta >= 0 
										ifTrue: 
											[mantissa := (self shift: mantissa by: delta) - b mantissa.
											biasedExponent := biasedExponent - delta]
										ifFalse: [mantissa := mantissa - (self shift: b mantissa by: delta negated)]]]]].
	self round!

inPlaceSubtractNoRound: b 
	| delta |
	b isZero ifTrue: [^self].
	self isZero 
		ifTrue: 
			[mantissa := b mantissa negated.
			biasedExponent := b biasedExponent]
		ifFalse: 
			[delta := biasedExponent - b biasedExponent.
			delta isZero 
				ifTrue: [mantissa := mantissa - b mantissa]
				ifFalse: 
					[delta >= 0 
						ifTrue: 
							[mantissa := (self shift: mantissa by: delta) - b mantissa.
							biasedExponent := biasedExponent - delta]
						ifFalse: [mantissa := mantissa - (self shift: b mantissa by: delta negated)]]]!

inPlaceTimesTwoPower: n 
	self isZero
		ifFalse: [biasedExponent := biasedExponent + n]!

isZero
	^mantissa isZero!

ln
	"Answer the neperian logarithm of the receiver."

	| x4 one two p res s16 |
	self <= self zero ifTrue: [self error: 'ln is only defined for x > 0.0'].
	
	one := self one.
	self = one ifTrue: [^self zero].
	self < one ifTrue: [^self reciprocal ln negated].
	two := one timesTwoPower: 1.

	"Use Salamin algorithm (approximation is good if x is big enough)
		x ln = Pi  / (2 * (1 agm: 4/x) ).
	If x not big enough, compute (x timesTwoPower: p) ln - (2 ln * p)"
	s16 := self asArbitraryPrecisionFloatNumBits: nBits + 16.
	x4 := (4 asArbitraryPrecisionFloatNumBits: nBits + 16) 
				inPlaceDivideBy: s16.
	s16 exponent > (nBits + 16) 
		ifTrue: [p := 0]
		ifFalse: 
			[p := nBits + 16 - s16 exponent.
			x4 inPlaceTimesTwoPower: p negated].
	res := s16 pi / (one agm: x4) timesTwoPower: -1.
	^(self = two 
		ifTrue: [res / (p + 1)]
		ifFalse: [p = 0 ifTrue: [res] ifFalse: [res - ((two asArbitraryPrecisionFloatNumBits: nBits + p highBit + 16) ln * p)]]) 
			asArbitraryPrecisionFloatNumBits: nBits!

mantissa
	^mantissa!

mantissa: m exponent: e nBits: n 
	mantissa := m.
	biasedExponent := e.
	nBits := n.
	self round!

multiplyBy: b
	"This must be sent with an ArbitraryPrecisionFloat argument"
	
	^self copy inPlaceMultiplyBy: b!

negated
	^self copy inPlaceNegated!

negative
	^mantissa negative!

nextToward: aNumber 
	"answer the nearest floating point number to self with same precision than self,
	toward the direction of aNumber argument.
	If the nearest one falls on the other side of aNumber, than answer a Number"

	| next |

	"if self is greater, decrease self, but never under aNumber"
	self > aNumber 
		ifTrue: 
			[next := self nextTowardNegativeInfinity.
			^next >= aNumber 
				ifTrue: [next]
				ifFalse: [aNumber]].

	"if self is smaller, increase self, but never above aNumber"
	self < aNumber 
		ifTrue: [next := self nextTowardPositiveInfinity.
			^next <= aNumber 
				ifTrue: [next]
				ifFalse: [aNumber]].

	"if we are equal, return self"
	^self!

nextTowardNegativeInfinity
	"answer the nearest floating point number less than self with same precision than self"

	self normalize.
	^self class new 
		mantissa: mantissa - 1
		exponent: biasedExponent
		nBits: nBits!

nextTowardPositiveInfinity
	"answer the nearest floating point number greater than self with same precision than self"

	self normalize.
	^self class new 
		mantissa: mantissa + 1
		exponent: biasedExponent
		nBits: nBits!

normalize
	"normalize the receiver.
	a normalized floating point is either 0, or has mantissa highBit = nBits"
	
	| delta |
	mantissa isZero
		ifTrue: [biasedExponent := 0]
		ifFalse: 
			[self round.
			delta := self numBitsInMantissa - nBits.
			delta < 0 
				ifTrue: 
					[mantissa := self shift: mantissa by: delta negated.
					biasedExponent := biasedExponent + delta]]!

numBits
	^nBits!

numBitsInMantissa
	"this is equal to nBits only if we are normalized.
	If we are reduced (low bits being zero are removed), then it will be less.
	If we haven't been rounded/truncated then it will be more"

	^mantissa abs highBit!

one
	^self class 
		mantissa: 1
		exponent: 0
		nBits: nBits!

pi
	"answer the value of pi rounded to nBits.
	Note: use the Brent-Salamin Arithmetic Geometric Mean algorithm"

	| a b c k pi oldpi oldExpo expo |
	a := self one asArbitraryPrecisionFloatNumBits: nBits + 16.
	b := (a timesTwoPower: 1) sqrt reciprocal.
	c := a timesTwoPower: -1.
	k := 1.
	oldpi := Float pi.
	oldExpo := 2.
	
	[| am gm a2 |
	am := a + b timesTwoPower: -1.
	gm := (a * b) sqrt.
	a := am.
	b := gm.
	a2 := a squared.
	c inPlaceSubtract: (a2 - b squared timesTwoPower: k).
	pi := (a2 timesTwoPower: 1) / c.
	expo := (oldpi - pi) exponent.
	expo isZero or: [expo > oldExpo or: [expo < (-1 - nBits)]]] 
			whileFalse: 
				[oldpi := pi.
				oldExpo := expo.
				k := k + 1].
	^pi asArbitraryPrecisionFloatNumBits: nBits!

positive
	^mantissa positive!

printOn: aStream
	^self printOn: aStream base: 10!

printOn: aStream base: base 
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		space;
		nextPutAll: #readFrom:;
		space;
		nextPut: $'.
	self negative ifTrue: [aStream nextPut: $-].
	self absPrintExactlyOn: aStream base: base.
	aStream
		nextPut: $';
		space;
		nextPutAll: #readStream;
		space;
		nextPutAll: #numBits:;
		space;
		print: nBits;
		nextPut: $)!

reciprocal
	^self copy inPlaceReciprocal!

reduce
	"remove trailing zero bits from mantissa so that we can do arithmetic on smaller integer
	(that will un-normalize self)"
	
	| delta |
	delta := mantissa abs lowBit - 1.
	delta > 0
		ifTrue: [mantissa := self shift: mantissa by: delta negated.
			biasedExponent := biasedExponent + delta]!

round
	"apply algorithm round to nearest even used by IEEE arithmetic"
	
	| delta ma carry |
	mantissa isZero 
		ifTrue: 
			[biasedExponent := 0.
			^self].
	ma := mantissa abs.
	delta := ma highBit - nBits.
	delta > 0 
		ifTrue: 
			["inexact := ma lowBit <= delta."
			carry := (ma anyMask: (1 bitShift: delta - 1)) ifTrue: [1] ifFalse: [0].
			mantissa := self shift: mantissa by: delta negated.
			biasedExponent := biasedExponent + delta.
			(carry = 1 and: [mantissa odd or: [ma lowBit < delta]]) 
				ifTrue: 
					[mantissa := mantissa + mantissa sign.
					self truncate]]!

setPrecisionTo: n 
	nBits := n.
	self round!

shift: m by: d
	"shift mantissa m absolute value by some d bits, then restore sign"
	
	^m negative
		ifTrue: [(m negated bitShift: d) negated]
		ifFalse: [m bitShift: d]!

sign
	^mantissa sign!

significandAsInteger
	self normalize.
	^mantissa abs!

sqrt
	"Answer the square root of the receiver."

	| decimalPlaces n norm guess previousGuess one stopIteration |
	self < 0 
		ifTrue: 
			[^ FloatingPointException signal: 'undefined if less than zero.'].

	"use additional bits"
	decimalPlaces := nBits + 16.

	"constants"
	one := 1 asArbitraryPrecisionFloatNumBits: decimalPlaces.
	one normalize.
	n := self asArbitraryPrecisionFloatNumBits: decimalPlaces.

	"normalize n"
	norm := n exponent quo: 2.
	n := n timesTwoPower: norm * -2.

	"Initial guess for sqrt(1/n)"
	previousGuess := self class 
				mantissa: 1
				exponent: (n exponent negated quo: 2)
				nBits: decimalPlaces.
	guess := previousGuess copy.

	"use iterations x(n+1) := x*( 1 +  (1-x*x*n)/2) to guess sqrt(1/n)"
	
	[guess inPlaceMultiplyNoRoundBy: guess.
	guess inPlaceMultiplyBy: n.
	guess inPlaceNegated.
	guess inPlaceAddNoRound: one.

	"stop when no evolution of numBits + 12 first bits"
	stopIteration := guess isZero or: [guess exponent < (decimalPlaces - 4) negated].
	guess inPlaceTimesTwoPower: -1.
	guess inPlaceAddNoRound: one.
	guess inPlaceMultiplyNoRoundBy: previousGuess.
	guess negative ifTrue: [guess inPlaceNegated].

	stopIteration] 
			whileFalse: 
				[guess round.
				previousGuess inPlaceCopy: guess].

	"multiply by n and un-normalize"
	guess inPlaceMultiplyBy: n.
	guess inPlaceTimesTwoPower: norm.
	^guess asArbitraryPrecisionFloatNumBits: nBits!

storeOn: aStream
	aStream nextPut: $(; nextPutAll: self class name.
	aStream space; nextPutAll: 'mantissa:'; space; print: mantissa.
	aStream space; nextPutAll: 'exponent:'; space; print: biasedExponent.
	aStream space; nextPutAll: 'nBits:'; space; print: nBits.
	aStream nextPut: $)!

subtract: b
	"This must be sent with an ArbitraryPrecisionFloat argument"
	
	^self copy inPlaceSubtract: b!

timesTwoPower: n 
	^ self isZero
		ifTrue: [self]
		ifFalse: [self copy inPlaceTimesTwoPower: n]!

truncate
	"remove trailing bits if they exceed our allocated number of bits"

	| delta |
	delta := self numBitsInMantissa - nBits.
	delta > 0 
		ifTrue: 
			[mantissa := self shift: mantissa by: delta negated.
			biasedExponent := biasedExponent + delta]!

truncated
	"answer the integer that is nearest to self in the interval between zero and self"
	
	^biasedExponent negated > self numBitsInMantissa 
		ifTrue: [0]
		ifFalse: [self shift: mantissa by: biasedExponent]!

zero
	^self class 
		mantissa: 0
		exponent: 0
		nBits: nBits! !
!ArbitraryPrecisionFloat categoriesFor: #-!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #*!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #/!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #+!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #<!comparing!public! !
!ArbitraryPrecisionFloat categoriesFor: #=!comparing!public! !
!ArbitraryPrecisionFloat categoriesFor: #absPrintExactlyOn:base:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #add:!arithmetic-internal!private! !
!ArbitraryPrecisionFloat categoriesFor: #agm:!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asFloat!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asTrueFraction!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #biasedExponent!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #coerce:!coercing!private! !
!ArbitraryPrecisionFloat categoriesFor: #copy!copying!public! !
!ArbitraryPrecisionFloat categoriesFor: #digitCompare:!private! !
!ArbitraryPrecisionFloat categoriesFor: #divideBy:!arithmetic-internal!private! !
!ArbitraryPrecisionFloat categoriesFor: #exp!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #exponent!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #generality!coercing!private! !
!ArbitraryPrecisionFloat categoriesFor: #hash!comparing!public! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceAdd:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceAddNoRound:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceCopy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceDivideBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyBy:andAccumulate:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyNoRoundBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceNegated!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceReciprocal!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceSubtract:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceSubtractNoRound:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceTimesTwoPower:!private! !
!ArbitraryPrecisionFloat categoriesFor: #isZero!public!testing! !
!ArbitraryPrecisionFloat categoriesFor: #ln!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #mantissa!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #mantissa:exponent:nBits:!initialize/release!public! !
!ArbitraryPrecisionFloat categoriesFor: #multiplyBy:!arithmetic-internal!private! !
!ArbitraryPrecisionFloat categoriesFor: #negated!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #negative!public!testing! !
!ArbitraryPrecisionFloat categoriesFor: #nextToward:!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #nextTowardNegativeInfinity!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #nextTowardPositiveInfinity!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #normalize!private! !
!ArbitraryPrecisionFloat categoriesFor: #numBits!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #numBitsInMantissa!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #one!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #pi!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #positive!public!testing! !
!ArbitraryPrecisionFloat categoriesFor: #printOn:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #printOn:base:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #reciprocal!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #reduce!private! !
!ArbitraryPrecisionFloat categoriesFor: #round!private! !
!ArbitraryPrecisionFloat categoriesFor: #setPrecisionTo:!initialize/release!public! !
!ArbitraryPrecisionFloat categoriesFor: #shift:by:!private! !
!ArbitraryPrecisionFloat categoriesFor: #sign!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #significandAsInteger!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #sqrt!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #storeOn:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #subtract:!arithmetic-internal!private! !
!ArbitraryPrecisionFloat categoriesFor: #timesTwoPower:!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #truncate!private! !
!ArbitraryPrecisionFloat categoriesFor: #truncated!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #zero!arithmetic!public! !

!ArbitraryPrecisionFloat class methodsFor!

mantissa: mantisInteger exponent: expoInteger nBits: nbitsInteger 
	^self basicNew 
		mantissa: mantisInteger
		exponent: expoInteger
		nBits: nbitsInteger!

readFrom: aStream numBits: n
	"read a number from an ASCII encoded decimal representation"
	
	self error: 'NOT IMPLEMENTED YET'! !
!ArbitraryPrecisionFloat class categoriesFor: #mantissa:exponent:nBits:!instance creation!public! !
!ArbitraryPrecisionFloat class categoriesFor: #readFrom:numBits:!instance creation!public! !

ArbitraryPrecisionFloatTest guid: (GUID fromString: '{642E6D3F-8269-423C-8A82-B8C013E2C79E}')!
ArbitraryPrecisionFloatTest comment: 'Test to check FloatingPoint numbers with arbitrary precision'!
!ArbitraryPrecisionFloatTest categoriesForClass!Unclassified! !
!ArbitraryPrecisionFloatTest methodsFor!

setUp
	zero := 0 asArbitraryPrecisionFloatNumBits: Float precision.
	one := 1 asArbitraryPrecisionFloatNumBits: Float precision.
	two := 2 asArbitraryPrecisionFloatNumBits: Float precision.
	half := 1/2 asArbitraryPrecisionFloatNumBits: Float precision.
	minusOne := -1 asArbitraryPrecisionFloatNumBits: Float precision.
	minusTwo := -2 asArbitraryPrecisionFloatNumBits: Float precision.
	huge := (10 raisedTo: 100) asArbitraryPrecisionFloatNumBits: Float precision.!

testEqual
	self assert: zero = zero.
	self deny: zero = one.
	self deny: minusOne = one.

	self assert: zero = 0.
	self assert: 0 = zero.
	self assert: zero = 0.0.
	self assert: 0.0 = zero.
	
	self deny: two = (1/2).
	self deny: (1/2) = two.
	self deny: zero = 1.0.
	self deny: 0.0 = one.!

testExpLn
	self assert: (1 asArbitraryPrecisionFloatNumBits: Float precision) exp asFloat = 1 asFloat exp.

	self assert: (5 asArbitraryPrecisionFloatNumBits: Float precision) exp asFloat = 5 asFloat exp.
	self assert: (5 asArbitraryPrecisionFloatNumBits: Float precision) exp ln asFloat = 5 asFloat exp ln.!

testExpVsFloat
	"Test if we find same results as double precision exp function.
	Unfortunately, we cannot trust native exp function too much...
	it appears to incorrectly round last bit sometimes (at least with x86)"

	| badFloat |
	badFloat := OrderedCollection new.
	-100 to: 100
		do: 
			[:v | 
			self 
				assert: ((v asArbitraryPrecisionFloatNumBits: Float precision) exp asFloat 
						= v asFloat exp or: 
								[badFloat add: v.
								(v asArbitraryPrecisionFloatNumBits: 100) exp asFloat 
									= (v asArbitraryPrecisionFloatNumBits: Float precision) exp])].
	Transcript cr; show: 'Bad float vis a vis exp function: ' , badFloat printString!

testIEEEArithmeticVersusFloat
	| floats ops ref new |
	floats := #(1.0 2.0 3.0 5.0 10.0 0.5 0.25 1.0e60 0.1 1.1e-30 1.0e-60) asOrderedCollection.
	-1 to: 1 do: [:pow | floats add: (1.0 timesTwoPower: Float precision + pow); add: (1.0 timesTwoPower: (Float precision + pow) negated)].
	floats add: Float pi.
	ops := #(#+ #- #* #/ #= #< #> ).
	ops
		do: [:op | floats
				do: [:f1 | floats
						do: [:f2 | 
							ref := f1 perform: op with: f2.
							new := (f1 asArbitraryPrecisionFloatNumBits:Float precision)
										perform: op
										with: (f2 asArbitraryPrecisionFloatNumBits: Float precision).
							self assert: new = ref]]]!

testIsZero
	self assert: zero isZero.
	self deny: one isZero.
	self deny: minusTwo isZero.!

testLessThan
	
	self assert: zero < one.
	self assert: one < two.
	self assert: two < huge.
	self assert: minusOne < one.
	self assert: minusTwo < minusOne.
	self assert: minusTwo < huge.
	
	self deny: huge < one.
	self deny: huge < zero.
	self deny: huge < minusOne.
	self deny: one < minusOne.
	self deny: minusOne < minusTwo.!

testLnVsFloat
	1 to: 100
		do: 
			[:v | 
			self assert: (v asArbitraryPrecisionFloatNumBits:Float precision) ln asFloat 
						= v asFloat ln]!

testMultiply
	self assert: zero * zero = zero.
	self assert: zero * minusOne = zero.
	self assert: huge * zero = zero.
	self assert: one * zero = zero.
	
	self assert: one * two = two.
	self assert: minusOne * one = minusOne.
	self assert: minusOne * minusTwo = two.
	
	self assert: half * two = one.
	
	"check rounding"
	self assert: huge * one = huge.!

testNegated
	self assert: zero negated = zero.
	self assert: one negated = minusOne.
	self assert: minusTwo negated = two.
	self assert: huge negated negated = huge.
!

testPi
	"check computation of pi"

	self assert: (1 asArbitraryPrecisionFloatNumBits: Float precision) pi = Float pi.!

testRoundToNearestEven
	"Check that IEEE default rounding mode is honoured,
	that is rounding to nearest even"
		
	self assert: ((one timesTwoPower: 52)+(0+(1/4))) asTrueFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(1/2))) asTrueFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(3/4))) asTrueFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/4))) asTrueFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/2))) asTrueFraction = ((1 bitShift: 52)+2).
	self assert: ((one timesTwoPower: 52)+(1+(3/4))) asTrueFraction = ((1 bitShift: 52)+2).!

testRoundToNearestEvenAgainstIEEEDouble
	"Check that IEEE default rounding mode is honoured"

	#(1 2 3 5 6 7) do: 
			[:i | 
			self assert: ((one timesTwoPower: 52) + (i / 4)) asTrueFraction 
						= ((1 asFloat timesTwoPower: 52) + (i / 4)) asTrueFraction.
			self assert: ((one timesTwoPower: 52) - (i / 4)) asTrueFraction 
						= ((1 asFloat timesTwoPower: 52) - (i / 4)) asTrueFraction]!

testSqrt
	self assert: (2 asArbitraryPrecisionFloatNumBits: Float precision) sqrt asFloat = 2 asFloat sqrt.

	"knowing that (10**3) < (2**10), 100 bits are enough for representing 10**30 exactly"
	self assert: ((10 raisedTo: 30) asArbitraryPrecisionFloatNumBits: 100) sqrt = (10 raisedTo: 15)!

testSubtract
	self assert: zero - zero = zero.
	self assert: zero - minusOne = one.
	self assert: huge - zero = huge.
	self assert: one - zero = one.
	
	self assert: one - minusOne = two.
	self assert: minusOne - minusTwo = one.
	self assert: minusOne - one = minusTwo.
	
	"check rounding"
	self assert: huge - one = huge.!

testSum
	self assert: zero + zero = zero.
	self assert: zero + minusOne = minusOne.
	self assert: huge + zero = huge.
	self assert: one + zero = one.
	
	self assert: one + minusOne = zero.
	self assert: minusOne + two = one.
	self assert: one + minusTwo = minusOne.
	
	"check rounding"
	self assert: huge + one = huge.!

testZeroOne
	"check computation of pi"

	self assert: (312 asArbitraryPrecisionFloatNumBits: 53) one = 1.
	self assert: (231 asArbitraryPrecisionFloatNumBits: 24) zero isZero.

	self assert: (213 asArbitraryPrecisionFloatNumBits: 24) one asInteger = 1.
	self assert: (123 asArbitraryPrecisionFloatNumBits: 53) zero asInteger isZero.! !
!ArbitraryPrecisionFloatTest categoriesFor: #setUp!public!setup! !
!ArbitraryPrecisionFloatTest categoriesFor: #testEqual!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testExpLn!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testExpVsFloat!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testIEEEArithmeticVersusFloat!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testIsZero!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLessThan!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLnVsFloat!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testMultiply!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testNegated!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testPi!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRoundToNearestEven!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRoundToNearestEvenAgainstIEEEDouble!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSqrt!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSubtract!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSum!public!testing! !
!ArbitraryPrecisionFloatTest categoriesFor: #testZeroOne!public!testing! !

"Binary Globals"!
