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

Only a few functions are implemented by now: exp ln sqrt sin cos tan.
Also the rounded value of pi can be computed.
TODO: implement arbitrary precision other usual functions (like arcSin, arcCos, ...).

Note: this package has been published in Squeak source and Visualworks public store.

License is MIT

Copyright (c) <2006-2010> <Nicolas Cellier>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

 '.


package classNames
	add: #ArbitraryPrecisionFloat;
	add: #ArbitraryPrecisionFloatForTrigonometry;
	yourself.

package methodNames
	add: #ArithmeticValue -> #addToArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #compareWithArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #divideIntoArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #greaterThanArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #multiplyByArbitraryPrecisionFloat:;
	add: #ArithmeticValue -> #subtractFromArbitraryPrecisionFloat:;
	add: #Float -> #asArbitraryPrecisionFloatNumBits:;
	add: #Fraction -> #asArbitraryPrecisionFloatNumBits:;
	add: #Fraction -> #compareWithArbitraryPrecisionFloat:;
	add: #Fraction -> #greaterThanArbitraryPrecisionFloat:;
	add: #Integer -> #asArbitraryPrecisionFloatNumBits:;
	add: #Integer -> #compareWithArbitraryPrecisionFloat:;
	add: #Integer -> #greaterThanArbitraryPrecisionFloat:;
	add: #Number -> #asArbitraryPrecisionFloatNumBits:;
	add: #ScaledDecimal -> #asArbitraryPrecisionFloatNumBits:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Number subclass: #ArbitraryPrecisionFloat
	instanceVariableNames: 'nBits mantissa biasedExponent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ArbitraryPrecisionFloat subclass: #ArbitraryPrecisionFloatForTrigonometry
	instanceVariableNames: 'pi'
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

compareWithArbitraryPrecisionFloat: aFloat
	^(self - aFloat) isZero!

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
!ArithmeticValue categoriesFor: #compareWithArbitraryPrecisionFloat:!double dispatch!public! !
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
			inPlaceTimesTwoPower: exponent!

compareWithArbitraryPrecisionFloat: aFloat
	^aFloat asTrueFraction = self!

greaterThanArbitraryPrecisionFloat: aFloat
	"Private - Answer whether the receiver is greater than the known Float, aFloat"

	^aFloat asTrueFraction < self! !
!Fraction categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !
!Fraction categoriesFor: #compareWithArbitraryPrecisionFloat:!double dispatch!public! !
!Fraction categoriesFor: #greaterThanArbitraryPrecisionFloat:!double dispatch!private! !

!Integer methodsFor!

asArbitraryPrecisionFloatNumBits: n
	^ArbitraryPrecisionFloat 
		mantissa: self
		exponent: 0
		nBits: n!

compareWithArbitraryPrecisionFloat: aFloat
	^aFloat asTrueFraction = self!

greaterThanArbitraryPrecisionFloat: aFloat
	"Private - Answer whether the receiver is greater than the known Float, aFloat."

	^aFloat asTrueFraction < self! !
!Integer categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !
!Integer categoriesFor: #compareWithArbitraryPrecisionFloat:!double dispatch!public! !
!Integer categoriesFor: #greaterThanArbitraryPrecisionFloat:!double dispatch!private! !

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
	^ (self asArbitraryPrecisionFloatNumBits: n) copy
		inPlaceSubtract: (aNumber asArbitraryPrecisionFloatNumBits: n)!

* aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber multiplyByArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n) copy
		inPlaceMultiplyBy: (aNumber asArbitraryPrecisionFloatNumBits: n)!

/ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber divideIntoArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n) copy
		inPlaceDivideBy: (aNumber asArbitraryPrecisionFloatNumBits: n)!

+ aNumber 
	| n |
	aNumber class = self class
		ifFalse: [^ aNumber addToArbitraryPrecisionFloat: self].
	n := nBits max: aNumber numBits.
	^ (self asArbitraryPrecisionFloatNumBits: n) copy
		inPlaceAdd: (aNumber asArbitraryPrecisionFloatNumBits: n)!

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
	^ aNumber compareWithArbitraryPrecisionFloat:self!

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

arcCos
	"Evaluate the arc cosine of the receiver."

	| arcCos x one |
	self isZero ifTrue: [^(self pi timesTwoPower: -1)].
	x := self asArbitraryPrecisionFloatNumBits: 8 + nBits.
	x inPlaceAbs.
	one := x one.
	x > one ifTrue: [self error: 'cannot compute arcCos of a number greater than 1'].
	arcCos := x = one
		ifTrue: [self zero]
		ifFalse: [((one - x squared) sqrt / x) arcTan].
	self negative ifTrue: [arcCos := x pi - arcCos].
	^arcCos asArbitraryPrecisionFloatNumBits: nBits!

arCosh
	"Evaluate the area hyperbolic cosine of the receiver."

	| arCosh x one y two |
	x := self asArbitraryPrecisionFloatNumBits: 6 + nBits.
	one := x one.
	x < one ifTrue: [self error: 'cannot compute arCosh of a number less than 1'].
	x = one ifTrue: [^self zero].
	y := x - one.
	y < one
		ifTrue:
			[y := y asArbitraryPrecisionFloatNumBits: 6 + nBits + (y exponent negated).
			two := one timesTwoPower: 1.
			arCosh := ((y * (y + two)) sqrt + y + one) ln]
		ifFalse: [arCosh := ((x squared - one) sqrt + x) ln].
	^arCosh asArbitraryPrecisionFloatNumBits: nBits!

arcSin
	"Evaluate the arc sine of the receiver."

	| arcSin x one |
	self isZero ifTrue: [^self].
	x := self asArbitraryPrecisionFloatNumBits: 6 + nBits.
	x inPlaceAbs.
	one := x one.
	x > one ifTrue: [self error: 'cannot compute arcSin of a number greater than 1'].
	arcSin := x = one
		ifTrue: [self pi timesTwoPower: -1]
		ifFalse: [(x / (one - x squared) sqrt) arcTan].
	self negative ifTrue: [arcSin inPlaceNegated].
	^arcSin asArbitraryPrecisionFloatNumBits: nBits!

arcTan
	"Evaluate the arc tangent of the receiver."

	| x arcTan one power |
	self isZero ifTrue: [^self].
	self > 1
		ifTrue:
			[x := self asArbitraryPrecisionFloatNumBits: 6 + nBits.
			x inPlaceAbs.
			arcTan := (x pi timesTwoPower: -1) - x reciprocal arcTan]
		ifFalse:
			[power := ((nBits bitShift: -1) + self exponent max: 0) highBit.
			x := self asArbitraryPrecisionFloatNumBits: nBits + (1 bitShift: 1 + power).
			x inPlaceAbs.
			one := x one.
			power timesRepeat: [x := x / (one + (one + x squared) sqrt)].
			arcTan := self powerExpansionArcTan: x precision: x numBits + 6.
			arcTan inPlaceTimesTwoPower: power].
	self negative ifTrue: [arcTan inPlaceNegated].
	^arcTan asArbitraryPrecisionFloatNumBits: nBits!

arcTan: denominator
	"Evaluate the four quadrant arc tangent of the argument denominator (x) and the receiver (y)."

	^self isZero
		ifTrue: [denominator sign positive
			ifTrue: [ (self + denominator) zero ]
			ifFalse: [ self positive
				ifTrue: [ (self + denominator) pi ]
				ifFalse: [ (self + denominator) pi negated ]]]
		ifFalse: [denominator isZero
			ifTrue: [self positive
				ifTrue: [ (self + denominator) pi timesTwoPower: -1 ]
				ifFalse: [ (self + denominator) pi negated timesTwoPower: -1 ]]
			ifFalse:
				[ | precision arcTan |
				precision := (self + denominator) numBits.
				arcTan := ((self asArbitraryPrecisionFloatNumBits: precision * 2) / (denominator asArbitraryPrecisionFloatNumBits: precision * 2)) arcTan.
				^(denominator > 0
					ifTrue: [ arcTan ]
					ifFalse: [ self > 0
						ifTrue: [ arcTan + arcTan pi ]
						ifFalse: [ arcTan - arcTan pi ]]) asArbitraryPrecisionFloatNumBits: precision]]!

arSinh
	"Evaluate the area hyperbolic cosine of the receiver."

	| arSinh x one |
	self isZero ifTrue: [^self].
	self exponent negated > nBits ifTrue: [^self].
	x := self asArbitraryPrecisionFloatNumBits: 6 + nBits + (0 max: self exponent negated).
	x inPlaceAbs.
	one := x one.
	arSinh := ((x squared + one) sqrt + x) ln.
	self negative ifTrue: [arSinh inPlaceNegated].
	^arSinh asArbitraryPrecisionFloatNumBits: nBits!

arTanh
	"Evaluate the area hyperbolic cosine of the receiver."

	| arTanh x one |
	self isZero ifTrue: [^self].
	self exponent negated > nBits ifTrue: [^self].
	x := self asArbitraryPrecisionFloatNumBits: 12 + nBits + (0 max: self exponent negated).
	x inPlaceAbs.
	one := x one.
	x >= one ifTrue: [self error: 'cannot evaluate arTanh of number of magnitude >= 1'].
	arTanh := ((one + x) / (one - x)) ln.
	arTanh inPlaceTimesTwoPower: -1.
	self negative ifTrue: [arTanh inPlaceNegated].
	^arTanh asArbitraryPrecisionFloatNumBits: nBits!

asApproximateFraction
	"Answer a Rational number--Integer or Fraction--representing the receiver.
	This conversion uses the continued fraction method to approximate 
	a floating point number."

	| num1 denom1 num2 denom2 int frac newD temp limit |
	num1 := self asFraction.	"use exact arithmetic"
	frac := num1 fractionPart.		"The fractional part of self"
	int := num1 truncated.		"The integer part of self"
	num1 := int.	"The first of two alternating numerators"
	denom1 := 1.		"The first of two alternating denominators"
	num2 := 1.		"The second numerator"
	denom2 := 0.		"The second denominator--will update"
      limit := nBits + 15 // 8.
	[frac = 0 or: [denom1 digitLength > limit or: [(self coerce: (Fraction numerator: num1 denominator: denom1)) = self]]]
		whileFalse: 
			["repeat while the fractional part is not zero"
			newD := frac reciprocal.			"Take reciprocal of the fractional part"
			int := newD truncated.		"get the integer part of this"
			frac := newD fractionPart.	"and save the fractional part for next time"
			temp := num2.				"Get old numerator and save it"
			num2 := num1.				"Set second numerator to first"
			num1 := num1 * int + temp.	"Update first numerator"
			temp := denom2.				"Get old denominator and save it"
			denom2 := denom1.			"Set second denominator to first"
			denom1 := int * denom1 + temp.		"Update first denominator"].
	"If fractional part is zero, return the first ratio"
	denom1 = 1
		ifTrue: ["Am i really an Integer?"
				^num1"Yes, return Integer result"]
		ifFalse: ["Otherwise return Fraction result"
				^Fraction numerator: num1 denominator: denom1]!

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

asFraction

	"First remove lowBits from mantissa.
	This can save a useless division and prevent gcd: cost"
	self reduce.

	^ biasedExponent >= 0
		ifTrue: [self shift: mantissa by: biasedExponent]
		ifFalse: [
			"Now avoid a costly GCD: algorihm.
			mantissa is odd and cannot be reduced by a power of two.
				mantissa / (1 bitShift: exponent negated)"
			^ Fraction numerator: mantissa denominator: (1 bitShift: biasedExponent negated)]!

asTrueFraction
	^self asFraction!

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

cos
	"Answer the cosine of the receiver."

	^(ArbitraryPrecisionFloatForTrigonometry
		mantissa: mantissa
		exponent: biasedExponent
		nBits: nBits) cos!

cosh
	"Answer the hyperbolic cosine of the receiver."

	| e x |
	self isZero ifTrue: [^self one].
	self exponent negated > nBits ifTrue: [^self one].
	x := self asArbitraryPrecisionFloatNumBits: nBits + 6.
	e := x exp.
	^e
		inPlaceAdd: e reciprocal;
		inPlaceTimesTwoPower: -1;
		asArbitraryPrecisionFloatNumBits: nBits!

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

	^nBits >= Float precision
		ifTrue: [Float zero generality + 1]
		ifFalse: [Float zero generality - 1]
!

greaterThanFraction: aFraction
	^self asTrueFraction > aFraction!

greaterThanInteger: anInteger
	^self asTrueFraction > anInteger!

hash
	"Hash is reimplemented because = is implemented."
	
	^ self asFraction hash!

inPlaceAbs
	mantissa := mantissa abs!

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
					["check for early truncation. beware, keep 2 bits for rounding"

					delta := self exponent - b exponent.
					delta - 2 > (nBits max: self numBitsInMantissa) 
						ifFalse: 
							[delta negated - 2 > (nBits max: b numBitsInMantissa) 
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
	h := ma highBit.
	mantissa := (1 bitShift: h + nBits) + ma quo: (self shift: mantissa by: 1).
	biasedExponent := biasedExponent negated - h - nBits + 1.
	self round
	
	"Implementation notes: if m is a power of 2, reciprocal is trivial.
	Else, we have 2^h > m >2^(h-1)
	thus 1 < 2^h/m < 2.
	thus 2^(n-1) < 2^(h+n-1)/m < 2^n
	We thus have to evaluate (2^(h+n-1)/m) rounded
	Tie is away from zero because there are always trailing bits (inexact op)
	(num/den) rounded is also ((num/den)+(sign/2)) truncated
	or (num*2)+(sign*den) quo: den*2
	That's finally what we evaluate"!

inPlaceSqrt
	"Replace the receiver by its square root."

	| guess guessSquared delta shift |
	self negative 
		ifTrue: 
			[^ FloatingPointException signal: 'sqrt undefined for number less than zero.'].
	self isZero ifTrue: [^self].

	shift := 2 * nBits - mantissa highBit.
	biasedExponent := biasedExponent - shift.
	biasedExponent odd
		ifTrue:
			[shift := shift + 1.
			biasedExponent := biasedExponent - 1].
	mantissa := mantissa bitShift: shift.
	guess := mantissa bitShift: mantissa highBit + 1 // 2.
	[
		guessSquared := guess * guess.
		delta := guessSquared - mantissa quo: (guess bitShift: 1).
		delta = 0 ] whileFalse:
			[ guess := guess - delta ].
	guessSquared = mantissa
		ifFalse:
			[(guessSquared - guess - mantissa) negative ifFalse: [guess := guess - 1]].
	mantissa := guess.
	biasedExponent := biasedExponent quo: 2.
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
					["check for early truncation. beware, keep 2 bits for rounding"

					delta := self exponent - b exponent.
					delta - 2 > (nBits max: self numBitsInMantissa) 
						ifFalse: 
							[delta negated - 2 > (nBits max: b numBitsInMantissa) 
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

powerExpansionArcTan: x precision: precBits
	"Evaluate the arc tangent of x by power series expansion."
	
	| count one sum term two x2 |
	x isZero ifTrue: [^x].
	one := x one.
	two := one timesTwoPower: 1.
	count := one.
	sum := x copy.
	term := x copy.
	x2 := x squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term / count.
	term exponent + precBits < sum exponent] whileFalse.
	^sum!

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

sin
	"Answer the sine of the receiver."

	^(ArbitraryPrecisionFloatForTrigonometry
		mantissa: mantissa
		exponent: biasedExponent
		nBits: nBits) sin!

sincos
	"Answer an Array with the sine and cosine of the receiver."

	^(ArbitraryPrecisionFloatForTrigonometry
		mantissa: mantissa
		exponent: biasedExponent
		nBits: nBits) sincos!

sinh
	"Answer the hyperbolic sine of the receiver."

	| e x |
	self isZero ifTrue: [^self].
	self exponent negated > nBits ifTrue: [^self].
	x := self asArbitraryPrecisionFloatNumBits: nBits + 6 + (0 max: self exponent negated).
	e := x exp.
	^e
		inPlaceSubtract: e reciprocal;
		inPlaceTimesTwoPower: -1;
		asArbitraryPrecisionFloatNumBits: nBits!

sqrt
	"Answer the square root of the receiver."

	| decimalPlaces n norm guess previousGuess one stopIteration |
	self < 0 
		ifTrue: 
			[^ FloatingPointException signal: 'undefined if less than zero.'].
	self isZero ifTrue: [^self].

	"use additional bits"
	decimalPlaces := nBits + 16.
	n := self asArbitraryPrecisionFloatNumBits: decimalPlaces.
	
	"constants"
	one := n one.

	"normalize n"
	norm := n exponent quo: 2.
	n := n timesTwoPower: norm * -2.

	"Initial guess for sqrt(1/n)"
	previousGuess := self class 
				mantissa: 3
				exponent: -2 - (n exponent quo: 2)
				nBits: decimalPlaces.
	guess := previousGuess copy.

	"use iterations x(k+1) := x*( 1 +  (1-x*x*n)/2) to guess sqrt(1/n)"
	
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

	guess isZero or: [stopIteration]] 
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

tan
	"Answer the tangent of the receiver."

	^(ArbitraryPrecisionFloatForTrigonometry
		mantissa: mantissa
		exponent: biasedExponent
		nBits: nBits) tan!

tanh
	"Answer the hyperbolic tangent of the receiver."

	| e x ep one |
	self isZero ifTrue: [^self].
	self exponent negated > nBits ifTrue: [^self].
	x := self asArbitraryPrecisionFloatNumBits: nBits + 6 + (0 max: self exponent negated).
	e := x exp.
	one :=x one.
	e inPlaceMultiplyBy: e.
	ep := e + one.
	^e
		inPlaceSubtract: one;
		inPlaceDivideBy: ep;
		asArbitraryPrecisionFloatNumBits: nBits!

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
!ArbitraryPrecisionFloat categoriesFor: #agm:!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arcCos!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arCosh!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arcSin!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arcTan!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arcTan:!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arSinh!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #arTanh!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #asApproximateFraction!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asArbitraryPrecisionFloatNumBits:!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asFloat!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asFraction!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #asTrueFraction!converting!public! !
!ArbitraryPrecisionFloat categoriesFor: #biasedExponent!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #coerce:!coercing!private! !
!ArbitraryPrecisionFloat categoriesFor: #copy!copying!public! !
!ArbitraryPrecisionFloat categoriesFor: #cos!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #cosh!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #digitCompare:!private! !
!ArbitraryPrecisionFloat categoriesFor: #exp!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #exponent!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #generality!coercing!private! !
!ArbitraryPrecisionFloat categoriesFor: #greaterThanFraction:!private! !
!ArbitraryPrecisionFloat categoriesFor: #greaterThanInteger:!private! !
!ArbitraryPrecisionFloat categoriesFor: #hash!comparing!public! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceAbs!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceAdd:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceAddNoRound:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceCopy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceDivideBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyBy:andAccumulate:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceMultiplyNoRoundBy:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceNegated!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceReciprocal!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceSqrt!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceSubtract:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceSubtractNoRound:!private! !
!ArbitraryPrecisionFloat categoriesFor: #inPlaceTimesTwoPower:!private! !
!ArbitraryPrecisionFloat categoriesFor: #isZero!public!testing! !
!ArbitraryPrecisionFloat categoriesFor: #ln!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #mantissa!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #mantissa:exponent:nBits:!initialize/release!public! !
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
!ArbitraryPrecisionFloat categoriesFor: #powerExpansionArcTan:precision:!private! !
!ArbitraryPrecisionFloat categoriesFor: #printOn:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #printOn:base:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #reciprocal!arithmetic!public! !
!ArbitraryPrecisionFloat categoriesFor: #reduce!private! !
!ArbitraryPrecisionFloat categoriesFor: #round!private! !
!ArbitraryPrecisionFloat categoriesFor: #setPrecisionTo:!initialize/release!public! !
!ArbitraryPrecisionFloat categoriesFor: #shift:by:!private! !
!ArbitraryPrecisionFloat categoriesFor: #sign!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #significandAsInteger!accessing!public! !
!ArbitraryPrecisionFloat categoriesFor: #sin!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #sincos!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #sinh!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #sqrt!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #storeOn:!printing!public! !
!ArbitraryPrecisionFloat categoriesFor: #tan!mathematical!public! !
!ArbitraryPrecisionFloat categoriesFor: #tanh!mathematical!public! !
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

ArbitraryPrecisionFloatForTrigonometry guid: (GUID fromString: '{3EEA7D3A-E96C-4EBD-8DEF-8A4B258F21DA}')!
ArbitraryPrecisionFloatForTrigonometry comment: ''!
!ArbitraryPrecisionFloatForTrigonometry categoriesForClass!Unclassified! !
!ArbitraryPrecisionFloatForTrigonometry methodsFor!

cos
	"Evaluate the cosine of the receiver"

	| halfPi quarterPi x neg |
	x := self moduloNegPiToPi.
	x inPlaceAbs.
	halfPi := pi timesTwoPower: -1.
	(neg := x > halfPi) ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi := halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			x := self sin: x]
		ifFalse: [x := self cos: x].
	neg ifTrue: [x inPlaceNegated].
	^x asArbitraryPrecisionFloatNumBits: nBits!

cos: x
	"Evaluate the cosine of x by recursive cos(2x) formula and power series expansion.
	Note that it is better to use this method with x <= pi/4."
	
	| one cos fraction power |
	x isZero ifTrue: [^x one].
	power := ((nBits bitShift: -1) + x exponent max: 0) highBit.
	fraction := x timesTwoPower: power negated.
	cos := self powerExpansionCos: fraction precision: nBits + (1 bitShift: 1 + power).
	one := x one.
	power timesRepeat:
		["Evaluate cos(2x)=2 cos(x)^2-1"
		cos inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1; inPlaceSubtract: one].
	^cos!

moduloNegPiToPi
	"answer a copy of the receiver modulo 2*pi, with doubled precision"

	| x quo twoPi |
	x := (ArbitraryPrecisionFloat
		mantissa: mantissa abs
		exponent: biasedExponent
		nBits: nBits * 2 + 2).
	pi := x pi.
	twoPi := pi timesTwoPower: 1.
	x > pi ifTrue:
		[quo := x + pi quo: twoPi.
		quo highBit * 2 > nBits ifTrue:
			[x := (ArbitraryPrecisionFloat
				mantissa: mantissa abs
				exponent: biasedExponent
				nBits: nBits * 3 // 2 + quo highBit + 2).
			pi := x pi.
			twoPi := pi timesTwoPower: 1.
			quo := x + pi quo: twoPi].
		x inPlaceSubtract: twoPi * quo].
	self negative ifTrue: [x inPlaceNegated].
	^x asArbitraryPrecisionFloatNumBits: nBits * 2!

powerExpansionCos: x precision: precBits
	"Evaluate the cosine of x by power series expansion."
	
	| count one sum term two x2 |
	x isZero ifTrue: [^x one].
	one := x one.
	two := one timesTwoPower: 1.
	count := one copy.
	sum := one copy.
	term := one copy.
	x2 := x squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum!

powerExpansionSin: x precision: precBits
	"Evaluate the sine of x by power series expansion."
	
	| count one sum term two x2 |
	x isZero ifTrue: [^x].
	one := x one.
	two := one timesTwoPower: 1.
	count := two copy.
	sum := x copy.
	term := x copy.
	x2 := x squared.
	
	[term inPlaceMultiplyBy: x2.
	term inPlaceDivideBy: count * (count + one).
	term inPlaceNegated.
	count inPlaceAdd: two.
	sum inPlaceAdd: term.
	term exponent + precBits < sum exponent] whileFalse.
	^sum!

powerExpansionSinCos: x precision: precBits
	"Evaluate the sine and cosine of x by power series expansion."
	
	| count one sin cos term |
	one := x one.
	count := one copy.
	cos := one copy.
	sin := x copy.
	term := x copy.
	
	[count inPlaceAdd: one.
	term
		inPlaceMultiplyBy: x;
		inPlaceDivideBy: count;
		inPlaceNegated.
	cos inPlaceAdd: term.

	count inPlaceAdd: one.
	term
		inPlaceMultiplyBy: x;
		inPlaceDivideBy: count.
	sin inPlaceAdd: term.
	
	term exponent + precBits < sin exponent] whileFalse.
	^Array with: sin with: cos!

sin
	"Evaluate the sine of the receiver"

	| halfPi quarterPi x neg |
	x := self moduloNegPiToPi.
	neg := x negative.
	x inPlaceAbs.
	halfPi := pi timesTwoPower: -1.
	x > halfPi ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi := halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			x := self cos: x]
		ifFalse: [x := self sin: x].
	neg ifTrue: [x inPlaceNegated].
	^x asArbitraryPrecisionFloatNumBits: nBits!

sin: x
	"Evaluate the sine of x by sin(5x) formula and power series expansion."
	
	| sin sin2 sin4 fifth five |
	x isZero ifTrue: [^x zero].
	five := 5 asArbitraryPrecisionFloatNumBits: x numBits.
	fifth := x / five.
	sin := self powerExpansionSin: fifth precision: nBits + 8.
	sin2 := sin squared.
	sin2 inPlaceTimesTwoPower: 2.
	sin4 := sin2 squared.
	sin2 inPlaceMultiplyBy: five.
	^sin4
		inPlaceSubtract: sin2;
		inPlaceAdd: five;
		inPlaceMultiplyBy: sin;
		yourself!

sincos
	"Evaluate the sine and cosine of the receiver"

	| halfPi quarterPi x sincos sinneg cosneg |
	x := self moduloNegPiToPi.
	sinneg := x negative.
	x inPlaceAbs.
	halfPi := pi timesTwoPower: -1.
	(cosneg := x > halfPi) ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi := halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			sincos := (self sincos: x) reverse]
		ifFalse:
			[sincos := self sincos: x].
	sinneg ifTrue: [sincos first inPlaceNegated].
	cosneg ifTrue: [sincos last inPlaceNegated].
	^sincos collect: [:e | e asArbitraryPrecisionFloatNumBits: nBits]!

sincos: x
	"Evaluate the sine and cosine of x by recursive sin(2x) and cos(2x) formula and power series expansion.
	Note that it is better to use this method with x <= pi/4."
	
	| one sin cos sincos fraction power |
	x isZero ifTrue: [^Array with: x zero with: x one].
	power := ((nBits bitShift: -1) + x exponent max: 0) highBit.
	fraction := x timesTwoPower: power negated.
	sincos := self powerExpansionSinCos: fraction precision: nBits + (1 bitShift: 1 + power).
	sin := sincos first.
	cos := sincos last.
	one := x one.
	power timesRepeat:
		["Evaluate sin(2x)=2 sin(x) cos(x)"
		sin inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1.
		"Evaluate cos(2x)=2 cos(x)^2-1"
		cos inPlaceMultiplyBy: cos; inPlaceTimesTwoPower: 1; inPlaceSubtract: one].
	^sincos!

tan
	"Evaluate the tangent of the receiver"

	| halfPi quarterPi x sincos sinneg cosneg |
	nBits := nBits + 6.
	x := self moduloNegPiToPi.
	sinneg := x negative.
	x inPlaceAbs.
	halfPi := pi timesTwoPower: -1.
	(cosneg := x > halfPi) ifTrue: [x inPlaceSubtract: pi; inPlaceNegated].
	quarterPi := halfPi timesTwoPower: -1.
	x > quarterPi
		ifTrue:
			[x inPlaceSubtract: halfPi; inPlaceNegated.
			sincos := (self sincos: x) reverse]
		ifFalse:
			[sincos := self sincos: x].
	nBits := nBits - 6.
	sinneg ifTrue: [sincos first inPlaceNegated].
	cosneg ifTrue: [sincos last inPlaceNegated].
	sincos first inPlaceDivideBy: sincos last.
	^sincos first asArbitraryPrecisionFloatNumBits: nBits! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #cos!mathematical!public! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #cos:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #moduloNegPiToPi!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #powerExpansionCos:precision:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #powerExpansionSin:precision:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #powerExpansionSinCos:precision:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #sin!mathematical!public! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #sin:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #sincos!mathematical!public! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #sincos:!private! !
!ArbitraryPrecisionFloatForTrigonometry categoriesFor: #tan!mathematical!public! !

"Binary Globals"!
