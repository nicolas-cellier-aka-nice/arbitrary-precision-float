| package |
package := Package name: 'ArbitraryPrecisionFloatTests'.
package paxVersion: 1;
	basicComment: 'Hold the tests for ArbitraryPrecisionFloat'.


package classNames
	add: #ArbitraryPrecisionFloatTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Core\Contributions\Camp Smalltalk\SUnit\SUnit').

package!

"Class Definitions"!

TestCase subclass: #ArbitraryPrecisionFloatTest
	instanceVariableNames: 'zero one two half minusOne minusTwo huge'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ArbitraryPrecisionFloatTest guid: (GUID fromString: '{642e6d3f-8269-423c-8a82-b8c013e2c79e}')!
ArbitraryPrecisionFloatTest comment: 'Test to check FloatingPoint numbers with arbitrary precision'!
!ArbitraryPrecisionFloatTest categoriesForClass!Unclassified! !
!ArbitraryPrecisionFloatTest methodsFor!

checkDoublePrecision: y forFunction: func nBits: n
	"Check that doubling the precision, then rounding would lead to the same result"
	
	| anArbitraryPrecisionFloat singlePrecisionResult |
	anArbitraryPrecisionFloat := y asArbitraryPrecisionFloatNumBits: n.
	singlePrecisionResult := anArbitraryPrecisionFloat perform: func.
	self checkThatEvaluatingFunction: func toDoublePrecisionOf: anArbitraryPrecisionFloat equals: singlePrecisionResult.
	^singlePrecisionResult!

checkDoublePrecisionSerie: serie forFunction: func 
	^self checkDoublePrecisionSerie: serie forFunction: func nBits: Float precision!

checkDoublePrecisionSerie: serie forFunction: func nBits: n
	serie do: [:y | self checkDoublePrecision: y forFunction: func nBits: n]!

checkDoublePrecisionSerieVsFloat: serie forFunction: func 
	^serie reject: [:y |
		| farb |
		farb := self checkDoublePrecision: y forFunction: func nBits: Float precision.
		[(y asFloat perform: func) = farb] on: ZeroDivide do: [false]]!

checkThatEvaluatingFunction: func toDoublePrecisionOf: anArbitraryPrecisionFloat equals: singlePrecisionResult
	"Check that doubling the precision, then rounding would lead to the same result"
	
	| n doublePrecision doublePrecisionResult lowBits |
	n := anArbitraryPrecisionFloat numBits.
	doublePrecision := anArbitraryPrecisionFloat asArbitraryPrecisionFloatNumBits: n * 2.
	doublePrecisionResult := doublePrecision perform: func.
	
	"Note: the test must be guarded against double rounding error condition.
	For example, suppose the single precision is 4 bits, double precision 8 bits.
	If exact result is 1.001 | 0111 | 1001...
	Then the nearest double is rounded to upper 1.001 | 1000
	Then the nearest single to the double is rounded to upper 1.010
	But the nearest single to the exact result should have been 1.001
	To avoid this, we have to check if the second rounding is an exact tie"
	doublePrecisionResult normalize.
	lowBits := doublePrecisionResult mantissa bitAnd: 1<<n-1.
	lowBits = (1<<(n-1))
		ifTrue:
			["double precision is ambiguous - retry with quadruple..."
			^self checkThatEvaluatingFunction: func toQuadruplePrecisionOf: anArbitraryPrecisionFloat equals: singlePrecisionResult].
	self assert: ((doublePrecisionResult asArbitraryPrecisionFloatNumBits: n)- singlePrecisionResult) isZero
	
!

checkThatEvaluatingFunction: func toQuadruplePrecisionOf: anArbitraryPrecisionFloat equals: singlePrecisionResult
	"Check that quadrupling the precision, then rounding would lead to the same result"
	
	| n quadruplePrecision quadruplePrecisionResult lowBits |
	n := anArbitraryPrecisionFloat numBits.
	quadruplePrecision := anArbitraryPrecisionFloat asArbitraryPrecisionFloatNumBits: n * 4.
	quadruplePrecisionResult := quadruplePrecision perform: func.
	
	"Guard against double rounding error condition (exact tie)"
	quadruplePrecisionResult normalize.
	lowBits := quadruplePrecisionResult mantissa bitAnd: 1<<(3*n)-1.
	lowBits = (1<<(3*n-1))
		ifTrue:
			["quadruple precision is ambiguous - give up..."
			^self].
	self assert: ((quadruplePrecisionResult asArbitraryPrecisionFloatNumBits: n)- singlePrecisionResult) isZero.!

hyperbolicSerie
	^#(-3.0e0  -0.1e0  0.0e0  1.0e-20  1.0e-10  0.99e0 1.0e0  2.5e0  3.0e0  10.25e0) , (Array with: (3/10) asFloat with: (22/7) asFloat)!

inverseTrigonometricSerie
	^((-20 to: 20) collect: [:e | (e / 20) asFloat]) , ((-6 to: 6) collect: [:e | (e / 7) asFloat])!

largeTrigonometricSerie
	^#(
		1.0e15 1.1e21 1.2e28 1.0e32 1.1e34 -1.23e51 1.345e67 1.777e151 1.211e308)!

setUp
	zero := 0 asArbitraryPrecisionFloatNumBits: Float precision.
	one := 1 asArbitraryPrecisionFloatNumBits: Float precision.
	two := 2 asArbitraryPrecisionFloatNumBits: Float precision.
	half := 1/2 asArbitraryPrecisionFloatNumBits: Float precision.
	minusOne := -1 asArbitraryPrecisionFloatNumBits: Float precision.
	minusTwo := -2 asArbitraryPrecisionFloatNumBits: Float precision.
	huge := (10 raisedTo: 100) asArbitraryPrecisionFloatNumBits: Float precision.!

testArcCos
	| badArcCos |
	badArcCos := self checkDoublePrecisionSerieVsFloat: self inverseTrigonometricSerie forFunction: #arcCos.
	badArcCos isEmpty ifFalse: [Transcript cr; show: 'bad arcCos for ' , badArcCos printString]!

testArcCosDomainError
	self should: [(2 asArbitraryPrecisionFloatNumBits: 24) arcCos] raise: Error.
	self should: [(-3 asArbitraryPrecisionFloatNumBits: 24) arcCos] raise: Error.!

testArCosh
	| serie |
	serie := (((1 to: 10) , #(1.0001e0 100.0e0 1000.0e0 1.0e20)) collect: [:e | e asFloat]).
	self checkDoublePrecisionSerie: serie forFunction: #arCosh!

testArCoshDomainError
	self should: [(1/2 asArbitraryPrecisionFloatNumBits: 24) arCosh] raise: Error!

testArcSin
	| badArcSin |
	badArcSin := self checkDoublePrecisionSerieVsFloat: self inverseTrigonometricSerie forFunction: #arcSin.
	badArcSin isEmpty ifFalse: [Transcript cr; show: 'bad arcSin for ' , badArcSin printString]!

testArcSinDomainError
	self should: [(2 asArbitraryPrecisionFloatNumBits: 24) arcSin] raise: Error.
	self should: [(-3 asArbitraryPrecisionFloatNumBits: 24) arcSin] raise: Error.!

testArcTan
	| badArcTan serie |
	serie := ((-50 to: 50) collect: [:e | (e / 10) asFloat]).
	badArcTan := self checkDoublePrecisionSerieVsFloat: serie forFunction: #arcTan.
	badArcTan isEmpty ifFalse: [Transcript cr; show: 'bad arcTan for ' , badArcTan printString]!

testArcTan2
	-5 to: 5 by: 4/10 do: [:y |
		| yf yd |
		yf := y asArbitraryPrecisionFloatNumBits: Float precision.
		yd := yf asArbitraryPrecisionFloatNumBits: Float precision * 2.
		-5 to: 5 by: 4/10 do: [:x |
			| xf xd  |
			xf := x asArbitraryPrecisionFloatNumBits: Float precision.
			xd := xf asArbitraryPrecisionFloatNumBits: Float precision * 2.
			self assert: ((yd arcTan: xd) asFloat - (yf arcTan: xf) asFloat) isZero]].!

testArSinh
	| serie |
	serie := (((-5 to: 10) , #(1.0e-20 1.0e-10  0.9999e0 1.0001e0 100.0e0 1000.0e0 1.0e20)) collect: [:e | e asFloat]).
	self checkDoublePrecisionSerie: serie forFunction: #arSinh!

testArTanh
	| serie |
	serie := ((-19 to: 19) collect: [:e | (e / 20) asFloat]) , ((-6 to: 6) collect: [:e | (e / 7) asFloat]) , #(1.0e-20 1.0e-10 0.99e0 0.9999e0 0.999999e0).
	self checkDoublePrecisionSerie: serie forFunction: #arTanh!

testArTanhDomainError
	self should: [(2 asArbitraryPrecisionFloatNumBits: 24) arTanh] raise: Error.
	self should: [(-3 asArbitraryPrecisionFloatNumBits: 24) arTanh] raise: Error.!

testAsFloat
	self assert: (half asArbitraryPrecisionFloatNumBits: Float precision) asFloat = 0.5e0.
	self assert: (half asArbitraryPrecisionFloatNumBits: Float precision * 2) asFloat = 0.5e0.!

testAsFloatWithUnderflow
	| fmin fminA |
	fmin := Float fmin.
	fminA := fmin asArbitraryPrecisionFloatNumBits: one numBits.
	-1022 - Float precision + 1 to: -1022 + 1 do: [:n |
		self assert: ((one timesTwoPower: n) + fminA) asFloat = ((1.0e0 timesTwoPower: n) + fmin)].!

testAsFloatWithUnderflowAndExcessPrecision
	| fmin expected shouldRoundUp shouldRoundDown tooSmall exactTie |
	fmin := Float fminDenormalized asArbitraryPrecisionFloatNumBits: Float precision * 2.

	shouldRoundUp := (fmin timesTwoPower: 1) + (fmin timesTwoPower: -1) + (fmin timesTwoPower: -1 - Float precision).
	expected := Float fminDenormalized * 3.
	self assert: shouldRoundUp asFloat = expected.
	self assert: shouldRoundUp negated asFloat = expected negated.

	shouldRoundDown := (fmin timesTwoPower: 1) + (fmin timesTwoPower: -1 - Float precision).
	expected := Float fminDenormalized * 2.
	self assert: shouldRoundDown asFloat = expected.
	self assert: shouldRoundDown negated asFloat = expected negated.

	tooSmall := (fmin negated timesTwoPower: -2).
	self assert: tooSmall asFloat isZero.
	self assert: tooSmall asFloat sign = -1.

	exactTie := (Float fminNormalized - Float fminDenormalized asArbitraryPrecisionFloatNumBits: Float precision * 2) + (fmin/2).
	expected := Float fminNormalized.
	self assert: exactTie asFloat = expected.
	self assert: exactTie negated asFloat = expected negated!

testAsMinimalDecimalFraction
	| emax emin leadingOne significands |
	significands := 0 to: 1<<10-1.
	leadingOne := 1<<10.
	emin := -14.
	emax := 15.
	
	"Test all normal finite half precision float"
	emin to: emax do: [:e | 
		significands do: [:s |
			| f |
			f := (leadingOne + s asArbitraryPrecisionFloatNumBits: 11) timesTwoPower: e - 10.
			self assert: (f asMinimalDecimalFraction asArbitraryPrecisionFloatNumBits: 11) = f]].
	
	"Test all subnormal finite half precision float"
	significands do: [:s |
		| f |
		f := (s asArbitraryPrecisionFloatNumBits: s highBit) timesTwoPower: emin - 10.
		self assert: (f asMinimalDecimalFraction asArbitraryPrecisionFloatNumBits: s highBit) = f].!

testCoercingDivide
	(Array with: 1/2 with: 0.5e0 with: 0.5s1) do: [:heteroHalf |
		self assert: one / heteroHalf = two.
		self assert: (one / heteroHalf) class = one class.
		self assert: (one / heteroHalf) numBits = one numBits.
		self assert: heteroHalf / one = half.
		self assert: (heteroHalf / one) class = one class.
		self assert: (heteroHalf / one) numBits = one numBits].

	self assert: one / 2 = half.
	self assert: (one / 2) class = one class.
	self assert: (one / 2) numBits = one numBits.
	self assert: -2 / two = minusOne.
	self assert: (-2 / two) class = two class.
	self assert: (-2 / two) numBits = two numBits.!

testCoercingEqual
	self assert: half = (1/2).
	self assert: (1/2) = half.
	self deny: half = (1/3).
	self deny: (1/3) = half.

	self assert: two = 2.
	self assert: -2 = minusTwo.
	self deny: -3 = two.
	self deny: two = 3.

	self assert: half = (0.5e0).
	self assert: (0.5e0) = half.
	self deny: half = (0.33e0).
	self deny: (0.33e0) = half.

	self assert: half = (0.5d0).
	self assert: (0.5d0) = half.
	self deny: half = (0.33d0).
	self deny: (0.33d0) = half.

	self assert: half = (0.5s1).
	self assert: (0.5s1) = half.
	self deny: half = (0.33s2).
	self deny: (0.33s2) = half.!

testCoercingLessThan
	self deny: half < (1/2).
	self assert: (1/3) < half.
	self assert: minusOne < (1/2).
	self deny: (1/3) < minusTwo.

	self assert: two < 3.
	self deny: two < 2.
	self deny: two < 1.
	self deny: two < -1.
	self assert:  minusTwo < -1.
	self assert:  minusTwo < 1.
	self deny: minusTwo < -2.
	self deny: minusTwo < -3.

	self deny: half < (0.5e0).
	self deny: half < (0.33e0).
	self assert: half < (0.66e0).
	self deny: (0.5e0) < half.
	self assert: (0.33e0) < half.
	self deny: (0.66e0) < half.

	self deny: half < (0.5d0).
	self deny: half < (0.33d0).
	self assert: half < (0.66d0).
	self deny: (0.5d0) < half.
	self assert: (0.33d0) < half.
	self deny: (0.66d0) < half.

	self deny: half < (0.5s1).
	self deny: half < (0.33s2).
	self assert: half < (0.66s2).
	self deny: (0.5s1) < half.
	self assert: (0.33s2) < half.
	self deny: (0.66s2) < half.!

testCoercingMultiply
	(Array with: 1/2 with: 0.5e0 with: 0.5s1) do: [:heteroHalf |
		self assert: two * heteroHalf = one.
		self assert: (two * heteroHalf) class = half class.
		self assert: (two * heteroHalf) numBits = half numBits.
		self assert: heteroHalf * two = one.
		self assert: (heteroHalf * two) class = half class.
		self assert: (heteroHalf * two) numBits = half numBits].

	self assert: minusOne * 2 = minusTwo.
	self assert: (minusOne * 2) class = minusOne class.
	self assert: (minusOne * 2) numBits = minusOne numBits.
	self assert: 2 * one = two.
	self assert: (2 * one) class = one class.
	self assert: (2 * one) numBits = one numBits.!

testCoercingSubtract
	(Array with: 1/2 with: 0.5e0 with: 0.5s1) do: [:heteroHalf |
		self assert: half - heteroHalf = zero.
		self assert: (half - heteroHalf) class = half class.
		self assert: (half - heteroHalf) numBits = half numBits.
		self assert: heteroHalf - half = zero.
		self assert: (heteroHalf - half) class = half class.
		self assert: (heteroHalf - half) numBits = half numBits].

	self assert: one - 1 = zero.
	self assert: (one - 1) class = minusOne class.
	self assert: (one - 1) numBits = minusOne numBits.
	self assert: -2 - minusTwo = zero.
	self assert: (-2 - minusTwo) class = minusTwo class.
	self assert: (-2 - minusTwo) numBits = minusTwo numBits.!

testCoercingSum
	(Array with: 1/2 with: 0.5e0 with: 0.5s1) do: [:heteroHalf |
		self assert: half + heteroHalf = one.
		self assert: (half + heteroHalf) class = half class.
		self assert: (half + heteroHalf) numBits = half numBits.
		self assert: heteroHalf + half = one.
		self assert: (heteroHalf + half) class = half class.
		self assert: (heteroHalf + half) numBits = half numBits].

	self assert: minusOne + 1 = zero.
	self assert: (minusOne + 1) class = minusOne class.
	self assert: (minusOne + 1) numBits = minusOne numBits.
	self assert: 2 + minusTwo = zero.
	self assert: (2 + minusTwo) class = minusTwo class.
	self assert: (2 + minusTwo) numBits = minusTwo numBits.!

testCos
	| badCos |
	badCos := self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #cos.
	badCos isEmpty ifFalse: [Transcript cr; show: 'bad cos for angles (degrees) ' , (badCos collect: [:i | i radiansToDegrees rounded]) printString]!

testCosh
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #cosh!

testDivide
	| serie |
	serie := #(1 2 3 5 6 7 9 10 11 12 19 243) , (Array
		with: (10 raisedTo: Float precision + 1)
		with: Float precision factorial
		with: Float pi).
	serie do: [:num |
		| nf na |
		nf := num asFloat.
		na := num asArbitraryPrecisionFloatNumBits: Float precision.
		serie do:[:den |
			| df da ff fa |
			df := den asFloat.
			da := den asArbitraryPrecisionFloatNumBits: Float precision.
			ff := nf / df.
			fa := na / da.
			self assert: ff = fa]].!

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

testErf
	| serie |
	serie := ((0 to: 10) , (1/10 to: 9/10 by: 1/10)) collect: [:e | e asFloat].
	self checkDoublePrecisionSerie: serie forFunction: #erf!

testExp
	| badExp serie |
	serie := ((-20 to: 20) collect: [:e |e asFloat]).
	badExp := self checkDoublePrecisionSerieVsFloat: serie forFunction: #exp.
	badExp isEmpty ifFalse: [Transcript cr; show: 'bad exp for ' , badExp printString]!

testExpLn
	self assert: (1 asArbitraryPrecisionFloatNumBits: Float precision) exp asFloat = 1 asFloat exp.

	self assert: (5 asArbitraryPrecisionFloatNumBits: Float precision) exp asFloat = 5 asFloat exp.
	self assert: (5 asArbitraryPrecisionFloatNumBits: Float precision) exp ln asFloat = 5 asFloat exp ln.!

testGreaterThan
	
	self assert: zero < one.
	self deny: one > two.
	self deny: two > huge.
	self deny: minusOne > one.
	self deny: minusTwo > minusOne.
	self deny: minusTwo > huge.
	
	self assert: huge > one.
	self assert: huge > zero.
	self assert: huge > minusOne.
	self assert: one > minusOne.
	self assert: minusOne > minusTwo.!

testHalfPi
	| pi halfPi |
	pi := one pi.
	halfPi := one halfPi.
	self assert: halfPi + halfPi = pi!

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
							self assert: new = ref.
							new := f1 perform: op
										with: (f2 asArbitraryPrecisionFloatNumBits: Float precision).
							self assert: new = ref.
							new := (f1 asArbitraryPrecisionFloatNumBits: Float precision) perform: op
										with: f2.
							self assert: new = ref]]]!

testIEEEArithmeticVersusIntegerAndFraction
	"check that results are the same as IEEE 754 accelerated hardware
	WARNING: this cannot be the case for denormalized numbers (gradual underflow)
	because our exponent is unlimited"

	| floats ops ref new intAndFractions |
	floats := #(1.0e0 2.0e0 3.0e0 5.0e0 10.0e0) 
				, (#(52 53 54 -52 -53 -54) collect: [:e | 1.0e0 timesTwoPower: e]) 
					, #(0.5e0 0.25e0 1.0e60 0.1e0 1.1e-30 1.0e-60) copyWith: Float pi.
	intAndFractions := #(1 3 5 10 12345678901234567890 -1 -22 -3) copyWith: 7/9.
	intAndFractions := intAndFractions , (intAndFractions collect: [:e | e reciprocal]).
	ops := #(#+ #- #* #/ #= #< #>).
	ops do: 
			[:op | 
			floats do: 
					[:f1 | 
					intAndFractions do: 
							[:f2 | 
							ref := f1 perform: op with: f2 asFloat.
							new := (f1 asArbitraryPrecisionFloatNumBits: Float precision) perform: op
										with: (f2 asArbitraryPrecisionFloatNumBits: Float precision).
							self assert: new = ref.
							new := f1 perform: op
										with: (f2 asArbitraryPrecisionFloatNumBits: Float precision).
							self assert: new = ref]]].
	ops := 1/10 = 0.1
		ifTrue: [#(#+ #- #* #/)]
		ifFalse: [#(#+ #- #* #/ #= #< #>)]. "BEWARE: ArbitraryPrecisionFloat compare exactly, Float don't unless patched"
	ops do: 
			[:op | 
			floats do: 
					[:f1 | 
					intAndFractions do: 
							[:f2 | 
							ref := f1 perform: op with: f2.
							new := (f1 asArbitraryPrecisionFloatNumBits: Float precision) perform: op
										with: f2.
							self assert: new = ref]]]!

testInfinityAndNaN
	| inf nan |
	inf := Float infinity.
	nan := Float nan.
	self assert: inf + two equals: inf.
	self assert: half + inf negated equals: inf negated.	
	self assert: (nan + minusOne)  isNaN .
	self assert: inf - huge equals: inf.
	self assert: half - inf equals: inf negated.
	self assert: minusTwo - inf negated equals: inf.
	self assert: (one - nan) isNaN.
	self assert: inf * two equals: inf.
	self assert: minusOne * inf equals: inf negated.
	self assert: inf negated * minusOne equals: inf.
	self assert: (huge * nan) isNaN.
	self assert: inf negated / minusTwo equals: inf.
	self assert: zero / inf negated equals: 0.	
	self assert: one / inf equals: 0.
		"self should: [inf / zero] raise: ZeroDivide."  "Not true in Dolphin"	
	self assert: (nan  / two) isNaN.	
	self assert: (inf raisedTo: huge) equals: inf.
	self assert: (huge raisedTo: inf) equals: inf.
	self assert: (nan raisedTo: two) isNaN.
	self assert: (two raisedTo: nan) isNaN.
	self deny: nan <= one.
	self deny: zero >= nan.
	self assert: one < inf.
	self assert: zero ~= nan.
	self deny: nan = one.!

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

testLn
	| badLn serie |
	serie := ((1 to: 100) collect: [:e |e asFloat]).
	badLn := self checkDoublePrecisionSerieVsFloat: serie forFunction: #ln.
	badLn isEmpty ifFalse: [Transcript cr; show: 'bad ln for ' , badLn printString]!

testLnDomainError
	self should: [(-2 asArbitraryPrecisionFloatNumBits: 24) ln] raise: Error.!

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

testNegative
	
	self deny: zero negative.
	self deny: two negative.
	self assert: minusTwo negative.!

testPi
	"check computation of pi"

	self assert: (1 asArbitraryPrecisionFloatNumBits: Float precision) pi = Float pi.!

testPositive
	
	self assert: zero positive.
	self assert: one positive.
	self deny: minusOne positive.!

testPrintAndEvaluate
	| emax emin leadingOne significands |
	significands := 0 to: 1<<10-1.
	leadingOne := 1<<10.
	emin := -14.
	emax := 15.
	
	"Test all normal finite half precision float"
	emin to: emax do: [:e | 
		significands do: [:s |
			| f |
			f := (leadingOne + s asArbitraryPrecisionFloatNumBits: 11) timesTwoPower: e - 10.
			self assert: (Compiler evaluate: f storeString) = f.
			self assert: (Compiler evaluate: f printString) = f.]].
	
	"Test all subnormal finite half precision float"
	significands do: [:s |
		| f |
		f := (s asArbitraryPrecisionFloatNumBits: s highBit) timesTwoPower: emin - 10.
		self assert: (Compiler evaluate: f storeString) = f.
		self assert: (Compiler evaluate: f printString) = f].!

testRaisedToNegativeInteger
	| n |
	n := 11.
	1 to: 1<<n-1 do: [:i |
		self assert: ((i asArbitraryPrecisionFloatNumBits: n) raisedToInteger: -49)
			= ((i raisedToInteger: -49) asArbitraryPrecisionFloatNumBits: n) ].!

testRaisedToPositiveInteger
	| n |
	n := 11.
	1 to: 1<<n-1 do: [:i |
		self assert: ((i asArbitraryPrecisionFloatNumBits: n) raisedToInteger: 49)
			= ((i raisedToInteger: 49) asArbitraryPrecisionFloatNumBits: n) ].!

testReciprocal
	| b |
	b := 1 << (Float precision - 1).
	1 to: 10000 do: [:i |
		| a |
		a := i asArbitraryPrecisionFloatNumBits: Float precision.
		self assert: a reciprocal = i asFloat reciprocal.
		self assert: (a+b) reciprocal = (i+b) asFloat reciprocal.
		self assert: a negated reciprocal = i asFloat negated reciprocal.]!

testRoundToNearestEven
	"Check that IEEE default rounding mode is honoured,
	that is rounding to nearest even"
		
	self assert: ((one timesTwoPower: 52)+(0+(1/4))) asFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(1/2))) asFraction = ((1 bitShift: 52)+0).
	self assert: ((one timesTwoPower: 52)+(0+(3/4))) asFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/4))) asFraction = ((1 bitShift: 52)+1).
	self assert: ((one timesTwoPower: 52)+(1+(1/2))) asFraction = ((1 bitShift: 52)+2).
	self assert: ((one timesTwoPower: 52)+(1+(3/4))) asFraction = ((1 bitShift: 52)+2).!

testRoundToNearestEvenAgainstIEEEDouble
	"Check that IEEE default rounding mode is honoured"

	#(1 2 3 5 6 7) do: 
			[:i | 
			self assert: ((one timesTwoPower: 52) + (i / 4)) asFraction 
						= ((1 asFloat timesTwoPower: 52) + (i / 4)) asFraction.
			self assert: ((one timesTwoPower: 52) - (i / 4)) asFraction 
						= ((1 asFloat timesTwoPower: 52) - (i / 4)) asFraction]!

testSin
	| badSin |
	badSin := self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #sin.
	badSin isEmpty ifFalse: [Transcript cr; show: 'bad sin for angles (degrees) ' , (badSin collect: [:i | i radiansToDegrees rounded]) printString]!

testSincos
	self trigonometricSerie do: [:aFloat |
		| x sc s c |
		x := aFloat asArbitraryPrecisionFloatNumBits: 53.
		sc := x sincos.
		s := x sin.
		c := x cos.
		self assert: sc size = 2.

		self assert: sc first = s.
		self assert: sc last = c]!

testSinCos
	-720 to: 720 by: 24 do: [:degrees |
		| arb |
		arb := degrees asArbitraryPrecisionFloatNumBits: 53.
		arb := arb * arb pi / 180.
		self assert: arb sincos = (Array with: arb sin with: arb cos)]!

testSinh
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #sinh!

testSqrt
	| badSqrt serie |
	"knowing that (10**3) < (2**10), 100 bits are enough for representing 10**30 exactly"
	self assert: ((10 raisedTo: 30) asArbitraryPrecisionFloatNumBits: 100) sqrt = (10 raisedTo: 15).

	serie := ((0 to: 20) collect: [:e | e asFloat]) , ((2 to: 20) collect: [:e | e reciprocal asFloat]).
	badSqrt := self checkDoublePrecisionSerieVsFloat: serie forFunction: #sqrt.
	badSqrt isEmpty ifFalse: [Transcript cr; show: 'bad sqrt for ' , badSqrt printString]!

testSqrtDomainError
	self should: [(-2 asArbitraryPrecisionFloatNumBits: 24) sqrt] raise: Error.!

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

testTan
	| badTan |
	badTan := self checkDoublePrecisionSerieVsFloat: self trigonometricSerie forFunction: #tan.
	badTan isEmpty ifFalse: [Transcript cr; show: 'bad tan for angles (degrees) ' , (badTan collect: [:i | i radiansToDegrees rounded]) printString]!

testTanh
	self checkDoublePrecisionSerie: self hyperbolicSerie forFunction: #tanh!

testVeryLargeCos
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #cos.!

testVeryLargeSin
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #sin.!

testVeryLargeTan
	self checkDoublePrecisionSerie: self largeTrigonometricSerie forFunction: #tan.!

testZeroOne
	"check computation of pi"

	self assert: (312 asArbitraryPrecisionFloatNumBits: 53) one = 1.
	self assert: (231 asArbitraryPrecisionFloatNumBits: 24) zero isZero.

	self assert: (213 asArbitraryPrecisionFloatNumBits: 24) one asInteger = 1.
	self assert: (123 asArbitraryPrecisionFloatNumBits: 53) zero asInteger isZero.!

trigonometricSerie
	^((-720 to: 720) collect: [:i | i asFloat degreesToRadians])! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkDoublePrecision:forFunction:nBits:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkDoublePrecisionSerie:forFunction:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkDoublePrecisionSerie:forFunction:nBits:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkDoublePrecisionSerieVsFloat:forFunction:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkThatEvaluatingFunction:toDoublePrecisionOf:equals:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #checkThatEvaluatingFunction:toQuadruplePrecisionOf:equals:!private! !
!ArbitraryPrecisionFloatTest categoriesFor: #hyperbolicSerie!private!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #inverseTrigonometricSerie!private!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #largeTrigonometricSerie!private!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #setUp!public!setup! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcCos!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcCosDomainError!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArCosh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArCoshDomainError!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcSin!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcSinDomainError!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcTan!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArcTan2!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArSinh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArTanh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testArTanhDomainError!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testAsFloat!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testAsFloatWithUnderflow!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testAsFloatWithUnderflowAndExcessPrecision!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testAsMinimalDecimalFraction!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingDivide!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingEqual!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingLessThan!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingMultiply!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingSubtract!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCoercingSum!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCos!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testCosh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testDivide!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testEqual!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testErf!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testExp!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testExpLn!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testGreaterThan!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testHalfPi!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testIEEEArithmeticVersusFloat!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testIEEEArithmeticVersusIntegerAndFraction!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testInfinityAndNaN!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testIsZero!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLessThan!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLn!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLnDomainError!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testLnVsFloat!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testMultiply!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testNegated!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testNegative!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testPi!public!testing-constants! !
!ArbitraryPrecisionFloatTest categoriesFor: #testPositive!public!testing-compare! !
!ArbitraryPrecisionFloatTest categoriesFor: #testPrintAndEvaluate!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRaisedToNegativeInteger!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRaisedToPositiveInteger!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testReciprocal!public! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRoundToNearestEven!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testRoundToNearestEvenAgainstIEEEDouble!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSin!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSincos!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSinCos!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSinh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSqrt!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSqrtDomainError!public!testing-functions! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSubtract!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testSum!public!testing-arithmetic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testTan!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testTanh!public!testing-hyperbolic! !
!ArbitraryPrecisionFloatTest categoriesFor: #testVeryLargeCos!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testVeryLargeSin!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testVeryLargeTan!public!testing-trigonometry! !
!ArbitraryPrecisionFloatTest categoriesFor: #testZeroOne!public!testing-constants! !
!ArbitraryPrecisionFloatTest categoriesFor: #trigonometricSerie!private!testing-trigonometry! !

"Binary Globals"!

