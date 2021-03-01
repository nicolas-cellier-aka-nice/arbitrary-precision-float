| package |
package := Package name: 'NumberParserTests'.
package paxVersion: 1;
	basicComment: 'Hold the tests for NumberParser'.


package classNames
	add: #FORTRANNumberParserTest;
	add: #SqNumberParserTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\..\Core\Object Arts\Dolphin\System\Random\Dolphin Random Stream'
	'NumberParser'
	'..\..\..\Core\Contributions\Camp Smalltalk\SUnit\SUnit').

package!

"Class Definitions"!

TestCase subclass: #FORTRANNumberParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #SqNumberParserTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

FORTRANNumberParserTest guid: (GUID fromString: '{890cfc1f-59bf-4468-9b50-eda60a66f94d}')!
FORTRANNumberParserTest comment: ''!
!FORTRANNumberParserTest categoriesForClass!Unclassified! !
!FORTRANNumberParserTest methodsFor!

testFloat
	| rs i |
	rs := ReadStream on: '712.'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 712.0.

	rs := ReadStream on: '-23.5'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = -23.5.

	rs := ReadStream on: '+.28E2'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 28.0.

	rs := ReadStream on: '+.28D+2'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 28.0.

	rs := ReadStream on: '125.E-3'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 0.125.
!

testInteger
	| rs i |
	rs := ReadStream on: '712'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 712.

	rs := ReadStream on: '-235'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = -235.

	rs := ReadStream on: '+28'.
	i := FORTRANNumberParser parse: rs.
	self assert: i = 28.
! !
!FORTRANNumberParserTest categoriesFor: #testFloat!public! !
!FORTRANNumberParserTest categoriesFor: #testInteger!public! !

SqNumberParserTest guid: (GUID fromString: '{bd41de41-0235-48a7-896c-df79ca415ea5}')!
SqNumberParserTest comment: ''!
!SqNumberParserTest categoriesForClass!Unclassified! !
!SqNumberParserTest methodsFor!

testFloatFromStreamAsNumber
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '10r-12.3456' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -12.3456 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2e2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs upToEnd = 'e2'.

	rs := '10r-12.3456d2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456q2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '-12.3456q2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: -1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '12.3456q2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1234.56 = aFloat.
	self assert: rs atEnd.

	rs := '12.3456z2' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 12.3456 = aFloat.
	self assert: rs upToEnd = 'z2'.
!

testFloatFromStreamWithExponent
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '1.0e-14' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs atEnd.

	rs := '1.0e-14 1' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs upToEnd = ' 1'.

	rs := '1.0e-14eee' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0e-14 = aFloat.
	self assert: rs upToEnd = 'eee'.

	rs := '1.0e14e10' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0e14 = aFloat.
	self assert: rs upToEnd = 'e10'.

	rs := '1.0e+14e' readStream. "Plus sign is not parseable"
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0 = aFloat.
	self assert: rs upToEnd = 'e+14e'.

	rs := '1.0e' readStream.
	aFloat := SqueakNumberParser parse: rs.
	self assert: 1.0 = aFloat.
	self assert: rs upToEnd = 'e'.!

testFloatPrintString
	"self debug: #testFloatPrintString"
	
	| bytes f r |
	bytes := ByteArray new: 8.
	r := Random new seed: 1234567.
	100
		timesRepeat: [bytes dwordAtOffset: 4 put: (r next * 16r100000000) truncated.
			bytes dwordAtOffset: 0 put: (r next * 16r100000000) truncated.
			f := bytes doubleAtOffset: 0.
			#(10)
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f abs absPrintExactlyOn: str base: base.
						self assert: (SqueakNumberParser parse: str contents) = f]].
	"test big num near infinity"
	10
		timesRepeat: [bytes dwordAtOffset: 4 put: 16r7FE00000 + ((r next * 16r100000) truncated).
			bytes dwordAtOffset: 0 put: (r next * 16r100000000) truncated.
			f := bytes doubleAtOffset: 0.
			#(10)
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f abs absPrintExactlyOn: str base: base.
						self assert: (SqueakNumberParser parse: str contents) = f]].
	"test infinitesimal (gradual underflow)"
	10
		timesRepeat: [bytes dwordAtOffset: 4 put: 0 + ((r next * 16r100000) truncated).
			bytes dwordAtOffset: 0 put: (r next * 16r100000000) truncated.
			f := bytes doubleAtOffset: 0.
			#(10)
				do: [:base | | str |
						str := (String new: 64) writeStream.
						f negative ifTrue: [str nextPut: $-].
						str print: base; nextPut: $r.
						f abs absPrintExactlyOn: str base: base.
						self assert: (SqueakNumberParser parse: str contents) = f]].!

testFloatReadError
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := SqueakNumberParser parse: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = 'e'.
	
	rs := '1s' readStream.
	num := SqueakNumberParser parse: rs.
	self assert: 1 = num.
	self assert: rs upToEnd = 's'.

	rs := '1.' readStream.
	num := SqueakNumberParser parse: rs.
	self assert: 1 = num.
	self assert: num isInteger.
	self assert: rs upToEnd = '.'.
	
	rs := '' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.
	
	rs := 'foo' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.
	
	rs := '.e0' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.
	
	rs := '-.e0' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.
	
	rs := '--1' readStream.
	self should: [SqueakNumberParser parse: rs] raise: Error.!

testFloatReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| aNumber rs |
	aNumber := (2r10101 / 2r10000) asFloat timesTwoPower: 9.
	self assert: 672.0 = aNumber.
	self assert: (SqueakNumberParser parse: '2r1.0101e9') = (1.3125 * (2 raisedTo: 9)).
	rs := ReadStream on: '2r1.0101e9e9'.
	self assert: (SqueakNumberParser parse: rs) = 672.0.
	self assert: rs upToEnd = 'e9'
!

testIntegerReadFrom
	"Ensure remaining characters in a stream are not lost when parsing an integer."

	| rs i s |
	rs := ReadStream on: '123s could be confused with a ScaledDecimal'.
	i := SqueakNumberParser parse: rs.
	self assert: i == 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' = s.
	rs := ReadStream on: '123.s could be confused with a ScaledDecimal'.
	i := SqueakNumberParser parse: rs.
	self assert: i == 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' = s
!

testIntegerReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| aNumber rs |
	aNumber := 2 raisedTo: 26.
	self assert: (SqueakNumberParser parse: '2r1e26') = aNumber.
	rs := '2r1e26eee' readStream.
	self assert: (SqueakNumberParser parse: rs) = aNumber.
	self assert: rs upToEnd = 'eee'
! !
!SqNumberParserTest categoriesFor: #testFloatFromStreamAsNumber!public! !
!SqNumberParserTest categoriesFor: #testFloatFromStreamWithExponent!public! !
!SqNumberParserTest categoriesFor: #testFloatPrintString!public! !
!SqNumberParserTest categoriesFor: #testFloatReadError!public! !
!SqNumberParserTest categoriesFor: #testFloatReadWithRadix!public! !
!SqNumberParserTest categoriesFor: #testIntegerReadFrom!public! !
!SqNumberParserTest categoriesFor: #testIntegerReadWithRadix!public! !

"Binary Globals"!

