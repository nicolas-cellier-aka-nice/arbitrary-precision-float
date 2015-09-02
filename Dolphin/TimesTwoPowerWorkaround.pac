| package |
package := Package name: 'TimesTwoPowerWorkaround'.
package paxVersion: 1;
basicComment: 'Float>>timesTwoPower: is broken because ldexp is broken in Windows.
Indeed, ldexp(2.75,-1074) is incorrectly rounded (it is truncated), and does not match 2.75*ldexp(1.0,-1074).
See http://stackoverflow.com/questions/32150888/should-ldexp-round-correctly
This package installs a workaround.
Note: Float emin is not used because -1021 is such a troubling answer...
Since we expect -1022, the magic constants have been hardcoded.'.


package methodNames
	add: #Float -> #ldexp:;
	add: #Float -> #timesTwoPower:;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Dolphin Smalltalk 6.0\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!


"Global Aliases"!


"Loose Methods"!

!Float methodsFor!

ldexp: aSmallInteger
	"Answer the receiver times two to the power of the argument, aSmallInteger."

	^CRTLibrary default ldexp: self exp: aSmallInteger!

timesTwoPower: aNumber
	"Answer the receiver times two to the power of the argument, aNumber."

	| int exp |
	int := aNumber asInteger.
	int class == SmallInteger
		ifFalse: [^self * (2 raisedToInteger: int) asFloat].
	exp := self exponent + int .
	exp >= -1022 ifTrue: [^self ldexp: int].
	exp < -1075 ifTrue: [^self ldexp: int].
	^(self ldexp: int + 54) * (1.0 ldexp: -54)! !
!Float categoriesFor: #ldexp:!mathematical!private! !
!Float categoriesFor: #timesTwoPower:!mathematical!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

"Binary Globals"!

