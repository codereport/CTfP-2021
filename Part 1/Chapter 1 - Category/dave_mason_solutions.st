Extension { #name : #Object }

{ #category : #'*CTfP' }
Object >> <> aBlock [
	^ [: x | self value: (aBlock value: x) ]
]

{ #category : #'*CTfP' }
Object >> id [
	^ [: x | x ]
]

{ #category : #'*CTfP' }
Object >> verifyIdComposable: aBlock [
	| simple composed |
	simple := aBlock value: self.
	composed := self id <> aBlock value: self.
	simple = composed ifFalse: [ ^ false ].
	composed := aBlock <> self id value: self.
	^ simple = composed
]

"Tests"

Class {
	#name : #C1Test,
	#superclass : #TestCase,
	#category : #'CTfP-Tests'
}

{ #category : #tests }
C1Test >> testCompose [
	self assert: (#(1 4 8) verifyIdComposable: #sum).
	self assert: (#(1 4 8) verifyIdComposable: #reversed).
	self assert: (#abs <> #negated value: 42) equals: 42.
	self assert: (#negated <> #abs value: -42) equals: -42.

]

{ #category : #tests }
C1Test >> testId [
	self assert: (self id value: 42) equals: 42.
	self assert: (self id value: #(1 2 3)) equals: #(1 2 3).

]
