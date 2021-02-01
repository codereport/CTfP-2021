Extension { #name : #Object }

{ #category : #'*CTfP' }
Object >> <> aBlock [
	^ [: x | self value: (aBlock value: x) ]
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
