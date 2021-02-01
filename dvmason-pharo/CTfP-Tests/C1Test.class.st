Class {
	#name : #C1Test,
	#superclass : #TestCase,
	#category : #'CTfP-Tests'
}

{ #category : #tests }
C1Test >> testCompose [
	self assert: (#(1 4 8) verifyIdComposable: #sum).
	self assert: (#(1 4 8) verifyIdComposable: #reversed).

]

{ #category : #tests }
C1Test >> testId [
	self assert: (self id value: 42) equals: 42.
	self assert: (self id value: #(1 2 3)) equals: #(1 2 3).

]
