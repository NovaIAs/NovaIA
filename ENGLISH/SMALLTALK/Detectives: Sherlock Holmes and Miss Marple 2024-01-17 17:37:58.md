```smalltalk
Object subclass: Detective [
    "A detective is an agent that can find things."

    instanceVariableNames: 'location'

    classVariableNames: ''

    category: 'Agents'

    initialize: aLocation [
        "Create a new detective at the given location."

        super initialize.
        location := aLocation.
    ]

    find: aThing [
        "Find the given thing.

        If the thing is at the detective's current location, return it.
        Otherwise, move to the thing's location and try again."

        | thing |
        thing := self findAt: location.
        thing isNil
            ifTrue: [
                self moveTo: aThing location.
                self find: aThing
            ]
            ifFalse: [
                thing
            ].
    ]

    findAt: aLocation [
        "Find the given thing at the given location.

        This method is overridden by subclasses to provide different
        find algorithms."

        "SubclassResponsibility"
    ]

    moveTo: aLocation [
        "Move to the given location."

        location := aLocation.
    ]
]

Detective subclass: SherlockHolmes [
    "Sherlock Holmes is a detective who uses his keen observational skills
        to find things."

    classVariableNames: ''

    category: 'Agents'

    findAt: aLocation [
        "Find the given thing at the given location.

        Sherlock Holmes uses his keen observational skills to find things.
        He can see things that other detectives cannot."

        | thing |
        thing := super findAt: aLocation.
        thing isNil
            ifTrue: [
                self useKeenObservationalSkillsToFind: aThing
            ]
            ifFalse: [
                thing
            ].
    ]

    useKeenObservationalSkillsToFind: aThing [
        "Use Sherlock Holmes' keen observational skills to find the given thing."

        | clues |
        clues := self observeLocation.
        self analyzeClues: clues for: aThing.
    ]

    observeLocation [
        "Observe the current location and return a list of clues."

        | clues |
        clues := List new.
        clues add: self seeFootprints.
        clues add: self seeBloodstains.
        clues add: self hearGunshots.
        clues.
    ]

    seeFootprints [
        "Return true if there are footprints at the current location."

        Random boolean.
    ]

    seeBloodstains [
        "Return true if there are bloodstains at the current location."

        Random boolean.
    ]

    hearGunshots [
        "Return true if there are gunshots at the current location."

        Random boolean.
    ]

    analyzeClues: clues for: aThing [
        "Analyze the given clues to find the given thing.

        This method is overridden by subclasses to provide different
        analysis algorithms."

        "SubclassResponsibility"
    ]
]

Detective subclass: MissMarple [
    "Miss Marple is a detective who uses her knowledge of human nature
        to find things."

    classVariableNames: ''

    category: 'Agents'

    findAt: aLocation [
        "Find the given thing at the given location.

        Miss Marple uses her knowledge of human nature to find things.
        She can understand people's motives and see patterns that
        other detectives cannot."

        | thing |
        thing := super findAt: aLocation.
        thing isNil
            ifTrue: [
                self useKnowledgeOfHumanNatureToFind: aThing
            ]
            ifFalse: [
                thing
            ].
    ]

    useKnowledgeOfHumanNatureToFind: aThing [
        "Use Miss Marple's knowledge of human nature to find the given thing."

        | suspects |
        suspects := self interviewPeopleAtLocation.
        self analyzeSuspects: suspects for: aThing.
    ]

    interviewPeopleAtLocation [
        "Interview the people at the current location and return a list of suspects."

        | suspects |
        suspects := List new.
        suspects addAll: self talkToNeighbors.
        suspects addAll: self talkToShopkeepers.
        suspects addAll: self talkToPoliceOfficers.
        suspects.
    ]

    talkToNeighbors [
        "Talk to the neighbors at the current location and return a list of suspects."

        | suspects |
        suspects := List new.
        suspects addAll: self askAboutStrangeBehavior.
        suspects addAll: self askAboutRecentVisitors.
        suspects.
    ]

    askAboutStrangeBehavior [
        "Ask the neighbors about any strange behavior they have seen."

        | suspects |
        suspects := List new.
        suspects addAll: self askAboutPeopleComingAndGoing.
        suspects addAll: self askAboutPeopleLurkingAround.
        suspects.
    ]

    askAboutPeopleComingAndGoing [
        "Ask the neighbors about any people they have seen coming and going."

        | suspects |
        suspects := List new.
        suspects addAll: self askAboutPeopleMovingIn.
        suspects addAll: self askAboutPeopleMovingOut.
        suspects.
    ]

    askAboutPeopleLurkingAround [
        "Ask the neighbors about any people they have seen lurking around."

        | suspects |
        suspects := List new.
        suspects addAll: self askAboutPeopleWatchingHouses.
        suspects addAll: self askAboutPeopleFollowingPeople.
        suspects.
    ]

    askAboutRecentVisitors [
        "Ask the neighbors about any recent visitors they have seen."

        | suspects |
        suspects := List new.
        suspects addAll: self askAboutPeopleVisitingNeighbors