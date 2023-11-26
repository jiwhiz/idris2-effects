module Examples.Door

import Effects

data DoorState = Closed | Open
data DoorInfo : DoorState -> Type where
     DI : DoorInfo s

data Door : Effect where
     OpenDoor : sig Door () (DoorInfo Closed) (DoorInfo Open)
     CloseDoor : sig Door () (DoorInfo Open) (DoorInfo Closed)
     Knock : sig Door () (DoorInfo Closed)

DOOR : DoorState -> EFFECT
DOOR t = MkEff (DoorInfo t) Door

openDoor : Eff () [DOOR Closed] [DOOR Open]
openDoor = call OpenDoor {prf=Here}

closeDoor : Eff () [DOOR Open] [DOOR Closed]
closeDoor = call CloseDoor {prf=Here}

knock : Eff () [DOOR Closed]
knock = call Knock {prf=Here}

doorProg : Eff () [DOOR Closed]
doorProg = do knock
              openDoor
              closeDoor
