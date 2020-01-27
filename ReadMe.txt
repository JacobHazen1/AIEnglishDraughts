Everything is inside Main to be able to run.
Red Ai
Black ai
applyMoves
Moves

**NOTICE**

There was a minor bug with the original Version that i just discovered after 1st submission, 
I Have fixed the bug. when blackjumps and becomes a king it generates a king and a blackpiece on the board too.
I have fixed the bug where it spawns the black pawn.  

This was because
_blackPieces = (next:byebye start (_blackPieces st))

where the corrected version now has

_blackPieces = (byebye start (_blackPieces st))

