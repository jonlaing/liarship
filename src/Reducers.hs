module Reducers where

import GameState
import Board
import Utils
import Data.Monoid

fire :: Coord -> State Fire -> State Defend
fire c s = switchPlayer $ s { activeCoordinate = Just c }

defend :: State Defend -> HitStatus
defend s = if shipAtActiveCoord s
  then Deflect $ decMissles (opponentPlayer s) s
  else Tricked $
    (decMissles curr . decMissles op) s
  where decMissles = modifyMissles pred
        curr = currentPlayer s
        op = opponentPlayer s

nodefend :: State Defend -> HitStatus
nodefend s = if shipAtActiveCoord s
  then DirectHit $ (decOpMissles . hitActiveCoord) s
  else Miss $ decOpMissles s
  where decOpMissles = modifyMissles pred (opponentPlayer s)

endGame :: State EndGame -> Either WinState (State NewGame)
endGame s =
  case win of
    Nothing -> Right $ s { activeCoordinate = Nothing }
    Just w -> Left w
  where win = getFirst $ (First $ winByAmmo s)
                      <> (First $ winByDestruction s)