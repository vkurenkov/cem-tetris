module TimeQuantizer where

import CodeWorld

-- | In seconds
type Time = Double

-- | Wrapper for real state
data TimeQuantizer state = TimeQuantizer state Time

-- | Quantize init
quantizeInit
  :: state                     -- Target initial state
  -> TimeQuantizer state
quantizeInit initState = TimeQuantizer initState 0.0

-- | Quantize update
quantizeUpdate 
  :: (state -> state)           -- Target state update function
  -> Time                       -- Update rate (in seconds)
  -> Time                       -- Delta time (in seconds)
  -> TimeQuantizer state        -- Current state in time
  -> TimeQuantizer state
quantizeUpdate update updateRate dt (TimeQuantizer curState lastFrameDt)
  | isUpdateTime = (TimeQuantizer (update curState) newLastFrameDt)
  | otherwise = (TimeQuantizer curState newLastFrameDt)
  where
    newLastFrameDt
      | (lastFrameDt + dt) < updateRate = lastFrameDt + dt
      | (lastFrameDt + dt) >= updateRate = (lastFrameDt + dt) - updateRate
    isUpdateTime = (lastFrameDt + dt) >= updateRate

-- | Wrapper for rendering quantization
quantizeRender
  :: (state -> Picture)          -- Target render function
  -> TimeQuantizer state         -- Current state in time
  -> Picture
quantizeRender render (TimeQuantizer curState _) = render curState

-- | Wrapper for event handling
quantizeHandler
  :: (Event -> state -> state)   -- Target event handling function
  -> Event
  -> TimeQuantizer state
  -> TimeQuantizer state
quantizeHandler handle event (TimeQuantizer curState lastFrameDt) = (TimeQuantizer (handle event curState) lastFrameDt)
