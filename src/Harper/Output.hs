module Harper.Output
  ( HarperOutput
  , showsPrt
  , outputMsg
  , outputLog
  , outputErr
  , outputConfl
  )
where

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Printer
import           ErrM
import           OutputM

type HarperOutput a = Output ShowS a

data Color = RGB Int Int Int

ansiFgColor :: Color -> ShowS
ansiFgColor (RGB r g b) =
  ("\ESC[38;2;" ++)
    . shows r
    . (";" ++)
    . shows g
    . (";" ++)
    . shows b
    . ("m" ++)

white :: Color
white = RGB 255 255 255

grey :: Color
grey = RGB 128 128 128

red :: Color
red = RGB 255 0 0

showsPrt :: Print a => a -> ShowS
showsPrt x = (render (prt 0 x) ++)

outputMsg :: String -> HarperOutput ()
outputMsg s = output (s ++)

outputLog :: ShowS -> HarperOutput ()
outputLog s = output msg
 where
  msg = ansiFgColor grey . ("Log: " ++) . s . ("\n" ++) . ansiFgColor white

outputErr :: (Position a, Print a) => ShowS -> a -> HarperOutput ()
outputErr s ctx = output msg
 where
  msg =
    ansiFgColor red
      . ("Error: " ++)
      . s
      . ("\nDuring evaluation of:\n" ++)
      . showsPrt ctx
      . prtPosition ctx
      . ("\n" ++)
      . ansiFgColor white


outputConfl
  :: (Position a, Print a, Position b, Print b)
  => ShowS
  -> a
  -> b
  -> HarperOutput ()
outputConfl s ctx1 ctx2 = output msg
 where
  msg =
    ansiFgColor red
      . ("Error: " ++)
      . s
      . ("\nDuring evaluation of:\n" ++)
      . showsPrt ctx2
      . prtPosition ctx2
      . ("\nConfilting with:\n" ++)
      . showsPrt ctx1
      . prtPosition ctx1
      . ("\n" ++)
      . ansiFgColor white

prtPosition :: Position a => a -> ShowS
prtPosition a = case pos a of
  Nothing -> id
  Just (l, c) ->
    ("\nLocated at line " ++) . shows l . (" column " ++) . shows c
