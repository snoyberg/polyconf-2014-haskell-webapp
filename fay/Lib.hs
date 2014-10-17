module Lib where

import Language.Fay.Yesod
import Fay.FFI

data JQuery
data Event

(<>) :: Text -> Text -> Text
x <> y = fromString (toString x ++ toString y)

getVal :: JQuery -> Fay Text
getVal = ffi "%1['val']()"

select :: Text -> Fay JQuery
select = ffi "jQuery(%1)"

click :: (Event -> Fay ()) -> JQuery -> Fay JQuery
click = ffi "%2['click'](%1)"

setText :: Text -> JQuery -> Fay JQuery
setText = ffi "%2['text'](%1)"

append :: JQuery -> JQuery -> Fay JQuery
append = ffi "%2['append'](%1)"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1['preventDefault']()"
