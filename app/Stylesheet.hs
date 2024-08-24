module Stylesheet where

import IHP.HSX.QQ
import Text.Blaze.Html

stylesheet :: Html
stylesheet = [hsx|
    <style>
        * {
          background-color: black;
          color: white;
          text-align: center;
        }
    </style>
|]
