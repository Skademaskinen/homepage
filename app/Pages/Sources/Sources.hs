module Pages.Sources.Sources where

import IHP.HSX.QQ
import Text.Blaze.Html

import Helpers.Utils

sources :: Html
sources = [hsx|
    <h2 style="text-align:center;">Sources</h2>
    <div class="section" style="text-align:center;">
        This website is written using the following software:<br>
        {link_image "IHP" "https://ihp.digitallyinduced.com/ihp.svg" "https://ihp.digitallyinduced.com/"}
        {link_image "Haskell" "https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png" "https://haskell.org"}
        {link_image "Nix" "https://raw.githubusercontent.com/NixOS/nixos-artwork/53ea652ec7d8af5d21fd2b79b6c49cb39078ddfb/logo/nix-snowflake-colours.svg" "https://nixos.org"}
        <br>
        Source code: <a href="https://github.com/Mast3rwaf1z/homepage">
            <img src="/static/github.svg" height="20">
        </a>
    </div>
|]


