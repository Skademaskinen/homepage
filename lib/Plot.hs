module Plot where
import Graphics.Matplotlib (onscreen, plot, Matplotlib, toSvg)
import Graphics.Matplotlib.Internal (pySVG)
import Data.Aeson (ToJSON)
import Text.Blaze.Html (Html, preEscapedToHtml)


plotSVG :: (ToJSON a, Num a) => [a] -> [a] -> IO Html
plotSVG xs ys = preEscapedToHtml <$> do 
    let p = plot xs ys
    svg <- toSvg p
    return $ case svg of
        (Left x) -> x
        (Right x) -> x
