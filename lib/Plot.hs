module Plot where
import Graphics.Matplotlib (onscreen, plot, Matplotlib, toSvg, bar, xticks, (##), (%))
import Data.Aeson (ToJSON)
import Text.Blaze.Html (Html, preEscapedToHtml)
import GHC.Base (maxInt)


plotSVG :: (ToJSON a, Num a) => [a] -> [a] -> IO Html
plotSVG xs ys = preEscapedToHtml <$> do 
    let p = plot xs ys
    svg <- toSvg p
    return $ case svg of
        (Left x) -> x
        (Right x) -> x

barSVG :: (ToJSON a, Num a) => [a] -> [String] -> IO Html
barSVG xs names = preEscapedToHtml <$> do
    svg <- toSvg $ bar names xs
    return $ case svg of
        (Left x) -> x
        (Right x) -> x

