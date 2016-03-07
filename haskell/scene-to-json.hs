import Control.Monad
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Numbers
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.JSON

type Vec3 = (Double, Double, Double)
type Color = (Int, Int, Int)

data Shape = Sphere Vec3 Double Color
           | Plain Vec3 Vec3 Color deriving Show;

fl = (parseFloat :: GenParser Char st Double)
nt = (nat :: GenParser Char st Int)
sep = many1 space
vec3 = liftM3 (,,) fl (sep >> fl) (sep >> fl)
color = liftM3 (,,) nt (sep >> nt) (sep >> nt)
shape =
  try (string "S" >> sep >> liftM3 Sphere vec3 (sep >> fl) (sep >> color))
  <|> try (string "P" >> sep >> liftM3 Plain vec3 (sep >> vec3) (sep >> color))
sceneFile =
  do width <- nt
     height <- (sep >> nt)
     cameraDepth <- (sep >> fl)
     lightPosition <- (sep >> vec3)
     cnt <- (sep >> nt)
     shapes <- count cnt (sep >> shape)
     return (width, height, cameraDepth, lightPosition, shapes)

makeVec (x, y, z) = JSArray [
    JSRational True $ toRational x, 
    JSRational True $ toRational y, 
    JSRational True $ toRational z]
makeColor (r, g, b) = JSArray [
    JSRational False $ toRational r, 
    JSRational False $ toRational g, 
    JSRational False $ toRational b]

makeShapes shapes = JSArray (map makeShape shapes)
makeShape (Plain origin n col) = makeObj [
  ("kind", JSString $ toJSString "plane"),
  ("origin", makeVec origin),
  ("n", makeVec n),
  ("color", makeColor col)]
makeShape (Sphere center rad col) = makeObj [
  ("kind", JSString $ toJSString "sphere"),
  ("center", makeVec center),
  ("radius", JSRational True $ toRational rad),
  ("color", makeColor col)]
buildJsValue (width, height, cameraDepth, lightPosition, shapes) =
  makeObj [
      ("width", JSRational False $ toRational width),
      ("height", JSRational False $ toRational height),
      ("cameraDepth", JSRational True $ toRational cameraDepth),
      ("light", makeVec lightPosition),
      ("shapes", makeShapes shapes)]
    
main =
    do c <- getContents
       case parse sceneFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> do putStrLn $ encodeStrict (buildJsValue r) 