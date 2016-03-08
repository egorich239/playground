import Control.Applicative ((<$>), (<*>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Numbers
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)

type Vec3 = (Double, Double, Double)
type Color = (Int, Int, Int)

data Shape = Sphere Vec3 Double Color
           | Plain Vec3 Vec3 Color 
    deriving Show;

fl = (parseFloat :: GenParser Char st Double)
nt = (nat :: GenParser Char st Int)
sep = many1 space
vec3 = (,,) <$> fl <*> (sep >> fl) <*> (sep >> fl)
color = (,,) <$> nt <*> (sep >> nt) <*> (sep >> nt)
shape =
  try (string "S" >> sep >> Sphere <$> vec3 <*> (sep >> fl) <*> (sep >> color))
  <|> try (string "P" >> sep >> Plain <$> vec3 <*> (sep >> vec3) <*> (sep >> color))
sceneFile :: GenParser Char st (Int, Int, Double, Vec3, [Shape])
sceneFile = do width <- many space >> nt
               height <- sep >> nt
               cameraDepth <- sep >> fl
               lightPosition <- sep >> vec3
               shapes <- (count <$> (sep >> nt)) >>= ($ sep >> shape)
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
            Right r -> putStrLn $ render $ pp_value $ buildJsValue r
