import Control.Applicative ((<$>), (<*>))
import Text.Parsec.Numbers (parseFloat)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (nat)
import Text.JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)

type Vec3 = (Double, Double, Double)
type Color = (Int, Int, Int)

data Shape = Sphere { s_center :: Vec3, s_radius :: Double, s_color :: Color }
           | Plane { p_origin :: Vec3, p_n :: Vec3, p_color :: Color }
    deriving Show;

sceneFile :: GenParser Char st (Int, Int, Double, Vec3, [Shape])
sceneFile = do width <- many space >> nt
               height <- sep >> nt
               cameraDepth <- sep >> fl
               lightPosition <- sep >> vec3
               shapes <- (count <$> (sep >> nt)) >>= ($ sep >> shape)
               return (width, height, cameraDepth, lightPosition, shapes)

shape :: GenParser Char st Shape
shape =
  try (string "S" >> sep >> Sphere <$> vec3 <*> (sep >> fl) <*> (sep >> color))
  <|> try (string "P" >> sep >> Plane <$> vec3 <*> (sep >> vec3) <*> (sep >> color))

vec3 = (,,) <$> fl <*> (sep >> fl) <*> (sep >> fl)
color = (,,) <$> nt <*> (sep >> nt) <*> (sep >> nt)

fl = parseFloat
nt = nat
sep = many1 space

makeJsDouble = (JSRational True) . toRational
makeJsInt = (JSRational False) . toRational
makeJsString = JSString . toJSString

makeVec (x, y, z) = JSArray $ map makeJsDouble [x, y, z]
makeColor (r, g, b) = JSArray $ map makeJsInt [r, g, b]

makeShapes = JSArray . (map makeShape)
makeShape (Plane origin n col) = makeObj [
  ("kind", makeJsString "plane"),
  ("origin", makeVec origin),
  ("n", makeVec n),
  ("color", makeColor col)]
makeShape (Sphere center rad col) = makeObj [
  ("kind", makeJsString "sphere"),
  ("center", makeVec center),
  ("radius", makeJsDouble rad),
  ("color", makeColor col)]
buildJsValue (width, height, cameraDepth, lightPosition, shapes) = makeObj [
  ("width", makeJsInt width),
  ("height", makeJsInt height),
  ("cameraDepth", makeJsDouble cameraDepth),
  ("light", makeVec lightPosition),
  ("shapes", makeShapes shapes)]
    
main =
    do c <- getContents
       case parse sceneFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> putStrLn $ render $ pp_value $ buildJsValue r
