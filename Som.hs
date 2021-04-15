module Som
    (
    notasMusicais
    ) where

import Euterpea

notasMusicais :: Int -> IO()
notasMusicais 1 = play $ a 4 qn
notasMusicais 2 = play $ c 4 qn
notasMusicais 3 = play $ g 4 qn
notasMusicais 4 = play $ e 4 qn
notasMusicais 5 = play $ a 2 qn
