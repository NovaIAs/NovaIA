```haskell
module PrettyPrint where

import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ (
  Doc,
  Pretty,
  hsep,
  hsepWith,
  parens,
  pbreak,
  ppShow,
  punctuate,
  text,
  vsep,
  words
  )

class Pretty a where
  pp :: a -> Doc

instance Pretty String where
  pp = text

instance (Pretty a) => Pretty [a] where
  pp = intercalate (text " ") . map pp

instance (Pretty a) => Pretty (Maybe a) where
  pp Nothing = text "Nothing"
  pp (Just a) = parens (pp a)

instance (Pretty a) => Pretty (Either a b) where
  pp (Left a) = parens (pp a)
  pp (Right b) = parens (pp b)

instance Pretty Int where
  pp = ppShow

instance Pretty Float where
  pp = ppShow

instance Pretty Bool where
  pp True = text "True"
  pp False = text "False"

instance Pretty () where
  pp = text "()"

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pp (a, b) = parens (pp a <> text " : " <> pp b)

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pp (a, b, c) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c)

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pp (a, b, c, d) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  pp (a, b, c, d, e) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
  pp (a, b, c, d, e, f) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) => Pretty (a, b, c, d, e, f, g) where
  pp (a, b, c, d, e, f, g) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h) => Pretty (a, b, c, d, e, f, g, h) where
  pp (a, b, c, d, e, f, g, h) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i) => Pretty (a, b, c, d, e, f, g, h, i) where
  pp (a, b, c, d, e, f, g, h, i) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j) => Pretty (a, b, c, d, e, f, g, h, i, j) where
  pp (a, b, c, d, e, f, g, h, i, j) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i <> text " , " <> pp j)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j, Pretty k) => Pretty (a, b, c, d, e, f, g, h, i, j, k) where
  pp (a, b, c, d, e, f, g, h, i, j, k) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i <> text " , " <> pp j <> text " , " <> pp k)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j, Pretty k, Pretty l) => Pretty (a, b, c, d, e, f, g, h, i, j, k, l) where
  pp (a, b, c, d, e, f, g, h, i, j, k, l) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i <> text " , " <> pp j <> text " , " <> pp k <> text " , " <> pp l)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j, Pretty k, Pretty l, Pretty m) => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  pp (a, b, c, d, e, f, g, h, i, j, k, l, m) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i <> text " , " <> pp j <> text " , " <> pp k <> text " , " <> pp l <> text " , " <> pp m)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j, Pretty k, Pretty l, Pretty m, Pretty n) => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  pp (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = parens (pp a <> text " , " <> pp b <> text " , " <> pp c <> text " , " <> pp d <> text " , " <> pp e <> text " , " <> pp f <> text " , " <> pp g <> text " , " <> pp h <> text " , " <> pp i <> text " , " <> pp j <> text " , " <> pp k <> text " , " <> pp l <> text " , " <> pp m <> text " , " <> pp n)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i, Pretty j, Pretty k, Pretty l, Pretty m, Pretty n, Pretty o) => Pretty (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  pp (a, b, c, d