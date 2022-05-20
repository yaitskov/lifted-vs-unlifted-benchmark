{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# language KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnliftedDatatypes #-}

module UnliftedBoxed where

import GHC.Exts

type MyUnList :: TYPE (BoxedRep Unlifted) -> TYPE (BoxedRep Unlifted)

data MyUnList a where
  UnNil :: MyUnList a
  UnCons :: a -> MyUnList a -> MyUnList a

fromLiftedList :: [a] -> (a -> b) -> MyUnList b
fromLiftedList [] _ = UnNil
fromLiftedList (h:t) f = UnCons (f h) (fromLiftedList t f)

{-# INLINE foldlUn #-}
foldlUn :: forall (a :: UnliftedType) (b :: UnliftedType) . (b -> a -> b) -> b -> MyUnList a -> b
foldlUn f = go
  where
    go b UnNil = b
    go b (UnCons a t) = go (f b a) t

data MyUnliftedBoxed :: TYPE (BoxedRep Unlifted) where
  STrue :: MyUnliftedBoxed
  SFalse :: MyUnliftedBoxed

-- instance Show MyUnliftedBoxed where
show' :: MyUnliftedBoxed -> String
show' STrue = "STrue"
show' SFalse = "SFalse"

-- instance Eq MyUnliftedBoxed where
--   STrue == STrue = True
--   SFalse == SFalse = True
--   _ == _ = False

{-# INLINE toBool' #-}
toBool' :: MyUnliftedBoxed -> Bool
toBool' SFalse = False
toBool' STrue = True


fromBool' ::  Bool -> MyUnliftedBoxed
fromBool' False = SFalse
fromBool'  True = STrue


{-# INLINE sAnd #-}
sAnd :: MyUnliftedBoxed -> MyUnliftedBoxed -> MyUnliftedBoxed
sAnd STrue s = s
sAnd SFalse _ = SFalse
