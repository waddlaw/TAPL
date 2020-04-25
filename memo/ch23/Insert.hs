sort :: (a -> a -> Bool) -> [a] -> [a]
sort cmp = foldr f []
  where
    f x acc = insert cmp acc x

insert :: (a -> a -> Bool) -> [a] -> a -> [a]
insert cmp l y = snd $ foldr f e l
  where
    e = ([], [y])
    f hd acc@(rest, restwithe) =
      let newrest = hd:rest
          newrestwithe =
            if cmp y hd
              then y:newrest
              else hd:restwithe
      in (newrest, newrestwithe)

ex :: [Int]
ex = sort (<=) [3,2,6,1,4]