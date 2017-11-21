module MaxPrice (maxPrice) where
  import qualified Data.List as List
  import qualified Data.Map as Map
  import qualified Data.MemoTrie as Memo

  getCutsMap length prices =
    Map.fromList $
      map (\x -> (x, 
                  List.sort $ 
                    filter (\cuts -> (sum cuts == x)) $
                    filter (\cuts -> all (\cut -> Map.member cut prices) cuts) $
                    getCuts length length 1 [[]])) 
          [1..length]

  getCuts length maxLength i prevCuts = candidateCuts
    where
      cutsUpToLengthI =
        if length == 1
        then [[x] | x <- [1..length]]
        else concatMap (\prevCut -> 
                          filter (\cut -> List.sort cut == cut) $ 
                          filter (\cut -> sum cut <= maxLength) $
                          map (\newElem -> prevCut ++ [newElem]) [1..length]) 
                        prevCuts
      candidateCuts =
        if i == length
        then cutsUpToLengthI
        else cutsUpToLengthI ++ getCuts length maxLength (i+1) cutsUpToLengthI

  getPrice cut length prices allCutsMap =
    if Map.member length prices
    then sum $ map (\x -> prices Map.! x) cut
    else sum $ map (\cutLength -> 
      fst $ maxPriceMemoized cutLength prices allCutsMap) cut

  maxPrice length prices = maxPriceMemoized length prices (getCutsMap length prices)

  maxPriceMemoized :: Int -> Map.Map Int Int -> Map.Map Int [[Int]] -> (Int, [[Int]])
  maxPriceMemoized = Memo.memo maxPriceCore
    where
      maxPriceCore 0 _ _ = (0, [[]])
      maxPriceCore 1 prices _ = (prices Map.! 1, [[1]])
      maxPriceCore length prices allCutsMap = (bestPrice, bestCuts)
        where
          cutsOfLengthI = allCutsMap Map.! length
          bestPrice = maximum $ map (\x -> getPrice x length prices allCutsMap) cutsOfLengthI
          bestCuts = filter (\x -> bestPrice == getPrice x length prices allCutsMap) cutsOfLengthI