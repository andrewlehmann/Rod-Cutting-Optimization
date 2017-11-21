module MaxPrice (maxPrice) where
  import qualified Data.List as List
  import qualified Data.Map as Map
  import qualified Data.MemoTrie as Memo

  getPossibleCutsMap length prices =
    Map.fromList $
      map (\x -> (x, List.sort (filter (\y -> (sum y == x) && all (\z -> Map.member z prices) y) $
                    getPossibleCutsCore length length 1 [[]])))
        [1..length]

  getPossibleCutsCore length maxLength i prevCuts = candidateCuts
    where
      cutsUpToLengthI =
        if length == 1
        then [[x] | x <- [1..length]]
        else concatMap (\prevCut ->
          (filter (\cut -> List.sort cut == cut && sum cut <= maxLength) $
              map (\newElem -> prevCut ++ [newElem]) [1..length])) prevCuts
      candidateCuts =
        if i == length
        then cutsUpToLengthI
        else cutsUpToLengthI ++ getPossibleCutsCore length maxLength (i+1) cutsUpToLengthI

  maxPrice length prices = maxPriceMemoized length prices (getPossibleCutsMap length prices)

  maxPriceMemoized :: Int -> Map.Map Int Int -> Map.Map Int [[Int]] -> (Int, [[Int]])
  maxPriceMemoized = Memo.memo maxPriceCore
    where
      maxPriceCore 0 _ _ = (0, [[]])
      maxPriceCore 1 prices _ = (prices Map.! 1, [[1]])
      maxPriceCore length prices allCutsMap = (bestPrice, bestCuts)
        where
          cutsOfLengthI = filter (\x -> List.length x > 1 || Map.member (List.head x) prices) $ allCutsMap Map.! length
          getPriceMemo = Memo.memo getPrice
            where
              getPrice cut =
                if Map.member length prices
                then sum $ map (\x -> prices Map.! x) cut
                else sum $ map (\cutLength -> fst $ maxPriceMemoized cutLength prices allCutsMap) cut
          bestPrice = maximum $ map (\x -> getPriceMemo x) cutsOfLengthI
          bestCuts = filter (\x -> bestPrice == getPriceMemo x) cutsOfLengthI