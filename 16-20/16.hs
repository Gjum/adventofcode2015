main = do
  sues <- map (parse . words) <$> lines <$> readFile "16.in"
  let searchedProps = parseProps ["children:", "3,",
                                  "cats:", "7,",
                                  "samoyeds:", "2,",
                                  "pomeranians:", "3,",
                                  "akitas:", "0,",
                                  "vizslas:", "0,",
                                  "goldfish:", "5,",
                                  "trees:", "3,",
                                  "cars:", "2,",
                                  "perfumes:", "1"]

  print $ [fst sue | sue <- sues, hasAll 1 searchedProps $ snd sue] -- 213
  print $ [fst sue | sue <- sues, hasAll 2 searchedProps $ snd sue] -- 323

parse ("Sue":sueNr:propStrs) = (init sueNr, parseProps propStrs)

parseProps (k:v:[]) = [(init k, v)]
parseProps (k:v:ps) = kv:kvs
  where [kv] = parseProps (k:(init v):[])
        kvs = parseProps ps

-- `part` is 1 or 2 and just passed through to `fits`
hasAll part [prop] sueProps = fits part prop sueProps
hasAll part (prop:props) sueProps = fits part prop sueProps && hasAll part props sueProps

fits part prop [] = True
fits part prop@(pk,pv) (sp@(sk,sv):sueProps) =
  if pk == sk
    then if part == 2 && pk `elem` ["cats", "trees"]
        then sv > pv
      else if part == 2 && pk `elem` ["pomeranians", "goldfish"]
        then sv < pv
      else sv == pv
    else fits part prop sueProps
