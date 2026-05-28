data PrviAliDrugi a b = Prvi a | Drugi b

newtype RecimoIzmenjujoci a b = PDSez [PrviAliDrugi a b]

primerIzmenujocega :: RecimoIzmenjujoci Int String
primerIzmenujocega = PDSez [Prvi 1, Drugi "dva", Prvi 3, Drugi "štiri"]

slabPrimerIzmenujocega :: RecimoIzmenjujoci Int String
slabPrimerIzmenujocega = PDSez [Prvi 1, Prvi 2, Prvi 3, Prvi 4]

data ResIzmenjujoci a b = Prazen | Sestavljen a (ResIzmenjujoci b a)

primerResIzmenjujoci :: ResIzmenjujoci Int String
primerResIzmenjujoci = Sestavljen 1 (Sestavljen "dva" (Sestavljen 3 (Sestavljen "štiri" Prazen)))

slabPrimerResIzmenjujoci :: ResIzmenjujoci Int String
slabPrimerResIzmenjujoci = Sestavljen 1 (Sestavljen 2 (Sestavljen 3 (Sestavljen 4 Prazen)))

data DvaNaNMinusEnaElementov a = Nic | EnPlusDvakrat a (DvaNaNMinusEnaElementov (a, a))

primer :: DvaNaNMinusEnaElementov Int
primer = EnPlusDvakrat 1 (EnPlusDvakrat (2, 3) (EnPlusDvakrat ((4, 5), (6, 7)) Nic))

prestej :: DvaNaNMinusEnaElementov a -> Int
prestej Nic = 0
prestej (EnPlusDvakrat _ t) = 1 + 2 * prestej t

data DvaNaNElementov a = En a | Dvakrat (DvaNaNElementov (a, a))

primerTree :: [DvaNaNElementov Int]
primerTree = [
    En 1,
    Dvakrat (En (2, 3)),
    Dvakrat (Dvakrat (En ((4, 5), (6, 7))))
    ]

prestejTree :: DvaNaNElementov a -> Int
prestejTree (En _) = 1
prestejTree (Dvakrat t) = 2 * prestejTree t

data Bootstrapping a = Nil | Zero (Bootstrapping (a, a)) | One a (Bootstrapping (a, a))

primerBootstrapping :: Bootstrapping Int
primerBootstrapping = One 1 (One (2, 3) (Zero (One (((4, 5), (6, 7)), ((8, 9), (10, 11))) Nil)))