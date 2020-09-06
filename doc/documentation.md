# Implementace B-Stromu v jazyce Haskell - Dokumentace
## Popis funkcionality
Jedná se o implementaci B Stromu v jazyce Haskell.
## Popis API
Program obsahuje implementaci následujících metod pro práci s B stromy:
- `treeFind :: BTree -> value -> isInTree` - rozhodne jestli je daná hodnota obsažená ve stromě
- `treeAdd :: tree -> value -> treeWithValue` - přidá hodnotu do stromu, a vrátí nový strom
- `treeDelete :: tree -> value -> treeWithoutValue` - smaže hodnotu ze stromu, a vrátí nový strom
- `treeFold :: f -> start -> tree -> result` - stromový fold
- `treeToList :: tree -> list` - převede strom na seznam hodnot
- `listToTree :: list -> treeOrder -> tree` - vytvoří strom s daným stupněm (treeOrder) ze seznamu hodnot postupným vkládáním
## Ovládání
Otevřete příkazový řádek v kořenové složce projektu, a zadejte příkaz: `ghci ./Main.hs`. Tímto se
spustí interpret Haskellu a uživatel tak může zadávat příkazy. \
Pro zadání testovacího vstupu zkopírujte obsah daného souboru ze složky *test_inputs* do ghci konzole a stiskněte Enter.