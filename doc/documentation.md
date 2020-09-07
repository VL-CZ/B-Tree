# Implementace B-Stromu v jazyce Haskell - Dokumentace
## Popis API
Program obsahuje implementaci následujících metod pro práci s B stromy:
- `treeFind :: BTree -> value -> isInTree` - rozhodne jestli je daná hodnota obsažená ve stromě
- `treeAdd :: tree -> value -> treeWithValue` - přidá hodnotu do stromu, a vrátí nový strom
- `treeDelete :: tree -> value -> treeWithoutValue` - smaže hodnotu ze stromu, a vrátí nový strom
- `treeFold :: f -> start -> tree -> result` - stromový fold
- `treeToList :: tree -> list` - převede strom na seznam hodnot
- `listToTree :: list -> treeOrder -> tree` - vytvoří strom s daným stupněm (treeOrder) ze seznamu hodnot postupným vkládáním

Výpis stromu je ve formátu `((child0) value0 (child1) value1 ... valueN (childN+1))`, listy jsou vypsány jako `Null`.
## Popis funkcionality
Jedná se o implementaci B Stromu (definice viz. https://en.wikipedia.org/wiki/B-tree#Definition) v jazyce Haskell.
- treeFind \
    Hledání hodnoty probíhá tak, že procházím celý strom postupně od kořene shora dolů. V každém vrcholu pak otestuju, jestli se hodnota nenachází v některém z jeho klíčů. Pokud ano, tak vrátím True, pokud ne, tak zavolám funkci rekurzivně na vhodného potomka. Vhodný potomek je ten, který leží mezi 2 klíči, z nichž levý klíč je menší než hledaná hodnota, a pravý klíč je naopak větší. V případě, že je hledaná hodnota menší (větší) než všechny klíče, tak vyberu prvního (posledního) potomka. Pokud se dostanu do vrcholu, který už má za potomky listy (v kódu značené jako Null), a hodnota se nenachází mezi klíči, tak vrátím False.
- treeAdd \
    Při vkládání do stromu nejprve vyhledám vhodné místo pro danou hodnotu, podobně jako ve funkci treeFind. Pokud se hodnota ve stromě už nachází, tak program vyhodí chybu:  `error "Value is already contained in the tree"`. Jinak do vybraného vrcholu (jeho synové jsou listy) vložím hodnotu. Pokud počet dětí tohoto vrcholu nepřeročil stupeň stromu, tak se program rekurzí vrátí zpět do kořene a vrátí nový strom. V opačném případě (počet dětí vrcholu je větší než stupeň B-stromu) zavolá funkci `balanceNthChildOverflow` (vyvážení n-tého potomka), která poté zavolá funkci `split` pro rozdělení vrcholu. To probíhá tak, že daný vrchol rozdělíme na dva, přidáme prostřední klíč (medián) ke klíčům předka, a dvojici nových vrcholů k jeho potomkům. Tím jsme mohli překročit max. počet dětí předka, takže tento postup opakuju při rekurzivním návratu do kořene ve všech vrcholech, poté program vrátí nový strom.
- treeDelete \
    Při mazání ze stromu nejprve program nejprve vrchol stromu, který obsahuje danou hodnotu. Pokud strom hodnotu neobsahuje, tak program vyhodí chybu: `error "Value is not contained in the tree."`, jinak příslušný klíč smaže. Pokud nejsou synové vrcholu listy, tak program najde následníka (nejmenší hodnotu ve stromě, která je větší než daný klíč), a smazaný klíč nahradí následníkem - metoda `extractSmallest`. Při mazání klíčů vrcholu může dojít k "podtečení" (tzn. vrchol má méně než minimální počet dětí), takže je potřeba vrchol vyvážit. K tomu slouží funkce `balanceNthChildUnderflow`, která vyváží n-tého potomka zadaného vrcholu (Vyvažování vrcholu probíhá z jeho předka, protože jsou potřeba sourozenci daného vrcholu).

    Při vyvažování daného vrcholu program nejprve zjistí, jestli tam může přesunout nějaký klíč + potomka jeho sourozence (tzn. sourozenec nemá min. počet dětí). Přesun hodnot (funkce `shift`) může probíhat z levého nebo pravého sourozence. Přesun hodnot z levého sourozence probíhá tak, že nejprve od něj odpojím poslední klíč a posledního potomka, a daného potomka jej jako 1. potomka k danému vrcholu. Poté přesunu klíč v předkovi, který oba vrcholy odděluje do zadaného vrcholu jako první klíč. Následně odebraný klíč od sourozence přesunu do předka tak, aby tyto 2 vrcholy odděloval. Přesun od pravého sourozence probíhá podobně, akorát zrcadlově obráceně.

    Pokud nelze provést přesun hodnot od žádného ze sourozenců (tzn. oba sourozenci mají min. počet dětí), tak se vrchol s jedním ze sourozenců sloučí. Sloučení 2 vrcholů probíhá tak, že nejprve z rodiče odstraním klíč k, který tyto 2 vrcholy odděloval. Pak vytvořím nový vrchol, jehož klíče jsou klíče daných 2 vrcholů oddělené klíčem k (odstraněný klíč z předka), a potomci jsou sjednocení potomků zadaných vrcholů. Tento vrchol pak přidám pod předka namísto zadaných 2 vrcholů. Tím může dojít k "podtečení" předka (tzn. předek bude mít méně než min. počet dětí), takže tento postup opakuju při rekurzivním návratu do kořene ve všech vrcholech, poté program vrátí nový strom. 

## Ovládání
Otevřete příkazový řádek v kořenové složce projektu, a zadejte příkaz: `ghci ./Main.hs`. Tímto se
spustí interpret Haskellu a uživatel tak může zadávat příkazy. \
Pro zadání testovacího vstupu zkopírujte obsah daného souboru ze složky *test_inputs* do ghci konzole a stiskněte Enter.