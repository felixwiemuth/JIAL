# Unit Tests

## Testanleitung

### Ziel des Tests
In diesem Test sollen Außenstehende einfache, selbstgewählte Algorithmen in der Input/Action-Sprache implementieren.
Die Algorithmen werden in Textdateien mit der vorgegebenen Syntax definiert und dann in das bereitgestellte Haskell-Testframework eingetragen.

### Vorkenntnisse
Ein grundlegendes Verständnis der Input/Action-Sprache sollte vorhanden sein. Eine Einführung findet sich in den Abschnitten 3.1 und 3.2(.0), Beispiele 3.2.2 der Arbeit.
Wenn du auch die Terminierungsanalyse testen willst, genügt es evtl., Abschnitt 3.3.6.1 "The message type graph" zu lesen - das Terminierungskriterium lautet: Ist dieser Graph azyklisch, terminiert der Algorithmus. Getestet werden soll, ob die implementierte Analyse die vorhandenen Kreise richtig erkennt.

### Syntax
Die konkrete Syntax der erstellten Implementierung der Input/Action-Sprache ist unter 6.1 der Arbeit zu entnehmen.
Sie ähnelt den Beispielen in der Arbeit, verwendet aber Java als Grundsprache.
Das 2-Phasen-Commit-Protokoll aus 3.2.2 der Arbeit ist in den Tasks 2pcC und 2pcP implementiert.

#### Wichtige Details
- Kein Überladen von Messagetypen (also gleichzeitiges Verwenden von `m(int x)` und `m()` ist nicht erlaubt)
- Tasks Ids werden aufsteigend in der angegebenen Reihenfolge vergeben, angefangen bei 0. Es kann aber immer mit dem Namen der Taskklasse alle Tasks dieser Klasse adressiert werden. D.h. wenn eine Task `TaskA` definiert ist, kann man `send m() to TaskA` benutzen.

### Test
Der Test besteht aus drei Teilen: einmal werden Lexer/Parser getestet, d.h. ob Beispiele überhaupt geparset werden können (dies geschieht implizit; kann eine Task nicht geparst werden, gibt es eine Fehlermeldung).
Dann wird die Terminierungsanalyse getestet, d.h. ob genau die erwarteten Kreise gefunden werden.
Zuletzt wird getestet, ob die Tasks auch so ablaufen, wie sie sollen (das ist bisher noch nicht implementiert, das Ergebnis erfahrt ihr also erst später).

#### Testframework
Das Testframework ist in Haskell implementiert. Testfälle werden in `AlgTest.hs` (Testliste ganz unten) hinzugefügt.
Im folgenden werden die Formate für die zwei Testsarten erklärt, Beispiele sind in der Testliste.

##### Zyklentest
`ct testName fileNames expectedCycles`

- `testName :: String`: Name für den Test 
- `fileNames :: [String]`: Liste der Pfade zu den Task-Dateien (es genügt, jede Task einmal anzugeben, egal, wie viele tatsächlich im Algorithmus ablaufen)
- `expectedCycles :: [String]`: Liste erwarteter Zyklen (Format: "taskA.inputx, taskB.inputy(x==4)" wobei in Klammern der eventuelle "when"-Ausdruck steht (ohne vorangehende und nachfolgende Leerzeichen etc. (whitspace))


##### Ergebnistest
`rt testName fileNames testConfigs`

- `testName`, `fileNames` wie oben
- `testConfigs :: [TestConfig]`: Liste von Testkonfigurationen, wobei `testConfig = (taskName, amount, checks) :: (String, Int, [String])`, was `amount` viele Instanzen der Task `taskName` (Name der Task, nicht der Datei) zum Algorithmus hinzufügt, mit einer Liste `checks` von Bool'schen Java-Ausdrücken (als String) im Scope der jeweiligen Task.

#### Ausführen der Tests
Falls haskell und HUnit installiert sind, kann der test mit `make algtest` gestartet werden.
