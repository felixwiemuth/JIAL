# JIAL compiler
A compiler and static termination analysis for the *Java input-action language* developed in my Master's thesis.
The language allows the event-driven formulation of distributed algorithms based on guarded commands by Dijkstra.

## Project contents
This project consists of:
- The JIAL compiler (Haskell project with makefile)
  - `jialc` is the JIAL compiler
  - `jialt` is the termination analysis tool
- The JIAL runtime system (Netbeans Java project)
- An example (the two-phase commit protocol)

## Short instructions

### Implementing an algorithm
To implement a distributed algorithm in JIAL, proceed as follows.

#### Define message types
Define all message types to be used by the algorithm in a file `messages.jialm` in the following format:

```
package mypackage.example;

import mypackage.example.MyClassA;
import mypackage.example.MyClassB;
...

msgtype1()
msgtype2(int)
msgtype3(MyClassA, int)
msgtype4(String, MyClassA, MyClassB)
...
```

The special `init()` message type is already defined and does not have to be specified.

#### Define tasks
For each task T, create a file `T.jial` of the following form: 


```
package example;

import ...

task T {
    private int i = 0;
    ...

    input init() {
      ...
    }

    input m(int k) {
      ...
    }
}
```

### Compiling an algorithm
Run `jialc msg_type_file task_file1 task_file2 ...`. This creates the java classes in the current working directory.

### Simulating an algorithm
- Add the compiled sources to a project which has the classes of the runtime system in class path
- Create an object of class `Simulator`
- Add tasks to the simulator with `Simulator.addTasks(...)`
- Start the simulator with `Simulator.run()` (this adds an `init()` message to each task and begins executing eligible actions at tasks)

### Running the termination analysis
Run the `jialt` tool on the task files:

`jialt task_file1 task_file2 ...`

It outputs the cycles of the message type graph.

## Technical Documentation

### Message type file format
The exact format parsed is:
- A line starting with `package ` or `import ` is added to the resulting class' header
- Comments are ignored
- A line starting otherwise is interpreted as message type definition

A message type definition starts with a valid Java identifier and is followed by further identifiers, separated by whitespaces, parentheses and/or commas.
The preferred way of specifying a message type is: `msg_name(ParameterType1, ParamterType2, ...)`

### Message type class format
- Each parameter is named `pN` where N is the number of the parameter

## Making the compiler project
### Prerequisites
- ghc (8.6.2)
  - runhaskell (for tests)
- HUnit (1.6) (for tests)

### Makefile
The makefile has the following targets:

- `jialc`: make the JIAL compiler
- `jialt`: make the JIAL termination analysis tool
- `ltest`: run lexer tests
- `ptest`: run parser tests
- `atest`: run analysis tests
- `ctest`: run code generator tests
- `algtest`: run blackbox tests with algorithms from files
- `test` : run all tests
- `mtest`: run "manual tests" command line tool

## Unit Test instructions (German)

### Ziel des Tests
In diesem Test sollen Außenstehende einfache, selbstgewählte Algorithmen in der Input/Action-Sprache implementieren.
Die Algorithmen werden in Textdateien mit der vorgegebenen Syntax definiert und dann in das bereitgestellte Haskell-Testframework eingetragen.

### Vorkenntnisse
Ein grundlegendes Verständnis der Input/Action-Sprache sollte vorhanden sein. Eine Einführung findet sich in den Abschnitten 3.1 und 3.2(.0), Beispiele in 3.2.2 der Arbeit.
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
Falls haskell und HUnit installiert sind, kann der Test mit `make algtest` gestartet werden.


## License
Copyright (C) 2018 Felix Wiemuth

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
