# SER502-DEVLang-Team4

## Team 4 :

- Dev Jani (djani4@asu.edu)
- Kalyani Joshi (kjoshi29@asu.edu)
- Kirtan Soni (kgsoni@asu.edu)
- Lakshmi Kruthi Hosamane Keshava Raman (lhosaman@asu.edu)

---
System Requirements : MacOS , Windows , Linus

## Installation
---
The Compiler version is Available on Github. 
```
git clone [URL]
```

and start Using it !

## Usage  
---
### Compiles and runs the code in one command. Check the Source code for more details.


MacOS / Linux 
```bash
./devlang [path/to/code]
```

Windows
```batchfile
./devlang.bat [path/to/code]
```





# Locally build the compiler
---
build Lexer (java)
```terminal
cd src/Lexer
javac -cp classes/antlr-runtime-4.13.1.jar CustomLanguageLexer.java
javac -cp "classes/antlr-runtime-4.13.1.jar:." Main.java
java -cp "classes/antlr-runtime-4.13.1.jar:." Main
```

call prolog Compiler from terminal
```
swipl -s /src/parser/Parser.pl -g "procedure(T,[Tokens] ,R), write(T),halt.
```

Execute the Parsed Tree
```terminal
swipl -s /src/interpreter.pl -g "eval_procedure([ Parsed Tree ], R),halt.
```