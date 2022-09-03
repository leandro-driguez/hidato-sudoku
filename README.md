
# Generador y solucionador de Hidatos

## Ejecución

Para la ejecución del software debe ejecutar los siguientes comandos: `make run` para ejecutar el script principal y poder acceder al interprete de Haskell `GHCI`, y luego ejecutar el comando `main` para correr el shell.

```sh
your_shell \$ make run
stack ghci app/Main.hs
Using configuration for hidato-sudoku:exe:hidato-sudoku-exe to load /home/leandro/Study/> > 3rd_Year/1st_semester/Declarative_Programming/Haskell/hidato-sudoku/app/Main.hs
Using main module: 1. Package `hidato-sudoku` component hidato-sudoku:exe:hidato-sudoku-exe with main-is file: /home/leandro/Study/3rd_Year/1st_semester/Declarative_Programming/Haskell/hidato-sudoku/app/Main.hs
hidato-sudoku> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: hidato-sudoku
GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( /home/leandro/Study/3rd_Year/1st_semester/Declarative_Programming/Haskell/hidato-sudoku/app/Main.hs, interpreted )
Ok, one module loaded.
Loaded GHCi configuration from /tmp/haskell-stack-ghci/1fdd8f95/ghci-script
ghci> main
/your_directory/hidato-sudoku $
```

### Funcionalidades implementadas

#### Añadir template
  
Para esto debe acceder al directorio `/newTemplates` y añadir un nuevo archivo `.txt`. Debe poner el caracter `'0'` en las casillas que desea dejar libres, y el caracter `'X'` donde desea poner obstáculos.

```sh
add -t <name_of_new_template>
```

**Ojo:** no necesita poner la extensión del archivo, solo su nombre.

#### Generar y resolver un hidato

```sh
solve <name_of_template>
```
