# Wordle

## Uso

Para jugar Wordle, ejecutar diwordle-exe

```bash
diwordle-exe [--daily [YYYY-MM-DD]] [--intentos N] [--random] [--filename FILENAME] [--palabra PALABRA]
```

Si no se especifica ninguna opción, `diwordle-exe` se ejecuta como `diwordle-exe --daily` utilizando la fecha de hoy.

## Opciones

- `--daily [YYYY-MM-DD]`: Jugar el Wordle diario. Si se especifica una fecha, se jugará el Wordle de esa fecha.
- `--intentos N`: Número de intentos para adivinar la palabra. Por defecto, 5. No funciona con `--daily`.
- `--random`: Jugar con una palabra aleatoria.
- `--filename FILENAME`: Ruta al archivo del diccionario. Debe ser .txt.
- `--palabra PALABRA`: Jugar con una palabra específica.
O para simplificar pueden correr:
- `cabal run`: Jugar el Wordle diario con 5 intentos.
