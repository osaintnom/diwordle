# Wordle

## Uso

Para jugar Wordle, simplemente ejecuta el script `diwordle-exe`.

```bash
./diwordle [--daily [YYYY-MM-DD]] [--intentos N] [--random] [--filename FILENAME] [--palabra PALABRA]
```

### Opciones

- `--daily [YYYY-MM-DD]`: Jugar el Wordle diario. Si se especifica una fecha, se jugará el Wordle de esa fecha.
- `--intentos N`: Número de intentos para adivinar la palabra. Por defecto, 5. No funciona con `--daily`.
- `--random`: Jugar con una palabra aleatoria. No funciona con `--daily`.
- `--filename FILENAME`: Ruta al archivo del diccionario.
- `--palabra PALABRA`: Jugar con una palabra específica. No funciona con `--daily` ni con `--random`.