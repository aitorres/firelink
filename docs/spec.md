# Nombre del Lenguaje

Descripción del lenguaje.

Realizada por:

- Germán Robayo (14-)
- Andrés Ignacio Torres (14-11082)

## Palabras Reservadas

```
const var small big of type fact doubt true false yes yas si ja da ok no nah nein nicht cancelled unknown idk something both schroedinger sink rune monogram box gister possibility pointer to nothing
```

## Identificadores

En <lenguaje>, un identificador válido es toda palabra que comienza con una letra minúscula, está formada posteriormente por letras, números y guiones bajos (`_`) y no es una palabra reservada por el lenguaje. 

Formalmente, es toda palabra de la forma `[a-z][A-Za-z0-9_]*` que no sea a su vez una palabra reservada por el lenguaje.

## Declaradores

En <lenguaje>, existen dos tipos de declaradores para los tipos de datos:

- `const` para declarar identificadores cuyos valores no pueden ser reasignados, y que requiere de una inicialización junto a su declaración.
- `var` para declarar identificadores cuyos valores pueden o no ser reasignados, y que no requiere de una inicialización al momento de declararse.

La *sintaxis* de declaración sin asignación es la siguiente:

```
<declarador de valor> <identificador> of type <declarador de tipo>
```

## Tipos de Datos

### Escalares

#### Entero

!TODO: small whole, big whole

El tipo de dato _entero_ representa un número entero con signo de 32 bits almacenado en complemento a dos. Su declarador de tipo es `whole`.

Los literales enteros son de la forma `[-]{0,1}[0-9]+` en expresión regular.

El valor por defecto de un entero es `0`.

#### Booleano

El tipo de dato _booleano_ representa un valor de verdad con dos posibilidades. Su declarador de tipo es `fact`.

Los literales booleanos son los siguientes:

- `true` para un valor de verdad, con los aliases `yes`, `yas`, `si`, `ja`, `da`, `ok`.
- `false` para un valor de falsedad, con los aliases `no`, `nah`, `nein`, `nicht`, `cancelled`.

El valor por defecto de un booleano es `true`.

#### 3-Booleano

El tipo de dato _3-booleano_ representa un valor de verdad con posibilidad de duda, es decir, con tres posibilidades. Su declarador de tipo es `doubt`.

Los literales 3-booleanos, además de incluir los literales del tipo de datos _booleano_ de verdad y falsedad, incluyen:

- `unknown` para un valor de duda, con los aliases `idk`, `something`, `both`, `schroedinger`.

El valor por defecto de un 3-booleano es `unknown`.

#### Punto flotante

El tipo de dato _punto flotante_ representa un número decimal con signo de precisión doble almacenado según el estándar _IEEE 754-2019_. Su declarador de tipo es _sink_.

Los literales punto flotante son de la forma `[-]{0,1}[0-9]+([.][0-9]+){0,1}` en expresión regular.

El valor por defecto de un punto flotante es `0.0`.

#### Caracter

El tipo de dato _caracter_ representa un caracter. Este tipo cuenta con dos representaciones distintas, cada una con su declarador de tipo.

- `small rune`, caracter con codificación ASCII (7 bits).
- `big rune`, caracter con codificación UTF-8 (8 bits).

Adicionalmente, el declarador de tipo `rune` es un alias para `small rune`.

Los literales de caracter están encerrados entre el caracter `|`. Adicionalmente, el lenguaje debe reconocer los siguientes caracteres especiales:

- Caracter nulo `|\0|`
- Salto de línea `|\n|`
- Tabulación `|\t|`

El valor por defecto de un caracter es el caracter nulo (`|\0|`).

### Colección

#### Cadena de Caracteres

Representa una cadena colección de cero, uno o varios caracteres, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-monogram` donde `n` es un literal entero o una expresión de tipo entero.

Los literales de cadena de caracteres están encerrados entre el caracter `@` y sus elementos no están separados entre sí por ningún caracter.

El valor por defecto de una cadena de caracteres `n-monogram` es la cadena consistente de n espacios (`@          @` para `n == 10`).

#### Arreglos

Representa una colección de valores del mismo tipo, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-box of type <tipo>`, donde `n` es un literal entero o una expresión de tipo entero y `tipo` es un declarador de tipo (que puede ser a su vez otro arreglo).

Los literales de arreglo están encerrados entre el caracter `$` y sus elementos están separados entre sí por una coma (`,`), seguida de cero, uno o varios espacios (` `). No se admite una coma entre el último elemento y el `$` de cierre.

Se puede acceder al valor en la posición `i` de un arreglo `a` con la sintaxis `a$i$`.

El valor por defecto de un arreglo `<n>-box of type <tipo>` es un arreglo cuyos valores están inicializados al valor por defecto del tipo dado.

#### Conjuntos (*)

### Estructurados

#### Registro

Representa un agrupamiento de distintas _propiedades_ (donde una _propiedad_ es un valor de un tipo determinado) bajo un mismo nombre en un bloque de memoria, cada propiedad con su identificador y valor, almacenados contiguamente. Su declarador de tipo es `gister`.

La sintaxis de declaración de un registro es la siguiente (con indentación agregada para mayor legibilidad):

```
gister {
  <nombre 1> of type <tipo 1>,
  <nombre 2> of type <tipo 2>,
  <nombre 3> of type <tipo 3>,
  ...
  <nombre n> of type <tipo n>
}
```

Donde `n` es la cantidad de _propiedades_ distintas del registro declarado, cada una con un nombre *único* para el mismo registro, independientemente de los tipos.

El acceso a una propiedad de nombre `prop` de un registro `a` se realiza mediante la sintaxis `a~>prop`.

Por defecto, todas las _propiedades_ de un registro se inicializan al valor por defecto correspondiente según el tipo.

#### Unión

Representa un valor que puede ser de algún tipo entre varios, bajo un mismo nombre en un mismo bloque de memoria. Su declarador de tipo es `possibility`.

La sintaxis de declaración de una unión es la siguiente (con indentación agregada para mayor legibilidad):

```
possibility {
  <type <tipo 1>,
  <type <tipo 2>,
  <type <tipo 3>,
  ...
  <type <tipo n>
}
```

Donde `n` es la cantidad de tipos *distintos entre sí* de la unión.

El valor por defecto de una unión corresponde al valor por defecto de su primer tipo declarado.

### Especiales

#### Valor nulo

El valor nulo es un tipo especial con un único valor, `nothing`, utilizado para indicar que un apuntador no tiene a quién referenciar, es decir, no está haciendo referencia a un valor válido.

#### Apuntador al Heap

Representa la dirección en la que un valor de un tipo dado se encuentra almacenado. Su declarador de tipo es `pointer to <tipo>`, donde `tipo` es un declarador de tipo.



El valor por defecto de un apuntador es `nothing

#### Enumeración (*)

## Instrucciones y Control de Flujo

### Programa

### Bloque de Declaraciones


### Bloque de código

### Secuenciación

Para concatenar dos instrucciones `I1` e `I2`, de modo que `I1` se ejecute antes de `I2` en secuencia, se deben separar ambas con el operador `\`, quedando:

```
I1 \
I2
```

No es necesario colocar el operador de secuenciación al final de la última instrucción de un bloque de código.

### Asignación

Una asignación consiste en almacenar un valor en una variable de algún tipo determinado.

La sintaxis de asignación de una variable previamente declarada de identificador `a` es la siguiente:

```
a <<= <valor>
```

### Función

Es un bloque aislado de código con una serie no nula de instrucciones a ejecutar que adicionalmente tiene un tipo de retorno asociado. La declaración

### Procedimientos

### Selección

### Selección por casos (*)

### Iteraciones acotadas

### Iteraciones condicionadas
