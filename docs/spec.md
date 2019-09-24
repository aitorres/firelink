# Fire Link

**Fire Link**

Realizada por:

- Germán Robayo (14-10924)
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

La *sintaxis* de declaración con asignación es la siguiente (obligatoria para constantes, opcional para variables):

```
<declarador de valor> <identificador> of type <declarador de tipo> [<<= <valor>]
```

## Tipos de Datos

### Escalares

#### Entero

El tipo de dato _entero_ representa un número entero almacenado en complemento a dos. Este tipo cuenta con dos representaciones distintas, cada una con su declarador de tipo.

- `small humanity`, entero de 16 bits.
- `big humanity`, entero de 32 bits.

Adicionalmente, el declarador de tipo `humanity` es un alias para `big humanity`.

Los literales enteros son de la forma `[-]{0,1}[0-9]+` en expresión regular.

El valor por defecto de un entero es `0`.

#### Booleano

El tipo de dato _booleano_ representa un valor de verdad con dos posibilidades. Su declarador de tipo es `bonfire`.

Los literales booleanos son los siguientes:

- `lit` para un valor de verdad, con los aliases `true`, `yes`.
- `unlit` para un valor de falsedad, con los aliases `false`, `no`.

El valor por defecto de un booleano es `lit`.

#### 3-Booleano

El tipo de dato _3-booleano_ representa un valor de verdad con posibilidad de duda, es decir, con tres posibilidades. Su declarador de tipo es `fate`.

Los literales 3-booleanos, además de incluir los literales del tipo de datos _booleano_ de verdad y falsedad, incluyen:

- `unknown` para un valor de duda, con el alias `pending`.

El valor por defecto de un 3-booleano es `unknown`.

#### Punto flotante

El tipo de dato _punto flotante_ representa un número decimal con signo de precisión doble almacenado según el estándar _IEEE 754-2019_. Su declarador de tipo es _hollow_.

Los literales punto flotante son de la forma `[-]{0,1}[0-9]+([.][0-9]+){0,1}` en expresión regular.

El valor por defecto de un punto flotante es `0.0`.

#### Caracter

El tipo de dato _caracter_ representa un caracter. Este tipo cuenta con dos representaciones distintas, cada una con su declarador de tipo.

- `small sign`, caracter con codificación ASCII (7 bits).
- `big sign`, caracter con codificación UTF-8 (8 bits).

Adicionalmente, el declarador de tipo `sign` es un alias para `small sign`.

Los literales de caracter están encerrados entre el caracter `|`. Adicionalmente, el lenguaje debe reconocer los siguientes caracteres especiales:

- Caracter nulo `|\0|`
- Salto de línea `|\n|`
- Tabulación `|\t|`

El valor por defecto de un caracter es el caracter nulo (`|\0|`).

### Colección

#### Cadena de Caracteres

Representa una cadena colección de cero, uno o varios caracteres, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-miracle` donde `n` es un literal entero o una expresión de tipo entero.

Los literales de cadena de caracteres están encerrados entre el caracter `@` y sus elementos no están separados entre sí por ningún caracter.

El valor por defecto de una cadena de caracteres `n-miracle` es la cadena consistente de n espacios (`@          @` para `n == 10`).

#### Arreglos

Representa una colección de valores del mismo tipo, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-chest of type <tipo>`, donde `n` es un literal entero o una expresión de tipo entero y `tipo` es un declarador de tipo (que puede ser a su vez otro arreglo).

Los literales de arreglo están encerrados entre el caracter `$` y sus elementos están separados entre sí por una coma (`,`), seguida de cero, uno o varios espacios (` `). No se admite una coma entre el último elemento y el `$` de cierre.

Se puede acceder al valor en la posición `i` de un arreglo `a` con la sintaxis `a$i$`.

El valor por defecto de un arreglo `<n>-chest of type <tipo>` es un arreglo cuyos valores están inicializados al valor por defecto del tipo dado.

#### Conjuntos

### Estructurados

#### Registro

Representa un agrupamiento de distintas _propiedades_ (donde una _propiedad_ es un valor de un tipo determinado) bajo un mismo nombre en un bloque de memoria, cada propiedad con su identificador y valor, almacenados contiguamente. Su declarador de tipo es `bezel`.

La sintaxis de declaración de un registro es la siguiente (con indentación agregada para mayor legibilidad):

```
bezel {
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

Representa un valor que puede ser de algún tipo entre varios, bajo un mismo nombre en un mismo bloque de memoria. Su declarador de tipo es `link`.

La sintaxis de declaración de una unión es la siguiente (con indentación agregada para mayor legibilidad):

```
link {
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

El valor nulo es un tipo especial con un único valor, `abyss`, utilizado para indicar que un apuntador no tiene a quién referenciar, es decir, no está haciendo referencia a un valor válido.

#### Apuntador al Heap

Representa la dirección en la que un valor de un tipo dado se encuentra almacenado. Su declarador de tipo es `arrow to <tipo>`, donde `tipo` es un declarador de tipo.



El valor por defecto de un apuntador es el valor nulo, `abyss`.

#### Enumeración

#### Aliases de Tipo

## Instrucciones y Control de Flujo

### Comentarios

### Programa

Un programa tiene la siguiente sintaxis:

```
hello ashen one

[<declaraciones de funciones y/o procedimientos>]

<bloque de instrucciones principal>

farewell ashen one
```

Es obligatorio declarar un bloque de instrucciones principal, análogo a un procedimiento `main`. El bloque de instrucciones principal corresponde a un bloque de instrucciones que no esté asociado a la declaración de alguna función o procedimiento. No debe existir más de un (1) bloque de instrucciones principal.

### Bloque de Declaraciones de Variables

Un bloque de declaraciones de variables incluye una lista de una o varias declaraciones de variable, opcionalmente con su inicialización (en el caso de `var`, ya que los `const` siempre deben ser inicializados).

La sintaxis de un bloque de declaraciones es la siguiente (con indentación opcional para facilitar la legibilidad):

```
with
  <declaracion 1>,
  <declaracion 2>,
  ...
  <declaracion n>
in your inventory
```

Donde _declaracion 1_, _declaracion 2_, _declaracion n_ son declaraciones de tipo según la sintaxis indicada anteriormente.

Por ejemplo,

```
with
  var dragon of type humanity,
  var fire of type hollow
in your inventory
```

### Bloque de Instrucciones

Un bloque de instrucciones incluye opcionalmente un bloque de declaraciones de variables, válidas dentro del alcance del bloque, junto a una o varias instrucciones, debidamente concatenadas con el operador de secuenciación de instrucciones. En cualquier momento se puede abrir un bloque de instrucciones, y puede haber bloques de instrucciones arbitrariamente anidados. Se permitirá redeclarar variables entre bloques.

La sintaxis de un bloque de instrucciones es la siguiente (con indentación opcional para facilitar la legibilidad):

```
traveling to <lugar>
[<bloque de declaraciones>]
  <lista de instruciones>
you died
```

Donde _lugar_ es una de las siguientes palabras reservadas (cualquiera):

- `anor londo`
- `undead settlement`
- `the firelink`
- `duke's archives`

### Secuenciación

Para concatenar dos instrucciones `I1` e `I2`, de modo que `I1` se ejecute antes de `I2` en secuencia, se deben separar ambas con el operador `\`, quedando:

```
I1 \
I2
```

No es necesario colocar el operador de secuenciación al final de la última instrucción de un bloque de instrucciones.

### Asignación

Una asignación consiste en almacenar un valor en una variable de algún tipo determinado.

La sintaxis de asignación de una variable previamente declarada de identificador `a` es la siguiente:

```
a <<= <valor>
```

### Declaración de Función

Es un bloque aislado de código con una secuencia no nula de instrucciones a ejecutar que adicionalmente tiene un tipo de retorno asociado. Toda función requiere de un identificador único y válido, un tipo de retorno y opcionalmente una serie de parámetros con su identificador local y su tipo.

La sintaxis para declarar una función es la siguiente:

```
invocation <identificador>
[requesting
  <tipo de parametro 1> <identificador 1> of type <tipo 1>
  <tipo de parametro 2> <identificador 2> of type <tipo 2>
  ...
  <tipo de parametro n> <identificador n> of type <tipo n>
]
with skill of type <tipo de retorno>
  <bloque de instrucciones>
after this return to your world
```

Donde el tipo de parámetro es:

- `val` para parámetros por valor
- `ref` para parámetros por referencia

Dentro de la función, se puede utilizar la siguiente sintaxis para retornar una expresión que debe corresponder con el tipo de retorno de la función.

```
go back with <expresion>
```


#### Llamada a Funciones

Dentro de un bloque de instrucciones, se puede realizar una llamada a una función de la siguiente manera:

```
summon <nombre de la función>
[granting
  <expresion 1>,
  <expresion 2>,
  ...
  <expresion n>
]
```

Donde cada _expresion i_ corresponde a un argumento a pasar a la función, que debe corresponder con el tipo declarado. Si la función no recibe parámetros, el bloque que inicia con _granting_ no es requerido.

La llamada a la función evalúa a una expresión que puede ser asignada a una variable o utilizada en alguna otra expresión.

### Declaración de Procedimientos

ES un bloque aislado de código con una secuencia no nula de instrucciones a ejecutar que no tiene un tipo de retorno asociado. Todo procedimiento requiere de un identificador único y válido, y opcionalmente una serie de parámetros con su identificador local y su tipo.

La sintaxis para declarar un procedimiento en la siguiente:

```
spell <identificador>
[requesting
  <tipo de parametro 1> <identificador 1> of type <tipo 1>
  <tipo de parametro 2> <identificador 2> of type <tipo 2>
  ...
  <tipo de parametro n> <identificador n> of type <tipo n>
]
  <bloque de instrucciones>
ashen estus flask consumed
```

Donde el tipo de parámetro es:

- `val` para parámetros por valor
- `ref` para parámetros por referencia

#### Llamada a Procedimientos

Dentro de un bloque de instrucciones, se puede realizar una llamada a un procedimiento de la siguiente manera:

```
cast <nombre de la función>
[offering
  <expresion 1>,
  <expresion 2>,
  ...
  <expresion n>
]
```

Donde cada _expresion i_ corresponde a un argumento a pasar a la función, que debe corresponder con el tipo declarado. Si la función no recibe parámetros, el bloque que inicia con _granting_ no es requerido.

La llamada a la función NO evalúa a ninguna expresión.

### Imprimir

Para imprimir una expresión a la salida estándar, se puede usar el siguiente procedimiento:

```
with orange saponite say <expresion>
```

### Leer

Para leer una expresión y almacenarla en una variable, se puede usar el siguiente procedimiento:

```
transpose into <identificador>
```

El identificador debe corresponder con una variable ya declarada de un tipo escalar.

### Selección

### Selección por casos

### Iteraciones acotadas

Las iteraciones acotadas permiten repetir una secuencia no vacía de instrucciones por una cantidad fija de iteraciones, conocida antes de la primera iteración.

La sintaxis para una iteración acotada es la siguiente:

```
upgrading <identificador de la variable de iteración> with <paso> soul[s] until level <cota superior>
  <bloque de instrucciones>
max level reached
```

Donde _identificador de la variable de iteración_ corresponde al identificador de una variable de tipo _humanity_ previamente declarada e inicializada, y _paso_ es la cantidad que será adicionada automáticamente a la variable al finalizar una iteración.

Por ejemplo:

```
upgrading i with 1 soul until level 20
  traveling to the firelink
    with orange saponite say i \
    with orange saponite say |\n|
  you died
max level reached
```

Dentro del bloque de instrucciones, no se permitirá modificar directamente la variable de iteración. La cota superior es una cota no alcanzada; es decir, la iteración no se ejecutará para el valor de la cota superior.

### Iteraciones condicionadas

Las iteraciones condicionadas permiten repetir una secuencia no vacía de instrucciones mientras se cumpla una condición de tipo `bonfire`, evaluada antes de la primera iteración y luego de cada una de las iteraciones realizadas, mientras el valor de la expresión `bonfire` corresponda a un valor de verdad.

La sintaxis para una iteración condicionada es la siguiente:

```
while the <expresion de tipo bonfire> covenant is active:
  <bloque de instrucciones>
covenant left
```

Donde _expresion de tipo bonfire_ corresponde a la condición de iteración que será evaluada como se refirió anteriormente.

Por ejemplo:

```
while the b covenant is active:
  b <<= unlit
covenant left
```
