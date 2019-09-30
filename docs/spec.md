# Fire Link

**Fire Link** es un lenguaje de programación basado en el _lore_ de la serie de videojuegos *Dark Souls*, diseñado para la cadena de electivas de área Lenguajes de Programación II y III (CI-4721, CI-4722), durante los trimestres Septiembre - Diciembre 2019 y Enero - Marzo 2020 en la Universidad Simón Bolívar.

**Autores**

- Germán Robayo (14-10924)
- Andrés Ignacio Torres (14-11082)

## Descripción General del Lenguaje

**Fire Link** es un lenguaje de programación imperativo no orientado a objetos.

## Palabras Reservadas

A continuación se muestran las palabras reservadas por el lenguaje, a manera de referencia. El uso de cada palabra se describe a lo largo de este documento.

|          |           |           |           |          |            |            |          |
|----------|-----------|-----------|-----------|----------|------------|------------|----------|
| const    | var       | of        | type      | small    | big        | humanity   | hollow   |
| bonfire  | lit       | unlit     | yes       | no       | true       | false      | fate     |
| unknown  | pending   | sign      | miracle   | chest    | bezel      | link       | abyss    |
| arrow    | to        | hello     | ashen     | one      | farewell   | with       | in       |
| your     | inventory | traveling | you       | died     | invocation | requesting | skill    |
| after    | this      | return    | world     | val      | ref        | summon     | granting |
| spell    | ashen     | estus     | flask     | consumed | cast       | offering   | orange   |
| saponite | say       | transpose | trust     | your     | inventory  | liar!      | enter    |
| enter    | dungeon   | empty     | upgrading | soul     | souls      | until      | level    |
| max      | reached   | while     | the       | covenant | is         | active     | left     |
| lte      | lt        | gte       | gt        | eq       | neq        | not        | ascii    |
| of       | closed    | exited    |           |          |            |            |          |

(Agregar lugares.)

## Identificadores

En `<lenguaje>`, un identificador válido es toda palabra que comienza con una letra minúscula, está formada posteriormente por letras, números y guiones bajos (`_`) y no es una palabra reservada por el lenguaje.

Formalmente, es toda palabra de la forma `[a-z][A-Za-z0-9_]*` que no sea a su vez una palabra reservada por el lenguaje.

## Declaradores

En `<lenguaje>`, existen dos tipos de declaradores para los tipos de datos:

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

En **FireLink** se soportan los siguientes tipos de datos:

### Escalares

Los tipos de datos escalares son los tipos más primitivos que soporta el lenguaje, que pueden retornarse a través de funciones.

#### Entero

El tipo de dato _entero_ representa un número entero almacenado en complemento a dos. Este tipo cuenta con dos representaciones distintas, cada una con su declarador de tipo.

- `small humanity`, entero de 16 bits.
- `big humanity`, entero de 32 bits.

Adicionalmente, el declarador de tipo `humanity` es un alias para `big humanity`.

Los literales enteros son de la forma `[-]{0,1}[0-9]+` en expresión regular.

El valor por defecto de un entero es `0`.

##### Operadores para los enteros

Nuestro lenguaje soporta todos los operadores básicos de enteros:

* `+`: adición de dos `humanity`s, retornando un `humanity`.
* `-`: substracción de dos `humanity`s, retornando un `humanity`.
* `*`: multiplicación de dos `humanity`s, retornando un `humanity`.
* `/`: división de dos `humanity`s, retornando un `humanity`.
* `%`: resto de la división de dos `humanity`s, retornando un `humanity`.
* `-`: operador menos unario una `humanity`, retornando un `humanity`.
* `lt`: operador _menor que_, que retorna un `bonfire`.
* `gt`: operador _mayor que_, que retorna un `bonfire`.
* `lte`: operador _menor que_, que retorna un `bonfire`.
* `gte`: operador _mayor que_, que retorna un `bonfire`.
* `eq`: operador _igual_, que retorna un `bonfire`.
* `neq`: operador _desigual_, que retorna un `bonfire`.

#### Bonfire

El tipo de dato _bonfire_ representa un valor de verdad con posibilidad de incertidumbre, es decir, con tres posibilidades. Su declarador de tipo es `bonfire`.

Los literales bonfire son los siguientes:

- `lit` para un valor de verdad.
- `unlit` para un valor de falsedad.
- `undiscovered` para un valor de incertidumbre.

El valor por defecto de un _bonfire_ es `undiscovered`.

##### Operadores para los vonfires

El tipo de datos _bonfire_ soporta los siguientes operadores:

* `and`: `a and b`
* `or`: `a or b`
* `eq`: `a eq b`
* `neq`: `a neq b`
* `not`: `not a`

Los resultados de aplicar estos operadores se muestran a continuación, en distintas tablas.

##### Tabla de Valores para el Operador *not*

A continuación, los resultados de aplicar el operador _not_ a distintos valores del tipo `bonfire`.

| **not** |         |
|---------|---------|
| lit     | unlit   |
| undiscovered | undiscovered |
| unlit   | lit     |

##### Tabla de Valores para el Operador *and*

A continuación, los resultados de aplicar el operador _and_ a distintos valores del tipo `bonfire`.

| **and** | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | undiscovered   | unlit   |
| undiscovered | undiscovered   | undiscovered     | undiscovered   |
| unlit   | unlit   | undiscovered   | unlit     |

##### Tabla de Valores para el Operador *or*

A continuación, los resultados de aplicar el operador _or_ a distintos valores del tipo `bonfire`.

| **eq**  | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | lit   | lit   |
| undiscovered | lit   | undiscovered     | undiscovered   |
| unlit   | lit   | undiscovered   | unlit     |

##### Tabla de Valores para el Operador *eq*

A continuación, los resultados de aplicar el operador _eq_ a distintos valores del tipo `bonfire`.

| **eq**  | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | lit     | undiscovered | unlit   |
| undiscovered | undiscovered | lit     | undiscovered |
| unlit   | unlit   | undiscovered | lit     |

##### Tabla de Valores para el Operador *neq*

A continuación, los resultados de aplicar el operador _neq_ a distintos valores del tipo `bonfire`.

| **neq** | lit     | undiscovered | unlit   |
|---------|---------|---------|---------|
| lit     | unlit   | undiscovered | lit     |
| undiscovered | undiscovered | unlit   | undiscovered |
| unlit   | lit     | undiscovered | unlit   |

#### Punto flotante

El tipo de dato _punto flotante_ representa un número decimal con signo de precisión doble almacenado según el estándar _IEEE 754-2019_. Su declarador de tipo es _hollow_.

Los literales punto flotante son de la forma `[-]{0,1}[0-9]+([.][0-9]+){0,1}` en expresión regular.

El valor por defecto de un punto flotante es `0.0`.

##### Operadores para los enteros

Nuestro lenguaje soporta todos los operadores básicos de enteros:

* `+`: adición de dos `humanity`s, retornando un `humanity`.
* `-`: substracción de dos `humanity`s, retornando un `humanity`.
* `*`: multiplicación de dos `humanity`s, retornando un `humanity`.
* `/`: división de dos `humanity`s, retornando un `humanity`.
* `-`: operador menos unario una `humanity`, retornando un `humanity`.
* `lt`: operador _menor que_, que retorna un `bonfire`.
* `gt`: operador _mayor que_, que retorna un `bonfire`.
* `lte`: operador _menor que_, que retorna un `bonfire`.
* `gte`: operador _mayor que_, que retorna un `bonfire`.
* `eq`: operador _igual_, que retorna un `bonfire`.
* `neq`: operador _desigual_, que retorna un `bonfire`.

#### Caracter

El tipo de dato _caracter_ representa un caracter. Este tipo cuenta con una representación:.

- `sign`, caracter con codificación ASCII (ocupa 8 bits).

Los literales de caracter están encerrados entre el caracter `|`. Adicionalmente, el lenguaje debe reconocer los siguientes caracteres especiales:

- Caracter nulo `|\0|`
- Salto de línea `|\n|`
- Tabulación `|\t|`

El valor por defecto de un caracter es el caracter nulo (`|\0|`).

##### Funciones de los caracteres

Se debe implementar la siguiente función en el preludio de **FireLink**.

* `ascii_of`: Retorna el código ascii  de la variable (como `humanity`).

### Colección

Los tipos colección permiten incorporar una noción de orden en datos al lenguaje.

#### Cadena de Caracteres

Representa una cadena colección de cero, uno o varios caracteres, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-miracle` donde `n` es un literal entero o una expresión de tipo entero.

Los literales de cadena de caracteres están encerrados entre el caracter `@` y sus elementos no están separados entre sí por ningún caracter.

El valor por defecto de una cadena de caracteres `n-miracle` es la cadena consistente de n espacios (`@          @` para `n == 10`).

##### Operadores de las cadenas de caracteres

Se cuenta con el siguiente operador:

* `>-<`: toma un `n-miracle` y un `m-miracle` y los concatena, retornando un `(n+m)-miracle`.

##### Funciones de los caracteres

Se debe implementar la siguiente función en el preludio de **FireLink**.

* `ascii_of`: Retorna un arreglo de `humanity`s, representando los códigos asciis de cada caracter en el input.

#### Arreglos

Representa una colección de valores del mismo tipo, almacenados contiguamente, de longitud conocida al momento de su declaración. Su declarador de tipo es `<n>-chest of type <tipo>`, donde `n` es un literal entero o una expresión de tipo entero y `tipo` es un declarador de tipo (que puede ser a su vez otro arreglo).

Los literales de arreglo están encerrados entre el caracter de inicio `<$` y el caracter de fin `$>`, y sus elementos están separados entre sí por una coma (`,`), seguida de cero, uno o varios espacios (` `). No se admite una coma entre el último elemento y el `$>` de cierre.

Se puede acceder al valor en la posición `i` de un arreglo `a` con la sintaxis `a$i$`.

El valor por defecto de un arreglo `<n>-chest of type <tipo>` es un arreglo cuyos valores están inicializados al valor por defecto del tipo dado.

Además, se debe proveer el operador `>-<` que concatena dos arreglos. El tipo de dato que encierra cada arreglo debe ser el mismo, aunque no está sujeto a que ambos arreglos sean del mismo tamaño.

#### Conjuntos [!]

### Estructurados

Los tipos de datos estructurados dan cierta noción de estructura al lenguaje.

#### Enumeración

Representa un agrupamiento de distintas _posibilidades_ de acuerdo a su nombre. Internamente, cada posibilidad corresponde a un valor entero ordinal autoincrementado, y cada posibilidad evalúa a su valor. Su declarador de tipo es `titanite`.

La sintaxis de declaración de una enumeración es la siguiente (con indentación agregada para mayor legibilidad):

```
titanite {
  <nombre 1>,
  <nombre 2>,
  <nombre 3>
}
```

Donde `n` es la cantidad de _posibilidades_ distintas de la enumeración declarada, cada una con un nombre *único* para la misma enumeración.

El acceso a una posibilidad de nombre `pos` de una enumeración `a` se realiza mediante la sintaxis `a~>pos`.

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
  <nombre 1> of type <tipo 1>,
  <nombre 2> of type <tipo 2>,
  <nombre 3> of type <tipo 3>,
  ...
  <nombre n> of type <tipo n>
}
```

Donde `n` es la cantidad de tipos *distintos entre sí* de la unión, cada una con un nombre *único* para el mismo registro, independientemente de los tipos.

El valor por defecto de una unión corresponde al valor por defecto de alguno de sus tipos.

### Especiales

#### Valor nulo

El tipo unitario `abyss` tiene un único valor (`abyss`), utilizado para indicar que un apuntador no tiene a quién referenciar, es decir, no está haciendo referencia a un valor válido.

#### Apuntador al Heap

Representa la dirección en la que un valor de un tipo dado se encuentra almacenado. Su declarador de tipo es `arrow to <tipo>`, donde `tipo` es un declarador de tipo escalar.

TO DO:

- Función malloc
- Función asignar
- Función dereferenciar

El valor por defecto de un apuntador es el valor nulo, `abyss`.

#### Aliases de Tipo

Un alias de tipo permite referenciar un tipo determinado de manera más sencilla para el programador. Su declarador de tipo es `knight`. Solo pueden existir de manera global en un programa. La sintaxis de creación de un alias de tipo es la siguiente:

```
knight <identificador> <tipo>
```

Donde `identificador` es un identificador válido para el alias de tipo, y `tipo` corresponde al tipo al que hará alias. Por ejemplo:

```
knight onion 10-chest of type 5-chest of type 15-miracle

...

var i of type knight
```

Se puede declarar una **lista** de `n` aliases de tipo, utilizando la siguiente sintaxis:

```
requiring help of
  <alias de tipo 1>,
  <alias de tipo 2>,
  ...
  <alias de tipo n>
help received
```

## Instrucciones y Control de Flujo

### Comentarios

Un programa puede contener comentarios en cualquier lugar. Los comentarios inician con `--` y abarcan el resto de la línea, hasta el próximo salto de línea. Sólo se admiten comentarios de una sola línea.

```
-- Esto es un comentario.

hello ashen one

-- Esto simula un bloque de comentarios
-- de muchas líneas,
-- ya van tres.

```

### Programa

Un programa tiene la siguiente sintaxis:

```
hello ashen one

[<lista de alias de tipo>]

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
traveling somewhere
[<bloque de declaraciones>]
  <lista de instruciones>
you died
```

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

El lenguaje poseee estructuras condicionales, de tipo if/elseif/else. La sintaxis es la siguiente:

```
trust your inventory
<condición 1>:
  <bloque de instrucciones 1>
[<condición 2>:
  <bloque de instrucciones 2>]
...
[<condición n>:
  <bloque de instrucciones n>]
[liar!:
  <bloque de instrucciones para el else>]
inventory closed
```

El token `liar!` es equivalente al token `else` de otros lenguajes de programación. Las condiciones deben ser expresiones que evaluen a un tipo de dato `bonfire` (booleano).

### Selección por casos

El lenguaje poseee estructuras condicionales, de tipo switch. La sintaxis es la siguiente:

```
enter dungeon with <identificador>
<expresión 1>:
  <bloque de instrucciones 1>
[<expresión 2>:
  <bloque de instrucciones 2>]
...
[<expresión n>:
  <bloque de instrucciones n>]
[empty dungeon:
  <bloque de instrucciones para el else>]
dungeon exited
```

El token `empty dungeon` es equivalente al token `default` de otros lenguajes de programación. No es requerido finalizar un bloque de instrucciones con un token especial (similar al `break` de otros lenguajes), ya que no se permite desbordar un caso hacia otro.

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

## Peso Máximo

**Fire Link** restringe el tamaño máximo (o _peso máximo_) de las variables en memoria estática y de pila a tiempo de ejecución, a una cantidad de _bytes_ conocida al compilar. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en bytes) de uso de memoria estática y de pila (ambas sumadas) que se permitirá al programa. En caso de sobrepasar el peso máximo durante la ejecución, el programa abortará y se avisará al usuario que se ha sobrepasado el mismo y por cuánta diferencia. Por defecto, se establecerá un *peso máximo* de **1000000 (1e6) bytes**.

## Cantidad Máxima de Funciones y Procedimientos

**Fire Link** restringe la cantidad máxima de armas (funciones y procedimientos) que pueden definirse en el programa a tiempo de compilación, a una cantidad máxima de funciones y procedimientos combinados. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en unidades) de funciones y procedimientos (ambos sumados) que se permitirá al programa. En caso de sobrepasar la cantidad máxima en compilación, el programa no compilará y el compilador avisará al usuario que se ha sobrepasado el máximo y por cuanta diferencia. Por defecto, se establecerá una *cantidad máxima* de **8 funciones y procedimientos**.

## Cantidad Máxima de Llamadas a Funciones y Procedimientos

**Fire Link** restringe la cantidad máxima de llamadas a funciones y procedimientos que pueden realizarse a tiempo de ejecución, a una cantidad máxima. Al momento de compilar, el compilador especificará una opción para escoger una cantidad entera que corresponde a la cantidad máxima (en unidades) de llamadas a funciones y procedimientos (ambos sumados) que se permitirá al programa. En caso de sobrepasar la cantidad máxima en ejecución, el programa abortará y se avisará al usuario que se ha sobrepasado el mismo. Este límite no incluirá llamadas recursivas de una misma función. Por defecto, se establecerá una *cantidad máxima* de **40 llamadas funciones y procedimientos**.
