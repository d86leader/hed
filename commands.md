# Hed commands

Команды вводятся с клавиатуры в терминал.
Нажатие клавиши RET отправляет набор команд на исполнение.
Также клавиша RET заканчивает некоторые режимы работы редактора.

### Shift

Многие команды различаются друг от друга нажатым шифтом.
Нажатие шифта для модификации команды может означать одну из следующих вещей:
- превратить команду, действующую на символы, в команду, действующую на строках
- превратить команду перемещения в команду добавления
- превратить команду, действующую вправо, в команду, действующую влево
- превратить команду, действующую вниз, в команду, действующую вверх

## Префиксы

Перед вводом команды можно ввести число (не начинающееся с цифры 0).
Некоторые команды меняют своё поведение от этого числа.
Например, `j` сместит курсоры на одну строку вниз, а `5j` сместит курсоры на
пять строк вниз.

Rule of the thumb таково:
- для команд абсолютного перемещения это выбор строки
- для команд относительного перемещения это насколько сместиться
- для команд вставки это сколько раз повторить вставку

## Самые важные команды

`p` - вывести содержимое файла и курсоры на экран

`:` - перейти в режим ввода длинной команды.
Из этого режима можно выйти только исполнив команду клавишей RET.

## Команды перемещения и выделения

`G` - add the first line to selection.
If number prefix is supplied, instead add the supplied line to selection.

`j` - move selection downwards

`k` - move selection upwards

`J` - add lines downwards

`K` - add lines upwards

`gg` - reset all selections and go to first line.
If number prefix is supplied, go to that line instead.

`h` - move one symbol to the left

`l` - move one symbol to the right

`H` - select one symbol to the left

`L` - select one symbol to the right

`B` - extend selection to the beginning of line

`E` - extend selection to the end of line

`0` - reset the selection to the beginning of line in each selected line

## Команды редактирования

`d` - yank and delete selected symbols

`D` - yank and delete selected lines

`c` - yank selected symbols, delete them and go into insert mode

`C` - yank selected lines, delete them and go into insert mode

`i` - insert symbols before selection

`a` - append symbols after selection

`I` - insert symbols to the beginning of selected lines

`A` - append symbols to the end of selected lines

`o` - append line after selected lines

`O` - insert line before selected lines

Each insert and append commands will move you to insert mode.
Enter the symbols you want to insert and press RET to exit it.

## Копирование-вставка

Все команды вырезания, копирования и вставки работают с "безымянным" регистром,
отмеченным символами `""`.

Вставка необычно работает с множественными выделениями.
Это долго описывать словами, лучше попробовать и увидеть.

`"X` - поменять содержимое безымянного регистра и региста X. Вместо X можно ввести любой символ с клавиатуры.

`y` - copy selected symbols

`Y` - copy selected lines whole

`[` - paste symbols before selection

`]` - paste symbols after selection

`{` - paste line before each selected line

`}` - paste line after each selected line

## Длинные команды

`:regs` - print contents of all registers

`:print` - вывести содержимое файла и курсоры на экран

`:w` - write file

`:q` - exit hed
