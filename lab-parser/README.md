# Лабораторная работа №4

`Бутов Иван` | `P3317`

## Описание решения
 - `Parser` -- библиотека парсер комбинаторов
 - `ParserJson` -- парсер json файлов во внутреннее представление `JsonValue` на основе `Parser`
 - `Lib` -- обработка ввода-вывода

## Пример работы

```
GET INPUT:
 {
        "name": "Ivan",
        "age": 20,
        "studying": true,
        "married": false,
        "array": [1, 2.0, +3, 004, -5, 6e2],
        "status": null
}
RESULT:
{
  name: "Ivan",
  age: 20.0,
  studying: True,
  married: False,
  array: [1.0,2.0,3.0,4.0,-5.0,600.0],
  status: null,
}
REST:

```