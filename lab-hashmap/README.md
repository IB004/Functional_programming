# Лабораторная работа №2

`Бутов Иван` | `P3317` | ` sc-dict`

## Описание решения

## Тесты

## Отчет по тетстированию
```
lab-hashmap                > test (suite: lab-hashmap-test)
Dictionary Tests
  Unit tests
    Insert test:                          OK
      Preparing...
      Elements count check
      Lookup inserted check
      Lookup not inserted check
    Replace test:                         OK
      Preparing...
      Elements count check
      Lookup inserted check
      Replace inserted..
      Elements count check
      Lookup replaced check
    Delete test:                          OK
      Preparing...
      Elements count check
      Lookup inserted check
      Delete elements...
      Elements count check
      Lookup check
      Delete rest elements...
      Elements count check
      Lookup check
  Property tests
    Mempty test:                          OK (0.01s)
      +++ OK, passed 100 tests.
    Concat associativity test:            OK (0.07s)
      +++ OK, passed 100 tests.
    Foldr to list and restore back:       OK
      +++ OK, passed 100 tests.
    Foldl to list length is elemetsCount: OK
      +++ OK, passed 100 tests.
    Filter doesn't increase elemetsCount: OK
      +++ OK, passed 100 tests.
    Map doesn't change elemetsCount:      OK
      +++ OK, passed 100 tests.
All 9 tests passed (0.07s)
lab-hashmap                > Test suite lab-hashmap-test passed
```