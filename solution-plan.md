# План решения задач

## Задача 1: Задача о восьми ферзях

### Описание
Задача о восьми ферзях заключается в размещении восьми ферзей на шахматной доске 8×8 таким образом, чтобы ни один ферзь не мог атаковать другого (т.е. никакие два ферзя не должны находиться на одной горизонтали, вертикали или диагонали).

### Алгоритм
1. Создаем массив `board` размером 8, где `board[i]` будет содержать позицию (столбец) ферзя в i-й строке.
2. Начинаем с первой строки (индекс 0) и пытаемся разместить ферзя в каждом столбце.
3. Для каждой позиции проверяем, не бьет ли новый ферзь уже размещенных.
4. Если позиция безопасна, размещаем ферзя и рекурсивно переходим к следующей строке.
5. Если все 8 ферзей размещены (дошли до 8-й строки), выводим решение.

### Структура кода
```pascal
// Нерекурсивная функция-обертка
procedure EightQueens;
var
  board: array[0..7] of Integer;
  solutionCount: Integer;
begin
  solutionCount := 0;
  // Инициализация массива
  // Вызов рекурсивной функции
  PlaceQueens(board, 0, solutionCount);
  WriteLn('Всего найдено решений: ', solutionCount);
end;

// Рекурсивная функция размещения ферзей
procedure PlaceQueens(var board: array of Integer; row: Integer; var solutionCount: Integer);
begin
  // Базовый случай: все ферзи размещены
  if row = 8 then
  begin
    // Вывод решения
    PrintSolution(board);
    solutionCount := solutionCount + 1;
  end
  else
  begin
    // Перебор всех столбцов в текущей строке
    for col := 0 to 7 do
    begin
      if IsSafe(board, row, col) then
      begin
        // Размещаем ферзя
        board[row] := col;
        // Рекурсивно переходим к следующей строке
        PlaceQueens(board, row + 1, solutionCount);
      end;
    end;
  end;
end;

// Функция проверки безопасности позиции
function IsSafe(const board: array of Integer; row, col: Integer): Boolean;
var
  i: Integer;
  safe: Boolean;
begin
  safe := True;
  for i := 0 to row - 1 do
  begin
    // Проверка на совпадение столбцов или диагоналей
    if (board[i] = col) or 
       (board[i] - i = col - row) or 
       (board[i] + i = col + row) then
    begin
      safe := False;
      break;
    end;
  end;
  IsSafe := safe;
end;

// Функция вывода решения
procedure PrintSolution(const board: array of Integer);
var
  i, j: Integer;
begin
  WriteLn('Решение:');
  for i := 0 to 7 do
  begin
    for j := 0 to 7 do
    begin
      if board[i] = j then
        Write('Q ')
      else
        Write('. ');
    end;
    WriteLn;
  end;
  WriteLn;
end;
```

## Задача 2: Биномиальный коэффициент Ньютона

### Описание
Биномиальный коэффициент C(n,k) - это коэффициент при x^k в разложении бинома Ньютона (x + y)^n. Он равен количеству способов выбрать k элементов из n элементов без учета порядка.

### Алгоритм
1. Используем рекурсивную формулу: C(n,k) = C(n-1,k-1) + C(n-1,k)
2. Базовые случаи: C(n,0) = C(n,n) = 1
3. Для оптимизации используем мемоизацию (сохранение уже вычисленных значений)

### Структура кода
```pascal
// Нерекурсивная функция-обертка
function BinomialCoefficient(n, k: Integer): Int64;
var
  memo: array of array of Int64;
  i, j: Integer;
begin
  // Проверка граничных условий
  if (k < 0) or (k > n) then
  begin
    BinomialCoefficient := 0;
    Exit;
  end;
  
  // Инициализация массива для мемоизации
  SetLength(memo, n + 1, k + 1);
  for i := 0 to n do
    for j := 0 to Min(i, k) do
      memo[i, j] := -1;
  
  // Вызов рекурсивной функции
  BinomialCoefficient := BinomialRecursive(n, k, memo);
end;

// Рекурсивная функция вычисления биномиального коэффициента
function BinomialRecursive(n, k: Integer; var memo: array of array of Int64): Int64;
begin
  // Базовые случаи
  if (k = 0) or (k = n) then
    BinomialRecursive := 1
  else
  begin
    // Проверка, вычислено ли уже значение
    if memo[n, k] <> -1 then
      BinomialRecursive := memo[n, k]
    else
    begin
      // Рекурсивное вычисление
      memo[n, k] := BinomialRecursive(n - 1, k - 1, memo) + BinomialRecursive(n - 1, k, memo);
      BinomialRecursive := memo[n, k];
    end;
  end;
end;
```

## Задача 3: Быстрая сортировка

### Описание
Быстрая сортировка (QuickSort) - эффективный алгоритм сортировки, основанный на принципе "разделяй и властвуй". Выбирается опорный элемент, и массив разделяется на две части: элементы меньше опорного и элементы больше опорного. Затем рекурсивно сортируются обе части.

### Алгоритм
1. Выбираем опорный элемент (обычно последний элемент в массиве).
2. Разделяем массив на две части: элементы меньше опорного и элементы больше опорного.
3. Рекурсивно сортируем обе части.
4. Базовый случай: если размер части меньше или равен 1, она уже отсортирована.

### Структура кода
```pascal
// Нерекурсивная процедура-обертка
procedure QuickSort(var arr: array of Integer);
begin
  // Вызов рекурсивной процедуры
  QuickSortRecursive(arr, 0, Length(arr) - 1);
end;

// Рекурсивная процедура быстрой сортировки
procedure QuickSortRecursive(var arr: array of Integer; low, high: Integer);
var
  pivotIndex: Integer;
begin
  // Базовый случай: если подмассив содержит более одного элемента
  if low < high then
  begin
    // Разделение массива и получение индекса опорного элемента
    pivotIndex := Partition(arr, low, high);
    
    // Рекурсивная сортировка левой части
    QuickSortRecursive(arr, low, pivotIndex - 1);
    
    // Рекурсивная сортировка правой части
    QuickSortRecursive(arr, pivotIndex + 1, high);
  end;
end;

// Функция разделения массива
function Partition(var arr: array of Integer; low, high: Integer): Integer;
var
  pivot, i, j, temp: Integer;
begin
  // Выбор опорного элемента (последний элемент)
  pivot := arr[high];
  
  // Индекс меньшего элемента
  i := low - 1;
  
  for j := low to high - 1 do
  begin
    // Если текущий элемент меньше или равен опорному
    if arr[j] <= pivot then
    begin
      // Увеличиваем индекс меньшего элемента
      i := i + 1;
      
      // Меняем местами arr[i] и arr[j]
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
    end;
  end;
  
  // Меняем местами arr[i+1] и arr[high] (опорный элемент)
  temp := arr[i + 1];
  arr[i + 1] := arr[high];
  arr[high] := temp;
  
  // Возвращаем индекс опорного элемента
  Partition := i + 1;
end;
```

## Основная программа

Основная программа будет содержать меню для выбора задачи и вызова соответствующей функции:

```pascal
program RecursiveAlgorithms;

uses
  SysUtils;

// [Здесь будут все функции и процедуры для трех задач]

// Функция безопасного ввода целого числа
function SafeReadInteger(const Prompt: string; MinValue, MaxValue: Integer): Integer;
var
  s: string;
  code: Integer;
  value: Integer;
  validInput: Boolean;
begin
  repeat
    validInput := False;
    Write(Prompt);
    ReadLn(s);
    
    // Проверка на пустую строку
    if s = '' then
    begin
      WriteLn('Ошибка: Пустой ввод. Пожалуйста, введите число.');
      Continue;
    end;
    
    // Попытка преобразовать строку в число
    Val(s, value, code);
    
    if code <> 0 then
      WriteLn('Ошибка: Введите корректное целое число.')
    else if (value < MinValue) or (value > MaxValue) then
      WriteLn('Ошибка: Число должно быть в диапазоне от ', MinValue, ' до ', MaxValue, '.')
    else
      validInput := True;
      
  until validInput;
  
  SafeReadInteger := value;
end;

// Основная процедура
begin
  var choice: Integer;
  
  repeat
    WriteLn('Выберите задачу:');
    WriteLn('1. Задача о восьми ферзях');
    WriteLn('2. Биномиальный коэффициент Ньютона');
    WriteLn('3. Быстрая сортировка');
    WriteLn('0. Выход');
    
    choice := SafeReadInteger('Ваш выбор: ', 0, 3);
    
    case choice of
      1: EightQueens;
      2: begin
           var n, k: Integer;
           n := SafeReadInteger('Введите n: ', 0, 30);
           k := SafeReadInteger('Введите k (0 <= k <= n): ', 0, n);
           WriteLn('C(', n, ',', k, ') = ', BinomialCoefficient(n, k));
         end;
      3: begin
           var n, i: Integer;
           var arr: array of Integer;
           
           n := SafeReadInteger('Введите размер массива: ', 1, 100);
           SetLength(arr, n);
           
           WriteLn('Введите элементы массива:');
           for i := 0 to n - 1 do
             arr[i] := SafeReadInteger('arr[' + IntToStr(i) + ']: ', -1000, 1000);
           
           WriteLn('Исходный массив:');
           for i := 0 to n - 1 do
             Write(arr[i], ' ');
           WriteLn;
           
           QuickSort(arr);
           
           WriteLn('Отсортированный массив:');
           for i := 0 to n - 1 do
             Write(arr[i], ' ');
           WriteLn;
         end;
    end;
    
    if choice <> 0 then
    begin
      WriteLn;
      WriteLn('Нажмите Enter для продолжения...');
      ReadLn;
    end;
    
  until choice = 0;
end.
