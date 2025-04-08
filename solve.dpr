program RecursiveAlgorithms;

uses
  SysUtils, Math;

const
  MIN_RANDOM_VALUE = -100;
  MAX_RANDOM_VALUE = 100;

type
  TMatrix = array of array of Int64;

{ Задача о восьми ферзях }
function IsSafe(const board: array of Integer; row, col: Integer): Boolean;
var
  i: Integer;
  safe: Boolean;
begin
  safe := True;
  for i := 0 to row - 1 do
  begin
  
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

procedure PlaceQueens(var board: array of Integer; row: Integer; var solutionCount: Integer);
var
  col: Integer;
begin

  if row = 8 then
  begin
  
    PrintSolution(board);
    solutionCount := solutionCount + 1;
  end
  else
  begin
  
    for col := 0 to 7 do
    begin
      if IsSafe(board, row, col) then
      begin
      
        board[row] := col;
      
        PlaceQueens(board, row + 1, solutionCount);
      end;
    end;
  end;
end;

procedure EightQueens;
var
  board: array[0..7] of Integer;
  solutionCount: Integer;
begin
  solutionCount := 0;

  PlaceQueens(board, 0, solutionCount);
  WriteLn('Всего найдено решений: ', solutionCount);
end;

{ Биномиальный коэффициент Ньютона }
function BinomialRecursive(n, k: Integer; var memo: TMatrix): Int64;
begin

  if (k = 0) or (k = n) then
    BinomialRecursive := 1
  else
  begin
  
    if memo[n, k] <> -1 then
      BinomialRecursive := memo[n, k]
    else
    begin
    
      memo[n, k] := BinomialRecursive(n - 1, k - 1, memo) + BinomialRecursive(n - 1, k, memo);
      BinomialRecursive := memo[n, k];
    end;
  end;
end;

function BinomialCoefficient(n, k: Integer): Int64;
var
  memo: TMatrix;
  i, j: Integer;
begin

  if (k < 0) or (k > n) then
  begin
    BinomialCoefficient := 0;
    Exit;
  end;
  

  SetLength(memo, n + 1, k + 1);
  for i := 0 to n do
    for j := 0 to Min(i, k) do
      memo[i, j] := -1;
  

  BinomialCoefficient := BinomialRecursive(n, k, memo);
end;

{ Быстрая сортировка }
function Partition(var arr: array of Integer; low, high: Integer): Integer;
var
  pivot, i, j, temp: Integer;
begin

  pivot := arr[high];
  

  i := low - 1;
  
  for j := low to high - 1 do
  begin
  
    if arr[j] <= pivot then
    begin
    
      i := i + 1;
      
    
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
    end;
  end;
  

  temp := arr[i + 1];
  arr[i + 1] := arr[high];
  arr[high] := temp;
  

  Partition := i + 1;
end;

procedure QuickSortRecursive(var arr: array of Integer; low, high: Integer);
var
  pivotIndex: Integer;
begin

  if low < high then
  begin
  
    pivotIndex := Partition(arr, low, high);
    
  
    QuickSortRecursive(arr, low, pivotIndex - 1);
    
  
    QuickSortRecursive(arr, pivotIndex + 1, high);
  end;
end;

procedure QuickSort(var arr: array of Integer);
begin

  QuickSortRecursive(arr, 0, Length(arr) - 1);
end;

{ Util }
procedure GenerateRandomArray(var arr: array of Integer; minValue, maxValue: Integer);
var
  i: Integer;
begin

  for i := 0 to Length(arr) - 1 do
    arr[i] := Random(maxValue - minValue + 1) + minValue;
end;

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
    
  
    if s = '' then
    begin
      WriteLn('Ошибка: Пустой ввод. Пожалуйста, введите число.');
      Continue;
    end;
    
  
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

{ Основная программа }

var
  choice, n, k, i: Integer;
  arr: array of Integer;
begin
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
           n := SafeReadInteger('Введите n: ', 0, 30);
           k := SafeReadInteger('Введите k (0 <= k <= n): ', 0, n);
           WriteLn('C(', n, ',', k, ') = ', BinomialCoefficient(n, k));
         end;
      3: begin
           n := SafeReadInteger('Введите размер массива: ', 1, 100);
           SetLength(arr, n);
           
           WriteLn('Выберите способ заполнения массива:');
           WriteLn('1. Ручной ввод');
           WriteLn('2. Случайная генерация');
           
           k := SafeReadInteger('Ваш выбор: ', 1, 2);
           
           if k = 1 then
           begin
             WriteLn('Введите элементы массива:');
             for i := 0 to n - 1 do
               arr[i] := SafeReadInteger('arr[' + IntToStr(i) + ']: ', -1000, 1000);
           end
           else
           begin
             GenerateRandomArray(arr, MIN_RANDOM_VALUE, MAX_RANDOM_VALUE);
             WriteLn('Массив заполнен случайными числами от ', MIN_RANDOM_VALUE, ' до ', MAX_RANDOM_VALUE);
           end;
           
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
