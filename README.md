<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Нестерук Анастасія Олександрівна КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: деструктивних операцій, циклів, функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
звіту наведені в п. 3.2.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 7
Алгоритм сортування Шелла за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun swap (lst idx new-elem)
  (cond
    ((null lst) nil)
    ((= idx 0) (cons new-elem (rest lst)))
    (t (cons (first lst) (swap (rest lst) (- idx 1) new-elem)))))

(defun shell-sorting (lst n gap i)
  (if (>= gap 1)
      (if (< i n)
          (let ((j i))
            (if (and (>= j gap) (> (nth (- j gap) lst) (nth j lst)))
                (shell-sorting (swap (replace-at lst j (nth (- j gap) lst)) 
                                            (- j gap) (nth j lst)) 
                                n gap (- j gap))
                (shell-sorting lst n gap (+ i 1))))
          (shell-sorting lst n (floor (/ gap 2)) 0))  
      lst))

(defun shell-sorting-functional (lst)
  (let ((n (length lst)))
    (shell-sorting lst n (floor (/ n 2)) 0)))
```

### Тестові набори
```lisp
(defun check-shell-sorting-functional (name input expected)
  "Execute shell-sorting-functional on input, compare result with expected and print comparison status"
  (let ((result (shell-sorting-functional input))) 
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            name expected result)))


(defun test-shell-sorting-functional ()
  (format t "Start testing shell-sorting-functional function~%")
  (check-shell-sorting-functional "test 1" '(346 23 0 32 44 76 2 120 34  32 65) '(0 2 23 32 32 34 44 65 76 120 346))
  (check-shell-sorting-functional "test 2" '(0 0 2 56 78 21 34 90 6751 1 1 1 -1 1) '(-1 0 0 1 1 1 1 2 21 34 56 78 90 6751))
  (check-shell-sorting-functional "test 3" '(3 4 2 9 34) '(2 3 4 9 34))
  (format t "EnD~%"))
```
### Тестування
```lisp
CL-USER> (test-shell-sorting-functional)
Start testing shell-sorting-functional function
test 1 passed! Expected: (0 2 23 32 32 34 44 65 76 120 346) Obtained: (0 2 23 32 32 34 44 65 76 120 346)
test 2 passed! Expected: (-1 0 0 1 1 1 1 2 21 34 56 78 90 6751) Obtained: (-1 0 0 1 1 1 1 2 21 34 56 78 90 6751)
test 3 passed! Expected: (2 3 4 9 34) Obtained: (2 3 4 9 34)
EnD
```
## Лістинг функції з використанням деструктивного підходу
```lisp
(defun shell-sorting-imperative (lst)
  (let ((my-copy-list (copy-list lst))
        (k (floor (/ (length lst) 2))))
    (loop while (>= k 1) do
          (loop for i from k below (length my-copy-list) do
                (let ((tmp (nth i my-copy-list))
                      (j i))
                  (loop while (and (>= (- j k) 0) (> (nth (- j k) my-copy-list) tmp)) do
                        (setf (nth j my-copy-list) (nth (- j k) my-copy-list))
                        (setf j (- j k)))
                  (setf (nth j my-copy-list) tmp)))
          (setf k (floor (/ k 2))))  
    my-copy-list))  
```

### Тестові набори
```lisp
(defun check-shell-sorting-imperative (name input   expected)
  "Execute shell-sorting-imperative on input, compare result with expected and print comparison status"
  (let ((result (shell-sorting-imperative input))) 
    (format t "~:[~a failed! Expected: ~a Obtained: ~a~;~a passed! Expected: ~a Obtained: ~a~]~%"
            (equal result expected)
            name expected result)))


(defun test-shell-sorting-imperative ()
  (format t "Start testing shell-sorting-imperative function~%")
  (check-shell-sorting-imperative "test 1" '(346 23 0 32 44 76 2 120 34  32 65) '(0 2 23 32 32 34 44 65 76 120 346))
  (check-shell-sorting-imperative "test 2" '(0 0 2 56 78 21 34 90 6751 1 1 1 -1 1) '(-1 0 0 1 1 1 1 2 21 34 56 78 90 6751))
  (check-shell-sorting-imperative "test 3" '(3 4 2 9 34) '(2 3 4 9 34))
  (format t "EnD~%"))
```

### Тестування
```lisp
CL-USER> (test-shell-sorting-imperative)
Start testing shell-sorting-imperative function
test 1 passed! Expected: (0 2 23 32 32 34 44 65 76 120 346) Obtained: (0 2 23 32 32 34 44 65 76 120 346)
test 2 passed! Expected: (-1 0 0 1 1 1 1 2 21 34 56 78 90 6751) Obtained: (-1 0 0 1 1 1 1 2 21 34 56 78 90 6751)
test 3 passed! Expected: (2 3 4 9 34) Obtained: (2 3 4 9 34)
EnD
```
