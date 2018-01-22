### Adaline
Алгоритм классификации ADALINE— адаптивны линейный элемент, в качестве функции потерь используется квадратичная функция потерь:
![equation](http://latex.codecogs.com/gif.latex?(<w,x>&space;-&space;y_i)^2)
Алгоритм обучается с помошью стахастического градиента.

Пусть дана обучающая выборка: множество входных значений X и множество выходящих значений Y, такие что каждому входу ![equation]http://latex.codecogs.com/gif.latex?x_j соответствует ![equation]http://latex.codecogs.com/gif.latex?y_j - выход, ![equation]http://latex.codecogs.com/gif.latex?j&space;=&space;1..m. Необходимо по этим данным построить ADALINE, которая допускает наименьшее количество ошибок на этой обучающей выборке. Обучение ADALINE заключается в подборе "наилучших" значений вектора весов w. Какие значение весов лучше определяет функционал потерь.В ADALINE используется функционал, предложенный Видроу и Хоффом, ![equation]http://latex.codecogs.com/gif.latex?L(a,x)&space;=&space;(a-y)^2. Таким образом необходимо минимизировать функционал ![equation]http://latex.codecogs.com/gif.latex?L(a,x)&space;=&space;Q(w): 
![equation]http://latex.codecogs.com/gif.latex?Q(w)=&space;\sum^m_{i=1}{a(x_i,w)-y_i)^2\rightarrow&space;min_w}.

Построим алгоритм на классификации ирисов Фишера, с классами virginica и versicolo по 3 и 4 параметрам datafram'а. Проверим 
работу алгоритма на 2,5,10 и 43 шагах стохастического градиента. Получим: 
![]https://github.com/Goncharoff/SMPR/blob/master/imgs/adadline_result.png

Линия на графике - разделяющая гиперплоскость. Оптимум достигается на 43'ом шаге.
Веса получились равны: w_1 = 3.24, w_2 = -1.7

#### Перцептрон Розенблата
Будем ипсользовать virginica и versicolor. По 3 и 4 параметрам datafram'а

Будем проверять результаты работы на 10, 75, 230, 275 шаге стохастического градиентного спуска. В реузльтате получим:

![](https://github.com/Goncharoff/SMPR/blob/master/perceptron.png)

Линия на графике - раздиляющая гиперплоскость. Оптимум достигается при 275'ом шаге.
В результате получили веса, равные w_1 = -102.9, w_2 = 152.1, w_3 = 119.

