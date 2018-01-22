### Adaline
  Алгоритм классификации ADALINE— адаптивны линейный элемент, в качестве функции потерь используется квадратичная функция потерь:
![equation](http://latex.codecogs.com/gif.latex?(<w,x>&space;-&space;y_i)^2)
 Алгоритм обучается с помошью стохастического градиента.

  Обучение ADALINE заключается в подборе "наилучших" значений вектора весов w. Какие значение весов лучше определяет функционал потерь. В ADALINE используется функционал, предложенный Видроу и Хоффом, ![equation](http://latex.codecogs.com/gif.latex?L(a,x)&space;=&space;(a-y)^2). Таким образом необходимо минимизировать функционал ![equation](http://latex.codecogs.com/gif.latex?L(a,x)&space;=&space;Q(w)): 

![equation](http://latex.codecogs.com/gif.latex?$$Q(w)=&space;\sum^m_{i=1}{(a(x_i,w)-y_i)^2}\rightarrow&space;min_w$$).
Построим алгоритм на классификации ирисов Фишера, с классами virginica и versicolo по 3 и 4 параметрам datafram'а. Проверим 
работу алгоритма на 2,5,10 и 43 шагах стохастического градиента. Получим: 
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/adadline_result.png)

Линия на графике - разделяющая гиперплоскость. Оптимум достигается на 43'ом шаге.
Веса получились равны: w_1 = 3.24, w_2 = -1.7

#### Перцептрон Розенблата
Будем ипсользовать virginica и versicolor. По 3 и 4 параметрам datafram'а

Будем проверять результаты работы на 10, 75, 230, 275 шаге стохастического градиентного спуска. В реузльтате получим:

![](https://github.com/Goncharoff/SMPR/blob/master/perceptron.png)

Линия на графике - раздиляющая гиперплоскость. Оптимум достигается при 275'ом шаге.
В результате получили веса, равные w_1 = -102.9, w_2 = 152.1, w_3 = 119.


### Логическая регрессия
Логическая регрессия - линейный байесовский классификатор, использующий логарифмическую функцию потерь: 
![equation](http://latex.codecogs.com/gif.latex?L(M)&space;=&space;\log_2(1&plus;e^{-M}))
Метод логистической регрессии основан на довольно сильных вероятностных
предположениях, которые имеют несколько интересных последствий:
1. Линейный классификатор оказывается оптимальным байесовским;
2. Однозначно определяется функция потерь;
3. Можно вычислять не только принадлежность объектов классам, но
также получать и численные оценки вероятности их принадлежности.
  Обучения происходит с помошью стохастического градиента.
  Необходимо мниимизировать функционал ![equation](http://latex.codecogs.com/gif.latex?Q(w,&space;X^l)&space;=&space;\sum_{i=1}^l&space;{\log_2&space;\sigma(<w,&space;x_i>y_i)&space;&plus;&space;const(w)}&space;\rightarrow&space;max_w)

  Построим алгоритм на классификации ирисов Фишера, с классами virginica и versicolo по 3 и 4 параметрам datafram'а. Проверим 
работу алгоритма на 2,5,10 и 8791 шагах стохастического градиента. Получим: 
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/log_reg_result.png)

Линия на графике - разделяющая гиперплоскость. Оптимум достигается на 8791'ом шаге.
Веса получились равны: w_1 = 0.56, w_2 = -1.54

Достоинства логистической регрессии:

• Как правило, логистическая регрессия дает лучшие результаты по сравнению с линейным дискриминантом Фишера (поскольку она основана на менее жестких гипотезах), а также по сравнению с дельта-правилом и правилом Хэбба (поскольку она использует „более правильную” функцию потерь).

•Возможность оценивать апостериорные вероятности и риски.

Недостатки логистической регрессии:

•Оценки вероятностей и рисков могут оказаться неадекватными, если не выполняются предположения теоремы.

•Градиентный метод обучения логистической регрессии наследует все недостатки метода стохастического градиента. Практичная реализация должна предусматривать стандартизацию данных, отсев выбросов, регуляризацию (сокращение весов), отбор признаков, и другие эвристики для улучшения сходимости. Возможно применение метода второго порядка, но он требует обращения n×n-матриц на каждом шаге и nакже не застрахован от плохой сходимости.

### Итоговый график для трех классификаторов:

![](https://github.com/Goncharoff/SMPR/blob/master/imgs/all_result.png)
