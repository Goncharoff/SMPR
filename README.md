# Метрические алгоритмы классификации
### kNN
 Алгоритм k ближайших соседей - kNN относит объект u к тому классу элементов которого больше среди k ближайших соседей 
![equation](http://latex.codecogs.com/gif.latex?x_u^{i},&space;i=1,...,k:)

![equation](http://latex.codecogs.com/gif.latex?w(i,&space;u)&space;=&space;[i&space;\leq&space;k];&space;a(u;&space;X^l,&space;k)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum^k_{i&space;=&space;1}{[y^i_{u}&space;=&space;y]})

Для выбора оптимального k используют метод скользащего контроля (LOO).
Применив kNN и LOO к датасету Ириса Фишера получим результат:

![](https://github.com/Goncharoff/SMPR/blob/master/imgs/LOO_kNN.png)


Видно, что лучишй результат получаем при k = 6, с оценкой ошибки равной 0.33, что равно 96% успешных классификаций.

 kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, а размерность пространства — K, то количество операций для классификации тестовой выборки может быть оценено как O(K*M*N).

### kwNN
Алогоритм k взвешенных ближайших соседей:
![equation](http://latex.codecogs.com/gif.latex?w(i,&space;u)&space;=&space;[i&space;\leq&space;k]w(i);&space;a(u;&space;X^l,&space;k)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum^k_{i&space;=&space;1}{[y^i_{u}&space;=&space;y]}w(i))
возьмем за вес ![equation](http://latex.codecogs.com/gif.latex?w(i)&space;=&space;q^i,q\epsilon&space;(0,1)), и его же будем перебирать по LOO при фиксированном k = 6, получим результат:
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/loo_kwnn.png)
Видем что лучший результат при k = 6 и q = 1. Равен 0.33, что примерно 96% успешных классификаций.
Зачем использовать kwNN если там больше расчетов? В задачах с числом классов 3 и более нечётность уже не помогает и сутации неодназначности могут возниктаь. Тогда на помошь приходят веса, и объект классифицируется к тому классу, чей суммарны вес больше среди k соседий.
### Парзеновские окна
Рассмотрим весовую функцию w(i,u) как функцию не от ранга соседа, а как функцию от расстояния 
![equation](http://latex.codecogs.com/gif.latex?\rho(u,x_u^i):)

![equation](http://latex.codecogs.com/gif.latex?w(i,u)&space;=&space;K(\frac{1}{h}&space;\rho&space;(u,x_u^i)))

где K(z) - невозрастающая на ![equation](http://latex.codecogs.com/gif.latex?[0,\infty&space;]) (гипотеза комактности) функция яда. В этом случае метричесикй классификатор примет следующий вид:
![equation](http://latex.codecogs.com/gif.latex?a(u;X^l,h)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{K(\frac{\rho(u,x_u^i)}{h})})
Этот алгоритм - алогритм парзеновского окна. h - ширина окна, подбирается по LOO.
Рассмотрим ядра: прямоугольно, епачниково, квадратное, гауссовское, треугольное. На датасете ирисов и подберем оптимальный h.
в итоге получим:
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/loo_parsen_results.png)

Красные точки на изображениях - оптимальные h для выбора. Итоговый результат получился чуть лучше чем при kwNN и kNN и равен - 97% при гауссовском ядре и ядре епачникова.
### Потенциальные функции 
Если в методе парзеновского окна центр окна поместить в классифицируемый объект, то получим метод потенциальных функций:
![equation](http://latex.codecogs.com/gif.latex?a(u;X^l)&space;=&space;argmax_{y\epsilon&space;Y}&space;\sum_{i:y_u^i=y}{\gamma_i&space;K(\frac{\rho(u,x_i)}{h_i})},&space;\gamma_i&space;\geq&space;0,&space;h_i&space;>&space;0)
Теперь ширина окна h зависит не от классифицуруемого объекта, а от обучающего x.
Реализован с помошью библиотека "plotrix". Получим:
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/pot_func_result.jpg)
Результат - 97% успещных классификаций.
### Байесовские алгоритмы классификации
Линейный дискриминант Фишера 
Пусть ковариационные матрицы классов одинаковы и равны ![equation](http://latex.codecogs.com/gif.latex?\sum). Оценим ![equation](http://latex.codecogs.com/gif.latex?\sum^{-}) по всем l объектам обучающей выборке. С учетом поправки на смещённость,

![equation](http://latex.codecogs.com/gif.latex?\sum^{-}&space;=&space;\frac{1}{l-|Y|}\sum_{i=1}^l&space;(x_i&space;-&space;\mu^{-}_{y_i})(x_i&space;-&space;\mu^{-}_{y_i})^T)

В этом случае разделяющая поверхность линейна. Подстановочный алгоритм имеет вид:

![equation](http://latex.codecogs.com/gif.latex?a(x)=argmax_{y\epsilon&space;Y}\lambda_y&space;P_y&space;\rho_y&space;(x)&space;=&space;argmax_{y\epsilon&space;Y}&space;(\ln&space;(\lambda_y&space;P_y)&space;-&space;\frac{1}{2}&space;\mu_y^{T}&space;\sum^{-1}&space;\mu_y&space;&plus;&space;x^{T}&space;\sum^{-1}&space;\mu_y))
Этот алгоритм называется линейным дискриминантом Фишера (ЛДФ). Он неплохо работает, когда формы классов действительно близки к нормальным и не слишком сильно различаются. В этом случае линейное решающее правило близко к оптимальному байесовскому, но существенно более устойчиво, чем квадратичное, и часто обладает лучшей обобщающей способностью.
Вероятность ошибки линейного дискриминанта Фишера выражается через расстояние Махаланобиса между классами, в случае, когда классов два:
![equation](http://latex.codecogs.com/gif.latex?R(a)&space;=&space;\Phi&space;(-\frac{1}{2}||\mu_1&space;-&space;\mu_2||_{\sum}))
где ![equation]http://latex.codecogs.com/gif.latex?\Phi&space;(x)&space;=&space;N(x;0,1) - функция стандартного нормального распределения.
Возьмем наш датасет ирисов, где   setosa и versicolor будут одним классом, а virginica другой. Классифицировать будем по Petal.Length и Petal.Width. 
В этом случае итоговый результат изобразим на графике:
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/lda_result.jpg).

# Линейные классификаторы
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

![](https://github.com/Goncharoff/SMPR/blob/master/imgs/all_results.png)


### SVM
Метод опорных векторов

В настоящее время метод опорных векторов (SVM) считается одним из лучших методов классификации!
Метод SVM обладает несколькими замечательными свойствами:

1. Обучение SVM сводится к задаче квадратичного программирования, имеющей единственное решение, которое вычисляется достаточно эффективно даже на выборках в сотни тысяч объектов;
2. Решение обладает свойством разреженности: положение оптимальной разделяющей гиперплоскости зависит лишь от небольшой доли обучающих объектов. Они и называются опорными векторами, остальные объекты фактически не задействуются;
СМПР
3. С помощью функции ядра метод обобщается на случай нелинейных разделяющих поверхностей. Вопрос о выборе ядра, оптимального для
данной прикладной задачи, до сих пор остается открытой теоретической проблемой.

Будем ипсользовать virginica и versicolor. По 3 и 4 параметрам datafram'а

В результате получим: 
![](https://github.com/Goncharoff/SMPR/blob/master/imgs/SVM_resul.png)
Благодоря библиотеке kernlab выведем также полученный результат:
Support Vector Machine object of class "ksvm" 

SV type: C-svc  (classification) 
 parameter : cost C = 1 

Gaussian Radial Basis kernel function. 
 Hyperparameter : sigma =  2.22977846013437 

Number of Support Vectors : 30 

Objective Function Value : -8.5529 
Training error : 0 
