#lang racket

(define (find-number list1 name number) ;This function find the desired country within the list then return score.
  (if(null? list1)
     number ;return score
     (if(eq? name (caar list1)) ;If desired name is equal country name
        (cadr(car list1)) ;return score
        (find-number (cdr list1) name number)))) ;If desired name is not equal country name, calls this function again.

(define (add-number list1 name number newlist) ;This function find the desired country within the score list, then update score list.
  (if(null? list1)
     newlist ;return new list
     (if(eq? name (caar list1)) ;If desired name is equal country name
        (add-number (cdr list1) name number (append newlist (list(append (list name)(list(+ number (cadr(car list1)))))))) ;Calls this function again but an updated score.
        (add-number (cdr list1) name number (append newlist (list(car list1))))))) ;Calls this function again. 
  
(define (checklist list1 name) ;the country does not give itself points. Also to get rid of the first element of the list
  (if(null? list1)
     (cdr '(1)) 
     (if(eq? name (car list1))
        (cdr '(1)) ;return empty list because the country does not give itself points.
        (cdr list1)))) ;return  the list without the first element.

(define (calculate-point list1 list2 list3 list4) ;To collect points earned by countries. List1 is given score list. List2 country list. List3 empty list. List4 earned score list.
  (if(null? list2)
     list4 ;return earned score list.
     (if (null? list1) ;if given score list is null, all scores of a country are collected.
         (calculate-point list3 (cdr list2) list1 list4) ;Function passes to the other country.
         (if( = (length(checklist (car list1) (car list2))) 0) ;A country has not received score from a country or itself.
            (calculate-point (cdr list1) list2 (append list3 (list(car list1))) list4) ;Calls this function again and country that given score is changed.
            (calculate-point (cdr list1) list2 (append list3 (list(car list1))) (add-number list4 (car list2) (find-number (checklist (car list1) (car list2)) (car list2) 0) '())))))) ;Calls this function and country that given score is changed and update the score list.




(define (separate-numbers list blanklist)    ;Separete numbers from point this Ex. list= '('a 2) ('b 3) ('c 1)) The function takes this list and return '(2 3 1)
  (if (null? list) ; If list is null that means separating is end and then we can return new list.
      blanklist ;Return separate-numbers list, It named blanklist but, it just blank in the start then some thing added in it.
      (separate-numbers (cdr list) (append blanklist (cdr(car list)))) ; Take  second item in lists there are numbers and then append it blanklist that is return at the end of this function
  )

)


(define (sorting list1 numberlist blanklist)   ;Takes 3 input, For Ex.: list1 = '('a 2) ('b 3))  numberlist = '(2 3) blanklist ='() , and sort list1 like number list: returns '('b 3) ('a 2))
  (if (null? list1) ; If list is null that means separating is end and then we can return new list.
      blanklist
      (if (=(car(cdr(car list1))) (car numberlist)) ; Check numbers list number and number that in list1
          (sorting (cdr list1) (cdr numberlist) (append blanklist (list(car list1)))) ;If they are same then the list that in list1[ex. ('b ^)] is appended to blanklist
          (sorting (append (cdr list1) (list(car list1))) numberlist blanklist  ) ; If they are not same the first item in list1 is popped and append to end of list1, and recursion contionies for others
      )
  )
)

(define (add-numbers list1 n list2 ) ;After the sorting list that is like '('b 3) ('a 2)), should added ranking numbers. Return is like '(1 'b 3) (2 'a 2))
  (if(null? list1) ; If list is null that means separating is end and then we can return new list.
     list2
     (add-numbers (cdr list1) (+ n 1) (append list2 (list (append (list (+ n 1)) (list(caar list1)) (list(car(cdr(car list1)))))))) ;Add numbers one by one and append it our blanklist : list2 
   )
 )



#|  This is final state, and edit all of funcsions is here work together.|#
(define (eurovision-ranking-program list1 list2 list4 ) ;  It takes three input there are like list1='('a 2) ('b 3)) list2= '('b 'a):(Include all strings in lista)  list4= '(('a 0) ('b 0)):(Point chart)
  (add-numbers (sorting (calculate-point list1 list2 '() list4)
                     (sort ( separate-numbers (calculate-point list1 list2 '() list4) '()) >)  '() )
             0 '())
  )
#|  For above function:
1- I takes list1 and it calculate sum for all stirngs that in list1 (return it like point chart for example: '(('a 20) ('b 44))
2- Separete-numbers from list1 and sort the numbers 
3- Use the point chart and separete numbers for separete the point chart
4- Add ranking numbers to sorted point chart
5- Return it
|#



#|  That list include all countries that have voted for other countries in Eurovision.  |#
(define list1 '(("Norway" ("Bulgaria" 12) ("Portugal" 10) ("Germany" 8) ("Romania" 7) ("Croatio" 6) ("Austria" 5) ("Moldova" 4) ("Belgium" 3) ("Sweden" 2) ("Hungary" 1))
        ("Portugal" ("Norway" 12) ("Bulgaria" 10) ("Germany" 8) ("Romania" 7) ("Croatio" 6) ("Austria" 5) ("Moldova" 4) ("Belgium" 3) ("Sweden" 2) ("Hungary" 1))
        ("Bulgaria" ("Hungary" 12) ("Norway" 10)  ("Germany" 8) ("Austria" 7) ("Croatio" 6) ("Romania" 5) ("Sweden" 4) ("Belgium" 3) ("Moldova" 2) ("Portugal" 1))
        ("Germany" ("Croatio"  12) ("Bulgaria" 10) ("Norway" 8) ("Sweden" 7) ("Portugal" 6) ("Austria" 5) ("Moldova" 4) ("Belgium" 3) ("Romania" 2) ("Hungary" 1))
        ("Romania" ("Portugal" 12) ("Moldova" 10) ("Belgium" 8) ("Norway" 7) ("Croatio" 6) ("Austria" 5) ("Bulgaria" 4) ("Germany" 3) ("Sweden" 2) ("Hungary" 1))
        ("Croatio" ("Portugal" 12) ("Bulgaria" 10) ("Austria" 8) ("Romania" 7) ("Norway" 6) ("Germany" 5) ("Moldova" 4) ("Belgium" 3) ("Sweden" 2) ("Hungary" 1))
        ("Austria" ("Portugal" 12) ("Bulgaria" 10) ("Germany" 8) ("Romania" 7) ("Croatio" 6) ("Norway" 5) ("Moldova" 4) ("Belgium" 3) ("Sweden" 2) ("Hungary" 1))
        ("Moldova" ("Croatio" 12) ("Portugal" 10) ("Germany" 8) ("Romania" 7) ("Bulgaria" 6) ("Austria" 5) ("Norway" 4) ("Belgium" 3) ("Sweden" 2) ("Hungary" 1))
        ("Belgium" ("Portugal" 12) ("Bulgaria" 10) ("Germany" 8) ("Romania" 7) ("Croatio" 6) ("Austria" 5) ("Moldova" 4) ("Norway" 3) ("Sweden" 2) ("Hungary" 1))
        ("Sweden" ("Hungary" 12) ("Bulgaria" 10) ("Croatio" 8) ("Romania" 7) ("Germany" 6) ("Austria" 5) ("Moldova" 4) ("Belgium" 3) ("Norway" 2) ("Portugal" 1))
        ("Hungary" ("Portugal" 12) ("Bulgaria" 10) ("Germany" 8) ("Romania" 7) ("Croatio" 6) ("Austria" 5) ("Moldova" 4) ("Belgium" 3) ("Sweden" 2) ("Norway" 1))))


#|  That list include all countries that is in list1.|#
(define list2 '("Norway" "Portugal" "Bulgaria" "Germany" "Romania" "Croatio" "Austria" "Moldova" "Belgium" "Sweden" "Hungary"))

#|  That list point chart, then the list use for storing points|#
(define list4 '(("Norway" 0) ("Portugal" 0) ("Bulgaria" 0) ("Germany" 0) ("Romania" 0) ("Croatio" 0) ("Austria" 0) ("Moldova" 0) ("Belgium" 0) ("Sweden" 0) ("Hungary" 0)))


(eurovision-ranking-program list1 list2 list4)  ; start the program with three inputs that typen above. 
