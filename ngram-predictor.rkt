#lang racket

(require csc151)
(require rackunit)
(require rackunit/text-ui)



; NO BOX COMMENTS THIS TIME:
; PART 1 - Calculating N-Grams

;;; example-sentence-words --> list?
;;;
;;; returns an example list we can use to test individual functions
(define example-sentence-words
  (:> "They make take our lives, but they'll never take our freedom!"
      (r-s string-split #px"[^A-Za-z0-9]")
      (l-s filter (lambda (w) (< 0 (string-length w))))
      (l-s map string-downcase)))

;;; (ngrams l n) -> listof listof string?
;;; l : listof string?
;;; n : number?
;;;
;;; returns a list of lists that each have length n based on each element of l, producing an n-gram for l
(define ngrams
  (lambda (l n)
    (if (or (null? l) (= (length l) (- n 1)))
        null
        (cons (take l n) (ngrams (drop l 1) n)))))

(define ngrams-tests
  (test-suite "tests of ngrams"
              (check-equal? (ngrams example-sentence-words 2)
                            '(("they" "make") ("make" "take") ("take" "our") ("our" "lives") ("lives" "but") ("but" "they") ("they" "ll") ("ll" "never") ("never" "take") ("take" "our") ("our" "freedom")))
              (check-equal? (ngrams example-sentence-words 3)
                            '(("they" "make" "take") ("make" "take" "our") ("take" "our" "lives") ("our" "lives" "but") ("lives" "but" "they") ("but" "they" "ll") ("they" "ll" "never") ("ll" "never" "take") ("never" "take" "our") ("take" "our" "freedom")))
              (check-equal? (ngrams '("I" "am" "good" "at" "soccer") 2)
                            '(("I" "am") ("am" "good") ("good" "at") ("at" "soccer")))
              (check-equal? (ngrams '("I" "am" "good" "at" "soccer") 3)
                            '(("I" "am" "good") ("am" "good" "at") ("good" "at" "soccer")))
              (check-equal? (ngrams '("I" "am" "good" "at" "soccer") 4) 
                            '(("I" "am" "good" "at") ("am" "good" "at" "soccer")))
              (check-equal? (ngrams '() 50) '())))

; PART 2 - Frequency Analysis

#|

NOTE: Even though I figured them out after getting PMs hint on the announcements tab in Teams
I still spent hours and hours on this part. I love CS, but dang this was ridiculous (if only
I didn't think I had to use recursion on inc-freq and inc-lead-dict, this part would have been
much easier).

|#

;;; (inc-freq freqs y) --> list?
;;; freqs : list?
;;; y : string?
;;;
;;; increments one frequency of y onto a dictionary freqs
(define inc-freq
  (lambda (freqs y)
    (if (dict-has-key? freqs y)
        (dict-set freqs y (+ (dict-ref freqs y) 1))
        (dict-set freqs y 1))))

;;; (inc-lead-dict leads x y) --> list?
;;; leads : list?
;;; x : string?
;;; y : string?
;;;
;;; appends a pair '(x . y) onto a lead dictionary leads
(define inc-lead-dict
  (lambda (leads x y)
    (if (dict-has-key? leads x)
        (dict-set leads x (inc-freq (dict-ref leads x) y))
        (dict-set leads x (list (cons y 1))))))

;;; (calculate-word-frequencies-num d lst) --> list?
;;; d : list?
;;; lst: list?
;;;
;;; calculates and appends all words from word d and converts it into a lead dictionary lst
(define calculate-word-frequencies-num
  (lambda (d lst)
    (match d
      ['() null]
      [(cons head tail) (cons (inc-lead-dict lst (list-ref head 0) (list-ref head 1))
                              (calculate-word-frequencies-num tail (inc-lead-dict lst (list-ref head 0) (list-ref head 1)) ))])))

;;; (calculate-word-frequencies d) --> list?
;;; d : list?
;;;
;;; converts a list into a lead dictionary and cleans it up to match requirements.
(define calculate-word-frequencies
  (lambda (d)
    (let ([new-list (calculate-word-frequencies-num d '())])
      (list-ref new-list (- (length new-list) 1)))))

(define calc-word-freq-tests
  (test-suite "tests of calculate-word-frequencies"
              (check-equal? (calculate-word-frequencies (ngrams example-sentence-words 2)) '(("they" ("make" . 1) ("ll" . 1))
                                                                                             ("make" ("take" . 1))
                                                                                             ("take" ("our" . 2))
                                                                                             ("our" ("lives" . 1) ("freedom" . 1))
                                                                                             ("lives" ("but" . 1))
                                                                                             ("but" ("they" . 1))
                                                                                             ("ll" ("never" . 1))
                                                                                             ("never" ("take" . 1))
                                                                                             ))
              (check-equal? (calculate-word-frequencies '(("a" "b") ("b" "c") ("c" "d") ("c" "e"))) '(("a" ("b" . 1)) ("b" ("c" . 1)) ("c" ("d" . 1) ("e" . 1))))
              (check-equal? (calculate-word-frequencies (ngrams '("I" "am" "good" "at" "soccer") 2)) '(("I" ("am" . 1)) ("am" ("good" . 1)) ("good" ("at" . 1)) ("at" ("soccer" . 1))))
              (check-equal? (calculate-word-frequencies '(("yee" "yoo"))) '(("yee" ("yoo" . 1))))))
; NOTE: Any null cases or single-string cases would result in error.
; I didn't know how to incorporate that into the Racket test-suite,
; so I decided to put it as a note here



; PART 3 - Frequency-based Random Word Generation

;;; (pairs-to-list-num l n1 n2) --> list?
;;; l : list?
;;; n1 : number?
;;; n2 : number?
;;;
;;; converts string-to-number pairs into a list of strings that occur as many times as the pair was assigned
;;; (e.g. (pairs-to-list-num '(("a" . 2) ("b" . 6) ("c" . 10)) 0 0) --> '("a" "a" "b" "b" "b" "b" "b" "b" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c"))
(define pairs-to-list-num
  (lambda (l n1 n2)
    (if (or (null? l) (= (length l) n1))
        null
        (if (pair? (list-ref l n1))
            (if (= n2 (cdr (list-ref l n1)))
                (pairs-to-list-num l (+ n1 1) 0)
                (cons (car (list-ref l n1)) (pairs-to-list-num l n1 (+ n2 1))))
            null))))

;;; (pairs-to-list l) --> list?
;;; l : list?
;;;
;;; converts string-to-number pairs into a list of strings that occur as many times as the pair was assigned
;;; removes n1 and n2 requirements from pairs-to-list-num for easy usage
(define pairs-to-list
  (lambda (l)
    (pairs-to-list-num l 0 0)))

;;; (pick-word-range freqs idx) --> string?
;;; freqs : list?
;;; idx : number?
;;;
;;; determines the appropriate string that matches the specified index of the list of pairs
(define pick-word-range
  (lambda (freqs idx)
    (list-ref (pairs-to-list freqs) idx)))

(define pick-word-range-tests
  (test-suite "pick-word-range tests"
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 3)
                            "b")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 0)
                            "a")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 2)
                            "a")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 7)
                            "c")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 4)
                            "c")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 10)
                            "d")
              (check-equal? (pick-word-range '(("a" . 3)
                                               ("b" . 1)
                                               ("c" . 5)
                                               ("d" . 2)) 9)
                            "d")))
; NOTE: there would be errors for the cases that idx = -1 or idx = 11 because of index-out-of-bounds
; I do not know how to list errors on racket test suite so I'm listing it here.

;;; (pick-random-word freqs) --> string?
;;; freqs : list?
;;;
;;; chooses a random string from a list of pairs based on frequency
;;; pair strings with higher associated numbers are more likely to be drawn
(define pick-random-word
  (lambda (freqs)
    (pick-word-range freqs (random (length (pairs-to-list freqs)) ))))


; PART 4 - Random Sentence Generation
;;; (make-random-list-num n leads prev n2) --> list?
;;; n : number?
;;; leads : list?
;;; prev : string?
;;; n2 : number?
;;;
;;; makes a list of strings as a random sentence based on word frequency 
(define make-random-list-num
  (lambda (n leads prev n2)
    (if (= n2 n)
        null
        (cons (pick-random-word (dict-ref leads prev)) (make-random-list-num n leads (pick-random-word (dict-ref leads prev)) (+ n2 1))))))

;;; (make-random-list n leads prev) --> list?
;;; n : number?
;;; leads : list?
;;; prev : string?
;;;
;;; makes a list of strings as a random sentence based on word frequency, but simplifies make-random-list-num for easier usage
(define make-random-list
  (lambda (n leads prev)
    (make-random-list-num n leads prev 0)))

;;; (list-to-string l) --> string?
;;; l : list?
;;;
;;; converts a list of strings into a single string
(define list-to-string
  (lambda (l)
    (match l
      ['() ""]
      [(cons head tail) (string-append head " " (list-to-string tail))])))

;;; (make-random-sentence n leads init) --> string?
;;; n : number?
;;; leads : list?
;;; init : string?
;;;
;;; puts together a list of random words (based on frequency) and converts it into a single trimmed string.
(define make-random-sentence
  (lambda (n leads init)
    (string-trim (string-append init " " (list-to-string (make-random-list n leads init))))))

; Part 5: Try It Out!

;;; trump-tweets --> list?
;;;
;;; a relatively small compilation of Donald Trump tweets in 2016
(define trump-tweets
  (:> "My Administration will follow two simple rules.
Economists say Trump delivered hope.
The world was gloomy before I won - there was no hope. Now the market is up nearly 10% and Christmas spending is over a trillion dollars!
The United Nations has such great potential but right now it is just a club for people to get together, talk and have a good time. So sad!
President Obama said that he thinks he would have won against me. He should say that but I say NO WAY! - jobs leaving, ISIS, OCare, etc.
The big loss yesterday for Israel in the United Nations will make it much harder to negotiate peace.Too bad, but we will get it done anyway!
Campaigning to win the Electoral College is much more difficult &amp; sophisticated than the popular vote. Hillary focused on the wrong states!
Bill Clinton stated that I called him after the election. Wrong, he called me (with a very nice congratulations).
We did it! Thank you to all of my great supporters, we just officially won the election (despite all of the distorted and inaccurate media).
Today there were terror attacks in Turkey, Switzerland and Germany - and it is only getting worse. The civilized world must change thinking!
We should tell China that we don't want the drone they stole back.- let them keep it!
China steals United States Navy research drone in international waters - rips it out of water and takes it to China in unprecedented act.
Last night in Orlando, Florida, was incredible - massive crowd - THANK YOU FLORIDA!
Thank you Florida. My Administration will follow two simple rules: BUY AMERICAN and HIRE AMERICAN!
Well, we all did it, together! I hope the \"MOVEMENT\" fans will go to D.C. on Jan 20th for the swearing in. Let's set the all time record!
Are we talking about the same cyberattack where it was revealed that head of the DNC illegally gave Hillary the questions to the debate?
Thank you Pennsylvania! Together, we are going to MAKE AMERICA GREAT AGAIN!
Can you imagine if the election results were the opposite and WE tried to play the Russia/CIA card. It would be called conspiracy theory!
their country (the U.S. doesn't tax them) or to build a massive military complex in the middle of the South China Sea?  I don't think so!
Did China ask us if it was OK to devalue their currency (making it hard for our companies to compete), heavily tax our products going into.
I have recieved and taken calls from many foreign leaders despite what the failing @nytimes said. Russia, U.K., China, Saudi Arabia, Japan.
China wouldn't provide a red carpet stairway from Air Force One and then Philippines President calls Obama.
The @USCHAMBER must fight harder for the American worker. China, and many others, are taking advantage of U.S. with our terrible trade pacts.
Hillary Clinton surged the trade deficit with China 40% as\nSecretary of State, costing Americans millions of jobs.
Crooked Hillary has zero imagination and even less stamina. ISIS, China, Russia and all would love for her to be president. 4 more years!
Crooked Hillary Clinton put out an ad where I am misquoted on women. Can't believe she would misrepresent the facts! My hit was on China.
The pathetic new hit ad against me misrepresents the final line. \"You can tell them to go BLANK themselves\" - was about China, NOT WOMEN!
If Crooked Hillary Clinton can't close the deal on Crazy Bernie, how is she going to take on China, Russia, ISIS and all of the others?
Crooked Hillary just can't close the deal with Bernie. It will be the same way with ISIS, and China on trade, and Mexico at the border. Bad!
Minimizing dependency on China is crucial."
      (r-s string-split #px"[^A-Za-z0-9]")
      (l-s filter (lambda (w) (< 0 (string-length w))))
      (l-s map string-downcase)))

;;; (final-test num-words sentence trigger) --> string?
;;; num-words : number?
;;; sentence : string?
;;; trigger : string?
;;;
;;; Makes a string of word length num-words based around an initial word trigger and word frequency in the string sentence
(define final-test
  (lambda (num-words sentence trigger)
    (make-random-sentence num-words (calculate-word-frequencies (ngrams sentence 2)) trigger)))

; Some results:
;> (final-test 50 trump-tweets "administration")
;"administration will follow president him theory their country making it was i and we it will gloomy china not u k with isis ocare etc the american and have others leaders despite what of be a massive dollars the swearing they orlando election well we are it russia i have me"
;> (final-test 50 trump-tweets "hillary")
;"hillary has zero great supporters can t want the distorted would so are we to the time china to we just taking advantage of the leaving hillary clinton put the south way where it have him with the all my great will go two simple rules economists american worker then a"
;> (final-test 50 trump-tweets "china")
;"china and t believe a club military thank you florida if the hillary the officially won there same are hillary just all s doesn t provide so did it china anyway campaigning to play the opposite was change thinking we did of u crooked hillary the put tell florida was you"
;> (final-test 50 trump-tweets "china")
;"china ask international switzerland and germany philippines president calls from the same nations bill research drone in stole back let them with the drone is much going into i hope the russia bad but dependency on women russia as secretary of my movement are hillary the stated you them russia women"
;> (final-test 50 trump-tweets "clinton")
;"clinton surged out of the costing americans millions of my and it of jobs costing americans millions of the administration will be to themselves was on massive crowd complex in the florida my you florida if crooked deal and have tried be called opposite he navy research drone in stole back"
;> (final-test 50 trump-tweets "my")
;"my administration will make two simple rules buy american well we will going advantage of the united deficit with bernie and it spending is just would be called deal did the russia crooked hillary focused officially good dollars the election nations bill research drone in unprecedented s or get their country"
;> (final-test 50 trump-tweets "will")
;"will follow it would win peace too bad minimizing dependency on the if tell think the distorted cia china not she can very nice congratulations we talking about the russia he hope a and hire won against failing cyberattack where it is for people in the movement states thinks tell that"
;> (final-test 56 trump-tweets "my")
;"my hit supporters we will about the electoral bad but dependency on china wouldn and and then of u great will make to devalue heavily tax our terrible trade deficit inaccurate spending is much a good carpet stairway from air force one and all thank for people in let act last night in the popular nytimes said"
;> (final-test 28 trump-tweets "my")
;"my great will go it great supporters but we talking t provide she going to devalue a massive nice congratulations we will t close them keep compete the united"
;> (final-test 28 trump-tweets "my")
;"my administration will make two simple rules buy american well china is t want the south k set the deal is crucial can won there debate k set china"
;> (final-test 28 trump-tweets "my")
;"my administration will be two simple rules economists american well we talking say that hope with leaving isis china christmas american worker we just officially won the me with"
;> (final-test 28 trump-tweets "china")
;"china saudi united nations navy clinton put the united crooked hillary focused put out of state and it was in questions with china products trade and hillary clinton put"










